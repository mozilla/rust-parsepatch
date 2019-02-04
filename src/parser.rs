use chardet::{detect, charset2encoding};
use encoding::{Encoding, DecoderTrap};
use encoding::all::UTF_8;
use encoding::label::encoding_from_whatwg_label;
use serde::Deserialize;
use std::path::PathBuf;
use std::fs::File;
use std::io::Read;

struct Patch<'a> {
    pub buf: &'a [u8],
    pub pos: usize,
}

#[derive(Debug)]
struct Line<'a> {
    buf: &'a [u8],
    pos: usize,
}

#[derive(Deserialize, Debug, PartialEq)]
pub struct LineChange {
    pub line: u64,
    pub deleted: bool,
    pub data: String,
}

#[derive(Deserialize, Debug)]
pub struct Changes {
    pub filename: String,
    pub new: bool,
    pub deleted: bool,
    pub binary: bool,
    pub renamed_from: Option<String>,
    pub lines: Vec<LineChange>,
}

impl Changes {
    fn new(filename: String) -> Changes {
        Changes {
            filename,
            new: false,
            deleted: false,
            binary: false,
            renamed_from: None,
            lines: Vec::new(),
        }
    }

    fn set_renamed(&mut self, filename: String) {
        self.renamed_from = Some(filename);
    }
}

impl<'a> Line<'a> {

    fn to_string(&self) -> String {
        String::from_utf8(self.buf.to_vec()).unwrap()
    }

    fn is_binary(&self) -> bool {
        return self.buf == [b'G', b'I', b'T', b' ',
                            b'b', b'i', b'n', b'a', b'r', b'y', b' ',
                            b'p', b'a', b't', b'c', b'h'];
    }

    fn starts_with(&self, v: &[u8]) -> bool {
        v.len() <= self.buf.len() && self.buf.iter().zip(v.iter()).all(|(x, y)| *x == *y)
    }

    fn first_is(&self, c: u8) -> bool {
        self.buf.len() >= 1 && self.buf[0] == c
    }

    fn get_number(&mut self) -> u64 {
        let mut x = 0;
        for (n, c) in self.buf[self.pos..].iter().enumerate() {
            if *c >= b'0' && *c <= b'9' {
                x = x * 10 + (*c - b'0') as u64; 
            } else {
                self.pos += n;
                return x;
            }
        }
        0
    }

    fn parse_numbers(&mut self) -> (u64, u64, u64, u64) {
        // NUMS_PAT = re.compile(r'^@@ -([0-9]+),?([0-9]+)? \+([0-9]+),?([0-9]+)? @@')
        self.pos = 4;
        let x = self.get_number();
        let y = if self.buf[self.pos] == b',' {
            self.pos += 1;
            self.get_number()
        } else {
            1
        };
        self.pos += 2;
        let z = self.get_number();
        let t = if self.buf[self.pos] == b',' {
            self.pos += 1;
            self.get_number()
        } else {
            1
        };
        (x, y, z, t)
    }

    fn parse_files(&mut self) -> (String, String) {
        // diff --git a/... b/...
        // diff -r
        let mut old_path: &[u8] = &[];
        let mut pos = 0;
        let mut white_count = 0;
        for (n, c) in self.buf.iter().enumerate() {
            if *c == b' ' {
                white_count += 1;
                match white_count {
                    2 => {
                        pos = n + 1;
                    }
                    3 => {
                        old_path = &self.buf[pos..n];
                        pos = n + 1;
                        break;
                    }
                    _ => { }
                }
            }
        }
        let mut new_path = &self.buf[pos..];
        if old_path.len() >= 2 && old_path[0] == b'a' && old_path[1] == b'/' {
            old_path = &old_path[2..]
        }
        if new_path.len() >= 2 && new_path[0] == b'b' && new_path[1] == b'/' {
            new_path = &new_path[2..]
        }
        (String::from_utf8(old_path.to_vec()).unwrap(), String::from_utf8(new_path.to_vec()).unwrap())
    }
}

impl<'a> Patch<'a> {

    fn get(path: &PathBuf) -> Option<Vec<Changes>> {
        match File::open(path) {
            Ok(mut reader) => {
                let mut data = Vec::new();
                reader.read_to_end(&mut data).unwrap();
                let mut p = Patch {
                    buf: &data,
                    pos: 0,
                };
                Some(p.parse())
            }
            Err(_) => {
                assert!(false, format!("Failed to read the file: {:?}", path));
                None
            },
        }
    }
    
    fn parse(&mut self) -> Vec<Changes> {
        let mut all_changes = Vec::new();
        loop {
            if let Some(mut line) = self.next(Patch::diff, false) {
                self.parse_diff(&mut line, &mut all_changes);
            } else {
                break;
            }
        }
        all_changes
    }

    fn parse_diff(&mut self, line: &mut Line, all_changes: &mut Vec<Changes>) {
        let (old, new) = line.parse_files();
        let mut changes = Changes::new(new);
        let mut line = self.next(Patch::mv, false).unwrap();
        if line.starts_with(&[b'r', b'e', b'n', b'a', b'm', b'e', b' ', b'f', b'r', b'o', b'm']) {
            changes.set_renamed(old);
        } else if line.starts_with(&[b'n', b'e', b'w', b' ', b'f', b'i', b'l', b'e']) {
            changes.new = true;
        } else if line.starts_with(&[b'd', b'e', b'l', b'e', b't', b'e', b'd', b' ', b'f', b'i', b'l', b'e']) {
            changes.deleted = true;
        } else if Patch::diff(&line) {
            all_changes.push(changes);
            self.parse_diff(&mut line, all_changes);
            return;
        }
        if let Some(mut line) = self.next(Patch::useful, false) {
            if Patch::diff(&line) {
                all_changes.push(changes);
                self.parse_diff(&mut line, all_changes);
                return;
            } else if Patch::hunk_at(&line) {
                self.parse_hunks(&mut line, &mut changes);
            } else if line.is_binary() {
                changes.binary = true;
                self.skip_until_empty_line();
            }
        }
        all_changes.push(changes);
    }

    fn parse_hunks(&mut self, line: &mut Line,  mut changes: &mut Changes) {
        let (o1, _, n1, _) = line.parse_numbers();
        self.parse_hunk(o1, n1, &mut changes);
        loop {
            if let Some(mut line) = self.next(Patch::hunk_at, true) {
                let (o1, _, n1, _) = line.parse_numbers();
                self.parse_hunk(o1, n1, &mut changes);
            } else {
                break;
            }
        }
    }

    fn parse_hunk(&mut self, o: u64, n: u64, changes: &mut Changes) {
        let mut old_count = o;
        let mut new_count = n;
        loop {
            if let Some(line) = self.next(Patch::hunk_change, true) {
                if line.first_is(b'-') {
                    changes.lines.push(LineChange {
                        line: old_count,
                        deleted: true,
                        data: Patch::get_line(&line.buf[1..]),
                    });
                    old_count += 1;
                } else if line.first_is(b'+') {
                    changes.lines.push(LineChange {
                        line: new_count,
                        deleted: false,
                        data: Patch::get_line(&line.buf[1..]),
                    });
                    new_count += 1;
                } else {
                    old_count += 1;
                    new_count += 1;
                }
            } else {
                break;
            }
        }
    }

    fn get_line(buf: &[u8]) -> String {
        match std::str::from_utf8(buf) {
            Ok(s) => {
                s.to_string()
            }
            Err(_) => {
                let res = detect(buf);
                let coder = encoding_from_whatwg_label(charset2encoding(&res.0));
                if let Some(coder) = coder {
                    coder.decode(buf, DecoderTrap::Ignore).unwrap()
                } else {
                    UTF_8.decode(buf, DecoderTrap::Ignore).unwrap()
                }
            }
        }
    }
    
    fn new(buf: &[u8]) -> Patch {
        Patch {
            buf,
            pos: 0,
        }
    }

    fn next<F>(&mut self, filter: F, return_on_false: bool) -> Option<Line<'a>> where F: Fn(&Line) -> bool {
        if self.pos >= self.buf.len() {
            return None;
        }

        let mut pos = self.pos;
        for (n, c) in self.buf[self.pos..].iter().enumerate() {
            if *c == b'\n' {
                let line = Line {
                    buf: &self.buf[pos..self.pos + n],
                    pos: 0,
                };
                if filter(&line) {
                    self.pos += n + 1;
                    return Some(line);
                } else if return_on_false {
                    return None;
                }
                pos = self.pos + n + 1;
            }
        }
        None
    }

    fn skip_lines(&mut self, to_skip: usize) {
        let mut to_skip = to_skip;
        for (n, c) in self.buf[self.pos..].iter().enumerate() {
            if *c == b'\n' {
                if to_skip == 1 {
                    self.pos += n + 1;
                    break;
                } else {
                    to_skip -= 1;
                }
            }
        }
    }

    fn skip_until_empty_line(&mut self) {
        let mut spos = 0;
        for (n, c) in self.buf[self.pos..].iter().enumerate() {
            if *c == b'\n' {
                if n == spos {
                    self.pos += n + 1;
                    return;
                } else {
                    spos = n + 1;
                }
            }
        }
        self.pos = self.buf.len();
    }

    fn useful(line: &Line) -> bool {
        !line.starts_with(&[b'-', b'-', b'-'])
            && !line.starts_with(&[b'+', b'+', b'+'])
            && !line.starts_with(&[b'i', b'n', b'd', b'e', b'x', b' '])
            && !line.starts_with(&[b'o', b'l', b'd', b' ', b'm', b'o', b'd', b'e'])
            && !line.starts_with(&[b'n', b'e', b'w', b' ', b'm', b'o', b'd', b'e'])
            && !line.starts_with(&[b'r', b'e', b'n', b'a', b'm', b'e', b' '])
    }

    fn empty(line: &Line) -> bool {
        line.buf.len() == 0
    }

    fn diff(line: &Line) -> bool {
        line.starts_with(&[b'd', b'i', b'f', b'f', b' ', b'-', b'-', b'g', b'i', b't', b' ', b'a', b'/']) || line.starts_with(&[b'd', b'i', b'f', b'f', b' ', b'-', b'r', b' '])
    }

    fn mv(_: &Line) -> bool {
        true
    }

    fn hunk_at(line: &Line) -> bool {
        line.first_is(b'@')
    }

    fn hunk_change(line: &Line) -> bool {
        line.first_is(b'-') || line.first_is(b'+') || line.first_is(b' ')
    }
}

#[cfg(test)]
mod tests {
    
    use std::fs::{self, DirEntry};
    use std::io::BufReader;
    
    use super::*;

    fn compare(json: &Vec<Changes>, patch: &mut Vec<Changes>) {
        patch.retain(|c| !c.binary);
        assert!(json.len() == patch.len(), "Not the same length");
        for (cj, cp) in json.iter().zip(patch.iter()) {
            assert!(cj.filename == cp.filename, format!("Not the same filename: {} ({} expected)", cp.filename, cj.filename));
            //assert!(cj.renamed_from == cp.renamed_from, format!("Not the same filename: {} ({} expected)", cj.filename, cp.filename)));
            assert!(cj.lines.len() == cp.lines.len(), "Not the same length for changed lines");
            for (lj, lp) in cj.lines.iter().zip(cp.lines.iter()) {
                assert!(lj == lp, format!("Not the same line change: {:?} ({:?} expected", lp, lj));
            }
        }
    }

    #[test]
    fn test_numbers() {
        let cases = [("@@ -123,456 +789,101112 @@", (123, 456, 789, 101112)),
                     ("@@ -123 +789,101112 @@", (123, 1, 789, 101112)),
                     ("@@ -123,456 +789 @@", (123, 456, 789, 1)),
                     ("@@ -123 +789 @@", (123, 1, 789, 1))];
        for c in cases.into_iter() {
            let buf = c.0.as_bytes();
            let mut line = Line {
                buf: &buf,
                pos: 0,
            };
            assert!(line.parse_numbers() == c.1);
        }
    }

    #[test]
    fn test_line_starts_with() {
        let cases = [("+++ hello", "+++"),
                     ("+++ hello", "++++"),];
        for c in cases.into_iter() {
            let buf = c.0.as_bytes();
            let line = Line {
                buf: &buf,
                pos: 0,
            };
            let pat = c.1.as_bytes();
            assert!(line.starts_with(pat) == c.0.starts_with(c.1));
        }
    }

    #[test]
    fn test_skip_until_empty_line() {
        let s = vec!["a. string1",
                     "b. string2",
                     "",
                     "c. string3"];
        let s = s.join("\n");
        let cpos = s.find('c').unwrap();
        let mut p = Patch {
            buf: s.as_bytes(),
            pos: 0,
        };
        p.skip_until_empty_line();
        assert!(p.pos == cpos);
    }

    #[test]
    fn test_filter_non_empty() {
        let s = vec!["a. string1",
                     "b. string2",
                     "",
                     "c. string3"];
        let s = s.join("\n");
        let cpos = s.find('c').unwrap();
        let mut p = Patch {
            buf: s.as_bytes(),
            pos: 0,
        };
        let line = p.next(Patch::empty, false).unwrap();
        assert!(line.to_string() == "".to_string());
        assert!(p.pos == cpos);
    }

    #[test]
    fn test_filter_useless() {
        let s = vec!["a. string1",
                     "+++ abcde",
                     "index abcde",
                     "old mode abcde",
                     "new mode abcde",
                     "b. string2",
                     "C. string3",
        ];
        let s = s.join("\n");
        let cpos = s.find('C').unwrap();
        let ppos = s.find('+').unwrap();
        let mut p = Patch {
            buf: s.as_bytes(),
            pos: 0,
        };
        let line = p.next(Patch::mv, false).unwrap();
        assert!(line.to_string() == "a. string1".to_string());
        assert!(p.pos == ppos);

        let line = p.next(Patch::useful, false).unwrap();
        assert!(line.to_string() == "b. string2".to_string());
        assert!(p.pos == cpos);
    }

    #[test]
    fn test_parse_files() {
        let s = "diff --git a/foo/bar.cpp b/Foo/Bar/bar.cpp";
        let mut line = Line {
            buf: s.as_bytes(),
            pos: 0,
        };
        let (old, new) = line.parse_files();
        assert!(old == "foo/bar.cpp".to_string());
        assert!(new == "Foo/Bar/bar.cpp".to_string());
    }

    #[test]
    fn test_parse() {
        for entry in fs::read_dir(PathBuf::from("./tests/output")).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if !path.is_dir() && path.extension().unwrap() == "json" {
                let file = File::open(&path).unwrap();
                let reader = BufReader::new(file);
                let json_patch = serde_json::from_reader::<_, Vec<Changes>>(reader).unwrap();

                let filename = path.with_extension("patch");
                let filename = filename.file_name().unwrap();
                let path = PathBuf::from("./tests/patches/").join(filename);
                let mut patch = Patch::get(&path).unwrap();

                compare(&json_patch, &mut patch);
            }
        }
    }
}
