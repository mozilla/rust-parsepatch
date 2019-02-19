use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

pub trait Diff {
    fn set_info(&mut self, old_name: &str, new_name: &str, op: FileOp, binary: bool);
    fn add_line(&mut self, old_line: u64, new_line: u64, line: &[u8]);
    fn close(&mut self);
}

pub trait Patch<D: Diff> {
    fn new_diff(&mut self) -> &mut D;
    fn close(&mut self);
}

#[derive(Debug, PartialEq)]
pub enum FileOp {
    New,
    Deleted,
    Renamed,
    None,
}

pub struct PatchReader<'a> {
    buf: &'a [u8],
    pos: usize,
}

#[derive(Debug)]
struct LineReader<'a> {
    buf: &'a [u8],
}

impl<'a> LineReader<'a> {
    fn to_string(&self) -> String {
        std::str::from_utf8(self.buf).unwrap().to_string()
    }

    fn is_binary(&self) -> bool {
        self.buf
            == [
                b'G', b'I', b'T', b' ', b'b', b'i', b'n', b'a', b'r', b'y', b' ', b'p', b'a', b't',
                b'c', b'h',
            ]
    }

    fn is_rename_from(&self) -> bool {
        self.starts_with(&[
            b'r', b'e', b'n', b'a', b'm', b'e', b' ', b'f', b'r', b'o', b'm',
        ])
    }

    fn is_new_file(&self) -> bool {
        self.starts_with(&[b'n', b'e', b'w', b' ', b'f', b'i', b'l', b'e'])
    }

    fn is_deleted_file(&self) -> bool {
        self.starts_with(&[
            b'd', b'e', b'l', b'e', b't', b'e', b'd', b' ', b'f', b'i', b'l', b'e',
        ])
    }

    fn get_file_op(&self) -> FileOp {
        if self.is_new_file() {
            FileOp::New
        } else if self.is_deleted_file() {
            FileOp::Deleted
        } else if self.is_rename_from() {
            FileOp::Renamed
        } else {
            FileOp::None
        }
    }

    fn starts_with(&self, v: &[u8]) -> bool {
        v.len() <= self.buf.len() && self.buf.iter().zip(v.iter()).all(|(x, y)| *x == *y)
    }

    fn first_is(&self, c: u8) -> bool {
        !self.buf.is_empty() && self.buf[0] == c
    }

    fn parse_numbers(&self) -> (u64, u64) {
        let buf = &self.buf[4..];
        let mut x = 0;
        let mut y = 0;
        let mut iter = buf.iter();
        while let Some(c) = iter.next() {
            if *c >= b'0' && *c <= b'9' {
                x = x * 10 + u64::from(*c - b'0');
            } else {
                break;
            }
        }
        while let Some(c) = iter.next() {
            if *c == b'+' {
                break;
            }
        }
        for c in iter {
            if *c >= b'0' && *c <= b'9' {
                y = y * 10 + u64::from(*c - b'0');
            } else {
                break;
            }
        }
        (x, y)
    }

    fn parse_files(&self) -> (&str, &str) {
        // We know we start with 'diff '
        let buf = &self.buf[5..];
        let mut iter = buf.split(|c| *c == b' ');

        // skip --git or -r
        iter.next();

        let mut old_path = iter.next().unwrap();
        let mut new_path = iter.next().unwrap();

        if old_path.len() >= 2 && old_path[..2] == [b'a', b'/'] {
            old_path = &old_path[2..]
        }
        if new_path.len() >= 2 && new_path[..2] == [b'b', b'/'] {
            new_path = &new_path[2..]
        }
        (
            std::str::from_utf8(old_path).unwrap(),
            std::str::from_utf8(new_path).unwrap(),
        )
    }
}

impl<'a> PatchReader<'a> {
    pub fn by_path<D: Diff, P: Patch<D>>(path: &PathBuf, patch: &mut P) {
        match File::open(path) {
            Ok(mut reader) => {
                let mut data = Vec::new();
                reader.read_to_end(&mut data).unwrap();
                PatchReader::by_buf(&data, patch);
            }
            Err(_) => {
                panic!(format!("Failed to read the file: {:?}", path));
            }
        }
    }

    pub fn by_buf<D: Diff, P: Patch<D>>(buf: &[u8], patch: &mut P) {
        let mut p = PatchReader { buf, pos: 0 };
        p.parse(patch);
    }

    fn parse<D: Diff, P: Patch<D>>(&mut self, patch: &mut P) {
        while let Some(mut line) = self.next(PatchReader::diff, false) {
            self.parse_diff(&mut line, patch);
        }
        patch.close();
    }

    fn parse_diff<D: Diff, P: Patch<D>>(&mut self, line: &mut LineReader, patch: &mut P) {
        let (old, new) = line.parse_files();
        let diff = patch.new_diff();
        let mut line = self.next(PatchReader::mv, false).unwrap();
        let op = line.get_file_op();

        if op == FileOp::None && PatchReader::diff(&line) {
            diff.set_info(old, new, op, false);
            diff.close();
            self.parse_diff(&mut line, patch);
        } else if let Some(mut line) = self.next(PatchReader::useful, false) {
            diff.set_info(old, new, op, line.is_binary());
            if PatchReader::diff(&line) {
                diff.close();
                self.parse_diff(&mut line, patch);
            } else if PatchReader::hunk_at(&line) {
                self.parse_hunks(&mut line, diff);
                diff.close();
            } else if line.is_binary() {
                diff.close();
                self.skip_until_empty_line();
            }
        } else {
            diff.set_info(old, new, op, false);
            diff.close();
        }
    }

    fn parse_hunks<D: Diff>(&mut self, line: &mut LineReader, diff: &mut D) {
        let (o1, n1) = line.parse_numbers();
        self.parse_hunk(o1, n1, diff);
        while let Some(line) = self.next(PatchReader::hunk_at, true) {
            let (o1, n1) = line.parse_numbers();
            self.parse_hunk(o1, n1, diff);
        }
    }

    fn parse_hunk<D: Diff>(&mut self, o: u64, n: u64, diff: &mut D) {
        let mut old_count = o;
        let mut new_count = n;
        while let Some(line) = self.next(PatchReader::hunk_change, true) {
            match line.buf[0] {
                b'-' => {
                    diff.add_line(old_count, 0, &line.buf[1..]);
                    old_count += 1;
                }
                b'+' => {
                    diff.add_line(0, new_count, &line.buf[1..]);
                    new_count += 1;
                }
                b' ' => {
                    diff.add_line(old_count, new_count, &line.buf[1..]);
                    old_count += 1;
                    new_count += 1;
                }
                _ => {}
            }
        }
    }

    fn next<F>(&mut self, filter: F, return_on_false: bool) -> Option<LineReader<'a>>
    where
        F: Fn(&LineReader) -> bool,
    {
        if self.pos >= self.buf.len() {
            return None;
        }

        let mut pos = self.pos;
        for (n, c) in self.buf[self.pos..].iter().enumerate() {
            if *c == b'\n' {
                let mut npos = self.pos + n;
                if npos > 0 && self.buf[npos - 1] == b'\r' {
                    npos -= 1;
                }
                let line = LineReader {
                    buf: &self.buf[pos..npos],
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

    fn useful(line: &LineReader) -> bool {
        !line.starts_with(&[b'-', b'-', b'-'])
            && !line.starts_with(&[b'+', b'+', b'+'])
            && !line.starts_with(&[b'i', b'n', b'd', b'e', b'x', b' '])
            && !line.starts_with(&[b'o', b'l', b'd', b' ', b'm', b'o', b'd', b'e'])
            && !line.starts_with(&[b'n', b'e', b'w', b' ', b'm', b'o', b'd', b'e'])
            && !line.starts_with(&[b'r', b'e', b'n', b'a', b'm', b'e', b' '])
    }

    fn diff(line: &LineReader) -> bool {
        line.starts_with(&[b'd', b'i', b'f', b'f', b' '])
    }

    fn mv(_: &LineReader) -> bool {
        true
    }

    fn hunk_at(line: &LineReader) -> bool {
        line.first_is(b'@')
    }

    fn hunk_change(line: &LineReader) -> bool {
        line.first_is(b'-')
            || line.first_is(b'+')
            || line.first_is(b' ')
            || line.starts_with(&[
                b'\\', b' ', b'N', b'o', b' ', b'n', b'e', b'w', b'l', b'i', b'n', b'e',
            ])
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_numbers() {
        let cases = [
            ("@@ -123,456 +789,101112 @@", (123, 789)),
            ("@@ -123 +789,101112 @@", (123, 789)),
            ("@@ -123,456 +789 @@", (123, 789)),
            ("@@ -123 +789 @@", (123, 789)),
        ];
        for c in cases.into_iter() {
            let buf = c.0.as_bytes();
            let line = LineReader { buf: &buf };
            assert!(line.parse_numbers() == c.1);
        }
    }

    #[test]
    fn test_line_starts_with() {
        let cases = [("+++ hello", "+++"), ("+++ hello", "++++")];
        for c in cases.into_iter() {
            let buf = c.0.as_bytes();
            let line = LineReader { buf: &buf };
            let pat = c.1.as_bytes();
            assert!(line.starts_with(pat) == c.0.starts_with(c.1));
        }
    }

    #[test]
    fn test_skip_until_empty_line() {
        let s = vec!["a. string1", "b. string2", "", "c. string3"];
        let s = s.join("\n");
        let cpos = s.find('c').unwrap();
        let mut p = PatchReader {
            buf: s.as_bytes(),
            pos: 0,
        };
        p.skip_until_empty_line();
        assert!(p.pos == cpos);
    }

    #[test]
    fn test_filter_useless() {
        let seps = vec!["\n", "\r\n"];
        let v = vec![
            "a. string1",
            "+++ abcde",
            "index abcde",
            "old mode abcde",
            "new mode abcde",
            "b. string2",
            "C. string3",
        ];
        for sep in seps {
            let s = v.join(sep);
            let cpos = s.find('C').unwrap();
            let ppos = s.find('+').unwrap();
            let mut p = PatchReader {
                buf: s.as_bytes(),
                pos: 0,
            };
            let line = p.next(PatchReader::mv, false).unwrap();
            assert!(line.to_string() == "a. string1".to_string());
            assert!(p.pos == ppos);

            let line = p.next(PatchReader::useful, false).unwrap();
            assert!(line.to_string() == "b. string2".to_string());
            assert!(p.pos == cpos);
        }
    }

    #[test]
    fn test_parse_files() {
        let s = "diff --git a/foo/bar.cpp b/Foo/Bar/bar.cpp";
        let line = LineReader { buf: s.as_bytes() };
        let (old, new) = line.parse_files();
        assert!(old == "foo/bar.cpp");
        assert!(new == "Foo/Bar/bar.cpp");
    }
}
