use std::fmt::{Debug, Formatter, Result};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

/// A type to handle lines in a diff
pub trait Diff {
    /// Set the file info
    fn set_info(&mut self, old_name: &str, new_name: &str, op: FileOp, binary: bool);

    /// Add a line in the diff
    ///
    /// If a line is added (+) then old_line is 0 and new_line is the line in the destination file.
    ///
    /// If a line is added (-) then old_line is the line in the source file and new_line is 0.
    ///
    /// If a line is added ( ) then old_line is the line in the source file and new_line is the line in the destination file.
    fn add_line(&mut self, old_line: u32, new_line: u32, line: &[u8]);

    /// Close the diff: no more lines will be added
    fn close(&mut self);
}

/// A type to handle patch
pub trait Patch<D: Diff> {
    /// Create a new diff where lines will be added
    fn new_diff(&mut self) -> &mut D;

    /// Close the patch
    fn close(&mut self);
}

/// The different file operation
#[derive(Debug, PartialEq)]
pub enum FileOp {
    /// The file is new
    New,
    /// The file is deleted
    Deleted,
    /// The file is renamed
    Renamed,
    /// The file is copied
    Copied,
    /// The file is touched
    None,
}

/// Type to read a patch
pub struct PatchReader<'a> {
    buf: &'a [u8],
    pos: usize,
}

pub struct LineReader<'a> {
    pub buf: &'a [u8],
}

impl<'a> Debug for LineReader<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "buffer: {}", std::str::from_utf8(self.buf).unwrap())
    }
}

impl<'a> LineReader<'a> {
    fn is_binary(&self) -> bool {
        self.buf
            == [
                b'G', b'I', b'T', b' ', b'b', b'i', b'n', b'a', b'r', b'y', b' ', b'p', b'a', b't',
                b'c', b'h',
            ]
    }

    fn is_rename_from(&self) -> bool {
        self.buf.starts_with(&[
            b'r', b'e', b'n', b'a', b'm', b'e', b' ', b'f', b'r', b'o', b'm',
        ])
    }

    fn is_copy_from(&self) -> bool {
        self.buf.starts_with(&[
            b'c', b'o', b'p', b'y', b' ', b'f', b'r', b'o', b'm',
        ])
    }

    fn is_new_file(&self) -> bool {
        self.buf.starts_with(&[b'n', b'e', b'w', b' ', b'f', b'i', b'l', b'e'])
    }

    fn is_triple_minus(&self) -> bool {
        self.buf.starts_with(&[b'-', b'-', b'-'])
    }

    fn is_index(&self) -> bool {
        self.buf.starts_with(&[b'i', b'n', b'd', b'e', b'x', b' '])
    }

    fn is_deleted_file(&self) -> bool {
        self.buf.starts_with(&[
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
        } else if self.is_copy_from() {
            FileOp::Copied
        } else {
            FileOp::None
        }
    }

    pub fn parse_numbers(&self) -> (u32, u32) {
        // we know that line is beginning with "@@ -"
        let buf = unsafe { self.buf.get_unchecked(4..) };
        let mut x = 0;
        let mut y = 0;
        let mut iter = buf.iter();
        while let Some(c) = iter.next() {
            if *c >= b'0' && *c <= b'9' {
                x = x * 10 + u32::from(*c - b'0');
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
                y = y * 10 + u32::from(*c - b'0');
            } else {
                break;
            }
        }
        (x, y)
    }

    fn get_filename(buf: &[u8]) -> &str {
        let mut iter = buf.iter();
        let pos1 = iter.position(|c| *c != b' ').unwrap();
        let pos2 = iter.position(|c| *c == b'\t');
        let buf = if let Some(pos2) = pos2 {
            unsafe { buf.get_unchecked(pos1..=pos1 + pos2) }
        } else {
            unsafe { buf.get_unchecked(pos1..) }
        };
        let buf = if let Some(start) = buf.get(..2) {
            if start == [b'a', b'/'] || start == [b'b', b'/'] {
                unsafe { buf.get_unchecked(2..) }
            } else {
                buf
            }
        } else {
            buf
        };
        if buf == [b'/', b'd', b'e', b'v', b'/', b'n', b'u', b'l', b'l'] {
            ""
        } else {
            std::str::from_utf8(buf).unwrap()
        }
    }

    fn get_file<'b>(slice: Option<&'b [u8]>, starter: &[u8]) -> &'b [u8] {
        if let Some(path) = slice {
            if let Some(start) = path.get(..2) {
                if start == starter {
                    unsafe { path.get_unchecked(2..) }
                } else {
                    path
                }
            } else {
                path
            }
        } else {
            &[]
        }
    }

    fn parse_files(&self) -> (&str, &str) {
        // We know we start with 'diff '
        let buf = unsafe { self.buf.get_unchecked(5..) };
        let mut iter = buf.split(|c| *c == b' ');

        // skip --git or -r
        iter.next();

        let old_path = LineReader::get_file(iter.next(), &[b'a', b'/']);
        let new_path = LineReader::get_file(iter.next(), &[b'b', b'/']);

        (
            std::str::from_utf8(old_path).unwrap(),
            std::str::from_utf8(new_path).unwrap(),
        )
    }
}

impl<'a> PatchReader<'a> {
    /// Read a patch from the given path
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

    /// Read a patch from the given buffer
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

    fn parse_diff<D: Diff, P: Patch<D>>(&mut self, diff_line: &mut LineReader, patch: &mut P) {
        trace!("Diff {:?}", diff_line);
        let mut line = if let Some(line) = self.next(PatchReader::old_new_mode, false) {
            line
        } else {
            // Nothing more... so close it
            let (old, new) = diff_line.parse_files();

            trace!("Single diff line: new: {}", new);

            let diff = patch.new_diff();
            diff.set_info(old, new, FileOp::None, false);
            diff.close();
            return;
        };
        let op = line.get_file_op();

        trace!("Diff (op = {:?}): {:?}, next_line: {:?}", op, diff_line, line);

        if PatchReader::diff(&line) {
            let (old, new) = diff_line.parse_files();

            trace!("Single diff line: old: {} -- new: {}", old, new);

            let diff = patch.new_diff();
            diff.set_info(old, new, FileOp::None, false);
            diff.close();
            self.parse_diff(&mut line, patch);
            return;
        }

        if op == FileOp::Renamed || op == FileOp::Copied {
            // when no changes in the file there is no ---/+++ stuff
            // so need to get info here
            let (shift_1, shift_2) = if op == FileOp::Renamed {
                // 12 == len("rename from ")
                // 10 == len("rename to ")
                (12, 10)
            } else {
                // 10 == len("copy from ")
                // 8 == len("copy to ")
                (10, 8)
            };
            let old = LineReader::get_filename(unsafe { line.buf.get_unchecked(shift_1..) });
            let _line = self.next(PatchReader::mv, false).unwrap();
            let new = LineReader::get_filename(&_line.buf[shift_2..]);

            trace!("Copy/Renamed from {} to {}", old, new);

            let diff = patch.new_diff();
            diff.set_info(old, new, FileOp::Renamed, false);

            if let Some(mut _line) = self.next(PatchReader::mv, false) {
                if _line.is_triple_minus() {
                    // skip +++ line
                    self.next(PatchReader::mv, false);
                    line = self.next(PatchReader::mv, false).unwrap();
                    self.parse_hunks(&mut line, diff);
                    diff.close();
                } else {
                    // we just have a rename/copy but no changes in the file
                    diff.close();
                    if PatchReader::diff(&_line) {
                        self.parse_diff(&mut _line, patch);
                    }
                }
            }
            return;
        } else {
            if op == FileOp::New || op == FileOp::Deleted || line.is_index() {
                trace!("New/Delete file: {:?}", line);
                line = if let Some(line) = self.next(PatchReader::useful, false) {
                    line
                } else {
                    // Nothing more... so close it
                    let (old, new) = diff_line.parse_files();

                    trace!("Single new/delete diff line: new: {}", new);

                    let diff = patch.new_diff();
                    diff.set_info(old, new, op, false);
                    diff.close();
                    return;
                };
                trace!("New/Delete file: next useful line {:?}", line);
                if line.is_binary() {
                    // We've file info only in the diff line
                    // TODO: old is probably useless here
                    let (old, new) = diff_line.parse_files();

                    trace!("Binary file (op == {:?}): {}", op, new);

                    let diff = patch.new_diff();
                    diff.set_info(old, new, op, true);
                    diff.close();
                    self.skip_binary();
                    return;
                } else if PatchReader::diff(&line) {
                    let (old, new) = diff_line.parse_files();

                    trace!("Single new/delete diff line: new: {}", new);

                    let diff = patch.new_diff();
                    diff.set_info(old, new, op, false);
                    diff.close();
                    self.parse_diff(&mut line, patch);
                    return;
                }
            }

            if !line.is_triple_minus() {
                trace!("DEBUG (not a ---): {:?}", line);
                return;
            }

            trace!("DEBUG (---): {:?}", line);

            // here we've a ---
            let old = LineReader::get_filename(&line.buf[3..]);
            let _line = self.next(PatchReader::mv, false).unwrap();
            // 3 == len("+++")
            let new = LineReader::get_filename(&_line.buf[3..]);

            trace!("Files: old: {} -- new: {}", old, new);

            let diff = patch.new_diff();
            diff.set_info(old, new, op, false);
            line = self.next(PatchReader::mv, false).unwrap();
            self.parse_hunks(&mut line, diff);
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

    fn parse_hunk<D: Diff>(&mut self, o: u32, n: u32, diff: &mut D) {
        let mut old_count = o;
        let mut new_count = n;
        while let Some(line) = self.next(PatchReader::hunk_change, true) {
            // we know that line is beginning with -, +, ... so no need to check bounds
            let first = unsafe { *line.buf.get_unchecked(0) };
            match first {
                b'-' => {
                    diff.add_line(old_count, 0, unsafe { line.buf.get_unchecked(1..) });
                    old_count += 1;
                }
                b'+' => {
                    diff.add_line(0, new_count, unsafe { line.buf.get_unchecked(1..) });
                    new_count += 1;
                }
                b' ' => {
                    diff.add_line(old_count, new_count, unsafe { line.buf.get_unchecked(1..) });
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
        let mut pos = self.pos;
        if let Some(buf) = self.buf.get(self.pos..) {
            for (n, c) in buf.iter().enumerate() {
                if *c == b'\n' {
                    let mut npos = self.pos + n;
                    if npos > 0 {
                        let prev = unsafe { *self.buf.get_unchecked(npos - 1) };
                        if prev == b'\r' {
                            npos -= 1;
                        }
                    }
                    let line = LineReader {
                        buf: unsafe { self.buf.get_unchecked(pos..npos) },
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
        }
        None
    }

    fn skip_until_empty_line(&mut self) {
        let mut spos = 0;
        if let Some(buf) = self.buf.get(self.pos..) {
            for (n, c) in buf.iter().enumerate() {
                if *c == b'\n' {
                    if n == spos {
                        self.pos += n + 1;
                        return;
                    } else {
                        spos = n + 1;
                    }
                }
            }
        }
        self.pos = self.buf.len();
    }

    fn skip_binary(&mut self) {
        loop {
            self.skip_until_empty_line();
            if let Some(buf) = self.buf.get(self.pos..) {
                // 8 == len(literal )
                if let Some(buf) = buf.get(..8) {
                    if buf == [b'l', b'i', b't', b'e', b'r', b'a', b'l', b' '] {
                        continue;
                    }
                }
                // 6 == len(delta )
                if let Some(buf) = buf.get(..6) {
                    if buf == [b'd', b'e', b'l', b't', b'a', b' '] {
                        continue;
                    }
                }
            }
            break;
        }
    }

    fn useful(line: &LineReader) -> bool {
        line.is_binary() || line.is_triple_minus() || Self::diff(&line)
    }

    fn diff(line: &LineReader) -> bool {
        line.buf.starts_with(&[b'd', b'i', b'f', b'f', b' ', b'-'])
    }

    fn mv(_: &LineReader) -> bool {
        true
    }

    fn hunk_at(line: &LineReader) -> bool {
        line.buf.starts_with(&[b'@', b'@', b' ', b'-'])
    }

    fn old_new_mode(line: &LineReader) -> bool {
        !line.buf.starts_with(&[b'o', b'l', b'd', b' ']) && !line.buf.starts_with(&[b'n', b'e', b'w', b' ', b'm', b'o', b'd', b'e'])
    }

    fn hunk_change(line: &LineReader) -> bool {
        if let Some(c) = line.buf.get(0) {
            let c = *c;
            c == b'-' || c == b'+' || c == b' ' || line.buf.starts_with(&[
                b'\\', b' ', b'N', b'o', b' ', b'n', b'e', b'w', b'l', b'i', b'n', b'e'
            ])
        } else {
            false
        }
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
    fn test_get_filename() {
        let cases = [
            (" a/hello/world", "hello/world"),
            (" b/world/hello", "world/hello"),
            ("  a/hello/world", "hello/world"),
            ("   b/world/hello\t", "world/hello"),
            (" /dev/null\t", ""),
        ];
        for c in cases.into_iter() {
            let buf = c.0.as_bytes();
            let p = LineReader::get_filename(buf);
            assert!(p == c.1);
        }
    }

    #[test]
    fn test_line_starts_with() {
        let cases = [("+++ hello", "+++"), ("+++ hello", "++++")];
        for c in cases.into_iter() {
            let buf = c.0.as_bytes();
            let line = LineReader { buf: &buf };
            let pat = c.1.as_bytes();
            assert!(line.buf.starts_with(pat) == c.0.starts_with(c.1));
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
    fn test_skip_binary() {
        let s = vec![
            "abcdef",
            "ghijkl",
            "",
            "literal 1",
            "abcdef",
            "ghijkl",
            "",
            "delta 1",
            "abcdef",
            "ghijkl",
            "",
            "Hello",
        ];
        let s = s.join("\n");
        let hpos = s.find('H').unwrap();
        let mut p = PatchReader {
            buf: s.as_bytes(),
            pos: 0,
        };
        p.skip_binary();
        assert!(p.pos == hpos);
    }

    #[test]
    fn test_parse_files() {
        let diffs = vec![
            (
                "diff --git a/foo/bar.cpp b/Foo/Bar/bar.cpp",
                ("foo/bar.cpp", "Foo/Bar/bar.cpp"),
            ),
            (
                "diff -r a/foo/bar.cpp b/Foo/Bar/bar.cpp",
                ("foo/bar.cpp", "Foo/Bar/bar.cpp"),
            ),
            (
                "diff --git foo/bar.cpp Foo/Bar/bar.cpp",
                ("foo/bar.cpp", "Foo/Bar/bar.cpp"),
            ),
        ];
        for s in diffs {
            let line = LineReader {
                buf: s.0.as_bytes(),
            };
            let (old, new) = line.parse_files();
            assert!(old == (s.1).0);
            assert!(new == (s.1).1);
        }
    }
}
