use std::fmt::{self, Debug, Display, Formatter};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::result;

#[derive(Debug)]
pub enum ParsepatchError {
    InvalidHunkHeader(usize),
    NewModeExpected(usize),
    NoFilename(usize),
    IOError(String),
    InvalidString(usize),
}

pub type Result<T> = result::Result<T, ParsepatchError>;

impl Display for ParsepatchError {
    fn fmt(&self, f: &mut Formatter) -> result::Result<(), fmt::Error> {
        match self {
            ParsepatchError::InvalidHunkHeader(n) => {
                writeln!(f, "Invalid hunk header at line {}", n)
            }
            ParsepatchError::NewModeExpected(n) => {
                writeln!(f, "New mode expected after old mode at line {}", n)
            }
            ParsepatchError::NoFilename(n) => writeln!(f, "Cannot get filename at line {}", n),
            ParsepatchError::IOError(s) => writeln!(f, "Cannot read the file {}", s),
            ParsepatchError::InvalidString(n) => writeln!(f, "Invalid utf-8 at line {}", n),
        }
    }
}

/// A type to handle lines in a diff
pub trait Diff {
    /// Set the file info
    fn set_info(
        &mut self,
        old_name: &str,
        new_name: &str,
        op: FileOp,
        binary_sizes: Option<Vec<BinaryHunk>>,
        file_mode: Option<FileMode>,
    );

    /// Add a line in the diff
    ///
    /// If a line is added (+) then old_line is 0 and new_line is the line in the destination file.
    ///
    /// If a line is removed (-) then old_line is the line in the source file and new_line is 0.
    ///
    /// If a line is nothing ( ) then old_line is the line in the source file and new_line is the line in the destination file.
    fn add_line(&mut self, old_line: u32, new_line: u32, line: &[u8]);

    /// A new hunk is created
    fn new_hunk(&mut self);

    /// Close the diff: no more lines will be added
    fn close(&mut self);
}

/// An enum containing the size of binary hunks
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryHunk {
    Literal(usize),
    Delta(usize),
}

/// File mode change
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FileMode {
    pub old: u32,
    pub new: u32,
}

/// A type to handle patch
pub trait Patch<D: Diff> {
    /// Create a new diff where lines will be added
    fn new_diff(&mut self) -> &mut D;

    /// Close the patch
    fn close(&mut self);
}

/// The different file operation
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FileOp {
    /// The file is new with its mode
    New(u32),
    /// The file is deleted with its mode
    Deleted(u32),
    /// The file is renamed
    Renamed,
    /// The file is copied
    Copied,
    /// The file is touched
    None,
}

impl FileOp {
    pub fn is_new_or_deleted(self) -> bool {
        match self {
            FileOp::New(_) => true,
            FileOp::Deleted(_) => true,
            _ => false,
        }
    }
}

/// Type to read a patch
pub struct PatchReader<'a> {
    buf: &'a [u8],
    pos: usize,
    line: usize,
    last: Option<LineReader<'a>>,
}

pub struct LineReader<'a> {
    pub buf: &'a [u8],
    line: usize,
}

impl<'a> Debug for LineReader<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "buffer: !{}!", std::str::from_utf8(self.buf).unwrap())
    }
}

impl<'a> LineReader<'a> {
    fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    fn is_binary(&self) -> bool {
        self.buf == b"GIT binary patch"
    }

    fn is_rename_from(&self) -> bool {
        self.buf.starts_with(b"rename from")
    }

    fn is_copy_from(&self) -> bool {
        self.buf.starts_with(b"copy from")
    }

    fn is_new_file(&self) -> bool {
        self.buf.starts_with(b"new file")
    }

    fn is_triple_minus(&self) -> bool {
        self.buf.starts_with(b"--- ")
    }

    fn is_triple_plus(&self) -> bool {
        self.buf.starts_with(b"+++ ")
    }

    fn is_index(&self) -> bool {
        self.buf.starts_with(b"index ")
    }

    fn is_deleted_file(&self) -> bool {
        self.buf.starts_with(b"deleted file")
    }

    fn get_line(&self) -> usize {
        self.line
    }

    fn get_file_op(&self) -> FileOp {
        if self.is_new_file() {
            FileOp::New(self.parse_mode("new file mode "))
        } else if self.is_deleted_file() {
            FileOp::Deleted(self.parse_mode("deleted file mode "))
        } else if self.is_rename_from() {
            FileOp::Renamed
        } else if self.is_copy_from() {
            FileOp::Copied
        } else {
            FileOp::None
        }
    }

    pub fn parse_numbers(&self) -> Result<(u32, u32, u32, u32)> {
        // we know that line is beginning with "@@ -"
        let buf = unsafe { self.buf.get_unchecked(4..) };

        // NUMS_PAT = re.compile(r'^@@ -([0-9]+),?([0-9]+)? \+([0-9]+),?([0-9]+)? @@')
        let iter = &mut buf.iter();
        let old_start = iter
            .take_while(|&&c| c >= b'0' && c <= b'9')
            .fold(0, |r, &c| r * 10 + u32::from(c - b'0'));

        let c = *iter
            .next()
            .ok_or_else(|| ParsepatchError::InvalidHunkHeader(self.get_line()))?;
        let old_lines = if c >= b'0' && c <= b'9' {
            iter.take_while(|&&c| c >= b'0' && c <= b'9')
                .fold(u32::from(c - b'0'), |r, &c| r * 10 + u32::from(c - b'0'))
        } else {
            1
        };

        if c != b'+' {
            iter.find(|&&c| c == b'+');
        }

        let new_start = iter
            .take_while(|&&c| c >= b'0' && c <= b'9')
            .fold(0, |r, &c| r * 10 + u32::from(c - b'0'));

        let c = *iter
            .next()
            .ok_or_else(|| ParsepatchError::InvalidHunkHeader(self.get_line()))?;
        let new_lines = if c >= b'0' && c <= b'9' {
            iter.take_while(|&&c| c >= b'0' && c <= b'9')
                .fold(u32::from(c - b'0'), |r, &c| r * 10 + u32::from(c - b'0'))
        } else {
            1
        };

        Ok((old_start, old_lines, new_start, new_lines))
    }

    fn get_filename(buf: &[u8], line: usize) -> Result<&str> {
        let mut iter = buf.iter();
        let pos1 = iter
            .position(|c| *c != b' ')
            .ok_or_else(|| ParsepatchError::NoFilename(line))?;
        let pos2 = iter.position(|c| *c == b'\t');
        let buf = if let Some(pos2) = pos2 {
            unsafe { buf.get_unchecked(pos1..=pos1 + pos2) }
        } else {
            unsafe { buf.get_unchecked(pos1..) }
        };
        let buf = if let Some(start) = buf.get(..2) {
            if start == b"a/" || start == b"b/" {
                unsafe { buf.get_unchecked(2..) }
            } else {
                buf
            }
        } else {
            buf
        };

        if buf == b"/dev/null" {
            Ok("")
        } else {
            std::str::from_utf8(buf).map_err(|_| ParsepatchError::InvalidString(line))
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

    fn parse_files(&self) -> Result<(&str, &str)> {
        // We know we start with 'diff '
        let buf = unsafe { self.buf.get_unchecked(5..) };
        let mut iter = buf.split(|c| *c == b' ');

        // skip --git or -r
        iter.next();

        let old_path = LineReader::get_file(iter.next(), b"a/");
        let new_path = LineReader::get_file(iter.next(), b"b/");

        let old = std::str::from_utf8(old_path)
            .map_err(|_| ParsepatchError::InvalidString(self.get_line()))?;
        let new = std::str::from_utf8(new_path)
            .map_err(|_| ParsepatchError::InvalidString(self.get_line()))?;

        Ok((old, new))
    }

    fn parse_mode(&self, start: &'static str) -> u32 {
        // we know that line is beginning with "old/new mode "
        // the following number is an octal number with 6 digits (so max is 8^6 - 1)
        let buf = unsafe { self.buf.get_unchecked(start.len()..) };
        buf.iter()
            .take_while(|&&c| c >= b'0' && c <= b'7')
            .fold(0, |r, &c| r * 8 + u32::from(c - b'0'))
    }
}

impl<'a> PatchReader<'a> {
    /// Read a patch from the given path
    pub fn by_path<D: Diff, P: Patch<D>>(path: &PathBuf, patch: &mut P) -> Result<()> {
        match File::open(path) {
            Ok(mut reader) => {
                let mut data = Vec::new();
                reader
                    .read_to_end(&mut data)
                    .map_err(|_| ParsepatchError::IOError(format!("{:?}", path)))?;
                PatchReader::by_buf(&data, patch)
            }
            _ => Err(ParsepatchError::IOError(format!("{:?}", path))),
        }
    }

    /// Read a patch from the given buffer
    pub fn by_buf<D: Diff, P: Patch<D>>(buf: &[u8], patch: &mut P) -> Result<()> {
        let mut p = PatchReader {
            buf,
            pos: 0,
            line: 1,
            last: None,
        };
        p.parse(patch)
    }

    fn get_line(&self) -> usize {
        self.line
    }

    fn parse<D: Diff, P: Patch<D>>(&mut self, patch: &mut P) -> Result<()> {
        while let Some(mut line) = self.next(PatchReader::starter, false) {
            self.parse_diff(&mut line, patch)?;
        }
        patch.close();

        Ok(())
    }

    fn parse_diff<D: Diff, P: Patch<D>>(
        &mut self,
        diff_line: &mut LineReader,
        patch: &mut P,
    ) -> Result<()> {
        trace!("Diff {:?}", diff_line);
        if diff_line.is_triple_minus() {
            // The diff starts with a ---: need to look ahead for no "diff ..."
            // to be sure that we aren't in the header.
            let diff = b"\ndiff -";
            let buf = unsafe { self.buf.get_unchecked(self.pos..) };
            if let Some(pos) = buf.windows(diff.len()).position(|win| win == diff) {
                // +1 for the '\n'
                self.pos += pos + 1;
                return Ok(());
            }
            self.parse_minus(diff_line, FileOp::None, None, patch)?;
            return Ok(());
        }

        let mut line = if let Some(line) = self.next(PatchReader::mv, false) {
            line
        } else {
            // Nothing more... so close it
            let (old, new) = diff_line.parse_files()?;

            trace!("Single diff line: new: {}", new);

            let diff = patch.new_diff();
            diff.set_info(old, new, FileOp::None, None, None);
            diff.close();
            return Ok(());
        };

        let file_mode = if PatchReader::old_mode(&line) {
            let old = line.parse_mode("old mode ");
            let l = self
                .next(PatchReader::mv, false)
                .ok_or_else(|| ParsepatchError::NewModeExpected(self.get_line()))?;
            let new = l.parse_mode("new mode ");
            let file_mode = FileMode { old, new };
            if let Some(l) = self.next(PatchReader::mv, false) {
                line = l;
                Some(file_mode)
            } else {
                // Nothing more... so close it
                let (old, new) = diff_line.parse_files()?;

                trace!("Single diff line (mode change): new: {}", new);

                let diff = patch.new_diff();
                diff.set_info(old, new, FileOp::None, None, Some(file_mode));
                diff.close();
                return Ok(());
            }
        } else {
            None
        };

        let op = line.get_file_op();

        trace!(
            "Diff (op = {:?}): {:?}, next_line: {:?}",
            op,
            diff_line,
            line
        );

        if PatchReader::diff(&line) || line.is_empty() {
            let (old, new) = diff_line.parse_files()?;

            trace!("Single diff line: old: {} -- new: {}", old, new);

            let diff = patch.new_diff();
            diff.set_info(old, new, FileOp::None, None, file_mode);
            diff.close();
            self.set_last(line);
            return Ok(());
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
            let old = LineReader::get_filename(
                unsafe { line.buf.get_unchecked(shift_1..) },
                line.get_line(),
            )?;
            let _line = self
                .next(PatchReader::mv, false)
                .ok_or_else(|| ParsepatchError::InvalidHunkHeader(self.get_line()))?;
            let new = LineReader::get_filename(&_line.buf[shift_2..], line.get_line())?;

            trace!("Copy/Renamed from {} to {}", old, new);

            let diff = patch.new_diff();
            diff.set_info(old, new, op, None, file_mode);

            if let Some(mut _line) = self.next(PatchReader::mv, false) {
                if _line.is_triple_minus() {
                    // skip +++ line
                    self.next(PatchReader::mv, false);
                    line = self
                        .next(PatchReader::mv, false)
                        .ok_or_else(|| ParsepatchError::InvalidHunkHeader(self.get_line()))?;
                    self.parse_hunks(&mut line, diff)?;
                    diff.close();
                } else {
                    // we just have a rename/copy but no changes in the file
                    diff.close();
                    self.set_last(_line);
                }
            }
        } else {
            if op.is_new_or_deleted() || line.is_index() {
                trace!("New/Delete file: {:?}", line);
                line = if let Some(line) = self.next(PatchReader::useful, false) {
                    line
                } else {
                    // Nothing more... so close it
                    let (old, new) = diff_line.parse_files()?;

                    trace!("Single new/delete diff line: new: {}", new);

                    let diff = patch.new_diff();
                    diff.set_info(old, new, op, None, file_mode);
                    diff.close();
                    return Ok(());
                };
                trace!("New/Delete file: next useful line {:?}", line);
                if line.is_binary() {
                    // We've file info only in the diff line
                    // TODO: old is probably useless here
                    let (old, new) = diff_line.parse_files()?;

                    trace!("Binary file (op == {:?}): {}", op, new);

                    let diff = patch.new_diff();
                    let sizes = self.skip_binary();

                    diff.set_info(old, new, op, Some(sizes), file_mode);
                    diff.close();
                    return Ok(());
                } else if PatchReader::diff(&line) {
                    let (old, new) = diff_line.parse_files()?;

                    trace!("Single new/delete diff line: new: {}", new);

                    let diff = patch.new_diff();
                    diff.set_info(old, new, op, None, file_mode);
                    diff.close();
                    self.set_last(line);
                    return Ok(());
                }
            }

            if line.is_triple_minus() {
                self.parse_minus(&mut line, op, file_mode, patch)?;
            }
        }

        Ok(())
    }

    fn parse_minus<D: Diff, P: Patch<D>>(
        &mut self,
        line: &mut LineReader,
        op: FileOp,
        file_mode: Option<FileMode>,
        patch: &mut P,
    ) -> Result<()> {
        trace!("DEBUG (---): {:?}", line);

        // here we've a ---
        let old = LineReader::get_filename(&line.buf[3..], line.get_line())?;
        let _line = self
            .next(PatchReader::mv, false)
            .ok_or_else(|| ParsepatchError::InvalidHunkHeader(self.get_line()))?;

        if !_line.is_triple_plus() {
            trace!("DEBUG (not a +++): {:?}", _line);
            return Ok(());
        }
        // 3 == len("+++")
        let new = LineReader::get_filename(&_line.buf[3..], line.get_line())?;

        trace!("Files: old: {} -- new: {}", old, new);

        let diff = patch.new_diff();
        diff.set_info(old, new, op, None, file_mode);
        let mut line = self
            .next(PatchReader::mv, false)
            .ok_or_else(|| ParsepatchError::InvalidHunkHeader(self.get_line()))?;
        self.parse_hunks(&mut line, diff)?;
        diff.close();

        Ok(())
    }

    fn parse_hunks<D: Diff>(&mut self, line: &mut LineReader, diff: &mut D) -> Result<()> {
        let (o1, o2, n1, n2) = line.parse_numbers()?;
        self.parse_hunk(o1, o2, n1, n2, diff);
        while let Some(line) = self.next(PatchReader::hunk_at, true) {
            let (o1, o2, n1, n2) = line.parse_numbers()?;
            self.parse_hunk(o1, o2, n1, n2, diff);
        }

        Ok(())
    }

    fn parse_hunk<D: Diff>(
        &mut self,
        mut old_count: u32,
        mut old_lines: u32,
        mut new_count: u32,
        mut new_lines: u32,
        diff: &mut D,
    ) {
        diff.new_hunk();

        while let Some(line) = self.next(PatchReader::hunk_change, true) {
            // we know that line is beginning with -, +, ... so no need to check bounds
            let first = unsafe { *line.buf.get_unchecked(0) };
            match first {
                b'-' => {
                    diff.add_line(old_count, 0, unsafe { line.buf.get_unchecked(1..) });
                    old_count += 1;
                    old_lines -= 1;
                }
                b'+' => {
                    diff.add_line(0, new_count, unsafe { line.buf.get_unchecked(1..) });
                    new_count += 1;
                    new_lines -= 1;
                }
                b' ' => {
                    diff.add_line(old_count, new_count, unsafe { line.buf.get_unchecked(1..) });
                    old_count += 1;
                    new_count += 1;
                    old_lines -= 1;
                    new_lines -= 1;
                }
                _ => {}
            }
            if old_lines == 0 && new_lines == 0 {
                break;
            }
        }
    }

    fn set_last(&mut self, line: LineReader<'a>) {
        self.last = Some(line);
    }

    fn next<F>(&mut self, filter: F, return_on_false: bool) -> Option<LineReader<'a>>
    where
        F: Fn(&LineReader) -> bool,
    {
        let mut l = None;
        std::mem::swap(&mut l, &mut self.last);

        if let Some(line) = l {
            if filter(&line) {
                return Some(line);
            } else if return_on_false {
                return None;
            }
        }

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
                        line: self.line,
                    };
                    self.line += 1;
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
                    self.line += 1;
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

    fn parse_usize(buf: &[u8]) -> usize {
        buf.iter()
            .take_while(|&&c| c >= b'0' && c <= b'9')
            .fold(0, |r, &c| r * 10 + usize::from(c - b'0'))
    }

    fn skip_binary(&mut self) -> Vec<BinaryHunk> {
        let mut sizes = Vec::new();
        while let Some(buf) = self.buf.get(self.pos..) {
            if buf.starts_with(b"literal ") {
                self.pos += 8;
                let buf = unsafe { self.buf.get_unchecked(self.pos..) };
                sizes.push(BinaryHunk::Literal(Self::parse_usize(buf)));
            } else if buf.starts_with(b"delta ") {
                self.pos += 6;
                let buf = unsafe { self.buf.get_unchecked(self.pos..) };
                sizes.push(BinaryHunk::Delta(Self::parse_usize(buf)));
            } else {
                break;
            }
            self.skip_until_empty_line();
        }
        sizes
    }

    fn useful(line: &LineReader) -> bool {
        line.is_binary() || line.is_triple_minus() || Self::diff(&line)
    }

    fn starter(line: &LineReader) -> bool {
        line.is_triple_minus() || Self::diff(&line)
    }

    fn diff(line: &LineReader) -> bool {
        line.buf.starts_with(b"diff -")
    }

    fn mv(_: &LineReader) -> bool {
        true
    }

    fn hunk_at(line: &LineReader) -> bool {
        line.buf.starts_with(b"@@ -")
    }

    fn old_mode(line: &LineReader) -> bool {
        line.buf.starts_with(b"old mode ")
    }

    fn hunk_change(line: &LineReader) -> bool {
        if let Some(c) = line.buf.get(0) {
            let c = *c;
            c == b'-' || c == b'+' || c == b' ' || line.buf.starts_with(b"\\ No newline")
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
            ("@@ -123,456 +789,101112 @@", (123, 456, 789, 101112)),
            ("@@ -123 +789,101112 @@", (123, 1, 789, 101112)),
            ("@@ -123,456 +789 @@", (123, 456, 789, 1)),
            ("@@ -123 +789 @@", (123, 1, 789, 1)),
        ];
        for c in cases.iter() {
            let buf = c.0.as_bytes();
            let line = LineReader { buf: &buf, line: 1 };
            assert_eq!(line.parse_numbers().unwrap(), c.1);
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
        for c in cases.iter() {
            let buf = c.0.as_bytes();
            let p = LineReader::get_filename(buf, 0).unwrap();
            assert!(p == c.1);
        }
    }

    #[test]
    fn test_line_starts_with() {
        let cases = [("+++ hello", "+++"), ("+++ hello", "++++")];
        for c in cases.iter() {
            let buf = c.0.as_bytes();
            let line = LineReader { buf: &buf, line: 1 };
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
            line: 1,
            last: None,
        };
        p.skip_until_empty_line();
        assert!(p.pos == cpos);
    }

    #[test]
    fn test_skip_binary() {
        let s = vec![
            "literal 1",
            "abcdef",
            "ghijkl",
            "",
            "delta 2",
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
            line: 1,
            last: None,
        };
        let sizes = p.skip_binary();
        assert_eq!(p.pos, hpos);
        assert_eq!(sizes, vec![BinaryHunk::Literal(1), BinaryHunk::Delta(2)]);
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
                line: 1,
            };
            let (old, new) = line.parse_files().unwrap();
            assert!(old == (s.1).0);
            assert!(new == (s.1).1);
        }
    }
}
