extern crate chardet;
extern crate encoding;
extern crate env_logger;
extern crate parsepatch;
extern crate serde;

#[macro_use]
extern crate serde_derive;

extern crate serde_json;

use chardet::{charset2encoding, detect};
use encoding::all::UTF_8;
use encoding::label::encoding_from_whatwg_label;
use encoding::{DecoderTrap, Encoding};
use std::fmt::{Debug, Formatter, Result};
use std::fs::{self, File};
use std::io::BufReader;
use std::path::PathBuf;

use crate::parsepatch::*;

#[derive(Deserialize, PartialEq)]
pub struct LineChange {
    pub line: u32,
    pub deleted: bool,
    pub data: String,
}

#[derive(Default, Deserialize, PartialEq)]
pub struct Hunk {
    pub lines: Vec<LineChange>,
}

#[derive(Default, Deserialize, PartialEq)]
pub struct FileModeChange {
    pub old: u32,
    pub new: u32,
}

#[derive(Deserialize)]
pub struct DiffImpl {
    pub filename: String,
    pub new: bool,
    pub deleted: bool,
    pub binary: bool,
    pub copied_from: Option<String>,
    pub renamed_from: Option<String>,
    pub hunks: Vec<Hunk>,
    pub file_mode: Option<FileModeChange>,
}

#[derive(Deserialize)]
struct PatchImpl {
    diffs: Vec<DiffImpl>,
}

impl Debug for LineChange {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "line: {}, deleted: {}, data: {}",
            self.line, self.deleted, self.data
        )
    }
}

impl Debug for DiffImpl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "filename: {}", self.filename)?;
        writeln!(
            f,
            "new: {}, deleted: {}, binary: {}",
            self.new, self.deleted, self.binary
        )?;
        writeln!(
            f,
            "copied_from: {:?}, renamed_from: {:?}",
            self.copied_from, self.renamed_from
        )?;
        if let Some(file_mode) = &self.file_mode {
            writeln!(
                f,
                "old mode: {:06o}, new mode: {:06o}",
                file_mode.old, file_mode.new
            )?;
        }
        for hunk in self.hunks.iter() {
            writeln!(f, " @@")?;
            for line in hunk.lines.iter() {
                writeln!(f, " - {:?}", *line)?;
            }
        }
        Ok(())
    }
}

impl Debug for PatchImpl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for diff in self.diffs.iter() {
            writeln!(f, "Diff:\n{:?}", *diff)?;
        }
        Ok(())
    }
}

impl Patch<DiffImpl> for PatchImpl {
    fn new_diff(&mut self) -> &mut DiffImpl {
        self.diffs.push(DiffImpl {
            filename: "".to_string(),
            new: false,
            deleted: false,
            binary: false,
            copied_from: None,
            renamed_from: None,
            hunks: Vec::new(),
            file_mode: None,
        });
        self.diffs.last_mut().unwrap()
    }

    fn close(&mut self) {}
}

impl Diff for DiffImpl {
    fn set_info(
        &mut self,
        old_name: &str,
        new_name: &str,
        op: FileOp,
        binary_sizes: Option<Vec<BinaryHunk>>,
        file_mode: Option<FileMode>,
    ) {
        match op {
            FileOp::New(m) => {
                self.new = true;
                self.filename = new_name.to_string();
                self.file_mode = Some(FileModeChange { old: 0, new: m });
            }
            FileOp::Deleted(m) => {
                self.deleted = true;
                self.filename = old_name.to_string();
                self.file_mode = Some(FileModeChange { old: m, new: 0 });
            }
            FileOp::Renamed => {
                self.filename = new_name.to_string();
                self.renamed_from = Some(old_name.to_string());
            }
            _ => {
                self.filename = new_name.to_string();
            }
        }
        self.binary = binary_sizes.is_some();

        if self.file_mode.is_none() {
            self.file_mode = file_mode.map(|c| FileModeChange {
                    old: c.old,
                    new: c.new,
                });
        }
    }

    fn add_line(&mut self, old_line: u32, new_line: u32, line: &[u8]) {
        if old_line == 0 {
            self.hunks.last_mut().unwrap().lines.push(LineChange {
                line: new_line,
                deleted: false,
                data: get_line(line),
            });
        } else if new_line == 0 {
            self.hunks.last_mut().unwrap().lines.push(LineChange {
                line: old_line,
                deleted: true,
                data: get_line(line),
            });
        }
    }

    fn new_hunk(&mut self) {
        self.hunks.push(Hunk::default());
    }

    fn close(&mut self) {}
}

fn get_line(buf: &[u8]) -> String {
    match std::str::from_utf8(buf) {
        Ok(s) => s.to_string(),
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

fn compare(path: PathBuf, json: &PatchImpl, patch: &mut PatchImpl) {
    if path == PathBuf::from("./tests/patches/6f5fc0d644dd.patch") {
        let modes = patch.diffs[0].file_mode.as_ref().unwrap();
        assert_eq!(modes.old, 0o100644);
        assert_eq!(modes.new, 0o100755);
    }

    if path == PathBuf::from("./tests/patches/d8571fb523a0.patch") {
        let modes = patch.diffs[1].file_mode.as_ref().unwrap();
        assert_eq!(modes.new, 0o100644);
        assert_eq!(modes.old, 0);
    }

    if path == PathBuf::from("./tests/patches/d7a700707ddb.patch") {
        eprintln!("{:?}", patch);
        for diff in patch.diffs.iter() {
            if diff.filename == "layout/style/test/test_overscroll_behavior_pref.html" {
                let modes = diff.file_mode.as_ref().unwrap();
                assert_eq!(modes.new, 0);
                assert_eq!(modes.old, 0o100644);
            }
        }
    }

    //eprintln!("{:?}", patch);

    assert!(
        json.diffs.len() == patch.diffs.len(),
        "Not the same length in patch {:?}: {} ({} expected)",
        path,
        patch.diffs.len(),
        json.diffs.len(),
    );
    for (cj, cp) in json.diffs.iter().zip(patch.diffs.iter()) {
        assert!(
            cj.filename == cp.filename,
            "Not the same filename: {} ({} expected)",
            cp.filename,
            cj.filename
        );
        assert!(
            cj.copied_from == cp.renamed_from || cj.copied_from == cp.copied_from,
            "Not renamed/copied (in {}): {:?} {:?} {:?}",
            cp.filename,
            cj.copied_from,
            cp.renamed_from,
            cp.copied_from
        );
        assert!(
            cj.hunks.len() == cp.hunks.len(),
            "Not the same length for hunks: {} ({} expected)",
            cp.hunks.len(),
            cj.hunks.len()
        );
        for (hj, hp) in cj.hunks.iter().zip(cp.hunks.iter()) {
            assert!(
                hj.lines.len() == hp.lines.len(),
                "Not the same length for changed lines: {} ({} expected)",
                hp.lines.len(),
                hj.lines.len()
            );
            for (lj, lp) in hj.lines.iter().zip(hp.lines.iter()) {
                assert!(
                    lj == lp,
                    "Not the same line change: {:?} ({:?} expected",
                    lp,
                    lj
                );
            }
        }
    }
}

#[test]
fn test_parse() {
    env_logger::init();
    for entry in fs::read_dir(PathBuf::from("./tests/output")).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if !path.is_dir() && path.extension().unwrap() == "json" {
            /*if path != PathBuf::from("./tests/output/b8802b591ce2.json") {
                continue;
            }*/
            let file = File::open(&path).unwrap();
            let reader = BufReader::new(file);
            let json_patch = serde_json::from_reader::<_, PatchImpl>(reader).unwrap();

            let filename = path.with_extension("patch");
            let filename = filename.file_name().unwrap();
            let path = PathBuf::from("./tests/patches/").join(filename);

            println!("Parse patch {:?}", path);

            let mut patch = PatchImpl { diffs: Vec::new() };
            PatchReader::by_path(&path, &mut patch).unwrap();

            if path.ends_with(PathBuf::from(".patch")) {
                println!("Patch:\n{:?}", patch);
            }

            compare(path, &json_patch, &mut patch);
        }
    }
}
