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

#[derive(Deserialize)]
pub struct DiffImpl {
    pub filename: String,
    pub new: bool,
    pub deleted: bool,
    pub binary: bool,
    pub copied_from: Option<String>,
    pub renamed_from: Option<String>,
    pub lines: Vec<LineChange>,
}

#[derive(Deserialize)]
struct PatchImpl {
    diffs: Vec<DiffImpl>,
}

impl Debug for LineChange {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "line: {}, deleted: {}, data: {}", self.line, self.deleted, self.data)
    }
}

impl Debug for DiffImpl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "filename: {}", self.filename)?;
        writeln!(f, "new: {}, deleted: {}, binary: {}", self.new, self.deleted, self.binary)?;
        writeln!(f, "copied_from: {:?}, renamed_from: {:?}", self.copied_from, self.renamed_from)?;
        for line in self.lines.iter() {
            writeln!(f, " - {:?}", *line)?;
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
            lines: Vec::new(),
        });
        self.diffs.last_mut().unwrap()
    }

    fn close(&mut self) {}
}

impl Diff for DiffImpl {
    fn set_info(&mut self, old_name: &str, new_name: &str, op: FileOp, binary: bool) {
        match op {
            FileOp::New => {
                self.new = true;
                self.filename = new_name.to_string();
            }
            FileOp::Deleted => {
                self.deleted = true;
                self.filename = old_name.to_string();
            }
            FileOp::Renamed => {
                self.filename = new_name.to_string();
                self.renamed_from = Some(old_name.to_string());
            }
            _ => {
                self.filename = new_name.to_string();
            }
        }
        self.binary = binary;
    }

    fn add_line(&mut self, old_line: u32, new_line: u32, line: &[u8]) {
        if old_line == 0 {
            self.lines.push(LineChange {
                line: new_line,
                deleted: false,
                data: get_line(line),
            });
        } else if new_line == 0 {
            self.lines.push(LineChange {
                line: old_line,
                deleted: true,
                data: get_line(line),
            });
        }
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
    patch.diffs.retain(|c| !c.binary);
    assert!(
        json.diffs.len() == patch.diffs.len(),
        "Not the same length in patch: {:?}",
        path
    );
    for (cj, cp) in json.diffs.iter().zip(patch.diffs.iter()) {
        assert!(
            cj.filename == cp.filename,
            format!(
                "Not the same filename: {} ({} expected)",
                cp.filename, cj.filename
            )
        );
        assert!(
            cj.copied_from == cp.renamed_from || cj.copied_from == cp.copied_from,
            format!(
                "Not the same filename: {:?} ({:?} expected)",
                cj.filename, cp.filename
            )
        );
        assert!(
            cj.lines.len() == cp.lines.len(),
            format!(
                "Not the same length for changed lines: {} ({} expected)",
                cp.lines.len(),
                cj.lines.len()
            )
        );
        for (lj, lp) in cj.lines.iter().zip(cp.lines.iter()) {
            assert!(
                lj == lp,
                format!("Not the same line change: {:?} ({:?} expected", lp, lj)
            );
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
            let file = File::open(&path).unwrap();
            let reader = BufReader::new(file);
            let json_patch = serde_json::from_reader::<_, PatchImpl>(reader).unwrap();

            let filename = path.with_extension("patch");
            let filename = filename.file_name().unwrap();
            let path = PathBuf::from("./tests/patches/").join(filename);

            println!("Parse patch {:?}", path);

            let mut patch = PatchImpl { diffs: Vec::new() };
            PatchReader::by_path(&path, &mut patch);

            if path.ends_with(PathBuf::from(".patch")) {
                println!("Patch:\n{:?}", patch);
            }

            compare(path, &json_patch, &mut patch);
        }
    }
}
