extern crate chardet;
extern crate encoding;
extern crate parsepatch;
extern crate serde;

#[macro_use]
extern crate serde_derive;

extern crate serde_json;

use chardet::{charset2encoding, detect};
use encoding::all::UTF_8;
use encoding::label::encoding_from_whatwg_label;
use encoding::{DecoderTrap, Encoding};
use std::fs::{self, File};
use std::io::BufReader;
use std::path::PathBuf;

use crate::parsepatch::*;

#[derive(Deserialize, Debug, PartialEq)]
pub struct LineChange {
    pub line: u64,
    pub deleted: bool,
    pub data: String,
}

#[derive(Deserialize, Debug)]
pub struct DiffImpl {
    pub filename: String,
    pub new: bool,
    pub deleted: bool,
    pub binary: bool,
    pub renamed_from: Option<String>,
    pub lines: Vec<LineChange>,
}

#[derive(Deserialize, Debug)]
struct PatchImpl {
    diffs: Vec<DiffImpl>,
}

impl Patch<DiffImpl> for PatchImpl {
    fn new_diff(&mut self) -> &mut DiffImpl {
        self.diffs.push(DiffImpl {
            filename: "".to_string(),
            new: false,
            deleted: false,
            binary: false,
            renamed_from: None,
            lines: Vec::new(),
        });
        self.diffs.last_mut().unwrap()
    }

    fn close(&mut self) {}
}

impl Diff for DiffImpl {
    fn set_info(&mut self, old_name: &str, new_name: &str, op: FileOp, binary: bool) {
        self.filename = new_name.to_string();
        match op {
            FileOp::New => {
                self.new = true;
            }
            FileOp::Deleted => {
                self.deleted = true;
            }
            FileOp::Renamed => {
                self.renamed_from = Some(old_name.to_string());
            }
            _ => {}
        }
        self.binary = binary;
    }

    fn add_line(&mut self, old_line: u64, new_line: u64, line: &[u8]) {
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

fn compare(json: &PatchImpl, patch: &mut PatchImpl) {
    patch.diffs.retain(|c| !c.binary);
    assert!(json.diffs.len() == patch.diffs.len(), "Not the same length");
    for (cj, cp) in json.diffs.iter().zip(patch.diffs.iter()) {
        assert!(
            cj.filename == cp.filename,
            format!(
                "Not the same filename: {} ({} expected)",
                cp.filename, cj.filename
            )
        );
        assert!(
            cj.renamed_from == cp.renamed_from,
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
            let mut patch = PatchImpl { diffs: Vec::new() };
            PatchReader::by_path(&path, &mut patch);

            compare(&json_patch, &mut patch);
        }
    }
}
