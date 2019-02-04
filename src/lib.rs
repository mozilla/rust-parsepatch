extern crate chardet;
extern crate encoding;
extern crate serde;

#[macro_use]
extern crate serde_derive;

extern crate serde_json;

pub mod parser;
pub use self::parser::*;
