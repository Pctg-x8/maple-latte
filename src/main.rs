extern crate mapleparser;
extern crate rustc_serialize;

use rustc_serialize::*;

fn main()
{
    if let Some(fname) = std::env::args().nth(1)
    {
        use std::io::prelude::*;

        let source = std::fs::File::open(fname).and_then(|mut fp|
        {
            let mut s = String::new(); fp.read_to_string(&mut s).map(|_| s.chars().collect::<Vec<_>>())
        }).unwrap();
        let mut source = mapleparser::SourceSlice::new(&source);
        loop
        {
            let tok = mapleparser::tokenize(&mut source).unwrap();
            println!("{}", json::encode(&tok).unwrap());
            if tok.subtype == mapleparser::TokenSubtype::Term { break; }
        }
    }
}
