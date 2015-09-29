extern crate mwdict;

use mwdict::*;
use std::env::args;

fn main() {
    let word = args().nth(1).unwrap().trim().to_lowercase();
    let text = ProductFormat(Product::Collegiate, Format::XML).search(&word);
    println!("{:#?}", text);
}
