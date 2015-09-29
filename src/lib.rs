mod collegiate_xml;
mod text;

extern crate hyper;
extern crate url;

use hyper::Client;
use hyper::status::StatusCode;
use std::env;
use std::fmt;
use std::fs;
use std::io::{Read, Write};
use std::path::PathBuf;
use text::Text;
use url::percent_encoding::*;

#[derive(Debug)]
pub enum Format {
    XML,
}

impl fmt::Display for Format {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            Format::XML => "xml",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
pub enum Product {
    Collegiate,
}

impl Product {
    fn api_key(&self) -> Result<String, env::VarError> {
        env::var(format!("MWDICT_API_KEY_{}", self).to_uppercase())
    }
}

impl fmt::Display for Product {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            Product::Collegiate => "collegiate",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
pub struct ProductFormat(pub Product, pub Format);

impl ProductFormat {
    fn origin(&self, word: &str) -> String {
        let w = utf8_percent_encode(word, DEFAULT_ENCODE_SET);
        let k = self.0.api_key().unwrap();
        format!("http://www.dictionaryapi.com/api/v1/references/{}/{}/{}?key={}",
                self.0, self.1, w, k)
    }

    fn cache(&self, word: &str) -> PathBuf {
        let mut p = match env::var_os("MWDICT_CACHE_HOME") {
            Some(s) => PathBuf::from(s),
            None => {
                let mut xdg = match env::var_os("XDG_CACHE_HOME") {
                    Some(s) => PathBuf::from(s),
                    None => {
                        let mut home = PathBuf::from(env::var_os("HOME").unwrap());
                        home.push(".cache");
                        home
                    }
                };
                xdg.push("mwdict");
                xdg
            }
        };
        p.push(format!("{}", self.0));
        fs::create_dir_all(&p).unwrap();
        p.push(format!("{}.{}", word, self.1));
        p
    }

    fn parse<A: Read>(&self, src: A) -> Result<Text, Text> {
        match self {
            &ProductFormat(Product::Collegiate, Format::XML) => collegiate_xml::parse(src),
        }
    }

    pub fn search(&self, word: &str) -> Text {
        let cache = self.cache(word);
        let result = if fs::metadata(&cache).is_ok() {
            let src = fs::File::open(cache).unwrap();
            self.parse(src)
        } else {
            let url = self.origin(word);
            let mut resp = Client::new().get(&url).send().unwrap();
            assert_eq!(resp.status, StatusCode::Ok);
            let mut buf = Vec::new();
            resp.read_to_end(&mut buf).unwrap();
            let result = self.parse(buf.as_slice());
            if result.is_ok() {
                let mut file = fs::File::create(cache).unwrap();
                file.write_all(&buf).unwrap();
            }
            result
        };
        result.unwrap_or_else(|t| t)
    }
}
