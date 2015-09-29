extern crate xml;

use std::char;
use std::io::Read;
use text::*;

use self::xml::reader::EventReader;
use self::xml::reader::XmlEvent::*;
use self::xml::reader::XmlEvent;
use self::xml::attribute::OwnedAttribute;

struct ParseOptions {
    ignore_whitespace: bool,
    transform_characters: Box<Fn(String) -> Text>,
}

impl Default for ParseOptions {
    fn default() -> Self {
        ParseOptions {
            ignore_whitespace: true,
            transform_characters: Box::new(|s| s.into()),
        }
    }
}

fn to_superscript(c: char) -> char {
    match c {
        '1'               => '\u{00B9}',
        '2'               => '\u{00B2}',
        '3'               => '\u{00B3}',
        '0' | '4' ... '9' => char::from_u32(c as u32 - '0' as u32 + 0x2070).unwrap(),
        _                 => panic!("invalid input: {}", c),
    }
}

struct Parser<R: Read> {
    reader: EventReader<R>,
    entry_found: Option<bool>,
}

impl<R: Read> Parser<R> {
    fn new(src: R) -> Parser<R> {
        Parser {
            reader: EventReader::new(src),
            entry_found: None,
        }
    }

    fn parse(&mut self, event: XmlEvent, options: &ParseOptions) -> Option<Text> {
        match event {
            StartDocument { .. } => self.parse_next(options),
            StartElement { name, attributes, .. } => {
                let t = match name.local_name.as_str() {
                    "entry_list" => self.parse_entry_list(),
                    "entry"      => self.parse_entry(),
                    "hw"         => self.parse_hw(attributes),
                    "suggestion" => self.parse_suggestion(),
                    other        => panic!("unknown tag: {}", other),
                };
                Some(t)
            }
            Characters(s) => Some((options.transform_characters)(s)),
            Whitespace(s) => if options.ignore_whitespace {
                None
            } else {
                Some(((options.transform_characters)(s)))
            },
            other => panic!("unexpected XmlEvent: {:?}", other),
        }
    }

    fn parse_next(&mut self, options: &ParseOptions) -> Option<Text> {
        let e = self.reader.next().unwrap();
        self.parse(e, options)
    }

    fn entry_found(&self) -> bool {
        self.entry_found.unwrap_or(false)
    }

    fn parse_entry_list(&mut self) -> Text {
        let texts = self.collect_content("entry_list", &Default::default());
        match self.entry_found {
            Some(true) => Text::Join { texts: texts, sep: "\n\x0C\n".into() },
            Some(false) => Text::Join {
                texts: vec![
                    concat!("The word you've entered isn't in the dictionary.  ",
                            "Choose a spelling suggestion below or try again.").into(),
                    Text::Join { texts: texts, sep: "\n".into() },
                ],
                sep: "\n\n".into(),
            },
            None => concat!("The word you've entered was not found.  ",
                            "Please try your search again.").into(),
        }
    }

    fn parse_entry(&mut self) -> Text {
        assert!(self.entry_found.unwrap_or(true));
        self.entry_found = Some(true);
        let mut head = Vec::new();
        let mut body = Vec::new();
        loop {
            let e = self.reader.next().unwrap();
            let name = match e {
                StartElement { ref name, .. } => name.local_name.clone(),
                EndElement { ref name, .. } if name.local_name == "entry" => break,
                other => panic!("unexpected XmlEvent: {:?}", other),
            };
            let name = name.as_str();
            match name {
                "hw" => head.push(self.parse(e, &Default::default()).unwrap()),
                "ew" | "subj" | "sound" | "et" | "grp" | "art" => self.ignore(name),
                _ => self.ignore(name), // TODO
            };
        }
        let texts = vec![Text::from_vec(head, "\n".into())].into_iter().chain(body).collect();
        Text::from_vec(texts, "\n\n".into())
    }

    fn parse_suggestion(&mut self) -> Text {
        assert!(!self.entry_found());
        self.entry_found = Some(false);
        self.collect_content("suggestion", &Default::default()).into()
    }

    fn parse_hw_like(&mut self, element_name: &str, attrs: Vec<OwnedAttribute>) -> Text {
        let hindex = attrs.iter().find(|a| { a.name.local_name == "hindex" }).map(|a| {
            a.value.chars().map(to_superscript).collect::<String>()
        });
        let mut opts = ParseOptions::default();
        opts.transform_characters = Box::new(move |s0| {
            let mut s = String::with_capacity(s0.len());
            for hi in hindex.as_ref() { s.push_str(hi) }
            for c in s0.chars() { s.push(if c == '*' { '\u{00B7}' } else { c }) }
            s.into()
        });
        Text::Boldface(Box::new(self.collect_content(element_name, &opts).into()))
    }

    fn parse_hw(&mut self, attrs: Vec<OwnedAttribute>) -> Text {
        self.parse_hw_like("hw", attrs)
    }

    fn collect_content(&mut self, element_name: &str, options: &ParseOptions) -> Vec<Text> {
        let mut texts = Vec::new();
        loop {
            match self.reader.next().unwrap() {
                EndElement { name } => {
                    assert_eq!(name.local_name, element_name);
                    break;
                }
                other => for t in self.parse(other, options) {
                    texts.push(t);
                },
            };
        }
        texts
    }

    fn ignore(&mut self, element_name: &str) {
        let mut path = Vec::new();
        loop {
            match self.reader.next().unwrap() {
                StartElement { name, .. } => path.push(name),
                EndElement { name } => if name.local_name == element_name {
                    debug_assert!(path.is_empty());
                    break;
                } else {
                    let n = path.pop().unwrap();
                    debug_assert_eq!(name, n);
                },
                _ => {}
            };
        }
    }
}

pub fn parse<R: Read>(src: R) -> Result<Text, Text> {
    let mut parser = Parser::new(src);
    let t = parser.parse_next(&Default::default()).unwrap();
    if parser.entry_found() { Ok(t) } else { Err(t) }
}
