#[derive(Debug)]
pub enum Text {
    Roman(String),
    Boldface(Box<Text>),
    Italic(Box<Text>),
    Join { texts: Vec<Text>, sep: Box<Text> },
}

impl Text {
    pub fn from_vec(texts: Vec<Text>, sep: Text) -> Text {
        if texts.len() == 1 {
            texts.into_iter().next().unwrap()
        } else {
            Text::Join { texts: texts, sep: sep.into() }
        }
    }
}

impl From<String> for Text {
    fn from(s: String) -> Text {
        Text::Roman(s)
    }
}

impl<'a> From<&'a str> for Text {
    fn from(s: &str) -> Text {
        s.to_owned().into()
    }
}

impl<'a> From<&'a str> for Box<Text> {
    fn from(s: &str) -> Box<Text> {
        Box::new(s.into())
    }
}

impl From<Vec<Text>> for Text {
    fn from(texts: Vec<Text>) -> Text {
        Text::from_vec(texts, "".into())
    }
}
