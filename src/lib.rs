pub mod eval;
pub mod flat;
pub mod fs;
pub mod parse;
pub mod parse_untyped;
pub mod print;

// TODO:  should this represent labels?  phandles?

#[derive(Debug, Default)]
pub struct Node {
    pub name: String,
    pub properties: Vec<Property>,
    pub children: Vec<Node>,
}

#[derive(Debug, Default)]
pub struct Property {
    pub name: String,
    pub value: Vec<u8>,
}

impl Node {
    fn walk<'a>(&self, path: impl IntoIterator<Item = &'a str>) -> Option<&Node> {
        let mut path = path.into_iter();
        match path.next() {
            None | Some("") => Some(self),
            Some(segment) => {
                for child in &self.children {
                    if child.name == segment {
                        return child.walk(path);
                    }
                }
                None
            }
        }
    }

    fn walk_mut<'a>(&mut self, path: impl IntoIterator<Item = &'a str>) -> Option<&mut Node> {
        let mut path = path.into_iter();
        match path.next() {
            None | Some("") => Some(self),
            Some(segment) => {
                for child in &mut self.children {
                    if child.name == segment {
                        return child.walk_mut(path);
                    }
                }
                None
            }
        }
    }

    fn add_child(&mut self, name: &str) -> &mut Node {
        // work around https://github.com/rust-lang/rust/issues/21906
        for (index, child) in self.children.iter().enumerate() {
            if child.name == name {
                return &mut self.children[index];
            }
        }
        self.children.push(Node {
            name: name.to_owned(),
            ..Default::default()
        });
        return self.children.last_mut().unwrap();
    }
}
