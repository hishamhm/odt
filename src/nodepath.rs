/// A portable subset of PathBuf needed for working with &{...} DTS path references.  The embedded
/// string is normalized and always ends with a slash, simplifying `starts_with()` queries.
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct NodePath(String);

impl NodePath {
    pub fn display(&self) -> &str {
        if self.is_root() {
            "/"
        } else {
            self.0.strip_suffix('/').unwrap()
        }
    }

    pub fn is_root(&self) -> bool {
        self.0.len() == 1
    }

    pub fn join(&self, suffix: &str) -> Self {
        let mut r = self.clone();
        r.push(suffix);
        r
    }

    pub fn leaf(&self) -> &str {
        self.0.rsplit_terminator('/').next().unwrap()
    }

    pub fn parent(&self) -> Self {
        if self.is_root() {
            return self.clone();
        }
        Self(self.0[..self.0.len() - 1 - self.leaf().len()].into())
    }

    pub fn push(&mut self, suffix: &str) {
        for segment in suffix.split('/').filter(|s| !s.is_empty()) {
            self.0.push_str(segment);
            self.0.push('/');
        }
    }

    pub fn root() -> Self {
        Self("/".into())
    }

    pub fn segments(&self) -> impl Iterator<Item = &str> {
        self.0.split_terminator('/').filter(|s| !s.is_empty())
    }

    pub fn starts_with(&self, prefix: &Self) -> bool {
        self.0.starts_with(&prefix.0)
    }
}

impl core::fmt::Debug for NodePath {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.fmt(f)
    }
}

impl core::fmt::Display for NodePath {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.fmt(f)
    }
}
