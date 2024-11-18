use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

#[derive(Parser)]
#[grammar = "tree/dts.pest"]
struct DtsParser;

// TODO: look at ginko grammar

/// An Abstract Syntax Tree for a Devicetree source file.
pub type Tree<'a> = Pair<'a, Rule>;

pub fn parse(source: &str) -> Tree {
    match DtsParser::parse(Rule::dtsfile, &source) {
        Ok(mut dts) => dts.next().unwrap(),
        Err(err) => panic!(
            "parsing failed:\n{}",
            err.renamed_rules(|rule| format!("{:?}", rule))
        ),
    }
}

// TODO: helper function for processing a file with includes

/// A stateful helper for loading source files from a list of include directories to be searched.
/// TODO: look at clang::FileManager and clang::HeaderSearch
pub struct IncludeLoader {
    /// The list of directories to search
    search_paths: Vec<PathBuf>,
    /// cache of previously loaded files
    file_contents: Mutex<HashMap<PathBuf, Option<Vec<u8>>>>,
    /// existing parent directories of files observed not to exist
    parents_of_missing: Mutex<HashSet<PathBuf>>,
}

impl IncludeLoader {
    pub fn new(search_paths: Vec<PathBuf>) -> Self {
        Self {
            search_paths,
            file_contents: Default::default(),
            parents_of_missing: Default::default(),
        }
    }

    // TODO: do we need relative includes?
    pub fn find(&self, included_path: &Path) -> Option<&[u8]> {
        for dir in &self.search_paths {
            let path = dir.join(included_path);
            if let Some(content) = self.read(path) {
                return Some(content);
            }
        }
        None
    }

    pub fn read(&self, path: PathBuf) -> Option<&[u8]> {
        let mut file_contents = self.file_contents.lock().unwrap();
        let contents = match file_contents.entry(path.clone()) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => {
                let path = entry.key();
                let result = std::fs::read(path).ok();
                if result.is_none() {
                    self.track_parent_of_missing(path);
                }
                entry.insert(result)
            }
        };
        // SAFETY:  We never remove an entry from the map, so the values live as long as &self.
        unsafe { core::mem::transmute(contents.as_deref()) }
    }

    fn track_parent_of_missing(&self, mut path: &Path) {
        let mut parents_of_missing = self.parents_of_missing.lock().unwrap();
        while let Some(parent) = path.parent() {
            path = parent;
            if path.as_os_str().is_empty() {
                // final ancestor of a relative path is ""; translate to "."
                parents_of_missing.insert(PathBuf::from("."));
            }
            if parents_of_missing.contains(path) {
                return;
            }
            if path.exists() {
                parents_of_missing.insert(path.to_owned());
                return;
            }
        }
    }

    /// Produce a representation of the files accessed, in the format of a ninja depfile
    /// or the output of `cpp -MD` or `makedepend`.
    /// TODO: make this osstr clean?
    /// TODO: escape space and backslash
    pub fn write_depfile(&self, goal: &str) -> String {
        let mut files: Vec<_> = self.file_contents.lock().unwrap().keys().cloned().collect();
        files.sort();
        let mut dirs: Vec<_> = self
            .parents_of_missing
            .lock()
            .unwrap()
            .iter()
            .cloned()
            .collect();
        dirs.sort();
        let mut out = String::new();
        _ = write!(out, "{goal}:");
        for f in files {
            _ = write!(out, " {}", f.display());
        }
        for d in dirs {
            _ = write!(out, " {}", d.display());
        }
        _ = writeln!(out);
        out
    }
}
