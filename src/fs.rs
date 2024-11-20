//! Facilities for reading sources from the filesystem.

use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

/// A stateful helper for loading source files from a list of include directories to be searched.
/// TODO: look at clang::FileManager and clang::HeaderSearch
/// TODO: make this "FileLoader" and put the initial file in it too.  will simplify lifetimes.
pub struct IncludeLoader {
    /// The list of directories to search
    search_path: Vec<PathBuf>,
    /// cache of previously loaded files
    file_contents: Mutex<HashMap<PathBuf, Option<Vec<u8>>>>,
    /// existing parent directories of files observed not to exist
    parents_of_missing: Mutex<HashSet<PathBuf>>,
}

impl IncludeLoader {
    pub fn new(search_path: Vec<PathBuf>) -> Self {
        Self {
            search_path,
            file_contents: Default::default(),
            parents_of_missing: Default::default(),
        }
    }

    // TODO: do we need relative includes?
    pub fn find(&self, relative_to: Option<&Path>, included_path: &Path) -> Option<&[u8]> {
        let search_path = self.search_path.iter().map(PathBuf::as_ref);
        for dir in relative_to.into_iter().chain(search_path) {
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
