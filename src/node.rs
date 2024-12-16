use core::fmt::Display;
use hashlink::linked_hash_map::Entry;
use hashlink::{LinkedHashMap, LinkedHashSet};

/// An intermediate representation of a node tree, used to gather deletes and overrides.
/// Preserves the input ordering while allowing random access.
///
/// Behavior doesn't match `dtc` in all cases.  Deleting a node and redefining it with
/// the same name will move it to the end, but `dtc` remembers the original ordering.
#[derive(Clone, Default)]
pub struct Node<P> {
    labels: LinkedHashSet<String>,
    properties: LinkedHashMap<String, P>,
    children: LinkedHashMap<String, Node<P>>,
}

impl<P> Node<P> {
    pub fn walk<'a, 'b>(&'a self, path: impl IntoIterator<Item = &'b str>) -> Option<&'a Node<P>> {
        let mut path = path.into_iter();
        match path.next() {
            None | Some("") => Some(self),
            Some(segment) => self.children.get(segment)?.walk(path),
        }
    }

    pub fn walk_mut<'a, 'b>(
        &'a mut self,
        path: impl IntoIterator<Item = &'b str>,
    ) -> Option<&'a mut Node<P>> {
        let mut path = path.into_iter();
        match path.next() {
            None | Some("") => Some(self),
            Some(segment) => self.children.get_mut(segment)?.walk_mut(path),
        }
    }

    pub fn remove_child(&mut self, name: &str) {
        self.children.remove(name);
    }

    pub fn get_property(&self, name: &str) -> Option<&P> {
        self.properties.get(name)
    }

    pub fn get_property_or_insert_with(
        &mut self,
        name: &str,
        default: impl FnOnce() -> P,
    ) -> &mut P {
        // Avoid `Entry::or_insert_with()` because on LinkedHashMap that reorders existing entries.
        match self.properties.entry(name.into()) {
            Entry::Vacant(entry) => entry.insert(default()),
            Entry::Occupied(entry) => entry.into_mut(),
        }
    }

    pub fn set_property(&mut self, name: &str, value: P) {
        self.properties.replace(name.into(), value);
    }

    pub fn remove_property(&mut self, name: &str) {
        self.properties.remove(name);
    }

    pub fn children(&self) -> impl Iterator<Item = (&String, &Self)> {
        self.children.iter()
    }

    pub fn children_mut(&mut self) -> impl Iterator<Item = (&String, &mut Self)> {
        self.children.iter_mut()
    }

    pub fn properties(&self) -> impl Iterator<Item = (&String, &P)> {
        self.properties.iter()
    }

    pub fn properties_mut(&mut self) -> impl Iterator<Item = (&String, &mut P)> {
        self.properties.iter_mut()
    }

    pub fn add_label(&mut self, name: &str) {
        self.labels.replace(name.into());
    }

    pub fn labels_as_display(&self) -> LabelsDisplay {
        LabelsDisplay(&self.labels)
    }
}

impl<P: Default> Node<P> {
    pub fn add_child(&mut self, name: &str) -> &mut Node<P> {
        // Avoid `Entry::or_insert_with()` because on LinkedHashMap that reorders existing entries.
        match self.children.entry(name.into()) {
            Entry::Vacant(entry) => entry.insert(Default::default()),
            Entry::Occupied(entry) => entry.into_mut(),
        }
    }
}

impl<P: Display> Display for Node<P> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        writeln!(f, "{{")?;
        for (name, value) in &self.properties {
            writeln!(f, "{name} = {value};")?;
        }
        if !self.properties.is_empty() && !self.children.is_empty() {
            writeln!(f)?;
        }
        for (name, node) in &self.children {
            writeln!(f, "{}{name} {node};", node.labels_as_display())?;
        }
        write!(f, "}}")
    }
}

pub struct LabelsDisplay<'a>(&'a LinkedHashSet<String>);

impl<'a> Display for LabelsDisplay<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for label in self.0 {
            write!(f, "{label}: ")?;
        }
        Ok(())
    }
}
