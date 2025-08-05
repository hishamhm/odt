use crate::error::SourceError;
use crate::node::Node;
use crate::parse::TypedRuleExt;
use crate::parse::rules::NodeReference;
use crate::path::NodePath;
use hashlink::LinkedHashMap;

pub type LabelMap = LinkedHashMap<String, NodePath>;

pub struct LabelResolver<'a, P>(pub &'a LabelMap, pub &'a Node<P>);

impl<P> LabelResolver<'_, P> {
    pub fn resolve(&self, noderef: &NodeReference) -> Result<NodePath, SourceError> {
        self.resolve_str(noderef.str())
            .ok_or_else(|| noderef.err("no such node"))
    }

    fn resolve_str(&self, noderef: &str) -> Option<NodePath> {
        let path = noderef.trim_matches(['&', '{', '}']);
        let mut segments = path.split('/');
        let first = segments.next().unwrap();
        let segments = segments.filter(|s| !s.is_empty());
        let (root, mut result) = if first.is_empty() {
            // The node reference is absolute.
            (self.1, NodePath::root())
        } else {
            // The first segment is a label name.
            let target = self.0.get(first)?;
            (self.1.walk(target.segments())?, target.clone())
        };
        // Check that the path exists.
        root.walk(segments.clone())?;
        for s in segments {
            result.push(s);
        }
        Some(result)
    }
}
