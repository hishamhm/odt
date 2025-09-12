use crate::error::SourceError;
use crate::node::Node;
use crate::parse::TypedRuleExt;
use crate::parse::rules::NodeReference;
use crate::path::NodePath;
use hashlink::LinkedHashMap;

pub type LabelMap = LinkedHashMap<String, NodePath>;

pub struct LabelResolver<'a, P>(pub &'a LabelMap, pub &'a Node<P>);

impl<P> LabelResolver<'_, P> {
    pub fn resolve(
        &self,
        relative_to: &NodePath,
        noderef: &NodeReference,
    ) -> Result<NodePath, SourceError> {
        self.resolve_str(relative_to, noderef.str())
            .ok_or_else(|| noderef.err("no such node"))
    }

    fn resolve_str(&self, relative_to: &NodePath, noderef: &str) -> Option<NodePath> {
        let path = noderef.trim_matches(['&', '{', '}']);
        let mut segments = path.split('/');
        let first = segments.next().unwrap();
        let segments = segments.filter(|s| !s.is_empty());
        let (root, mut result) = match first {
            "" => {
                // The node reference is absolute.
                (self.1, NodePath::root())
            }
            "." => {
                // The node reference is relative to the current node.
                let current: &Node<P> = self.1.walk(relative_to.segments()).unwrap();
                (current, relative_to.clone())
            }
            label => {
                // The first segment is a label name.
                let target = self.0.get(label)?;
                (self.1.walk(target.segments())?, target.clone())
            }
        };
        // Check that the path exists.
        root.walk(segments.clone())?;
        for s in segments {
            result.push(s);
        }
        Some(result)
    }
}
