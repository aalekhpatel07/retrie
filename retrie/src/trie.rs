use std::{
    collections::HashMap, 
    hash::Hash, fmt::Debug
};
use derive_builder::Builder;


/// A data structure that represents a particular character in a word over a alphabet
/// in its compiled representation.
/// 
/// It stores the pointers to the start of all suffixes of words 
/// that begin with this character and have a prefix ending at the parent node.
/// 
/// It also stores the frequency of words that end at this node.
/// 
/// # Example
/// 
/// ```
/// use retrie::{
///     TrieNodeBuilder,
///     TrieNode
/// };
/// 
/// let mut node: TrieNode<_> = 
///     TrieNodeBuilder::default()
///     .value(Some('a'))
///     .indices(None)
///     .build()
///     .expect("Failed to build TrieNode");
/// 
/// assert_eq!(node.value, Some('a'));
/// assert_eq!(node.indices, None);
/// assert_eq!(node.parent, None);
/// assert_eq!(node.children, None);
/// ```
#[derive(Debug, Clone, Builder)]
#[builder(name = "TrieNodeBuilder")]
#[builder(derive(Debug))]
pub struct TrieNode<A> 
where 
    A: Hash
{
    /// A pointer to the parent node, in case we'd like to backtrack.
    #[builder(default)]
    pub parent: Option<Box<TrieNode<A>>>,

    /// A collection of pointers to the start of suffixes
    /// of all words that begin with this character and have a prefix ending at the parent node.
    #[builder(default)]
    pub children: Option<HashMap<A, Option<Box<TrieNode<A>>>>>,
    
    /// The current character stored at this position.
    #[builder(default)]
    pub value: Option<A>,

    /// The indices of the inserted words that end at this node
    /// in the order of insertion, if any word ends here.
    #[builder(default)]
    pub indices: Option<Vec<usize>>
}

#[cfg(test)]
impl<A> Eq for TrieNode<A> 
where
    A: Hash + PartialEq + Eq
{}

/// The PartialEq is only here for debugging purposes.
#[cfg(test)]
impl<A> PartialEq for TrieNode<A>
where 
    A: Hash + PartialEq + Eq
{
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
        && self.parent == other.parent
        && self.indices == other.indices
        && {
            match (&self.children, &other.children) {
                (Some(ref own), Some(ref another)) => {
                    own.len() == another.len() 
                    && own.iter().all(|(k, v)| {
                        another.get(k).map_or(false, |v2| v == v2)
                    }) 
                    && another.iter().all(|(k, v)| {
                        own.get(k).map_or(false, |v2| v == v2)
                    }) 
                },
                (None, None) => true,
                _ => false,
            }
        }
    }
}


impl<A> Default for TrieNode<A> 
where 
    A: Hash
{
    fn default() -> Self {
        Self {
            parent: None,
            children: None,
            value: None,
            indices: None,
        }
    }
}

pub struct Trie<A> 
where 
    A: Hash
{
    pub root: TrieNode<A>,
}

impl<A> Default for Trie<A>
where 
    A: Hash
{
    fn default() -> Self {
        Self {
            root: TrieNode::default(),
        }
    }
}


impl<A> Trie<A> 
where 
    A: Hash,
{
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(
        &mut self, 
        word: impl Iterator<Item=A>
    ) {

    }

}

impl<A> Debug for Trie<A> 
where
    A: Hash + Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Trie")
            .field("root", &self.root)
            .finish()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alphabet() {
        let trie = Trie::<char>::new();
        assert_eq!(trie.root, TrieNodeBuilder::default().build().unwrap());
    }
}