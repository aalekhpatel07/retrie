use std::{
    collections::HashMap, 
    hash::Hash, fmt::Debug
};
use derive_builder::Builder;

pub type Result<T> = std::result::Result<T, TrieNodeBuilderError>;

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
/// assert!(node.indices.is_none());
/// assert!(node.parent.is_none());
/// assert!(node.children.is_none());
/// ```
#[derive(Debug, Clone, Builder)]
#[builder(name = "TrieNodeBuilder")]
#[builder(derive(Debug))]
pub struct TrieNode<A, V> 
where 
    A: Hash
{
    // /// A pointer to the parent node, in case we'd like to backtrack.
    // #[builder(default)]
    // pub parent: Option<Box<TrieNode<A>>>,

    /// A collection of pointers to the start of suffixes
    /// of all words that begin with this character and have a prefix ending at the parent node.
    #[builder(default)]
    pub children: Option<HashMap<A, Option<Box<TrieNode<A, V>>>>>,
    
    /// The current character stored at this position.
    #[builder(default)]
    pub value: Option<V>,

    /// The indices of the inserted words that end at this node
    /// in the order of insertion, if any word ends here.
    #[builder(default)]
    pub indices: Option<Vec<usize>>
}

#[cfg(test)]
impl<A, V> Eq for TrieNode<A, V> 
where
    A: Hash + PartialEq + Eq,
    V: PartialEq + Eq
{}

/// The PartialEq is only here for debugging purposes.
#[cfg(test)]
impl<A, V> PartialEq for TrieNode<A, V>
where 
    A: Hash + PartialEq + Eq,
    V: PartialEq + Eq
{
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
        // && self.parent == other.parent
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


impl<A, V> Default for TrieNode<A, V> 
where 
    A: Hash
{
    fn default() -> Self {
        Self {
            // parent: None,
            children: None,
            value: None,
            indices: None,
        }
    }
}

pub type TrieWithoutValue<A> = Trie<A, ()>;

pub struct Trie<A, V> 
where 
    A: Hash
{
    pub root: TrieNode<A, V>,
    size: usize,
}

impl<A, V> Default for Trie<A, V>
where 
    A: Hash
{
    fn default() -> Self {
        Self {
            root: TrieNode::default(),
            size: 0,
        }
    }
}


impl<A, V> Trie<A, V> 
where 
    A: Hash + Clone + PartialEq + Eq + Debug,
    V: Clone + Debug
{
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert_many(
        &mut self,
        words: impl Iterator<Item=(impl Iterator<Item=A>, Option<V>)>
    ) -> crate::Result<()> {

        for (index, (word, value)) in words.enumerate() {
            self.insert(word, value)?;
        }

        Ok(())
    }

    pub fn insert(
        &mut self, 
        word: impl Iterator<Item=A>,
        value: Option<V>,
    ) -> crate::Result<()> {

        let mut current_node = &mut self.root;

        for character in word {

            match current_node.children {
                Some(ref mut children) => {
                    let character_node_entry = children.entry(character.clone());
                    
                    character_node_entry
                    .and_modify(
                        |e| {
                            if e.is_none() {
                                *e = Some(
                                    Box::new(TrieNodeBuilder::default()
                                    .value(value.clone())
                                    .build()
                                    .unwrap())
                                );
                            }
                        }
                    )
                    .or_insert_with(|| {
                        Some(
                            Box::new(TrieNodeBuilder::default()
                            .value(value.clone())
                            .build()
                            .unwrap())
                        )
                    });
                },
                None => {
                    let character_node = 
                        TrieNodeBuilder::default()
                        .value(value.clone())
                        .build()?;
                    
                    current_node.children = Some({
                        let mut children = HashMap::new();
                        children.insert(character.clone(), Some(Box::new(character_node)));
                        children
                    });
                }
            }

            current_node = 
                current_node
                .children
                .as_mut()
                .unwrap()
                .get_mut(&character)
                .unwrap()
                .as_mut()
                .unwrap();
        }

        match current_node.indices {
            Some(ref mut indices) => {
                indices.push(self.size);
            },
            None => {
                current_node.indices = Some(vec![self.size]);
            }
        }

        self.size += 1;
        
        Ok(())
    }

    pub fn find_with_prefix(
        &self,
        prefix: impl Iterator<Item=A>
    ) -> Vec<(Vec<A>, usize, Option<V>)> {

        let mut current_node = &self.root;
        let mut prefix_so_far = vec![];

        for character in prefix {
            match current_node.children {
                Some(ref children) => {
                    match children.get(&character) {
                        Some(ref node) => {
                            current_node = node.as_ref().unwrap();
                            prefix_so_far.push(character);
                        },
                        None => {
                            // We found that we don't have any word with a smaller prefix of the given prefix.
                            return vec![];
                        }
                    }
                },
                None => {
                    // We found that we don't have any word with a smaller prefix of the given prefix.
                    return vec![];
                }
            }
        }

        self.bfs(current_node, prefix_so_far)
    }

    fn bfs<O>(&self, node: &TrieNode<A, V>, prefix: Vec<A>) -> Vec<O>
    where
        O: From<(Vec<A>, usize, Option<V>)>
    {
        let mut queue = vec![(node, prefix)];
        let mut results = vec![];

        while let Some((node, current_prefix)) = queue.pop() {
            if let Some(ref indices) = node.indices {

                for index in indices {
                    results.push((
                        current_prefix.clone(),
                        *index,
                        node.value.clone()
                    ).into());
                }
            }

            if let Some(ref children) = node.children {
                for (child_character, child) in children.iter() {
                    let mut new_prefix = current_prefix.clone();
                    new_prefix.push(child_character.clone());
                    queue.push((child.as_ref().unwrap(), new_prefix));
                }
            }
        }

        results
    }

}

impl<A, V> Debug for Trie<A, V> 
where
    A: Hash + Debug,
    V: Debug
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
        let trie = Trie::<char, ()>::new();
        assert_eq!(trie.root, TrieNodeBuilder::default().build().unwrap());
    }

    #[test]
    fn test_insert() {
        let mut trie = Trie::<char, ()>::new();
        trie.insert("hello".chars(), None).unwrap();
        trie.insert("help".chars(), None).unwrap();
        println!("{:#?}", trie);
    }

    #[test]
    fn test_insert_dictionary() {
        let mut trie = TrieWithoutValue::<char>::new();
        let words: Vec<_> = vec![
            "hello", "help", "hell", "he", "h", "hello"
        ].into_iter().map(String::from).collect();

        trie.insert_many(
            words
            .iter()
            .map(|x| (x.chars(), None))
        ).unwrap();
        println!("{:#?}", trie);
    }

    #[test]
    fn test_search_prefix() {
        let mut trie = TrieWithoutValue::<char>::new();
        let words: Vec<_> = vec![
            "hello", "help", "hell", "he", "h", "hello"
        ].into_iter().map(String::from).collect();

        trie.insert_many(
            words
            .iter()
            .map(|x| (x.chars(), None))
        ).unwrap();

        let mut results: Vec<_> = 
            trie
            .find_with_prefix("hel".chars())
            .into_iter()
            .map(|(word, index, _)| (word.into_iter().collect::<String>(), index))
            .collect();
        results.sort_by_key(|v| v.1);

        assert_eq!(results, vec![
            ("hello".to_string(), 0),
            ("help".to_string(), 1),
            ("hell".to_string(), 2),
            ("hello".to_string(), 5),
        ])
    }

    #[test]
    fn test_search_prefix_full() {
        let mut trie = TrieWithoutValue::<char>::new();
        let words: Vec<_> = vec![
            "hello", "help", "hell", "he", "h", "hello"
        ].into_iter().map(String::from).collect();

        trie.insert_many(
            words
            .iter()
            .map(|x| (x.chars(), None))
        ).unwrap();

        let mut results: Vec<_> = 
            trie
            .find_with_prefix("hello".chars())
            .into_iter()
            .map(|(word, index, _)| (word.into_iter().collect::<String>(), index))
            .collect();
        results.sort_by_key(|v| v.1);

        assert_eq!(results, vec![
            ("hello".to_string(), 0),
            ("hello".to_string(), 5),
        ])
    }


    #[test]
    fn test_search_prefix_empty() {
        let mut trie = TrieWithoutValue::<char>::new();
        let words: Vec<_> = vec![
            "hello", "help", "hell", "he", "h", "hello"
        ].into_iter().map(String::from).collect();

        trie.insert_many(
            words
            .iter()
            .map(|x| (x.chars(), None))
        ).unwrap();

        let mut results: Vec<_> = 
            trie
            .find_with_prefix("".chars())
            .into_iter()
            .map(|(word, index, _)| (word.into_iter().collect::<String>(), index))
            .collect();
        results.sort_by_key(|v| v.1);

        assert_eq!(results, vec![
            ("hello".to_string(), 0),
            ("help".to_string(), 1),
            ("hell".to_string(), 2),
            ("he".to_string(), 3),
            ("h".to_string(), 4),
            ("hello".to_string(), 5),
        ])
    }

}