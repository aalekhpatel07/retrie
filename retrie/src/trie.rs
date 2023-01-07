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
/// assert!(node.children.is_none());
/// ```
#[derive(Debug, Clone, Builder)]
#[builder(name = "TrieNodeBuilder")]
#[builder(derive(Debug))]
pub struct TrieNode<A, V> 
where 
    A: Hash
{
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
#[cfg(not(tarpaulin_include))]
#[cfg(test)]
impl<A, V> PartialEq for TrieNode<A, V>
where 
    A: Hash + PartialEq + Eq,
    V: PartialEq + Eq
{
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
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
    root: TrieNode<A, V>,
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

    pub fn len(&self) -> usize {
        self.size
    }

    pub fn insert_many(
        &mut self,
        words: impl Iterator<Item=(impl Iterator<Item=A>, Option<V>)>
    ) {
        for (word, value) in words {
            self.insert(word, value);
        }
    }

    pub fn insert(
        &mut self, 
        word: impl Iterator<Item=A>,
        value: Option<V>,
    ) {

        let mut current_node = &mut self.root;
        for character in word {

            match current_node.children {
                Some(ref mut children) => {
                    let character_node_entry = children.entry(character.clone());
                    character_node_entry
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
                        .build()
                        .unwrap();
                    
                    current_node.children = Some({
                        let mut children = HashMap::new();
                        children.insert(character.clone(), Some(Box::new(character_node)));
                        children
                    });
                }
            }
            current_node = current_node.children.as_mut().unwrap().get_mut(&character).unwrap().as_mut().unwrap();
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


/// Utility struct when it is known that 
/// we're working with a Trie of words of utf-8 strings.
#[derive(Debug, Default)]
pub struct StringTrie {
    trie: Trie<char, ()>,
}

impl<'a> StringTrie {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, word: &str) {
        self.trie.insert(word.chars(), None)
    }

    pub fn insert_many(&mut self, words: impl Iterator<Item=&'a str>) {
        self.trie.insert_many(words.map(|word| (word.chars(), None)))
    }

    pub fn find_with_prefix(&self, prefix: &str) -> Vec<(String, usize)> {
        self.trie
            .find_with_prefix(prefix.chars())
            .into_iter()
            .map(|(word, index, _)| (word.into_iter().collect(), index))
            .collect()
    }
    pub fn len(&self) -> usize {
        self.trie.len()
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;
    use proptest::prelude::*;

    #[test]
    fn test_alphabet() {
        let trie = Trie::<char, ()>::new();
        assert_eq!(trie.root, TrieNodeBuilder::default().build().unwrap());
    }

    #[test]
    fn test_insert() {
        let mut trie = Trie::<char, ()>::new();
        trie.insert("hello".chars(), None);
        trie.insert("help".chars(), None);
        trie.insert("".chars(), None);
        println!("{:#?}", trie);
        assert_eq!(trie.len(), 3);
    }

    #[test]
    fn test_insert_string_trie() {
        let mut trie = StringTrie::new();
        trie.insert("hello");
        trie.insert("help");
        trie.insert("");
        println!("{:#?}", trie);
        assert_eq!(trie.len(), 3);
    }
    #[test]
    fn test_find_with_prefix_empty_trie() {
        let trie = StringTrie::new();
        assert_eq!(trie.find_with_prefix("hello"), vec![]);
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
        );
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
        );

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
        );

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
            "hello", "help", "hell", "he", "h", "hello", "heé"
        ].into_iter().map(String::from).collect();

        trie.insert_many(
            words
            .iter()
            .map(|x| (x.chars(), None))
        );

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
            ("heé".to_string(), 6),
        ])
    }

    #[test_case(
        vec!["ab", "aba", "aaba", "aaaaaaa", "aab", "aba", "b"], 
        "a",
        vec![0, 1, 2, 3, 4, 5]
        ;
        "Matching prefix returns all valid matches."
    )]
    #[test_case(
        vec!["trie", "startsWith"], 
        "a",
        vec![]
        ;
        "no matching prefix returns no matches."
    )]
    #[test_case(
        vec!["trie", "startsWith"], 
        "",
        vec![0, 1]
        ;
        "no prefix returns all words as valid matches."
    )]
    fn test_string_trie_prefix_search(
        corpus: Vec<&str>, 
        prefix: &str, 
        mut expected_matches: Vec<usize>
    ) {
        let mut trie = StringTrie::new();
        trie.insert_many(corpus.into_iter());
        let mut observed_matches: Vec<_> = trie.find_with_prefix(prefix).into_iter().map(|(_, index)| index).collect();
        observed_matches.sort();
        expected_matches.sort();
        assert_eq!(observed_matches, expected_matches);
    }

    proptest! {
        #[test]
        fn test_stringtrie_prefix_search_against_std_str_starts_with(
            corpus in r#"([ab]{10,} )*([ab]{10,})"#,
            prefix in r#"[ab]{3,10}"#
        ) {
            let mut trie = StringTrie::new();
            assert_eq!(trie.len(), 0);
            let words: Vec<&str> = corpus.split_whitespace().collect::<Vec<_>>();
            trie.insert_many(words.clone().into_iter());
            assert_eq!(trie.len(), words.len());

            let mut observed_matches: Vec<_> = 
                trie
                .find_with_prefix(&prefix)
                .into_iter()
                .map(|(_, index)| index)
                .collect();

            observed_matches.sort();

            let mut expected_matches: Vec<_> = 
                corpus
                .split_whitespace()
                .enumerate()
                .filter(|(_, word)| word.starts_with(&prefix))
                .map(|(index, _)| index)
                .collect();
            
            expected_matches.sort();
            assert_eq!(observed_matches, expected_matches);
        }
    }
}