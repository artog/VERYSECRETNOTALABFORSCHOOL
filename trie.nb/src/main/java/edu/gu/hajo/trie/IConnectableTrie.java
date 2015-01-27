package edu.gu.hajo.trie;

import java.util.List;

public interface IConnectableTrie {

    // Add a word to Trie, return node containing last char in key    
    public abstract TrieNode insert(String key);

    // Get all keys starting with prefix
    public abstract List<String> getKeys(String prefix);

    // Get all values (words) form other Trie connected to this
    // Tries key
    public abstract List<String> getValues(String key);

    // True if this Trie contains key
    public abstract boolean contains(String key);
}
