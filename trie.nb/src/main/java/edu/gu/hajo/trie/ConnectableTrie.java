package edu.gu.hajo.trie;

import java.util.ArrayList;
import java.util.List;

public class ConnectableTrie implements IConnectableTrie {

    private final TrieNode root;

    private ConnectableTrie() {
        root = new TrieNode(null, TrieNode.ROOT);
    }

    // Factory method
    public static IConnectableTrie newInstance() {
        return new ConnectableTrie();
    }

    // The node structure of ConnectableTrie is final so you can't modify
    // or remove any nodes from it, only add new nodes;
    @Override
    public Connector insert(String key) {
        TrieNode node = root;

        for (char i : key.toCharArray()) {
            node = node.addChild(i);
        }
        node.setEndOfWord();
        return new Connector(node);
    }

    @Override
    public boolean contains(String key) {
        TrieNode node = getNode(key);
        return node != null && node.isEndOfWord();
    }

    @Override
    public List<String> getValues(String key) {
        List<String> values = new ArrayList();
        TrieNode node = getNode(key);

        if(node != null){
            for (TrieNode i : node.getPeers()) {
                values.add(getWord(i));
            }
        }
        return values;
    }

    @Override
    public List<String> getKeys(String prefix) {
        List<String> keys = new ArrayList();
        List<TrieNode> nodes = new ArrayList();

        TrieNode node = getNode(prefix);
        if(node != null){
            nodes.add(node);
        }

        while (!nodes.isEmpty()) {
            node = nodes.remove(0);
            nodes.addAll(node.getChilds());
            
            if (node.isEndOfWord()) {
                keys.add(getWord(node));
            }
        }
        return keys;
    }
    
    private TrieNode getNode(String key){
        TrieNode node = root;
        
        for (char i : key.toCharArray()) {
            node = node.getChild(i);
            if (node == null) {
                break;
            }
        }
        return node;
    }

    private String getWord(TrieNode node) {
        String word = "";

        while (node.parent != null) {
            word = node.element + word;
            node = node.parent;
        }
        return word;
    }
}
