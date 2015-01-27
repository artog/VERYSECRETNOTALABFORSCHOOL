package edu.gu.hajo.trie;

import java.util.ArrayList;
import java.util.List;

/*
 *  Nodes in Trie
 *  Note: Package private class
 */
public class TrieNode {

    private boolean endOfWord;
    public final char element;
    public final TrieNode parent;
    private final List<TrieNode> childs;
    private final List<TrieNode> peers;

    // Used to mark the root node
    public static final char ROOT = '§';

    public TrieNode(TrieNode parent, char element) {
        this.parent = parent;
        this.element = element;
        this.childs = new ArrayList();
        this.peers = new ArrayList();
    }

    public void setEndOfWord() {
        endOfWord = true;
    }

    public boolean isEndOfWord() {
        return endOfWord;
    }

    public List<TrieNode> getPeers() {
        List<TrieNode> nodes = new ArrayList();
        nodes.addAll(peers);
        return nodes;
    }

    public List<TrieNode> getChilds() {
        List<TrieNode> nodes = new ArrayList();
        nodes.addAll(childs);
        return nodes;
    }

    public TrieNode getChild(char element) {
        for (TrieNode i : childs) {
            if (i.element == element) {
                return i;
            }
        }
        return null;
    }

    public TrieNode addChild(char element) {
        TrieNode child = this.getChild(element);
        if(child == null){
            child = new TrieNode(this, element);
            childs.add(child);    
        }
        return child;
    }

    // Connect one node in this Trie with node in other Trie
    public void addPeer(TrieNode peer) {
        for (TrieNode p : peers) {
            if (peer == p) {
                return;
            }
        }
        
        peers.add(peer);
        peer.addPeer(this);
    }

    // Possible good for debug
    @Override
    public String toString() {
        return "Replace with better data";  // TODO 
    }
}
