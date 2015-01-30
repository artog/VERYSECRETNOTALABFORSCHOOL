/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.trie;

/**
 *
 * @author Mikael
 */
public class Connector {
    
    TrieNode node;
    
    public Connector(TrieNode node){
        this.node = node;
    }
    
    public void connect(Connector connector){
        node.addPeer(connector.node);
    }
}
