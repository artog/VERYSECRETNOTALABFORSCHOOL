/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.trie;

/**
 *
 * @author adam
 */
public class ConnectableTrieFactory {
    
    private ConnectableTrieFactory() {}
    // Factory method
    
    public static IConnectableTrie newInstance() {
        return new ConnectableTrie();
    }
}
