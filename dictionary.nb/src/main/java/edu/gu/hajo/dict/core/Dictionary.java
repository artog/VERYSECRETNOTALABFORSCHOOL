package edu.gu.hajo.dict.core;

import edu.gu.hajo.trie.IConnectableTrie;

/**
 * Implementation of the dictionary
 *
 * Dictionary uses two tries - One for the source language and one for the
 * target language (translations). Nodes in the source language are connected to
 * nodes in the target language
 *
 */
public class Dictionary implements IDictionary {

    // The words in the "from" language
    private final IConnectableTrie source;
    // Translations of the words in source (the "target" language)
    private final IConnectableTrie translations;

    public Dictionary(IConnectableTrie source, IConnectableTrie target) {
        this.source = source;
        this.translations = target;
    }

}
