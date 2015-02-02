package edu.gu.hajo.dict;

import edu.gu.hajo.dict.core.Dictionary;
import edu.gu.hajo.dict.core.DictionaryEntry;
import java.io.IOException;
import java.net.URI;
import edu.gu.hajo.dict.core.IDictionary;
import edu.gu.hajo.dict.core.Language;
import edu.gu.hajo.dict.io.DictionaryReader;
import edu.gu.hajo.trie.ConnectableTrieFactory;
import edu.gu.hajo.trie.Connector;
import edu.gu.hajo.trie.IConnectableTrie;
import java.io.File;
import java.util.List;

/**
 * A Factory for dictionaries
 *
 * @author hajo
 *
 */
public final class DictionaryFactory {

    public static IDictionary getDictionary(URI uri) throws IOException {
        List<String> words = DictionaryReader.INSTANCE.open(uri);
       
        IConnectableTrie source = ConnectableTrieFactory.newInstance();
        IConnectableTrie target = ConnectableTrieFactory.newInstance();
       
        for(String i : words){
            DictionaryEntry entry = DictionaryEntryConverter.toObject(i);
           
            Connector connector = source.insert(entry.getSource());
            for(String translations : entry.getTranslations()){
                connector.connect(target.insert(translations));
            }   
        }

       return new Dictionary(source, target);
    }

    public static URI getDictionaryUri(String dictionaryPath, Language from, Language to) {
        return new File(dictionaryPath + from.toString() + DictionaryReader.SEPARATOR + to.toString()
                + DictionaryReader.SUFFIX).toURI();
    }
    
    public static URI getDictionaryUri(Language from, Language to) {
        return new File(DictionaryReader.DEFAULT_PATH + from.toString() + 
                DictionaryReader.SEPARATOR + to.toString()
                + DictionaryReader.SUFFIX).toURI();
    }
}
