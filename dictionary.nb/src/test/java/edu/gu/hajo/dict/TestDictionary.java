package edu.gu.hajo.dict;


import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import edu.gu.hajo.dict.core.Dictionary;
import edu.gu.hajo.dict.core.IDictionary;
import edu.gu.hajo.dict.io.DictionaryReader;
import edu.gu.hajo.trie.ConnectableTrie;

/**
 *
 * @author hajo
 *
 */
public class TestDictionary {

    URI en2sv = new File(DictionaryReader.DEFAULT_PATH + "en_US2sv_SV.dict").toURI();
    URI sv2en = new File(DictionaryReader.DEFAULT_PATH + "sv_SV2en_US.dict").toURI();

    @Test
    public void testAddGetMatches() {
        List<String> vs = new ArrayList<>();
        vs.add("aaa");
        vs.add("bbb");
        vs.add("ccc");

        Dictionary d = new Dictionary(ConnectableTrie.newInstance(), ConnectableTrie.newInstance());
     
        // TODO 
        
        
    }

    @Test
    public void testGetMatchesForEmpty() throws URISyntaxException,
            MalformedURLException, IOException {
        IDictionary d = DictionaryFactory.getDictionary(sv2en);

        // TODO
    }

    @Test
    public void testGetMatchesForAWordInFile() throws URISyntaxException,
            MalformedURLException, IOException {
        IDictionary d = DictionaryFactory.getDictionary(sv2en);
       
        // TODO
       
    }
}
