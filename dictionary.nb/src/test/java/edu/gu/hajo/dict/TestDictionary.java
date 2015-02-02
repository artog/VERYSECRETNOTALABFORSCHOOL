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
import edu.gu.hajo.dict.core.DictionaryEntry;
import edu.gu.hajo.dict.core.IDictionary;
import edu.gu.hajo.dict.io.DictionaryReader;
import edu.gu.hajo.trie.ConnectableTrieFactory;
import static org.junit.Assert.assertTrue;
import org.junit.Before;

/**
 *
 * @author hajo
 *
 */
public class TestDictionary {

    URI en2sv = new File(DictionaryReader.DEFAULT_PATH + "en_US2sv_SV.dict").toURI();
    URI sv2en = new File(DictionaryReader.DEFAULT_PATH + "sv_SV2en_US.dict").toURI();
    
    @Before
    public void before() {
        
    }

    @Test
    public void testAddGetMatches() {
        List<String> vs = new ArrayList<>();
        vs.add("aaa");
        vs.add("bbb");
        vs.add("ccc");

        Dictionary d = new Dictionary(ConnectableTrieFactory.newInstance(), ConnectableTrieFactory.newInstance());
//     
//        for (String s : vs) {
//            List<String> matches = d.getTranslations(s);
//            
//        }
    }

    @Test
    public void testGetMatchesForEmpty() throws URISyntaxException,
            MalformedURLException, IOException {
        IDictionary d = DictionaryFactory.getDictionary(sv2en);
        DictionaryEntry de = d.getEntry("");
        assertTrue(de.getTranslations().isEmpty());
    }

    @Test
    public void testGetMatchesForAWordInFile()
            throws URISyntaxException,MalformedURLException, IOException 
    {
        IDictionary d = DictionaryFactory.getDictionary(sv2en);

        DictionaryEntry de = d.getEntry("personbil");
        List<String> matches = de.getTranslations();
        assertTrue(matches.size() == 4);
        assertTrue(matches.get(1).equals("motorcar"));
    }
    
    @Test
    public void testGetTranslationsByPrefix() 
        throws URISyntaxException,MalformedURLException, IOException 
    {
        IDictionary d = DictionaryFactory.getDictionary(sv2en);
        
        List<DictionaryEntry> result = d.getEntries("bi");
        assertTrue(result.size() == 6);
        assertTrue(result.get(1).getTranslations().size() == 4);
    }
}
 