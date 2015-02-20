package edu.gu.hajo.dict.io;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import junit.framework.Assert;

import org.junit.Test;

/**
 * Test for the DictionaryReader
 *
 * @author hajo
 *
 */
public class TestDictionaryReader {

    @Test
    public void testLoadFromFile() throws URISyntaxException,
            MalformedURLException, IOException {
        URI en2sv = new File(DictionaryReader.DEFAULT_PATH + "en_US2sv_SV.dict").toURI();
        // URI will look like ...
        // file:/home/hajo/courses/oo2/ass/ass.src/dictionary/dictionary.nb/src/main/resources/dict/en_US2sv_SV.dict
        //System.out.println(en2sv);
        List<String> data = DictionaryReader.INSTANCE.open(en2sv);
        Assert.assertTrue(data != null);
        for (String s : data) {
            System.out.println(s);
        }
    }

    @Test
    public void testLoadViaHttp() throws URISyntaxException,
            MalformedURLException, IOException {
        URI uri = new URI("http://www.cse.chalmers.se/~hajo/en_US2sv_SV.dict");
        List<String> data = DictionaryReader.INSTANCE.open(uri);
        Assert.assertTrue(data != null);
        for (String s : data) {
            System.out.println(s);
        }
    }

}
