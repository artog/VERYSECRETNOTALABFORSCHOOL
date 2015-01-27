package edu.gu.hajo.dict;

import java.io.IOException;
import java.net.URI;
import edu.gu.hajo.dict.core.IDictionary;
import edu.gu.hajo.dict.core.Language;
import edu.gu.hajo.dict.io.DictionaryReader;
import java.io.File;

/**
 * A Factory for dictionaries
 *
 * @author hajo
 *
 */
public final class DictionaryFactory {

    public static IDictionary getDictionary(URI uri) throws IOException {
       return null;
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
