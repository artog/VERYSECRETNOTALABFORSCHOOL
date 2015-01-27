package edu.gu.hajo.dict;

import edu.gu.hajo.dict.core.IDictionary;
import edu.gu.hajo.dict.core.Language;
import edu.gu.hajo.dict.io.DictionaryReader;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class TestDictionaryFactory {

    @Test
    public void testGetLanguages() throws IOException, URISyntaxException {
        URI uri = DictionaryFactory.getDictionaryUri(DictionaryReader.DEFAULT_PATH,
                Language.en_US, Language.sv_SV);
        IDictionary d = DictionaryFactory.getDictionary(uri);
        assertTrue(d != null);
    }

}
