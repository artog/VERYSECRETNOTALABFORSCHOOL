package edu.gu.hajo.translator;


import org.junit.Test;

import edu.gu.hajo.dict.core.Language;
import edu.gu.hajo.translator.core.Constants;
import edu.gu.hajo.translator.core.Translator;
import java.io.IOException;
import static org.junit.Assert.assertTrue;

public class TestTranslator{

    @Test
    public void testAddRemovePrefix() throws IOException {
        Translator t = new Translator(Language.sv_SV, Language.en_US);
        
        t.addToPrefix('a');
        t.addToPrefix('b');
        assertTrue(t.getPrefix().equals("ab"));
        
        t.removeFromPrefix();
        assertTrue(t.getPrefix().equals("a"));
        
        t.removeFromPrefix();
        t.removeFromPrefix();
        assertTrue(t.getPrefix().equals(Constants.EMPTY_STR));
    }
    
    @Test
    public void testGetTranslations() throws IOException {
        Translator t = new Translator(Language.sv_SV, Language.en_US);
        t.addToPrefix('p');
        
        String[] translations = t.getTranslations();
        
        // Hardcoded values based on the language file we're reading from (not optimal).
        assertTrue(translations.length == 2);
        assertTrue(translations[1].contains("motorcar"));
        String[] ss = translations[1].replaceAll("\\s+", " ").split("\\s");
        assertTrue(ss.length == 5);
    }

}
