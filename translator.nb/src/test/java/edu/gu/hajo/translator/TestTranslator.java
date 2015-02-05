package edu.gu.hajo.translator;


import org.junit.Test;

import edu.gu.hajo.dict.core.Language;
import edu.gu.hajo.translator.core.Translator;
import java.io.IOException;

public class TestTranslator {

    @Test
    public void testTranslator() throws IOException {
        Translator t = new Translator(Language.sv_SV, Language.en_US);

    }

}
