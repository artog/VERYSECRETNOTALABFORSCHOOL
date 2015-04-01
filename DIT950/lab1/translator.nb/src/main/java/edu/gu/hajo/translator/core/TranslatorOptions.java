package edu.gu.hajo.translator.core;

import edu.gu.hajo.dict.DictionaryFactory;
import edu.gu.hajo.dict.core.Language;
import java.net.URI;

/**
 * Options possible set by end user (path to dictionary data)
 *
 * @author hajo
 */
public class TranslatorOptions {

    private TranslatorOptions() {
    }

    // Get from default path
    public static URI getDictionaryUri(Language from, Language to) {
        return DictionaryFactory.getDictionaryUri(from, to);
    }

    public static URI getDictionaryUri(String path, Language from, Language to) {
        return DictionaryFactory.getDictionaryUri(path, from, to);
    }
}
