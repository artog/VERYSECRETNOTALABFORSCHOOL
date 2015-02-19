package edu.gu.hajo.translator.core;

import edu.gu.hajo.dict.DictionaryFactory;
import edu.gu.hajo.dict.core.DictionaryEntry;
import edu.gu.hajo.dict.core.IDictionary;
import edu.gu.hajo.dict.core.Language;
import java.net.URI;
import java.util.List;

/**
 * The main model class.
 *
 * @author hajo
 *
 */
public class Translator implements ITranslator {

    private IDictionary dictionary;
    protected String prefix;
    
    public Translator(Language from, Language to) {
        try {
            URI dictionaryUri = DictionaryFactory.getDictionaryUri(from, to);
            dictionary = DictionaryFactory.getDictionary(dictionaryUri);
        } catch (Exception ex) {
            dictionary = DictionaryFactory.getDummyDictionary();
        }
        prefix = Constants.EMPTY_STR;
    }

    @Override
    public void addToPrefix(char c) {
        prefix += c;
    }

    @Override
    public void removeFromPrefix() {
        if(!prefix.isEmpty()){
            prefix = prefix.substring(0, prefix.length() - 1);
        }
    }
    
    @Override
    public void clearPrefix(){
        prefix = Constants.EMPTY_STR;
    }

    @Override
    public String[] getTranslations() {
        List<DictionaryEntry> entries = dictionary.getEntries(prefix);
        String[] translations = new String[entries.size()];
        
        for(int i = 0; i < entries.size(); i++){
            translations[i] = entries.get(i).toString();
        }
        
        return translations;
    }
    
    // Used for testing.
    public String getPrefix(){
        return prefix;
    }
}
