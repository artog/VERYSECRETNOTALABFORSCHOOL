package edu.gu.hajo.dict.core;

import java.util.List;

/**
 * Interface to module
 * 
 * @author Adam,Mikael
 * 
 */
public interface IDictionary {

	// What method(s) would the translator like to have???
    public List<String> getTranslations(String key);
    
    public List<String> getKeys(String prefix);
    
    public List<DictionaryEntry> getEntries(String prefix);
}
