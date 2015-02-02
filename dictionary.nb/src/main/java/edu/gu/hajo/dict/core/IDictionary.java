package edu.gu.hajo.dict.core;

import java.util.List;

/**
 * Interface to module
 * 
 * @author Adam,Mikael
 * 
 */
public interface IDictionary {

    public List<DictionaryEntry> getEntries(String prefix);
    public DictionaryEntry       getEntry(String key);
}
