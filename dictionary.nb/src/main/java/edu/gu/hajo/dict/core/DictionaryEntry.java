package edu.gu.hajo.dict.core;

import java.util.ArrayList;
import java.util.List;

/**
 * A helper class holding one word in the source language
 * and a list of translations in the target language
 * @author hajo
 *
 */
public final class DictionaryEntry  {
	private final String source;
	private final List<String> translations;

	public DictionaryEntry(String source, List<String> translations) {
		this.source = source;
                List<String> t = new ArrayList();
                t.addAll(translations);
		this.translations = t;
	}
	
	public String getSource() {
		return source;
	}
        
        public List<String> getTranslations() {
            List<String> copy = new ArrayList();
            copy.addAll(this.translations);
            return copy;
        }
	// This is good for debug
	@Override
	public String toString() {
		return "[source=" + source + ", translations="
				+ translations + "]";
	}
}
