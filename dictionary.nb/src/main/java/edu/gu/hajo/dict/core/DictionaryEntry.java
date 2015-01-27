package edu.gu.hajo.dict.core;

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
		this.translations = translations;
	}
	
	public String getSource() {
		return source;
	}
        
	// This is good for debug
	@Override
	public String toString() {
		return "[source=" + source + ", translations="
				+ translations + "]";
	}
}
