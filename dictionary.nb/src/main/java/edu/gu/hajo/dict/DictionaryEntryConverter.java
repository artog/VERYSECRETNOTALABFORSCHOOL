package edu.gu.hajo.dict;

import java.util.Arrays;

import edu.gu.hajo.dict.core.DictionaryEntry;
import java.util.List;

/**
 * Utility to convert from/to strings 
 *
 * @author hajo
 *
 */
class DictionaryEntryConverter {

    public final static String wSepChar = "=";
    public final static String tSepChar = ",";

    // No onstances
    private DictionaryEntryConverter() {
    }

    public static DictionaryEntry toObject(String s) {
        // To problematic wich  swedish chars
        String[] splitLine = s.split(wSepChar);
        String word = splitLine[0].trim();
        String[] trans = splitLine[1].trim().split(tSepChar);
        return new DictionaryEntry(word, Arrays.asList(trans));
    }

    // The inverse, not implemented, not needed.
    public static String toString(DictionaryEntry e) {
        List<String> translations = e.getTranslations();
        String word = e.getSource();
        
        StringBuilder sb = new StringBuilder(word);
        sb.append("=");
        boolean first = true;
        for (String translation : translations) {
            if (!first) {
                sb.append(",");
            } else {
                first = false;
            }
            sb.append(translation);
            
        }
        return sb.toString();
    }

}
