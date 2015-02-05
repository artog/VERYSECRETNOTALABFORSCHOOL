package edu.gu.hajo.translator.core;

import edu.gu.hajo.dict.core.Language;

/**
 * Extend model class Translator with events (messaging)
 *
 * @author hajo
 */
public class EventTranslator extends Translator {

    public EventTranslator(Language from, Language to) {
        super(from, to);
    }

}
