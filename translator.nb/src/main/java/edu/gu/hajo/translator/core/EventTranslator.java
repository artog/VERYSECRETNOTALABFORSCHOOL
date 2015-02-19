package edu.gu.hajo.translator.core;

import edu.gu.hajo.dict.core.Language;
import edu.gu.hajo.translator.event.EventBus;
import edu.gu.hajo.translator.event.Event;
import edu.gu.hajo.translator.exception.TranslatorException;

/**
 * Extend model class Translator with events (messaging)
 *
 * @author hajo
 */
public class EventTranslator extends Translator {

    public EventTranslator(Language from, Language to) {
        super(from, to);
        EventBus.INSTANCE.publish(new Event(Event.Tag.LANG_CHANGED, this));
    }
    
    @Override
    public void addToPrefix(char c) {
        super.addToPrefix(c);
        EventBus.INSTANCE.publish(new Event(Event.Tag.PREFIX_CHANGED, prefix));
        EventBus.INSTANCE.publish(new Event(Event.Tag.TRANSLATIONS_CHANGED, getTranslations()));
    }

    @Override
    public void removeFromPrefix() {
        super.removeFromPrefix();
        EventBus.INSTANCE.publish(new Event(Event.Tag.PREFIX_CHANGED, prefix));
        EventBus.INSTANCE.publish(new Event(Event.Tag.TRANSLATIONS_CHANGED, getTranslations()));
    }
    
    @Override
    public void clearPrefix(){
        super.clearPrefix();
        EventBus.INSTANCE.publish(new Event(Event.Tag.PREFIX_CHANGED, prefix));
        EventBus.INSTANCE.publish(new Event(Event.Tag.TRANSLATIONS_CHANGED, getTranslations()));
    }
}
