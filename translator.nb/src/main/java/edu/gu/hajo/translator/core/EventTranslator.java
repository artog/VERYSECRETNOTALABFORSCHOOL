package edu.gu.hajo.translator.core;

import edu.gu.hajo.dict.core.Language;
import edu.gu.hajo.translator.event.EventBus;
import edu.gu.hajo.translator.event.Event;

/**
 * Extend model class Translator with events (messaging)
 *
 * @author hajo
 */
public class EventTranslator extends Translator {

    public EventTranslator(Language from, Language to) {
        super(from, to);
    }
    
    @Override
    public void addToPrefix(char c) {
        super.addToPrefix(c);
        EventBus.INSTANCE.publish(new Event(Event.Tag.PREFIX_CHANGED, prefix));
    }

    @Override
    public void removeFromPrefix() {
        super.removeFromPrefix();
        EventBus.INSTANCE.publish(new Event(Event.Tag.PREFIX_CHANGED, prefix));
    }
    
    @Override
    public void clearPrefix(){
        super.clearPrefix();
        EventBus.INSTANCE.publish(new Event(Event.Tag.PREFIX_CHANGED, prefix));
    }
}
