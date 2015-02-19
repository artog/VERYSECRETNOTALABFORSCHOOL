package edu.gu.hajo.translator.ctrl;


import edu.gu.hajo.dict.DictionaryFactory;
import edu.gu.hajo.dict.core.Language;
import edu.gu.hajo.translator.core.EventTranslator;
import edu.gu.hajo.translator.core.ITranslator;
import edu.gu.hajo.translator.event.EventBus;
import edu.gu.hajo.translator.event.Event;

/**
 * Handles interaction between GUI and Translator
 *
 * Will create a new Translator at language switch
 * 
 * @author hajo
 *
 */
public class LanguageCtrl {

    private Language from;
    private Language to;
    private ITranslator translator;

    public LanguageCtrl(){
        this.from = Language.xx_XX;
        this.to = Language.xx_XX;
        translator = new EventTranslator(from, to);
    }
    
    public void setFrom(Language from) {
        if (from != null && !this.from.equals(from)) {   // Mysterious NullPointerException!
            this.from = from;
            EventBus.INSTANCE.publish(new Event(Event.Tag.KEYBOARD_CHANGE, from));
            load();
        }
    }

    public void setTo(Language to) {
        if (to != null && !this.to.equals(to)) {      // Mysterious NullPointerException!
            this.to = to;
            load();
        }
    }

    private void load() {
       System.out.println("Load "+from+"->"+to);
       translator = new EventTranslator(from, to);
    }
}
