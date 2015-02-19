package edu.gu.hajo.translator.ctrl;


import edu.gu.hajo.dict.core.Language;
import edu.gu.hajo.translator.core.EventTranslator;
import edu.gu.hajo.translator.core.ITranslator;

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

    public void LanguageCtrl(){
        this.from = Language.xx_XX;
        this.to = Language.xx_XX;
    }
    
    public void setFrom(Language from) {
        //if (from != null && !this.from.equals(from)) {   // Mysterious NullPointerException!
            this.from = from;
            load();
        //}
    }

    public void setTo(Language to) {
        //if (to != null && !this.to.equals(to)) {      // Mysterious NullPointerException!
            this.to = to;
            load();
        //}
    }

    private void load() {
       System.out.println("Load "+from+"->"+to);
       translator = new EventTranslator(from, to);
    }
}
