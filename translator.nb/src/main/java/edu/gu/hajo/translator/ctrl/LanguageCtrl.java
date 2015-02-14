package edu.gu.hajo.translator.ctrl;


import edu.gu.hajo.dict.core.Language;

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

    public void setFrom(Language from) {
        if (this.from == null || !this.from.equals(from)) {   
            this.from = from;
            load();
        }
    }

    public void setTo(Language to) {
        if (this.to == null || !this.to.equals(to)) {   
            this.to = to;
            load();
        }
    }

    private void load() {
       System.out.println("Load "+from+"->"+to);
    }
}
