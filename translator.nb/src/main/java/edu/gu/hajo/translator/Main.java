package edu.gu.hajo.translator;

import edu.gu.hajo.dict.core.Language;
import edu.gu.hajo.translator.core.EventTranslator;
import edu.gu.hajo.translator.core.ITranslator;
import javax.swing.SwingUtilities;

import edu.gu.hajo.translator.gui.GUIFactory;
import edu.gu.hajo.translator.gui.MainFrame;
import java.lang.reflect.InvocationTargetException;
import javax.swing.JOptionPane;

/**
 * Application entry point
 *
 * @author hajo
 *
 */
public class Main {

    public static void main(String[] args) {
        
        // Construct GUI 
        try {
            SwingUtilities.invokeAndWait(new Runnable() {
                @Override
                public void run() {
                    MainFrame mf = GUIFactory.getMainFrame();
                    mf.setLocationRelativeTo(null);
                    mf.setVisible(true);

                }
            });

        } catch (InterruptedException | InvocationTargetException ex) {
            JOptionPane.showMessageDialog(null, "Application couldn't start. Sorry!");
            System.exit(1);
        }
        Language from = Language.sv_SV;
        Language to = Language.en_US;

        // Build model
        ITranslator translator = new EventTranslator(from, to);

        // Notify GUI 
        /*
        EventBus.INSTANCE.publish(
                new Event(Event.Tag.LANG_CHANGED, TODO));
        EventBus.INSTANCE.publish(
                new Event(Event.Tag.KEYBOARD_CHANGED, TODO )));
                */
    }
}
