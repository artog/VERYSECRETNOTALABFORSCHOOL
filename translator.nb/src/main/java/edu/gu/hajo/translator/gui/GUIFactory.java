package edu.gu.hajo.translator.gui;

import edu.gu.hajo.translator.ctrl.LanguageCtrl;

import edu.gu.hajo.translator.event.EventBus;
import java.awt.BorderLayout;
import javax.swing.JPanel;

/**
 * Class responsible for build complete GUI and connect 
 * handlers to EventBus
 *
 * @author hajo
 *
 */
public final class GUIFactory {

    public static MainFrame getMainFrame() {

        DisplayPanel display = new DisplayPanel();

        KeyboardPanel keyboard = new KeyboardPanel();
        ButtonPanel buttons = new ButtonPanel();

        JPanel container = new JPanel();
        container.setLayout(new BorderLayout());
        container.add(keyboard, BorderLayout.CENTER);
        container.add(buttons, BorderLayout.EAST);

        LanguageSelectionPanel languageSelection
                = new LanguageSelectionPanel(new LanguageCtrl());

        MainFrame mf = new MainFrame(container, display, languageSelection);

        // Set as event listeners

        return mf;
    }
}
