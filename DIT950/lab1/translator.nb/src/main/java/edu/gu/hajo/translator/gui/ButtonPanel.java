package edu.gu.hajo.translator.gui;

import edu.gu.hajo.translator.core.ITranslator;
import edu.gu.hajo.translator.event.Event;
import edu.gu.hajo.translator.event.EventBus;
import edu.gu.hajo.translator.event.IEventHandler;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;

/**
 * Small panel for two buttons, clear and backspace
 *
 * @author hajo
 *
 */
@SuppressWarnings("serial")
public class ButtonPanel extends JPanel implements ActionListener, IEventHandler {

    private final JButton backspace = new JButton("Backsp");
    private final JButton clear = new JButton("Clear");
    private ITranslator translator;

    public ButtonPanel() {
        init();
    }

    @Override
    public void onEvent(Event evt) {
        switch (evt.getTag()) {
            case LANG_CHANGED:
                if (evt.getValue() instanceof ITranslator) {
                    translator = (ITranslator) evt.getValue();
                }
                break;
        }
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        String cmd = e.getActionCommand();
        switch(cmd.toUpperCase()){
            case "BACKSP":
                translator.removeFromPrefix();
                break;
            case "CLEAR":
                translator.clearPrefix();
                break;
            default:
                System.out.println("Error: Invalid ButtonPanel command!");
        }
    }

    private void init() {
        EventBus.INSTANCE.register(this);
        backspace.addActionListener(this);
        clear.addActionListener(this);
        this.setLayout(new GridLayout(3, 1));
        this.add(backspace);
        this.add(clear);
    }

}
