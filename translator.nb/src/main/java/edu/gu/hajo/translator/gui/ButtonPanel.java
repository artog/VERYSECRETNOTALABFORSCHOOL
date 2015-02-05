package edu.gu.hajo.translator.gui;

import edu.gu.hajo.translator.core.ITranslator;
import edu.gu.hajo.translator.event.Event;
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
      
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      
    }

    private void init() {
        backspace.addActionListener(this);
        clear.addActionListener(this);
        this.setLayout(new GridLayout(3, 1));
        this.add(backspace);
        this.add(clear);
    }

}
