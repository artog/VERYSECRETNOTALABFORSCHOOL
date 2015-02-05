package edu.gu.hajo.translator.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.ListModel;

import edu.gu.hajo.translator.core.Constants;
import edu.gu.hajo.translator.event.Event;
import edu.gu.hajo.translator.event.IEventHandler;

/**
 * Panel for the input text field (word to translate) and the output (list of
 * translations)
 *
 * @author hajo
 *
 * TODO Localize (border)
 */
@SuppressWarnings("serial")
public class DisplayPanel extends JPanel implements IEventHandler {

    private final JTextField inPut = new JTextField();
    private final JList<String> outPut = new JList<>();
    private static final ListModel<String> EMPTY_MODEL = new DefaultListModel();

    public DisplayPanel() {
        init();
    }

    @Override
    public void onEvent(Event evt) {
     
    }

    private void clear() {
        outPut.setModel(EMPTY_MODEL);
        inPut.setText(Constants.EMPTY_STR);

    }

    private void init() {
        this.setLayout(new BorderLayout());
        this.setPreferredSize(new Dimension(200, 200));
        inPut.setEditable(false);
        this.add(inPut, BorderLayout.NORTH);
        this.add(outPut, BorderLayout.CENTER);

    }
}
