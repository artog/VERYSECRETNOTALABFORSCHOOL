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
import edu.gu.hajo.translator.event.EventBus;
import edu.gu.hajo.translator.event.IEventHandler;
import java.util.List;

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
        Object o = evt.getValue();
        switch (evt.getTag()) {
            case PREFIX_CHANGED:
                if (o instanceof String) {
                    String newValue = (String) o;
                    inPut.setText(newValue);
                }
                break;
            case TRANSLATIONS_CHANGED:
                if (o instanceof List) {
                    List<Object> translations = (List)o;
                    if (translations.get(0) instanceof String) {
                        DefaultListModel<String> lm = new DefaultListModel<>();
                        for (Object translation : translations) {
                            lm.addElement(translation.toString());                            
                        }
                        outPut.setModel(lm);
                    }
                }
                break;
        }
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
        
        EventBus.INSTANCE.register(this);

    }
}
