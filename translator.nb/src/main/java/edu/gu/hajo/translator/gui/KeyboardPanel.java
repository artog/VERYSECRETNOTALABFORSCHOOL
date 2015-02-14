package edu.gu.hajo.translator.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import edu.gu.hajo.dict.core.Language;
import edu.gu.hajo.translator.core.ITranslator;
import edu.gu.hajo.translator.event.Event;
import edu.gu.hajo.translator.event.IEventHandler;
import java.awt.Font;
import java.awt.Insets;

/**
 * A keyboard
 *
 * @author hajo
 *
 */
@SuppressWarnings("serial")
public class KeyboardPanel extends JPanel implements ActionListener,
        IEventHandler {

    
    // TODO Should of course be in some file
    private final static String KEYS_ENGLISH = "q w e r t y u i o p a s d f g h j k l z x c v b n m";
    private final static String KEYS_SWEDISH = "q w e r t y u i o p å a s d f g h j k l ö ä z x c v b n m";
    private final static int ROWS = 3;
    private final static int COLS = 14;
    public static final Font font = new Font("Serif", Font.BOLD, 12);
     public static final Insets insets =  new Insets(2,2,2,2);
    private ITranslator translator;
    private JButton[] buttons;

    public KeyboardPanel() {
        init();
        initKeys(Language.sv_SV);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      String cmd = e.getActionCommand();
      if (cmd.length() > 0) {
          char c = cmd.charAt(0);
          translator.addToPrefix(c);
      }
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

    public final void init() {
        GridLayout gridLayout = new GridLayout(ROWS, COLS);
        gridLayout.setHgap(2);
        gridLayout.setVgap(2);
        setLayout(gridLayout);
        setBorder(new LineBorder(Color.BLACK, 1));
        setPreferredSize(new Dimension(300, 80));
    }

    private void initKeys(Language lang) {
        if (buttons != null) {
            for (JButton button : buttons) {
                this.remove(button);
            }
        }
        String keys = getKeys(lang);
        String[] ls = keys.split(" ");
        buttons = new JButton[ls.length];
        int i;
        for (i = 0; i < ls.length; i++) {
            JButton btn = new JButton(ls[i]);
            btn.setFont(font);
            btn.setMargin(insets);
            btn.addActionListener(this);
            buttons[i] = btn;
            this.add(btn, null);
        }
       validate(); // To update gui
    }

    private String getKeys(Language lang) {
        switch (lang) {
            case en_US:
                return KEYS_ENGLISH;
            case sv_SV:
                return KEYS_SWEDISH;
            default:
                return KEYS_ENGLISH;
        }
    }
}
