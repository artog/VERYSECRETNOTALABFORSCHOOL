package edu.gu.hajo.translator.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import edu.gu.hajo.dict.core.Language;
import edu.gu.hajo.translator.ctrl.LanguageCtrl;
import edu.gu.hajo.translator.event.Event;
import edu.gu.hajo.translator.event.IEventHandler;

/**
 * Panel for the combo boxes to select from and to language.
 *
 * @author hajo
 *
 */
@SuppressWarnings("serial")
public class LanguageSelectionPanel extends JPanel implements ActionListener, IEventHandler {

    private final JComboBox<SelectItem> fromBox = new JComboBox<>();
    private final JComboBox<SelectItem> toBox = new JComboBox<>();
    private final JLabel fromLabel = new JLabel("from");
    private final JLabel toLabel = new JLabel("to");
    private final LanguageCtrl languageCtrl;

    public LanguageSelectionPanel(LanguageCtrl languageCtrl) {
        this.languageCtrl = languageCtrl;
        init();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
     
    }

    @Override
    public void onEvent(Event evt) {
     
    }

    private void setSelected(JComboBox<SelectItem> cBox, Language lang) {
        // Else the combo box will fire
        cBox.removeActionListener(this);
        for (int i = 0; i < cBox.getItemCount(); i++) {
            SelectItem si = cBox.getItemAt(i);
            if (si.getValue() == lang) {
                cBox.setSelectedIndex(i);
                // Put back listener
                cBox.addActionListener(this);
                break;
            }
        }
    }

    private void init() {
        SelectItem[] selections = getSelectItems();
        fromBox.setModel(new DefaultComboBoxModel<>(selections));
        toBox.setModel(new DefaultComboBoxModel<>(selections));
        fromBox.addActionListener(this);
        toBox.addActionListener(this);
        this.add(fromLabel);
        this.add(fromBox);
        this.add(toLabel);
        this.add(toBox);
    }

    private SelectItem[] getSelectItems() {
        SelectItem[] selections = {
            new SelectItem("Select", Language.xx_XX),
            new SelectItem("English", Language.en_US),
            new SelectItem("Swedish", Language.sv_SV),
            new SelectItem("German", Language.de_DE)};
        return selections;
    }

    /**
     * Helper class. Used to populate ComboBoxes (key shown in GUI because of
     * overridden toString, but value used internal by application).
     */
    private class SelectItem {

        private final String key;
        private final Language lang;

        public SelectItem(String key, Language lang) {
            this.key = key;
            this.lang = lang;
        }

        // This is shown in GUI "the key"
        @Override
        public String toString() {
            return key;
        }

        // This is the selected item used internally by application
        public Language getValue() {
            return lang;
        }
    }

}
