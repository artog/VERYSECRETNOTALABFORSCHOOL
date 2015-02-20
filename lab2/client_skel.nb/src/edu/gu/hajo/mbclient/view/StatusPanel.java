package edu.gu.hajo.mbclient.view;

import java.awt.BorderLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.BevelBorder;

import edu.gu.hajo.mbclient.core.Options;
import edu.gu.hajo.mbclient.event.Event;
import edu.gu.hajo.mbclient.event.IEventHandler;

/**
 * Status message
 * @author hajo
 * 
 *      **** NOTHING TO DO HERE ****
 *
 */
@SuppressWarnings("serial")
public class StatusPanel extends JPanel implements IEventHandler {

    public StatusPanel() {
        init();
    }

    @Override
    public void onEvent(Event evt) {
        if (evt.getTag() == Event.Tag.CONNECTED) {
            status.setText("Status: Connected as "
                    + Options.INSTANCE.getUser().getName());
        } else if (evt.getTag() == Event.Tag.DISCONNECTED) {
            status.setText("Status: Disconnected");
        }
    }

    private JLabel status = new JLabel("Status");
    private JLabel msg = new JLabel("");

    private void init() {
        this.setLayout(new BorderLayout());
        this.add(status, BorderLayout.WEST);
        this.add(msg, BorderLayout.CENTER);
        this.setBorder(new BevelBorder(BevelBorder.RAISED));
    }
}
