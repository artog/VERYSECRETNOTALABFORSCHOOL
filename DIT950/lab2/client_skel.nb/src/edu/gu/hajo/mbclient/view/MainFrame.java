package edu.gu.hajo.mbclient.view;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import edu.gu.hajo.mbclient.core.ILocalClient;
import edu.gu.hajo.mbclient.event.Event;
import edu.gu.hajo.mbclient.event.IEventHandler;

/**
 * Client application main window. 
 * 
 * @author hajo
 */
@SuppressWarnings("serial")
public class MainFrame extends JFrame implements IEventHandler {

    private final ILocalClient client;

    public MainFrame(ControlPanel control, StatusPanel status, JPanel message,
            ConnectedPanel conn, ILocalClient client) {
        this.client = client;
        init();
        Container p = this.getContentPane();
        p.add(control, BorderLayout.NORTH);
        p.add(status, BorderLayout.SOUTH);
        p.add(message, BorderLayout.CENTER);
        p.add(conn, BorderLayout.EAST);
        this.setLocationRelativeTo(null);
        this.setSize(400, 400);
    }

    @Override
    public void onEvent(Event evt) {
        if (evt.getTag() == Event.Tag.EXCEPTION) {
            String msg = (String) evt.getValue();
            JOptionPane.showMessageDialog(this, msg, "Problems!",
                    JOptionPane.ERROR_MESSAGE);
        }

    }

    private final WindowListener listener = new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
            client.disconnect();
        }
    };

    private void init() {
        this.setTitle("MAY BAY");
        this.addWindowListener(listener);
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        this.setLayout(new BorderLayout());
    }

}
