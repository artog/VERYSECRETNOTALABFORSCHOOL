package edu.gu.hajo.mbclient.view;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.border.BevelBorder;

import edu.gu.hajo.mbclient.core.ILocalClient;
import edu.gu.hajo.mbclient.event.Event;
import edu.gu.hajo.mbclient.event.IEventHandler;

/**
 * Buttons
 * @author hajo
 *
 *               
 */
@SuppressWarnings("serial")
public class ControlPanel extends JPanel implements ActionListener, IEventHandler {

    private final ILocalClient client;
    
    public ControlPanel(ILocalClient client) {
        this.client = client;
        init();
        
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == connect) {
            client.connect();
        } else {
            client.disconnect();
        }
    }
    
    @Override
    public void onEvent(Event evt) {
        if( evt.getTag() == Event.Tag.CONNECTED){
            connect.setEnabled(false);
            disconnect.setEnabled(true);
        }else if( evt.getTag() == Event.Tag.DISCONNECTED){
            connect.setEnabled(true);
            disconnect.setEnabled(false); 
        }   
    }

    private JButton connect = new JButton("Connect");
    private JButton disconnect = new JButton("Disconnect");

    private void init() {
        add(connect);
        add(disconnect);
        disconnect.setEnabled(false);
        connect.addActionListener(this);
        disconnect.addActionListener(this);
        this.setBorder(new BevelBorder(BevelBorder.RAISED));
    }

   

}
