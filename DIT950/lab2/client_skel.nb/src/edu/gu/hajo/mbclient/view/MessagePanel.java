package edu.gu.hajo.mbclient.view;

import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import edu.gu.hajo.mbclient.core.ILocalClient;
import edu.gu.hajo.mbclient.event.Event;
import edu.gu.hajo.mbclient.event.IEventHandler;
import edu.gu.hajo.mbcommon.IMessage;

@SuppressWarnings("serial")
public class MessagePanel extends JPanel implements ActionListener, IEventHandler {

    private final ILocalClient client;

    public MessagePanel(ILocalClient client){
        this.client = client;
        init();
    }

    private JTextArea receive = new JTextArea();
    private JTextField send = new JTextField();
    
    @Override
    public void onEvent(Event evt) {
        if( evt.getTag() == Event.Tag.NEW_MESSAGE ){
            IMessage m = (IMessage) evt.getValue();
            receive.append(m.getSender().getName() + ": " + m.getText() + "\n");
        }else if( evt.getTag() == Event.Tag.CONNECTED ){
            send.setEnabled(true);
            send.requestFocusInWindow();
        }else if( evt.getTag() == Event.Tag.DISCONNECTED ){
            receive.removeAll();
            send.setText("");
            send.setEnabled(false);
        }  
    }

    @Override
    public void actionPerformed(ActionEvent e) {
       String msg = send.getText(); 
       client.broadcast(msg);
       send.setText("");
    }
    
    private void init() {
        this.setLayout(new BorderLayout());
        receive.setBorder(new TitledBorder("Messages"));
        send.setBorder(new TitledBorder("Send"));
        send.addActionListener(this);
        send.setEnabled(false);
        receive.setEnabled(false);
        receive.setFont(new Font("Arial", Font.BOLD, 16));
        this.add( receive, BorderLayout.CENTER);
        this.add( send, BorderLayout.NORTH);
  }
    
}
