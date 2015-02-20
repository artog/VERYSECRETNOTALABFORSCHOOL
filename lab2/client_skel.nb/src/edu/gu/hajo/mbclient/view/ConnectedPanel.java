package edu.gu.hajo.mbclient.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.List;

import javax.swing.DefaultListModel;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.ListSelectionModel;
import javax.swing.border.TitledBorder;

import edu.gu.hajo.mbclient.core.ILocalClient;
import edu.gu.hajo.mbclient.event.Event;
import edu.gu.hajo.mbclient.event.IEventHandler;

/**
 * A list of connected users
 * @author hajo
 *     
 */
@SuppressWarnings("serial")
public class ConnectedPanel extends JPanel implements IEventHandler {
   
    private final JFrame parent;
    private final ILocalClient client;
    
    public ConnectedPanel(JFrame parent, ILocalClient client) {
        this.parent = parent;
        this.client = client;
        init();
    }
     
    @SuppressWarnings("unchecked")
    @Override
    public void onEvent(Event evt) {
        if( evt.getTag() == Event.Tag.UPDATE_USERS ){
            model.clear();
            List<String> userNames = (List<String>) evt.getValue();
            for(String name : userNames){
                model.addElement(name);
            }
        }else if( evt.getTag() == Event.Tag.CONNECTED ){
            connected.setEnabled(true);
        }else if( evt.getTag() == Event.Tag.DISCONNECTED ){
            model.clear();
            connected.setEnabled(true);
        }
    }
    
    private final DefaultListModel<String> model = new DefaultListModel<>();
    private JList<String> connected = new JList<>(model);

    private void init() {
        this.setLayout(new BorderLayout());
        connected.setBorder(new TitledBorder("Connected"));
        connected.setModel(model);
        connected.setEnabled(false);
        connected.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        connected.addMouseListener(listener);
        this.setPreferredSize(new Dimension(100, 300));
        this.add(connected, BorderLayout.CENTER);
       
    }
    
    private MouseListener listener = new MouseAdapter(){
        @Override
        public void mouseClicked(MouseEvent e) {
            if( e.getClickCount() == 2 ){
               String other =  (String) connected.getSelectedValue();
               //if( !other.equals(Options.INSTANCE.getUser().getName())){
                   new PeerDialog(parent, other, client).setVisible(true);
               //}    
            }
        }

  
    };

  

}
