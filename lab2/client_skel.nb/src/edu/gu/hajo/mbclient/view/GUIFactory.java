package edu.gu.hajo.mbclient.view;

import javax.swing.JFrame;

import edu.gu.hajo.mbclient.core.ClientFactory;
import edu.gu.hajo.mbclient.core.ILocalClient;
import edu.gu.hajo.mbclient.event.EventBus;

/**
 * Creating the GUI and connect observers
 * @author hajo
 *
 */
public class GUIFactory {

    public static JFrame getGUI(){
        ILocalClient c = ClientFactory.getLocalClient();
         
        ControlPanel control = new ControlPanel(c);
        StatusPanel status = new StatusPanel();
        MessagePanel msg = new MessagePanel(c);
        ConnectedPanel conn = new ConnectedPanel(null, c);
        
        MainFrame mf = new MainFrame(control, status, msg, conn, c);  
        mf.setSize(400, 400);
        
        EventBus.INSTANCE.register(control);
        EventBus.INSTANCE.register(conn);
        EventBus.INSTANCE.register(msg);
        EventBus.INSTANCE.register(status);
        EventBus.INSTANCE.register(mf);
        
        return mf;
    }

}
