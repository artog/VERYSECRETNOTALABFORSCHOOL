package edu.gu.hajo.chat.client.client;

import edu.gu.hajo.chat.client.util.ChatClientOptions;

import edu.gu.hajo.chat.server.spec.IChatClient;
import edu.gu.hajo.chat.server.spec.IPeer;
import edu.gu.hajo.chat.server.core.User;
import java.io.Serializable;

import static edu.gu.hajo.chat.client.client.IObserver.Event;
import edu.gu.hajo.chat.client.exception.ChatClientException;
import java.rmi.RemoteException;

/**
 * Implementation of many interfaces. Serializable important!
 *
 * @author hajo
 */
public class Client implements ILocalClient, IChatClient, IPeer,
        Serializable {

    private final transient StateContext context = new StateContext();

    // The logged in user
    private User me;
    private final String login = ChatClientOptions.getLogin();
    private final String passwd = ChatClientOptions.getPasswd();
    private final IObserver observer;

    public Client(IObserver observer) {
        this.observer = observer;
    }

    // ILocalClient ------------------------------------------
    // -------- IPeer --------------------
    // IClient -----------------------------------------------
    // -------------------------------------------------------------------
    // Handle over to Swing thread (from RMI thread)
    private void publishSwing(final Event event, final Object data) {
        observer.onEvent(event, data);
    }

    @Override
    public void ping() {
        // Just checking if its alive
    }

    @Override
    public void userLeft(String user) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void userJoined(String user) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void message(){
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void connect() {
        try {
            
            me = context.connect(this);

            if(me != null){
                publishSwing(Event.CONNECTED, me);
            }
        } catch (ChatClientException ex) {
            publishSwing(Event.EXCEPTION, ex.getMessage());
        }
    }

    @Override
    public void disconnect() {
        context.disconnect();
        publishSwing(Event.DISCONNECTED, "");
    }

    @Override
    public String getLogin() throws RemoteException {
        return login;
    }

    @Override
    public String getPassword() throws RemoteException {
        return passwd;
    }
    
    public void setUser(User user){
        me = user;
    }

}
