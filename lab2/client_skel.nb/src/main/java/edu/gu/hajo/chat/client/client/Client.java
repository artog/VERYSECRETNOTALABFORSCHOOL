package edu.gu.hajo.chat.client.client;

import edu.gu.hajo.chat.client.util.ChatClientOptions;

import edu.gu.hajo.chat.server.spec.IChatClient;
import edu.gu.hajo.chat.server.spec.IPeer;
import edu.gu.hajo.chat.server.core.User;
import java.io.Serializable;

import static edu.gu.hajo.chat.client.client.IObserver.Event;
import edu.gu.hajo.chat.client.exception.ChatClientException;
import edu.gu.hajo.chat.client.io.FileHandler;
import edu.gu.hajo.chat.server.io.ChatFile;
import edu.gu.hajo.chat.server.spec.IMessage;
import java.io.IOException;
import java.rmi.RemoteException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

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
    private final String LOGIN = ChatClientOptions.getLogin();
    private final String PASSWORD = ChatClientOptions.getPasswd();
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
        publishSwing(Event.USER_LEFT, user);
    }

    @Override
    public void userJoined(String user) {
        publishSwing(Event.USER_JOINED, user);
    }

    @Override
    public void send(String message){
        try{
            context.send(me, message);
        }
        catch(ChatClientException ex){
            publishSwing(Event.DISCONNECTED, "");
            publishSwing(Event.EXCEPTION, ex.getMessage());
        }
    }
    
    @Override
    public void recieve(IMessage message){
        publishSwing(Event.MESSAGE, message);
    }

    @Override
    public void connect() {
        me = context.connect(this, LOGIN, PASSWORD);
        
        if(me != null){
            publishSwing(Event.CONNECTED, me);
        }
    }

    @Override
    public void disconnect() {
        context.disconnect(me);
        publishSwing(Event.DISCONNECTED, "");
    }
    
    public void setUser(User user){
        me = user;
    }

    @Override
    public List<String> getFilelist() throws RemoteException {
        try {
            return FileHandler.listDirectoryContent(
                    ChatClientOptions.getUploadPath()
            );
        } catch (IOException ex) {
            Logger.getLogger(Client.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

    @Override
    public List<String> getFileListFromPeer(String peer) {
        return context.getFileListFromPeer(peer);
    }

    @Override
    public ChatFile getFile(String name) throws RemoteException {
        try {
            
            byte[] bytes = FileHandler.readFile(
                    ChatClientOptions.getUploadPath(),
                    name
            );
            ChatFile file = new ChatFile(name, bytes);
            return file;
        } catch (IOException ex) {
            Logger.getLogger(Client.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return null;
    }

    @Override
    public void download(String filename, String username) {
        try {
            context.download(filename,username);
        } catch (ChatClientException ex) {
            publishSwing(Event.EXCEPTION, ex.getMessage());
        }
    }
    
    
    
    
    
    

}
