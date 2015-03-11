package edu.gu.hajo.chat.server.service;

import edu.gu.hajo.chat.server.core.Chat;
import edu.gu.hajo.chat.server.core.Constants;
import edu.gu.hajo.chat.server.spec.IChatClient;
import edu.gu.hajo.chat.server.spec.IChatServer;
import edu.gu.hajo.chat.server.core.User;
import java.rmi.ConnectException;
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This class adds networking and notification to other clients by wrapping the
 * model (delegation). No application logic here
 *
 * @author hajo
 *
 */
public class Server implements IChatServer {

    // Usage, see ping below
    private static final Logger LOG = Logger.getLogger(Server.class.getName());
    private static final int PING_DELAY = 3000;

    private final Chat chat;

    private final Map<String,IChatClient> clients = new HashMap();
    
    public Server(Chat chat) {
        this.chat = chat;
        Timer pingTimer = new Timer(true);
        pingTimer.schedule(pinger, PING_DELAY, PING_DELAY);
    }

    // ------- IServer ---------------------------------------------
    @Override
    public synchronized String ping(){
        LOG.log(Level.INFO, "Pinged!");
        return Constants.SERVER_PING_MESSAGE;
    }

    // --------- Privates -----------
    // Clients alive?
    private final TimerTask pinger = new TimerTask() {

        @Override
        public void run() {
            synchronized (clients) {
                Set<String> keys = clients.keySet();
                
                for (String key : keys) {
                    try {
                        clients.get(key).ping();
                    
                    } catch (Exception ex) {
                        Logger.getLogger(Server.class.getName()).log(Level.SEVERE, "User connection failed.");
                        disconnectUser(key);
                    }
                }
            }
        }
        
    };

    @Override
    public void sendMessage(String msg){
        // TODO: stuff...
    }

    @Override
    public User connect(IChatClient client) throws RemoteException {
        User user = chat.login(client.getLogin(), client.getPassword());
        if(user != null){
            
            clients.put(
                client.getLogin(),
                client
            );
            
            LOG.log(Level.INFO, "{0} has connected.", client.getLogin());
        }
        else{
            LOG.log(Level.INFO, "Failed to login.");
        }
        
        return user;
    }

    @Override
    public void disconnect(IChatClient client) throws RemoteException {
        disconnectUser(client.getLogin());
    }
    
    
    
    
    
    private void disconnectUser(String key) {
        chat.logout(chat.getUser(key));
        clients.remove(key);
        LOG.log(Level.INFO, "{0} has disconnected.", key);
    }
}

