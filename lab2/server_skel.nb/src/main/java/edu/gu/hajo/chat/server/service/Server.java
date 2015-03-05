package edu.gu.hajo.chat.server.service;

import edu.gu.hajo.chat.server.core.Chat;
import edu.gu.hajo.chat.server.core.Constants;
import edu.gu.hajo.chat.server.spec.IChatClient;
import edu.gu.hajo.chat.server.spec.IChatServer;
import edu.gu.hajo.chat.server.core.User;
import java.rmi.ConnectException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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
            Set<String> keys = clients.keySet();
            List<String> deadClients = new ArrayList();
            
            for (String key : keys) {
                try {
                    clients.get(key).ping();
                } catch (Exception ex) {
                    deadClients.add(key);
                    Logger.getLogger(Server.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
            for (String key : deadClients) {
                disconnectUser(key);
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
            for (IChatClient c : clients.values()) {
                c.userJoined(client.getLogin());
            }
            
            clients.put(
                client.getLogin(),
                client
            );
            
            for (Entry<String,IChatClient> e : clients.entrySet()) {
                client.userJoined(e.getKey());
            }
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
        synchronized (clients) {
            for (IChatClient c : clients.values()) {
                try {
                    c.userLeft(key);
                } catch (RemoteException ex) {
                    Logger.getLogger(Server.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
        LOG.log(Level.INFO, "{0} has disconnected.", key);
    }
}

