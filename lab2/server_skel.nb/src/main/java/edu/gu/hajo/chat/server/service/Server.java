package edu.gu.hajo.chat.server.service;

import edu.gu.hajo.chat.server.core.Chat;
import edu.gu.hajo.chat.server.core.ChatMessage;
import edu.gu.hajo.chat.server.core.Constants;
import edu.gu.hajo.chat.server.spec.IChatClient;
import edu.gu.hajo.chat.server.spec.IChatServer;
import edu.gu.hajo.chat.server.core.User;
import edu.gu.hajo.chat.server.spec.IMessage;
import edu.gu.hajo.chat.server.spec.IPeer;
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
            List<String> deadClients = new ArrayList();

            clients.forEach((login, client) -> {
                try {
                    client.ping();
                } catch (Exception ex) {
                    deadClients.add(login);
                }
            });

            deadClients.forEach(login -> disconnectUser(login));
        }
    };
    

    @Override
    public void message(User sender, String msg){
        IMessage message = new ChatMessage(null, sender, msg);
        
        clients.forEach((login, client) -> {
            try {
                client.recieve(message);
            } catch (RemoteException ex) {
                disconnectUser(login);
            }
        });
    }

    @Override
    public User connect(IChatClient client, String login, String password) throws RemoteException {
        User user = chat.login(login, password);
        if(user != null){
            clients.put(login, client);
            
            for (IChatClient c : clients.values()) {
                c.userJoined(login);
            }

            LOG.log(Level.INFO, "{0} has connected.", login);
        }
        else{
            LOG.log(Level.INFO, "Failed to login.");
        }
        
        return user;
    }

    @Override
    public void disconnect(User user) throws RemoteException {
        disconnectUser(user.getLogin());
    }
    
    private void disconnectUser(String key) {
        chat.logout(chat.getUser(key));
        clients.remove(key);
        synchronized (clients) {
            for (IChatClient c : clients.values()) {
                try {
                    c.userLeft(key);
                } catch (RemoteException ex) {
                    // Skip, ping will take care of dead client.
                }
            }
        }
        LOG.log(Level.INFO, "{0} has disconnected.", key);
    }

    @Override
    public List<String> getFilelistFromUser(String username) throws RemoteException {
        User user = chat.getUser(username);
        if (chat.isLoggedIn(user)) {
            return clients.get(username).getFilelist();
        } else {
            return null;
        }
    }
    
    public IPeer getUserForFile(String name) {
        return (IPeer) clients.get(name);
    }

}

