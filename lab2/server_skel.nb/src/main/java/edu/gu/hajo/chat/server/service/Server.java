package edu.gu.hajo.chat.server.service;

import edu.gu.hajo.chat.server.core.Chat;
import edu.gu.hajo.chat.server.core.Constants;
import edu.gu.hajo.chat.server.spec.IChatClient;
import edu.gu.hajo.chat.server.spec.IChatServer;
import edu.gu.hajo.chat.server.core.User;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;
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

    private final List<IChatClient> clients = new ArrayList();
    
    public Server(Chat chat) {
        this.chat = chat;
        Timer pingTimer = new Timer(true);
        pingTimer.schedule(pinger, 0, PING_DELAY);
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
                for (int i = 0; i < clients.size();i++) {
                    try {
                        clients.get(i).ping();
                    } catch (RemoteException ex) {
                        clients.remove(i);
                        i--;
                        Logger.getLogger(Server.class.getName()).log(Level.SEVERE, null, ex);
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
            clients.add(client);
            System.out.println(client.getLogin() + " has connected.");
        }
        else{
            System.out.println("Failed attempt to login."); 
        }
        
        return user;
    }
}

