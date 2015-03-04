package edu.gu.hajo.chat.server.service;

import edu.gu.hajo.chat.server.core.Chat;
import edu.gu.hajo.chat.server.core.Constants;
import edu.gu.hajo.chat.server.spec.IChatClient;
import edu.gu.hajo.chat.server.spec.IPeer;
import edu.gu.hajo.chat.server.spec.IChatServer;
import edu.gu.hajo.chat.server.core.User;
import edu.gu.hajo.chat.server.spec.IMessage;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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
    }

    // ------- IServer ---------------------------------------------
    @Override
    public synchronized String ping() throws RemoteException {
        LOG.log(Level.INFO, "Pinged!");
        return Constants.SERVER_PING_MESSAGE;
    }

    // --------- Privates -----------
    // Clients alive?
    private final TimerTask pinger = null;

    @Override
    public void sendMessage(String msg) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
