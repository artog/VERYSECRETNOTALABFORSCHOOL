package edu.gu.hajo.chat.server.spec;

import edu.gu.hajo.chat.server.core.User;
import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Client view of server
 *
 * @author hajo
 *
 */
public interface IChatServer extends Remote {

    // Utility
    public String ping() throws RemoteException;
    
    public User connect(IChatClient client) throws RemoteException;
    public void sendMessage(String msg) throws RemoteException;
}
