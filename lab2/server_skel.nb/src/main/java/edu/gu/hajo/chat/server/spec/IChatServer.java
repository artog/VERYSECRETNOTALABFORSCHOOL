package edu.gu.hajo.chat.server.spec;

import edu.gu.hajo.chat.server.core.User;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;

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
    public void disconnect(IChatClient client) throws RemoteException;
    public void sendMessage(String msg) throws RemoteException;
    
    public List<String> getFilelistFromUser(String username) throws RemoteException;
    public IPeer getUserForFile(String name) throws RemoteException;
}
