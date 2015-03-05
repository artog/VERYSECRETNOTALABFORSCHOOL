package edu.gu.hajo.chat.server.spec;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;

/**
 * Contract for client (from Server view). Call backs from server
 *
 * @author hajo
 *
 */
public interface IChatClient extends Remote {
    
    public void ping() throws RemoteException;
    
    public void userLeft(String user) throws RemoteException;
    public void userJoined(String user) throws RemoteException;

    public void message() throws RemoteException;
    
    public String getLogin() throws RemoteException;
    public String getPassword() throws RemoteException; //Very secure.
    
    public List<String> getFilelist() throws RemoteException;
}
