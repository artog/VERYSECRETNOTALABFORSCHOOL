package edu.gu.hajo.chat.server.spec;

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
}
