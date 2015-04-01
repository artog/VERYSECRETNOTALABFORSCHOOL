package edu.gu.hajo.chat.server.spec;

import edu.gu.hajo.chat.server.io.ChatFile;
import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * One clients view of another (peer2peer)
 *
 * @author hajo
 *
 */
public interface IPeer extends Remote {
    public ChatFile getFile(String name) throws RemoteException;
}
