package edu.gu.hajo.mbclient.core.state;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;

import edu.gu.hajo.mbcommon.IClient;
import edu.gu.hajo.mbcommon.IMessage;
import edu.gu.hajo.mbcommon.IPeer;

/**
 * The contract for the different states
 * @author hajo
 *
 */
public interface IState {
   public abstract void connect(IClient client) throws RemoteException, NotBoundException;
   public abstract void disconnect(IClient client) throws RemoteException;
   public abstract void broadcast(IMessage msg) throws RemoteException; 
   public abstract IPeer getPeer(String otherName)throws RemoteException; 
}
