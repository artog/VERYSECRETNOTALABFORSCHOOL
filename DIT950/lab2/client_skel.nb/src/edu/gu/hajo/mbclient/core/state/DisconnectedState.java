package edu.gu.hajo.mbclient.core.state;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

import edu.gu.hajo.mbclient.core.Options;
import edu.gu.hajo.mbcommon.Constants;
import edu.gu.hajo.mbcommon.IClient;
import edu.gu.hajo.mbcommon.IMessage;
import edu.gu.hajo.mbcommon.IPeer;
import edu.gu.hajo.mbcommon.IServer;

/**
 * Used by Client when disconnected
 * @author hajo
 *
 */
class DisconnectedState implements IState {
  
  // This is never null! 
  private final StateContext ctx;
   
  public DisconnectedState(StateContext ctx) {
     this.ctx = ctx;
   }

  public void broadcast(IMessage msg) {
    throw new IllegalStateException("Not connected");
  }

  /**
   * Failure atomicity is used so if fail
   * nothing needs to be reset. Just inform
   * user (GUI) i.e. tunnel the exception
 * @throws RemoteException 
 * @throws NotBoundException 
   */
  public void connect(IClient client) throws RemoteException, NotBoundException { 
      /*
       *  Order matters to preserve failure atomicity! 
       */
       Registry registry = LocateRegistry.getRegistry(
               Options.INSTANCE.getConnection().getRegistryIp(), 
               Options.INSTANCE.getConnection().getRegistryPort());
      IServer server = (IServer) registry.lookup(Constants.MB_SERVER_NAME);
      
  
      // Will not be exported if exception above...
      UnicastRemoteObject.exportObject(client, 
              Options.INSTANCE.getConnection().getMyPort());
      
      server.register(client);    
      
      // Used by other states
      ctx.setServer(server);
  }

  public void disconnect(IClient client) {
    // Do nothing it's not an Exception
  }

  public IPeer getPeer(String otherName) {
    // Shouldn't happen
    throw new IllegalStateException("Not connected");
  }
}
