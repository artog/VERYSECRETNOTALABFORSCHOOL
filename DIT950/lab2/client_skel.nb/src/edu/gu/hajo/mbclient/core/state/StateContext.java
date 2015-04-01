package edu.gu.hajo.mbclient.core.state;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;

import edu.gu.hajo.mbcommon.IClient;
import edu.gu.hajo.mbcommon.IMessage;
import edu.gu.hajo.mbcommon.IPeer;
import edu.gu.hajo.mbcommon.IServer;

/**
 * Handles the states and transitions between them
 * 
 * @author hajo
 * 
 */
final class StateContext implements IState {

    private final IState disconnected = new DisconnectedState(this);
    private final IState connected = new ConnectedState(this);
    private IState current = disconnected;
    private static IServer server;

    // Failure atomicity
    public void connect(IClient client) throws RemoteException,
            NotBoundException {
        current.connect(client);
        current = connected;
    }

    public void disconnect(IClient client) throws RemoteException {
        current.disconnect(client);
    }

    // Must throw to inform Client to reset
    public void broadcast(IMessage msg) throws RemoteException {
        current.broadcast(msg);
    }

    public IPeer getPeer(String other) throws RemoteException {
        return current.getPeer(other);
    }

    // Used by actual states
    public IServer getServer() {
        return server;
    }

    public void setServer(IServer server) {
        StateContext.server = server;
    }
}
