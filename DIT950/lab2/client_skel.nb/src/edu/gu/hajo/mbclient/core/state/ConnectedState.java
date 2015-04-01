package edu.gu.hajo.mbclient.core.state;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

import edu.gu.hajo.mbcommon.IClient;
import edu.gu.hajo.mbcommon.IMessage;
import edu.gu.hajo.mbcommon.IPeer;
import edu.gu.hajo.mbcommon.IServer;

/**
 * Used by Client when connected
 * 
 * @author hajo
 * 
 */
class ConnectedState implements IState {

    // This is never null!
    private StateContext ctx;

    public ConnectedState(StateContext ctx) {
        this.ctx = ctx;
    }

    public void broadcast(IMessage msg) throws RemoteException {
        ctx.getServer().broadcast(msg);
    }

    public void connect(IClient client) throws RemoteException {
        // Silly but needed
    }

    public void disconnect(IClient client) throws RemoteException {
        try {
            ctx.getServer().unRegister(client);
        } finally {
            // Should always be done!
            UnicastRemoteObject.unexportObject(client, true);
        }
    }

    @Override
    public IPeer getPeer(String otherName) throws RemoteException {
        IServer server = ctx.getServer();
        return (IPeer) server.getClient(otherName);
    }
}
