package edu.gu.hajo.chat.client;

import edu.gu.hajo.chat.client.util.ChatClientOptions;
import edu.gu.hajo.chat.client.client.Client;
import edu.gu.hajo.chat.client.client.IObserver;
import edu.gu.hajo.chat.server.core.Constants;
import edu.gu.hajo.chat.server.spec.IChatClient;
import edu.gu.hajo.chat.server.spec.IChatServer;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * To get a first contact
 * @author hajo
 */
public class TestServer {

    static IChatServer server;
    static IChatClient client;

    @BeforeClass

    public static void beforeClass() throws RemoteException, NotBoundException {
        Registry registry = LocateRegistry.getRegistry(
                ChatClientOptions.getConnection().getRegistryIp(),
                ChatClientOptions.getConnection().getRegistryPort());
        server = (IChatServer) registry.lookup(Constants.SERVER_NAME);
        client = new Client(new IObserver() {
            @Override
            public void onEvent(Event event, Object data) {
                System.out.println("Observer: " + event + ":" + data);
            }

        });
        UnicastRemoteObject.exportObject(client,
                ChatClientOptions.getConnection().getMyPort());
    }

    @Test
    public void testPing() throws RemoteException, NotBoundException {
        System.out.println("***" + server.ping() + "***");
    }

   
}
