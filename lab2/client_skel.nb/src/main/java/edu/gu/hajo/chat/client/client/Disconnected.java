/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.chat.client.client;

import edu.gu.hajo.chat.client.util.ChatClientOptions;
import edu.gu.hajo.chat.server.core.Constants;
import edu.gu.hajo.chat.server.core.User;
import edu.gu.hajo.chat.server.spec.IChatServer;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

/**
 *
 * @author Mikael
 */
public class Disconnected implements IState{
    private StateContext context;
    
    
    public Disconnected(StateContext context){
        this.context = context;
        System.out.println("New Disonnected was born!"); //TODO: remove this
    }

    @Override
    public User connect(Client client) {
        try {
            Registry registry = LocateRegistry.getRegistry(
                ChatClientOptions.getConnection().getRegistryIp(),
                ChatClientOptions.getConnection().getRegistryPort()
            );
            
            IChatServer server = (IChatServer) registry.lookup(Constants.SERVER_NAME);
            
            UnicastRemoteObject.exportObject(client,
                    ChatClientOptions.getConnection().getMyPort());
            
            User user = server.connect(client);
            
            if(user != null){
                client.setUser(user);
                context.set(new Connected(context, client, server));
            }
            
        } catch (RemoteException | NotBoundException ex) {
            ex.printStackTrace();
        }
    }

    @Override
    public void disconnect() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
