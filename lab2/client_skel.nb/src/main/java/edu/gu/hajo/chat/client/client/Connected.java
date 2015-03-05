/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.chat.client.client;

import edu.gu.hajo.chat.client.util.ChatClientOptions;
import edu.gu.hajo.chat.server.core.Constants;
import edu.gu.hajo.chat.server.spec.IChatClient;
import edu.gu.hajo.chat.server.spec.IChatServer;
import java.net.ConnectException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Mikael
 */
public class Connected implements IState{
    private IChatServer server;
    private IChatClient client;
    
    public Connected(IChatClient client){

        try {
            Registry registry = LocateRegistry.getRegistry(
                ChatClientOptions.getConnection().getRegistryIp(),
                ChatClientOptions.getConnection().getRegistryPort()
            );
            
            server = (IChatServer) registry.lookup(Constants.SERVER_NAME);
            this.client = client;
            
            UnicastRemoteObject.exportObject(client,
                    ChatClientOptions.getConnection().getMyPort());
            
        } catch (RemoteException ex) {
            ex.printStackTrace();
        } catch (NotBoundException ex){
            ex.printStackTrace();
        }
        
        System.out.println("New Connected was born!"); //TODO: remove this
    }
}
