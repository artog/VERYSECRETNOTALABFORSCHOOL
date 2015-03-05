/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.chat.client.client;

import edu.gu.hajo.chat.client.util.ChatClientOptions;
import edu.gu.hajo.chat.server.core.Constants;
import edu.gu.hajo.chat.server.core.User;
import edu.gu.hajo.chat.server.service.Server;
import edu.gu.hajo.chat.server.spec.IChatClient;
import edu.gu.hajo.chat.server.spec.IChatServer;
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
    private StateContext context;
    private IChatServer server;
    private Client client;
    
    public Connected(StateContext context, Client client, IChatServer server){
        this.context = context;
        this.server = server;
        this.client = client;

        System.out.println("New Connected was born!"); //TODO: remove this
    }

    @Override
    public User connect(Client client) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void disconnect() {
        try {
            server.disconnect(client);
            UnicastRemoteObject.unexportObject(client, true);
        } catch (RemoteException ex) {
            Logger.getLogger(Connected.class.getName()).log(Level.SEVERE, null, ex);
            // Wat?
            ex.printStackTrace();
        }
        
        context.set(new Disconnected(context));
    }
}
