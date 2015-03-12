/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.chat.client.client;

import com.sun.istack.internal.logging.Logger;
import edu.gu.hajo.chat.client.exception.ChatClientException;
import edu.gu.hajo.chat.client.util.ChatClientOptions;
import edu.gu.hajo.chat.server.core.Constants;
import edu.gu.hajo.chat.server.core.User;
import edu.gu.hajo.chat.server.spec.IChatServer;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.List;
import java.util.logging.Level;

/**
 *
 * @author Mikael
 */
class Disconnected implements IState{
    private final StateContext context;
    
    
    public Disconnected(StateContext context){
        this.context = context;
    }

    @Override
    public User connect(Client client, String login, String password) {
        User user = null;
        try {
            Registry registry = LocateRegistry.getRegistry(
                ChatClientOptions.getConnection().getRegistryIp(),
                ChatClientOptions.getConnection().getRegistryPort()
            );
            
            IChatServer server = (IChatServer) registry.lookup(Constants.SERVER_NAME);
                        
            UnicastRemoteObject.exportObject(client,
                    ChatClientOptions.getConnection().getMyPort());
            
            user = server.connect(client, login, password);
            
            if(user != null){
                context.set(new Connected(context, client, server));
                user.getMessages().forEach(msg -> client.recieve(msg));
            }
            
        } catch (RemoteException | NotBoundException ex) {
            throw new ChatClientException("Unable to reach server.");
        }
        return user;
    }

    @Override
    public void disconnect(User userS) {
        throw new ChatClientException("Not connected to server.");
    }
    
    @Override
    public void send(User sender, String message){
        throw new ChatClientException("Disconnected from server.");
    }

    @Override
    public List<String> getFileListFromPeer(String peer) {
        throw new ChatClientException("Not connected to server.");
    }

    @Override
    public void download(String filename, String username) {
        throw new ChatClientException("Not connected to server.");
    }
    
     
    
}
