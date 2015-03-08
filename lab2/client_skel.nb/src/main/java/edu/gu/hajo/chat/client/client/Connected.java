/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.chat.client.client;

import edu.gu.hajo.chat.client.exception.ChatClientException;
import edu.gu.hajo.chat.client.io.FileHandler;
import edu.gu.hajo.chat.client.util.ChatClientOptions;
import edu.gu.hajo.chat.server.core.Constants;
import edu.gu.hajo.chat.server.core.User;
import edu.gu.hajo.chat.server.io.ChatFile;
import edu.gu.hajo.chat.server.service.Server;
import edu.gu.hajo.chat.server.spec.IChatClient;
import edu.gu.hajo.chat.server.spec.IChatServer;
import edu.gu.hajo.chat.server.spec.IPeer;
import java.io.IOException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingWorker;


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
    public User connect(Client client, String login, String password) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void disconnect(User user) {
        try {
            server.disconnect(user);
            UnicastRemoteObject.unexportObject(client, true);
        } catch (RemoteException ex) {
            Logger.getLogger(Connected.class.getName()).log(Level.SEVERE, null, ex);
            // Wat?
            ex.printStackTrace();
        }
        
        context.set(new Disconnected(context));
    }
    
    @Override
    public void send(User sender, String message){
        try {
            server.message(sender, message);
        } catch (RemoteException ex) {
            Logger.getLogger(Connected.class.getName()).log(Level.SEVERE, null, ex); // TODO: Change this!
        }
    }

    @Override
    public List<String> getFileListFromPeer(String peer) {
        try {
            return server.getFilelistFromUser(peer);
        } catch (RemoteException ex) {
            Logger.getLogger(Connected.class.getName()).log(Level.SEVERE, null, ex);
            return null;
        }
    }

    @Override
    public void download(final String filename,final  String username) 
            throws ChatClientException
    {
        final IPeer peer;
        try {
            peer = server.getUserForFile(username);
            class Downloader extends SwingWorker<Void, Object> {
                @Override
                public Void doInBackground() {
                    try {
                        ChatFile file = peer.getFile(filename);
                        FileHandler.saveFile(
                                ChatClientOptions.getDownloadPath(), 
                                filename, 
                                file.getBytes()

                        );
                    } catch (RemoteException ex) {
                        Logger.getLogger(Connected.class.getName()).log(Level.SEVERE, "Unable to download file.", ex);
                        throw new ChatClientException("Unable to download file.");
                    } catch (IOException ex) {
                        Logger.getLogger(Connected.class.getName()).log(Level.SEVERE, "Unable to save file to disk.", ex);
                        throw new ChatClientException("Unable to save file to disk.");
                    }
                    return null;
                }

                @Override
                protected void done() {

                }
            }

            (new Downloader()).execute();
            
        } catch (RemoteException ex) {
            Logger.getLogger(Connected.class.getName()).log(Level.SEVERE, "Unable to connect to client.", ex);
            throw new ChatClientException("Unable to connect to client.");
        }
            
    }
    
    
    
    
}
