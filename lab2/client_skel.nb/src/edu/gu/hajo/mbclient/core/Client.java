package edu.gu.hajo.mbclient.core;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.rmi.NoSuchObjectException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.SwingUtilities;

import edu.gu.hajo.mbclient.core.state.IState;
import edu.gu.hajo.mbclient.event.Event;
import edu.gu.hajo.mbclient.event.EventBus;
import edu.gu.hajo.mbclient.io.FileHandler;
import edu.gu.hajo.mbcommon.Constants;
import edu.gu.hajo.mbcommon.IClient;
import edu.gu.hajo.mbcommon.IMessage;
import edu.gu.hajo.mbcommon.IPeer;
import edu.gu.hajo.mbcommon.MBFile;
import edu.gu.hajo.mbcommon.Message;
import edu.gu.hajo.mbcommon.User;

/**
 * Implementation of the many interfaces. The "real" client Serializable
 * important! 
 * 
 * @author hajo
 */
class Client implements ILocalClient, IClient, IPeer, Serializable {

    private static final long serialVersionUID = Constants.SERIAL_V_ID;

    // Here EventBus is used for incoming from server (to GUI)
    private transient final EventBus bus;

    // Server doesn't use these so make'em transient
    private transient List<String> connectedUsers = new ArrayList<String>();
    // Need observers to signal connection state
    private transient final IState context;

    // Used on server side
    private User user;

    public Client(EventBus bus, IState context) {
        this.bus = bus;
        this.context = context;
        System.out.println(context);
    }

    // ILocalClient ------------------------------------------

    // Connection changes handled by context
    public void connect() {
        try {
            user = Options.INSTANCE.getUser();
            context.connect(this);
            publishSwing(new Event(Event.Tag.CONNECTED, null));
        } catch (RemoteException | NotBoundException e) {
            publishSwing(new Event(Event.Tag.EXCEPTION,
                    "Can't conntect to server"));
        }

    }

    // Connection changes handled by context
    public void disconnect() {
        try {
            context.disconnect(this);
        } catch (RemoteException e) {
            publishSwing(new Event(Event.Tag.EXCEPTION,
                    "No clean disconnection"));
        }
        connectedUsers.clear();
    }

    public void broadcast(String msg) {
        try {
            context.broadcast(new Message(user, msg));
        } catch (RemoteException e) {
            publishSwing(new Event(Event.Tag.EXCEPTION, "Couldn't send message"));
            try {
                UnicastRemoteObject.unexportObject(this, true);
            } catch (NoSuchObjectException e1) {
                Logger.getAnonymousLogger().log(Level.INFO, "Couldn't unexport object " + this);
            }
            // If exception we are disconnected
            connectedUsers.clear();
            publishSwing(new Event(Event.Tag.UPDATE_USERS, connectedUsers));
        }
    }

    public IPeer getPeer(String name) {
        System.out.println("Client getPeer " + name);
        String other = null;
        IPeer peer = null;
        for (String un : connectedUsers) {
            System.out.println("User" + un);
            if (un.equals(name)) {
                other = un;
            }
        }
        try {
            peer = context.getPeer(other);
        } catch (RemoteException e) {
            publishSwing(new Event(Event.Tag.EXCEPTION,
                    "Couldn't get remote user"));
        }
        // Possible null
        return peer;
    }

    public void store(MBFile file, File downloadDir) {
        try {
            FileHandler.store(file, downloadDir);
        } catch (IOException e) {
            bus.publish(new Event(Event.Tag.EXCEPTION, "Couln't store file"));
        }
    }

    // IClient -----------------------------------------------

    public void updateUsers(List<String> usersNames) throws RemoteException {
        connectedUsers.clear();
        connectedUsers.addAll(usersNames);
        publishSwing(new Event(Event.Tag.UPDATE_USERS, connectedUsers));
    }

    public void newMessage(IMessage msg) throws RemoteException {
        // Just for demo use
        System.out.println("Client incomming msg " + msg.getText() + ":"
                + Thread.currentThread());
        publishSwing(new Event(Event.Tag.NEW_MESSAGE, msg));
    }

    public User getUser() {
        return user;
    }

    // Handle over to Swing thread (from RMI thread)
    private void publishSwing(final Event evt) {
        SwingUtilities.invokeLater(new Runnable() {

            @Override
            public void run() {
                bus.publish(evt);
            }
        });
    }

    // IPeer ------------------------------------------------------

    // Interface throws RemoteException
    public MBFile getFile(String fileName, File uploadDir)
            throws RemoteException {
        MBFile file = null;
        try {
            file = FileHandler.getFile(fileName, uploadDir);
        } catch (IOException e) {
            bus.publish(new Event(Event.Tag.EXCEPTION, "Couldn't get file"));
        }
        return file;
    }

    public String[] getFileList(File directory) throws RemoteException {
        return directory.list();
    }
}
