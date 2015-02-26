package edu.gu.hajo.chat.server.spec;

import java.rmi.Remote;

/**
 * Contract for client (from Server view). Call backs from server
 *
 * @author hajo
 *
 */
public interface IChatClient extends Remote {
    
    public void ping();
    
    public void userLeft(String user);
    public void userJoined(String user);

    public void message();
}
