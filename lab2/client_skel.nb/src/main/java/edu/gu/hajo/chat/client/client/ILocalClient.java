package edu.gu.hajo.chat.client.client;

import java.util.List;


/**
 * The Client seen from the GUI.
 * Implemented by Client used by GUI 
 * @author hajo
 */
public interface ILocalClient {
    public void connect();
    public void disconnect();
    
    public void send(String message);
    
    public List<String> getFileListFromPeer(String peer);
    public void download(String filename, String username);
}
