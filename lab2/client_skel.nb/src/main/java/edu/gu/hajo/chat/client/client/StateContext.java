package edu.gu.hajo.chat.client.client;

import edu.gu.hajo.chat.client.exception.ChatClientException;
import edu.gu.hajo.chat.server.core.User;
import java.util.List;

/**
 * Handles the states and transitions between them
 *
 * @author hajo
 *
 */
final class StateContext {
    private IState state = new Disconnected(this);
    
    public void set(IState state){
        this.state = state;
    }
    
    public User connect(Client client){
        return state.connect(client);
    }
    
    public void disconnect(){
        state.disconnect();
    }
    
    public List<String> getFileListFromPeer(String peer) {
        return state.getFileListFromPeer(peer);
    }
    
    public void download(String filename, String username) 
            throws ChatClientException
    {
        state.download(filename,username);
    }
}
