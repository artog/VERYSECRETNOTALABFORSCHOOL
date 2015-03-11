package edu.gu.hajo.chat.client.client;

import edu.gu.hajo.chat.client.exception.ChatClientException;
import edu.gu.hajo.chat.server.core.User;
import java.util.List;



/**
 * The contract for the different states using
 * the State pattern
 * @author hajo
 *
 */
public interface IState {
    public User connect(Client client, String login, String password) throws ChatClientException;
    public void disconnect(User user);
    
    public void send(User sender, String message);
    
    public List<String> getFileListFromPeer(String peer);
    
    public void download(String filename, String username);
}
