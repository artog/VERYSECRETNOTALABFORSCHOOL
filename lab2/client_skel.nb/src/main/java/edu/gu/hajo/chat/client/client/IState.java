package edu.gu.hajo.chat.client.client;

import edu.gu.hajo.chat.server.core.User;



/**
 * The contract for the different states using
 * the State pattern
 * @author hajo
 *
 */
public interface IState {
    public User connect(Client client);
    public void disconnect();
}
