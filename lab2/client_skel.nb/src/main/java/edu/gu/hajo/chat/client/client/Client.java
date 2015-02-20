package edu.gu.hajo.chat.client.client;

import edu.gu.hajo.chat.client.util.ChatClientOptions;

import edu.gu.hajo.chat.server.spec.IChatClient;
import edu.gu.hajo.chat.server.spec.IPeer;
import edu.gu.hajo.chat.server.core.User;
import java.io.Serializable;

/**
 * Implementation of many interfaces. Serializable important!
 *
 * @author hajo
 */
public class Client implements ILocalClient, IChatClient, IPeer,
        Serializable {

    private final transient StateContext context = null;

    // The logged in user
    private User me;
    private final String login = ChatClientOptions.getLogin();
    private final String passwd = ChatClientOptions.getPasswd();
    private final IObserver observer;

    public Client(IObserver observer) {
        this.observer = observer;
    }

    // ILocalClient ------------------------------------------
    // -------- IPeer --------------------
    // IClient -----------------------------------------------
    // -------------------------------------------------------------------
    // Handle over to Swing thread (from RMI thread)
    private void publishSwing(final IObserver.Event event, final Object data) {
       
    }

}
