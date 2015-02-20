package edu.gu.hajo.mbclient.core;

import edu.gu.hajo.mbclient.core.state.IState;
import edu.gu.hajo.mbclient.core.state.StateFactory;
import edu.gu.hajo.mbclient.event.EventBus;
import edu.gu.hajo.mbcommon.IClient;
import edu.gu.hajo.mbcommon.IPeer;

/**
 * 
 * Handling the client as a singleton (in different shapes)
 * @author hajo
 * 
 *                   **** NOTHING TO DO HERE ****
 */
public class ClientFactory {

    private static final Client INSTANCE;

    static {
        EventBus eb = EventBus.INSTANCE;
        IState ctx = StateFactory.getStateContext();
        INSTANCE = new Client(eb, ctx);
    }
    
    private ClientFactory(){
    }

    public static IClient getClient() {
        return INSTANCE;
    }

    public static ILocalClient getLocalClient() {
        return INSTANCE;
    }

    public static IPeer getPeer() {
        return INSTANCE;
    }

}
