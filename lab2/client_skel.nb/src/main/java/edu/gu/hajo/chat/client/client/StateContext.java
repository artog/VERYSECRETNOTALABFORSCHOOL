package edu.gu.hajo.chat.client.client;

import edu.gu.hajo.chat.server.core.User;

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
}
