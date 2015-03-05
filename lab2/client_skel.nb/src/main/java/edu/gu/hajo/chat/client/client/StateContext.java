package edu.gu.hajo.chat.client.client;

/**
 * Handles the states and transitions between them
 *
 * @author hajo
 *
 */
final class StateContext {
    private IState state = new Disconnected();
    
    public void set(IState state){
        this.state = state;
    }
}
