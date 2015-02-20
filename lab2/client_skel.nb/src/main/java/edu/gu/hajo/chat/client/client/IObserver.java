package edu.gu.hajo.chat.client.client;

/**
 * Simple observer contract
 * @author hajo
 */
public interface IObserver {

    public enum Event {

        CONNECTED,
        DISCONNECTED,
        MESSAGE,
        USER_LEFT,
        USER_JOINED,
        EXCEPTION,
    }

    public void onEvent(Event event, Object data);
}
