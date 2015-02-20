package edu.gu.hajo.translator.event;

import java.util.ArrayList;
import java.util.List;

/**
 * Very simple event bus. All observers can register for events and an
 * observable can publish (send) events.
 *
 * @author hajo
 *
 */
public enum EventBus {

    INSTANCE;

    private final List<IEventHandler> handlers = new ArrayList<>();
    private final boolean trace = true;

    public void register(IEventHandler handler) {
        handlers.add(handler);
    }

    public void unRegister(IEventHandler handler) {
        handlers.remove(handler);
    }

    public void publish(Event evt) {
        // Tracking all events
        if (trace) {
            System.out.println(evt);
        }
        for (IEventHandler evh : handlers) {
            evh.onEvent(evt);
        }
    }
}
