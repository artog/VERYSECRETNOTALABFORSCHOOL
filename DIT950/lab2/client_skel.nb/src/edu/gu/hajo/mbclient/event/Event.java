package edu.gu.hajo.mbclient.event;

/**
 * A single event
 * @author hajo
 * 
 *                **** NOTHING TO DO HERE ****
 *
 */
public class Event {
    // All possible events listed
    public enum Tag {
        NEW_MESSAGE, 
        CONNECTED, 
        DISCONNECTED, 
        UPDATE_USERS, 
        EXCEPTION
    }
    private final Tag tag;
    // The new value 
    private final Object value;
    public Event(Tag tag, Object value){
        this.tag = tag;
        this.value = value;
    }
    public Tag getTag() {
        return tag;
    }
    public Object getValue() {
        return value;
    }
    @Override
    public String toString() {
        return "Event [tag=" + tag + ", value=" + value + "]";
    } 
    
    
}
