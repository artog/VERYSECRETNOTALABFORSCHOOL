package edu.gu.hajo.translator.event;

/**
 * A single event
 *
 * @author hajo
 *
 */
public class Event {

    // All possible events listed
    public enum Tag {

        TODO         // Add your own events here
    }

    private final Tag tag;
    // Data to send
    private final Object value;

    public Event(Tag tag, Object value) {
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