package edu.gu.hajo.chat.server.spec;

import edu.gu.hajo.chat.server.core.User;
import java.io.Serializable;

/**
 * Minimal contract for messages
 *
 * @author hajo
 */
public interface IMessage extends Serializable {

    public User getSender();

    public User getReceiver();

    public String getContent();

}
