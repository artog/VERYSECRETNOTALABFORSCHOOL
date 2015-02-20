package edu.gu.hajo.chat.server.core;

import edu.gu.hajo.chat.server.spec.IMessage;
import java.io.Serializable;

/**
 * User for chat ( a bit strange to send this over the network but example of a
 * local object
 *
 * @author hajo
 *
 */
public class User implements Serializable {

    // A dummy user
    public static transient final User NONE;

    static {
        MessageBox m = new MessageBox();
        NONE = new User(Constants.NONE, Constants.NONE, m, m);
    }
    private final String login;
    private final String passwd;
    private final MessageBox inbox;
    private final MessageBox outbox;

    public User(String login, String passwd, MessageBox inbox, MessageBox outbox) {
        this.login = login;
        this.passwd = passwd;
        this.inbox = inbox;
        this.outbox = outbox;
    }

    void putInbox(IMessage msg) {
        inbox.add(msg);
    }

    void putOutbox(IMessage msg) {
        outbox.add(msg);
    }

    public String getLogin() {
        return login;
    }

    public String getPasswd() {
        return passwd;
    }

    @Override
    public String toString() {
        return "User[" + login + "]";
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((login == null) ? 0 : login.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        User other = (User) obj;
        if (login == null) {
            if (other.login != null) {
                return false;
            }
        } else if (!login.equals(other.login)) {
            return false;
        }
        return true;
    }

}
