package edu.gu.hajo.chat.server.core;

import edu.gu.hajo.chat.server.spec.IMessage;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Main model class for a chat, no network code here
 *
 * @author hajo
 */
public class Chat {

    private final Map<String, User> registred;
    private final Set<User> loggedIn = new HashSet<>();

    public Chat(Map<String, User> registred) {
        if (registred == null) {
            this.registred = new HashMap<>();
        } else {
            this.registred = registred;
        }
    }

    // Possible we should not allow logged in to login again
    public User login(String login, String passwd) {

        User u = registred.get(login);
        if (isLoggedIn(u)) {
            return null;
        }
        boolean b = (u == null) ? false : u.getPasswd().equals(passwd);
        if (b) {
            loggedIn.add(u);
            return u;
        }
        return null;
    }

    public void logout(User user) {
        loggedIn.remove(user);
    }

    public void broadcast(IMessage msg) {
        msg.getSender().putOutbox(msg);
        for (User u : registred.values()) {
            u.putInbox(msg);
        }
    }

    public boolean isLoggedIn(User user) {
        return loggedIn.contains(user);
    }

    public Iterable<User> getLoggedIn() {
        return loggedIn;
    }

    public User getUser(String login) {
        return registred.get(login);
    }
}
