package edu.gu.hajo.chat.server.core;

import edu.gu.hajo.chat.server.spec.IMessage;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Where to store messages
 * @author hajo
 */
public class MessageBox implements Serializable {

    private final List<IMessage> msgs;

    public MessageBox() {
        this.msgs = new ArrayList<>();
    }

    void add(IMessage msg) {
        msgs.add(msg);
    }
    
    public List<IMessage> getMessages() {
        List<IMessage> copy = new ArrayList();
        msgs.forEach((IMessage msg) -> {
            copy.add(
                    new ChatMessage(
                            null,
                            msg.getSender(),
                            msg.getContent()
                    )
            );
        });
        return copy; 
    }
    
}
