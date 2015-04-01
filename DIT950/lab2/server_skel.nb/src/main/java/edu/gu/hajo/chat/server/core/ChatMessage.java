package edu.gu.hajo.chat.server.core;

import edu.gu.hajo.chat.server.spec.IMessage;

/**
 * Messages to send
 * @author hajo
 *
 */
public class ChatMessage implements IMessage {

    private final String text;
    private final User sender;
    private final User receiver;
   

    public ChatMessage(User receiver, User sender, String text) {
        this.receiver = receiver;
        this.sender = sender;
        this.text = text;
    }

    @Override
    public String getContent() {
        return text;
    }

    @Override
    public User getSender() {
        return sender;
    }

    @Override
    public User getReceiver() {
        return receiver;
    }
    
    
}
