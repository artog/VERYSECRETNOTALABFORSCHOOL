package edu.gu.hajo.chat.client.exception;

/**
 * Used to eliminate checked exceptions and introduce 
 * exceptions at a higher level
 */
public class ChatClientException extends RuntimeException {

    public ChatClientException(String msg) {
        super(msg);
    }
}
