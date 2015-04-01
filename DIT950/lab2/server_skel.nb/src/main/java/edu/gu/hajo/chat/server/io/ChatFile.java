package edu.gu.hajo.chat.server.io;

import java.io.Serializable;
import java.util.Arrays;

/**
 * Utility, wrapping bytes from a file.
 *
 * @author hajo
 *
 */
public class ChatFile implements Serializable { 
  
    private final byte[] bytes;
    private final String fileName;
    
    public ChatFile( String fileName, byte[] bytes ){
        this.bytes = Arrays.copyOf(bytes, bytes.length);
        this.fileName = fileName;
    }
   
    public byte[] getBytes(){
        return Arrays.copyOf(bytes, bytes.length);
    }
    
    public String getFileName(){
        return fileName;
    }
    
    @Override
    public String toString(){
      return String.valueOf(bytes);  
    }
}
