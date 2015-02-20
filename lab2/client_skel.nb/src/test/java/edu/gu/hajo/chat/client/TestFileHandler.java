/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.chat.client;

import edu.gu.hajo.chat.client.io.FileHandler;
import edu.gu.hajo.chat.client.util.ChatClientOptions;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

/**
 * File handling using new NIO (Java new IO)
 * @author hajo
 */
public class TestFileHandler {

    byte[] content;

    //@Before  // Order matters
    public void testRead() throws IOException {
        Path path = ChatClientOptions.getUploadPath();
        content = FileHandler.readFile(path, "pig.jpeg");
        assertTrue(content.length > 0);
    }

    //@Test
    public void testWrite() throws IOException {
        Path path = ChatClientOptions.getDownloadPath();
        FileHandler.saveFile(path, "pig.jpeg", content);
    }
    
    @Test
    public void testDirListing() throws IOException  {
        Path path = ChatClientOptions.getUploadPath();
        List<String> fileNames = FileHandler.listDirectoryContent(path);
        assertTrue( fileNames.size() > 0);
        for(String s : fileNames){
            System.out.println(s);
        }
    }
    

}
