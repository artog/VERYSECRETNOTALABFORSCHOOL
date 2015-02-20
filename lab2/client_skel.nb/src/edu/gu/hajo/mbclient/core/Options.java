   package edu.gu.hajo.mbclient.core;

import java.io.File;

import edu.gu.hajo.mbcommon.User;

/**
 * To hold user option 
 *
 * @author hajo
 * 
 *         **** NOTHING TO DO HERE ****
 *
 */

public enum Options {
    INSTANCE;
  
    // Default values 
    private Connection connection = new Connection("127.0.0.1", 6666, 7890);
    private User user = new User("Default");
    private File uploadDir =  new File("upload");
    private File downloadDir =  new File("download");
    
    public void setConnection(Connection connection) {
       this.connection = connection; 
    }  
    
    public Connection getConnection(){
        return connection;
    }
    
    public void setUser(User user) {
        this.user = user;
    }
    
    public User getUser(){
        return user;
    }
        
    public File getUploadDir() {
        return uploadDir;
    }

    public void setUploadDir(File uploadDir) {
        this.uploadDir = uploadDir;
    }

    public File getDownloadDir() {
        return downloadDir;
    }

    public void setDownloadDir(File downloadDir) {
        this.downloadDir = downloadDir;
    }
}
