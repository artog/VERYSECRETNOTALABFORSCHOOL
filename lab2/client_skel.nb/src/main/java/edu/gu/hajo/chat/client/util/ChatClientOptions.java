package edu.gu.hajo.chat.client.util;

import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * User options and defaults
 *
 * @author hajo
 *
 */
public class ChatClientOptions {

    private ChatClientOptions() {
    }

    // Default values 
    private static Connection connection = new Connection("127.0.0.1", 6666, 7890);
    private static Path uploadDir = Paths.get("upload");
    private static Path downloadDir = Paths.get("download");
   
    private static String login = "qqq";
    private static String passwd = "111";

    public static void setConnection(Connection connection) {
        ChatClientOptions.connection = connection;
    }

    public static Connection getConnection() {
        return connection;
    }

    public static void setPasswd(String passwd) {
        ChatClientOptions.passwd = passwd;
    }

    public static void setLogin(String login) {
        ChatClientOptions.login = login;
    }

    public static String getLogin() {
        return login;
    }

    public static String getPasswd() {
        return passwd;
    }

    public static Path getUploadPath() {
        return uploadDir;
    }

    public static Path getDownloadPath() {
        return downloadDir;
    }

    public static void setUploadPath(Path uploadDir) {
        ChatClientOptions.uploadDir = uploadDir;
    }

    public static void setDownloadPath(Path downloadDir) {
        ChatClientOptions.downloadDir = downloadDir;
    }
}
