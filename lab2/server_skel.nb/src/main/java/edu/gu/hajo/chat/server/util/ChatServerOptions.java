package edu.gu.hajo.chat.server.util;

/**
 * ChatServer options and defaults
 *
 * @author hajo
 *
 */
public class ChatServerOptions {

    private ChatServerOptions() {
    }

    private static int serverPort = 6666;
    private static int registryPort = 7777;
 

    public static int getServerPort() {
        return serverPort;
    }

    public static int getRegistryPort() {
        return registryPort;
    }

    public static void setServerPort(int serverPort) {
        ChatServerOptions.serverPort = serverPort;
    }

    public static void setRegistryPort(int registryPort) {
        ChatServerOptions.registryPort = registryPort;
    }
}
