package edu.gu.hajo.chat.server;

import edu.gu.hajo.chat.server.core.Chat;
import edu.gu.hajo.chat.server.core.Constants;
import edu.gu.hajo.chat.server.core.MessageBox;
import edu.gu.hajo.chat.server.core.User;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

import edu.gu.hajo.chat.server.util.ChatServerOptions;
import edu.gu.hajo.chat.server.spec.IChatServer;
import edu.gu.hajo.chat.server.service.Server;
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Server application Entry.
 *
 * @author hajo
 *
 * NOTE: Run this using the script runserver.sh, see proj directory
 *
 * Application needs a policy file in project directory, see security.policy
 * 
 * 
 *
 */
public class Main {

    private static final Logger LOG = Logger.getLogger(Main.class.getName());

    public static void main(String[] args) {
        // Must have security manager and policy file
        SecurityManager sm = System.getSecurityManager();
        if (sm == null) {
            System.setSecurityManager(new SecurityManager());
        }

        // Possible args to program
        if (args.length == 2) {
            try {
                int registryPort = Integer.parseInt(args[0]);
                int serverPort = Integer.parseInt(args[1]);
                ChatServerOptions.setRegistryPort(registryPort);
                ChatServerOptions.setServerPort(serverPort);
            } catch (NumberFormatException e) {
                LOG.log(Level.INFO, "Bad port for registry and(or server");
                System.exit(1);
            }
        }
        int registryPort = ChatServerOptions.getRegistryPort();
        int serverPort = ChatServerOptions.getServerPort();

        LOG.log(Level.INFO, "Trying to start server with registryport {0} and server port {1}",
                new Object[]{registryPort, serverPort});

        // Some default users
        Map<String, User> registred = new HashMap<>();
        registred.put("pelle", new User("pelle", "111",
                new MessageBox(), new MessageBox()));
        registred.put("fia", new User("fia", "222",
                new MessageBox(), new MessageBox()));
        /*
         * Create remote object and chat
         */
        Server server = new Server(new Chat(registred));

        try {
            /*
             * Export object and get at stub (i.e. a proxy for the server, 
             * see Proxy design pattern
             */
            IChatServer stub = (IChatServer) UnicastRemoteObject.exportObject(server,
                    serverPort);
            LOG.log(Level.INFO, "Got server stub{0}", stub);
            /*
             * Create a registry
             */
            LocateRegistry.createRegistry(registryPort);
            Registry registry = LocateRegistry.getRegistry(registryPort);
            LOG.log(Level.INFO, "Got server registry");
            /*
             * Bind the stub in the registry
             */
            registry.rebind(Constants.SERVER_NAME, stub);
            LOG.log(Level.INFO, "Server stub bound in registry");

            // Commandline info
            System.out.println("Server running at localhost " + serverPort);
            System.out.println("Ctrl c to quit ");

        } catch (RemoteException e) {
            System.err.println("Server exception: " + e.getMessage());

        }
    }
}
