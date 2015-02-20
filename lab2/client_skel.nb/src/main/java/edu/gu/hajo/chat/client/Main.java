package edu.gu.hajo.chat.client;

import javax.swing.JOptionPane;

import edu.gu.hajo.chat.client.util.Connection;
import edu.gu.hajo.chat.client.util.ChatClientOptions;
import edu.gu.hajo.chat.client.view.MainFrame;
import java.lang.reflect.InvocationTargetException;
import javax.swing.SwingUtilities;

/**
 * Client application entry
 *
 * NOTE: Application needs a policy file in project directory, see
 * security.policy
 *
 * Use runclient.sh scripts to run in shell
 *
 * @author hajo
 *
 */
//------------ Nothing to do here -----------------------------------
public class Main {

    // Login, passwd, 127.0.0.1, 6666 7890
    private static final int N_ARGS = 5;

    public static void main(String[] args) {
        try {
            if (System.getSecurityManager() == null) {
                System.setSecurityManager(new SecurityManager());
            }
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null, "Security problems " + e);
            System.exit(1);
        }
        if (args.length == N_ARGS) {
            ChatClientOptions.setLogin(args[0]);
            ChatClientOptions.setPasswd(args[1]);

            int registryPort;
            int myPort;
            String registryIp;
            try {
                registryIp = args[2];
                registryPort = Integer.valueOf(args[3]);
                myPort = Integer.valueOf(args[4]);

                ChatClientOptions.setConnection(new Connection(registryIp, registryPort, myPort));
            } catch (NumberFormatException e) {
                JOptionPane.showMessageDialog(null, "Application couldn't start. Bad arguments! "
                        + e.getMessage());
                System.exit(1);
            }
        }
        System.out.println("User " + ChatClientOptions.getLogin()
                + " passwd " + ChatClientOptions.getPasswd());

        // Construct GUI
        try {
            SwingUtilities.invokeAndWait(new Runnable() {
                @Override
                public void run() {
                    new MainFrame().setVisible(true);
                }
            });

        } catch (InterruptedException | InvocationTargetException e) {
            JOptionPane.showMessageDialog(null, "Application couldn't start. Sorry! " + e.getMessage());
            System.exit(1);
        }
    }
}
