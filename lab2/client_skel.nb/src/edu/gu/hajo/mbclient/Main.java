package edu.gu.hajo.mbclient;

import java.awt.EventQueue;

import javax.swing.JOptionPane;

import edu.gu.hajo.mbclient.core.Connection;
import edu.gu.hajo.mbclient.core.Options;
import edu.gu.hajo.mbclient.view.GUIFactory;
import edu.gu.hajo.mbcommon.User;

/**
 * Client application entry. 
 * 
 * NOTE: Application needs a policy file in
 * project directory, see security.policy
 *
 * @author hajo
 * 
 * 
 *            **** NOTHING TO DO HERE ****
 */
public class Main {

    private static final int N_ARGS = 4;

    public static void main(String[] args) {
        try {
            if (System.getSecurityManager() == null) {
                System.setSecurityManager(new SecurityManager());
            }
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null, "Security problems");
        }
        if (args.length == N_ARGS) {
            Options.INSTANCE.setUser(new User(args[0]));
            int registryPort = 0;
            int myPort = 0;
            try {
                registryPort = Integer.valueOf(args[2]);
                myPort = Integer.valueOf(args[3]);
            } catch (NumberFormatException e) {
                // Handled by finally
            } finally {
                Options.INSTANCE.setConnection(new Connection(args[1], registryPort, myPort));
            }
        }

        EventQueue.invokeLater(new Runnable() {

            public void run() {
                GUIFactory.getGUI().setVisible(true);
            }
        });
    }
}
