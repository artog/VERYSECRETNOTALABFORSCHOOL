package edu.gu.hajo.translator.gui;

import edu.gu.hajo.translator.core.ITranslator;
import edu.gu.hajo.translator.event.Event;
import edu.gu.hajo.translator.event.IEventHandler;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

/**
 * Application main window
 *
 * @author hajo
 *
 */
@SuppressWarnings("serial")
public class MainFrame extends JFrame implements IEventHandler {

    private ITranslator translator;

    public MainFrame(JPanel controlPanel, JPanel ioPanel,
            JPanel languageSelectionPanel) {
        
        
        this.setLayout(new BorderLayout());
        this.addWindowListener(listener);
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        this.getContentPane().add(controlPanel, BorderLayout.SOUTH);
        this.getContentPane().add(ioPanel, BorderLayout.CENTER);
        this.getContentPane().add(languageSelectionPanel, BorderLayout.NORTH);
        initMenu();
        pack();
    }

    @Override
    public void onEvent(Event evt) {
        if (evt.getTag() == Event.Tag.ERROR) {
            JOptionPane.showMessageDialog(this, evt.getValue(), "Fel", JOptionPane.ERROR_MESSAGE);
        }
    }

    // Handling shutdown -----------------
    private void exit() {
     
        System.exit(0);
    }

    private final WindowListener listener = new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
            exit();
        }
    };

    // ----- The menu ---------
    // Common listener for menu
    private final ActionListener menuListener = new ActionListener() {

        @Override
        public void actionPerformed(ActionEvent e) {
            Object s = e.getSource();
            if (s == fileExit) {
                MainFrame.this.exit();
            } else if (s == aboutAbout) {
                JOptionPane.showMessageDialog(MainFrame.this, "Translator by Adam and Mikael!");
            }
        }
    };

    private final JMenuBar menuBar = new JMenuBar();
    private final JMenu file = new JMenu("File");
    private final JMenu about = new JMenu("About");
    private final JMenuItem fileExit = new JMenuItem("Exit");
    private final JMenuItem aboutAbout = new JMenuItem("About");

    private void initMenu() {
        this.setJMenuBar(menuBar);
        menuBar.add(file);
        menuBar.add(about);
        file.add(fileExit);
        about.add(aboutAbout);
        fileExit.addActionListener(menuListener);
        aboutAbout.addActionListener(menuListener);
    }
  
}
