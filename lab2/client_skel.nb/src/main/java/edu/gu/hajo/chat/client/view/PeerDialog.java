package edu.gu.hajo.chat.client.view;

import edu.gu.hajo.chat.client.client.ILocalClient;
import edu.gu.hajo.chat.server.io.ChatFile;
import edu.gu.hajo.chat.server.spec.IPeer;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.rmi.RemoteException;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.SwingWorker;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowListener;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

/**
 * Window for direct file transfer between clients (peer 2 peer)
 *
 * @author hajo
 */
@SuppressWarnings("serial")
public class PeerDialog extends JDialog implements ActionListener {

    public static final String CANCEL_CMD = "cancel";
    public static final String DOWNLOAD_CMD = "download";
    private final JButton cancel = new JButton("Cancel");
    private final JButton download = new JButton("Download");
    private final JList files = new JList(); // File select for download
    private final ILocalClient client;
    private final String peerLogin;

    public PeerDialog(JFrame parent, String peerLogin, ILocalClient client) {
        super(parent, "Download from " + peerLogin, true);
        this.client = client;
        this.peerLogin = peerLogin;
        init();
        this.setLocationRelativeTo(parent);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        switch (e.getActionCommand()) {
            case CANCEL_CMD:
                dispose();
                break;
            case DOWNLOAD_CMD:
                download();
                break;
            default: ;
        }

    }

    // ------- Privates --------------------------------------------
    private void download() {
        final String fileName = (String) files.getSelectedValue();
        if (fileName == null) {
            return;
        }
       
    }

    private final WindowListener windowListener = new WindowAdapter() {
        @Override
        public void windowOpened(WindowEvent e) {
           
        }
    };

    private JPanel createButtonPanel() {
        JPanel p = new JPanel();
        cancel.setActionCommand(CANCEL_CMD);
        cancel.addActionListener(this);
        download.setActionCommand(DOWNLOAD_CMD);
        download.addActionListener(this);
        p.add(download);
        p.add(cancel);
        return p;
    }

    private JPanel createFilesPanel() {
        JPanel p = new JPanel();
        p.setBorder(BorderFactory.createTitledBorder("Files"));
        files.setBorder(BorderFactory.createLineBorder(Color.GRAY));
        files.setPreferredSize(new Dimension(200, 280));
        files.setBackground(Color.WHITE);
        files.setListData(new Object[]{"No files"});
        p.add(files);
        return p;
    }

    private void init() {
        this.setLayout(new BorderLayout());
        this.addWindowListener(windowListener);
        this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        this.add(createButtonPanel(), BorderLayout.SOUTH);
        this.add(createFilesPanel(), BorderLayout.CENTER);
        this.setSize(240, 380);
    }

}
