package edu.gu.hajo.mbclient.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.IOException;
import java.rmi.RemoteException;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingWorker;
import javax.swing.WindowConstants;

import edu.gu.hajo.mbclient.core.ILocalClient;
import edu.gu.hajo.mbclient.core.Options;
import edu.gu.hajo.mbcommon.IPeer;
import edu.gu.hajo.mbcommon.MBFile;

/**
 * Window for direct connections to other client (peer 2 peer)
 * 
 * @author hajo
 */
@SuppressWarnings("serial")
public class PeerDialog extends JDialog implements ActionListener {

    private ILocalClient client;
    private String otherName;

    public PeerDialog(JFrame parent, String otherName, ILocalClient client) {
        super(parent, "Peer", true);
        this.client = client;
        this.otherName = otherName;
        init();
        this.setLocationRelativeTo(parent);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == cancel) {
            this.dispose();
        } else if (e.getSource() == download) {
            final String fileName = files.getSelectedValue();
            if (fileName == null) {
                return;
            }
            // Possible time consuming use a SwingWorker
            SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
                public Void doInBackground() {
                    try {
                        MBFile file = client.getPeer(otherName).getFile(fileName,
                                Options.INSTANCE.getUploadDir());
                        System.out.println( file);
                        client.store(file,
                                Options.INSTANCE.getDownloadDir());
                    } catch (RemoteException e) {
                        JOptionPane.showMessageDialog(PeerDialog.this, e.getMessage());
                    }
                    return null;
                }

                public void done() {
                    dispose();
                }
            };
            worker.execute();
        }
    }

    WindowListener listener = new WindowAdapter() {
        @Override
        public void windowOpened(WindowEvent evt) {
            try {
                IPeer peer = client.getPeer(otherName);
                String[] files = peer.getFileList(Options.INSTANCE
                        .getUploadDir());
                DefaultListModel<String> model = new DefaultListModel<>();
                for (String f : files) {
                    model.addElement(f);
                }
                PeerDialog.this.files.setModel(model);
            } catch (IOException e) {
                JOptionPane.showMessageDialog(PeerDialog.this, e.getMessage());
            }
        }
    };

    private JButton download = new JButton("Download");
    private JButton cancel = new JButton("Cancel");
    private JList<String> files = new JList<String>();

    private void init() {
        this.setLayout(new BorderLayout());
        this.setSize(new Dimension(200, 300));
        this.addWindowListener(listener);
        this.getContentPane().add(files, BorderLayout.CENTER);
        JPanel buttons = new JPanel();
        buttons.add(download);
        buttons.add(cancel);
        this.getContentPane().add(buttons, BorderLayout.SOUTH);
        this.setTitle("Files : " + otherName);
        this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        download.addActionListener(this);
        cancel.addActionListener(this);
    }
}
