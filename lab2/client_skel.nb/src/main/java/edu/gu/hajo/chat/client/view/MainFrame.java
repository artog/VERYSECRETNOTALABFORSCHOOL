package edu.gu.hajo.chat.client.view;

import edu.gu.hajo.chat.client.client.Client;
import edu.gu.hajo.chat.client.client.ILocalClient;
import edu.gu.hajo.chat.client.client.IObserver;
import edu.gu.hajo.chat.server.core.User;
import edu.gu.hajo.chat.server.spec.IMessage;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

/**
 * Chat client main window
 *
 * @author hajo
 */
public class MainFrame extends JFrame implements ActionListener, IObserver {

    public static final String SEND_CMD = "send";
    public static final String CONNECT_CMD = "connect";
    public static final String DISCONNECT_CMD = "disconnect";

    private ILocalClient client = new Client(this);
    private final JButton connect = new JButton("Connect");
    private final JButton disconnect = new JButton("Disconnect");
    private final JTextArea messages = new JTextArea();
    private final JTextField send = new JTextField();
    private final DefaultListModel listModel = new DefaultListModel();
    private final JList connected = new JList(listModel);
    private final JLabel status = new JLabel();

    public MainFrame() {
        init();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        switch (e.getActionCommand()) {
            case SEND_CMD:
                String msg = send.getText();
               
                send.setText("");
                break;
            case CONNECT_CMD:
                client.connect();
                
                break;
            case DISCONNECT_CMD:
                client.disconnect();
                break;
            default:
        }
    }

    @Override
    public void onEvent(IObserver.Event event, Object data) {
        switch (event) {
            case USER_JOINED:
                String login = (String) data;
                listModel.addElement(login);
                connected.setModel(listModel);
                break;
            case CONNECTED:
                User u = (User) data;
                status.setText("Logged in as: " + u.getLogin());
                setEnabled();
                break;
            case DISCONNECTED:
                status.setText("Not connected");
                setDisabled();
                break;
            case MESSAGE:
                IMessage msg = (IMessage) data;
                messages.append(msg.getSender().getLogin() + ":"
                        + msg.getContent() + "\n");
                break;
            case USER_LEFT:
                login = (String) data;
                listModel.removeElement(login);
                connected.setModel(listModel);
                break;
            case EXCEPTION:
                String s = (String) data;
                JOptionPane.showMessageDialog(this, s);
                break;
            default:
                throw new AssertionError(event.name());  // TODO 
        }
    }

    private final WindowListener windowListener = new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
            if( client != null){
               
            }
        }
    };

    private final MouseListener mouseListener = new MouseAdapter() {
        @Override
        public void mouseClicked(MouseEvent e) {
            if (e.getClickCount() == 2) {
                JList l = (JList) e.getSource();
                String login = (String) l.getSelectedValue();
                if (client != null) {
                    new PeerDialog(MainFrame.this, login, client).setVisible(true);
                } else {
                    JOptionPane.showMessageDialog(MainFrame.this, "Not connected");
                }
            }
        }
    };

    // ---------- Privates (boring GUI code )------------------------
    private void init() {
        this.setTitle("MAY BAY");
        this.addWindowListener(windowListener);
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        this.setLayout(new BorderLayout());
        this.setLocationRelativeTo(null);
        this.add(creatButtonPanel(), BorderLayout.NORTH);
        this.add(createSendMessagesPanel(), BorderLayout.CENTER);
        this.add(createConnectedPanel(), BorderLayout.EAST);
        this.add(createStatusPanel(), BorderLayout.SOUTH);
        this.setSize(400, 400);
    }

    private void setEnabled() {
        disconnect.setEnabled(true);
        connect.setEnabled(false);
        send.setEnabled(true);
    }

    private void setDisabled() {
        connect.setEnabled(true);
        disconnect.setEnabled(false);
        messages.replaceRange("", 0, messages.getText().length());
        send.setText("");
        send.setEnabled(false);
        listModel.clear();
        connected.setModel(listModel);
    }

    private JPanel creatButtonPanel() {
        JPanel p = new JPanel();
        p.setPreferredSize(new Dimension(400, 40));
        connect.setActionCommand(CONNECT_CMD);
        connect.addActionListener(this);
        disconnect.setActionCommand(DISCONNECT_CMD);
        disconnect.addActionListener(this);
        disconnect.setEnabled(false);
        p.add(connect);
        p.add(disconnect);
        return p;
    }

    private JPanel createSendMessagesPanel() {
        JPanel p = new JPanel();
        p.setLayout(new BorderLayout());
        p.setPreferredSize(new Dimension(300, 360));

        JPanel sendPanel = new JPanel();
        sendPanel.setBorder(BorderFactory.createTitledBorder("Send"));
        send.setPreferredSize(new Dimension(260, 30));
        send.setActionCommand(SEND_CMD);
        send.addActionListener(this);
        send.setEnabled(false);
        sendPanel.add(send);

        JPanel msgPanel = new JPanel();
        msgPanel.setBorder(BorderFactory.createTitledBorder("Messages"));
        messages.setBorder(BorderFactory.createLineBorder(Color.GRAY));
        messages.setPreferredSize(new Dimension(260, 200));
        messages.setBackground(Color.WHITE);
        messages.setEditable(false);
        msgPanel.add(messages);

        p.add(sendPanel, BorderLayout.NORTH);
        p.add(msgPanel, BorderLayout.CENTER);

        return p;
    }

    private JPanel createConnectedPanel() {
        JPanel p = new JPanel();
        p.setBorder(BorderFactory.createTitledBorder("Connected"));
        p.setPreferredSize(new Dimension(100, 360));
        connected.setBorder(BorderFactory.createLineBorder(Color.GRAY));
        connected.setPreferredSize(new Dimension(80, 260));

        connected.setBackground(Color.WHITE);
        connected.setListData(new Object[]{"None"});
        connected.addMouseListener(mouseListener);
        p.add(connected);
        return p;
    }

    private JPanel createStatusPanel() {
        JPanel p = new JPanel();
        p.setBorder(BorderFactory.createTitledBorder("Status"));
        p.setLayout(new GridLayout(1, 1));
        status.setText("Disconnected");
        p.add(status);
        return p;
    }

}
