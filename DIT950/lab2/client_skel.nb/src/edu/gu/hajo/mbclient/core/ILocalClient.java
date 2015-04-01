package edu.gu.hajo.mbclient.core;

import java.io.File;

import edu.gu.hajo.mbcommon.IPeer;
import edu.gu.hajo.mbcommon.MBFile;

/**
 * The Client seen from the GUI.
 * Implemented by Client used by GUI 
 * @author hajo
 */
public interface ILocalClient {
    public void connect();
    public void disconnect();
    public void broadcast(String string);
    public IPeer getPeer(String name); 
    public void store(MBFile file, File downloadDir);
}
