package edu.gu.hajo.mbclient.core;

/**
 * Simple data, immutable 
 *
 * @author hajo
 * 
 *                **** NOTHING TO DO HERE ****
 */
public final class Connection {

    private final String registryIp;
    private final int registryPort;
    private final int myPort;
   
    public Connection(String registryIp, int registryPort, int myPort) {
        this.registryIp = registryIp;
        this.registryPort = registryPort;
        this.myPort = myPort;
    }

    public int getMyPort() {
        return myPort;
    }

    public String getRegistryIp() {
        return registryIp;
    }

    public int getRegistryPort() {
        return registryPort;
    }     
}
