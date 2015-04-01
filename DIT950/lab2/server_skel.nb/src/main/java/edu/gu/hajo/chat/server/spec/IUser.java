/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.chat.server.spec;

import java.util.List;

/**
 *
 * @author adam
 */
public interface IUser {
    
    public void putInbox(IMessage msg);
    public void putOutbox(IMessage msg);
    public List<IMessage> getMessages();
    public String getLogin();
    public String getPasswd();

}
