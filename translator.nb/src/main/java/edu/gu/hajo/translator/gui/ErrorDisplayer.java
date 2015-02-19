/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.translator.gui;

import edu.gu.hajo.translator.event.Event;
import edu.gu.hajo.translator.event.EventBus;
import edu.gu.hajo.translator.event.IEventHandler;
import javax.swing.JOptionPane;

/**
 *
 * @author adam
 */
public class ErrorDisplayer implements IEventHandler {

    
    public static final ErrorDisplayer instance = new ErrorDisplayer();
    
    
    public ErrorDisplayer() {
        EventBus.INSTANCE.register(this);
    }
    
    @Override
    public void onEvent(Event evt) {
        if (evt.getTag() == Event.Tag.ERROR) {
            JOptionPane.showMessageDialog(null, evt.getValue(), "Fel", JOptionPane.ERROR_MESSAGE);
        }
    }
    
    
    
}
