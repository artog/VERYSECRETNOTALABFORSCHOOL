/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.translator.gui;

import edu.gu.hajo.translator.event.Event;
import edu.gu.hajo.translator.event.EventBus;
import java.awt.Component;
import java.util.List;
import java.util.ArrayList;
import javax.swing.JList;
import javax.swing.JTextField;
import javax.swing.ListModel;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Adam
 */
public class DisplayPanelTest {
    
    public DisplayPanelTest() {
    }

    /**
     * Test of onEvent method, of class DisplayPanel.
     */
    @Test
    public void testPrefixChangedEvent() {
        System.out.println("Event");
        DisplayPanel instance = new DisplayPanel();
        
        Event evt = new Event(Event.Tag.PREFIX_CHANGED,"test");
        EventBus.INSTANCE.publish(evt);
        
        Component a = instance.getComponent(0);
        if (a instanceof JTextField) {
            assertEquals(((JTextField)a).getText(),"test");
        } else {
            fail("Component not JTextField");
        }
    }
    
    
    @Test
    public void testTranslationsChangedEvent() {
        System.out.println("Event");
        DisplayPanel instance = new DisplayPanel();
        List<String> strings = new ArrayList();
        strings.add("string 1");
        strings.add("string 2");
        strings.add("string 3");
        strings.add("string 4");
        List<String> expResult = new ArrayList();
        expResult.addAll(strings);
        
        Event evt = new Event(Event.Tag.TRANSLATIONS_CHANGED,strings);
        EventBus.INSTANCE.publish(evt);
        
        Component a = instance.getComponent(1);
        if (a instanceof JList) {
            JList jl = (JList)a;
            ListModel<Class<?>> dlm = jl.getModel();
            
            assertTrue(expResult.size() == dlm.getSize());
            for (int i = 0;i<expResult.size();i++) {
                assertEquals(expResult.get(i),dlm.getElementAt(i));
            }
        } else {
            fail("Component not JList");
        }
    }
    
}
