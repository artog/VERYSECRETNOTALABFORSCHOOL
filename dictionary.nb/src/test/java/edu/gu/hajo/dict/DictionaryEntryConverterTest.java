/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.dict;

import edu.gu.hajo.dict.core.DictionaryEntry;
import java.util.ArrayList;
import java.util.List;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Adam
 */
public class DictionaryEntryConverterTest {
    
    /**
     * Test of toObject method, of class DictionaryEntryConverter.
     */
    @Test
    public void testToObject() {
        
        String s = "åå=aaa,aab,aac";
        
        List<String> list = new ArrayList();
        list.add("aaa");
        list.add("aab");
        list.add("aac");
        DictionaryEntry expResult = new DictionaryEntry("åå",list);
        
        DictionaryEntry result = DictionaryEntryConverter.toObject(s);
        
        assertEquals(
                expResult.toString(), 
                result.toString()
        );
    }

    /**
     * Test of toString method, of class DictionaryEntryConverter.
     */
    @Test
    public void testToString() {
        System.out.println("toString");
        
        List<String> list = new ArrayList();
        list.add("aaa");
        list.add("aab");
        list.add("aac");
        DictionaryEntry instance = new DictionaryEntry("åå",list);
        
        String expResult = "åå=aaa,aab,aac";
        
        String result = DictionaryEntryConverter.toString(instance);
        assertEquals(expResult, result);
    }
    
}
