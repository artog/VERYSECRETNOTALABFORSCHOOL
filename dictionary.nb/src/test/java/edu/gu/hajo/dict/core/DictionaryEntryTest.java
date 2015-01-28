/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.gu.hajo.dict.core;

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
public class DictionaryEntryTest {
    
    /**
     * Test of getSource method, of class DictionaryEntry.
     */
    @Test
    public void testGetSource() {
        List<String> list = new ArrayList();
        list.add("ab");
        list.add("ac");
        DictionaryEntry instance = new DictionaryEntry("aa",list);
        String expResult = "aa";
        
        String result = instance.getSource();
        assertEquals(expResult, result);
    }

    /**
     * Test of getTranslations method, of class DictionaryEntry.
     */
    @Test
    public void testGetTranslations() {
        
        List<String> list = new ArrayList();
        list.add("aaa");
        list.add("aab");
        list.add("aac");
        DictionaryEntry instance = new DictionaryEntry("aa",list);
        list.remove(1);
        
        List<String> result = instance.getTranslations();
        assertTrue(result.size() == 2 && "aab".equals(result.get(1)));
    }

    /**
     * Test of toString method, of class DictionaryEntry.
     */
    @Test
    public void testToString() {
        List<String> list = new ArrayList();
        list.add("aaa");
        list.add("aab");
        list.add("aac");
        DictionaryEntry instance = new DictionaryEntry("aa",list);
        
        list.remove(1);
        
        String expResult = "[source=aa, translations=[aaa, aab, aac]]";
        String result = instance.toString();
        assertEquals(expResult, result);
    }
    
}
