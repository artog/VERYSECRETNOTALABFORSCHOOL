package edu.gu.hajo.trie;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

public class TestConnectableTrie {

    // ------- Testing non connected trie ------------
    
    @Test(expected = NullPointerException.class)
    public void testInsertNull() {
        IConnectableTrie t = ConnectableTrieFactory.newInstance();
        t.insert(null);
    }

    @Test
    public void testInsertEmpty() {
        IConnectableTrie t = ConnectableTrieFactory.newInstance();
        Connector c = t.insert("");
        assertTrue(c != null);
    }

    @Test
    public void testInsert() {
        IConnectableTrie t = ConnectableTrieFactory.newInstance();
        Connector c = t.insert("abc");
        assertTrue(c != null);
    }

    @Test(expected = NullPointerException.class)
    public void testContainsNull() {
        IConnectableTrie t = ConnectableTrieFactory.newInstance();
        t.contains(null);
    }

    @Test
    public void testContainsEmpty() {
        IConnectableTrie t = ConnectableTrieFactory.newInstance();
        assertFalse(t.contains(""));
    }

    @Test
    public void testInsertAndContains() {
        IConnectableTrie t = ConnectableTrieFactory.newInstance();
        t.insert("aaa");
        t.insert("aab");
        t.insert("abc");
        assertTrue(t.contains("aaa"));
        assertTrue(t.contains("aab"));
        assertTrue(t.contains("abc"));

        assertFalse(t.contains("ab"));
        assertFalse(t.contains("a"));
        assertFalse(t.contains("abcxxxxx"));
    }

    @Test(expected = NullPointerException.class)
    public void testGetKeysForNull() {
        IConnectableTrie t = ConnectableTrieFactory.newInstance();
        t.getKeys(null);
    }

    @Test
    public void testGetKeysForEmpty() {
        IConnectableTrie t = ConnectableTrieFactory.newInstance();
        List<String> ks = t.getKeys("");
        assertTrue(ks != null && ks.size() == 0);
    }

    @Test
    public void testGetKeys() {
        IConnectableTrie t = ConnectableTrieFactory.newInstance();
        List<String> ks = t.getKeys("abcdefghi");
        assertTrue(ks != null && ks.isEmpty());
    }

    @Test(expected = NullPointerException.class)
    public void testGetValuesForNull() {
        IConnectableTrie t = ConnectableTrieFactory.newInstance();
        t.getKeys(null);
    }

    @Test
    public void testGetValuesForEmpty() {
        IConnectableTrie t = ConnectableTrieFactory.newInstance();
        List<String> vs = t.getKeys("");
        assertTrue(vs != null && vs.isEmpty());
    }

    @Test
    public void testGetValues() {
        IConnectableTrie t = ConnectableTrieFactory.newInstance();
        List<String> vs = t.getKeys("abcdefghij");
        assertTrue(vs != null && vs.isEmpty());
    }

    // ----- Testing addPeered tries --------
    @Test
    public void testInsertKeysAndValuesAndGetAllValues() {
        IConnectableTrie k = ConnectableTrieFactory.newInstance();
        IConnectableTrie v = ConnectableTrieFactory.newInstance();

        Connector ck = k.insert("");
        Connector cv1 = v.insert("v1");
        Connector cv2 = v.insert("v2");
        Connector cv3 = v.insert("v3");
        ck.connect(cv1);
        ck.connect(cv2);
        ck.connect(cv3);

        List<String> vs = k.getValues(""); // Will get all
        assertTrue(vs != null && vs.size() == 3);
    }

    @Test
    public void testInsertKeysAndCheckPrefix() {
        IConnectableTrie k = ConnectableTrieFactory.newInstance();
        IConnectableTrie v = ConnectableTrieFactory.newInstance();
        // The keys
        Connector ck1 = k.insert("aaa");
        Connector ck2 = k.insert("aab");
        Connector ck3 = k.insert("abc");
        // The values
        Connector cv1 = v.insert("");
        Connector cv2 = v.insert("");
        Connector cv3 = v.insert("");

        ck1.connect(cv1);
        ck2.connect(cv2);
        ck3.connect(cv3);

        // Checking different prefix's
        List<String> ks = k.getKeys("aaa");
        assertTrue(ks.size() == 1);

        ks = k.getKeys("aa");
        assertTrue(ks.size() == 2);

        ks = k.getKeys("a");
        assertTrue(ks.size() == 3);
    }

    @Test
    public void testGetValuesForKeys() {
        IConnectableTrie k = ConnectableTrieFactory.newInstance();
        IConnectableTrie v = ConnectableTrieFactory.newInstance();
        // The keys
        Connector ck1 = k.insert("aaa");
        Connector ck2 = k.insert("aab");
        Connector ck3 = k.insert("abc");
        // The values
        Connector cv1 = v.insert("AAA");
        Connector cv2 = v.insert("AAB");
        Connector cv3 = v.insert("ABC");

        ck1.connect(cv1);
        ck2.connect(cv2);
        ck3.connect(cv3);

        // Get all keys starting with 'a'
        List<String> ks = k.getKeys("a");
        assertTrue(ks.size() == 3);

        List<String> vs = new ArrayList<>();
        for (String s : ks) {
            vs.addAll(k.getValues(s));
        }
        assertTrue(vs.contains("AAA") && vs.contains("AAB") && vs.contains("ABC"));
    }
}
