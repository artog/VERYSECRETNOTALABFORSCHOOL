package edu.gu.hajo.dict;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import edu.gu.hajo.dict.io.TestDictionaryReader;


/**
 * A test suite run all tests
 * 
 * @author hajo
 * 
 */

@RunWith(Suite.class)
@Suite.SuiteClasses({ TestDictionaryReader.class,
		TestDictionary.class, TestDictionaryFactory.class })
public class TestSuite {
	// Not much use
}
