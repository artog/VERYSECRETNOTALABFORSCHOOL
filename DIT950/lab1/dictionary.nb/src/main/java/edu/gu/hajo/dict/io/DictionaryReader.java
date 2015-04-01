package edu.gu.hajo.dict.io;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * Simple reader for row based data
 *
 * File format for data;
 *
 * Row Row Row ...
 *
 * Where Row is;
 *
 * word=translation1, translation2,....translationN \n
 *
 * Rows starting with # are comments
 *
 * @author hajo
 *
 */
public enum DictionaryReader {

    INSTANCE;

    public static final char COMMENT = '#';
    public static final int EMPTY_LINE = 0;
    public static final String DEFAULT_PATH = "src/main/resources/dict/";
    public static final String SUFFIX = ".dict";
    public static final String SEPARATOR = "2";

    public List<String> open(URI uri) throws IOException {
        Scanner sc = null;
        List<String> entries = new ArrayList<>();
        switch (uri.getScheme()) {
            case "file":
                sc = new Scanner(new File(uri));
                break;
            case "http":
                sc = new Scanner(uri.toURL().openStream());
                break;
            default:
                throw new IllegalArgumentException("Bad scheme");
        }
        try {
            String line;
            while (sc.hasNextLine()) {
                line = sc.nextLine();
                if (line.length() > EMPTY_LINE && line.charAt(0) != COMMENT) {
                    entries.add(line.trim());
                }
            }
        } finally {
            sc.close();
        }
        return entries;
    }

}
