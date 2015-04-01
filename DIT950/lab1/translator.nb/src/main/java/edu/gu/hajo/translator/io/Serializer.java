package edu.gu.hajo.translator.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * Serialize/Deserialize map's from/to disk Used to save options/preferences
 *
 * @author hajo
 *
 */
public class Serializer {

    public static Map<String, Object> load(String fileName) throws IOException,
            ClassNotFoundException {

        File file = new File(fileName);
        FileInputStream fileStream;
        fileStream = new FileInputStream(file);
        ObjectInputStream in = new ObjectInputStream(fileStream);
        @SuppressWarnings("unchecked")
        HashMap<String, Object> keyValue = (HashMap<String, Object>) in
                .readObject();
        in.close();
        System.out.println("Load: " + keyValue);
        return keyValue;
    }

    public static void save(String fileName, Map<String, Object> keyValue)
            throws IOException {
        System.out.println("Save: " + keyValue);
        File filename = new File(fileName);
        ObjectOutputStream out;
        out = new ObjectOutputStream(new FileOutputStream(filename));
        out.writeObject(keyValue);
        out.close();
    }

}
