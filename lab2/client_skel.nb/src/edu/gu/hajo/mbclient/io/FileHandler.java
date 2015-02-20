package edu.gu.hajo.mbclient.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import edu.gu.hajo.mbcommon.MBFile;

/**
 * @author hajo
 * 
 */
public final class FileHandler {

    public static void store(MBFile file, File downloadDir) throws IOException {
        FileOutputStream out = null;
        out = new FileOutputStream(downloadDir + "/" + file.getFileName());
        try {
            out.write(file.getBytes());
        } finally {
            out.close();
        }
    }
    /*
     * NOTE: This will possibly use **A LOT** of memory Don't try to read a
     * movie!!
     */
    public static MBFile getFile(String fileName, File uploadDir) throws IOException {
        FileInputStream in = null;
        MBFile file = null;
        File f = new File(uploadDir + "/" + fileName);
        int size = (int) f.length();
        byte[] bytes = new byte[size];
        in = new FileInputStream(f); 
                                    
        int nBytes = 0;
        try {
            nBytes = in.read(bytes);
        } finally {
            in.close();
        }
        if (nBytes != size) {
            throw new IOException("Exception while reading file");
        }
        file = new MBFile(fileName, bytes);
        return file;
    }
}
