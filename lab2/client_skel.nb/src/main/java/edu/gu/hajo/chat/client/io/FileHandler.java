package edu.gu.hajo.chat.client.io;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Utility for file handling using NIO
 * classes (new IO)
 *
 * @author hajo
 *
 */
public final class FileHandler {

    public static byte[] readFile(Path path, String fileName) throws FileNotFoundException, IOException {
        return Files.readAllBytes(path.resolve(fileName));
    }

    public static void saveFile(Path path, String fileName, byte[] data) throws IOException {
        Files.write(path.resolve(fileName), data);
    }

    public static List<String> listDirectoryContent(Path path) throws IOException {
        List<String> fileNames = new ArrayList<>();
        DirectoryStream<Path> directoryStream = Files.newDirectoryStream(path);
        for (Path p : directoryStream) { 
            fileNames.add(p.getFileName().toString());
        }
        return fileNames;
    }
}
