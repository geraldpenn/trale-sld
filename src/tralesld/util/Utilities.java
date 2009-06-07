package tralesld.util;

import java.io.File;

public class Utilities {
	
	public static void deleteRecursively(File directory) {
		if (directory.isDirectory()) {
			for (File file : directory.listFiles()) {
				deleteRecursively(file);
			}
		}
		
		directory.delete();
	}

}
