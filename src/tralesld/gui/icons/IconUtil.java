package tralesld.gui.icons;

import java.net.URL;

public class IconUtil {
    
    public static URL getIcon(String filename) {
	return IconUtil.class.getResource(filename);
    }

}
