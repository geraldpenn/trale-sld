package tralesld.server;

import java.net.Socket;

public interface IClientListener {
    
    public void newClient(Socket socket);

}
