package tralesld;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;

import tralesld.server.IClientListener;
import tralesld.server.SocketServer;
import tralesld.util.Log;

public class Controller implements IClientListener {

    private SocketServer server;

    private Socket clientSocket;

    private BufferedWriter out;

    public void startServer() {
	try {
	    // TODO
	    InetAddress address = InetAddress.getByName("127.0.0.1");
	    server = new SocketServer(address, 5001);
	    server.registerClientListener(this);
	    server.startListening();
	} catch (UnknownHostException e) {
	    Log.error("Could not start server - unknown host");
	} catch (IOException e) {
	    Log.error("Could not start server - could not start to listen on "
		    + "socket");
	}
    }

    public void stopServer() {
	server.removeClientListener(this);
	closeClientSocket();

	try {
	    server.stopListening();
	} catch (IOException e) {
	    Log.warn("I/O exception while trying to stop server");
	}
    }

    @Override
    public void newClient(Socket socket) {
	closeClientSocket();
	clientSocket = socket;

	try {
	    out = new BufferedWriter(new OutputStreamWriter(clientSocket
		    .getOutputStream()));
	} catch (IOException e) {
	    Log.error("Could not get client socket output stream");
	}
    }

    private void closeClientSocket() {
	if (clientSocket != null && !clientSocket.isClosed()) {
	    try {
		clientSocket.close();
	    } catch (IOException e) {
		Log.warn("I/O exception while trying to close old client "
			+ "socket.");
		Log.debug(e);
	    }
	}

	clientSocket = null;
	out = null;
    }

    public void creep() {
	talkToClient("c\n");
    }

    public void skip() {
	talkToClient("s\n");
    }

    public void retry() {
	talkToClient("r\n");
    }

    public void abort() {
	talkToClient("a\n");
    }

    public void fail() {
	talkToClient("f\n");
    }

    public void leap() {
	talkToClient("l\n");
    }

    private void talkToClient(String talk) {
	if (out != null) {
	    try {
		out.append(talk);
		out.flush();
	    } catch (IOException e) {
		Log.error("Could not communicate to client - I/O exception");
	    }
	} else {
	    Log.error("Could not communicate to client - no connection");
	}
    }

    public void exit() {
	stopServer();
    }

}
