package tralesld.server;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import tralesld.util.Log;

/**
 * A generic central unit for a multi-client TCP socket server. When a new
 * client connection comes in, it is handed to the listener(s), who are then
 * solely responsible for the actual communication with the client - even for
 * closing the client socket.
 * 
 * @author Kilian Evang, based on Niels Ott's {@link gralej.server.SocketServer}
 */
public class SocketServer {

    private InetAddress address;

    private int port;

    private Set<IClientListener> listeners;

    private ServerSocket socket;

    private ConnectionWaiter waiter;

    public SocketServer(InetAddress address, int port) {
	this.address = address;
	this.port = port;
	listeners = Collections.synchronizedSet(new HashSet<IClientListener>());
    }

    /**
     * Always waits for the next incoming connection, hands the client socket to
     * the listener(s), and starts waiting again, until the socket is closed.
     */
    private class ConnectionWaiter extends Thread {

	private boolean stopping;

	public void run() {
	    try {
		while (!stopping) {
		    Socket clientSocket = socket.accept();
		    notifyListeners(clientSocket);
		}
	    } catch (IOException e) {
		if (stopping) {
		    // socket was closed
		} else {
		    Log.error("I/O exception while waiting for client "
			    + "connection. Server terminates");
		}
	    }
	}

	public void shutdown() {
	    stopping = true;

	    try {
		socket.close();
	    } catch (IOException e) {
		Log.warn("I/O exception while trying to close socket");
	    }
	}

    }

    public void registerClientListener(IClientListener listener) {
	if (listener == null) {
	    throw new NullPointerException("Listeners must not be null.");
	}

	listeners.add(listener);
    }

    public void removeClientListener(IClientListener listener) {
	listeners.remove(listener);
    }

    protected void notifyListeners(Socket socket) {
	for (IClientListener listener : listeners) {
	    listener.newClient(socket);
	}
    }

    public void startListening() throws IOException {
	if (waiter != null) {
	    return;
	}

	// open the port, this may go wrong
	socket = new ServerSocket(port, 0, address);

	// run the server main loop thread
	waiter = new ConnectionWaiter();
	waiter.start();
    }

    public boolean isListening() {
	return (waiter != null && socket.isBound());
    }

    public void stopListening() throws IOException {
	waiter.shutdown();
	waiter = null;
	Log.debug("Socket server has stopped listening");
    }

}
