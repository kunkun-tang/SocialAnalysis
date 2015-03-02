import javax.net.SocketFactory;
import java.net.Socket;
import java.net.InetAddress;
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Map.Entry;


class MongoSocketFactory extends SocketFactory {

    private HashMap<Integer, Boolean> portMap = new HashMap<Integer, Boolean> ();
    private String remoteHost;
    private int remotePort;
    private String localHost = "127.0.0.1";
    private int localPort;


    public MongoSocketFactory(String remoteHost, int remotePort, int localPort){

        this.remoteHost = remoteHost;
        this.remotePort = remotePort;
        this.localPort = localPort;

        portMap.put(6002, true);
        portMap.put(6003, true);
        portMap.put(6004, true);
        portMap.put(6005, true);
        portMap.put(6006, true);
        portMap.put(6007, true);
    }

    public Socket createSocket() {

        for(Entry<Integer, Boolean> entry : portMap.entrySet()) {

            String key = entry.getKey();
            boolean value = entry.getValue();
            if(value == true){
                
            }
        }

        return new Socket();
    }

    public Socket createSocket(String host, int port)
    throws IOException, UnknownHostException
    {
        return new Socket(host, port);
    }

    public Socket createSocket(InetAddress address, int port)
    throws IOException
    {
        return new Socket(address, port);
    }

    public Socket createSocket(String host, int port,
        InetAddress clientAddress, int clientPort)
    throws IOException, UnknownHostException
    {
        return new Socket(host, port, clientAddress, clientPort);
    }

    public Socket createSocket(InetAddress address, int port,
        InetAddress clientAddress, int clientPort)
    throws IOException
    {
        return new Socket(address, port, clientAddress, clientPort);
    }
}
