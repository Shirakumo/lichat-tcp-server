#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.tcp-server)

(docs:define-docs
  (variable *default-port*
    "The standard port on which the server will run.

Should be 1111.")

  (function ensure-hostname
    "Ensures that the host-ish is turned into a hostname string.")

  (type server
    "Server class. You should instantiate this.

Access to the server is properly handled for
mutual exclusion from threads by its LOCK. Particularly,
the following updates and processing methods for updates
are mutually excluded via this lock:

 LICHAT-SERVERLIB:TEARDOWN-CONNECTION
 LICHAT-PROTOCOL:CONNECT
 LICHAT-PROTOCOL:REGISTER
 LICHAT-PROTOCOL:CREATE

See LICHAT-SERVERLIB:FLOOD-PROTECTED-SERVER
See HOSTNAME
See PORT
See THREAD
See PING-INTERVAL
See LOCK
See CONNECTIONS")

  (function hostname
    "Accessor to the hostname of the object.

See SERVER")

  (function port
    "Accessor to the port of the object.

See SERVER
See *DEFAULT-PORT*")

  (function thread
    "Accessor to the background processing thread of the object.

See SERVER
See CONNECTION")

  (function ping-interval
    "Accessor to the amount of seconds to wait for an update before sending a PING.

See SERVER")

  (function lock
    "Accessor to the lock of the object that is used to mutually exclude access.

See SERVER 
See CONNECTION
See USER
See CHANNEL")

  (function connections
    "Accessor to the list of connections on the server.

See SERVER
See CONNECTION")

  (type connection
    "Connection class. Each connection to a client will have an instance of this class.

Access to the connection is properly handled for
mutual exclusion from threads by its LOCK. Particularly,
the following methods are mutually excluded via this lock:

  LICHAT-SERVERLIB:SEND

See LICHAT-SERVERLIB:FLOOD-PROTECTED-CONNECTION
See SOCKET
See THREAD
See LOCK")

  (function socket
    "Accessor to the TCP socket of the connection.

See USOCKET:SOCKET
See CONNECTION")
  
  (function hostname
    "Accessor to the hostname of the connection.

See CONNECTION")

  (function port
    "Accessor to the port of the connection.

See CONNECTION")

  (type channel
    "Channel class.

Access to the channel is properly handled for
mutual exclusion from threads by its LOCK. Particularly,
the following methods are mutually excluded via this lock:

  LICHAT-SERVERLIB:JOIN
  LICHAT-SERVERLIB:LEAVE

See LICHAT-SERVERLIB:CHANNEL
See LOCK")

  (type user
    "User class.

Access to the user is properly handled for
mutual exclusion from threads by its LOCK. Particularly,
the following methods are mutually excluded via this lock:

  LICHAT-SERVERLIB:JOIN
  LICHAT-SERVERLIB:LEAVE

See LICHAT-SERVERLIB:USER
See LOCK")

  (function open-connection
    "Start accepting incoming connections on the server.

This will launch a background thread which will
call HANDLE-CONNECTION on the socket and server.

See HANDLE-CONNECTION
See THREAD
See SERVER")

  (function close-connection
    "Stop accepting incoming connections and close all existing ones.

Can be used with either a SERVER or a CONNECTION.

See CONNECTIONS
See THREAD
See SERVER
See CONNECTION")

  (function handle-connection
    "Handle the socket with the given object.

The object should be either a SERVER or a CONNECTION.

In the case of the server, it will listen for new clients
and if one is found, call ESTABLISH-CONNECTION.

In the case of the client, it will first manage connection
establishment as per the Lichat protocol, then repeatedly
wait for a new update with a timeout. If the timeout is 
reached, a PING update is sent to the connection. If an
update is received, it is PROCESSED. If the socket ever
experiences a connection problem (timeout, shutdown, reset,
etc) then the connection is immediately closed.
Provides a CLOSE-CONNECTION restart as mandated by the
serverlib.

See SERVER
See CONNECTION
See ESTABLISH-CONNECTION
See LICHAT-SERVERLIB:PROCESS")

  (function establish-connection
    "Responsible for establishing a new connection to a client.

This will construct and push a new CONNECTION object
onto the server using the given socket. It will also
launch the CONNECTION's background handling thread."))
