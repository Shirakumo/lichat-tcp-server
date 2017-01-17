## About Lichat-TCP-Server
This is a simple, threaded, TCP-based server for the [Lichat protocol](https://shirakumo.org/projects/lichat-protocol).

## How To
Create a new server instance and pass it whatever settings you would like.

```
(defvar *server* (make-instance 'lichat-tcp-server:server))
```

Notable initargs of potential interest are:

* `:name` The name the server goes by on its own network. Defaults to `(machine-instance)`.
* `:hostname` The hostname to which the TCP listener should bind. The default is `0.0.0.0`.
* `:port` The port the TCP listener should listen on. The default is `1111`.
* `:ping-interval` The interval in which pings should be sent out to clients. The default is `60`.
* `:salt` The salt with which passwords are hashed. The default is an empty string.
* `:idle-timeout` The number of seconds without a response from a client after which it is considered to have timed out. The default is `120`.
* `:flood-frame` The size of a flood prevention frame. The default is `30`.
* `:flood-limit` The number of allowed updates within a frame before flood limitation takes effect. The default is `40`.
* `:connection-limit` The number of connections in total that the server supports before dropping incoming ones. The default is `100`.

Once a server exists, it can be started to listen to incoming connections:

```
(lichat-tcp-server:open-connection *server*)
```

The server logs information via [Verbose](http://shinmera.github.io/verbose/). If you set the REPL level to `:trace` you should see a bunch of status messages being printed every now and again.

Once you're done with the server, you can shut it down again.

```
(lichat-tcp-server:close-connection *server*)
```

## Also See

* [lichat-protocol](https://shirakumo.github.io/lichat-protocol) The Lichat protocol specification.
* [lichat-serverlib](https://shirakumo.github.io/lichat-serverlib) An agnostic implementation of the server-side protocol.
* [lichat-tcp-client](https://shirakumo.github.io/lichat-tcp-client) A basic, threaded, TCP-based implementation of a Lichat client.
* [LionChat](https://github.com/Shirakumo/lionchat) A Qt GUI client for a TCP server.
