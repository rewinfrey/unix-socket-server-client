# Unix Socket Server / Client

This is a simple Unix socket echo server and client debugging tool I built while working on Unix sockets for a work project. This is not intended to be used in any serious capacity. It is fragile, has little to no error checking, and is not intended to be run anywhere near a production environment.

### Why?

I wanted to better understand socket operations in Haskell, and to have a debugging tool for verification that socket traffic adhered to the correct shape. This program has two run modes, a `server` and a `client`. The server writes to its standard output whatever is received from the client, and echoes the message back to the client over the socket. The client prompts for user input, and sends whatever is input to the client's standard input to the socket.

### Setup

```
$ git clone git@github.com:rewinfrey/unix-socket-server-client.git
$ cd unix-socket-server-client
$ stack build
```

### Run modes

**Server mode**:

`stack exec socket -- --mode server --path path/to/socket`

**Client mode**:

`stack exec socket -- --mode client --path path/to/socket`

To exit and close the socket, input `:quit` to the client's standard input. This will be interpreted as a kill signal by the server.

### License

MIT
