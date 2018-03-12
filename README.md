# abstract-network

An abstraction over the network layer in Haskell, facilitating easy development of distributed applications.

This allows you to create a virtual in-memory network for development easy, and then switch over to a UDP based network seamlessly, with a single line change in your application.

## Example usage:
Here, two networks are set up (lists of User Network Contexts).
```
uncsVirtual <- liftIO $ createFakeNetwork addrs
uncsUDPReal <- liftIO $ mapM createUDPNode addrs
```

`uncsVirtual` is a list of virtual network endpoints. An application can send a message over one of them, which will be received on the target end.
`uncsUDPReal` is a list of UDP network sockets, which can be used across processes.

Only `SockAddrInet` addresses are supported right now. A provided function `simpleAddrToNetAddr` allows converting `(String, Int)` tuples to this format easily.

## Important API
The following methods are available on every `UserNetContext`. These methods will function correctly, regardless of whether the underlying network is a UDP network or an in-memory virtual network.

### sendMsg
```
sendMsg  :: MonadIO m => NetAddr -> B.ByteString -> m ()
```

This method will send the given bytes to the given network address.

### msgQueue
```
msgQueue :: OutChan NetMsg
```

This is a blocking output channel ([Unagi](https://github.com/jberryman/unagi-chan)), from which received messages can be read-off.

## Misc
My project [gossip-haskell](https://github.com/sakshamsharma/gossip-haskell) uses this. It allows creating a gossip-protocol based communication network over such a network.

## TODO
* Allow non-blocking message queues.
* Better network address handling (support all types of SockAddrs).
* Support TCP connections.
* Keep UDP sockets open for a while, do not close every single time.
* Better documentation.
