POT
===

Provides:

- a simple operational transform implementation (quickchecked!)
- a server over TCP
- a client that can hook up to some text editor via websockets (currently using codemirror.js)

Run:

```
./pot-server <tcp-port>
./pot-client 127.0.0.1 <websocket-port> <server addr> <tcp-port>
```
serve the js client at ``localhost:xxxx/?<websocket-port>``

Problems:

- race between the client and the js-based editor

Wishlist:

- a client that operates on ``Delta`` (the operational transform type) natively so we don't have to deal with the above race 

maybe helpful slides: http://defma.in/talk.pdf
