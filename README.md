POT
===

Provides:

- a simple operational transform implementation (quickchecked!)
- a server over TCP
- a client that can hook up to some text editor via websockets (currently using codemirror.js)

Problems:

- race between the client and the js-based editor

Wishlist:

- a client that operates on ``Delta`` (the operational transform type) natively so we don't have to deal with the above race 
