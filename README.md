# How Versioning Works

I need to close the gap between what we have in production and this toy example.
Let's make V3 of the protocol a subset of the local query protocol.

open questions:

* How do we handle the case of a client wanting to split behaviour based on
  version! I.e. John Ky's work on system start time checks the
  NodeToClientVersion and if the request is supported it makes it omits it.
  * Lets say the server is at version *10* and supports versions *5 to 10*. The
    client is aware of versions *3 to 8* and picks version *5*. Clients
    generally pick a version as low as possible to support their use case.

      ^^^ NOT REALLY! They want to pick something hight enough that the server
      supports it, but low enought that we don't reaqure you to upgrade to the
      latest greatest server.

    Now  the client wants to still support version 5 while optionally using some
    new feature in version 6 if available (i.e. if the server supports version 6
    as well as version 5). I think what we need to do is have the client be
    described as a version 5 pear and occasionally split into a version 5 and 6
    peer. see TODO 1 below
* Do we want to make serialisation instances parameterized on protocol version?
* Many data types are defined in external libraries.
  * I suspect we can make a `SerialiseVersioned (version :: Nat) (a :: Type)`
    class in cardano-base and use that universally. Bye-bye From/ToCBOR and
    Serialise.
    * How much does a type have to change before it is a "different type" rather
      than just a "different version"?
* Can we nicely create a new version without copying all the code from the
  previous version?
  * Imagine adding a single simple query to the Message type and having to copy
    the whole module with serialisation instances and all, yuk!
  * We'd even have to write some boring server downgrade code :-(
  * I think the states will not change too often. Primarily the Message type
    will change.

## TODO

1. client versions
    * [X] Make a V3 client that allows for "requests".
    * [ ] Make a v4 client that adds a single request.
    * [ ] Write a client that primarily uses V3 but optionally uses V4 for it's
      new request.
