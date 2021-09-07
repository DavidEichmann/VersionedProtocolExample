
# How it works

Most of the work is in core. Like the current paradigm used in
ouroborus-network, we model protocols as a state machine described with the
`Protocol` type class. The difference here with versioning support is that we
keep a copy of each version of the protocol as well as its Protocol instance. We
then manually write code to upgrade client peers and downgrade server peers e.g.

```haskell
import Core
import qualified Protocol.V1 as V1
import qualified Protocol.V2 as V2

downgradeServerV2ToV1 ::
  forall m a.
  Monad m =>
  Peer V2.MyProtocol AsServer V2.StIdle m a ->
  Peer V1.MyProtocol AsServer V1.StIdle m a

upgradeClientV1ToV2 ::
  forall m a.
  Monad m =>
  Peer V1.MyProtocol AsClient V1.StIdle m a ->
  Peer V2.MyProtocol AsClient V2.StIdle m a
```

the client is then implemented in a relatively low version and the server
implemented in a relatively high version. We call these the client or server
"implementation" protocol version. Given the client/server implementation
protocol version and a chain of clientUpgrade/serverDowngrade functions, the
client/server effectively support a range of protocol versions.

On connecting, the client and server perform a handshake, advertising which
versions they support and agreeing on the highest mutually supported version
which we call the "on-the-wire" protocol version. For example we may have:

```haskell
import Core
import qualified Protocol.V1 as V1
import qualified Protocol.V2 as V2
import qualified Protocol.V3 as V3

clientV1 :: Peer V1.MyProtocol AsClient V1.StIdle m a
clientV1 = ... -- an actual implementation
clientV2 :: Peer V2.MyProtocol AsServer V2.StIdle m a
clientV2 = upgradeClientV1ToV2 clientV1
clientV3 :: Peer V3.MyProtocol AsServer V3.StIdle m a
clientV3 = upgradeClientV2ToV3 clientV2
upgradeClientV1ToV2 :: Monad m =>
  Peer V1.MyProtocol AsClient V1.StIdle m a ->
  Peer V2.MyProtocol AsClient V2.StIdle m a
upgradeClientV2ToV3 :: Monad m =>
  Peer V2.MyProtocol AsClient V2.StIdle m a ->
  Peer V3.MyProtocol AsClient V3.StIdle m a
```

and

```haskell
import Core
import qualified Protocol.V3 as V3
import qualified Protocol.V4 as V4
import qualified Protocol.V5 as V5

serverV5 :: Peer V5.MyProtocol AsServer V5.StIdle m a
serverV5 = ... -- an actual implementation
serverV4 :: Peer V4.MyProtocol AsServer V4.StIdle m a
serverV4 = downgradeServerV5ToV4 serverV5
serverV3 :: Peer V3.MyProtocol AsServer V3.StIdle m a
serverV3 = downgradeServerV4ToV3 serverV4
downgradeServerV5ToV4 :: Monad m =>
  Peer V5.MyProtocol AsServer V5.StIdle m a ->
  Peer V4.MyProtocol AsServer V4.StIdle m a
downgradeServerV4ToV3 :: Monad m =>
  Peer V4.MyProtocol AsServer V4.StIdle m a ->
  Peer V3.MyProtocol AsServer V3.StIdle m a
```

In which case the client supports versions 1,2,3 and the server supports 3,4,5
and the handshake result in an on-the-wire protocol version 3:

* client implementation version: 1
* client supported versions: 1,2,3
* server implementation version: 5
* server supported versions: 3,4,5
* on-the-wire version: 3 (= maximum (intersect [1,2,3] [3,4,5]))

At that point the client uses `clientv3` and the server uses `serverV3`. An all
works out well.

# Changing protocol Version

In some cases we want to support multiple client behaviours based on the
on-the-wire protocol version! For example, John Ky's recently implemented some
behavior that says:

  If we are using version X or newer then make a "system start time" query
  else that query is not supported so do something else.

Originally, Johns implementation did not do the version check which and resulted
in a runtime bug when the protocol version was lower than X. We'd like the types
to guide us better here such that we could avoid such bugs.

I'm (David Eichmann) currently working on a solution on the
`ChangeProtocolVersion` branch, but am having trouble getting the types to work
out conveniently with good type inference...

## The idea

The main idea is to add something like these constructors to the `Peer` Type

```haskell
data Peer p pr st m a where
  -- ...
  UpgradeVersion ::
    forall (pr :: PeerRole) (protocolA :: Type) (stA :: protocolA) (protocolB :: Type) m a.
    Proxy protocolB ->
    Peer protocolB pr (UpgradeSt pr stA protocolB) m a ->
    -- | alternative incase upgrade in not possible (i.e. the on-the-wire protocol is too low)
    Peer protocolA pr stA m a ->
    Peer protocolA pr stA m a
  DowngradeVersion ::
    forall (pr :: PeerRole) (protocolA :: Type) (stA :: protocolA) (protocolB :: Type) (stB :: protocolB) m a.
    Proxy stB ->
    Peer protocolB pr stB m a ->
    -- NOTE we dont need an alternative case like in UpgradeVersion; downgrading should always be possible, regardless of the on-the-wire version, assuming the relevant client upgrade functions exist.
    Peer protocolA pr stA m a

-- | Upgrade the protocol state in protocolA to the equivalent protocol state in protocolB
type family UpgradeSt (pr :: PeerRole) (stA :: protocolA) (protocolB :: Type) :: protocolB
```

The fundamental idea here is that when we actually run the client Peer, we use
the upgrade functions as usual to match the on-the-wire version, but whenever we
encounter a `UpgradeVersion vA vB` or `DowngradeVersion vA`, we check to see if
an upgrade path exists from the `vA` versioned peer to the on-the-wire version.
If it does, then that peer is upgraded and used to continue the protocol. Note
that even in the `DowngradeVersion` case we are actually still **upgrading** to
the on-the-wire version.

### What's the problem?

This sounds simple, but the hard part seems to be with implementing `UpgradeSt`.
When the programer uses `UpgradeVersion`/`DowngradeVersion` we want static
checks that:

* The new state is equivalent to the old state (see `UpgradeSt`) so we can't
  accidentally transition e.g. from a V1.StIdle state to a V2.StPinged state
  without sending a ping message.
* Attempting to `UpgradeVersion`/`DowngradeVersion` is only possible when the
  relevant upgrade functions exist i.e.

# Thoughts / Future Work / Ideas

I need to close the gap between what we have in production and this toy example.
Let's make V3 of the protocol a subset of the local query protocol.

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
