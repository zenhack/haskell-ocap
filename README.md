Libraries for writing object-capability (ocap) safe code in Haskell.

# Why

In theory, Haskell is a great language for running sandboxed code.
Being purely functional means that most code can't perform IO. While
features such as `unsafePerformIO` threaten this guarantee, there exists
a `SafeHaskell` language extension that can be used to wall off the
unsound parts of the language, so that you can really believe the types.

And indeed, some projects have had some success sandboxing Haskell code
using these mechanisms.

However, when some access to IO is needed things get more difficult.
The standard library has the `IO` God-monad, which grants code running
in it the ability to do anything.

There has been extensive exploration of finer-grained effect monads,
the idea being rather than grant code access to the all powerful `IO`,
have separate effect monads for filesystem access, network access, etc.
This is better in terms of avoiding unnecessary authority, but it lacks
composability and is still too coarse-grained.

## Composability

Much ink has been spilled on the problem of composing monads, and while
useful things have come of this, composing monads is clumsier than is
ideal. Furthermore, the separation of access we're interested in here
has nothing to do with monads: Ultimately, our effect monads for the
filesystem, networking etc, will be trivial wrappers around `IO` whose
bind and return methods do nothing of interest. Monads fundamentally
have no relationship to the composition we're after -- it thus seems
silly to create composability problems for ourselves by introducing
multiple monads in the first place, if we can avoid it.

## Granularity

Being able to grant only filesystem access, or only network access, is
an improvement over having to grant arbitrary IO access, but it still
leaves something to be desired. What if I only want to grant some code
access to a single directory, or a particular server?

# The Solution

The solution is object capabilities -- rather than control the available
effects through the type of the *monad*, we instead ban effects
involving global variables -- including the implicit global variables
provided by the operating system, like file descriptor tables. Then,
effects can only involve values (files, network hosts) that code
actually has access to. We call these access-granting values
*capabilities* (or sometimes *object capabilities* or just *ocaps*)
A hypothetical network API might look like:

```haskell

-- | A handle for a remote host
data Host

-- | A tcp port on a remote host
data TcpPort

-- | Get a 'TcpPort' for a specific host. Does not establish a
-- connection, just gets a value with more limited authority.
getHostPort :: Host -> Word16 -> TcpPort

-- | An active connection to a remote host.
data Conn

-- | Connect to a remote tcp port.
connect :: TcpPort -> OCapIO Conn

-- | Send/receive bytes on a connection:
send :: ByteString -> Conn -> OCapIO ()
recv :: Conn -> Int -> OCapIO ByteString

-- ...
```

Notice: by itself, this API provides no way to access the network at
all; just running in `OCapIO` isn't enough to do anything. If you
want to establish a connection, you need a `TcpPort`. To get one of
those, you can either accept one as an argument, or, if you have access
to a `Host` you could derive a `TcpPort` using `getHostPort`. But that
just begs the question, where does the `Host` come from? So `OCapIO`
is like `IO` in that it is one monad that lets you do any kind of IO,
but by itself doesn't actually grant any authority -- you need to have
capabilities (like `Host`s and `Port`s) to the relevant resources for
that.

This is both much finer grained and much more composable than the effect
monad approach -- no fancy types, just plain old values.

For a longer introduction to object capabilities, see [This
post](http://habitatchronicles.com/2017/05/).

# So is it turtles all the way down?

Obviously, at some point you have to get a capability to _something_ in
order to write useful programs -- otherwise you're just heating the CPU.
To that end, the `ocap-io` package provides an `IOKey` capability, that
can created in the `IO` monad and which can be used to run `IO` actions
inside `OCapIO`. Finer grained access can then be built on top of this by
capturing this IO key inside of some opaque data type (or a lambda) and
providing a more limited API to consumers. See the documentation for
that package for more information.
