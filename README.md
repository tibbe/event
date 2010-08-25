Scalable event notification
===========================

The event library provides scalable event notification for file
handles, sockets, timers, etc.

Contributing
------------

The event library has been merged into the main GHC tree and this
repository only exists preserve the project history.  If you want to
modify the event manager you can do so by checking out the base
library

    darcs get --lazy http://darcs.haskell.org/libraries/base

and changing the code under `System.Event`.
