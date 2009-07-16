An Event Notification Library for Haskell
=========================================

Overview
--------

This library provides an efficient way to monitor a large number of
file descriptors for readiness change notifications and timers for
timeouts.  The API executes a user-supplied callback on each event.

### Goals

The primary goal of this library is to provide an event notification
system scalable enough to build high performance servers as outlined
in [The C10K Problem](http://www.kegel.com/c10k.html).

The secondary goal is to allow the library to replace the `select`-based
event notification system currently used in the GHC RTS.

High Level Design
-----------------

The library is designed around a so-called event loop.  In each
iteration of the loop the library checks

* if any file descriptors have become ready, and
* if any timeouts have occurred.

The first point is achieved using a system call like `select` or
`epoll`.  Different operating systems provide different system calls.
We will refer to the different system calls as different *back-ends*.
The second point is achieved using a priority queue that stores
all timeouts sorted by remaining time.

Detailed Design
---------------

### Storing The Callbacks

Once we know which file descriptors are ready, by polling the back-end,
we need to find the callback associated with each file descriptor.
To do so efficiently we store the callbacks in an array indexed by the
file descriptor.  Since operating systems typically allocate file
descriptors in an increasing fashion the size of this array should not
be much bigger than the number of watched file descriptors.

The callback associated with a given file descriptor might change over
time and thus we need a mutable array to store the callbacks.
Furthermore, since the number of watched file descriptors grows over
time we need to grow the array.  We need something similar to a C++
`vector`:

    -- | A mutable, dynamic array type for use in the @ST@ monad.
    newtype Vector s a = Vector (STRef s (C s a))

    -- The actual array contents.
    data C s a = C
        {-# UNPACK #-} !(MutableArray# s a)  -- Elements
        {-# UNPACK #-} !Int                  -- Capacity
        {-# UNPACK #-} !Int                  -- Current length

It might be possible to use a different data type as long as it
supports efficient indexing and is resizable.

Note: Once the library is used from the RTS we might need to
synchronize access to it as several different threads might use it at
the same time.

### Back-end Interface

Each back-end must support polling for new events and registering
interest in events on file descriptors.  This suggests an interface of
the form:

    -- | The possible I/O events.
    data Event = Read | Write

    class Backend a where
        set :: a
            -> CInt     -- File descriptor
            -> [Event]  -- Events to watch for
            -> IO ()

        poll :: a
             -> (CInt -> [Event] -> IO ())  -- Callback (see below)
             -> IO ()

Note that the back-end does not store the client supplied callbacks
directly.  Instead we provide the back-end with a callback that in
turn will look up the user-supplied callback in the array of file
descriptors described in the previous section.
