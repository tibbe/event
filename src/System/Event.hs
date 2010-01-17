module System.Event
    ( -- * Types
      EventManager,

      -- * Creation
      new,

      -- * Running
      loop,

      -- * Registering interest in I/O events
      Event,
      evtRead,
      evtWrite,
      IOCallback,
      FdRegistration,
      registerFd,
      unregisterFd,
      fdWasClosed,

      -- * Registering interest in timeout events
      TimeoutCallback,
      registerTimeout,
      updateTimeout,
      clearTimeout
    ) where

import System.Event.Manager
