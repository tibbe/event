module System.Event
    ( -- * Types
      EventManager,

      -- * Creation
      new,

      -- * Registering interest in I/O events
      Event,
      evtRead,
      evtWrite,
      IOCallback,
      FdRegistration,
      registerFd,
      unregisterFd,

      -- * Registering interest in timeout events
      TimeoutCallback,
      registerTimeout,
      updateTimeout,
      clearTimeout
    ) where

import System.Event.Manager
