Scalable event notification
===========================

The event library provides scalable event notification for file
handles, sockets, timers, etc.

Contributing
------------

### Prerequisites

Make sure you read the [Haskell Style Guide] [1].

The existing code doesn't follow the style guide fully but you should
follow it for all new code.

### Creating patches

The preferred way of contributing changes to the project is to use Git
and send the patches over email using the Git commands `format-patch`
and `send-email`.  Step by step instructions:

Clone the repository:

    git clone http://github.com/tibbe/event

Make your changes:

    cd event
    $EDITOR <file>

Commit your changes in one or more commits:

    git add <file>
    git commit

Make sure you write a good commit message.  Commit messages should
contain a short summary on a separate line and, if needed, a more
thorough explanation of the change.  Write full sentences and use
proper spelling, punctuation, and grammar.  See
[A Note About Git Commit Messages] [2] for more information.

You might want to use `git rebase` to make sure your commits
correspond to nice, logical commits.  Make sure whitespace only
changes are kept in separate commits to ease reviewing.

Prepare your patches for sending:

    git format-patch -n origin

This will create one patch file per patch.

    git send-email --to <maintainer> <patch files>

The maintainer is specified in the Cabal file.  The maintainer will
review your changes and may ask you to make changes to them.  Make the
changes to your local repository and use `git rebase` to massage them
into nice, logical commits and resend the patches.

Running the tests
-----------------

You can run all unit tests by running:

    cabal configure && cabal build && make -C tests run-tests

References
----------

[1]: http://github.com/tibbe/haskell-style-guide
[2]: http://www.tpope.net/node/106
