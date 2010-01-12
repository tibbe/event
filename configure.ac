AC_INIT([event], [0.0], [io-manager@serpentine.com], [event])

AC_CONFIG_SRCDIR([src/System/Event.hs])

AC_CONFIG_HEADERS([include/EventConfig.h])

AC_CANONICAL_HOST

AC_ARG_WITH([compiler],
            [Haskell compiler],
            [HSC=$withval])
AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

AC_CHECK_HEADERS([sys/epoll.h sys/event.h ys/eventfd.h])

AC_CHECK_FUNCS([epoll_create1 eventfd kevent kevent64])

AC_OUTPUT
