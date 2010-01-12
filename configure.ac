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

AC_CHECK_HEADERS([sys/epoll.h sys/event.h sys/eventfd.h])

AC_CHECK_FUNCS([epoll_create1 epoll_ctl eventfd kevent kevent64 kqueue])

if test "$ac_cv_header_sys_epoll_h" = yes -a "$ac_cv_func_epoll_ctl" = yes; then
  AC_DEFINE([HAVE_EPOLL], [1], [Define if you have epoll support.])
fi

if test "$ac_cv_header_sys_event_h" = yes -a "$ac_cv_func_kqueue" = yes; then
  AC_DEFINE([HAVE_KQUEUE], [1], [Define if you have kqueue support.])
fi

AC_OUTPUT
