#ifndef __HS_EVENT_H__
#define __HS_EVENT_H__

#include "EventConfig.h"

#include <signal.h>
#include <pthread.h>

#if defined(HAVE_SYS_SELECT_H)
#include <sys/select.h>
#endif
#if defined(HAVE_SYS_TIME_H)
#include <sys/time.h>
#endif
#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif
#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if !defined(INLINE)
# if defined(_MSC_VER)
#  define INLINE extern __inline
# else
#  define INLINE inline
# endif
#endif

INLINE int __hsevent_num_signals(void)
{
#if defined(_NSIG)
    return _NSIG;
#else
    return 128; /* best guess */
#endif
}

INLINE void __hsevent_thread_self(pthread_t *tid)
{
    *tid = pthread_self();
}

INLINE int __hsevent_kill_thread(pthread_t *tid, int sig)
{
    return pthread_kill(*tid, sig);
}

INLINE int  __hsevent_fd_isset(int fd, fd_set *fds)
{
    return FD_ISSET(fd, fds);
}

INLINE void __hsevent_fd_set(int fd, fd_set *fds)
{
    FD_SET(fd, fds);
}

INLINE void __hsevent_fd_zero(fd_set *fds)
{
    FD_ZERO(fds);
}

#endif /* __HS_EVENT_H__ */
/*
 * Local Variables: 
 * c-file-style: "stroustrup" 
 * End: 
 */
