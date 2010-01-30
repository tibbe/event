#ifndef __HS_EVENT_H__
#define __HS_EVENT_H__

#include "EventConfig.h"

#if defined(HAVE_SIGNAL_H)
# include <signal.h>
#endif

#if !defined(INLINE)
# if defined(_MSC_VER)
#  define INLINE extern __inline
# else
#  define INLINE static inline
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

INLINE int __hsevent_sigusr2(void)
{
    return SIGUSR2;
}

#endif /* __HS_EVENT_H__ */
/*
 * Local Variables: 
 * c-file-style: "stroustrup" 
 * End: 
 */
