#ifndef LONG_RUNNABLE_H_DTTWC7K3
#define LONG_RUNNABLE_H_DTTWC7K3

#include "async_notifiable.h"
#include "stoppable.h"
#include "progressable.h"

////////////////////////////////////////////////////////////////////////////////
//
// Notice Arg should be a struct when we need arguments more thant 2
//
////////////////////////////////////////////////////////////////////////////////
template <typename R, typename Arg>
class long_runnable
  : public async_notifiable<R, Arg>
  , public stoppable
  , public progressable
{};

template <typename R>
class long_runnable<R, void> 
  : public async_notifiable<R, void>
  , public stoppable
  , public progressable
{};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
