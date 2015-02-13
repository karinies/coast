#include <errno.h>
#include <stdio.h>
#include <zmq.h>


#define d(e) printf("(define %s %d)\n", #e, e)

int main()
{
  d(EADDRINUSE);
  d(EADDRNOTAVAIL);
  d(EAGAIN);
  d(EFAULT);
  d(EFSM);
  d(EHOSTUNREACH);
  d(EINTR);
  d(EMFILE);
  d(EMTHREAD);
  d(ENOCOMPATPROTO);
  d(ENODEV);
  d(ENOTSOCK);
  d(ENOTSUP);
  d(EPROTONOSUPPORT);
  d(ETERM);

}


