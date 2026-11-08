/* SPDX-License-Identifier: MIT
 * Hold the thread that opens the test keymap until the release file exists.
 */
#define _GNU_SOURCE
#include <fcntl.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <time.h>
#include <unistd.h>
int open(const char *path, int flags, ...) {
 mode_t mode=0;
 if (flags & O_CREAT) {va_list args;va_start(args,flags);mode=va_arg(args,int);va_end(args);}
 const char *held=getenv("SEKI_KEYMAP_HELD"), *release=getenv("SEKI_KEYMAP_RELEASE");
 if (held && release && strstr(path,"two-layouts.xkb")) {
  int marker=syscall(SYS_openat,AT_FDCWD,held,O_WRONLY|O_CREAT|O_APPEND|O_CLOEXEC,0600);
  if (marker>=0) {syscall(SYS_write,marker,"held\n",5);syscall(SYS_close,marker);}
  struct timespec pause={0,20000000};
  while (syscall(SYS_faccessat,AT_FDCWD,release,F_OK,0)) nanosleep(&pause,0);
 }
 return syscall(SYS_openat,AT_FDCWD,path,flags,mode);
}
