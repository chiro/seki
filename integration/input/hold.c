/* SPDX-License-Identifier: MIT
 * Stop only this test process's keymap-loader child before its private file read.
 */
#define _GNU_SOURCE
#include <fcntl.h>
#include <stdarg.h>
#include <string.h>
#include <signal.h>
#include <sys/syscall.h>
#include <unistd.h>
int open(const char *path, int flags, ...) {
 mode_t mode=0;
 if (flags & O_CREAT) {va_list args;va_start(args,flags);mode=va_arg(args,int);va_end(args);}
 if (strstr(path,"two-layouts.xkb")) syscall(SYS_kill,getpid(),SIGSTOP);
 return syscall(SYS_openat,AT_FDCWD,path,flags,mode);
}
