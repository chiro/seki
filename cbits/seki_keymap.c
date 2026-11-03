#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>

#define KEYMAP_LIMIT (4 * 1024 * 1024)

/* The post-fork child uses async-signal-safe calls only, with no Haskell or allocator state. */
static void load_keymap(int socket, const char *path) {
    unsigned char buffer[16384];
    struct stat st;
    int source = open(path, O_RDONLY | O_CLOEXEC | O_NONBLOCK | O_NOCTTY);
    if (source < 0 || fstat(source, &st) || !S_ISREG(st.st_mode) || st.st_size <= 0 || st.st_size > KEYMAP_LIMIT) _exit(1);
    int snapshot = (int)syscall(SYS_memfd_create, "seki-keymap", MFD_CLOEXEC | MFD_ALLOW_SEALING);
    if (snapshot < 0) _exit(1);
    size_t total = 0;
    unsigned pending = 0;
    uint32_t codepoint = 0, minimum = 0;
    int terminated = 0;
    for (;;) {
        ssize_t count = read(source, buffer, sizeof buffer);
        if (count < 0 && errno == EINTR) continue;
        if (count < 0) _exit(1);
        if (!count) break;
        if (total + (size_t)count > KEYMAP_LIMIT) _exit(1);
        for (ssize_t i = 0; i < count; i++) {
            unsigned c = buffer[i];
            if (terminated) _exit(1);
            if (!pending) {
                if (!c) terminated = 1;
                else if (c < 0x80) {}
                else if (c >= 0xc2 && c <= 0xdf) { pending = 1; codepoint = c & 31; minimum = 0x80; }
                else if (c >= 0xe0 && c <= 0xef) { pending = 2; codepoint = c & 15; minimum = 0x800; }
                else if (c >= 0xf0 && c <= 0xf4) { pending = 3; codepoint = c & 7; minimum = 0x10000; }
                else _exit(1);
            } else {
                if ((c & 0xc0) != 0x80) _exit(1);
                codepoint = (codepoint << 6) | (c & 63);
                if (!--pending && (codepoint < minimum || codepoint > 0x10ffff || (codepoint >= 0xd800 && codepoint <= 0xdfff))) _exit(1);
            }
        }
        ssize_t written = 0;
        while (written < count) {
            ssize_t n = write(snapshot, buffer + written, (size_t)(count - written));
            if (n < 0 && errno == EINTR) continue;
            if (n <= 0) _exit(1);
            written += n;
        }
        total += (size_t)count;
    }
    if (!total || pending) _exit(1);
    if (!terminated && write(snapshot, "", 1) != 1) _exit(1);
    if (fcntl(snapshot, F_ADD_SEALS, F_SEAL_WRITE | F_SEAL_SHRINK | F_SEAL_GROW | F_SEAL_SEAL)) _exit(1);
    if (lseek(snapshot, 0, SEEK_SET) < 0) _exit(1);
    char payload = 1;
    struct iovec io = { &payload, 1 };
    union { struct cmsghdr alignment; char data[CMSG_SPACE(sizeof(int))]; } control;
    memset(&control, 0, sizeof control);
    struct msghdr message = {0};
    message.msg_iov = &io; message.msg_iovlen = 1;
    message.msg_control = control.data; message.msg_controllen = sizeof control.data;
    struct cmsghdr *c = CMSG_FIRSTHDR(&message);
    c->cmsg_level = SOL_SOCKET; c->cmsg_type = SCM_RIGHTS; c->cmsg_len = CMSG_LEN(sizeof(int));
    memcpy(CMSG_DATA(c), &snapshot, sizeof snapshot);
    if (sendmsg(socket, &message, MSG_NOSIGNAL) != 1) _exit(1);
    _exit(0);
}

int seki_keymap_start(const char *path, int *child) {
    int sockets[2];
    if (socketpair(AF_UNIX, SOCK_SEQPACKET | SOCK_CLOEXEC | SOCK_NONBLOCK, 0, sockets)) return -1;
    pid_t pid = fork();
    if (pid < 0) { close(sockets[0]); close(sockets[1]); return -1; }
    if (!pid) {
        if (sockets[1] != 3 && dup3(sockets[1], 3, O_CLOEXEC) < 0) _exit(1);
        /* close_range is required: never load with inherited compositor sockets. */
        if (syscall(SYS_close_range, 4u, ~0u, 0u)) _exit(1);
        close(0); close(1); close(2);
        load_keymap(3, path);
    }
    close(sockets[1]); *child = (int)pid;
    return sockets[0];
}

/* -2 pending, -1 failed/EOF, otherwise one CLOEXEC immutable descriptor. */
int seki_keymap_receive(int socket) {
    char payload;
    struct iovec io = { &payload, 1 };
    union { struct cmsghdr alignment; char data[CMSG_SPACE(sizeof(int))]; } control;
    struct msghdr message = {0};
    message.msg_iov = &io; message.msg_iovlen = 1;
    message.msg_control = control.data; message.msg_controllen = sizeof control.data;
    ssize_t n = recvmsg(socket, &message, MSG_DONTWAIT | MSG_CMSG_CLOEXEC);
    if (n < 0 && (errno == EAGAIN || errno == EINTR)) return -2;
    if (n != 1) return -1;
    struct cmsghdr *c = CMSG_FIRSTHDR(&message);
    int fd;
    if (!c || c->cmsg_level != SOL_SOCKET || c->cmsg_type != SCM_RIGHTS || c->cmsg_len != CMSG_LEN(sizeof fd)) return -1;
    memcpy(&fd, CMSG_DATA(c), sizeof fd);
    return fd;
}

int seki_keymap_reap(int child, int cancel) {
    if (cancel) kill((pid_t)child, SIGKILL);
    int status;
    pid_t result = waitpid((pid_t)child, &status, WNOHANG);
    return result == child || (result < 0 && errno == ECHILD);
}
