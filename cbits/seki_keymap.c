#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <unistd.h>

#define KEYMAP_LIMIT (4 * 1024 * 1024)

static int copy_valid(int source, int snapshot) {
    unsigned char buffer[16384];
    size_t total = 0;
    unsigned pending = 0;
    uint32_t codepoint = 0, minimum = 0;
    int terminated = 0;
    for (;;) {
        ssize_t count = read(source, buffer, sizeof buffer);
        if (count < 0 && errno == EINTR) continue;
        if (count < 0) return -1;
        if (!count) break;
        if (total + (size_t)count > KEYMAP_LIMIT) return -1;
        for (ssize_t i = 0; i < count; i++) {
            unsigned c = buffer[i];
            if (terminated) return -1;
            if (!pending) {
                if (!c) terminated = 1;
                else if (c < 0x80) {}
                else if (c >= 0xc2 && c <= 0xdf) { pending = 1; codepoint = c & 31; minimum = 0x80; }
                else if (c >= 0xe0 && c <= 0xef) { pending = 2; codepoint = c & 15; minimum = 0x800; }
                else if (c >= 0xf0 && c <= 0xf4) { pending = 3; codepoint = c & 7; minimum = 0x10000; }
                else return -1;
            } else {
                if ((c & 0xc0) != 0x80) return -1;
                codepoint = (codepoint << 6) | (c & 63);
                if (!--pending && (codepoint < minimum || codepoint > 0x10ffff || (codepoint >= 0xd800 && codepoint <= 0xdfff))) return -1;
            }
        }
        ssize_t written = 0;
        while (written < count) {
            ssize_t n = write(snapshot, buffer + written, (size_t)(count - written));
            if (n < 0 && errno == EINTR) continue;
            if (n <= 0) return -1;
            written += n;
        }
        total += (size_t)count;
    }
    if (!total || pending) return -1;
    if (!terminated && write(snapshot, "", 1) != 1) return -1;
    return 0;
}

/* -1, or a sealed CLOEXEC snapshot of a NUL-terminated UTF-8 regular file of at most 4 MiB. */
int seki_keymap_snapshot(const char *path) {
    int source = open(path, O_RDONLY | O_CLOEXEC | O_NONBLOCK | O_NOCTTY);
    if (source < 0) return -1;
    struct stat st;
    int snapshot = -1;
    if (!fstat(source, &st) && S_ISREG(st.st_mode) && st.st_size > 0 && st.st_size <= KEYMAP_LIMIT)
        snapshot = (int)syscall(SYS_memfd_create, "seki-keymap", MFD_CLOEXEC | MFD_ALLOW_SEALING);
    if (snapshot >= 0 && (copy_valid(source, snapshot) || fcntl(snapshot, F_ADD_SEALS, F_SEAL_WRITE | F_SEAL_SHRINK | F_SEAL_GROW | F_SEAL_SEAL) || lseek(snapshot, 0, SEEK_SET) < 0)) {
        close(snapshot);
        snapshot = -1;
    }
    close(source);
    return snapshot;
}
