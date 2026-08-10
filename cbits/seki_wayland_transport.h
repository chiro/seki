#ifndef SEKI_WAYLAND_TRANSPORT_H
#define SEKI_WAYLAND_TRANSPORT_H

#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>

enum seki_recv_status {
    SEKI_RECV_OK = 0,
    SEKI_RECV_SYSCALL_ERROR = 1,
    SEKI_RECV_CONTROL_TRUNCATED = 2,
    SEKI_RECV_MALFORMED_CONTROL = 3,
    SEKI_RECV_UNSUPPORTED_CONTROL = 4,
    SEKI_RECV_TOO_MANY_FDS = 5,
    SEKI_RECV_CLOEXEC_FAILED = 6,
    SEKI_RECV_DATA_TRUNCATED = 7
};

ssize_t seki_send_message(
    int socket_fd,
    const uint8_t *bytes,
    size_t byte_count,
    const int *fds,
    size_t fd_count,
    int *error_number);

int seki_receive_message(
    int socket_fd,
    uint8_t *bytes,
    size_t byte_capacity,
    int *fds,
    size_t fd_capacity,
    ssize_t *byte_count,
    size_t *fd_count,
    int *error_number);

#endif
