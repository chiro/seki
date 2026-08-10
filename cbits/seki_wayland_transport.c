#define _GNU_SOURCE

#include "seki_wayland_transport.h"

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

static void close_fds(int *fds, size_t fd_count)
{
    int saved_errno = errno;

    for (size_t index = 0; index < fd_count; ++index) {
        (void)close(fds[index]);
    }

    errno = saved_errno;
}

ssize_t seki_send_message(
    int socket_fd,
    const uint8_t *bytes,
    size_t byte_count,
    const int *fds,
    size_t fd_count,
    int *error_number)
{
    struct iovec payload = {
        .iov_base = (void *)bytes,
        .iov_len = byte_count,
    };
    struct msghdr message;
    void *control = NULL;

    memset(&message, 0, sizeof(message));
    message.msg_iov = &payload;
    message.msg_iovlen = 1;

    if (fd_count > 0) {
        if (byte_count == 0 ||
            fd_count > (SIZE_MAX - CMSG_SPACE(0)) / sizeof(int)) {
            *error_number = EINVAL;
            return -1;
        }

        size_t control_size = CMSG_SPACE(fd_count * sizeof(int));
        control = calloc(1, control_size);
        if (control == NULL) {
            *error_number = ENOMEM;
            return -1;
        }

        message.msg_control = control;
        message.msg_controllen = control_size;

        struct cmsghdr *header = CMSG_FIRSTHDR(&message);
        header->cmsg_level = SOL_SOCKET;
        header->cmsg_type = SCM_RIGHTS;
        header->cmsg_len = CMSG_LEN(fd_count * sizeof(int));
        memcpy(CMSG_DATA(header), fds, fd_count * sizeof(int));
    }

    ssize_t sent = sendmsg(socket_fd, &message, MSG_NOSIGNAL);
    if (sent < 0) {
        *error_number = errno;
    }

    free(control);
    return sent;
}

int seki_receive_message(
    int socket_fd,
    uint8_t *bytes,
    size_t byte_capacity,
    int *fds,
    size_t fd_capacity,
    ssize_t *byte_count,
    size_t *fd_count,
    int *error_number)
{
    struct iovec payload = {
        .iov_base = bytes,
        .iov_len = byte_capacity,
    };
    struct msghdr message;
    size_t control_size;
    void *control;

    *byte_count = 0;
    *fd_count = 0;
    *error_number = 0;

    if (byte_capacity == 0 || fd_capacity == 0 ||
        fd_capacity > (SIZE_MAX - CMSG_SPACE(0)) / sizeof(int)) {
        *error_number = EINVAL;
        return SEKI_RECV_SYSCALL_ERROR;
    }

    control_size = CMSG_SPACE(fd_capacity * sizeof(int));
    control = calloc(1, control_size);
    if (control == NULL) {
        *error_number = ENOMEM;
        return SEKI_RECV_SYSCALL_ERROR;
    }

    memset(&message, 0, sizeof(message));
    message.msg_iov = &payload;
    message.msg_iovlen = 1;
    message.msg_control = control;
    message.msg_controllen = control_size;

    int receive_flags = 0;
#ifdef MSG_CMSG_CLOEXEC
    receive_flags |= MSG_CMSG_CLOEXEC;
#endif

    ssize_t received = recvmsg(socket_fd, &message, receive_flags);
    if (received < 0) {
        *error_number = errno;
        free(control);
        return SEKI_RECV_SYSCALL_ERROR;
    }

    *byte_count = received;
    int status = SEKI_RECV_OK;
    unsigned char *control_end = (unsigned char *)control + message.msg_controllen;

    for (struct cmsghdr *header = CMSG_FIRSTHDR(&message);
         header != NULL;
         header = CMSG_NXTHDR(&message, header)) {
        unsigned char *header_end = (unsigned char *)header + header->cmsg_len;
        if (header->cmsg_len < CMSG_LEN(0) || header_end > control_end) {
            status = SEKI_RECV_MALFORMED_CONTROL;
            break;
        }

        if (header->cmsg_level != SOL_SOCKET || header->cmsg_type != SCM_RIGHTS) {
            if (status == SEKI_RECV_OK) {
                status = SEKI_RECV_UNSUPPORTED_CONTROL;
            }
            continue;
        }

        size_t data_size = header->cmsg_len - CMSG_LEN(0);
        size_t message_fd_count = data_size / sizeof(int);
        if (data_size % sizeof(int) != 0 && status == SEKI_RECV_OK) {
            status = SEKI_RECV_MALFORMED_CONTROL;
        }

        int *message_fds = (int *)CMSG_DATA(header);
        for (size_t index = 0; index < message_fd_count; ++index) {
            if (*fd_count < fd_capacity) {
                fds[*fd_count] = message_fds[index];
                ++*fd_count;
            } else {
                (void)close(message_fds[index]);
                if (status == SEKI_RECV_OK) {
                    status = SEKI_RECV_TOO_MANY_FDS;
                }
            }
        }
    }

    if ((message.msg_flags & MSG_CTRUNC) != 0) {
        status = SEKI_RECV_CONTROL_TRUNCATED;
    } else if ((message.msg_flags & MSG_TRUNC) != 0 && status == SEKI_RECV_OK) {
        status = SEKI_RECV_DATA_TRUNCATED;
    }

    if (status == SEKI_RECV_OK) {
        for (size_t index = 0; index < *fd_count; ++index) {
            int flags = fcntl(fds[index], F_GETFD);
            if (flags < 0 || fcntl(fds[index], F_SETFD, flags | FD_CLOEXEC) < 0) {
                *error_number = errno;
                status = SEKI_RECV_CLOEXEC_FAILED;
                break;
            }
        }
    }

    if (status != SEKI_RECV_OK) {
        close_fds(fds, *fd_count);
        *fd_count = 0;
    }

    free(control);
    return status;
}
