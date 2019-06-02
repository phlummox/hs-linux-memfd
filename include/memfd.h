/* Adapted from A. Darwish,
 * https://raw.githubusercontent.com/a-darwish/memfd-examples/master/memfd.h
 * 
 * License: public domain
 */

#ifndef _MEMFD_H
#define _MEMFD_H

/* syscall() won't be defined under glibc unless _GNU_SOURCE
 * defined
 */
#ifdef __GLIBC__
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#endif

/* On either glibc or musl systems, syscall should be defined
 * in _one_ of these headers.
 */
#include <unistd.h>
#include <sys/syscall.h>

#ifdef __cplusplus
extern "C" {
#endif


/*
 * No glibc wrappers exist for memfd_create(2), so provide our own.
 *
 * Also define memfd fcntl sealing macros. While they are already
 * defined in the kernel header file <linux/fcntl.h>, that file as
 * a whole conflicts with the original glibc header <fnctl.h>.
 */
int memfd_create(const char *name, unsigned int flags);

#ifndef F_LINUX_SPECIFIC_BASE
#define F_LINUX_SPECIFIC_BASE 1024
#endif

#ifndef MFD_CLOEXEC

/* flags for memfd_create(2) (unsigned int) */
#define MFD_CLOEXEC             0x0001U
#define MFD_ALLOW_SEALING       0x0002U

#endif /* MFD_CLOEXEC */


#ifndef F_ADD_SEALS

/*
 * Set/Get seals
 */
#define F_ADD_SEALS (F_LINUX_SPECIFIC_BASE + 9)
#define F_GET_SEALS (F_LINUX_SPECIFIC_BASE + 10)

/*
 * Types of seals
 */
#define F_SEAL_SEAL     0x0001  /* prevent further seals from being set */
#define F_SEAL_SHRINK   0x0002  /* prevent file from shrinking */
#define F_SEAL_GROW     0x0004  /* prevent file from growing */
#define F_SEAL_WRITE    0x0008  /* prevent writes */

#endif /* F_ADD_SEALS */


#ifdef __cplusplus
}
#endif


#endif /* _MEMFD_H */
