# Process this file with autoconf to produce a configure script.
AC_PREREQ([2.69])
AC_INIT([bk-terak-emu],[2006],[http://sourceforge.net/projects/bk-terak-emu/])
AC_CONFIG_SRCDIR([terakdisk.c])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL

case "$GCC" in
yes) CFLAGS="-Wall $CFLAGS" ;;
esac

# Checks for libraries.
AC_SEARCH_LIBS(SDL_Init, SDL)
AC_SEARCH_LIBS([bindtextdomain], [intl])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h libintl.h locale.h malloc.h sgtty.h stdlib.h string.h sys/ioctl.h sys/param.h sys/socket.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_OFF_T
AC_HEADER_TIME

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_MMAP
AC_FUNC_SELECT_ARGTYPES
AC_CHECK_FUNCS([atexit isascii memset munmap select setlocale strcasecmp strchr strdup strspn])

AC_CONFIG_FILES([Makefile po/Makefile.in])
AC_OUTPUT

GETTEXT_PACKAGE=bk
