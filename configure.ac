#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.61])
AC_INIT([apple2sys],[1.2.1])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_SRCDIR([src/dos/system/common/rwts.m4])
AM_INIT_AUTOMAKE([-Wall -Werror])


AC_PATH_PROGS([XA65],[xa65 xa])
if test "X$ac_cv_path_XA65" == X
then
	AC_MSG_ERROR([Cannot find xa (xa65) 6502 cross assembler by Andre Fachat])
fi
AC_ARG_VAR([XA65],[path to xa (xa65) 6502 cross assembler by Andre Fachat])

AC_PATH_PROG([LDO65],[ldo65])
if test "X$ac_cv_path_LDO65" == X
then
	AC_MSG_ERROR([Cannot find ldo65, from xa (xa65) 6502 cross assembler by Andre Fachat])
fi
AC_ARG_VAR([LDO65],[path to ldo65, from xa (xa65) 6502 cross assembler by Andre Fachat])

AC_PATH_PROG([RELOC65],[reloc65])
if test "X$ac_cv_path_RELOC65" == X
then
	AC_MSG_ERROR([Cannot find reloc, from xa (xa65) 6502 cross assembler by Andre Fachat])
fi
AC_ARG_VAR([RELOC65],[path to reloc65, from xa (xa65) 6502 cross assembler by Andre Fachat])

AC_PATH_PROG([M4],[m4])
if test "X$ac_cv_path_M4" == X
then
	AC_MSG_ERROR([Cannot find m4])
fi
AC_ARG_VAR([M4],[path to m4])

AC_PATH_PROG([MD5SUM],[md5sum])
AC_ARG_VAR([MD5SUM],[path to md5sum])


AC_CONFIG_FILES([

Makefile
src/Makefile
src/dos/Makefile
src/dos/system/Makefile
src/dos/system/dos310/Makefile
src/dos/system/dos320/Makefile
src/dos/system/dos321/Makefile
src/dos/system/dos330/Makefile
src/dos/system/dos331/Makefile
src/dos/system/dos332/Makefile
src/dos/controller/Makefile
src/dos/controller/13sector/Makefile
src/dos/controller/16sector/Makefile
src/system/Makefile
src/system/monitor/Makefile
src/system/monitor/apple2/Makefile
src/system/monitor/apple2plus/Makefile
src/system/other/Makefile
src/system/intbasic/Makefile
src/system/applesoft/Makefile

])

AC_OUTPUT
