CC      := gcc

LD_SHARED       := @LD_SHARED@
INCLUDES := @INCLUDES@

DESTDIR := 
prefix := /usr/local/
exec_prefix := ${prefix}
ecae_user := @user@
bindir := ${exec_prefix}/bin
libexecdir := ${exec_prefix}/libexec
sysconfdir := ${prefix}/etc
libdir := ${exec_prefix}/lib
includedir := ${prefix}/include
mandir := ${datarootdir}/man
datarootdir = ${prefix}/share
datadir = ${datarootdir}
INSTALL := /usr/bin/install -c
ERTS_VSN := 6.3.1
WITH_ZEROMQ := @with_zeromq@

CFLAGS  := -g -O2 -DPACKAGE_NAME=\"bnow\" -DPACKAGE_TARNAME=\"bnow\" -DPACKAGE_VERSION=\"1.0\" -DPACKAGE_STRING=\"bnow\ 1.0\" -DPACKAGE_BUGREPORT=\"ecae@shopex.cn\" -DPACKAGE_URL=\"\" -DCPU_VENDOR_OS=\"x86_64-apple-darwin14.3.0\" -I"${includedir}"
LDFLAGS :=  -L"${libdir}"
