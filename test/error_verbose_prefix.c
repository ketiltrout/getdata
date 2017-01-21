/* Copyright (C) 2012-2013, 2017 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * GetData is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * GetData is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "test.h"

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

int main(void)
{
#if !defined HAVE_DUP2 || !defined HAVE_KILL || !defined HAVE_PIPE || \
  !defined HAVE_WORKING_FORK || defined __CYGWIN__
  return 77; /* skip */
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int r = 0, status, pipefd[2];
  pid_t pid;

  rmdirfile();
  mkdir(filedir, 0700);
  MAKEFORMATFILE(format, "#");

  /* make a pipe */
  if (pipe(pipefd)) {
    perror("pipe");
    exit(1);
  }

  /* fork a child to read our standard error */
  if ((pid = fork()) == 0) {
    char string[1024];
    FILE *stream;

    /* CHILD: close the write-side of the pipe */
    close(pipefd[1]);

    /* Associate the read-side of the pipe with a stream */
    stream = fdopen(pipefd[0], "r");

    fgets(string, 1024, stream);
    CHECKS(string, "getdata-test: libgetdata: Field not found: data\n");
    return r;
  }

  /* PARENT: close the read-side of the pipe */
  close(pipefd[0]);

  /* point stderr at the write side of the pipe */
  if (dup2(pipefd[1], STDERR_FILENO) < 0) {
    close(pipefd[1]);
    perror("dup2");
    kill(pid, SIGKILL);
  } else {
    int e1, e2;
    DIRFILE *D;

    close(pipefd[1]);

    D = gd_open(filedir, GD_RDONLY);
    e1 = gd_error(D);
    CHECKI(e1, 0);

    gd_flags(D, GD_VERBOSE, 0);
    gd_verbose_prefix(D, "getdata-test: ");
    gd_validate(D, "data");

    e2 = gd_error(D);
    CHECKI(e2, GD_E_BAD_CODE);

    gd_discard(D);

    fputs("\n", stderr);
    fflush(stderr);

    /* restore stderr */
    freopen("/dev/stderr", "w", stderr);
  }

  unlink(format);
  rmdir(filedir);

  waitpid(pid, &status, 0);
  if (status)
    r = 1;

  return r;
#endif
}
