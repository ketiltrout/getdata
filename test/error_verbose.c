/* Copyright (C) 2012-2014, 2017 D. V. Wiebe
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

#define DIRFILENAME "a non-existant dirfile"
int main(void)
{
#if !defined HAVE_DUP2 || !defined HAVE_KILL || !defined HAVE_PIPE || \
  !defined HAVE_WORKING_FORK || defined __CYGWIN__
  return 77; /* skip */
#else
  int r = 0, status, pipefd[2];
  pid_t pid;

  rmdirfile();

  /* make a pipe */
  if (pipe(pipefd)) {
    perror("pipe");
    exit(1);
  }

  /* fork a child to read our standard error */
  if ((pid = fork()) == 0) {
    char string[10000];
    FILE *stream;

    /* CHILD: close the write-side of the pipe */
    close(pipefd[1]);

    /* Associate the read-side of the pipe with a stream */
    stream = fdopen(pipefd[0], "r");

    fgets(string, 10000, stream);
    CHECKBOS(string, "libgetdata:");
    CHECKSS(string, DIRFILENAME);

    /* Child exits */
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
    DIRFILE *D;
    int error;

    close(pipefd[1]);

    D = gd_open(DIRFILENAME, GD_RDONLY | GD_VERBOSE);
    error = gd_error(D);
    gd_discard(D);

    fputs("\n", stderr);
    fflush(stderr);

    /* restore stderr */
    freopen("/dev/stderr", "w", stderr);

    CHECKI(error, GD_E_IO);
  }

  waitpid(pid, &status, 0);
  if (status) {
    printf("status=%i", status);
    r = 1;
  }

  return r;
#endif
}
