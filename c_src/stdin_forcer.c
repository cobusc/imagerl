/**
 *  Based on https://github.com/mattsta/erlang-stdinout-pool/blob/master/c_src/stdin_forcer.c
 */
#include <sys/wait.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>

#define PARENT_READ     readpipe[0]
#define CHILD_WRITE     readpipe[1]
#define CHILD_READ      writepipe[0]
#define PARENT_WRITE    writepipe[1]

#define DUP2CLOSE(oldfd, newfd) (dup2(oldfd, newfd) == 0 && close(oldfd) == 0)

int main(int argc, char *argv[]) {
  int readpipe[2], writepipe[2];
  int unused __attribute__((unused));
  pid_t cpid;

  assert(1 < argc && argc < 64);

  if (pipe(readpipe) == -1 || pipe(writepipe) == -1) {
    perror("pipe");
    exit(EXIT_FAILURE);
  }

  cpid = fork();
  if (cpid == -1) { perror("fork"); exit(EXIT_FAILURE); }

  if (cpid == 0) {
    /* Forked Child with STDIN forwarding */
    char *cmd = argv[1];
    char *exec_args[64] = {0};
    int i;
  
    for (i = 0; i < argc - 1; i++) {
      /* args to stdin_forcer are the program and optional args to exec.
         Here we copy pointers pointing to strings of cmd/args.
         exec_args is indexed one lower than argv. */
      exec_args[i] = argv[i+1];
    }
    
    close(PARENT_READ);  /* We aren't the parent. Decrement fd refcounts. */
    close(PARENT_WRITE);

    /* CHILD_READ  = STDIN  to the exec'd process.
       CHILD_WRITE = STDOUT to the exec'd process. */
    if (!DUP2CLOSE(CHILD_READ,   STDIN_FILENO) &&
         DUP2CLOSE(CHILD_WRITE, STDOUT_FILENO)) {
      perror("dup2 or close");
      _exit(EXIT_FAILURE);
    }

    /* At this point, the execv'd program's STDIN and STDOUT are the pipe */
    if (execv(cmd, exec_args) == -1) {
      perror("execve");
    }
    _exit(EXIT_FAILURE);  /* Silence a warning */
  }
  else {
    /* Original Parent Process */
    char buf[4*1024*1024];
    int32_t l = 0;
    int32_t data_length = 0;
    int32_t num_read = 0;

    close(CHILD_READ);  /* We aren't the child.  Close its read/write. */
    close(CHILD_WRITE);

    assert(4 == read(STDIN_FILENO, &l, 4));
    data_length = ntohl(l);
    /* We should catch the child's exit signal if it dies before we send STDIN*/
    while (num_read < data_length) {
        ssize_t n = read(STDIN_FILENO, &buf, 4096);
        num_read += n;
        unused = write(PARENT_WRITE, &buf, n);
    }
    close(PARENT_WRITE); /* closing PARENT_WRITE sends EOF to CHILD_READ */
    wait(NULL);          /* Wait for child to exit */
    while (1) {
      ssize_t n = read(PARENT_READ, &buf, sizeof(buf));
      if (0 == n)
          break;
      unused = write(STDOUT_FILENO, &buf, n);  /* Vomit forth our output on STDOUT */
    }
    close(PARENT_READ);      /* done reading from writepipe */
    exit(EXIT_SUCCESS);      /* This was a triumph */
  }
}
