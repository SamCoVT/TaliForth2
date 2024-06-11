// PROGRAMMERS : Patrick Surry and Sam Colwell
// FILE        : io.c
// DATE        : 2024-05
// DESCRIPTION : This abstracts the I/O and allows supporting multiple
// environments (Linux, OSX, WSL, Native Windows) that all have gcc.
#ifdef WINDOWS_NATIVE
#include <stdio.h>
#include <conio.h> // Windows specific

void set_terminal_nb() {} // No-op
//int _kbhit(); // _kbhit already available in conio.h
int _getc() { return getch(); } // getch() from conio.h has no echo.
void _putc(char ch) { putchar(ch); fflush(stdout); return; }
#else
// These should work on Linux, OSX, and WSL.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <termios.h>
#include <unistd.h>

struct termios orig_termios;

void reset_terminal() { tcsetattr(0, TCSANOW, &orig_termios); }

void set_terminal_nb() {
  struct termios new_termios;

  /* take two copies - one for now, one for later */
  tcgetattr(0, &orig_termios);
  memcpy(&new_termios, &orig_termios, sizeof(new_termios));

  /* register cleanup handler, and set the new terminal mode */
  atexit(reset_terminal);

  /* see https://man7.org/linux/man-pages/man3/tcsetattr.3.html */
  /* we want something close to cfmakeraw but we'll keep ONLCR and SIGINT etc */
  new_termios.c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
  new_termios.c_oflag |= OPOST | ONLCR;
  new_termios.c_lflag &= ~(ECHO | ECHONL | ICANON | IEXTEN);
  new_termios.c_cflag &= ~(CSIZE | PARENB);
  new_termios.c_cflag |= CS8;

  tcsetattr(0, TCSANOW, &new_termios);

  setbuf(stdout, NULL); /* unbuffered output */
}

/*
compatibility with windows _kbhit, return non-zero if key ready
see https://stackoverflow.com/questions/448944/c-non-blocking-keyboard-input
*/
int _kbhit() {
  struct timeval tv = {0L, 0L};
  fd_set fds;
  FD_ZERO(&fds);
  FD_SET(0, &fds);
  return select(1, &fds, NULL, NULL, &tv) > 0;
}

/* non-blocking version of getch() */
int _getc() {
  int r;
  unsigned char c;
  r = read(0, &c, sizeof(c));
  return r < 0 ? r : c;
}

void _putc(char ch) { putchar((int)ch); }
#endif

#include <signal.h>
#include <stdint.h>
#include "magicio.h"
#include "c65.h"

/*
blkio supports the following action values.  write the action value
after setting other parameters to initiate the action.  All actions
return 0x0 on success and nonzero (typically 0xff) otherwise.
A portable check for blkio is to write 0x1 to status, then write 0x0 to action
and check if status is now 0.
    0 - status: detect if blkio available, 0x0 if enabled, 0xff otherwise
    1 - read: read the 1024 byte block @ blknum to bufptr
    2 - write: write the 1024 byte block @ blknum from bufptr
*/
typedef struct BLKIO {
  uint8_t action;  // I: request an action (write after setting other params)
  uint8_t status;  // O: action status
  uint16_t blknum; // I: block to read or write
  uint16_t bufptr; // I/O: low-endian pointer to 1024 byte buffer to read/write
} BLKIO;

BLKIO *blkiop;
FILE *fblk = NULL;
int io_addr = 0xf000;
long mark = 0;    // used for timer

#define io_putc   (io_addr + 1)
#define io_kbhit  (io_addr + 3)
#define io_getc   (io_addr + 4)
#define io_timer  (io_addr + 6)
#define io_blkio  (io_addr + 16)


void sigint_handler() {
  // catch ctrl-c and break back to monitor
  /*TODO in input loop still have to hit a key after ctrl-c */
  break_flag |= MONITOR_SIGINT;
}

void io_init(int debug) {
  /* initialize blkio struct once io_addr is set */
  blkiop = (BLKIO *)(memory + io_blkio);
  set_terminal_nb();
  if (debug) signal(SIGINT, sigint_handler);
}

void io_exit() {
    io_blkfile(NULL);
}


FILE* io_blkfile(const char *fname) {
  if (fblk) fclose(fblk);
  if (fname) fblk = fopen(fname, "r+b");
  return fblk;
}


void io_magic_read(uint16_t addr) {
  int ch;
  long delta;

  if (addr == io_kbhit) {
    memory[addr] = _kbhit() ? 0xff : 0;
  } else if (addr == io_getc) {
    ch = break_flag ? 0x03 : (_kbhit() ? _getc() : 0);
    if (ch == EOF) break_flag |= MONITOR_EXIT;
    memory[addr] = (uint8_t)ch;
  } else if (addr == io_timer /* start timer */) {
    mark = ticks;
  } else if (addr == io_timer + 1 /* stop timer */) {
    delta = ticks - mark;
    memory[io_timer + 2] = (uint8_t)((delta >> 16) & 0xff);
    memory[io_timer + 3] = (uint8_t)((delta >> 24) & 0xff);
    memory[io_timer + 4] = (uint8_t)((delta >> 0) & 0xff);
    memory[io_timer + 5] = (uint8_t)((delta >> 8) & 0xff);
  }
}


void io_magic_write(uint16_t addr, uint8_t val) {
  if (addr == io_putc) {
    _putc(val);
  } else if (addr == io_blkio) {
    blkiop->status = 0xff;
    if (fblk) {
      if (val < 3) {
        blkiop->status = 0;
        if (val == 1 || val == 2) {
          fseek(fblk, 1024 * blkiop->blknum, SEEK_SET);
          if (val == 1) {
            fread(memory + blkiop->bufptr, 1024, 1, fblk);
          } else {
            fwrite(memory + blkiop->bufptr, 1024, 1, fblk);
            fflush(fblk);
          }
        }
      }
    }
  }
}

