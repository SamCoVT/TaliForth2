#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <ctype.h>

#define FAKE6502_NOT_STATIC 1
#define FAKE6502_INCLUDE 1
#include "fake65c02.h"

#include "monitor.h"
#include "parse.h"
#include "c65.h"
#include "magicio.h"
#include "linenoise.h"


typedef struct Command {
    const char *name;
    const char *help;
    int repeatable;
    void (*handler)();
} Command;


Command _cmds[];
int org = -1;       /* the current address, reset to PC after each simulation step */

char _prompt[512];

#define TXT_LO "\x1b[34m"
#define TXT_B1 "\x1b[41m"
#define TXT_B2 "\x1b[42m"
#define TXT_B3 "\x1b[44m"
#define TXT_N  "\x1b[0m"

#define FLAG_FMT(f, ch) (status & FLAG_##f ? TXT_B3 : TXT_LO), (status & FLAG_##f ? ch: tolower(ch))

const char* _monitor_names[] = {
    "read", "write", "data", "execute", "x", 0
};

const int _monitor_vals[] = {
    MONITOR_READ, MONITOR_WRITE, MONITOR_DATA, MONITOR_PC, MONITOR_PC
};

char* prompt() {
    sprintf(_prompt,
        TXT_LO "PC" TXT_N " %.4x  "
        "%s%c" TXT_N "%s%c" TXT_N TXT_LO "-" TXT_N "%s%c" TXT_N
        "%s%c" TXT_N "%s%c" TXT_N "%s%c" TXT_N "%s%c" TXT_N "  "
        TXT_LO "A " TXT_N "%.2x " TXT_LO "X " TXT_N "%.2x " TXT_LO "Y " TXT_N "%.2x " TXT_LO "SP " TXT_N "%.2x "
        TXT_LO "> " TXT_N,
        pc,
        FLAG_FMT(SIGN, 'N'), FLAG_FMT(OVERFLOW, 'V'), /* - */ FLAG_FMT(BREAK, 'B'),
        FLAG_FMT(DECIMAL, 'D'), FLAG_FMT(INTERRUPT, 'I'), FLAG_FMT(ZERO, 'Z'), FLAG_FMT(CARRY, 'C'),
        a, x, y, sp
    );
    return _prompt;
}

int heat_scale = 0;

int bitlen(uint64_t v) {
    int bits;
    for (bits=0; v != 0; bits++, v >>=1) /**/ ;
    return bits;
}

static char _heatbuf[16];

char* heatstr(uint64_t d) {
    const char heat_ascii[] = " .:+=*#@";
    const int heat_colors[] = {100, 104, 106, 102, 103, 101, 105, 107};

    int c;
    if (heat_scale == 0) sprintf(_heatbuf, " ");
    else {
        if (d == 0) {
            c = 0;
        } else {
            c = 1 + (bitlen(d) - 1) / heat_scale;
            if (c > 7) c = 7;
        }
        sprintf(_heatbuf, "\x1b[%dm%c\x1b[0m", heat_colors[c], heat_ascii[c]);
    }
    return _heatbuf;
}

void clear_heatmap(uint16_t start, uint16_t end, int mode) {
    heat_scale = 0;
    int addr, endl;

    endl = end > start ? end : 0x10000;
    for(addr=start; addr<endl; addr++) {
        if (mode & MONITOR_READ) heat_rs[addr] = 0;
        if (mode & MONITOR_WRITE) heat_ws[addr] = 0;
        if (mode & MONITOR_PC) heat_xs[addr] = 0;
    }
}

int save_heatmap(const char* fname, uint16_t start, uint16_t end, int mode) {
    FILE *fout;
    int addr, endl;
    uint64_t data[0x10000], d;

    endl = end > start ? end : 0x10000;
    for (addr=start; addr<endl; addr++) {
        d = 0;
        if (mode & MONITOR_READ) d += heat_rs[addr];
        if (mode & MONITOR_WRITE) d += heat_ws[addr];
        if (mode & MONITOR_PC) d += heat_xs[addr];
        data[addr] = d;
    }

    fout = fopen(fname, "wb");
    if (!fout) {
        fprintf(stderr, "Error writing %s\n", fname);
        return -1;
    }
    fwrite(data+start, sizeof(uint64_t), endl-start, fout);
    fclose(fout);

    return 0;
}

void heatmap(uint16_t start, uint16_t end, int mode) {
    uint64_t data[1024], dmax, d;
    int i, zoom, addr, endl;

    endl = end > start ? end : 0x10000;

    /* figure out a scaling where we can round down start and encompass end.
       consider only even zoom levels since it makes labeling easier */
    for(zoom=0; /**/; zoom+=2)
        if (((start >> zoom) << zoom) + (1024 << zoom) >= endl) break;
    /* round down the start to a multiple of zoom */
    start = (start >> zoom) << zoom;

    /* aggregate the data we're after */
    /* we aggregate by max to preserve the same scale as we zoom
        in some cases sum might be more useful but for max is better for finding hotspots */
    for(i=0; i<1024; i++) data[i] = 0;
    for(addr=start; addr < start + (1024 << zoom) && addr < 0x10000; addr++) {
        i = (addr-start) >> zoom;
        if (mode & MONITOR_READ && data[i] < heat_rs[addr]) data[i] = heat_rs[addr];
        if (mode & MONITOR_WRITE && data[i] < heat_ws[addr]) data[i] = heat_ws[addr];
        if (mode & MONITOR_PC && data[i] < heat_xs[addr]) data[i] = heat_xs[addr];
    }
    /*
    set up a log color scale by picking a number of bits for each bucket
    we'll assign our eight colors like so:

    bit length is 0 (ie. the value is 0) => color 0
    bit length 1:k (values 1..2^k-1) => color 1
    bit length ...:2k => color 2
    ...
    bit length ...:7k => color 7
    */
    /* first get the largest value for scaling the color ramp */
    for(dmax=i=0; i<1024; i++)
        if (data[i] > dmax) dmax = data[i];
    /* now calculate k by dividing bit length of largest value by 7 and rounding: */
    heat_scale = (int)(bitlen(dmax)/7.0 + 0.5);
    if (!heat_scale) heat_scale = 1;        /* want at least one bit per bucket or labels looks silly */

    /* add a header line.  with even zoom levels each row of 64 blocks shares the same labeling */
    if (zoom&2)
        printf("\n      0   1   2   3    4   5   6   7    8   9   a   b    c   d   e   f   \n");
    else
        printf("\n      0123456789abcdef 0123456789abcdef 0123456789abcdef 0123456789abcdef\n");
    /* draw the data */
    for(i=0; i<1024; i++) {
        addr = start + (i << zoom);
        if (addr >= endl) break;
        if (!(i & 0x3f)) printf("%.4x ", addr);
        if (!(i & 0xf)) printf(" ");
        printf("%s", heatstr(data[i]));
        if ((i & 0x3f) == 0x3f) puts("");
    }
    /* add a legend */
    printf("\n%c%c%c count 0",
        mode & MONITOR_READ ? 'r':'-',
        mode & MONITOR_WRITE ? 'w':'-',
        mode & MONITOR_PC ? 'x':'-'
    );
    for(i=0; i<8; i++) {
        d = 1 << (i * heat_scale);
        printf(" %s $%" PRIx64, heatstr(d-1), d);
    }
    printf(" ($%x byte%s/char)\n\n", 1 << zoom, zoom ? "s": "");
}

char* _fmt_addr(char *buf, uint16_t addr, int len) {
    const Symbol *sym = get_next_symbol_by_value(NULL, addr);
    if (sym) {
        sprintf(buf, "%.16s", sym->name);
    } else {
        sprintf(buf, "$%.*x", len, addr);
    }
    return buf;
}

uint16_t disasm(uint16_t start, uint16_t end) {
    uint8_t op, k, n;
    int8_t offset;
    char line[80], buf[32], *p;
    const char *fmt;
    const Symbol *sym;
    const int n_fmt = strlen(TXT_LO);
    int addr, endl = end < start ? 0x10000 : end;

    /*
    0          1          2         3
    012345678 9012345678 90123456789012345
       abcd  <aa bb cc  >bbr3 $zp,$rr
    *B abce  <aa bb     >bne  $rr
       abdf  <aa bb cc  >lda  ($abcd,x)
    */
    for (addr = start; addr < endl; /**/){
        /* show any labels for this address */
        sym = NULL;
        for(;;) {
            sym = get_next_symbol_by_value(sym, addr);
            if (!sym) break;
            printf("%s:\n", sym->name);
        }

        /* clear line and show addresss with break and PC info */
        memset(line, ' ', sizeof(line));
        p = line;
        p += sprintf(
            line, "%c%c %.4x  " TXT_LO,
            pc == addr ? '*' : ' ',
            breakpoints[addr] & MONITOR_PC ? 'B': ' ',
            addr
        );

        /* show bytes associated with this opcode */
        op = memory[addr];
        n = oplen(op);
        for (k=0; k<n; k++)
            p += sprintf(p, "%.2x ", memory[addr+k]);
        *p = ' ';

        /* show the name of the opcode, with heatmap */
        p = line + 19 + n_fmt;
        p += sprintf(p, TXT_N "%s %.4s ", heatstr(heat_rs[addr]), opname(op));
        addr++;

        /* show the addressing mode detail */
        fmt = opfmt(op);
        if (n == 1) {
            p += sprintf(p, "%s", fmt);
        } else if (strchr(fmt, '#')) { /* immediate? */
            p += sprintf(p, fmt, memory[addr++]);
        } else if (strchr(fmt, ';')) { /* relative? */
            /* 1 or 2 bytes with relative address */
            offset = memory[addr + n-2];
            if (n==3) {
                p += sprintf(p, fmt, _fmt_addr(buf+16, memory[addr], 2), _fmt_addr(buf, addr+2+offset, 4), offset);
                addr+=2;
            } else {
                p += sprintf(p, fmt, _fmt_addr(buf, addr+1+offset, 4), offset);
                addr++;
            }
        } else if (n==2) {
            p += sprintf(p, fmt, _fmt_addr(buf, memory[addr], 2));
            addr++;
        } else {
            p += sprintf(p, fmt, _fmt_addr(buf, *(uint16_t*)(memory+addr), 4));
            addr += 2;
        }
        puts(line);
    }
    return addr;
}

void dump(uint16_t start, uint16_t end) {
    /*
    0         1         2         3         4         5         6         7
    01234567890123456789012345678901234567890123456789012345678901234567890123
    0000  d8 a9 78 85 12 a9 be 85  13 a2 1d bd 49 a3 95 00  |..x.........I...|
    */
    int addr = start & 0xfff0, endl = end < start ? 0x10000 : end;
    char line[256], chrs[16], *p;
    uint8_t c, v;

    puts("       0  1  2  3  4  5  6  7   8  9  a  b  c  d  e  f   0123456789abcdef");
    while(addr < endl) {
        memset(chrs, ' ', 16);
        p = line;
        /* show the address */
        p += sprintf(p, "%.4x  ", addr);
        /* show blanks before truncated address */
        while (addr < start) p += sprintf(p, "   %s", (++addr & 0xf) == 8 ? " ":"");

        do {
            if (addr < endl) {
                c = memory[addr];
                chrs[addr & 0xf] = (c >= 32 && c < 128 ? c : '.');
                c = breakpoints[addr] & MONITOR_DATA;
                v = memory[addr++];
                p += sprintf(p, "%s%.2x%s%s",
                    c & MONITOR_READ ?
                        (c & MONITOR_WRITE ? TXT_B1: TXT_B2)
                        : (c & MONITOR_WRITE ? TXT_B3: ""),
                    v,
                    c ? TXT_N: "",
                    (addr & 0xf) == 8 ? "  " : " "
                );
            } else {
                addr++;
                p += sprintf(p, "   %s", (addr & 0xf) == 8 ? " ":"");
            }
        } while((addr & 0xf) && addr < endl);
        /* show blanks padding end of range */
        while (addr & 0xf) p += sprintf(p, "   %s", (++addr & 0xf) == 8 ? " ":"");

        sprintf(p, " |%.16s|", chrs);
        puts(line);
    }
}


void cmd_go() {
    /* run indefinitely from optional addr or PC*/
    if (E_OK != parse_addr(&pc, pc) || E_OK != parse_end()) return;

    step_mode = STEP_RUN;
}

void cmd_continue() {
    /* run imdefinitely, optionally to one-time breakpoint */
    uint16_t addr;
    if (E_OK != parse_addr(&addr, pc) || E_OK != parse_end()) return;

    if (addr != pc && !(breakpoints[addr] & MONITOR_PC)) breakpoints[addr] |= MONITOR_PC | MONITOR_ONCE;
    step_mode = STEP_RUN;
}

void _cmd_single(int mode) {
    int v;

    step_mode = mode;
    if (E_OK != parse_int(&v, 1) || E_OK != parse_end()) return;
    step_target = v;
    if (step_target > 1) puts("...");
}

void cmd_step() {
    /* show current line and run one step */
    _cmd_single(STEP_INST);
}

void cmd_next() {
    /* like step, but treat jsr ... rts as one step */
    _cmd_single(STEP_NEXT);
}

void cmd_call() {
    uint16_t ret = pc, target;

    if (E_OK != parse_addr(&target, DEFAULT_REQUIRED) || E_OK != parse_end()) return;

    /* trigger break once we return from subroutine */
    pc = target;
    if (!(breakpoints[ret] & MONITOR_PC)) breakpoints[ret] |= MONITOR_PC | MONITOR_ONCE;
    ret--;  // 6502 rts is weird...

    /* write return address to stack, directly via memory[] to avoid callbacks */
    memory[BASE_STACK + sp] = (ret >> 8) & 0xFF;
    memory[BASE_STACK + ((sp - 1) & 0xFF)] = ret & 0xFF;
    sp -= 2;

    step_mode = STEP_RUN;
}

void cmd_trigger() {
    const char* _names[] = {"reset", "irq", "nmi", 0};
    const int _vals[] = {0, 1, 2};
    uint8_t v;

    if (E_OK != parse_enum(_names, _vals, &v, DEFAULT_REQUIRED) || E_OK != parse_end()) return;

    switch (v) {
        case 0: reset6502(); break;
        case 1: irq6502(); break;
        case 2: nmi6502(); break;
    }
}

void cmd_disasm() {
    uint16_t start, end;

    if (E_OK != parse_range(&start, &end, org, 48) || E_OK != parse_end()) return;

    org = disasm(start, end);
}

void cmd_memory() {
    uint16_t start, end;

    if (E_OK != parse_range(&start, &end, org, 256) || E_OK != parse_end()) return;

    dump(start, end);
    org = end;
}

void cmd_stack() {
    if (E_OK != parse_end()) return;

    dump(sp + 0x101, 0x200);
}

void cmd_break() {
    uint16_t start, end;
    uint8_t mode;
    int endl, addr;

    if (E_OK != parse_range(&start, &end, pc, 1)) return;
    endl = end < start ? 0x10000 : end;

    if (E_OK != parse_enum(_monitor_names, _monitor_vals, &mode, MONITOR_PC) || E_OK != parse_end()) return;

    for(addr=start; addr < endl; addr++)
        breakpoints[addr] |= mode;
    org = start;
}

void cmd_delete() {
    uint16_t start, end;
    int addr, endl, n;
    uint8_t mode;

    if (
        E_OK != parse_range(&start, &end, pc, 1)
        || E_OK != parse_enum(_monitor_names, _monitor_vals, &mode, MONITOR_ANY)
        || E_OK != parse_end()
    ) return;

    endl = end <= start ? 0x10000 : end;

    for(n=0, addr=start; addr < endl; addr++)
        if (breakpoints[addr] & mode) {
            breakpoints[addr] &= ~mode;
            n++;
        }
    org = start;
    printf("Removed %d breakpoint%s.\n", n, n==1?"":"s");
}

void cmd_inspect() {
    /* show breakpoints and labels for a range of memory */
    uint16_t start, end, nmax, raddr, waddr, xaddr;
    uint64_t rmax, wmax, xmax;
    int endl, addr, span, n, prv, brk, new;
    const Symbol *sym;

    if (
        E_OK != parse_range(&start, &end, 0, 0)
        || E_OK != parse_addr(&nmax, 16)
        || E_OK != parse_end()
    ) return;

    endl = end <= start ? 0x10000 : end;

    for (prv=n=0, addr=start; addr < endl; prv=brk, addr++) {
        if (addr==start || heat_rs[addr] > rmax) rmax = heat_rs[raddr=addr];
        if (addr==start || heat_ws[addr] > wmax) wmax = heat_ws[waddr=addr];
        if (addr==start || heat_xs[addr] > xmax) xmax = heat_xs[xaddr=addr];
        if (n >= nmax) continue;

        brk = breakpoints[addr] & MONITOR_ANY;
        sym = get_next_symbol_by_value(NULL, addr);

        new = brk & (brk ^ prv);

        if (!sym && !new) continue;

        printf("%.4x", addr);
        for(/**/; sym; sym = get_next_symbol_by_value(sym, addr))
            printf("  %s", sym->name);
        puts("");
        n++;

        if (new) {   /* did some flags just switch on? */
            printf("  break");
            if (new & MONITOR_PC) {
                for(span=addr; span<0x10000 && breakpoints[span] & MONITOR_PC; span++) /**/;
                if (--span != addr) printf("  x %.4x.%.4x", addr, span);
                else printf("  x %.4x", addr);
            }
            if (new & MONITOR_READ ) {
                for(span=addr; span<0x10000 && breakpoints[span] & MONITOR_READ; span++) /**/;
                if (--span != addr) printf("  r %.4x.%.4x", addr, span);
                else printf("  r %.4x", addr);
            }
            if (new & MONITOR_WRITE) {
                for(span=addr; span<0x10000 && breakpoints[span] & MONITOR_WRITE; span++) /**/;
                if (--span != addr) printf("  w %.4x.%.4x", addr, span);
                else printf("  w %.4x", addr);
            }
            puts("");
            n++;
        }
    }
    if (n >= nmax) puts("...");
    else if (n == 0) puts("(no labels or breakpoints found)");
    printf(
        "hotspots: $%" PRIx64 " read @ %.4x; $%" PRIx64 " write @ %.4x; $%" PRIx64 " execute @ %.4x\n",
        rmax, raddr, wmax, waddr, xmax, xaddr
    );
}

void cmd_set() {
    char *name;
    int v;
    name = parse_delim();
    if (!name) {
        puts("Missing register/flag, expected one of a,x,y,sp,pc or n,v,b,d,i,z,c");
        return;
    }
    if (E_OK != parse_int(&v, DEFAULT_REQUIRED) || E_OK != parse_end()) return;
    if (E_OK != set_reg_or_flag(name, v))
        puts("Unknown register/flag, expected one of a,x,y,sp,pc or n,v,b,d,i,z,c");
}

void cmd_ticks() {
    /* set or show ticks */
    int v, err;

    if (E_OK == (err = parse_int(&v, DEFAULT_OPTIONAL))) {
        ticks = v;
    } else if (E_MISSING == err) {
        printf("%" PRIu64 " ticks\n", ticks);
    }
}

void cmd_fill() {
    uint16_t start, end;
    uint8_t v;
    int addr, endl, err;

    if (E_OK != parse_range(&start, &end, DEFAULT_REQUIRED, 1)) return;

    org = start;

    endl = end < start ? 0x10000 : end;
    if (start == end) {
        puts("Empty range, nothing to fill");
        return;
    }
    addr = start;

    if (E_OK != parse_byte(&v, 0)) return;
    memory[addr++] = v;
    for(;;) {
        err = parse_byte(&v, DEFAULT_OPTIONAL);
        if (err == E_MISSING) break;
        if (err != E_OK) return;
        memory[addr++] = v;
    }

    /* repeat the pattern until we've filled the range */
    while (addr < endl) {
        memory[addr++] = memory[start++];
    }
}

static char _bits[33];
const char* bitstr(int v) {
    char *p;
    for(p=_bits+31; p>=_bits; p--, v >>= 1)
        *p = (v & 1) ? '1': '0';
    _bits[32] = 0;
    for(p=_bits; (*p == '0') && *(p+1); p++) /**/ ;
    return p;
}

void cmd_convert() {
    /* display one or more expression results in multiple bases */
    int v, err, n;

    for(n=0;;n++) {
        err = parse_int(&v, DEFAULT_OPTIONAL);
        if (err == E_MISSING) break;
        if (E_OK != err) return;
        printf(
            "%.*s\t:=  $%x  #%d  %%%s",
            parsed_length(), parsed_str(), v, v, bitstr(v)
        );
        if (32 <= v && v < 127)
            printf("  '%c", v);
        puts("");
    }
    if (!n) puts("No values to convert");
}

void cmd_label() {
    const char *lbl;
    uint16_t addr;

    lbl = parse_delim();
    if (strlen(lbl) != symlen(lbl)) {
        puts("Invalid label");
        return;
    }
    if (E_OK != parse_addr(&addr, pc) || E_OK != parse_end()) return;

    add_symbol(lbl, addr);
}

void cmd_unlabel() {
    uint16_t addr;

    if (E_OK != parse_addr(&addr, DEFAULT_REQUIRED) || E_OK != parse_end()) return;

    if (0 == remove_symbols_by_value(addr))
        printf("No labels for $%.4x\n", addr);

}

void cmd_blockfile() {
    char *p = parse_delim();
    if (E_OK != parse_end()) return;

    if(!p) puts("Missing block file name");
    else io_blkfile(p);
}

void cmd_load() {
    const char* fname = parse_delim();
    uint16_t addr;

    if (!fname) {
        puts("Missing rom file name");
        return;
    }
    if (E_OK != parse_addr(&addr, DEFAULT_REQUIRED) || E_OK != parse_end()) return;

    (void)load_memory(fname, addr);
    org = addr;
}

void cmd_save() {
    const char* fname = parse_delim();
    uint16_t start, end;

    if (!fname) {
        puts("Missing file name");
        return;
    }
    if (E_OK != parse_range(&start, &end, 0, 0) || E_OK != parse_end()) return;

    if (start == end) end = 0;
    (void)save_memory(fname, start, end);
    org = start;
}


void cmd_heatmap() {
    /* heatmap [clear|save mapfile] [range] [r|w|d|x] */
    uint16_t start=0, end=0;
    uint8_t mode, cmd=0;
    const char *fname, *_sub_names[] = { "clear", "save", 0 };
    const int _sub_vals[] = {1, 2};
    int err;

    err = parse_enum(_sub_names, _sub_vals, &cmd, DEFAULT_OPTIONAL);
    if (err != E_OK && err != E_MISSING) return;
    if (cmd == 2 && !(fname = parse_delim())) {
        puts("Missing filename");
        return;
    }
    /* try parsing a mode before range in case range was omitted, otherwise 'heat x' interprets
     * x as an expression defining the range... */
    if (E_OK != parse_enum(_monitor_names, _monitor_vals, &mode, DEFAULT_OPTIONAL)) {
        if (E_OK != parse_range(&start, &end, 0, 0)) return;
        if (E_OK != parse_enum(_monitor_names, _monitor_vals, &mode, MONITOR_DATA)) return;
    }

    switch (cmd) {
        case 0: heatmap(start, end, mode); break;
        case 1: clear_heatmap(start, end, mode); break;
        case 2: (void)save_heatmap(fname, start, end, mode); break;
    }
}

void cmd_quit() {
    if (E_OK != parse_end()) return;
    monitor_exit();
    exit(0);
}


void cmd_help() {
    int i;
    puts("\nAvailable commands:\n\n");
    for (i=0; 0 != strcmp(_cmds[i].name, "?"); i++)
        printf("  %s %s\n", _cmds[i].name, _cmds[i].help);

    puts(
        "\n"
        "Type q to exit. Use ctrl-C to interrupt the simulator.\n"
        "Commands can be shortened to their first few letters, searched in the order above.\n"
        "For example 'd' means 'disassemble' while 'de' becomes 'delete'.\n"
        "Tab completion and command history (up/down) are also available.\n"
        "Enter repeats commands like step, next, dis or mem as they advance through memory.\n"
        "Literal values are assumed to be hex, e.g. 123f, unless prefixed as binary (%),\n"
        "decimal (#), hex ($) or an ascii character ('). Use the ~ command as a calculator\n"
        "to display values or expression results in all bases. C-style expressions can be used\n"
        "for most address and value arguments.  Expressions can include (case-sensitive) labels\n"
        "as well as CPU registers A, X, Y, PC, SP and flags N, V, B, D, I, Z and C.\n"
        "The usual operators are available, along with * and @ to deference memory by byte or word\n"
        "and unary < and > to extract least and most significant bytes like most assemblers.\n"
        "For example >(@(*(pc+1))+x) takes the high byte of an indirect zp address via a PC operand.\n"
        "Write ranges as start.end or start..offset, e.g. 1234.1268, pc .. 10, 1234, ..20, .label.\n"
        "Think of the second . as a shortcut: start..offset is the same as start.start+offset\n"
    );
}

Command _cmds[] = {
    { "go", "[addr] - run from pc (or optional addr) until breakpoint", 0, cmd_go },
    { "continue [addr]", "- run from pc until breakpoint (or optional addr)", 0, cmd_continue },
    { "step", "- [count] step by single instructions", 1, cmd_step },
    { "next", "- [count] like step but treats jsr ... rts as one step", 1, cmd_next },
    { "call", "addr - call subroutine leaving PC unchanged", 0, cmd_call },
    { "trigger", "irq|nmi|reset - trigger an interrupt signal", 0, cmd_trigger },

    { "disassemble", "[range] - show code disassembly for range (or current)", 1, cmd_disasm },
    { "memory", "[range] - dump memory contents for range (or current)", 1, cmd_memory },
    { "stack", "- show stack contents, sp+1 through $1ff", 0, cmd_stack },
    { "break",  "[range] [r|w|d|x] - trigger break on read, write, any access or execute (default)", 0, cmd_break },
    { "delete",  "[range] - remove all breakpoints in range (default PC)", 0, cmd_delete },
    { "inspect", "[range] [max] - show labels and breakpoints in range, up to max lines", 0, cmd_inspect },

    { "set", "{a|x|y|sp|pc|n|v|d|i|z|c} value - modify a register or flag", 0, cmd_set },
    { "ticks", "[value] - query or set the current cycle count", 0, cmd_ticks },
    { "fill", "range value ... - set memory contents in range to value(s)", 0, cmd_fill },
    { "~", "value ... - show each value in binary, octal, hex and decimal", 0, cmd_convert },
    { "label", "name addr - add a symbolic name for addr", 0, cmd_label },
    { "unlabel", "name|addr - remove label by name or addr", 0, cmd_unlabel },

    { "load", "romfile addr - read binary file to memory", 0, cmd_load },
    { "save", "romfile [range] - write memory to file (default full dump)", 0, cmd_save },
    { "heatmap", " [clear|save mapfile] [range] [r|w|d|x] - view, reset or save heatmap data", 0, cmd_heatmap },
    { "blockfile", "[blockfile] - use binary file for block storage, empty to disable", 0, cmd_blockfile },
    { "quit", "- leave c65", 0, cmd_quit },
    { "help", "or ? - show this help", 0, cmd_help },
    { "?", "", 0, cmd_help },    // this must be last
};

#define n_cmds sizeof(_cmds)/sizeof(_cmds[0])

Command *_repeat_cmd = NULL;

void do_cmd() {
    char* p;
    Command *cmd;
    int i;

    p = parse_delim();
    if (!p) return;

    for(i=0; i<n_cmds; i++) {
        /* check for case insenstive match */
        if(!strncasecmp(p, _cmds[i].name, strlen(p))) {
            cmd = _cmds+i;
            _repeat_cmd = cmd->repeatable ? cmd: NULL;
            cmd->handler();
            return;
        }
    }
    puts("Unknown command, try ? for help");
}


void completion(const char *buf, linenoiseCompletions *lc, void *user_data) {
    int i;
    if (!strlen(buf)) return;
    for(i=0; i<n_cmds; i++) {
        if(!strncmp(buf, _cmds[i].name, strlen(buf))) {
            linenoiseAddCompletion(lc, _cmds[i].name);
        }
    }
}


void monitor_init(const char * labelfile) {
    FILE *f;
    char buf[128], *s;
    const char *label;
    int fail=0, tmp;
    linenoiseSetCompletionCallback(completion, NULL);
    linenoiseHistorySetMaxLen(256);
    linenoiseHistoryLoad(".c65");

    if (labelfile) {
        f = fopen(labelfile, "r");
        if (!f) {
            printf("Can't read labels from %s\n", labelfile);
            return;
        }
        while (fgets(buf, sizeof(buf), f) > 0) {
            label = strtok(buf, "\t =");
            s = strtok(NULL, "\t =\n");
            /*
                tailored to 64tass --labels output with 'symbol\t=\t$1234
                enumerated constants are emitted in similar format, eg. with two digits
                but we want to ignore those
            */
            if (s && s[0] == '$' && strlen(s) == 5 && (E_OK == strexpr(s+1, &tmp))) {
                add_symbol(label, (uint16_t)tmp);
            } else {
                fail++;
            }
        }
        if (fail) printf("Skipped %d lines from %s\n", fail, labelfile);
        fclose(f);
    }
    puts("Type ? for help, ctrl-C to interrupt, quit to exit.");
}

void monitor_exit() {
    linenoiseHistorySave(".c65");
}

void monitor_command() {
    char *line;

    if (step_mode != STEP_NONE) {
        org = pc;
        step_mode = STEP_NONE;
        if (break_flag & MONITOR_DATA) {
            printf("%.4x: memory %s\n", rw_brk, break_flag & MONITOR_READ ? "read": "write");
            dump(rw_brk & 0xfff0, rw_brk | 0xf);
        }
        disasm(pc, pc+1);
    }

    line = linenoise(prompt());
    if (!line) return;

    if (parse_start(line)) {
        linenoiseHistoryAdd(line);
        do_cmd();
    } else if (_repeat_cmd) {
        /* empty line can repeat some commands */
        _repeat_cmd->handler();
    }
    free(line);
}

