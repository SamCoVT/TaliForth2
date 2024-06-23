/*
Simple shunting yard expression parser modeled on:
https://www.engr.mun.ca/~theo/Misc/exp%5fparsing.htm

E --> T { + T }
T --> val | "(" E ")" | ~ T
+ --> <binary operator>
~ --> <unary operator>
*/

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>

#include "parse.h"
#include "c65.h"  /* for memory[] */

#define EX_OK 0
#define EX_EMPTY 1
#define EX_EXTRA 2
#define EX_VALSTK 3
#define EX_PAREN 4
#define EX_TERM 5
#define EX_UNARY 6
#define EX_BINARY 7
#define EX_TERNARY 8


#define OP_NONE 0               /* special sentinal */
/* most operators are represented by a single ascii symbol char, but we need a few extra */
#define OP_LSHFT 0x0e
#define OP_RSHFT 0x0f
#define OP_LE 0x1e
#define OP_GE 0x1f
#define OP_AND 0x01
#define OP_OR 0x02
#define BINOP_FLAG 0x80     /* flags binary operators in the operator stack */

Symbol *symbols = NULL;

char *cursor, *parse_last;

/* parser state variables */
static unsigned char opstk[32]; /* stack of operators */
static int valstk[32], qstk[32], literal; /* stack of values, ?: conditions, and most recent literal */
static uint16_t n_op, n_val, n_q;    /* stack pointers */

/* characters representing each operator, usually corresponding to the input text */
const char
    unaryops[] = "+-!~*@<>",
    binaryops[] = "*/%+-\x0e\x0f<>\x1e\x1f=!&^|\x01\x02?:";
/*
unary operators are all precedence 0 (highest)
binary operators are mapped from the binaryops[] list above
*/
uint8_t binaryprecs[] = {
    1,1,1,      // * / %
    2,2,        // + -
    3,3,        // << >>
    4,4,4,4,    // < > <= >=
    5,5,        // = != and == <>
    6,          // &
    7,          // ^
    8,          // |
    9,          // &&
    10,         // ||
    11,         // ?
    12,         // :
};

uint8_t binaryprec(unsigned char op) {
    const char *p = strchr(binaryops, op);
    if (!p) {
        /* paranoid */
        printf("Unknown operator in binaryprec '%c' (%d)\n", op, op);
        p = binaryops;
    }
    return binaryprecs[p - binaryops];
}

int apply_unary(unsigned char op, int v, int *out) {
    switch (op) {
        case '+': *out = v; return EX_OK;
        case '-': *out = -v; return EX_OK;
        case '~': *out = ~v; return EX_OK;
        case '!': *out = !v; return EX_OK;
        case '@': *out = memory[v & 0xffff] + (memory[(v+1) & 0xffff] << 8); return EX_OK;
        case '*': *out = memory[v & 0xffff]; return EX_OK;
        case '<': *out = v & 0xff; return EX_OK;
        case '>': *out = (v >> 8) & 0xff; return EX_OK;
    }
    return EX_UNARY;
}

int apply_binary(unsigned char op, int a, int b, int *out) {
    switch (op) {
        case '*': *out = a * b; return EX_OK;
        case '/': *out = a / b; return EX_OK;
        case '%': *out = a % b; return EX_OK;

        case '+': *out = a + b; return EX_OK;
        case '-': *out = a - b; return EX_OK;

        case OP_LSHFT: *out = a << b; return EX_OK;
        case OP_RSHFT: *out = a >> b; return EX_OK;

        case '<': *out = a < b; return EX_OK;
        case '>': *out = a > b; return EX_OK;
        case OP_LE: *out = a <= b; return EX_OK;
        case OP_GE: *out = a >= b; return EX_OK;

        case '=': *out = a == b; return EX_OK;
        case '!': *out = a != b; return EX_OK;

        case '&': *out = a & b; return EX_OK;
        case '^': *out = a ^ b; return EX_OK;
        case '|': *out = a | b; return EX_OK;

        case OP_AND: *out = a && b; return EX_OK;
        case OP_OR: *out = a || b; return EX_OK;

        case '?': qstk[n_q++] = a; *out = b; return EX_OK;
        case ':':
            if (!n_q) return EX_TERNARY;
            *out = qstk[--n_q] ? a : b;
            return EX_OK;
    }
    return EX_BINARY;
}

void add_symbol(const char* name, uint16_t value) {
    Symbol *sym, *prv;

    /* first discard any existing label with the same name */
    for (prv=NULL, sym=symbols; sym; prv=sym, sym=sym->next) {
        if (0==strcmp(sym->name, name)) {
            if (prv) prv->next = sym->next;
            else symbols = sym->next;
            free((void*)sym->name);
            free(sym);
            break;
        }
    }

    /* add the new label to head of list*/
    sym = malloc(sizeof(Symbol));
    sym->name = strdup(name);
    sym->value = value;
    sym->next = symbols;
    symbols = sym;
}

const Symbol* get_symbol(const char *name) {
    Symbol *sym;
    for(sym=symbols; sym && 0 != strcmp(sym->name, name); sym = sym->next) /**/ ;
    return sym;
}

const Symbol* get_next_symbol_by_value(const Symbol* sym, uint16_t value) {
    for(sym = sym ? sym ->next : symbols; sym && sym->value != value; sym = sym->next) /**/ ;
    return sym;
}

void remove_symbol(const char *name) {
    Symbol *prv, *sym;
    for (prv=NULL, sym=symbols; sym && 0 != strcmp(sym->name, name); prv=sym, sym=sym->next) /**/ ;

    if (sym) {
        if (prv) prv->next = sym->next;
        else symbols = sym->next;
        free((void*)sym->name);
        free(sym);
    }
}

int remove_symbols_by_value(uint16_t value) {
    Symbol *prv, *sym;
    int n=0;
    /* remove all labels matching addr */
    for (prv=NULL, sym=symbols; sym; prv=sym, sym=sym->next) {
        if(sym->value == value) {
            if (prv) prv->next = sym->next;
            else symbols = sym->next;
            free((void*)sym->name);
            free(sym);
            sym = prv ? prv : symbols;
            n++;
        }
    }
    return n;
}

int symlen(const char *s) {
    int n=0;
    if (s && (*s == '_' || isalpha(*s)))
        for (n=1; *(s+n) == '_' || isalnum(*(s+n)); n++) /**/ ;
    return n;
}

unsigned char maybe_literal(void) {
    char *p;
    static char name[64];
    int base, n;
    const Symbol *sym;

    n = symlen(cursor);
    if (!strchr("$#%'", *cursor) && !isdigit(*cursor) && !n) return OP_NONE;

    /* first try symbols since user can disambiguate numbers with a prefix */
    if (n) {
        strncpy(name, cursor, n);
        name[n] = 0;
        /* is it dynamic symbol? */
        if ((literal = get_reg_or_flag(name)) >= 0) {
            cursor += n;
            return '#';
        }
        /* regular symbol? */
        if ((sym = get_symbol(name))) {
            literal = sym->value;
            cursor += n;
            return '#';
        }
    }
    /* no luck with symbols, maybe literal constant with optional prefix */
    /* what about a character literal like 'a or '3' (close quote is optional) */
    if (*cursor == '\'') {
        cursor++;
        if (0 == (literal = *cursor)) return OP_NONE;
        cursor++;
        if (*cursor == '\'') cursor++; /* optional closing quote */
        return '#';
    }
    base = 16;
    switch(*cursor) {
        case '$': base = 16; cursor++; break;
        case '#': base = 10; cursor++; break;
        case '%': base = 2; cursor++; break;
    }
    literal = strtol(cursor, (char**)&p, base);
    if (p == cursor) return OP_NONE;
    cursor = p;
    return '#';
}


unsigned char next_token(uint8_t need_binary) {
    unsigned char tok=0;

    while (isblank(*cursor)) cursor++;
    if (!*cursor) return OP_NONE;
    if (need_binary) {
        if (strchr(binaryops, *cursor)) {
            tok = *cursor++;
            /*
            some binary operators are overloaded on their first character
            so we resolve that ambiguity here, using special chars to distinguish
            */
            if (strchr("<>", tok)) {
                if (*cursor == '=') {
                    /* <=, >= */
                    tok = tok == '<' ? OP_LE : OP_GE;
                    cursor++;
                } else if (*cursor == tok) {
                    /* <<, >> */
                    tok = tok == '<' ? OP_LSHFT : OP_RSHFT;
                    cursor++;
                } else if (*cursor == '>') {
                    /* <> */
                    tok = '!';
                    cursor++;
                }
            } else if (tok == '=') {
                if (*cursor == '=') cursor++;    /* allow both = ane == */
            } else if (tok == '!') {
                /* require trailing = for != */
                if (*cursor == '=') cursor++;
                else return OP_NONE;
            } else if (strchr("&|", tok)) {
                /* distinguish && / || from & / | */
                if (*cursor == tok) {
                    tok = tok == '&' ? OP_AND : OP_OR;
                    cursor++;
                }
            }
        }
    } else {
        /* otherwise we're looking for symbol, literal value, open bracket, unary op */
        if (strchr("()", *cursor) || strchr(unaryops, *cursor)) {
            tok = *cursor++;
        } else {
            tok = maybe_literal();
        }
    }
    return tok;
}

int pop_op() {
    /* process the top operator on the stack, consuming its input value(s) to produce an output value */
    unsigned char tok = opstk[--n_op];
    int err;
    if (tok & BINOP_FLAG) {
        tok ^= BINOP_FLAG;
        err = apply_binary(tok, valstk[n_val-2], valstk[n_val-1], valstk + n_val-2);
        n_val--;
    } else
        err = apply_unary(tok, valstk[n_val-1], valstk + n_val - 1);
    return err;
}

int has_lower_precedence(unsigned char this, unsigned char other) {
    /* sentinel is lowest, unary are all highest */
    if (other == OP_NONE || !(this & BINOP_FLAG)) return 0;
    /* binary is lower than any unary */
    if (!(other & BINOP_FLAG)) return 1;

    /* numbered from highest to lowest precdence */
    return binaryprec(this ^ BINOP_FLAG) > binaryprec(other ^ BINOP_FLAG);
}

int push_op(unsigned char tok) {
    /* add a new operator to the stack, first resolving any higher precdence operators already there */
    int err;
    while (has_lower_precedence(tok, opstk[n_op-1]))
        if ((err = pop_op())) return err;
    opstk[n_op++] = tok;
    return EX_OK;
}

int match_expr();

int match_term() {
    unsigned char tok;
    int err;

    tok = next_token(0);
    if (tok == '#') {
        valstk[n_val++] = literal;
    } else if (tok == '(') {
        if ((err = push_op(OP_NONE))) return err;
        if ((err = match_expr())) return err;
        if (next_token(0) != ')') return EX_PAREN;
        n_op--;
    } else if (tok) {
        if ((err = push_op(tok))) return err;
        if ((err = match_term())) return err;
    } else {
        return EX_TERM;
    }
    return EX_OK;
}

int match_expr() {
    int err;
    unsigned char tok;

    if ((err = match_term())) return err;
    while ((tok = next_token(1))) {
        if ((err = push_op(tok | BINOP_FLAG))) return err;
        if ((err = match_term())) return err;
    }
    while (opstk[n_op-1] != OP_NONE)
        if ((err = pop_op())) return err;
    return EX_OK;
}

int strexpr(char *src, int *result) {
    int err;
    n_op = n_val = n_q = 0;

    cursor = src;
    if (!cursor) return EX_EMPTY;

    while (isblank(*cursor)) cursor++;
    if (!*cursor) return EX_EMPTY;

    opstk[n_op++] = OP_NONE;
    if ((err = match_expr())) return cursor==src ? EX_EMPTY : err;
    if (n_val != 1) return EX_VALSTK;
    if (n_q != 0) return EX_TERNARY;
    *result = valstk[0];
    return EX_OK;
}


void show_error(int error) {
    int i;
    switch(error) {
        case EX_EMPTY:
            puts("missing expression");
            return;
        case EX_EXTRA:
            puts("unexpected");
            break;
        case EX_VALSTK:
            puts("invalid expression");
            break;
        case EX_PAREN:
            puts("unbalanced parentheses");
            break;
        case EX_TERM:
            puts("unrecognized");
            break;
        case EX_UNARY:
            puts("unknown unary operator");
            break;
        case EX_BINARY:
            puts("unknown binary operator");
            break;
        case EX_TERNARY:
            puts("unbalanced or ambiguous ?: expression");
            break;
    }
    /* restore delimited word if that was last */
    if (strlen(parse_last) < cursor - parse_last)
        parse_last[strlen(parse_last)] = ' ';
    printf("    %s\n----", parse_last);
    for(i=0; i<cursor-parse_last; i++) putchar('-');
    puts("^");
}


/* start parsing a new line, setting the global cursor and returning non-zero for non-empty string */
int parse_start(char *src) {
    char *p;

    /* strip comments starting with ; along with any preceding spaces */
    p = strpbrk(src, ";");
    if (p) {
        while (p > src && *(p-1) == ' ') p--;
        *p = 0;
    }

    cursor = src;
    parse_last = NULL;
    return *src;
}

/* assert that we've finished the line (only whitespace remains) */
int parse_end() {
    while (cursor && isblank(*cursor)) cursor++;
    if (cursor && *cursor) {
        show_error(EX_EXTRA);
        return E_PARSE;
    }
    return E_OK;
}

/* return the start of the last parsed section, typically an expression */
char* parsed_str() {
    return parse_last;
}

/* return the length of the last parsed section, e.g. for expression which isn't delimited by \0 */
int parsed_length() {
    return (int)(cursor - parse_last);
}

/* find a delimited word using strtok semnatics */
char* parse_delim() {
    char *p;
    parse_last = cursor;
    p = strtok_r(parse_last, " \t", &cursor);
    return p;
}

int parse_enum(const char *names[], const int vals[], uint8_t *v, int dflt) {
    int i;
    char *p;
    const char *q;
    while (cursor && isblank(*cursor)) cursor++;
    if (!cursor || !(*cursor)) {
            switch (dflt) {
                case DEFAULT_OPTIONAL: return E_MISSING;
                case DEFAULT_REQUIRED:
                    printf("Missing required value:");
                    for(i=0; names[i]; i++) printf(" %s", names[i]);
                    puts("");
                    return E_PARSE;
                default: *v = (uint8_t)dflt; return E_OK;
            }
    }
    for (i=0; names[i]; i++) {
        p = cursor;
        q = names[i];
        while (*p && !isblank(*p) && tolower(*p) == *q) {
            p++;
            q++;
        }
        if(!(*p) || isblank(*p)) {
            *v = vals[i];
            cursor = p;
            return E_OK;
        }
    }
    if (dflt == DEFAULT_OPTIONAL) return E_MISSING;

    printf("Invalid value, expected:");
    for(i=0; names[i]; i++) printf(" %s", names[i]);
    puts("");
    return E_RANGE;
}

/* parse an expression as an integer, returning an error status */
int parse_int(int *v, int dflt) {
    int err;
    parse_last = cursor;
    if (EX_OK != (err = strexpr(parse_last, v))) {
        if (err == EX_EMPTY && dflt != DEFAULT_REQUIRED) {
            if (dflt == DEFAULT_OPTIONAL) return E_MISSING;
            *v = dflt;
        } else {
            show_error(err);
            return E_PARSE;
        }
    }
    return E_OK;
}

/* restrict an expression to a byte */
int parse_byte(uint8_t *v, int dflt) {
    int tmp, err;

    if (E_OK != (err = parse_int(&tmp, dflt))) return err;
    if (abs(tmp) > 255) {
        printf("byte: value %d out of range\n", tmp);
        return E_RANGE;
    }
    *v = (uint8_t)(tmp & 0xff);
    return E_OK;
}

/* restrict an expression to an unsigned word */
int parse_addr(uint16_t *v, int dflt) {
    int tmp, err;

    if (E_OK != (err = parse_int(&tmp, dflt))) return err;
    if (tmp < 0 || tmp > 0xffff) {
        printf("address: value %d out of range\n", tmp);
        return E_RANGE;
    }
    *v = (uint16_t)(tmp & 0xffff);
    return E_OK;
}

/* parse a range expression start.end or start,offset */
int parse_range(uint16_t* start, uint16_t* end, int dflt_start, int dflt_length) {
    int dflt, err;
    uint16_t tmp;

    if (E_OK != (err = parse_addr(start, dflt_start))) return err;
    while(cursor && isblank(*cursor)) cursor++;
    /* is there an explicit . end or .. offset? */
    if (cursor && *cursor == '.') {
        cursor++;
        /* is it .. for offset? */
        if (*cursor == '.') {
            cursor++;
            if (E_OK != (err = parse_addr(&tmp, dflt_length))) return err;
            *end = *start + tmp;
        } else {
            /* it's just . for end */
            dflt = (dflt_length == DEFAULT_REQUIRED) ? DEFAULT_REQUIRED : (*start + dflt_length);
            if (E_OK != (err = parse_addr(end, dflt))) return err;
        }
    } else {
        /* no explicit . end or .. offset so use default if there is one */
        if (dflt_length == DEFAULT_REQUIRED) {
            puts("range missing required .end or ..offset");
            return E_MISSING;
        }
        *end = *start + dflt_length;
    }
    return E_OK;
}
