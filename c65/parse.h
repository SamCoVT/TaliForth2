/*
Linked list of symbolic address labels.
We can have multiple labels for a single address,
but require that label names are unique.
*/
typedef struct Symbol {
    const char *name;
    uint16_t value;
    struct Symbol* next;
} Symbol;

extern const char *pexpr;

void add_symbol(const char* name, uint16_t value);
const Symbol* get_symbol(const char *name);
const Symbol* get_next_symbol_by_value(const Symbol* sym, uint16_t value);
void remove_symbol(const char *name);
int remove_symbols_by_value(uint16_t value);

#define DEFAULT_REQUIRED 0x10001
#define DEFAULT_OPTIONAL 0x10002

#define E_OK 0
#define E_MISSING -1
#define E_PARSE -2
#define E_RANGE -3

int strexpr(char *src, int *result);
int symlen(const char *s);

int parse_start(char *src);
int parse_end();
char* parse_delim();
int parse_enum(const char *names[], const int vals[], uint8_t *v, int dflt);
int parse_int(int *v, int dflt);
int parse_byte(uint8_t *v, int dflt);
int parse_addr(uint16_t *v, int dflt);
int parse_range(uint16_t* start, uint16_t* end, int dflt_start, int dflt_length);

char* parsed_str();
int parsed_length();
