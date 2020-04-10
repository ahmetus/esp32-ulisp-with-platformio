#include <Arduino.h>

// Lisp Library
const char LispLibrary[] = "";

// Compile options

// #define resetautorun

#define printfreespace
#define serialmonitor

// #define printgcs
// #define sdcardsupport
// #define lisplibrary

// Includes

// #include "LispLibrary.h"

#include <setjmp.h>
#include <SPI.h>
#include <Wire.h>
#include <limits.h>
#include <EEPROM.h>
#if defined (ESP8266)
  #include <ESP8266WiFi.h>
#elif defined (ESP32)
  #include <WiFi.h>
#endif

#if defined(sdcardsupport)
  #include <SD.h>
  #define SDSIZE 172
#else
  #define SDSIZE 0
#endif



// Constants

const int TRACEMAX = 3; // Number of traced functions
enum type { ZERO=0, SYMBOL=2, NUMBER=4, STREAM=6, CHARACTER=8, FLOAT=10, STRING=12, PAIR=14 };  // STRING and PAIR must be last
enum token { UNUSED, BRA, KET, QUO, DOT };
enum stream { SERIALSTREAM, I2CSTREAM, SPISTREAM, SDSTREAM, WIFISTREAM };

enum function { NIL, TEE, NOTHING, OPTIONAL, AMPREST, LAMBDA, LET, LETSTAR, CLOSURE, SPECIAL_FORMS, QUOTE,
DEFUN, DEFVAR, SETQ, LOOP, RETURN, PUSH, POP, INCF, DECF, SETF, DOLIST, DOTIMES, TRACE, UNTRACE,
FORMILLIS, WITHSERIAL, WITHI2C, WITHSPI, WITHSDCARD, WITHCLIENT, TAIL_FORMS, PROGN, IF, COND, WHEN,
UNLESS, CASE, AND, OR, FUNCTIONS, NOT, NULLFN, CONS, ATOM, LISTP, CONSP, SYMBOLP, STREAMP, EQ, CAR, FIRST,
CDR, REST, CAAR, CADR, SECOND, CDAR, CDDR, CAAAR, CAADR, CADAR, CADDR, THIRD, CDAAR, CDADR, CDDAR, CDDDR,
LENGTH, LIST, REVERSE, NTH, ASSOC, MEMBER, APPLY, FUNCALL, APPEND, MAPC, MAPCAR, MAPCAN, ADD, SUBTRACT,
MULTIPLY, DIVIDE, MOD, ONEPLUS, ONEMINUS, ABS, RANDOM, MAXFN, MINFN, NOTEQ, NUMEQ, LESS, LESSEQ, GREATER,
GREATEREQ, PLUSP, MINUSP, ZEROP, ODDP, EVENP, INTEGERP, NUMBERP, FLOATFN, FLOATP, SIN, COS, TAN, ASIN,
ACOS, ATAN, SINH, COSH, TANH, EXP, SQRT, LOG, EXPT, CEILING, FLOOR, TRUNCATE, ROUND, CHAR, CHARCODE,
CODECHAR, CHARACTERP, STRINGP, STRINGEQ, STRINGLESS, STRINGGREATER, SORT, STRINGFN, CONCATENATE, SUBSEQ,
READFROMSTRING, PRINCTOSTRING, PRIN1TOSTRING, LOGAND, LOGIOR, LOGXOR, LOGNOT, ASH, LOGBITP, EVAL, GLOBALS,
LOCALS, MAKUNBOUND, BREAK, READ, PRIN1, PRINT, PRINC, TERPRI, READBYTE, READLINE, WRITEBYTE, WRITESTRING,
WRITELINE, RESTARTI2C, GC, ROOM, SAVEIMAGE, LOADIMAGE, CLS, PINMODE, DIGITALREAD, DIGITALWRITE,
ANALOGREAD, ANALOGWRITE, DELAY, MILLIS, SLEEP, NOTE, EDIT, PPRINT, PPRINTALL, REQUIRE, LISTLIBRARY,
AVAILABLE, WIFISERVER, WIFISOFTAP, CONNECTED, WIFILOCALIP, WIFICONNECT, ENDFUNCTIONS };

// Typedefs

typedef unsigned int symbol_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      unsigned int type;
      union {
        symbol_t name;
        int integer;
        float single_float;
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);

typedef struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t min;
  uint8_t max;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);
typedef int PinMode;

// Workspace
#define WORDALIGNED __attribute__((aligned (4)))
#define BUFFERSIZE 34  // Number of bits+2

#if defined(ESP8266)
  #define PSTR(s) s
  #define PROGMEM
  #define WORKSPACESIZE 3072-SDSIZE       /* Cells (8*bytes) */
  #define EEPROMSIZE 4096                 /* Bytes available for EEPROM */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define SDCARD_SS_PIN 10
  uint8_t _end;
  typedef int BitOrder;

#elif defined(ESP32)
  #define WORKSPACESIZE 8000-SDSIZE       /* Cells (8*bytes) */
  #define EEPROMSIZE 4096                 /* Bytes available for EEPROM */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define analogWrite(x,y) dacWrite((x),(y))
  #define SDCARD_SS_PIN 13
  uint8_t _end;
  typedef int BitOrder;

#endif

object Workspace[WORKSPACESIZE] WORDALIGNED;
char SymbolTable[SYMBOLTABLESIZE];

// Global variables

jmp_buf exception;
unsigned int Freespace = 0;
object *Freelist;
char *SymbolTop = SymbolTable;
unsigned int I2CCount;
unsigned int TraceFn[TRACEMAX];
unsigned int TraceDepth[TRACEMAX];

object *GlobalEnv;
object *GCStack = NULL;
object *GlobalString;
int GlobalStringIndex = 0;
char BreakLevel = 0;
char LastChar = 0;
char LastPrint = 0;

// Flags
enum flag { PRINTREADABLY, RETURNFLAG, ESCAPE, EXITEDITOR, LIBRARYLOADED, NOESC };
volatile char Flags = 0b00001; // PRINTREADABLY set by default

// Forward references
object *tee;
object *tf_progn (object *form, object *env);
object *eval (object *form, object *env);
object *read ();
void repl(object *env);
void printobject (object *form, pfun_t pfun);
char *lookupbuiltin (symbol_t name);
intptr_t lookupfn (symbol_t name);
int builtin (char* n);
void error (symbol_t fname, PGM_P string, object *symbol);
void error2 (symbol_t fname, PGM_P string);


/* ulisp.c */
extern void initworkspace(void);
extern object *myalloc(void);
extern object *number(int n);
extern object *makefloat(float f);
extern object *character(char c);
extern object *cons(object *arg1, object *arg2);
extern object *symbol(symbol_t name);
extern object *newsymbol(symbol_t name);
extern object *stream(unsigned char streamtype, unsigned char address);
extern void markobject(object *obj);
extern void sweep(void);
extern void gc(object *form, object *env);
extern void movepointer(object *from, object *to);
extern int compactimage(object **arg);
extern char *MakeFilename(object *arg);
extern unsigned int saveimage(object *arg);
extern int EpromReadInt(int *addr);
extern unsigned int loadimage(object *arg);
extern void autorunimage(void);
extern void trace(symbol_t name);
extern void untrace(symbol_t name);
extern int toradix40(char ch);
extern int fromradix40(int n);
extern int pack40(char *buffer);
extern int digitvalue(char d);
extern char *symbolname(symbol_t x);
extern int checkinteger(symbol_t name, object *obj);
extern float checkintfloat(symbol_t name, object *obj);
extern int checkchar(symbol_t name, object *obj);
extern int isstream(object *obj);
extern int issymbol(object *obj, symbol_t n);
extern void checkargs(symbol_t name, object *args);
extern int eq(object *arg1, object *arg2);
extern int listlength(symbol_t name, object *list);
extern object *assoc(object *key, object *list);
extern object *delassoc(object *key, object **alist);
extern void indent(int spaces, pfun_t pfun);
extern void buildstring(char ch, int *chars, object **head);
extern object *readstring(char delim, gfun_t gfun);
extern int stringlength(object *form);
extern char nthchar(object *string, int n);
extern char *cstringbuf(object *arg);
extern char *cstring(object *form, char *buffer, int buflen);
extern object *lispstring(char *s);
extern object *value(symbol_t n, object *env);
extern object *findvalue(object *var, object *env);
extern object *closure(int tc, symbol_t name, object *state, object *function, object *args, object **env);
extern object *apply(symbol_t name, object *function, object *args, object *env);
extern object **place(symbol_t name, object *args, object *env);
extern void serialbegin(int address, int baud);
extern void serialend(int address);
extern gfun_t gstreamfun(object *args);
extern pfun_t pstreamfun(object *args);
extern void checkanalogread(int pin);
extern void checkanalogwrite(int pin);
extern void tone(int pin, int note);
extern void noTone(int pin);
extern void playnote(int pin, int note, int octave);
extern void nonote(int pin);
extern void initsleep(void);
extern void sleep(int secs);
extern object *sp_quote(object *args, object *env);
extern object *sp_defun(object *args, object *env);
extern object *sp_defvar(object *args, object *env);
extern object *sp_setq(object *args, object *env);
extern object *sp_loop(object *args, object *env);
extern object *sp_return(object *args, object *env);
extern object *sp_push(object *args, object *env);
extern object *sp_pop(object *args, object *env);
extern object *sp_incf(object *args, object *env);
extern object *sp_decf(object *args, object *env);
extern object *sp_setf(object *args, object *env);
extern object *sp_dolist(object *args, object *env);
extern object *sp_dotimes(object *args, object *env);
extern object *sp_trace(object *args, object *env);
extern object *sp_untrace(object *args, object *env);
extern object *sp_formillis(object *args, object *env);
extern object *sp_withserial(object *args, object *env);
extern object *sp_withi2c(object *args, object *env);
extern object *sp_withspi(object *args, object *env);
extern object *sp_withsdcard(object *args, object *env);
extern object *sp_withclient(object *args, object *env);
extern object *tf_progn(object *args, object *env);
extern object *tf_if(object *args, object *env);
extern object *tf_cond(object *args, object *env);
extern object *tf_when(object *args, object *env);
extern object *tf_unless(object *args, object *env);
extern object *tf_case(object *args, object *env);
extern object *tf_and(object *args, object *env);
extern object *tf_or(object *args, object *env);
extern object *fn_not(object *args, object *env);
extern object *fn_cons(object *args, object *env);
extern object *fn_atom(object *args, object *env);
extern object *fn_listp(object *args, object *env);
extern object *fn_consp(object *args, object *env);
extern object *fn_symbolp(object *args, object *env);
extern object *fn_streamp(object *args, object *env);
extern object *fn_eq(object *args, object *env);
extern object *fn_car(object *args, object *env);
extern object *fn_cdr(object *args, object *env);
extern object *fn_caar(object *args, object *env);
extern object *fn_cadr(object *args, object *env);
extern object *fn_cdar(object *args, object *env);
extern object *fn_cddr(object *args, object *env);
extern object *fn_caaar(object *args, object *env);
extern object *fn_caadr(object *args, object *env);
extern object *fn_cadar(object *args, object *env);
extern object *fn_caddr(object *args, object *env);
extern object *fn_cdaar(object *args, object *env);
extern object *fn_cdadr(object *args, object *env);
extern object *fn_cddar(object *args, object *env);
extern object *fn_cdddr(object *args, object *env);
extern object *fn_length(object *args, object *env);
extern object *fn_list(object *args, object *env);
extern object *fn_reverse(object *args, object *env);
extern object *fn_nth(object *args, object *env);
extern object *fn_assoc(object *args, object *env);
extern object *fn_member(object *args, object *env);
extern object *fn_apply(object *args, object *env);
extern object *fn_funcall(object *args, object *env);
extern object *fn_append(object *args, object *env);
extern object *fn_mapc(object *args, object *env);
extern object *fn_mapcar(object *args, object *env);
extern object *fn_mapcan(object *args, object *env);
extern object *add_floats(object *args, float fresult);
extern object *fn_add(object *args, object *env);
extern object *subtract_floats(object *args, float fresult);
extern object *negate(object *arg);
extern object *fn_subtract(object *args, object *env);
extern object *multiply_floats(object *args, float fresult);
extern object *fn_multiply(object *args, object *env);
extern object *divide_floats(object *args, float fresult);
extern object *fn_divide(object *args, object *env);
extern object *fn_mod(object *args, object *env);
extern object *fn_oneplus(object *args, object *env);
extern object *fn_oneminus(object *args, object *env);
extern object *fn_abs(object *args, object *env);
extern object *fn_random(object *args, object *env);
extern object *fn_maxfn(object *args, object *env);
extern object *fn_minfn(object *args, object *env);
extern object *fn_noteq(object *args, object *env);
extern object *fn_numeq(object *args, object *env);
extern object *fn_less(object *args, object *env);
extern object *fn_lesseq(object *args, object *env);
extern object *fn_greater(object *args, object *env);
extern object *fn_greatereq(object *args, object *env);
extern object *fn_plusp(object *args, object *env);
extern object *fn_minusp(object *args, object *env);
extern object *fn_zerop(object *args, object *env);
extern object *fn_oddp(object *args, object *env);
extern object *fn_evenp(object *args, object *env);
extern object *fn_integerp(object *args, object *env);
extern object *fn_numberp(object *args, object *env);
extern object *fn_floatfn(object *args, object *env);
extern object *fn_floatp(object *args, object *env);
extern object *fn_sin(object *args, object *env);
extern object *fn_cos(object *args, object *env);
extern object *fn_tan(object *args, object *env);
extern object *fn_asin(object *args, object *env);
extern object *fn_acos(object *args, object *env);
extern object *fn_atan(object *args, object *env);
extern object *fn_sinh(object *args, object *env);
extern object *fn_cosh(object *args, object *env);
extern object *fn_tanh(object *args, object *env);
extern object *fn_exp(object *args, object *env);
extern object *fn_sqrt(object *args, object *env);
extern object *fn_log(object *args, object *env);
extern int intpower(int base, int exp);
extern object *fn_expt(object *args, object *env);
extern object *fn_ceiling(object *args, object *env);
extern object *fn_floor(object *args, object *env);
extern object *fn_truncate(object *args, object *env);
extern int myround(float number);
extern object *fn_round(object *args, object *env);
extern object *fn_char(object *args, object *env);
extern object *fn_charcode(object *args, object *env);
extern object *fn_codechar(object *args, object *env);
extern object *fn_characterp(object *args, object *env);
extern object *fn_stringp(object *args, object *env);
extern object *fn_stringeq(object *args, object *env);
extern object *fn_stringless(object *args, object *env);
extern object *fn_stringgreater(object *args, object *env);
extern object *fn_sort(object *args, object *env);
extern object *fn_stringfn(object *args, object *env);
extern object *fn_concatenate(object *args, object *env);
extern object *fn_subseq(object *args, object *env);
extern int gstr(void);
extern object *fn_readfromstring(object *args, object *env);
extern void pstr(char c);
extern object *fn_princtostring(object *args, object *env);
extern object *fn_prin1tostring(object *args, object *env);
extern object *fn_logand(object *args, object *env);
extern object *fn_logior(object *args, object *env);
extern object *fn_logxor(object *args, object *env);
extern object *fn_lognot(object *args, object *env);
extern object *fn_ash(object *args, object *env);
extern object *fn_logbitp(object *args, object *env);
extern object *fn_eval(object *args, object *env);
extern object *fn_globals(object *args, object *env);
extern object *fn_locals(object *args, object *env);
extern object *fn_makunbound(object *args, object *env);
extern object *fn_break(object *args, object *env);
extern object *fn_read(object *args, object *env);
extern object *fn_prin1(object *args, object *env);
extern object *fn_print(object *args, object *env);
extern object *fn_princ(object *args, object *env);
extern object *fn_terpri(object *args, object *env);
extern object *fn_readbyte(object *args, object *env);
extern object *fn_readline(object *args, object *env);
extern object *fn_writebyte(object *args, object *env);
extern object *fn_writestring(object *args, object *env);
extern object *fn_writeline(object *args, object *env);
extern object *fn_restarti2c(object *args, object *env);
extern object *fn_gc(object *obj, object *env);
extern object *fn_room(object *args, object *env);
extern object *fn_saveimage(object *args, object *env);
extern object *fn_loadimage(object *args, object *env);
extern object *fn_cls(object *args, object *env);
extern object *fn_pinmode(object *args, object *env);
extern object *fn_digitalread(object *args, object *env);
extern object *fn_digitalwrite(object *args, object *env);
extern object *fn_analogread(object *args, object *env);
extern object *fn_analogwrite(object *args, object *env);
extern object *fn_delay(object *args, object *env);
extern object *fn_millis(object *args, object *env);
extern object *fn_sleep(object *args, object *env);
extern object *fn_note(object *args, object *env);
extern object *fn_edit(object *args, object *env);
extern object *edit(object *fun);
extern void pcount(char c);
extern int atomwidth(object *obj);
extern int subwidth(object *obj, int w);
extern int subwidthlist(object *form, int w);
extern void superprint(object *form, int lm, pfun_t pfun);
extern void supersub(object *form, int lm, int super, pfun_t pfun);
extern object *fn_pprint(object *args, object *env);
extern object *fn_pprintall(object *args, object *env);
extern object *fn_require(object *args, object *env);
extern object *fn_listlibrary(object *args, object *env);
extern object *fn_available(object *args, object *env);
extern object *fn_wifiserver(object *args, object *env);
extern object *fn_wifisoftap(object *args, object *env);
extern object *fn_connected(object *args, object *env);
extern object *fn_wifilocalip(object *args, object *env);
extern object *fn_wificonnect(object *args, object *env);
extern int builtin(char *n);
extern int longsymbol(char *buffer);
extern char *lookupbuiltin(symbol_t name);
extern char *lookupsymbol(symbol_t name);
extern void deletesymbol(symbol_t name);
extern void testescape(void);
extern object *eval(object *form, object *env);
extern void pserial(char c);
extern void pcharacter(char c, pfun_t pfun);
extern void pstring(char *s, pfun_t pfun);
extern void printstring(object *form, pfun_t pfun);
extern void pfstring(const char *s, pfun_t pfun);
extern void pint(int i, pfun_t pfun);
extern void pmantissa(float f, pfun_t pfun);
extern void pfloat(float f, pfun_t pfun);
extern void pfl(pfun_t pfun);
extern void printobject(object *form, pfun_t pfun);
extern int glibrary(void);
extern void loadfromlibrary(object *env);
extern int gserial(void);
extern object *nextitem(gfun_t gfun);
extern object *readrest(gfun_t gfun);
extern object *read(gfun_t gfun);
extern void initenv(void);
extern void setup(void);
extern void repl(object *env);
extern void loop(void);

