{                                                                           }
{ File:       MSVCRT.pas                                                    }
{ Function:   Translations of the functions found in msvcrt.dll, as         }
{             replacement for missing externals when a C object file is     }
{             used in Delphi. That is also the reason why all functions     }
{             have a leading underscore, and functions like _putc() have    }
{             Integer parameters, instead of Char.                          }
{ Language:   Delphi 6                                                      }
{ Author:     Rudolph Velthuis                                              }
{ Versions:   1.0.0 Started with functions in msvcrt10.dll, 1997.           }
{             1.1.0 Added msvcrt.dll v7.0 as dynamic imports.               }
{             1.1.1 Added __fputc(), used by putc() macro.                  }
{             1.1.2 Implemented fgetpos() and fsetpos() because of          }
{                   differences between MSVC and BCB.                       }
{             1.1.3 Added __streams and Streams, because getc() and         }
{                   putc() and the depending getchar() and putchar() are    }
{                   macros which access the level field of stdin or         }
{                   stdout before they call any code.                       }
{             1.1.4 Added __mbctype and MBCType, because _isleadbyte()      }
{                   is a macro which accesses them. InitMbcsTable           }
{                   initializes the table.                                  }
{             1.1.5 Added __lstrxfrm, __lstrcoll, __lsetlocale.             }
{ Copyright:  (c) 2006 drs. Rudolph Velthuis                                }
{ Disclaimer: This code is freeware. All rights are reserved.               }
{             This code is provided as is, expressly without a warranty     }
{             of any kind. You use it at your own risk.                     }
{                                                                           }
{             If you use this code, please credit me.                       }
{                                                                           }

{ TODO: add functions like _ftol(), which are RTL routines compiled by the
        compiler. They are never explicitly called. Find other routines
        like them.
        Note that some of them use unconventional calling, e.g. _ftol()
        passes a float on the FPU stack. }

{ TODO: Perhaps this unit should be renamed, especially if RTL routines are
        also implemented. }

{$ALIGN OFF}      // Because of #pragma pack(push, 1)
{$MINENUMSIZE 4}

unit MSVCRT;

interface

uses Windows, Common2;

type
  ptime_t = ^time_t;
  {$EXTERNALSYM time_t}
  time_t = Longint;

  pdiv_t = ^div_t;
  {$EXTERNALSYM div_t}
  div_t = record
    quot: Integer;
    rem: Integer;
  end;

  pldiv_t = ^ldiv_t;
  {$EXTERNALSYM ldiv_t}
  ldiv_t = record
    quot: Longint;
    rem: Longint;
  end;

  pfpos_t = ^fpos_t;
  {$EXTERNALSYM fpos_t}
  fpos_t = Longint;

  pwchar_t = PWideChar;
  {$EXTERNALSYM wchar_t}
  wchar_t = WideChar;

  pwint_t = pwchar_t;
  {$EXTERNALSYM wint_t}
  wint_t = wchar_t;

  PFILE = Pointer;

  psize_t = ^size_t;
  {$EXTERNALSYM size_t}
  size_t = Cardinal;

  ptm = ^tm;
  {$EXTERNALSYM tm}
  tm = record
    tm_sec: Integer;        // seconds after the minute (from 0)
    tm_min: Integer;        // minutes after the hour (from 0)
    tm_hour: Integer;       // hour of the day (from 0)
    tm_mday: Integer;       // day of the month (from 1)
    tm_mon: Integer;        // month of the year (from 0)
    tm_year: Integer;       // years since 1900 (from 0)
    tm_wday: Integer;       // days since Sunday (from 0)
    tm_yday: Integer;       // day of the year (from 0)
    tm_isdst: Integer;      // Daylight Saving Time flag
  end;

  plconv = ^lconv;
  {$EXTERNALSYM lconv}
  lconv = record
    decimal_point: PChar;
    thousands_sep: PChar;
    grouping: PChar;
    int_curr_symbol: PChar;
    currency_symbol: PChar;
    mon_decimal_point: PChar;
    mon_thousands_sep: PChar;
    mon_grouping: PChar;
    positive_sign: PChar;
    negative_sign: PChar;
    int_frac_digits: Char;
    frac_digits: Char;
    p_cs_precedes: Char;
    p_sep_by_space: Char;
    n_cs_precedes: Char;
    n_sep_by_space: Char;
    p_sign_posn: Char;
    n_sign_posn: Char;
  end;

  {$EXTERNALSYM __jmp_buf}
  __jmp_buf = record
    j_ebp: Cardinal;
    j_ebx: Cardinal;
    j_edi: Cardinal;
    j_esi: Cardinal;
    j_esp: Cardinal;
    j_ret: Cardinal;
    j_excep: Cardinal;
    j_context: Cardinal;
  end;
  {$EXTERNALSYM jmp_buf}
  jmp_buf = ^_jmp_buf;
  {$EXTERNALSYM _jmp_buf}
  _jmp_buf = array[0..0] of __jmp_buf;

  {$EXTERNALSYM va_list}
  va_list = Pointer;

  {$EXTERNALSYM wctype_t}
  wctype_t = wchar_t;

const
  {$EXTERNALSYM WEOF}
  WEOF = wint_t($FFFF);
  {$EXTERNALSYM EOF}
  EOF  = -1;

  {$EXTERNALSYM SEEK_SET}
  SEEK_SET = 0;
  {$EXTERNALSYM SEEK_CUR}
  SEEK_CUR = 1;
  {$EXTERNALSYM SEEK_END}
  SEEK_END = 2;

  {$EXTERNALSYM L_tmpnam}
  L_tmpnam = 13;

{$EXTERNALSYM _abort}
procedure _abort; cdecl;
{$EXTERNALSYM _abs}
function _abs(i: Integer): Integer; cdecl;
{$EXTERNALSYM _acos}
function _acos(x: Double): Double; cdecl;
{$EXTERNALSYM _asctime}
function _asctime(tptr: ptm): PChar; cdecl;
{$EXTERNALSYM _asin}
function _asin(x: Double): Double; cdecl;
{$EXTERNALSYM _atan}
function _atan(x: Double): Double; cdecl;
{$EXTERNALSYM _atan2}
function _atan2(x, y: Double): Double; cdecl;
{$EXTERNALSYM _atexit}
function _atexit(func: Pointer): Integer; cdecl;
{$EXTERNALSYM _atof}
function _atof(const s: PChar): Double; cdecl;
{$EXTERNALSYM _atoi}
function _atoi(const s: PChar): Integer; cdecl;
{$EXTERNALSYM _atol}
function _atol(const s: PChar): Longint; cdecl;

type
  cmp_f = function(ck, ce: Pointer): Integer cdecl;

{$EXTERNALSYM _bsearch}
function _bsearch(key, base: Pointer; nelem, size: Cardinal;
  cmp: cmp_f): Pointer; cdecl;
{$EXTERNALSYM _calloc}
function _calloc(nelem, size: size_t): Pointer; cdecl;
{$EXTERNALSYM _ceil}
function _ceil(x: Double): Double; cdecl;
{$EXTERNALSYM _clearerr}
procedure _clearerr(stream: PFILE); cdecl;
{$EXTERNALSYM _clock}
function _clock: Longint; cdecl;
{$EXTERNALSYM _cos}
function _cos(x: Double): Double; cdecl;
{$EXTERNALSYM _cosh}
function _cosh(x: Double): Double; cdecl;
{$EXTERNALSYM _ctime}
function _ctime(tod: ptime_t): PChar; cdecl;
{$EXTERNALSYM _difftime}
function _difftime(t0, t1: time_t): Integer; cdecl;
{$EXTERNALSYM _div}
function _div(numer, denom: Integer): div_t; cdecl;
{$EXTERNALSYM _exit}
procedure _exit(status: Integer); cdecl;
{$EXTERNALSYM _exp}
function _exp(x: Double): Double; cdecl;
{$EXTERNALSYM _fabs}
function _fabs(x: Double): Double; cdecl;
{$EXTERNALSYM _fclose}
function _fclose(stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _feof}
function _feof(stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _ferror}
function _ferror(stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _fflush}
function _fflush(stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _fgetc}
function _fgetc(stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _fgetpos}
function _fgetpos(stream: PFILE; pos: pfpos_t): Integer; cdecl;
{$EXTERNALSYM _fgets}
function _fgets(s: PChar; n: Integer; stream: PFILE): PChar; cdecl;
{$EXTERNALSYM _fgetwc}
function _fgetwc(stream: PFILE): wint_t; cdecl;
{$EXTERNALSYM _floor}
function _floor(x: Double): Double; cdecl;
{$EXTERNALSYM _fmod}
function _fmod(x, y: Double): Double; cdecl;
{$EXTERNALSYM _fopen}
function _fopen(filename, mode: PChar): PFILE; cdecl;
//{$EXTERNALSYM _fprintf}
//function _fprintf(stream: PFILE; const format: PChar): Integer; cdecl; varargs;
{$EXTERNALSYM _fputc}
function _fputc(c: Integer; stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _fputs}
function _fputs(const s: PChar; stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _fputwc}
function _fputwc(c: wchar_t; stream: PFILE): wint_t; cdecl;
{$EXTERNALSYM _fread}
function _fread(ptr: Pointer; size, nelem: size_t;
  stream: PFILE): size_t; cdecl;
{$EXTERNALSYM _free}
procedure _free(ptr: Pointer); cdecl;
{$EXTERNALSYM _freopen}
function _freopen(const filename, mode: PChar; stream: PFILE): PFILE; cdecl;
{$EXTERNALSYM _frexp}
function _frexp(x: Double; pexp: PInteger): Double; cdecl;
//{$EXTERNALSYM _fscanf}
//function _fscanf(stream: PFILE; const format: PChar): Integer; cdecl; varargs;
{$EXTERNALSYM _fseek}
function _fseek(stream: PFILE; offset: Longint; mode: Integer): Integer; cdecl;
{$EXTERNALSYM _fsetpos}
function _fsetpos(stream: PFILE; pos: pfpos_t): Integer; cdecl;
{$EXTERNALSYM _ftell}
function _ftell(stream: PFILE): Longint; cdecl;
function _ftol():Integer; cdecl;
//{$EXTERNALSYM _fwprintf}
//function _fwprintf(stream: PFILE; format: pwchar_t): Integer; cdecl; varargs;
{$EXTERNALSYM _fwrite}
function _fwrite(ptr: Pointer; size, nelem: size_t;
  stream: PFILE): size_t; cdecl;
//{$EXTERNALSYM _fwscanf}
//function _fwscanf(stream: PFILE; format: pwchar_t): Integer; cdecl; varargs;
{$EXTERNALSYM _getc}
function _getc(stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _getchar}
function _getchar: Integer; cdecl;
{$EXTERNALSYM _getenv}
function _getenv(const name: PChar): PChar; cdecl;
{$EXTERNALSYM _gets}
function _gets(s: PChar): PChar; cdecl;
{$EXTERNALSYM _gmtime}
function _gmtime(const tod: ptime_t): ptm; cdecl;
{$EXTERNALSYM _isalnum}
function _isalnum(c: Integer): LongBool; cdecl;
{$EXTERNALSYM _isalpha}
function _isalpha(c: Integer): LongBool; cdecl;
{$EXTERNALSYM _iscntrl}
function _iscntrl(c: Integer): LongBool; cdecl;
{$EXTERNALSYM _isdigit}
function _isdigit(c: Integer): LongBool; cdecl;
{$EXTERNALSYM _isgraph}
function _isgraph(c: Integer): LongBool; cdecl;
{$EXTERNALSYM _isleadbyte}
function _isleadbyte(c: Integer): LongBool; cdecl;
{$EXTERNALSYM _islower}
function _islower(c: Integer): LongBool; cdecl;
{$EXTERNALSYM _isprint}
function _isprint(c: Integer): LongBool; cdecl;
{$EXTERNALSYM _ispunct}
function _ispunct(c: Integer): LongBool; cdecl;
{$EXTERNALSYM _isspace}
function _isspace(c: Integer): LongBool; cdecl;
{$EXTERNALSYM _isupper}
function _isupper(c: Integer): LongBool; cdecl;
{$EXTERNALSYM _iswalnum}
function _iswalnum(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _iswalpha}
function _iswalpha(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _iswascii}
function _iswascii(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _iswcntrl}
function _iswcntrl(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _iswctype}
function _iswctype(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _iswdigit}
function _iswdigit(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _iswgraph}
function _iswgraph(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _iswlower}
function _iswlower(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _iswprint}
function _iswprint(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _iswpunct}
function _iswpunct(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _iswspace}
function _iswspace(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _iswupper}
function _iswupper(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _iswxdigit}
function _iswxdigit(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _isxdigit}
function _isxdigit(c: Integer): LongBool; cdecl;
{$EXTERNALSYM _labs}
function _labs(i: Longint): Longint; cdecl;
{$EXTERNALSYM _ldexp}
function _ldexp(x: Double; ex: Integer): Double; cdecl;
{$EXTERNALSYM _ldiv}
function _ldiv(numer, denom: LongInt): ldiv_t; cdecl;
{$EXTERNALSYM _localeconv}
function _localeconv: plconv; cdecl;
{$EXTERNALSYM _localtime}
function _localtime(const tod: ptime_t): ptm; cdecl;
{$EXTERNALSYM _log}
function _log(x: Double): Double; cdecl;
{$EXTERNALSYM _log10}
function _log10(x: Double): Double; cdecl;
{$EXTERNALSYM _longjmp}
procedure _longjmp(env: jmp_buf; val: Integer); cdecl;
{$EXTERNALSYM _malloc}
function _malloc(size: size_t): Pointer; cdecl;

{$EXTERNALSYM _mblen}
function _mblen(const s: PChar; n: size_t): Integer; cdecl;
{$EXTERNALSYM _mbstowcs}
function _mbstowcs(wcs: pwchar_t; const s: PChar; n: size_t): size_t; cdecl;
{$EXTERNALSYM _mbtowc}
function _mbtowc(pwc: pwchar_t; const s: PChar; n: size_t): Integer; cdecl;
{$EXTERNALSYM _memchr}
function _memchr(const s: Pointer; c: Integer; n: size_t): Pointer; cdecl;
{$EXTERNALSYM _memcmp}
function _memcmp(const s1, s2: Pointer; n: size_t): Integer; cdecl;
{$EXTERNALSYM _memcpy}
function _memcpy(s1: Pointer; const s2: Pointer; n: size_t): Pointer; cdecl;
{$EXTERNALSYM _memmove}
function _memmove(s1: Pointer; const s2: Pointer; n: size_t): Pointer; cdecl;
{$EXTERNALSYM _memset}
function _memset(s: Pointer; c: Integer; n: size_t): Pointer; cdecl;
{$EXTERNALSYM _mktime}
function _mktime(tptr: ptm): time_t; cdecl;
{$EXTERNALSYM _modf}
function _modf(x: Double; pint: PDouble): Double; cdecl;
{$EXTERNALSYM _perror}
procedure _perror(const s: PChar); cdecl;
{$EXTERNALSYM _pow}
function _pow(x, y: Double): Double; cdecl;
//{$EXTERNALSYM _printf}
//function _printf(const format: PChar): Integer; cdecl; varargs;
{$EXTERNALSYM _putc}
function _putc(c: Integer; stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _putchar}
function _putchar(c: Integer): Integer; cdecl;
{$EXTERNALSYM _puts}
function _puts(const s: PChar): Integer; cdecl;

type
  qsortcmp = function(const e1, e2: Pointer): Integer cdecl;

{$EXTERNALSYM _qsort}
function _qsort(base: Pointer; nelem, size: size_t;
  cmp: qsortcmp): Integer; cdecl;
{$EXTERNALSYM _raise}
function _raise(sig: Integer): Integer; cdecl;
{$EXTERNALSYM _rand}
function _rand: Integer; cdecl;
{$EXTERNALSYM _realloc}
function _realloc(ptr: Pointer; size: size_t): Pointer; cdecl;
{$EXTERNALSYM _remove}
function _remove(const filename: PChar): Integer; cdecl;
{$EXTERNALSYM _rename}
function _rename(const old, new: PChar): Integer; cdecl;
{$EXTERNALSYM _rewind}
procedure _rewind(stream: PFILE); cdecl;
//{$EXTERNALSYM _scanf}
//function _scanf(const format: PChar): Integer; cdecl; varargs;
{$EXTERNALSYM _setbuf}
procedure _setbuf(stream: PFILE; buf: PChar); cdecl;
{$EXTERNALSYM _setlocale}
function _setlocale(category: Integer; const locname: PChar): PChar; cdecl;
{$EXTERNALSYM _setvbuf}
function _setvbuf(stream: PFILE; buf: PChar; mode: Integer;
  size: size_t): Integer; cdecl;
function _setjmp(env: jmp_buf):Integer;cdecl;

type
  signal_f = procedure(param: Integer) cdecl;

{$EXTERNALSYM _signal}
function _signal(sig: Integer; func: signal_f): signal_f; cdecl;
{$EXTERNALSYM _sin}
function _sin(x: Double): Double; cdecl;
{$EXTERNALSYM _sinh}
function _sinh(x: Double): Double; cdecl;
//{$EXTERNALSYM _sprintf}
//function _sprintf(s: PChar; const format: PChar): Integer; cdecl; varargs;
{$EXTERNALSYM _sqrt}
function _sqrt(x: Double): Double; cdecl;
{$EXTERNALSYM _srand}
procedure _srand(seed: Cardinal); cdecl;
//{$EXTERNALSYM _sscanf}
//function _sscanf(const s, format: PChar): Integer; cdecl; varargs;
{$EXTERNALSYM _strcat}
function _strcat(s1: PChar; const s2: PChar): PChar; cdecl;
{$EXTERNALSYM _strchr}
function _strchr(const s: PChar; c: Integer): PChar; cdecl;
{$EXTERNALSYM _strcmp}
function _strcmp(const s1, s2: PChar): Integer; cdecl;
{$EXTERNALSYM _strcoll}
function _strcoll(const s1, s2: PChar): Integer; cdecl;
{$EXTERNALSYM _strcpy}
function _strcpy(s1: PChar; const s2: PChar): PChar; cdecl;
{$EXTERNALSYM _strcspn}
function _strcspn(const s1, s2: PChar): size_t; cdecl;
{$EXTERNALSYM _strerror}
function _strerror(errcode: Integer): PChar; cdecl;
{$EXTERNALSYM _strftime}
function _strftime(s: PChar; n: size_t; const format: PChar;
  const tptr: Ptm): size_t; cdecl;
{$EXTERNALSYM _strlen}
function _strlen(const s: PChar): size_t; cdecl;
{$EXTERNALSYM _strncat}
function _strncat(s1: PChar; const s2: PChar; n: size_t): PChar; cdecl;
{$EXTERNALSYM _strncmp}
function _strncmp(const s1, s2: PChar; n: size_t): Integer; cdecl;
{$EXTERNALSYM _strncpy}
function _strncpy(s1: PChar; const s2: PChar; n: size_t): PChar; cdecl;
{$EXTERNALSYM _strpbrk}
function _strpbrk(const s1, s2: PChar): PChar; cdecl;
{$EXTERNALSYM _strrchr}
function _strrchr(const s: PChar; c: Integer): PChar; cdecl;
{$EXTERNALSYM _strspn}
function _strspn(const s1, s2: PChar): size_t; cdecl;
{$EXTERNALSYM _strstr}
function _strstr(const s1, s2: PChar): PChar; cdecl;
{$EXTERNALSYM _strtod}
function _strtod(const s: PChar; var endptr: PChar): Double; cdecl;
{$EXTERNALSYM _strtok}
function _strtok(s1: PChar; const s2: PChar): PChar; cdecl;
{$EXTERNALSYM _strtol}
function _strtol(const s: PChar; endptr: PPChar;
  base: Integer): Longint; cdecl;
{$EXTERNALSYM _strtoul}
function _strtoul(const s: PChar; var endptr: PChar;
  base: Integer): Longword; cdecl;
{$EXTERNALSYM _strxfrm}
function _strxfrm(s1: PChar; const s2: PChar; n: size_t): size_t; cdecl;
//{$EXTERNALSYM _swprintf}
//function _swprintf(s: pwchar_t; n: size_t;
//  const format: pwchar_t): Integer; cdecl; varargs;
//{$EXTERNALSYM _swscanf}
//function _swscanf(const s, format: pwchar_t): Integer; cdecl; varargs;
{$EXTERNALSYM _system}
function _system(const s: PChar): Integer; cdecl;
{$EXTERNALSYM _tan}
function _tan(x: Double): Double; cdecl;
{$EXTERNALSYM _tanh}
function _tanh(x: Double): Double; cdecl;
{$EXTERNALSYM _time}
function _time(tod: ptime_t): time_t; cdecl;
{$EXTERNALSYM _tmpfile}
function _tmpfile: PFILE; cdecl;
{$EXTERNALSYM _tmpnam}
function _tmpnam(s: PChar): PChar; cdecl;
{$EXTERNALSYM _tolower}
function _tolower(c: Integer): Integer; cdecl;
{$EXTERNALSYM _toupper}
function _toupper(c: Integer): Integer; cdecl;
{$EXTERNALSYM _towlower}
function _towlower(c: wint_t): wint_t; cdecl;
{$EXTERNALSYM _towupper}
function _towupper(c: wint_t): wint_t; cdecl;
{$EXTERNALSYM _ungetc}
function _ungetc(c: Integer; stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _ungetwc}
function _ungetwc(c: wint_t; stream: PFILE): wint_t; cdecl;
{$EXTERNALSYM _vfprintf}
function _vfprintf(stream: PFILE; const format: PChar;
  ap: va_list): Integer; cdecl;
{$EXTERNALSYM _vfwprintf}
function _vfwprintf(stream: PFILE; const format: pwchar_t;
  arg: va_list): Integer; cdecl;
{$EXTERNALSYM _vprintf}
function _vprintf(const format: PChar; ap: va_list): Integer; cdecl;
{$EXTERNALSYM _vsprintf}
function _vsprintf(s: PChar; const format: PChar; ap: va_list): Integer; cdecl;
{$EXTERNALSYM _vswprintf}
function _vswprintf(s: pwchar_t; n: size_t; const format: pwchar_t;
  arg: va_list): Integer; cdecl;
{$EXTERNALSYM _vwprintf}
function _vwprintf(const format: pwchar_t; arg: va_list): Integer; cdecl;
{$EXTERNALSYM _wcscat}
function _wcscat(s1: pwchar_t; const s2: pwchar_t): pwchar_t; cdecl;
{$EXTERNALSYM _wcschr}
function _wcschr(const s: pwchar_t; c: wchar_t): wchar_t; cdecl;
{$EXTERNALSYM _wcscmp}
function _wcscmp(const s1, s2: pwchar_t): Integer; cdecl;
{$EXTERNALSYM _wcscoll}
function _wcscoll(const s1, s2: pwchar_t): Integer; cdecl;
{$EXTERNALSYM _wcscpy}
function _wcscpy(s1: pwchar_t; const s2: pwchar_t): pwchar_t; cdecl;
{$EXTERNALSYM _wcscspn}
function _wcscspn(const s1, s2: pwchar_t): size_t; cdecl;
{$EXTERNALSYM _wcsftime}
function _wcsftime(s: pwchar_t; maxsize: size_t; const format: pwchar_t;
  const timeptr: ptm): size_t; cdecl;
{$EXTERNALSYM _wcslen}
function _wcslen(const s: pwchar_t): size_t; cdecl;
{$EXTERNALSYM _wcsncat}
function _wcsncat(s1: pwchar_t; const s2: pwchar_t; n: size_t): pwchar_t; cdecl;
{$EXTERNALSYM _wcsncmp}
function _wcsncmp(const s1, s2: pwchar_t; n: size_t): Integer; cdecl;
{$EXTERNALSYM _wcsncpy}
function _wcsncpy(s1: pwchar_t; const s2: pwchar_t; n: size_t): pwchar_t; cdecl;
{$EXTERNALSYM _wcspbrk}
function _wcspbrk(const s1, s2: pwchar_t): pwchar_t; cdecl;
{$EXTERNALSYM _wcsrchr}
function _wcsrchr(const s: pwchar_t; c: wchar_t): pwchar_t; cdecl;
{$EXTERNALSYM _wcsspn}
function _wcsspn(const s1, s2: pwchar_t): size_t; cdecl;
{$EXTERNALSYM _wcsstr}
function _wcsstr(const s1, s2: pwchar_t): pwchar_t; cdecl;
{$EXTERNALSYM _wcstod}
function _wcstod(const nptr: pwchar_t; var endptr: pwchar_t): Double; cdecl;
{$EXTERNALSYM _wcstok}
function _wcstok(s1: pwchar_t; const s2: pwchar_t;
  var ptr: pwchar_t): pwchar_t; cdecl;
{$EXTERNALSYM _wcstol}
function _wcstol(const nptr: pwchar_t; var endptr: pwchar_t;
  base: Integer): Longint; cdecl;
{$EXTERNALSYM _wcstombs}
function _wcstombs(const nptr: pwchar_t; var endptr: pwchar_t;
  base: Integer): size_t; cdecl;
{$EXTERNALSYM _wcstoul}
function _wcstoul(const nptr: pwchar_t; var endptr: pwchar_t;
  base: Integer): Longint; cdecl;
{$EXTERNALSYM _wcsxfrm}
function _wcsxfrm(s1: pwchar_t; const s2: pwchar_t; n: size_t): size_t; cdecl;
{$EXTERNALSYM _wctomb}
function _wctomb(s: PChar; wchar: wchar_t): Integer; cdecl;
//{$EXTERNALSYM _wprintf}
//function _wprintf(const format: pwchar_t): Integer; cdecl; varargs;

// Routines required by BCB functions implemented as macros:

{$EXTERNALSYM __fgetc}
function __fgetc(stream: PFILE): Integer; cdecl;
//{$EXTERNALSYM __fputc}
//function __fputc(c: Integer; f: PFILE): Integer; cdecl;
{$EXTERNALSYM __ltolower}
function __ltolower(c: Longint): Longint; cdecl;
{$EXTERNALSYM __ltoupper}
function __ltoupper(c: Longint): Longint; cdecl;
{$EXTERNALSYM __lsetlocale}
function __lsetlocale(category: Integer; const locname: PChar): PChar; cdecl;
{$EXTERNALSYM __lstrcoll}
function __lstrcoll(const s1, s2: PChar): Integer; cdecl;
{$EXTERNALSYM __lstrxfrm}
function __lstrxfrm(s1: PChar; const s2: PChar; size: size_t): size_t; cdecl;

// Additional routines in msvcrt.dll 7.0.2600.2180

{$EXTERNALSYM _fgetws}
function _fgetws(s: pwchar_t; n: Integer; stream: PFILE): pwchar_t; cdecl;
{$EXTERNALSYM _fputws}
function _fputws(const s: pwchar_t; stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _getwc}
function _getwc(stream: PFILE): wint_t; cdecl;
//{$EXTERNALSYM _getwchar}
//function _getwchar: wint_t; cdecl;
{$EXTERNALSYM _is_wctype}
function _is_wctype(c: wint_t): LongBool; cdecl;
{$EXTERNALSYM _putwc}
function _putwc(c: wchar_t; stream: PFILE): wint_t; cdecl;
//{$EXTERNALSYM _putwchar}
//function _putwchar(c: wchar_t): wint_t; cdecl;

// Some extras:

{$EXTERNALSYM _lock_file}
procedure _lock_file(stream: PFILE); cdecl;
{$EXTERNALSYM _unlock_file}
procedure _unlock_file(stream: PFILE); cdecl;

// These are not set by the msvc runtime, of course, but they are checked
// by some macros in stdio.h, especially the level field. That is why
// you see them here, and why they are filled with #$FF bytes, to
// make sure level is never positive.

// This does mean that ungetc() won't work properly, at least if that is
// defined as a macro. It might work if it is a proper function.

// Anyway, macros are evil, evil, evil.

type
  TFILE = record
    curp: PByte;        (* Current active pointer     *)
    buffer: PByte;      (* Data transfer buffer       *)
    level: Integer;     (* fill/empty level of buffer *)
    bsize: Integer;     (* Buffer size                *)
    istemp: WordBool;   (* Temporary file indicator   *)
    flags: Word;        (* File status flags          *)
    hold: wchar_t;      (* Ungetc char if no buffer   *)
    fd: Char;           (* File descriptor            *)
    token: Byte;        (* Used for validity checking *)
  end;

// For some reason or other, you can't simply declare an array.
// you must declare a pointer to the array, and that is what
// the object file uses.

var
  // reference generated by BCC32 when address of floating point
  // is used
  __turboFloat: LongBool;
  _errno: Integer;
  __fltused: Integer;

  // Accessed by getchar() macro.
  Streams: array[0..50] of TFILE; // one too many, I know.

  // Accessed by _mbblead() macro, when isleadbyte() is used.
  MBCType: array[-1..255] of Byte; // -1 is EOF.

const
  __streams: PFILE = @Streams;
  __mbctype: Pointer = @MBCType;

implementation

const
  msvcrtdll = 'msvcrt.dll';
  FLeadByte = 4;

procedure InitMbcsTable;
var
  I: Integer;
begin
  for I := -1 to 255 do
    if _isleadbyte(I) then
      MBCType[I] := MBCType[I] or FLeadByte;
end;

procedure _abort; external msvcrtdll name 'abort';
function _abs; external msvcrtdll name 'abs';
function _acos; external msvcrtdll name 'acos';
function _asctime; external msvcrtdll name 'asctime';
function _asin; external msvcrtdll name 'asin';
function _atan; external msvcrtdll name 'atan';
function _atan2; external msvcrtdll name 'atan2';
function _atexit; external msvcrtdll name 'atexit';
function _atof; external msvcrtdll name 'atof';
function _atoi; external msvcrtdll name 'atoi';
function _atol; external msvcrtdll name 'atol';
function _bsearch; external msvcrtdll name 'bsearch';
function _ceil; external msvcrtdll name 'ceil';
procedure _clearerr; external msvcrtdll name 'clearerr';
function _clock; external msvcrtdll name 'clock';
function _cos; external msvcrtdll name 'cos';
function _cosh; external msvcrtdll name 'cosh';
function _ctime; external msvcrtdll name 'ctime';
function _difftime; external msvcrtdll name 'difftime';
procedure _exit; external msvcrtdll name 'exit';
function _exp; external msvcrtdll name 'exp';
function _fabs; external msvcrtdll name 'fabs';
function _fclose; external msvcrtdll name 'fclose';
function _feof; external msvcrtdll name 'feof';
function _ferror; external msvcrtdll name 'ferror';
function _fflush; external msvcrtdll name 'fflush';
function _fgets; external msvcrtdll name 'fgets';
function _fgetwc; external msvcrtdll name 'fgetwc';
function _floor; external msvcrtdll name 'floor';
function _fmod; external msvcrtdll name 'fmod';
function _fopen; external msvcrtdll name 'fopen';
//function _fprintf; external msvcrtdll name 'fprintf';
function _fputc; external msvcrtdll name 'fputc';
function _fputs; external msvcrtdll name 'fputs';
function _fputwc; external msvcrtdll name 'fputwc';
function _fread; external msvcrtdll name 'fread';
function _freopen; external msvcrtdll name 'freopen';
function _frexp; external msvcrtdll name 'frexp';
//function _fscanf; external msvcrtdll name 'fscanf';
function _fseek; external msvcrtdll name 'fseek';
function _ftell; external msvcrtdll name 'ftell';
function _ftol; external msvcrtdll name '_ftol';
//function _fwprintf; external msvcrtdll name 'fwprintf';
function _fwrite; external msvcrtdll name 'fwrite';
//function _fwscanf; external msvcrtdll name 'fwscanf';
function _getc; external msvcrtdll name 'getc';
function _getchar; external msvcrtdll name 'getchar';
function _getenv; external msvcrtdll name 'getenv';
function _gets; external msvcrtdll name 'gets';
function _gmtime; external msvcrtdll name 'gmtime';
function _isalnum; external msvcrtdll name 'isalnum';
function _isalpha; external msvcrtdll name 'isalpha';
function _iscntrl; external msvcrtdll name 'iscntrl';
function _isdigit; external msvcrtdll name 'isdigit';
function _isgraph; external msvcrtdll name 'isgraph';
function _isleadbyte; external msvcrtdll name 'isleadbyte';
function _islower; external msvcrtdll name 'islower';
function _isprint; external msvcrtdll name 'isprint';
function _ispunct; external msvcrtdll name 'ispunct';
function _isspace; external msvcrtdll name 'isspace';
function _isupper; external msvcrtdll name 'isupper';
function _iswalnum; external msvcrtdll name 'iswalnum';
function _iswalpha; external msvcrtdll name 'iswalpha';
function _iswascii; external msvcrtdll name 'iswascii';
function _iswcntrl; external msvcrtdll name 'iswcntrl';
function _iswctype; external msvcrtdll name 'iswctype';
function _iswdigit; external msvcrtdll name 'iswdigit';
function _iswgraph; external msvcrtdll name 'iswgraph';
function _iswlower; external msvcrtdll name 'iswlower';
function _iswprint; external msvcrtdll name 'iswprint';
function _iswpunct; external msvcrtdll name 'iswpunct';
function _iswspace; external msvcrtdll name 'iswspace';
function _iswupper; external msvcrtdll name 'iswupper';
function _iswxdigit; external msvcrtdll name 'iswxdigit';
function _isxdigit; external msvcrtdll name 'isxdigit';
function _labs; external msvcrtdll name 'labs';
function _ldexp; external msvcrtdll name 'ldexp';
function _localeconv; external msvcrtdll name 'localeconv';
function _localtime; external msvcrtdll name 'localtime';
function _log; external msvcrtdll name 'log';
function _log10; external msvcrtdll name 'log10';
procedure _longjmp; external msvcrtdll name 'longjmp';
function _setjmp; external msvcrtdll name 'setjmp';
function _mblen; external msvcrtdll name 'mblen';
function _mbstowcs; external msvcrtdll name 'mbstowcs';
function _mbtowc; external msvcrtdll name 'mbtowc';
function _memchr; external msvcrtdll name 'memchr';
function _memcmp; external msvcrtdll name 'memcmp';
function _memcpy; external msvcrtdll name 'memcpy';
function _memmove; external msvcrtdll name 'memmove';
function _memset; external msvcrtdll name 'memset';
function _mktime; external msvcrtdll name 'mktime';
function _modf; external msvcrtdll name 'modf';
procedure _perror; external msvcrtdll name 'perror';
function _pow; external msvcrtdll name 'pow';
//function _printf; external msvcrtdll name 'printf';
function _putc; external msvcrtdll name 'putc';
function _putchar; external msvcrtdll name 'putchar';
function _puts; external msvcrtdll name 'puts';
function _qsort; external msvcrtdll name 'qsort';
function _raise; external msvcrtdll name 'raise';
function _rand; external msvcrtdll name 'rand';
function _remove; external msvcrtdll name 'remove';
function _rename; external msvcrtdll name 'rename';
procedure _rewind; external msvcrtdll name 'rewind';
//function _scanf; external msvcrtdll name 'scanf';
procedure _setbuf; external msvcrtdll name 'setbuf';
function _setlocale; external msvcrtdll name 'setlocale';
function _setvbuf; external msvcrtdll name 'setvbuf';
function _signal; external msvcrtdll name 'signal';
function _sin; external msvcrtdll name 'sin';
function _sinh; external msvcrtdll name 'sinh';
//function _sprintf; external msvcrtdll name 'sprintf';
function _sqrt; external msvcrtdll name 'sqrt';
procedure _srand; external msvcrtdll name 'srand';
//function _sscanf; external msvcrtdll name 'sscanf';
function _strcat; external msvcrtdll name 'strcat';
function _strchr; external msvcrtdll name 'strchr';
function _strcmp; external msvcrtdll name 'strcmp';
function _strcoll; external msvcrtdll name 'strcoll';
function _strcpy; external msvcrtdll name 'strcpy';
function _strcspn; external msvcrtdll name 'strcspn';
function _strerror; external msvcrtdll name 'strerror';
function _strftime; external msvcrtdll name 'strftime';
function _strlen; external msvcrtdll name 'strlen';
function _strncat; external msvcrtdll name 'strncat';
function _strncmp; external msvcrtdll name 'strncmp';
function _strncpy; external msvcrtdll name 'strncpy';
function _strpbrk; external msvcrtdll name 'strpbrk';
function _strrchr; external msvcrtdll name 'strrchr';
function _strspn; external msvcrtdll name 'strspn';
function _strstr; external msvcrtdll name 'strstr';
function _strtod; external msvcrtdll name 'strtod';
function _strtok; external msvcrtdll name 'strtok';
function _strtol; external msvcrtdll name 'strtol';
function _strtoul; external msvcrtdll name 'strtoul';
function _strxfrm; external msvcrtdll name 'strxfrm';
//function _swprintf; external msvcrtdll name 'swprintf';
//function _swscanf; external msvcrtdll name 'swscanf';
function _system; external msvcrtdll name 'system';
function _tan; external msvcrtdll name 'tan';
function _tanh; external msvcrtdll name 'tanh';
function _time; external msvcrtdll name 'time';
function _tmpfile; external msvcrtdll name 'tmpfile';
function _tmpnam; external msvcrtdll name 'tmpnam';
function _tolower; external msvcrtdll name 'tolower';
function _toupper; external msvcrtdll name 'toupper';
function _towlower; external msvcrtdll name 'towlower';
function _towupper; external msvcrtdll name 'towupper';
function _ungetc; external msvcrtdll name 'ungetc';
function _ungetwc; external msvcrtdll name 'ungetwc';
function _vfprintf; external msvcrtdll name 'vfprintf';
function _vfwprintf; external msvcrtdll name 'vfwprintf';
function _vprintf; external msvcrtdll name 'vprintf';
function _vsprintf; external msvcrtdll name 'vsprintf';
function _vswprintf; external msvcrtdll name 'vswprintf';
function _vwprintf; external msvcrtdll name 'vwprintf';
function _wcscat; external msvcrtdll name 'wcscat';
function _wcschr; external msvcrtdll name 'wcschr';
function _wcscmp; external msvcrtdll name 'wcscmp';
function _wcscoll; external msvcrtdll name 'wcscoll';
function _wcscpy; external msvcrtdll name 'wcscpy';
function _wcscspn; external msvcrtdll name 'wcscspn';
function _wcsftime; external msvcrtdll name 'wcsftime';
function _wcslen; external msvcrtdll name 'wcslen';
function _wcsncat; external msvcrtdll name 'wcsncat';
function _wcsncmp; external msvcrtdll name 'wcsncmp';
function _wcsncpy; external msvcrtdll name 'wcsncpy';
function _wcspbrk; external msvcrtdll name 'wcspbrk';
function _wcsrchr; external msvcrtdll name 'wcsrchr';
function _wcsspn; external msvcrtdll name 'wcsspn';
function _wcsstr; external msvcrtdll name 'wcsstr';
function _wcstod; external msvcrtdll name 'wcstod';
function _wcstok; external msvcrtdll name 'wcstok';
function _wcstol; external msvcrtdll name 'wcstol';
function _wcstombs; external msvcrtdll name 'wcstombs';
function _wcstoul; external msvcrtdll name 'wcstoul';
function _wcsxfrm; external msvcrtdll name 'wcsxfrm';
function _wctomb; external msvcrtdll name 'wctomb';
//function _wprintf; external msvcrtdll name 'wprintf';

procedure _lock_file; external msvcrtdll name '_lock_file';
procedure _unlock_file; external msvcrtdll name '_unlock_file';

// Functions allocating or freeing memory should use the Delphi memory manager.

function _calloc(nelem, size: size_t): Pointer; cdecl;
begin
  GetMem(Result, nelem * size);
end;

procedure _free(ptr: Pointer); cdecl;
begin
  FreeMem(ptr);
end;

function _malloc(size: size_t): Pointer; cdecl;
begin
  GetMem(Result, size);
end;

function _realloc(ptr: Pointer; size: size_t): Pointer; cdecl;
begin
  ReallocMem(ptr, size);
  Result := ptr;
end;


// Functions returning struct may not use the same return mechanism as in MSVC.
// Better to implement them ourselves.

function _div(numer, denom: Integer): div_t; cdecl;
begin
  Result.quot := numer div denom;
  Result.rem := numer - denom * Result.quot;
  if (Result.quot < 0) and (Result.rem <> 0) then
  begin
    Inc(Result.quot);
    Dec(Result.rem, denom);
  end;
end;

function _ldiv(numer, denom: Longint): ldiv_t; cdecl;
begin
  Result.quot := numer div denom;
  Result.rem := numer - denom * Result.quot;
  if (Result.quot < 0) and (Result.rem <> 0) then
  begin
    Inc(Result.quot);
    Dec(Result.rem, denom);
  end;
end;

// In msvcrt.dll, fpos_t is not just a long int, so errors when using it.
// Here, ftell() and fseek() are used to emulate fgetpos() and fsetpos().

function _fgetpos(stream: PFILE; pos: pfpos_t): Integer; cdecl;
begin
  pos^ := _ftell(stream);
  Result := _ferror(stream);
end;

function _fsetpos(stream: PFILE; pos: pfpos_t): Integer; cdecl;
begin
  Result := _fseek(stream, pos^, SEEK_SET);
end;

// Routines required by some macro implementations:

{function __fputc(c: Integer; f: PFILE): Integer; cdecl;
begin
  if f = @Streams[1] then
  begin
    Write(Output, Chr(C));
    Result := 0;
  end
  else if (f = @Streams[2]) then
  begin
    Write(ErrOutput, Chr(C));
    Result := 0;
  end
  else
    Result := _fputc(c, f);
end;}

function __ltoupper(c: Longint): Longint; cdecl;
begin
  Result := _toupper(c);
end;

function __ltolower(c: Longint): Longint; cdecl;
begin
  Result := _tolower(c);
end;

function __lsetlocale(category: Integer; const locname: PChar): PChar; cdecl;
begin
  InitMbcsTable;
  Result := _setlocale(category, locname);
end;

function __lstrcoll(const s1, s2: PChar): Integer; cdecl;
begin
  Result := _strcoll(s1, s2);
end;

function __lstrxfrm(s1: PChar; const s2: PChar; size: size_t): size_t; cdecl;
begin
  Result := _strxfrm(s1, s2, size);
end;

function fgetc(stream: PFILE): Integer; cdecl;
  external msvcrtdll name 'fgetc';

function _fgetc(stream: PFILE): Integer; cdecl;
begin
  Result := fgetc(stream);
end;

function __fgetc(stream: PFILE): Integer; cdecl;
var
  C: Char;
begin
  // Detect stdin, stdout or stderr.
  // Make this more sophisticated, just a first test.
  if stream = @Streams[0] then
  begin
    Readln(C);
    Result := Ord(C);
  end
  else
    Result := _fgetc(stream);
end;

// Routines not implemented in all versions of msvcrt.dll

function fgetws(s: pwchar_t; n: Integer; stream: PFILE): pwchar_t; cdecl;
var
  t: pwchar_t;
  wc: wint_t;
begin
  if n <= 1 then
  begin
    Result := nil;
    Exit;
  end;
  _lock_file(stream);
  try
    t := s;
    Dec(n);
    while n > 0 do
    begin
      wc := _fgetwc(stream);
      if wc = WEOF then
        break;
      t^ := wc;
      Inc(t);
      if wc = ^M then
        break;
      Dec(n);
    end;
    if t = s then
      s := nil
    else
      t^ := #0;
  finally
    _unlock_file(stream);
  end;
  Result := s;
end;

function fputws(const s: pwchar_t; stream: PFILE): Integer; cdecl;
var
  t: pwchar_t;
begin
  t := s;
  while t^ <> #0000 do
  begin
    if _fputwc(t^, stream) = WEOF then
    begin
      Result := EOF;
      Exit;
    end;
    Inc(t);
  end;
  Result := 0;
end;

function getwc(stream: PFILE): wint_t; cdecl;
begin
  Result := _fgetwc(stream);
end;

{function getwchar: wint_t; cdecl;
begin
  Read(Input, Result);
end;}

function is_wctype(c: wint_t): LongBool; cdecl;
begin
  Result := _iswctype(c);
end;

function putwc(c: wchar_t; stream: PFILE): wint_t; cdecl;
begin
  Result := _fputwc(c, stream);
end;

{function putwchar(c: wchar_t): wint_t; cdecl;
begin
  // No access to msvcrt's stdout, so this routine uses Delphi's
  // Write. No file-position indicator to be updated anyway.
  Write(Output, c);
  Result := wint_t(c);
end;}

var
  Module: THandle;

function SafeGetProcAddress(const ModuleName, ProcName: string; Default: Pointer): TFarProc;
begin
  if Module = 0 then
    Module := LoadLibrary(PChar(ModuleName));
  Result := GetProcAddress(Module, PChar(ProcName));
  if not Assigned(Result) then
  begin
    Result := Default;
{$IFDEF SFGPA_DEBUG}
    Writeln(msvcrtdll, ': ', ProcName, ' replaced');
{$ENDIF}
  end;
end;

var
  int_fgetws: function(s: pwchar_t; n: Integer; stream: PFILE): pwchar_t cdecl;
  int_fputws: function(const s: pwchar_t; stream: PFILE): Integer; cdecl;
  int_getwc: function(stream: PFILE): wint_t cdecl;
//  int_getwchar: function: wint_t cdecl;
  int_is_wctype: function(c: wint_t): LongBool; cdecl;
  int_putwc: function(c: wchar_t; stream: PFILE): wint_t; cdecl;
//  int_putwchar: function(c: wchar_t): wint_t; cdecl;

function _fgetws(s: pwchar_t; n: Integer; stream: PFILE): pwchar_t; cdecl;
begin
  if not Assigned(int_fgetws) then
    int_fgetws := SafeGetProcAddress(msvcrtdll, 'fgetws', @fgetws);
  Result := int_fgetws(s, n, stream);
end;

function _fputws(const s: pwchar_t; stream: PFILE): Integer; cdecl;
begin
  if not Assigned(int_fputws) then
    int_fputws := SafeGetProcAddress(msvcrtdll, 'fputws', @fputws);
  Result := int_fputws(s, stream);
end;

function _getwc(stream: PFILE): wint_t; cdecl;
begin
  if not Assigned(int_getwc) then
    int_getwc := SafeGetProcAddress(msvcrtdll, 'getwc', @getwc);
  Result := int_getwc(stream);
end;

{function _getwchar: wint_t; cdecl;
begin
  if not Assigned(int_getwchar) then
    int_getwc := SafeGetProcAddress(msvcrtdll, 'getwchar', @getwchar);
  Result := int_getwchar;
end;}

function _is_wctype(c: wint_t): LongBool; cdecl;
begin
  if not Assigned(int_is_wctype) then
    int_is_wctype := SafeGetProcAddress(msvcrtdll, 'is_wctype', @is_wctype);
  Result := int_is_wctype(c);
end;

function _putwc(c: wchar_t; stream: PFILE): wint_t; cdecl;
begin
  if not Assigned(int_putwc) then
    SafeGetProcAddress(msvcrtdll, 'putwc', @putwc);
  Result := int_putwc(c, stream);
end;

{function _putwchar(c: wchar_t): wint_t; cdecl;
begin
  Result := putwchar(c);
end;}

type
  TByteArray = array[0..256*2] of Byte;
  PByteArray = ^TByteArray;



initialization
  FillChar(Streams, SizeOf(Streams), #$FF);
  InitMbcsTable;

end.
