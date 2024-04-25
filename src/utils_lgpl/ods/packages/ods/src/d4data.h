/* d4data.h   (c)Copyright Sequiter Software Inc., 1990-2024.  All rights reserved. */

#ifndef S4MDX
#ifndef S4FOX
#ifndef S4CLIPPER
#ifndef S4NDX
   #ifdef S4UNIX
      error - Must compile with one of the indexing options (S4MDX, S4FOX, S4CLIPPER, OR S4NDX)
   #else
      #error - Must compile with one of the indexing options (S4MDX, S4FOX, S4CLIPPER, OR S4NDX)
   #endif
#endif
#endif
#endif
#endif

extern char v4buffer[257] ;
   
#ifdef S4VBASIC
#ifdef S4WINDOWS
   #define WINVER 0x0300
#endif
#endif

#ifdef S4MDX
#ifdef S4FOX
   #ifdef S4UNIX
      error - Both S4MDX and S4FOX switches set - only one is allowed.
   #else
      #error - Both S4MDX and S4FOX switches set - only one is allowed.
   #endif
#endif
#ifdef S4CLIPPER
   #ifdef S4UNIX
      error - Both S4MDX and S4CLIPPER switches set - only one is allowed.
   #else
      #error - Both S4MDX and S4CLIPPER switches set - only one is allowed.
   #endif
#endif
#ifdef S4NDX
   #ifdef S4UNIX
      error - Both S4MDX and S4NDX switches set - only one is allowed.
   #else
      #error - Both S4MDX and S4NDX switches set - only one is allowed.
   #endif
#endif
#endif

#ifdef S4FOX
#ifdef S4CLIPPER
   #ifdef S4UNIX
      error - Both S4FOX and S4CLIPPER switches set - only one is allowed.
   #else
      #error - Both S4FOX and S4CLIPPER switches set - only one is allowed.
   #endif
#endif
#ifdef S4NDX
   #ifdef S4UNIX
      error - Both S4FOX and S4NDX switches set - only one is allowed.
   #else
      #error - Both S4FOX and S4NDX switches set - only one is allowed.
   #endif
#endif
#endif

#ifdef S4CLIPPER
#ifdef S4NDX
   #ifdef S4UNIX
      error - Both S4CLIPPER and S4NDX switches set - only one is allowed.
   #else
      #error - Both S4CLIPPER and S4NDX switches set - only one is allowed.
   #endif
#endif
#endif
/*   OS2 2.0 SUPPORT */
#ifdef __OS2__       /* Watcom 386, Borland C++ for OS/2 */
   #define S4OS2
#endif
#ifdef __IBMC__  /* IBM C SET/2 */
   #define S4OS2
#endif

#ifdef __OS2__
   #ifndef S4OS2
      #define S4OS2
   #endif
   typedef unsigned HANDLE;
   #if __BORLANDC__ == 0x400
      #define S4MEMCMP
   #endif
#endif

#ifdef S4OS2
#ifndef __WATCOMC__
   #define INCL_DOSSEMAPHORES
   #ifndef S4OS2PM
      #define INCL_NOPMAPI    /* don't include PM.H */
   #endif
   #include <os2.h>
#endif
#endif

#ifndef S4OS2
#ifndef S4UNIX
#ifndef _SIZE_T_DEFINED_     /* Watcom */
#ifndef _SIZE_T_DEFINED      /* Microsoft, Metaware */
#ifndef _SIZE_T              /* Borland  */
   typedef unsigned size_t ;
   #define _SIZE_T_DEFINED_           /* Used to resolve Watcom 386 warning */
   #define _SIZE_T
#endif
#endif
#endif
#endif
#endif

#ifndef _A_RDONLY
   #define _A_RDONLY 0x01                  /* Read-only attribute  */
#endif

#ifdef __HIGHC__           /* align structures to word alignment, Metaware */
  #pragma Align_members(2) 
  #pragma Off(Char_default_unsigned)
#endif

#ifdef __SC__             
   #pragma SC align 2      /* align structures to word alignment, Symantec */
#endif

#ifdef __WATCOMC__
   #pragma off(unreferenced)
#endif
                           /* Foreign language support */
#ifdef S4GERMAN
   #define S4LANGUAGE
#endif   

#ifdef S4FRENCH
   #define S4LANGUAGE
#endif

#ifdef S4SWEDISH
   #define S4LANGUAGE
#endif

#ifdef S4FINNISH
   #define S4LANGUAGE
#endif

#ifndef S4LANGUAGE
   #define u4memcmp memcmp
#endif

#ifndef S4MEMCMP
   #define c4memcmp memcmp
#endif

#ifdef S4MEMCMP
   #ifdef __cplusplus
      extern "C" {
   #endif
      int c4memcmp(void *, void *, size_t) ;
   #ifdef __cplusplus
      }
   #endif
#endif      

#ifdef __TURBOC__
   #ifdef S4DLL
      #define S4CLASS huge
      #define S4FUNCTION far pascal
   #endif
#endif

#ifdef _MSC_VER
   #if _MSC_VER == 600
      #define S4NO_NEGATIVE_LOCK
   #endif
   #ifdef S4DLL
      #ifdef __cplusplus
         #define S4CLASS
      #else
         #define S4CLASS huge
      #endif
      #define S4FUNCTION far pascal
   #endif
#endif

#ifdef __ZTC__
   #ifdef S4DLL
      #ifdef __cplusplus
         #define S4CLASS
      #else
         #define S4CLASS huge
      #endif
      #define S4FUNCTION far pascal
   #endif
#endif

#ifdef S4PASCAL_DOS
   #define S4PASCAL
#endif

#ifdef S4PASCAL_WIN
   #define S4PASCAL
#endif

#ifdef S4DLL_BUILD
   #ifndef S4PASCAL_DOS
      #define S4WINDOWS
   #endif
   #define __DLL__ 1
#endif

#ifdef __DLL__
   #ifdef S4OS2
      #define S4CLASS _export
      #define S4FUNCTION _export
   #else
      #define S4DLL
      #define S4CLASS _export
      #define S4FUNCTION far pascal _export
   #endif
#endif

#ifndef S4CLASS
   #define S4CLASS
#endif
#ifndef S4FUNCTION
   #define S4FUNCTION
#endif

#ifdef S4DLL
   #ifndef S4OS2
      #ifndef S4PASCAL_DOS
         #define S4WINDOWS
      #endif
      #define S4CALL far _cdecl _export
   #endif
#else
   #ifdef _MSC_VER
      #define S4CALL _cdecl S4FUNCTION
   #else
      #ifdef __ZTC__
         #define S4CALL _cdecl S4FUNCTION
      #else
         #define S4CALL S4FUNCTION
      #endif
   #endif
#endif

#ifdef S4WINDOWS
   typedef unsigned int UINT ;
#endif

#ifdef S4WINDOWS
   #ifdef __TURBOC__
      #if __TURBOC__ == 0x297         /* Borland C++ 2.0 */
         #define M4PRINT sprintf
      #else
         #define M4PRINT wsprintf
      #endif
   #else
      #define M4PRINT wsprintf
   #endif
#else
   #define M4PRINT sprintf            /* DOS */
#endif

#ifdef S4DLL
   #ifndef S4OS2
      #define S4PTR far
   #endif
#endif

#ifndef S4PTR
   #define S4PTR
#endif

#ifdef S4NDX
   #define N4OTHER
   /* default is to use dBASE III+/Clipper memo file formats if using .NDX index file formats */
   #define S4MNDX
#endif

#ifdef S4CLIPPER
   #define N4OTHER
   /* default is to use dBASE III+/Clipper memo file formats if using .NTX index file formats */
   #define S4MNDX
   #define S4HAS_DESCENDING
#endif

#ifdef S4FOX
   /* default is to use foxpro memo file formats if using foxpro index file formats */
   #define S4MFOX
   #define S4HAS_DESCENDING
#endif

#ifdef S4UNIX
   #ifdef N4OTHER
      force error:  Clipper or dBase III support not allowed.
   #endif
#endif

#ifdef S4MDX
   #define S4MMDX
#endif

#ifdef S4MNDX
   #define   MEMO4SIZE   0x200
#endif

#ifndef S4UNIX
   typedef  const void S4PTR *  S4CMP_PARM ;
#endif

#ifdef S4OFF_OPTIMIZE
   #define S4OPTIMIZE_OFF
#endif

#ifdef S4OFF_INDEX
   #define S4INDEX_OFF
#endif

#ifdef S4OFF_MEMO
   #ifndef S4MEMO_OFF
      #define S4MEMO_OFF
   #endif
#endif

#ifdef S4MEMO_OFF
   #ifndef S4OFF_MEMO
      #define S4OFF_MEMO
   #endif
#endif

#ifdef S4OFF_MULTI
    #define S4SINGLE
#endif

#ifdef S4TEST
   #ifndef S4DEBUG_DEV
      #ifndef S4UNIX
      #ifndef S4MACINTOSH
      #ifndef S4WINDOWS
      #ifdef S4SINGLE
         #define S4DEBUG_DEV
      #endif
      #endif
      #endif
      #endif
   #endif
   #ifndef S4TESTING
      #define S4TESTING
   #endif
   #ifndef S4ERROR_HOOK
      #define S4ERROR_HOOK
   #endif
   #ifndef S4NO_OUT
      #define S4NO_OUT
   #endif
#endif

#ifdef S4DEBUG_DEV
   #ifndef S4INDEX_VERIFY
      #define S4INDEX_VERIFY
   #endif
#endif

typedef int S4CALL  S4CMP_FUNCTION( S4CMP_PARM, S4CMP_PARM, size_t) ;

#ifdef S4DLL
   #define sort4assign_cmp(s4,f)  (s4)->cmp = (S4CMP_FUNCTION S4PTR *) MakeProcInstance((FARPROC) f, (s4)->code_base->hInst)
#else
   #define sort4assign_cmp(s4,f)  (s4)->cmp = (S4CMP_FUNCTION S4PTR *) (f)
#endif

/* Integer Return Codes */
#define r4success     0
#define r4found       1       /* Primary Key Match */
#define r4after       2
#define r4eof         3
#define r4bof         4
#define r4entry       5       /* No index file entry or no record (go) */
#define r4descending 10
#define r4unique     20       /* Key is not unique, do not write/append */
#define r4unique_continue  25 /* Key is not unique, write/append anyway */
#define r4ignore     40
#define r4keep       45
#define r4locked     50
#define r4no_create  60       /* Could not create file */
#define r4no_open    70       /* Could not open file */
#define r4no_tag     80       /* DataIndex::seek, with no default tag */
#define r4terminate  90       /* no relation match with terminate set */

/* General Disk Access Errors */
#define e4close   -10
#define e4create  -20
#define e4len     -30
#define e4len_set -40
#define e4lock    -50
#define e4open    -60
#define e4read    -70
#define e4remove  -80
#define e4rename  -90
#define e4seek   -100
#define e4unlock -110
#define e4write  -120

/* Database Specific Errors */
#define e4data  -200
#define e4field_name -210     /* Invalid field name */
#define e4field_type -220
#define e4record_len -230

/* Index File Specific Errors */
#define e4entry      -300     /* Tag entry not located */
#define e4index      -310
#define e4tag_name   -330
#define e4unique     -340     /* Key is not unique */

/* Expression Errors */
#define e4comma_expected -400
#define e4complete       -410
#define e4data_name      -420
#define e4length_err     -422
#define e4not_constant   -425
#define e4num_parms      -430
#define e4overflow       -440 /* Overflow while evaluating expression */
#define e4right_missing  -450
#define e4type_sub       -460
#define e4unrec_function -470
#define e4unrec_operator -480
#define e4unrec_value    -490
#define e4unterminated   -500

/* Optimization Errors */
#define e4opt         -610
#define e4opt_suspend -620
#define e4opt_flush   -630

/* Relation Errors */
#define e4lookup_err  -710
#define e4relate      -720

/* Report Errors */
#define e4report  -810

/* Critical Errors */
#define e4info     -910  /* Unexpected information in internal variable */
#define e4memory   -920  /* Out of memory */
#define e4parm     -930  /* Unexpected parameter */
#define e4demo     -940  /* Exceeded maximum record number for demo */
#define e4result   -950  /* Unexpected result */

/* Library Errors */
/* call to library function calls not supported */
#define e4not_index  -1010  /* S4OFF_INDEX */
#define e4not_memo   -1020  /* S4OFF_MEMO */
#define e4not_rename -1030  /* S4NO_RENAME */
#define e4not_write  -1040  /* S4OFF_WRITE */

#define E4DEMO_MAX 200

/* garbage between expression and filter is length: */
#ifdef  S4FOX
   #define I4MULTIPLY       1
   #define B4DO_BACK_LEVEL  3
   #define I4MAX_KEY_SIZE 240
   #define F4MAX_NUMERIC   20
   #define F4MAX_DECIMAL   19
   #define F4DECIMAL_OFFSET 1
#else
   #ifdef N4OTHER
      #define F4MAX_NUMERIC   19
      #define F4MAX_DECIMAL   15
      #define F4DECIMAL_OFFSET 2
   #endif
   #ifdef S4MDX
      #define F4MAX_NUMERIC   20
      #define F4MAX_DECIMAL   18
      #define F4DECIMAL_OFFSET 2
   #endif

   #define I4MULTIPLY     512

   #ifdef S4NDX
      #define I4MAX_KEY_SIZE 100
   #else
      #ifdef S4CLIPPER
         #define I4MAX_KEY_SIZE 338
      #else
         #define I4MAX_KEY_SIZE 102
      #endif
   #endif
#endif

#define D4GARBAGE_LEN  518
#define E4ACCURACY     1.0e-13
#define E4ACCURACY_DIGITS  15


/* if S4NO_NEGATIVE_LOCK is defined, there is no dBASE IV compatibility */

#ifdef N4OTHER
   #define L4LOCK_POS     1000000000L
#endif
#ifdef S4FOX
   #ifdef S4NO_NEGATIVE_LOCK
      #define L4LOCK_POS_OLD 2000000000L
      #define L4LOCK_POS     2000000000L
   #else
      #define L4LOCK_POS_OLD 0x40000000L
      #define L4LOCK_POS     0x7FFFFFFEL
   #endif
#endif
#ifdef S4MDX
   #ifdef S4NO_NEGATIVE_LOCK
      #define L4LOCK_POS_OLD 2000000000L
      #define L4LOCK_POS     2000000000L
   #else
      #define L4LOCK_POS_OLD 0x40000000L
      #define L4LOCK_POS     0xEFFFFFFFL
   #endif
#endif

typedef void _cdecl C4STOK( char S4PTR *, char S4PTR *, int ) ;
typedef void _cdecl C4DTOK( char S4PTR *, double ) ;

struct DATA4_st ;
struct F4MEMO_st ;
struct INDEX4_st ;
struct TAG4_st ;
#ifdef __cplusplus
   class S4CLASS FILE4 ;
#else
   struct FILE4_st ;
#endif
#ifdef __cplusplus
   class S4CLASS CODE4 ;
#else
   struct CODE4_st ;
#endif

#ifdef __cplusplus
   #ifdef S4WINDOWS
   typedef struct _export l4link_st
   #else
   typedef struct l4link_st
   #endif
#else
typedef struct l4link_st
#endif
{
   struct l4link_st S4PTR *n, S4PTR *p ;
} LINK4 ;

#ifdef __cplusplus
class S4CLASS LIST4
{
public:
#else
typedef struct
{
#endif
   LINK4 S4PTR *last_node ;        /* The last Link */
   void   S4PTR *selected ;
   int     n_link ;            /* The number of links in the list */
#ifdef __cplusplus
} ;
#else
} LIST4 ;
#endif

typedef struct
{
   LINK4 link ;
   double  data ;  /* Make sure it is on a boundry good for at least a double  */
} Y4CHUNK ;

typedef struct
{
   LINK4      link ;

   LIST4      chunks ;      /* Chunks of pieces */
   LIST4      pieces ;      /* A list of available memory pieces */

   #ifdef __cplusplus
      CODE4 S4PTR    *code_base ;
   #else
      struct  CODE4_st *code_base ;
   #endif
   int        unit_start;   /* The starting # of entries for the Memory Type */
   unsigned   unit_size ;   /* The size of each allocated piece */
   int        unit_expand ; /* The expansion # of entries for the Memory Type */
   int        n_repeat ;    /* The number of times entry returned for 'new' */
                            /* If n_repeat is '-1', it is a temporary entry. */
   int        n_used ;      /* The number of entries used */
}  MEM4 ;

typedef struct OPT4BLOCK_st
{
   LINK4             link ;
   LINK4             lru_link ;
   char              changed ;
   unsigned          len ;

   /* these next 2 elements must match the OPT4CMP structure above */
   #ifdef __cplusplus
      FILE4          S4PTR *file ;
   #else
      struct FILE4_st *file ;
   #endif
   long              pos ;

   void              S4PTR *data ;
} OPT4BLOCK ;

#ifdef __cplusplus
   typedef struct S4CLASS
#else
   typedef struct
#endif
{
   int              num_buffers ;
   unsigned long    block_size ;
   unsigned long    buffer_size ;
   unsigned long    hash_trail ;       /* where last optimized file ended */
   unsigned long    num_blocks ;

   LIST4            avail ;
   LIST4            dbf_lru ;
   LIST4            index_lru ;
   LIST4            other_lru ;
   LIST4 S4PTR     *prio[3] ;

   unsigned char    old_mode ;
   unsigned char    do_update ;
   unsigned char    check_count ;
   unsigned char    dummy_char ;
   int              min_link ;
   void  S4PTR* S4PTR* buffers ;
   OPT4BLOCK    S4PTR *blocks ;
   char             block_power ;
   char             num_shift ;
   unsigned long    num_lists ;
   unsigned long    mask ;
   LIST4 S4PTR      *lists ;
   unsigned         max_blocks ;

   LIST4            opt_files ;

   char             is_skip ;
   char             force_current ;    /* switch forces a read of current contents */

   char S4PTR       *read_buffer ;
   unsigned long     read_start_pos ;
   int               in_read_buf ;

   char S4PTR       *write_buffer ;
   unsigned long     write_start_pos ;
   unsigned long     write_cur_pos ;
   unsigned          write_block_count ; /* is the buffer full? */
   #ifdef __cplusplus
      FILE4 S4PTR    *write_file ;
   #else
      struct FILE4_st S4PTR *write_file ;        /* which file has the write buffer? */
   #endif
} OPT4 ;

#ifdef __cplusplus
class S4CLASS CODE4
{
public:
#else
typedef struct CODE4_st
{
#endif
   #ifndef S4OPTIMIZE_OFF
      int  optimize_write ;
      OPT4 opt ;
      int  has_opt, do_opt ;
      unsigned int   mode ;
   #endif

   #ifdef S4CLIPPER
      int numeric_str_len ;    /* the default length for clipper index files */
      int decimals ;           /* the default # decimals for clipper index files */
   #endif

   unsigned hWnd ;                /* For use under Microsoft Windows */
   unsigned hInst ;

   LIST4   data_list ;           /* A list of open data files. */
   int default_unique_error ;     /* e4unique, r4unique, r4unique_continue */
   char date_format[20];          /* Longest is September 15, 1990 */

   unsigned mem_size_block ;      /* Block size (bytes) for memo and index files */
   unsigned mem_size_sort_pool ;  /* The default pool size for sorting */
   unsigned mem_size_sort_buffer ;/* The default file buffer size when sorting */
   unsigned mem_size_buffer ;     /* Pack, Zap */
   unsigned mem_size_memo ;
   unsigned mem_size_memo_expr ;

   int mem_expand_block ;         /* Expanding block memory allocation */
   int mem_expand_data ;          /* Expanding data file allocation */
   int mem_expand_index ;         /* Expanding index file allocation */
   int mem_expand_tag ;           /* Expanding index file allocation */

   unsigned mem_start_data ;           /* Initial data file allocation */
   unsigned mem_start_block ;          /* Initial block memory allocation for index files */
   unsigned mem_start_index ;          /* Initial index file allocation */
   unsigned mem_start_tag ;            /* Initial index file allocation */
   unsigned mem_start_buffer ;
   unsigned long mem_start_max ;

   /* True/False Flags */
   int  auto_open ;            /* Automatic production index file opening */
   int  create_error ;         /* Do 'file4create' error ? */
   int  off_error ;            /* Show error messages? */
   int  exclusive ;            /* how should files be opened? */
   int  file_flush ;           /* force hard file flush during write */
   int  expr_error ;
   int  field_name_error ;
   int  go_error ;             /* Do 'd4go' error ? */
   int  open_error ;           /* Do 'file4open' error ? */
   int  optimize ;             /* should files be automatically bufferred? */
   int  read_lock ;            /* Do lock when reading database ? */
   int  read_only ;
   int  relate_error ;         /* do relate4terminate error when no match and relate4terminate selected */
   int  safety ;               /* File create with safety ? */
   int  skip_error ;           /* Do 'DataIndex::skip' error ? */
   int  tag_name_error;

   int  lock_attempts ;        /* How many times to attempt locks. */

   MEM4 S4PTR *index_memory ;
   MEM4 S4PTR *data_memory ;
   MEM4 S4PTR *tag_memory ;
   MEM4 S4PTR *calc_memory ;
   MEM4 S4PTR *bitmap_memory ;
   LIST4 calc_list ;

   MEM4 S4PTR *total_memory ;
   LIST4  total_list ;       /* A list of T4TOTAL */
   int  num_reports ;

   int  error_code ;
   int  debug_int ;            /* used to check structure integrity (set to 0x5281) */

   short pageno;
  
   char S4PTR *field_buffer ;    /* used by the f4str() function */
   unsigned  buf_len ;

   char S4PTR *expr_work_buf ;   /* used by expression parsing */
   unsigned expr_buf_len ;

   char S4PTR *stored_key ;      /* used by the expr4key() function */
   unsigned stored_key_len ;

   int bitmap_disable ;     /* used for testing purposes to test disabled bitmaps */
   int do_index_verify ;    /* for internal purposes only at this point */
   #ifdef S4OS2SEM
   #ifdef S4OS2
   #ifndef __IBMC__
   #ifndef __WATCOMC__
      HMTX hmtx_mem, hmtx_expr ;
   #endif
   #endif
   #endif
   #endif
   char saved_key[I4MAX_KEY_SIZE + 2 * sizeof(long)] ;       /* used by i4remove.c, i4tag.c and i4addtag.c, i4version_check, t4version_check */
#ifdef __cplusplus
} ;
#else
} CODE4 ;
#endif

#ifdef __cplusplus
class S4CLASS FILE4
{
public:
#else
typedef struct FILE4_st
{
#endif
   #ifndef S4OPTIMIZE_OFF
      LINK4  link ;            /* set to 0 if file not optimized */
      long   hash_init ;
      long   len ;             /* internal if optimized */
      char   type ;            /* dbf, index, other */
      char   buffer_writes ;   /* are writes being bufferred? */
      int    do_buffer ;       /* is the file bufferring on? */
   #endif
   char S4PTR *name ;
   CODE4 S4PTR *code_base ;
   int   hand ;
   char  is_temp ;             /* True if it is a temporary file */

   /* is_exclusive and is_read_only both on indicate 'r' attribute on file */
   /* is_read_only only on indicates user access is limited to read only */
   /* if is_exclusive, will do full bufferring, if is_read_only, will avoid */
   /* performing any disk writes */

   char  is_exclusive ;        /* True if opened exclusive */
   char  is_read_only ;        /* True if file is read only */
   char  do_alloc_free ;
   char  write_buffer ;        /* buffer writes where possible */
   char  file_created ;        /* false if the file has not been created yet - i.e. if a memory-only file */


   #ifdef S4DEBUG_DEV
      char dup_name[255] ;
      int  has_dup ;
      int  in_use ;
   #endif
#ifdef __cplusplus
} ;
#else
} FILE4 ;
#endif

typedef struct
{
   FILE4     file ;
   short      block_size ;               /* Bytes */
   struct DATA4_st  S4PTR *data ;
   int      file_lock ;                  /* True if file is locked */
} MEMO4FILE ;

#ifdef __cplusplus
class S4CLASS FILE4SEQ_READ
{
public:
#else
typedef struct
{
#endif
   FILE4 S4PTR *file ;

   long  pos ;          /* The next position to read from */
   char S4PTR *buffer ;
   unsigned next_read_len ;
   unsigned total ;     /* Total buffer length */
   unsigned working ;   /* Temporary working buffer length (to help align write) */
   unsigned avail ;     /* # of bytes currently available */
#ifdef __cplusplus
} ;
#else
} FILE4SEQ_READ ;
#endif

#ifdef __cplusplus
class S4CLASS FILE4SEQ_WRITE
{
public:
#else
typedef struct
{
#endif
   FILE4 S4PTR *file ;

   long  pos ;          /* The next position to read from */
   char S4PTR *buffer ;
   unsigned total ;     /* Total buffer length */
   unsigned working ;   /* Temporary working buffer length (to help align write) */
   unsigned avail ;     /* # of bytes left in working buffer */
#ifdef __cplusplus
} ;
#else
} FILE4SEQ_WRITE ;
#endif

typedef struct  /* Data File Format */
{
   char     name[11] ;
   char     type ;
   char     filler[4] ;
   unsigned char  len ;
   unsigned char  dec ;
   char     filler2[13] ;
   char    has_tag ;
} FIELD4IMAGE ;

typedef struct  /* Internal Structure and Field Routines. */
{
   char        name[11] ;
   unsigned    len ;
   int         dec ;
   int         type ;
   int         offset ;
   struct DATA4_st  S4PTR *data ;
   struct F4MEMO_st  S4PTR *memo ;
   #ifdef S4VBASIC
      int  debug_int ;         /* used to check structure integrity (set to 0x5281) */
   #endif
} FIELD4 ;

typedef struct F4MEMO_st
{
   int    is_changed ;

   int    status ;           /* 0 - Current contents, 1 - Unknown */
   char  S4PTR *contents ;
   unsigned  len ;
   unsigned  len_max ;
   FIELD4  S4PTR *field ;
} F4MEMO ;

typedef struct  /* Creating Data File */
{
   char S4PTR  *name ;
   int         type ;
   unsigned int len ;
   unsigned int dec ;
} FIELD4INFO ;

typedef struct
{
   /* Database Header Information */
   char     version ;        /* 83H with .dbt, 03H without */
   char     yy ;             /* Last Update */
   char     mm ;
   char     dd ;
   long     num_recs ;
   unsigned short header_len; /* Header Length, Indicates start of data */
   unsigned short record_len;
   char     zero[16] ;
   char     has_mdx ;    /* 1 for true */
   char     dummy ;
   char     zero2[2] ;
} DATA4HEADER_FULL ;

typedef struct DATA4_st
{
   LINK4  link ;

   /* Database Header Information */
   char     version ;        /* 83H with .dbt, 03H without */
   char     yy ;             /* Last Update */
   char     mm ;
   char     dd ;
   long     num_recs ;
   unsigned short header_len; /* Header Length, Indicates start of data */

   char S4PTR *record ;              /* Data allocated with 'u4alloc' */
   char S4PTR *record_old ;          /* Data allocated with 'u4alloc' */
                                 /* Extra byte added for temporary CTRL_Z */
   unsigned record_width ;
   int      record_changed ;      /* T/F */
   long     rec_num ;             /* Record number; -1 unknown; 0 for append */
   long     rec_num_old ;         /* Record number, -1 none present; 0 for append */

   FILE4    file ;
   char     alias[11] ;

   char     memo_validated ; /* Can we be sure memo id #'s are up to date. */

   CODE4 S4PTR *code_base ;
   char     has_mdx ;        /* Has an MDX file attached to it */

   FIELD4  S4PTR *fields ;        /* An array of field pointers */
   int      n_fields ;       /* The number of data fields in the database */

   F4MEMO   S4PTR *fields_memo ;    /* A list of fields to be flushed */
   int      n_fields_memo ;  /* The number of memo files in the database */

   long     locked_record ;  /* 'locks' data when 'n_locks <= 1' */
   long     S4PTR *locks ;
   int      n_locks ;        /* Number of elements in 'locks' allocated */
   int      num_locked ;     /* Number of records locked */
   int      file_lock ;      /* True if entire file is locked */
   int      append_lock ;    /* True if the file is locked for appending */

   int      file_changed ;   /* True if the file has been changed since */
                                /* the header has been updated. */

   LIST4    indexes ;
   int      bof_flag, eof_flag ;    /* Beginning/End of File flags */

   short    block_size ;
   MEMO4FILE   memo_file ;      /* Memo file handle */
   #ifdef S4VBASIC
      int   debug_int ;      /* used to check structure integrity (set to 0x5281) */
   #endif
   #ifndef S4SINGLE
      long  minCount ;    /* used as a minimum record count for various functions */
   #endif
   long     count ;  /* a fairly current record count on the database */
                     /* used by relate module, by put here for compatibility with report writer... */
} DATA4 ;

typedef void S4OPERATOR(void) ;

typedef struct E4INFO_st
{
   FIELD4 S4PTR *field_ptr ;
   char S4PTR *p1 ;
   int   len ;         /* Length */
   int   num_entries ; /* Number of entries in sub-expression */
   int   num_parms ;
   int   result_pos ;  /* The position in the result array for result. */
   int   i1 ;          /* Could be constant position. 'i1' and 'result_pos'
                       and 'function_i' and 'function'
                       must be at last position due to memcmp() in e4is_tag() */
   int   function_i ;
   S4OPERATOR S4PTR *function ;
} E4INFO ;

typedef struct e4expr_st
{
   E4INFO S4PTR *info ;
   int      info_n ;
   char S4PTR *source ;
   char S4PTR *constants ;
   int      len ;
   int      type ;
   #ifdef S4CLIPPER
      int   key_dec ;         /* used for CLIPPER version */
      int   key_len ;
   #endif
   DATA4  S4PTR *data ;
   CODE4  S4PTR *code_base ;

   int   len_eval ;        /* This is the length of the buffer needed for evaluation. */
   int   num_parms ;       /* This is the # of parameter positions used in evaluation. */
   char  has_trim ;        /* special case for key evaluation */
} EXPR4 ;

#ifdef S4MDX
   #define I4MAX_EXPR_SIZE 220
#endif
#ifdef S4NDX
   #define I4MAX_EXPR_SIZE 220
#endif
#ifdef S4FOX
   #define I4MAX_EXPR_SIZE 255
#endif
#ifdef S4CLIPPER
   #define I4MAX_EXPR_SIZE 255
#endif

#ifdef N4OTHER
   typedef struct
   {
      long  pointer ;    /* =0L if record, not pointer */
      long  num ;
      char  value[1] ;  /* The key size is variable */
   } B4KEY_DATA ;

   #ifdef S4NDX
      #define B4BLOCK_SIZE 512
   #else
      #ifdef S4CLIPPER
         #define B4BLOCK_SIZE 1024
      #endif
   #endif
#else
   typedef struct
   {
      long  num ;
      char  value[1] ;  /* The key size is variable */
   } B4KEY_DATA ;
#endif

#ifdef S4FOX
/*#define VERSION_POS 498L*/

/* the following structure is used only on the leaf nodes of the tree structure */
typedef struct
{
   short            free_space ;        /* # bytes available in node */
   unsigned char    rec_num_mask[4] ;      /* record number mask */
   unsigned char    dup_byte_cnt ;      /* duplicate byte mask count */
   unsigned char    trail_byte_cnt ;    /* Trailing byte mask count */
   unsigned char    rec_num_len ;       /* # bits used for record number */
   unsigned char    dup_cnt_len ;       /* # bits used for duplicate count */
   unsigned char    trail_cnt_len ;     /* # bits used for trail count */
   unsigned char    info_len ;          /* # bytes for holding record number, */
} B4NODE_HEADER ;

typedef struct
{
      short      node_attribute ;    /* 0=index, 1=root, 2=leaf */
      short      n_keys ;            /* Block Image starts here */
      long       left_node ;         /* -1 if not present */
      long       right_node ;        /* -1 if not present */
} B4STD_HEADER ;

typedef struct
{
   LINK4 link ;
   struct TAG4_st  S4PTR *tag ;

   int   changed ;
   long  file_block ;     /* Identifies block within index file */
   int   key_on ;         /* The current key within the block */
   int   cur_trail_cnt ;  /* current value used for seeking */
   int   cur_dup_cnt ;    /* current value used for seeking */
   int   dup_pos ;        /* bit offset into the info for the duplicate data */
   int   trail_pos ;      /* bit offset into the info for the trail data */
   int   rec_pos ;        /* bit offset into the info for the record # data */
   char  *cur_pos ;       /* current position into the data (starts at end) */

   int   built_on ;       /* the 'current' key value (i.e. key really 'on') */
   char  S4PTR *built_pos ;     /* position where built on */
   B4KEY_DATA S4PTR *built_key ;

   B4STD_HEADER header ;
   B4NODE_HEADER node_hdr ;    /* only if the block is a leaf */
   char  data[1] ;        /* the remaining data */
} B4BLOCK ;

/* next is the # of bytes of important info for T4HEADER */
#define T4HEADER_WR_LEN 0x10
/* block_size is 512 for foxpro */
#define B4BLOCK_SIZE 512
#endif  /* ifdef S4FOX  */

#ifndef S4FOX

#ifdef S4NDX
   typedef struct
   {
      long     root ;
      long     eof ;
      char     n1_dummy ;
      char     type ;
      char     n2_dummy[2] ;
      short    key_len  ;
      short    keys_max ;           /* Maximum # of keys per block;  <= 100 */
      short    int_or_date ;        /* TRUE (1) if Numeric or Date Key */
      short    group_len ;          /* key_len plus 8 increased to a multiple of 2 */
      short    dummy ;
      short    unique ;             /* TRUE if Unique */

 /*   char     expression[256] ; */
 /*   long     version ; */
   } I4IND_HEAD_WRITE;
#else
   #ifdef S4CLIPPER
      typedef struct
      {
         short          sign ;
         short          version ;
         long           root ;          /* Root Block */
         long           eof ;           /* First Free Block Pointer */
         short          group_len ;     /* Key Length + 2*sizeof(long) */
         short          key_len ;       /* Key Length */
         short          key_dec ;       /* Number of Decimals in Key */
         short          keys_max ;      /* Maximum # of keys per block;  <= 100 */
         short          keys_half ;     /* Maximum # of keys per half block */

    /*   char           expression[256];   The index expression corresponding to the database. */
    /*   short          unique   ;         TRUE if Unique */
      } I4IND_HEAD_WRITE;
   #endif
#endif

#ifdef S4NDX
typedef struct
{
   long     old_version ;
   int      header_offset ;

   long     root ;
   long     eof ;
   char     n1_dummy ;
   char     type ;
   char     n2_dummy[2] ;
   short    key_len  ;
   short    keys_max ;           /* Maximum # of keys per block;  <= 100 */
   short    int_or_date ;        /* TRUE (1) if Numeric or Date Key */
   short    group_len ;          /* key_len plus 8 increased to a multiple of 2 */
   short    dummy ;
   short    unique ;             /* TRUE if Unique */
/* char     expression[256] ; */
   long     version ;
}  T4HEADER ;
#else
   #ifdef S4CLIPPER
      typedef struct
      {
         long       old_version ;
         int        header_offset ;
         long       virtual_eof ;   /* The next available file block */
         short      sign ;
         short      version ;
         long       root ;          /* Root Block */
         long       eof ;           /* First Free Block Pointer */
         short      group_len ;     /* Key Length + 2*sizeof(long) */
         short      key_len ;       /* Key Length */
         short      key_dec ;       /* Number of Decimals in Key */
         short      keys_max ;      /* Maximum # of keys per block;  <= 100 */
         short      keys_half ;     /* Maximum # of keys per half block */
      /* char       expression[256] ;   The index expression corresponding to the database. */
         short      unique   ;      /* TRUE if Unique */
         short      descending ;     /* The descending flag corresponding to the index file */
      /* char       filter[256] ;   The filter(for) expression corresponding to the database. */
      }  T4HEADER ;
   #else
      typedef struct
      {
         char   two ;                /* Version number (currently 2) */
         char   yymmdd[3] ;          /* Date of last reindex */
         char   data_name[12] ;      /* Name of associated data file */
         char   dummy1[4] ;          /* extra 4 bytes for data-names-not used in DOS */
         short  block_chunks ;       /* Block Size 1 to 32 (512 byte chunks) */
         short  block_rw ;           /* Block Read/Write Size in bytes */
         char   is_production ;      /* 1 if production index, else 0 */
         char   num_slots ;          /* number possible tags (48) */
         short  slot_size ;          /* number bytes/tag slot (32) */
         short  num_tags ;
         short  dummy2 ;
         long   eof ;
         long   free_list ;          /* start of the free list */
         char   zero[4] ;
         char   create_date[3];      /* not used by CodeBase */
         char   blank ;
      }  I4HEADER ;
   #endif     /*   ifdef S4CLIPPER  */
#endif     /*   ifdef S4NDX   */
#endif     /*   ifndef S4FOX  */


#ifndef S4FOX
typedef struct
{
   LINK4 link ;
   struct TAG4_st  S4PTR *tag ;

   long  file_block ;  /* Identifies block within index file */
   int   changed ;
   int   key_on ;      /* The current key within the block */

   short      n_keys ; /* Block Image starts here */
   #ifdef S4NDX
      char       dummy[2] ;
      B4KEY_DATA data ;
   #else
      #ifdef S4CLIPPER
         short pointers[( B4BLOCK_SIZE / 2 - 1 )] ;
         B4KEY_DATA S4PTR *data ;
      #else
         char       dummy[6] ;
         B4KEY_DATA info ;
      #endif
   #endif
} B4BLOCK ;

typedef struct
{
   long  header_pos ;          /* Header position (in 512 byte chunks) */
   char  tag[10] ;
   short x1000 ;               /* used for dBASE/SQL expression type - dBASE only allowed for CBPP 1.0x */
   char  left_chld ;
   char  right_chld ;
   char  parent ;
   char  x2 ;
   char  index_type ;
   char  zeros[11] ;
} T4DESC ;
#endif     /* ifndef  S4FOX  */

#ifndef N4OTHER
typedef struct
{
#ifdef S4FOX
   long           root ;            /* -1 means unknown */
   long           free_list ;       /* start of the free list (-1 if none) */
   unsigned long  version ;         /* used multi-user only */
   short          key_len ;         /* Key Length */
   unsigned char  type_code;        /* 0x01 Uniq; 0x08 For Clause; 0x32 Compact; 0x80 Compound */
   unsigned char  signature ;       /* unused */

/* char           dummy2[482] ;        unused */
   char           dummy3[4] ;
   short          descending   ;    /* 1 = descending, 0 = ascending */
   short          filter_pos ;      /* not used, == to expr_len */
   short          filter_len ;      /* length of filter clause */
   short          expr_pos ;        /* not used, == to 0  */
   short          expr_len ;        /* length of expression */
/* char           expr_pool[512] ;  expression and filter pool */
#else
   long           root ;            /* -1 means unknown */
   char           dummy1[4] ;
   char           type_code;        /* 0x10 Normal; 0x58 Uniq,Desc; 0x50 Uniq; 0x18 Desc */
   char           type ;            /* N,D, or C (F is type N)  */
   char           dummy2[2] ;
   short          key_len ;
   short          keys_max ;        /* Maximum # of keys per block;  <= 100  */
   char           dummy3[2] ;       /* 1 if Numeric or Date Key (NDX only) */
   short          group_len ;       /* key_len plus 4 (MDX); plus 8 (NDX) */
   unsigned char  version ;
   char           dummy4 ;
   short          unique   ;        /* 0x4000 (TRUE)if Unique */

   /* Note, 'expr_key[220]' comes after 'unique' and */
   /*       'expr_filter[220]' comes at position 0x2FA */
#endif
}  T4HEADER ;
#endif   /*  ifndef N4OTHER  */

typedef struct TAG4_st
{
   LINK4           link ;

   EXPR4   S4PTR  *expr ;
   EXPR4   S4PTR  *filter ;
   int             unique_error; /* Is rewriting a unique key an error ? */
   struct INDEX4_st S4PTR *index ;
   S4CMP_FUNCTION *cmp ;
   C4STOK S4PTR   *stok ;               /* Conversion for 'seek' */
   C4DTOK S4PTR   *dtok ;               /* Conversion for 'seek' */
   CODE4 S4PTR    *code_base ;
   char            alias[11] ;
   #ifdef S4MDX
      char            had_keys ;
   #endif
   char            has_keys ;
   LIST4           blocks ;

   #ifdef S4FOX
      char         p_char ;
   #endif

   #ifdef S4CLIPPER
      long         check_eof ;     /* used for debug purposes to verify eof length */
   #endif

   #ifdef N4OTHER
      FILE4        file ;
      int          file_locked ;
   #endif

   T4HEADER    header ;
   long        header_offset ;     /* Offset in file to the tag's header info. */
   int         root_write ;        /* True if 'header.root' needs to be written */
   int         key_dec ;
   LIST4       saved ;
   int         debug_int ;         /* used to check structure integrity (set to 0x5281) */
   #ifdef S4MDX
      long  changed ;
   #endif
   #ifdef S4UNIX
      int key_type ;
   #endif
} TAG4 ;

typedef struct
{
   char S4PTR  *name ;
   char S4PTR  *expression ;
   char S4PTR  *filter ;
   int unique ;
   unsigned int descending ;
} TAG4INFO ;

typedef struct INDEX4_st
{
   LINK4  link ;

   FILE4  file ;
   DATA4 S4PTR *data ;
   CODE4 S4PTR *code_base ;
   LIST4  tags ;

   #ifdef S4FOX
      TAG4 S4PTR *tag_index ;    /* the tags are a tag in the index file! */
      long  eof ;
   #else
      #ifdef N4OTHER
         char alias[11] ;
         char S4PTR *path ;
      #else
         I4HEADER header ;
      #endif
   #endif

   MEM4 S4PTR *block_memory ;
   int   file_locked ;    /* True if locked */
   #ifdef S4FOX
      unsigned long  version_old ;
   #else
      #ifdef S4MDX
         long  changed ;
      #else
         long  version_old ;
      #endif
   #endif
} INDEX4 ;


/* Memo File Structures */

typedef struct
{
   #ifdef S4MFOX
      long  next_block ;  /* Memo Entry 1,2, ... */
      char  usused[2] ;
      short block_size ;  /* Bytes */
   #else
      #ifdef S4MNDX
         long  next_block ;  /* Memo Entry 1,2, ... */
      #else
         long  next_block ;  /* Memo Entry 1,2, ... */
         long  zero ;
         char  file_name[8] ;
         short zero2 ;
         short x102 ;
         short block_size ;  /* Bytes */
         short zero3 ;
      #endif
   #endif
} MEMO4HEADER ;

#ifndef S4MNDX
#ifndef S4MFOX
typedef struct
{
   long  next ;       /* The next free block area */
   long  num ;        /* The number of free blocks in the free block area */
   int   to_disk ;    /* TRUE if this information needs to be written to disk */
   long  block_no ;   /* The current block number */
} MEMO4CHAIN_ENTRY ;
#endif /*  ifndef S4MFOX  */

typedef struct
{
   #ifdef S4MFOX
      long  type ;         /* 0 for picture, 1 for text -- picture not supported */
      long  num_chars ;    /* Including the 'MemoBlock' */
   #else
      short minus_one ;    /* '-1' for dBASE IV */
      short start_pos ;
      long  num_chars ;    /* Including the 'MemoBlock' */
   #endif
} MEMO4BLOCK ;
#endif  /*  ifndef S4MNDX  */

typedef struct
{
   unsigned  char  sig_dig ;  /* The number of significant digits; 52 is zero */
   unsigned  char  digit_info ; /* contains one, len and sign */
   unsigned  char  bcd[10] ;
}  C4BCD ;

#ifdef __cplusplus
   extern "C" {
#endif

void   S4FUNCTION date4assign( char S4PTR *, long ) ;
long   S4FUNCTION date4long( char S4PTR * ) ;  /* Date Arithmetic */
char S4PTR * S4FUNCTION date4cdow( char S4PTR * ) ;
char S4PTR * S4FUNCTION date4cmonth( char S4PTR * ) ;
int    S4FUNCTION date4day( char S4PTR * ) ;
int    S4FUNCTION date4dow( char S4PTR * ) ;
void   S4FUNCTION date4format( char S4PTR *, char S4PTR *, char S4PTR * ) ;/* 'dt' may be 'result'*/
double S4FUNCTION date4format_mdx( char S4PTR * ) ;
int    S4FUNCTION date4format_mdx2( char S4PTR * , double S4PTR * ) ;
void   S4FUNCTION date4init( char S4PTR *, char S4PTR *, char S4PTR * ) ;
int    S4FUNCTION date4month( char S4PTR * ) ;
void   S4FUNCTION date4today( char S4PTR * ) ;
int    S4FUNCTION date4year( char S4PTR * ) ;
void   S4FUNCTION date4time_now( char S4PTR * ) ;

int    S4FUNCTION b4skip( B4BLOCK S4PTR *, long ) ;
#ifdef S4INDEX_VERIFY
   int S4FUNCTION b4verify( B4BLOCK *b4 ) ;
#endif
#ifdef S4NDX
   int S4FUNCTION b4get_last_key( B4BLOCK S4PTR *,char S4PTR * ) ;
   int S4FUNCTION i4get_last_key( TAG4 S4PTR *,char S4PTR *, long ) ;
#endif

int    S4FUNCTION expr4start( CODE4 S4PTR * ) ;

#ifdef S4FOX
int    S4FUNCTION b4calc_blanks( char S4PTR *, int, char ) ;
int    S4FUNCTION b4calc_dups( char S4PTR *, char S4PTR *, int ) ;
void   S4FUNCTION b4go( B4BLOCK S4PTR *, int ) ;
int    S4FUNCTION b4insert_leaf( B4BLOCK S4PTR *, void S4PTR *, long ) ;
int    S4FUNCTION b4insert_branch( B4BLOCK S4PTR *, void S4PTR *, long, long, char ) ;
void   S4FUNCTION b4leaf_init( B4BLOCK S4PTR * ) ;
int    S4FUNCTION b4leaf_seek( B4BLOCK S4PTR *, char S4PTR *, int ) ;
int    S4FUNCTION b4reindex( B4BLOCK S4PTR * ) ;
void   S4FUNCTION b4remove_leaf( B4BLOCK S4PTR * ) ;
void   S4FUNCTION b4br_replace( B4BLOCK S4PTR *, char S4PTR *, long ) ;
void   S4FUNCTION b4top( B4BLOCK S4PTR * ) ;
int    S4CALL     t4desc_memcmp( S4CMP_PARM, S4CMP_PARM, size_t ) ;
void   S4FUNCTION t4branch_split( TAG4 S4PTR *, B4BLOCK S4PTR *, B4BLOCK S4PTR * ) ;
void   S4FUNCTION t4leaf_split( TAG4 S4PTR *, B4BLOCK S4PTR *, B4BLOCK S4PTR * ) ;
int    S4FUNCTION t4r_seek( TAG4 S4PTR *, void S4PTR *, int, long ) ;
int    S4FUNCTION b4r_brseek( B4BLOCK S4PTR *, char S4PTR *, int, long ) ;
long   S4FUNCTION x4recno( B4BLOCK S4PTR *, int ) ;
int    S4FUNCTION x4dup_cnt( B4BLOCK S4PTR *, int ) ;
int    S4FUNCTION x4trail_cnt( B4BLOCK S4PTR *, int ) ;
void   S4FUNCTION x4put_info( B4NODE_HEADER S4PTR *, void S4PTR *, long, int, int ) ;
int    S4FUNCTION b4insert( B4BLOCK S4PTR *, void S4PTR *, long, long, char ) ;
int    S4FUNCTION t4init( TAG4 S4PTR *, INDEX4 S4PTR *, long, char S4PTR *) ;
#else
   #ifdef N4OTHER
      void   S4FUNCTION b4append( B4BLOCK S4PTR *, long ) ;
      void   S4FUNCTION b4insert( B4BLOCK S4PTR *, void S4PTR *, long, long ) ;
      #ifdef S4CLIPPER
         int    S4FUNCTION c4clip( char S4PTR *, int ) ;
         void   S4FUNCTION b4append2( B4BLOCK S4PTR *, void S4PTR *, long, long ) ;
         int    S4FUNCTION t4get_replace_entry( TAG4 S4PTR *, B4KEY_DATA S4PTR *, B4BLOCK S4PTR * ) ;
         int    S4FUNCTION t4shrink( TAG4 S4PTR *, long ) ;
      #endif
      int    S4FUNCTION b4room( B4BLOCK S4PTR * ) ;
      int    S4FUNCTION t4close( TAG4 S4PTR * ) ;
      long   S4FUNCTION t4extend( TAG4 S4PTR * ) ;
      int    S4FUNCTION t4lock( TAG4 S4PTR * ) ;
      int    S4FUNCTION t4unlock( TAG4 S4PTR * ) ;
      TAG4  S4PTR *S4FUNCTION t4open( DATA4 S4PTR *, INDEX4 S4PTR *, char S4PTR * ) ;
      int    S4FUNCTION t4update_header( TAG4 S4PTR * ) ;
      int    S4FUNCTION t4do_version_check( TAG4 S4PTR *, int, int ) ;
      TAG4  S4PTR *S4FUNCTION t4create( DATA4 S4PTR *, TAG4INFO S4PTR *, INDEX4 S4PTR * ) ;
      int    S4FUNCTION t4reindex( TAG4 S4PTR * ) ;
      void   S4FUNCTION t4remove_branch( TAG4 S4PTR *, B4BLOCK S4PTR * ) ;
   #else
      void   S4FUNCTION b4insert( B4BLOCK S4PTR *, void S4PTR *, long ) ;
      int    S4FUNCTION t4init( TAG4 S4PTR *, INDEX4 S4PTR *, T4DESC S4PTR * ) ;
   #endif
#endif

B4BLOCK S4PTR *S4FUNCTION b4alloc( TAG4 S4PTR *, long ) ;
int    S4FUNCTION b4flush( B4BLOCK S4PTR * ) ;
void   S4FUNCTION b4free( B4BLOCK S4PTR * ) ;
void   S4FUNCTION b4go_eof( B4BLOCK S4PTR * ) ;
B4KEY_DATA S4PTR * S4FUNCTION b4key( B4BLOCK S4PTR *, int ) ;
unsigned char S4PTR * S4FUNCTION b4key_key( B4BLOCK S4PTR *, int ) ;
int    S4FUNCTION b4lastpos( B4BLOCK S4PTR * ) ;
int    S4FUNCTION b4leaf( B4BLOCK S4PTR * ) ;
long   S4FUNCTION b4recno( B4BLOCK S4PTR *, int ) ;
void   S4FUNCTION b4remove( B4BLOCK S4PTR *);
int    S4FUNCTION b4seek( B4BLOCK S4PTR *, char S4PTR *, int ) ;

double S4FUNCTION c4atod( char S4PTR *, int) ;
int    S4FUNCTION c4atoi( char S4PTR *, int) ;
long   S4FUNCTION c4atol( char S4PTR *, int) ;

void   S4FUNCTION c4dtoa_clipper( double, char S4PTR *, int, int ) ;
void   S4FUNCTION c4dtoa45( double, char S4PTR *, int, int) ;
void   S4FUNCTION c4encode( char S4PTR *, char S4PTR *, char S4PTR *, char S4PTR *) ;
void   S4FUNCTION c4lower( char S4PTR * ) ;
void   S4FUNCTION c4ltoa45( long, char S4PTR *, int) ;
void   S4FUNCTION c4trim_n( char S4PTR *, int ) ;
void   S4FUNCTION c4upper( char S4PTR * ) ;

#ifdef S4VBASIC
   int c4parm_check( void S4PTR *, int, char S4PTR * ) ;
#endif

int    S4FUNCTION d4changed( DATA4 S4PTR *, int ) ;
char S4PTR * S4FUNCTION d4alias( DATA4 S4PTR * ) ;
void   S4FUNCTION d4alias_set( DATA4 S4PTR *, char S4PTR * ) ;
int    S4FUNCTION d4append( DATA4 S4PTR * ) ;
int    S4FUNCTION d4append_data( DATA4 S4PTR * ) ;
int    S4FUNCTION d4append_blank( DATA4 S4PTR * ) ;
int    S4FUNCTION d4append_start( DATA4 S4PTR *, int ) ;
void   S4FUNCTION d4blank( DATA4 S4PTR * ) ;
int    S4FUNCTION d4bof( DATA4 S4PTR * ) ;
int    S4FUNCTION d4bottom( DATA4 S4PTR * ) ;
int    S4FUNCTION d4check( DATA4 S4PTR * ) ;
int    S4FUNCTION d4close( DATA4 S4PTR * ) ;
int    S4FUNCTION d4close_all( CODE4 S4PTR * ) ;
DATA4 S4PTR * S4FUNCTION d4create( CODE4 S4PTR *, char S4PTR *, FIELD4INFO S4PTR *, TAG4INFO S4PTR * ) ;
DATA4 S4PTR * S4FUNCTION d4data( CODE4 S4PTR *, char S4PTR * ) ;
void   S4FUNCTION d4delete( DATA4 S4PTR * ) ;
int    S4FUNCTION d4deleted( DATA4 S4PTR * ) ;
int    S4FUNCTION d4eof( DATA4 S4PTR * ) ;
FIELD4 S4PTR * S4FUNCTION d4field( DATA4 S4PTR *, char S4PTR * ) ;
FIELD4INFO S4PTR * S4FUNCTION d4field_info( DATA4 S4PTR * ) ;
FIELD4 S4PTR * S4FUNCTION d4field_j( DATA4 S4PTR *, int ) ;
int    S4FUNCTION d4field_number( DATA4 S4PTR *, char S4PTR * ) ;
int    S4FUNCTION d4flush( DATA4 S4PTR * ) ;
int    S4FUNCTION d4flush_data( DATA4 S4PTR * ) ;
int    S4FUNCTION d4flush_files( CODE4 S4PTR * ) ;
int    S4FUNCTION d4free_blocks( DATA4 S4PTR * ) ;
int    S4FUNCTION d4go( DATA4 S4PTR *, long ) ;
int    S4FUNCTION d4go_data( DATA4 S4PTR *, long ) ;
int    S4FUNCTION d4go_eof( DATA4 S4PTR * ) ;
INDEX4 S4PTR * S4FUNCTION d4index( DATA4 S4PTR *, char S4PTR * ) ;
void   S4FUNCTION d4init( CODE4 S4PTR * ) ;
int    S4FUNCTION d4init_undo( CODE4 S4PTR * ) ;
int    S4FUNCTION d4lock( DATA4 S4PTR *, long ) ;
int    S4FUNCTION d4lock_all( DATA4 S4PTR * ) ;
int    S4FUNCTION d4lock_append( DATA4 S4PTR * ) ;
int    S4FUNCTION d4lock_file( DATA4 S4PTR * ) ;
int    S4FUNCTION d4lock_index( DATA4 S4PTR * ) ;
int    S4FUNCTION d4lock_group( DATA4 S4PTR *, long S4PTR *, int ) ;
int    S4FUNCTION d4lock_test( DATA4 S4PTR *, long ) ;
int    S4FUNCTION d4lock_test_file( DATA4 S4PTR * ) ;
int    S4FUNCTION d4lock_test_append( DATA4 S4PTR * ) ;
int    S4FUNCTION d4lock_test_index( DATA4 S4PTR * ) ;
int    S4FUNCTION d4memo_compress( DATA4 S4PTR * ) ;
int    S4FUNCTION d4num_fields( DATA4 S4PTR * ) ;
DATA4 S4PTR *S4FUNCTION d4open( CODE4 S4PTR *, char S4PTR * ) ;
int    S4FUNCTION d4opt_restart( CODE4 S4PTR * ) ;
int    S4FUNCTION d4opt_start( CODE4 S4PTR * ) ;
int    S4FUNCTION d4opt_suspend( CODE4 S4PTR * ) ;
int    S4FUNCTION d4optimize( DATA4 S4PTR *, int ) ;
int    S4FUNCTION d4optimize_write( DATA4 S4PTR *, int ) ;
int    S4FUNCTION d4pack( DATA4 S4PTR * ) ;
int    S4FUNCTION d4pack_data( DATA4 S4PTR * ) ;
int    S4FUNCTION d4position2( DATA4 S4PTR *, double S4PTR * ) ;
double S4FUNCTION d4position( DATA4 S4PTR * ) ;
int    S4FUNCTION d4position_set( DATA4 S4PTR *, double ) ;
int    S4FUNCTION d4read( DATA4 S4PTR *, long, char S4PTR * ) ;
int    S4FUNCTION d4read_old( DATA4 S4PTR *, long ) ;
void   S4FUNCTION d4recall( DATA4 S4PTR * ) ;
long   S4FUNCTION d4reccount( DATA4 S4PTR * ) ;
long   S4FUNCTION d4recno( DATA4 S4PTR * ) ;
char  S4PTR *S4FUNCTION d4record( DATA4 S4PTR * ) ;
long   S4FUNCTION d4record_position( DATA4 S4PTR *, long ) ;
long   S4FUNCTION d4record_width( DATA4 S4PTR * ) ;
int    S4FUNCTION d4refresh( DATA4 S4PTR * ) ;
int    S4FUNCTION d4refresh_record( DATA4 S4PTR * ) ;
int    S4FUNCTION d4reindex( DATA4 S4PTR * ) ;
int    S4FUNCTION d4seek( DATA4 S4PTR *, char S4PTR * ) ;
int    S4FUNCTION d4seek_double( DATA4 S4PTR *, double ) ;
int    S4FUNCTION d4seek_n( DATA4 S4PTR *, char S4PTR *, int ) ;
int    S4FUNCTION d4skip( DATA4 S4PTR *, long ) ;
TAG4 S4PTR *S4FUNCTION d4tag( DATA4 S4PTR *, char S4PTR * ) ;
TAG4 S4PTR *S4FUNCTION d4tag_default( DATA4 S4PTR * ) ;
TAG4 S4PTR *S4FUNCTION d4tag_next( DATA4 S4PTR *, TAG4 S4PTR * ) ;
TAG4 S4PTR *S4FUNCTION d4tag_prev( DATA4 S4PTR *, TAG4 S4PTR * ) ;
void   S4FUNCTION d4tag_select( DATA4 S4PTR *, TAG4 S4PTR * ) ;
TAG4 S4PTR *S4FUNCTION d4tag_selected( DATA4 S4PTR * ) ;
int    S4FUNCTION d4top( DATA4 S4PTR * ) ;
int    S4FUNCTION d4unlock( DATA4 S4PTR * ) ;
int    S4FUNCTION d4unlock_data( DATA4 S4PTR * ) ;
int    S4FUNCTION d4unlock_files( CODE4 S4PTR * ) ;
int    S4FUNCTION d4unlock_append( DATA4 S4PTR * ) ;
int    S4FUNCTION d4unlock_file( DATA4 S4PTR * ) ;
int    S4FUNCTION d4unlock_index( DATA4 S4PTR * ) ;
int    S4FUNCTION d4unlock_records( DATA4 S4PTR * ) ;
int    S4FUNCTION d4update( DATA4 S4PTR * ) ;
int    S4FUNCTION d4update_header( DATA4 S4PTR *, int, int ) ;
int    S4FUNCTION d4update_record( DATA4 S4PTR *, int ) ;
int    S4FUNCTION d4validate_memo_ids( DATA4 S4PTR * ) ;
int    S4FUNCTION d4write( DATA4 S4PTR *, long ) ;
int    S4FUNCTION d4write_data( DATA4 S4PTR *, long ) ;
int    S4FUNCTION d4write_keys( DATA4 S4PTR *, long ) ;
int    S4FUNCTION d4zap( DATA4 S4PTR *, long, long ) ;
int    S4FUNCTION d4zap_data( DATA4 S4PTR *, long, long ) ;

void   S4FUNCTION e4exit(CODE4 S4PTR *) ;
void   S4FUNCTION e4exit_test( CODE4 S4PTR * ) ;

int    S4FUNCTION e4( CODE4 S4PTR *, int, char S4PTR * ) ;
int    S4FUNCTION e4code( CODE4 S4PTR * ) ;
int    S4FUNCTION e4describe( CODE4 S4PTR * ,int, char S4PTR *, char S4PTR *, char S4PTR * ) ;
void   S4FUNCTION e4hook( CODE4 S4PTR *, int, char S4PTR *, char S4PTR *, char S4PTR * ) ;
int    S4FUNCTION e4set( CODE4 S4PTR *, int ) ;
void   S4FUNCTION e4severe( int, char S4PTR * ) ;
char * S4FUNCTION e4text( int err_code ) ;
#ifdef S4VBASIC
   void   S4FUNCTION  e4severe_vbasic( int, char S4PTR * ) ;
#endif

void   S4FUNCTION f4assign( FIELD4 S4PTR *, char S4PTR * ) ;
void   S4FUNCTION f4assign_char( FIELD4 S4PTR *, int ) ;
void   S4FUNCTION f4assign_double( FIELD4 S4PTR *, double ) ;
void   S4FUNCTION f4assign_field( FIELD4 S4PTR *, FIELD4 S4PTR * ) ;
void   S4FUNCTION f4assign_int( FIELD4 S4PTR *, int ) ;
void   S4FUNCTION f4assign_long( FIELD4 S4PTR *, long ) ;
void   S4FUNCTION f4assign_n( FIELD4 S4PTR *, char S4PTR *, unsigned ) ;
char S4PTR * S4FUNCTION f4assign_ptr( FIELD4 S4PTR * ) ;
void   S4FUNCTION f4blank( FIELD4 S4PTR * ) ;
int    S4FUNCTION f4char( FIELD4 S4PTR * ) ;
DATA4 S4PTR *S4FUNCTION f4data( FIELD4 S4PTR * ) ;
int    S4FUNCTION f4decimals( FIELD4 S4PTR * ) ;
int    S4FUNCTION f4double2( FIELD4 S4PTR *, double S4PTR * ) ;
double S4FUNCTION f4double( FIELD4 S4PTR * ) ;
int    S4FUNCTION f4int( FIELD4 S4PTR * ) ;
unsigned  S4FUNCTION f4len( FIELD4 S4PTR * ) ;
long   S4FUNCTION f4long( FIELD4 S4PTR * ) ;
char S4PTR * S4FUNCTION f4name( FIELD4 S4PTR * ) ;
unsigned S4FUNCTION f4ncpy( FIELD4 S4PTR *, char S4PTR *, unsigned ) ;
char S4PTR * S4FUNCTION f4ptr( FIELD4 S4PTR * ) ;
char S4PTR * S4FUNCTION f4str( FIELD4 S4PTR * ) ;
int    S4FUNCTION f4true( FIELD4 S4PTR * ) ;
int    S4FUNCTION f4type( FIELD4 S4PTR * ) ;

int    S4FUNCTION file4close( FILE4 S4PTR * ) ;
int    S4FUNCTION file4create( FILE4 S4PTR *, CODE4 S4PTR *, char S4PTR *, int ) ;
int    S4FUNCTION file4flush( FILE4 S4PTR * ) ;
int    S4FUNCTION file4low_flush( FILE4 S4PTR *, int ) ;
long   S4FUNCTION file4len( FILE4 S4PTR * ) ;
int    S4FUNCTION file4len_set( FILE4 S4PTR *, long ) ;
int    S4FUNCTION file4lock( FILE4 S4PTR *, long, long ) ;
int    S4FUNCTION file4lock_hook( CODE4 S4PTR *cb, char S4PTR *file_name, long offset, long num_bytes, int num_tries ) ;
int    S4FUNCTION file4open( FILE4 S4PTR *, CODE4 S4PTR *, char S4PTR *, int ) ;
int    S4FUNCTION file4open_test( FILE4 S4PTR * ) ;
int    S4FUNCTION file4optimize( FILE4 S4PTR *, int, int ) ;
int    S4FUNCTION file4optimize_write( FILE4 S4PTR *, int ) ;
unsigned  S4FUNCTION file4read( FILE4 S4PTR *, long, void S4PTR *, unsigned ) ;
int    S4FUNCTION file4read_all( FILE4 S4PTR *, long, void S4PTR *, unsigned ) ;
int    S4FUNCTION file4read_error( FILE4 S4PTR * ) ;
int    S4FUNCTION file4refresh( FILE4 S4PTR * ) ;
int    S4FUNCTION file4replace( FILE4 S4PTR *, FILE4 S4PTR * ) ;
int    S4FUNCTION file4temp( FILE4 S4PTR *, CODE4 S4PTR *, char S4PTR *, int ) ;
int    S4FUNCTION file4unlock( FILE4 S4PTR *, long, long ) ;
int    S4FUNCTION file4write( FILE4 S4PTR *, long, void S4PTR *, unsigned ) ;

void   S4FUNCTION file4seq_read_init( FILE4SEQ_READ S4PTR *, FILE4 S4PTR *, long, void S4PTR *, unsigned ) ;
unsigned  S4FUNCTION file4seq_read( FILE4SEQ_READ S4PTR *, void S4PTR *, unsigned ) ;
int    S4FUNCTION file4seq_read_all( FILE4SEQ_READ S4PTR *, void S4PTR *, unsigned ) ;

void   S4FUNCTION file4seq_write_init( FILE4SEQ_WRITE S4PTR *, FILE4 S4PTR *, long, void S4PTR *, unsigned ) ;
int    S4FUNCTION file4seq_write( FILE4SEQ_WRITE S4PTR *, void S4PTR *, unsigned ) ;
int    S4FUNCTION file4seq_write_flush( FILE4SEQ_WRITE S4PTR * ) ;
int    S4FUNCTION file4seq_write_repeat( FILE4SEQ_WRITE S4PTR *, long, char ) ;

int    S4FUNCTION i4add_tag( INDEX4 S4PTR *, TAG4INFO S4PTR * ) ;
int    S4FUNCTION i4check( INDEX4 S4PTR * ) ;
int    S4FUNCTION i4close( INDEX4 S4PTR * ) ;
INDEX4 S4PTR *S4FUNCTION i4create( DATA4 S4PTR *, char S4PTR *, TAG4INFO S4PTR * ) ; /* 0 name -> productn */
long   S4FUNCTION i4extend( INDEX4 S4PTR * ) ;   /* Allocates a block at the end of the file */
int    S4FUNCTION i4flush( INDEX4 S4PTR * ) ;
int    S4FUNCTION i4is_production( INDEX4 S4PTR * ) ;
int    S4FUNCTION i4lock( INDEX4 S4PTR * ) ;
INDEX4 S4PTR *S4FUNCTION i4open( DATA4 S4PTR *, char S4PTR * ) ;
int    S4FUNCTION i4read_block( FILE4 S4PTR *, long, B4BLOCK S4PTR *, B4BLOCK S4PTR * ) ;
int    S4FUNCTION i4reindex( INDEX4 S4PTR * ) ;
int    S4FUNCTION i4shrink( INDEX4 S4PTR *, long ) ;  /* Returns a block of disk space */
TAG4 S4PTR *S4FUNCTION i4tag( INDEX4 S4PTR *, char S4PTR * ) ;
TAG4INFO *S4FUNCTION i4tag_info( INDEX4 * ) ;
int    S4FUNCTION i4unlock( INDEX4 S4PTR * ) ;
int    S4FUNCTION i4update( INDEX4 S4PTR * ) ;
int    S4FUNCTION i4update_header( INDEX4 S4PTR * ) ;
int    S4FUNCTION t4version_check( TAG4 S4PTR *, int, int ) ;
int    S4FUNCTION i4version_check( INDEX4 S4PTR *, int, int ) ;

void   S4FUNCTION l4add( LIST4 S4PTR *, void S4PTR * ) ;
void   S4FUNCTION l4add_after(   LIST4 S4PTR *, void S4PTR *, void S4PTR * ) ;
void   S4FUNCTION l4add_before( LIST4 S4PTR *, void S4PTR *, void S4PTR * ) ;
void   l4check( LIST4 S4PTR * ) ;
void S4PTR * S4FUNCTION l4first( LIST4 S4PTR * ) ;  /* Returns 0 if none */
void S4PTR * S4FUNCTION l4last( LIST4 S4PTR * ) ;   /* Returns 0 if none */
void S4PTR * S4FUNCTION l4next( LIST4 S4PTR *, void S4PTR * ) ;  /* Returns 0 if none */
void S4PTR * S4FUNCTION l4prev( LIST4 S4PTR *, void S4PTR * ) ;
void S4PTR * S4FUNCTION l4pop( LIST4 S4PTR * ) ;
void   S4FUNCTION l4remove( LIST4 S4PTR *, void S4PTR * ) ;
int    S4FUNCTION l4seek( LIST4 S4PTR *, void S4PTR * ) ;

int    S4FUNCTION l4lock_check( void ) ;
void   S4FUNCTION l4lock_remove( int,long,long) ;
void   S4FUNCTION l4lock_save( int, long, long ) ;

#ifndef S4MFOX
#ifndef S4MNDX
int    memo4file_chain_flush( MEMO4FILE S4PTR *, MEMO4CHAIN_ENTRY S4PTR * ) ;
int    memo4file_chain_skip( MEMO4FILE S4PTR *, MEMO4CHAIN_ENTRY S4PTR * ) ;
int    S4FUNCTION f4memo_check( MEMO4FILE S4PTR * ) ;
#endif  /*  ifndef S4MFOX  */
#endif  /*  ifndef S4MNDX  */

int    memo4file_check( MEMO4FILE S4PTR * ) ;
int    memo4file_create( MEMO4FILE S4PTR *, CODE4 S4PTR *, DATA4 S4PTR *, char S4PTR * );
          /* if (name == 0), it is a temporary file */
int    memo4file_dump( MEMO4FILE S4PTR *, long, char S4PTR *, unsigned ) ;
int    memo4file_open( MEMO4FILE S4PTR *, DATA4 S4PTR *, char S4PTR * ) ;
int    memo4file_read( MEMO4FILE S4PTR *, long , char S4PTR * S4PTR *, unsigned S4PTR * ) ;
int    memo4file_read_part( MEMO4FILE S4PTR *, long , char S4PTR * S4PTR *, unsigned S4PTR *, unsigned long, unsigned ) ;
int    memo4file_write( MEMO4FILE S4PTR *, long S4PTR *, char S4PTR *, unsigned ) ;
int    memo4file_write_part( MEMO4FILE S4PTR *, long S4PTR *, char S4PTR *, long, long, unsigned ) ;

int    S4FUNCTION f4memo_assign( FIELD4 S4PTR *, char S4PTR * ) ;
int    S4FUNCTION f4memo_assign_n( FIELD4 S4PTR *, char S4PTR *, unsigned ) ;
unsigned S4FUNCTION f4memo_len( FIELD4 S4PTR * ) ;
unsigned S4FUNCTION f4memo_ncpy( FIELD4 S4PTR *, char S4PTR *, unsigned ) ;
int    S4FUNCTION f4memo_set_len( FIELD4 S4PTR *, unsigned ) ;
char   S4PTR * S4FUNCTION f4memo_str( FIELD4 S4PTR * ) ;
char   S4PTR * S4FUNCTION f4memo_ptr( FIELD4 S4PTR * ) ;

int    S4FUNCTION f4memo_flush( FIELD4 S4PTR * ) ;
void   S4FUNCTION f4memo_free( FIELD4 S4PTR * ) ;
int    S4FUNCTION f4memo_read( FIELD4 S4PTR * ) ;       /* Validates memo id's first */
int    S4FUNCTION f4memo_read_low( FIELD4 S4PTR * ) ;   /* Assumes the current memo id is valid */
void   S4FUNCTION f4memo_reset( FIELD4 S4PTR * ) ;      /* Resets to 'Unknown state' */
int    S4FUNCTION f4memo_update( FIELD4 S4PTR * ) ;
int    S4FUNCTION f4memo_write( FIELD4 S4PTR * ) ;
int    S4FUNCTION memo4file_lock( MEMO4FILE S4PTR * ) ;
int    S4FUNCTION memo4file_unlock( MEMO4FILE S4PTR * ) ;
long   S4FUNCTION memo4len_part( MEMO4FILE S4PTR *, long ) ;

#ifdef S4CLIPPER
int    S4FUNCTION t4balance( TAG4 S4PTR *, B4BLOCK S4PTR *, int ) ;
#endif

void   S4FUNCTION t4descending( TAG4 S4PTR *, int ) ;

int    S4FUNCTION t4add( TAG4 S4PTR *, unsigned char S4PTR *, long ) ;  /* Returns r4unique, r4success, r4repeat */
int    S4FUNCTION t4add_calc( TAG4 S4PTR *, long ) ; /* Calculates expression and adds */
char  *S4FUNCTION t4alias( TAG4 S4PTR * ) ;
B4BLOCK S4PTR *S4FUNCTION t4block( TAG4 S4PTR * ) ;
int    S4FUNCTION t4bottom( TAG4 S4PTR * ) ;
int    S4FUNCTION t4check( TAG4 S4PTR * ) ;
int    S4FUNCTION t4is_descending( TAG4 S4PTR * ) ;
int    S4FUNCTION t4down( TAG4 S4PTR * ) ;
int    S4FUNCTION t4dump( TAG4 S4PTR *, int, int ) ;
int    S4FUNCTION t4eof( TAG4 S4PTR * ) ;
int    S4FUNCTION t4expr_key( TAG4 S4PTR *, char S4PTR * S4PTR * ) ;
int    S4FUNCTION t4flush( TAG4 S4PTR * ) ;
int    S4FUNCTION t4free_all( TAG4 S4PTR * ) ;
int    S4FUNCTION t4free_saved( TAG4 S4PTR * ) ;
int    S4FUNCTION t4go( TAG4 S4PTR *, char S4PTR *, long ) ;
int    S4FUNCTION t4go2( TAG4 S4PTR *, char S4PTR *, long ) ;
void   S4FUNCTION t4init_seek_conv( TAG4 S4PTR *, int) ;    /* Initialize 'stok' and 'dtok' */
char S4PTR *S4FUNCTION t4key( TAG4 S4PTR * ) ;
B4KEY_DATA S4PTR *S4FUNCTION t4key_data( TAG4 S4PTR * ) ;              /* The current key */
void   S4FUNCTION t4out_of_date( TAG4 S4PTR * ) ;
int    S4FUNCTION t4position2( TAG4 S4PTR *, double S4PTR * ) ;
double S4FUNCTION t4position( TAG4 S4PTR * ) ;              /* Returns the position as a percent */
double S4FUNCTION t4positionDbl( TAG4 S4PTR * ) ;              /* Returns the position as a percent */
int    S4FUNCTION t4position_set( TAG4 S4PTR *, double ) ;  /* Positions a percentage */
long   S4FUNCTION t4recno( TAG4 S4PTR * ) ;
int    S4FUNCTION t4remove_current( TAG4 S4PTR * ) ;        /* Remove the current key */
int    S4FUNCTION t4remove( TAG4 S4PTR *, char S4PTR *, long ) ;  /* Remove specified key */
int    S4FUNCTION t4remove_calc( TAG4 S4PTR *, long ) ; /* Calculates expression and removes */
int    S4FUNCTION t4rl_bottom( TAG4 S4PTR * ) ;
int    S4FUNCTION t4rl_top( TAG4 S4PTR * ) ;
int    S4FUNCTION t4seek( TAG4 S4PTR *, void S4PTR *, int ) ;    /* r4success, r4found, r4after, r4eof */
#ifdef S4HAS_DESCENDING
long   S4FUNCTION t4dskip( TAG4 *, long ) ;
#endif
long   S4FUNCTION t4skip( TAG4 S4PTR *, long ) ;
#ifdef S4CLIPPER
B4BLOCK S4PTR *S4FUNCTION t4split( TAG4 S4PTR *, B4BLOCK S4PTR *, int ) ;
#else
B4BLOCK S4PTR *S4FUNCTION t4split( TAG4 S4PTR *, B4BLOCK S4PTR * ) ;
#endif
int    S4FUNCTION t4top( TAG4 S4PTR * ) ;
int    S4FUNCTION t4type( TAG4 S4PTR * ) ;
int    S4FUNCTION t4unique( TAG4 S4PTR * ) ;
int    S4FUNCTION t4up( TAG4 S4PTR * ) ;
int    S4FUNCTION t4update( TAG4 S4PTR * ) ;
int    S4FUNCTION t4up_to_root( TAG4 S4PTR * ) ;

/* File name lengths include one extra null character at end */
void   S4FUNCTION u4delay_sec( void ) ;
int    S4FUNCTION u4name_char( unsigned char ) ;
void   S4FUNCTION u4name_ext( char S4PTR *, int, char S4PTR *, int ) ;
int    S4FUNCTION u4name_ret_ext( char S4PTR *, int, char S4PTR * ) ;
void   S4FUNCTION u4name_piece( char S4PTR *, int, char S4PTR *, int, int ) ;
int    S4FUNCTION u4name_path( char S4PTR *, int, char S4PTR * ) ;
long   S4FUNCTION u4switch( void ) ;

unsigned S4FUNCTION u4ncpy( char S4PTR *, char S4PTR *, unsigned ) ;
int    S4FUNCTION u4ptr_equal( void S4PTR *, void S4PTR * ) ;
int    S4FUNCTION u4remove( char S4PTR * ) ;
void   S4FUNCTION u4yymmdd( char S4PTR * ) ;
#ifndef S4NO_RENAME
   int    S4FUNCTION u4rename( char S4PTR *, char S4PTR * ) ;
#endif

#ifdef S4CLIPPER
   int   S4FUNCTION c4descend( FIELD4 S4PTR *, char S4PTR *, int ) ;
   char *S4FUNCTION c4descend_str( char S4PTR *, char S4PTR *, int ) ;
   char *S4FUNCTION c4descend_date( char S4PTR *, long, int );
   char *S4FUNCTION c4descend_num( char S4PTR *, char S4PTR *, int ) ;
#endif

void  S4PTR *S4FUNCTION u4alloc( long ) ;
void  S4PTR *S4FUNCTION u4alloc_er( CODE4 S4PTR *, long ) ;
void  S4PTR *S4FUNCTION u4alloc_free( CODE4 S4PTR *, long ) ;
int    S4FUNCTION u4alloc_again( CODE4 S4PTR *, char S4PTR * S4PTR *, unsigned S4PTR *, unsigned ) ;
void   S4FUNCTION u4free( void S4PTR * ) ;
void   S4FUNCTION u4name_make( char S4PTR *, int, char S4PTR *, char S4PTR *, char S4PTR * ) ;
short  S4FUNCTION x4reverse_short( void S4PTR * ) ;
long   S4FUNCTION x4reverse_long( void S4PTR * ) ;

#ifdef S4OS2SEM
#ifdef S4OS2
   int mem4start( CODE4 * ) ;
   void mem4stop( CODE4 * ) ;
#endif
#endif
void  S4PTR *S4FUNCTION mem4alloc( MEM4 S4PTR * ) ;  /* 0 Parm causes 0 return */
void  S4PTR *S4FUNCTION mem4alloc2( MEM4 S4PTR *, CODE4 S4PTR * ) ;  /* 0 Parm causes 0 return */
Y4CHUNK S4PTR *S4FUNCTION mem4alloc_chunk( MEM4 S4PTR * ) ;  /* 0 Parm causes 0 return */
void   S4FUNCTION mem4free( MEM4 S4PTR *, void S4PTR * ) ;
int    S4FUNCTION mem4free_check( int ) ;
void   S4FUNCTION mem4check_memory( void ) ;
void   S4FUNCTION mem4init( void ) ;
#ifdef S4OLD_CODE
MEM4 S4PTR *S4FUNCTION mem4type( int, unsigned, int, int ) ;
#endif
MEM4 S4PTR *S4FUNCTION mem4create( CODE4 S4PTR *, int, unsigned, int, int ) ;
void  S4PTR *S4FUNCTION mem4create_alloc( CODE4 S4PTR *, MEM4 S4PTR * S4PTR *, int, unsigned, int, int ) ;
void   S4FUNCTION mem4release( MEM4 S4PTR * ) ;
void   S4FUNCTION mem4reset( void ) ;

#ifndef S4FOX
#ifndef S4CLIPPER
   int S4CALL c4bcd_cmp( S4CMP_PARM, S4CMP_PARM, size_t ) ;
   int S4CALL t4cmp_doub( S4CMP_PARM, S4CMP_PARM, size_t ) ;
#endif
#endif

#ifdef S4DEBUG_DEV
   int S4FUNCTION file4write_part( char S4PTR *, FILE4 S4PTR *, long, unsigned ) ;
   int S4FUNCTION file4cmp_part( CODE4 S4PTR *, char S4PTR *, FILE4 S4PTR *, long, unsigned ) ;
   int S4FUNCTION file4cmp( FILE4 S4PTR * ) ;
#endif

#ifdef __cplusplus
   }
#endif

#ifdef S4LANGUAGE
   int S4CALL u4memcmp( S4CMP_PARM, S4CMP_PARM, size_t ) ;
#endif

#ifdef S4UNIX
   #ifdef S4MDX
      void c4bcd_from_a( char S4PTR *, char S4PTR *, int ) ;
      void t4str_to_date_mdx( char S4PTR *, char S4PTR *, int ) ;
      void t4no_change_str( char S4PTR *, char S4PTR *, int ) ;
      void c4bcd_from_d( char S4PTR *, double ) ;
      void t4no_change_double( char S4PTR *, double ) ;
   #endif

   #ifdef S4FOX
      void t4str_to_fox( char S4PTR *, char S4PTR *, int ) ;
      void t4dtstr_to_fox( char S4PTR *, char S4PTR *, int ) ;
      void t4no_change_str( char S4PTR *, char S4PTR *, int ) ;
      void t4str_to_log( char S4PTR *, char S4PTR *, int ) ;
      void t4dbl_to_fox( char S4PTR *, double ) ;
   #endif

   #ifdef S4CLIPPER
      void t4str_to_doub( char S4PTR *, char S4PTR *, int ) ;
      void t4no_change_str( char S4PTR *, char S4PTR *, int ) ;
      void t4date_doub_to_str( char S4PTR *, double ) ;
   #endif

   #ifdef S4NDX
      void t4str_to_doub( char S4PTR *, char S4PTR *, int ) ;
      void t4str_to_date_mdx( char S4PTR *, char S4PTR *, int ) ;
      void t4no_change_str( char S4PTR *, char S4PTR *, int ) ;
      void t4no_change_double( char S4PTR *, double ) ;
   #endif
#else
   #ifndef S4FOX
      C4STOK c4bcd_from_a ;
      C4DTOK c4bcd_from_d ;
   #else
      C4STOK t4dtstr_to_fox ;
      C4STOK t4str_to_fox ;
      C4DTOK t4dbl_to_fox ;
   #endif
#endif
