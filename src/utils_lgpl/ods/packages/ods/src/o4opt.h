/* o4opt.h   (c)Copyright Sequiter Software Inc., 1990-2024.  All rights reserved. */

#define OPT4AVAIL -1
#define OPT4NONE   0
#define OPT4DBF    1
#define OPT4INDEX  2
#define OPT4OTHER  3

/* OPT4CHECK_RATE must be a minimum of 5 in order to optimize correctly */
#define OPT4CHECK_RATE        10

#define OPT4GO_DBF_MODE        1
#define OPT4SKIP_DBF_MODE      2
#define OPT4SKIP_NDX_MODE      4
#define OPT4SEEK_NDX_MODE      8
#define OPT4APPEND_MODE       16
#define OPT4WRITE_KEYS_MODE   32
#define OPT4REINDEX_MODE      64
#define OPT4START_MODE       128

/* factor of 2 that estimates the hash distribution, #slots = between #blocks * OPT4BLOCK_DENSITY and #blocks * OPT4BLOCK_DENSITY * 2 */
#define OPT4BLOCK_DENSITY 2L

struct OPT4BLOCK_st ;

/* this structure must match OPT4BLOCK_st with opt4=file followed by pos=pos */
typedef struct
{
   FILE4             S4PTR *file ;
   long              pos ;
} OPT4CMP ;

#ifdef __cplusplus
   extern "C" {
#endif

int    S4FUNCTION c4calc_type( unsigned long ) ;

void   S4FUNCTION d4update_prio( CODE4 S4PTR * ) ;
void   S4FUNCTION file4set_write_opt( FILE4 S4PTR *, int ) ;

void   S4FUNCTION opt4block_clear( OPT4BLOCK S4PTR * ) ;
void   S4FUNCTION opt4block_remove( OPT4BLOCK S4PTR *, int ) ;

void   S4FUNCTION opt4file_delete( FILE4 S4PTR *, long, long ) ;
LIST4 *S4FUNCTION opt4file_find_list( FILE4 S4PTR * ) ;
int    S4FUNCTION opt4file_flush_list( FILE4 S4PTR *, LIST4 S4PTR *, int ) ;
long   S4FUNCTION opt4file_hash( FILE4 S4PTR *, unsigned long ) ;
void   S4FUNCTION opt4file_lru_top( FILE4 S4PTR *, LINK4 S4PTR *, char ) ;
unsigned S4FUNCTION opt4file_read( FILE4 S4PTR *, long, void S4PTR *, unsigned ) ;
OPT4BLOCK *S4FUNCTION opt4file_return_block( FILE4 S4PTR *, long, long ) ;
int    S4FUNCTION opt4file_write( FILE4 S4PTR *, long, unsigned, void S4PTR *, char ) ;

int    S4FUNCTION opt4flush_all( OPT4 S4PTR *, char ) ;
int    S4FUNCTION opt4flush_write_buffer( OPT4 S4PTR * ) ;
void   S4FUNCTION opt4set_priority( OPT4 S4PTR *, char S4PTR * ) ;

#ifdef __cplusplus
   }
#endif
