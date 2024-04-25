/* s4sort.h   (c)Copyright Sequiter Software Inc., 1990-2024.  All rights reserved. */

struct RELATE4_ST ;

typedef struct
{
   char  S4PTR *ptr ;       /* Pointer to the starting memory */
   unsigned  pos ;    /* The current position withing 'ptr' */
   unsigned  len ;    /* The current data length pointed to by 'ptr' */

   long   disk ;      /* Current Disk Position, Offset from start of 'spool' */
                      /* >= 'spool_bytes' means nothing is on disk. */
   int    spool_i ;   /* The spool number of this spool  */
} S4SPOOL ;

#ifdef __cplusplus
class S4CLASS SORT4
{
public:
#else
typedef struct
{
#endif
   CODE4 S4PTR *code_base ;

   FILE4        file ;
   char          file_name_buf[14] ;
   FILE4SEQ_WRITE   seqwrite ;
   char         S4PTR *seqwrite_buffer ;
   S4SPOOL S4PTR *spool_pointer ;
   unsigned int spools_n ;      /* The current # of "spools" */
   unsigned int spools_max ;    /* The # of "spools" memory is allocated for */

   char S4PTR * S4PTR *pointers ;
   unsigned int pointers_i ;    /* The next pointers to return using 'get'. */
   unsigned int pointers_used ; /* The number of pointers assigned so far. */
   unsigned int pointers_init ; /* The number of pointers initialied so far. */
   unsigned int pointers_max ;  /* The number of pointers for which memory */
                         /* has been allocated. */

   LIST4 pool ;      /* A number of memory pools for sorting */
   MEM4 S4PTR *pool_memory ;
   unsigned int pool_n ;        /* The number of pools */
   unsigned int pool_entries ;  /* Number of record entries in each pool */
   unsigned int sort_len ;

   long     spool_disk_len ; /* The # of bytes in each disk spool */
   unsigned spool_mem_len ;  /* The # of bytes in each memory spool */

   unsigned int info_offset ;   /* The spot to put the corresponding info. */
   unsigned int info_len ;
   unsigned int tot_len ;       /* The total length of each pool entry. */

   long   is_mem_avail ;
   S4CMP_FUNCTION *cmp ;
#ifdef __cplusplus
} ;
#else
} SORT4 ;
#endif

/* The 'len' is passed to the compare function as its third parameter */
/* The compare function returns '(int) 1' iff 'v1>v2'. */
/* s4quick also assumes that there is a record number after the sort data */

#ifdef __cplusplus
   extern "C" {
#endif

void  s4quick( void S4PTR * S4PTR *, int, S4CMP_FUNCTION *, int ) ;
void  s4delete_spool_entry( SORT4 S4PTR * ) ;
int   s4flush( SORT4 S4PTR * ) ;   /* Flushes current entries to disk */
int   sort4get_ptr_ptr( SORT4 S4PTR *, char S4PTR * S4PTR * ) ;
void  sort4init_pointers( SORT4 S4PTR *, char S4PTR *, unsigned ) ;
long  s4needed(SORT4 S4PTR *) ; /* Returns an estimate on the amount of memory needed */
int   s4next_spool_entry(SORT4 S4PTR *) ;

int   S4FUNCTION sort4free( SORT4 S4PTR * ) ;    /* Frees any allocated memory and uninitializes */
int   S4FUNCTION sort4get( SORT4 S4PTR *,  long S4PTR *, void S4PTR * S4PTR *, void S4PTR * S4PTR * ) ;
int   S4FUNCTION sort4get_init( SORT4 S4PTR * ) ;
int   S4FUNCTION sort4get_init_free( SORT4 *, struct RELATE4_ST * ) ;
void  S4FUNCTION sort4get_mem_init( SORT4 * ) ;
int   S4FUNCTION sort4init( SORT4 S4PTR *, CODE4 S4PTR *, int, int ) ;
int   S4FUNCTION sort4init_alloc( SORT4 S4PTR * ) ;
int   S4FUNCTION sort4init_free( SORT4 S4PTR *, CODE4 S4PTR *, int, int, struct RELATE4_ST * ) ;
int   S4FUNCTION sort4init_set( SORT4 S4PTR *, CODE4 S4PTR *, int, int ) ;
int   S4FUNCTION sort4put( SORT4 S4PTR *, long, void S4PTR *, void S4PTR * ) ;
int   S4FUNCTION sort4spools_init( SORT4 S4PTR *, int ) ;

#ifdef __cplusplus
   }
#endif
