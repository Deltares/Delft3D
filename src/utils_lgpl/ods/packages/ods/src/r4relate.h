/* r4relate.h   (c)Copyright Sequiter Software Inc., 1992-2024.  All rights reserved. */

#define relate4filter_record 101
#define relate4do_remove 102
#define relate4skipped 104
#define relate4blank 105
#define relate4skip_rec 106
#define relate4terminate 107
#define relate4exact 108
#define relate4scan 109
#define relate4approx 110
#define relate4sort_skip 120
#define relate4sort_done 121

struct DATA4LIST_ST ;
struct RELATE4_ST ;
struct RELATION4_ST ;

typedef struct
{
   struct DATA4LIST_ST *data_list ;
   TAG4 S4PTR *tag ;  /* The tag whose expression could replace the sub-expression */
} E4INFO_REPORT ;

typedef struct
{
   EXPR4 S4PTR *expr ;
   E4INFO_REPORT S4PTR *info_report ;  /* One per E4INFO entry */
   struct RELATION4_ST S4PTR *relation ;
   CODE4 S4PTR *code_base ;
   char  S4PTR *buf ;
   unsigned buf_pos, buf_len ;
} L4LOGICAL ;

typedef struct DATA4LIST_ST
{
    struct RELATE4_ST S4PTR * S4PTR *pointers ;
    unsigned mem_allocated ;
    int pointers_tot ;
    int pointers_used ;
} DATA4LIST ;

typedef struct RELATE4_ST
{
   LINK4 link ;

   EXPR4 S4PTR *master_expr ;     /* master expression */
   int     match_len ;      /* Number of characters to match */
   TAG4  S4PTR *data_tag ;        /* Null means record # lookup */
   int     relation_type, sort_type ;  /* relate4exact, relate4scan, relate4approx */
                            /* Top data file is considered to be a Scan */
   int     error_action ;   /* relate4blank, relate4skip_rec, relate4terminate */
   char   S4PTR *scan_value ;     /* Allocated Memory */
   int     scan_value_len ;

   int is_read ;   /*  0 - Neither this data file nor its slaves read */
                   /*  1 - This data file read, slaves unknown */
   DATA4 S4PTR *data ;   /* This is the master for 'slaves' (if any) and */
                   /* possibly the slave for another data file. */
   struct RELATE4_ST S4PTR *master ;
   LIST4  slaves ;

   F4FLAG  set ;  /* Specify records for bitmap optimizable sub-expression. */
   struct RELATION4_ST S4PTR *relation ;
   CODE4 S4PTR *code_base ;
   char S4PTR *old_record;
} RELATE4 ;

typedef struct
{
   LINK4 link ;
   RELATE4 S4PTR *ptr ;
} RELATE4LIST ;

typedef struct
{
   LINK4 link ;
   DATA4 S4PTR *data ;
   RELATE4 S4PTR *relate ;
} R4DATA_LIST ;

typedef struct RELATION4_ST
{
   RELATE4  relate ;

   LIST4 sort_data_list ;  /* data list for the sort */ 
   LIST4  relate_list ;  /* A list of scan Relate pointers; */
                        /* Includes the top level Relate */
   char S4PTR *expr_source ;

   char locked ;

   SORT4 sort ;
   char S4PTR *sort_source ;
   char S4PTR *other_data, bitmaps_freed, sort_eof_flag, sort_done_flag ;
   int  sort_other_len ;
   long sort_rec_to, sort_rec_on, sort_file_pos, sort_rec_count ;
   FILE4 sorted_file ;
   char sorted_file_name[13] ;

   L4LOGICAL log ;
   int is_initialized ;
   char in_sort ;       /* are we skipping for sorting purposes? */
   char skip_backwards ;   /* is backwards skipping enabled? */

   MEM4 S4PTR *relate_memory ;
   MEM4 S4PTR *relate_list_memory ;
   MEM4 S4PTR *data_list_memory ;
} RELATION4 ;


#define BITMAP4LEAF 0x40
#define BITMAP4AND  0x20

typedef struct CONST4_st
{
   LINK4  link ;
   int    offset ;
   int    len ;
} CONST4 ;

typedef struct BITMAP4_st
{
   LINK4 link ;
   LINK4 eval ;

   /* flags */
   char  branch ;
   char  and_or ;    /* 1 = and, 2 = or, 0 means neither */
   char  evaluated ;
   char  finished;
   char  save_count ;
   char  flip_base ;
   char  pure_map ;
   char  no_match ;    /* no records match an and case */

   L4LOGICAL  S4PTR *log ;
   RELATE4    S4PTR *relate ;
   struct BITMAP4_st S4PTR *base ;
   int type ;   /* r4num, etc. */

   /* Branch only */
   LIST4 children ;
   TAG4 S4PTR *tag ;

   CONST4 lt ;
   CONST4 le ;
   CONST4 gt ;
   CONST4 ge ;
   CONST4 eq ;
   LIST4 ne ;
} BITMAP4 ;

#ifdef __cplusplus
   extern "C" {
#endif

int S4FUNCTION bitmap4evaluate( L4LOGICAL S4PTR *, int ) ;
BITMAP4 S4PTR * S4FUNCTION bitmap4redistribute( BITMAP4 S4PTR *, BITMAP4 S4PTR *, char ) ;
BITMAP4 S4PTR * S4FUNCTION bitmap4redistribute_leaf( BITMAP4 S4PTR *, BITMAP4 S4PTR *, BITMAP4 S4PTR * ) ;
BITMAP4 S4PTR * S4FUNCTION bitmap4redistribute_branch( BITMAP4 S4PTR *, BITMAP4 S4PTR * ) ;
BITMAP4 S4PTR * S4FUNCTION bitmap4create( L4LOGICAL S4PTR *, RELATE4 S4PTR *, char, char ) ;
void S4FUNCTION bitmap4destroy( BITMAP4 S4PTR * ) ;
BITMAP4 S4PTR * S4FUNCTION bitmap4combine_leafs( BITMAP4 S4PTR *, BITMAP4 S4PTR *, BITMAP4 S4PTR * ) ;
long S4FUNCTION bitmap4seek( BITMAP4 S4PTR *, CONST4 S4PTR *, char, long, int ) ;
void S4FUNCTION bitmap4copy( BITMAP4 S4PTR *, BITMAP4 S4PTR * ) ;

void S4PTR * S4FUNCTION const4return( L4LOGICAL S4PTR *, CONST4 S4PTR * ) ;
int S4FUNCTION const4mem_alloc( L4LOGICAL S4PTR *, unsigned ) ;
int S4FUNCTION const4duplicate( CONST4 S4PTR *, CONST4 S4PTR *, L4LOGICAL S4PTR * ) ;
int S4FUNCTION const4get( CONST4 S4PTR *, BITMAP4 S4PTR *, L4LOGICAL S4PTR *, int ) ;
int S4FUNCTION const4less( CONST4 S4PTR *, CONST4 S4PTR *, BITMAP4 S4PTR * ) ;
int S4FUNCTION const4eq( CONST4 S4PTR *, CONST4 S4PTR *, BITMAP4 S4PTR * ) ;
int S4FUNCTION const4less_eq( CONST4 S4PTR *, CONST4 S4PTR *, BITMAP4 S4PTR * ) ;
void S4FUNCTION const4add_ne( BITMAP4 S4PTR *, CONST4 S4PTR * ) ;
void S4FUNCTION const4delete_ne( LIST4 S4PTR *, CONST4 S4PTR * ) ;


/* functions used to build the sort data list */
int   S4FUNCTION r4data_list_add( LIST4 S4PTR *, DATA4 S4PTR *, RELATE4 S4PTR * ) ;
int   S4FUNCTION r4data_list_find( LIST4 S4PTR *, RELATE4 S4PTR * ) ;
void  S4FUNCTION r4data_list_free( LIST4 S4PTR * ) ;
int   S4FUNCTION r4data_list_build( LIST4 S4PTR *, RELATE4 S4PTR *, EXPR4 S4PTR *, int ) ;

int   S4FUNCTION e4is_constant( E4INFO S4PTR * ) ;
int   S4FUNCTION e4is_tag( E4INFO_REPORT S4PTR *, EXPR4 S4PTR *, E4INFO S4PTR *, DATA4 S4PTR * ) ;

int   S4FUNCTION log4add_to_list( L4LOGICAL S4PTR *, E4INFO S4PTR *, DATA4LIST S4PTR * ) ;
/*static int log4build_database_lists( L4LOGICAL S4PTR * ) ;*/
int   S4FUNCTION log4true( L4LOGICAL S4PTR * ) ;
int   S4FUNCTION log4bitmap_do( L4LOGICAL S4PTR * ) ; /* Do everything regarding bitmaps */
int   S4FUNCTION log4determine_evaluation_order( L4LOGICAL S4PTR * ) ;
int   S4FUNCTION log4swap_entries( L4LOGICAL S4PTR *, int, int ) ;

int   S4FUNCTION data_list4add( DATA4LIST S4PTR *, CODE4 S4PTR *, RELATE4 S4PTR * ) ;
int   S4FUNCTION data_list4expand_from_db_tree( DATA4LIST S4PTR *, CODE4 S4PTR * ) ;
int   S4FUNCTION data_list4is_in( DATA4LIST S4PTR *, RELATE4 S4PTR * ) ;
int   S4FUNCTION data_list4read_records( DATA4LIST S4PTR * ) ;
void  S4FUNCTION data_list4remove( DATA4LIST S4PTR *, DATA4LIST S4PTR * ) ;

void  S4FUNCTION relate4init_relate( RELATE4 S4PTR *, RELATION4 S4PTR *, DATA4 S4PTR *, CODE4 S4PTR * ) ;
int   S4FUNCTION relate4blank_set( RELATE4 S4PTR *, char ) ;       /* Recursive Blank */
int   S4FUNCTION relate4build_scan_list( RELATE4 S4PTR *, RELATION4 S4PTR * ) ;
RELATE4 S4PTR * S4FUNCTION relate4create_slave( RELATE4 S4PTR *, DATA4 S4PTR *,
                               char S4PTR *, TAG4 S4PTR * ) ;
int   S4FUNCTION relate4free_relate( RELATE4 S4PTR *, int ) ;
void  S4FUNCTION relate4changed( RELATE4 S4PTR * ) ;  /* Slave has been added or freed */
RELATE4 S4PTR * S4FUNCTION relate4lookup_relate( RELATE4 S4PTR *, DATA4 S4PTR * ) ;
int   S4FUNCTION relate4lookup( RELATE4 S4PTR *, char ) ;  /* Lookup the record */
int   S4FUNCTION relate4match_len( RELATE4 S4PTR *, int ) ;
int   S4FUNCTION relate4next_record_in_scan( RELATE4 S4PTR * ) ;
int   S4FUNCTION relate4prev_record_in_scan( RELATE4 S4PTR * ) ;
int   S4FUNCTION relate4read_in( RELATE4 S4PTR * ) ;   /* Read a record for this specific data file. */
                            /* Assume Master has already been read ! */
int   S4FUNCTION relate4read_rest( RELATE4 S4PTR *, char ) ;
void  S4FUNCTION relate4set_not_read( RELATE4 S4PTR * ) ;
int   S4FUNCTION relate4sort( RELATE4 S4PTR * ) ;
int   S4FUNCTION relate4lock( RELATE4 S4PTR * ) ;
int   S4FUNCTION relate4unlock( RELATE4 S4PTR * ) ;

int   S4FUNCTION relate4error_action( RELATE4 S4PTR *, int ) ;  /* Set the error action */
int   S4FUNCTION relate4type( RELATE4 S4PTR *, int ) ;          /* Set the relate type */

int   S4FUNCTION relate4do( RELATE4 S4PTR * ) ;
int   S4FUNCTION relate4do_one( RELATE4 S4PTR * ) ;
int   S4FUNCTION relate4skip_enable( RELATE4 S4PTR *, int ) ;

int   S4FUNCTION relate4dbf_in_relation( RELATE4 S4PTR *, DATA4 S4PTR * ) ;


/* RELATION4 functions */
RELATE4 S4PTR * S4FUNCTION relate4init( DATA4 S4PTR * ) ;
int   S4FUNCTION relate4free( RELATE4 S4PTR *, int ) ;
void  S4FUNCTION relate4free_bitmaps( RELATE4 S4PTR * ) ;
int   S4FUNCTION relate4next_scan_record( RELATION4 S4PTR * ) ;
int   S4FUNCTION relate4prev_scan_record( RELATION4 S4PTR * ) ;
int   S4FUNCTION relate4skip( RELATE4 S4PTR *, long ) ;        /* Extended record skip */
int   S4FUNCTION relate4bottom( RELATE4 S4PTR * ) ;
int   S4FUNCTION relate4top( RELATE4 S4PTR * ) ;
int   S4FUNCTION relate4query_set( RELATE4 S4PTR *, char S4PTR * ) ;
int   S4FUNCTION relate4eof( RELATE4 S4PTR * ) ;
int   S4FUNCTION relate4sort_set( RELATE4 S4PTR *, char S4PTR * ) ;
void  S4FUNCTION relate4sort_free( RELATION4 S4PTR *, int ) ;
int   S4FUNCTION relate4sort_next_record( RELATION4 S4PTR * ) ;
int   S4FUNCTION relate4sort_prev_record( RELATION4 S4PTR * ) ;
int   S4FUNCTION relate4sort_get_record( RELATION4 S4PTR *, long ) ;

/* Used in designer, 1 Moved Down, 0 No Move, -X Moved Up X, 2 Done */
int S4FUNCTION relate4next( RELATE4 S4PTR * S4PTR * ) ;

#ifdef __cplusplus
   }
#endif
