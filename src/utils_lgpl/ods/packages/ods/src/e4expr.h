/* e4expr.h   (c)Copyright Sequiter Software Inc., 1990-2024.  All rights reserved. */

#define   E4MAX_PARMS          3
#define   E4MAX_STACK_ENTRIES 20

struct TOTAL4st ;

typedef struct
{
   void     (S4PTR *function_ptr)() ;
   char     S4PTR *name ;
   short    code ;
   char     name_len ;
   char     priority ;
   char     return_type ;
   #ifdef S4UNIX
      char  num_parms ;  /* -1 means that the number is flexible */
   #else
      signed char  num_parms ;  /* -1 means that the number is flexible */
   #endif
   char     type[E4MAX_PARMS] ;   /* Parameter types */
}  E4FUNCTIONS ;

typedef struct
{
   char    S4PTR *ptr ;     /* The original string */
   int      pos, len ;      /* Source expression position and length */
} S4SCAN ;

typedef struct
{
   char S4PTR *ptr ;
   unsigned int   pos, len ;
   int   do_extend ;
   CODE4 S4PTR *code_base ;
} S4STACK ;

typedef struct
{
   EXPR4   expr ;
   S4STACK  constants ;
   S4SCAN   scan ;          /* Character Expression */
   S4STACK  op ;            /* Operation stack */
   CODE4  S4PTR *code_base ;
} E4PARSE ;

typedef struct
{
   LINK4 link ;
   EXPR4 S4PTR *expr ;
   struct TOTAL4st *total ; /* If calculation is for a total. */
   char    name[20] ;
   int     cur_result_pos ;
} EXPR4CALC ;

extern char S4PTR *expr4buf ;  /* Pointer to CODE4 Working memory */
/* expr4buf_len no longer exists... */ /* extern unsigned expr4buf_len ;*/ /* Length, e4execute() assumes length long enough */

extern char S4PTR * S4PTR *expr4 ;          /* Expression Parms; Points to next parm to be placed */

extern EXPR4  S4PTR *expr4ptr ;     /* Points to expression being evaluated */
extern E4INFO S4PTR *expr4info_ptr ;
extern char   S4PTR *expr4constants ; /* Points to constant info */

extern E4FUNCTIONS  v4functions[] ;

/* Types normally used for Function/Operator parameters and returns */
#define  r4date      'D'
#define  r4date_doub 'd'
#define  r4log       'L'
#define  r4num       'N'
#define  r4num_doub  'n'
#define  r4str       'C'
#define  r4memo      'M'

/* Codes for Immediate Data in Compile String */
#define  E4FIELD_STR        0
#define  E4FIELD_STR_CAT    1
#define  E4FIELD_LOG        2
#define  E4FIELD_DATE_D     3
#define  E4FIELD_DATE_S     4
#define  E4FIELD_NUM_D      5
#define  E4FIELD_NUM_S      6
#define  E4FIELD_MEMO       7

#define  E4DOUBLE           8
#define  E4STRING           9

#define  E4LOG_LOW         10
#define  E4LOG_HIGH        13

#define  E4LAST_FIELD       7

#define  E4FIRST_LOG       10  /* Range of Logical Operators */
#define  E4LAST_LOG        14
#define  E4FIRST_OPERATOR  15  /* Range of Other Operators */
#define  E4LAST_OPERATOR   51
#define  E4COMPARE_START   26
#define  E4COMPARE_END     46
#define  E4FIRST_FUNCTION  52  /* Start of the List of Functions */

#define E4OR 15
#define E4AND 16
#define E4CONCATENATE 17
#define E4CONCAT_TRIM 18
#define E4CONCAT_TWO 22
#define E4EQUAL 26
#define E4NOT_EQUAL 30
#define E4GREATER_EQ 35
#define E4LESS_EQ 38
#define E4GREATER 41
#define E4LESS 44
#define E4DEL 52
#define E4STR 53
#define E4SUBSTR 54
#define E4TIME 55
#define E4UPPER 56
#define E4DTOS 57
#define E4DTOC 59
#define E4TRIM 61 
#define E4LTRIM 62
#define E4LEFT 63
#define E4IIF 64
#define E4STOD 68
#define E4CTOD 69
#define E4DELETED 77
#define E4RECCOUNT 78
#define E4RECNO 79
#define E4CALC_FUNCTION 81
#define E4TOTAL 82
#define E4DESCEND_STR 84

#define  E4DONE              -2
#define  E4NO_FUNCTION       -3
#define  E4COMMA             -4
#define  E4L_BRACKET         -5
#define  E4ANOTHER_PARM      -6

/* Interface Functions */
#ifdef __cplusplus
extern "C" {
#endif
double  S4FUNCTION expr4double( EXPR4 S4PTR * ) ;
int     S4FUNCTION expr4double2( EXPR4 S4PTR *, double * ) ;
void    S4FUNCTION expr4free( EXPR4 S4PTR * ) ;
int     S4FUNCTION expr4len( EXPR4 S4PTR * ) ;
int     S4FUNCTION expr4key( EXPR4 S4PTR *, char S4PTR * S4PTR * ) ;
int     S4FUNCTION expr4key_convert( EXPR4 S4PTR *, char S4PTR * S4PTR *, int, int ) ;
int     S4FUNCTION expr4key_len( EXPR4 S4PTR * ) ;
EXPR4 S4PTR *S4FUNCTION expr4parse( DATA4 S4PTR *, char S4PTR * ) ;
char S4PTR *S4FUNCTION expr4source( EXPR4 S4PTR * ) ;
int     S4FUNCTION expr4type( EXPR4 S4PTR * ) ;
int     S4FUNCTION expr4true( EXPR4 S4PTR * ) ;
long    S4FUNCTION e4long( EXPR4 S4PTR * ) ;
int     S4FUNCTION e4to_key( EXPR4 S4PTR *, char S4PTR * S4PTR * ) ;
int     S4FUNCTION expr4vary( EXPR4 S4PTR *, char S4PTR * S4PTR * ) ;
#ifdef __cplusplus
}
#endif

/* Parsing Functions */
E4INFO *e4function_add( EXPR4 *, int ) ;
int e4add_constant( E4PARSE *, int, void *, unsigned ) ;

int    e4get_operator( E4PARSE S4PTR *, int S4PTR * ) ;
int    S4FUNCTION e4lookup( char S4PTR *,int,int,int ) ;
int    expr4parse_expr( E4PARSE S4PTR * ) ;
int    expr4parse_function( E4PARSE S4PTR *, char S4PTR *,int ) ;
int    expr4parse_value( E4PARSE S4PTR * ) ;
int    expr4true_check( E4PARSE S4PTR * ) ;

char   s4scan_char( S4SCAN S4PTR * ) ;
void   s4scan_init( S4SCAN S4PTR *, char S4PTR * ) ;
char   s4scan_pop( S4SCAN S4PTR *) ; /* Returns current char and goes to the next */
int    s4scan_search( S4SCAN S4PTR *, char ) ; /* Returns # of characters scanned */
int    s4scan_range( S4SCAN S4PTR *, int, int ) ;

int    s4stack_cur( S4STACK S4PTR * ) ;
int    s4stack_pop( S4STACK S4PTR * ) ;
int    s4stack_push_int( S4STACK S4PTR *, int ) ;
int    s4stack_push_str( S4STACK S4PTR *, void S4PTR *, int ) ;

/* Execute Functions */
void e4add( void ) ;
void e4add_date( void ) ;
void e4and( void ) ;
void e4calc_function( void ) ;
void e4calc_total( void ) ;
void e4concat_special( char ) ;
void e4concat_trim( void ) ;
void e4concat_two( void ) ;
void e4contain( void ) ;
void e4copy_constant( void ) ;
void e4field_copy( void ) ;
void e4copy_parm( void ) ;
void e4ctod( void ) ;
void e4date( void ) ;
void e4day( void ) ;
void e4day_doub( void ) ;
void e4del( void ) ;
void e4deleted( void ) ;
void e4divide( void ) ;
void e4dtoc( void ) ;
void e4dtoc_doub( void ) ;
void e4dtos_doub( void ) ;
void e4equal( void ) ;
void e4false( void ) ;
void e4field_date_d( void ) ;
void e4field_log( void ) ;
void e4field_memo( void ) ;
void e4field_num_d( void ) ;
void e4greater( void ) ;
void e4greater_doub( void ) ;
void e4greater_eq( void ) ;
void e4greater_eq_doub( void ) ;
void e4iif( void ) ;
void e4less( void ) ;
void e4less_doub( void ) ;
void e4less_eq( void ) ;
void e4less_eq_doub( void ) ;
void e4ltrim( void ) ;
void e4month( void ) ;
void e4month_doub( void ) ;
void e4multiply( void ) ;
void e4new_function( void ) ;
void e4nop( void ) ;
void e4not( void ) ;
void e4not_equal( void ) ;
void e4or( void ) ;
void e4field_add( void ) ;
void e4parm_remove( void ) ;
void e4power( void ) ;
void e4reccount( void ) ;
void e4recno( void ) ;
void e4stod( void ) ;
void e4str( void ) ;
void e4sub( void ) ;
void e4sub_date( void ) ;
void e4substr( void ) ;
void e4time( void ) ;
void e4trim( void ) ;
void expr4true_function( void ) ;
void e4upper( void ) ;
void e4val( void ) ;
void e4year( void ) ;
void e4year_doub( void ) ;
void e4pageno( void ) ;
#ifdef S4CLIPPER
   void e4descend_str( void ) ;
   void e4descend_num_doub( void ) ;
   void e4descend_num_str( void ) ;
   void e4descend_date_doub( void ) ;
#endif


#ifdef __cplusplus
   extern "C" {
#endif

/* Relate Module and Report Writer Functions */
int S4FUNCTION expr4compare_flip( int ) ;
int S4FUNCTION expr4execute( EXPR4 S4PTR *, int, void S4PTR * S4PTR * ) ;
void S4FUNCTION expr4functions(E4FUNCTIONS S4PTR * S4PTR *) ;

EXPR4 S4PTR *S4FUNCTION expr4calc_parse( DATA4 S4PTR *, char S4PTR * ) ;
void    S4FUNCTION expr4calc_delete( EXPR4CALC S4PTR * ) ;
void    S4FUNCTION expr4calc_reset( CODE4 S4PTR * ) ;
void    S4FUNCTION expr4calc_massage( EXPR4CALC S4PTR * );
EXPR4CALC *S4FUNCTION expr4calc_create( CODE4 S4PTR *, EXPR4 S4PTR *, char S4PTR * ) ;
EXPR4CALC *S4FUNCTION expr4calc_lookup( CODE4 S4PTR *, char S4PTR *, unsigned ) ;
int     S4FUNCTION expr4calc_name_change( EXPR4 S4PTR * S4PTR *, char S4PTR *, char S4PTR * ) ;
void    S4FUNCTION expr4calc_result_pos( EXPR4CALC S4PTR *, int ) ;

#ifdef __cplusplus
   }
#endif
