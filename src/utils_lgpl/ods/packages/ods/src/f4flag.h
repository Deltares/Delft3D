/* f4flag.h   (c)Copyright Sequiter Software Inc., 1990-2024.  All rights reserved. */

typedef struct
{
   CODE4 S4PTR *code_base ;
   unsigned char S4PTR *flags ;
   unsigned long  num_flags ;
   int      is_flip ;
} F4FLAG ;

#ifdef __cplusplus
   extern "C" {
#endif

unsigned long S4FUNCTION f4flag_get_next_flip( F4FLAG S4PTR *, unsigned long , char ) ;
int  S4FUNCTION f4flag_init( F4FLAG S4PTR *, CODE4 S4PTR *, unsigned long ) ;
int  S4FUNCTION f4flag_set( F4FLAG S4PTR *, unsigned long ) ;
int  S4FUNCTION f4flag_reset( F4FLAG S4PTR *, unsigned long ) ;
int  S4FUNCTION f4flag_is_set( F4FLAG S4PTR *, unsigned long ) ;
int  S4FUNCTION f4flag_is_all_set( F4FLAG S4PTR *, unsigned long, unsigned long ) ;
int  S4FUNCTION f4flag_is_any_set( F4FLAG S4PTR *, unsigned long, unsigned long ) ;
void S4FUNCTION f4flag_set_all( F4FLAG S4PTR * ) ;
int  S4FUNCTION f4flag_set_range( F4FLAG S4PTR *, unsigned long, unsigned long ) ;

/* For Report Module */
void S4FUNCTION f4flag_or( F4FLAG S4PTR *, F4FLAG S4PTR * ) ;
void S4FUNCTION f4flag_and( F4FLAG S4PTR *, F4FLAG S4PTR * ) ;
void S4FUNCTION f4flag_flip_returns( F4FLAG S4PTR * ) ;
int  S4FUNCTION f4flag_is_set_flip( F4FLAG S4PTR *, unsigned long ) ;

#ifdef __cplusplus
   }
#endif

