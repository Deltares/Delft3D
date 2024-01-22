/*	Copyright (C)  Stichting Deltares, 2011-2013.

 System      : Viewer/Selector

 $Header: /u/cvsroot/viewsel/progsrc/vs/vs.y,v 1.2 1997/11/18 13:10:32 mooiman Exp $

 Programmer  : Abe.Hoekstra - CSO
 Part        : ViewSel

 $Log: vs.y,v $
 * Revision 1.2  1997/11/18  13:10:32  mooiman
 * Basic Types
 *
 * Revision 1.1  1997/01/30  14:31:12  scherjo
 * Clean up
 *
 * Revision 1.11  1993/11/22  14:17:49  hoeks_a
 * location of CVSROOT changed
 *
 * Revision 1.10  1993/09/27  15:20:53  hoeks_a
 * HP9000735 added
 *
 * Revision 1.9  1993/07/27  13:11:49  hoeks_a
 * All log messages removed
 *

 @begin@
 @end@
 */

%{
static char rcsid[] = "$Id: vs.y,v 1.2 1997/11/18 13:10:32 mooiman Exp $";

/* include files standaard */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <float.h>

#include "btps.h"
#include "au.h"
#include "fu.h"
#include "wr.h"
#include "vr.h"
/*
#include "st.h"
#include "gen.h"
#include "ex.h"
#include "fm.h"
#include "fu.h"
#include "gr.h"
#include "sm.h"
*/

/*
 * Global variable
 */
extern BInt4 neffds;

void FU_intrinsic ( BInt2   , const BText   , const BText   , BRea8 * );
extern int isatty( int );

/* variabelen die gedeeld worden met de lexical analyser */
double fl_value;
int int_value;
extern char * yytext;    /* lexer text buffer      */
extern int yylex(void);
char *promptstring = ">>";

int ret_val;

static int    grp_uindex[5][3];
static int    elm_uindex[5][3];
static int    lindex[3];
static char * varnms[MAXNUMBEROFVARS+1];
static char * parnms[MAXNUMBEROFPARS+1];
static int    grp_dimcnt=0;
static int    elm_dimcnt=0;
static int    indcnt=0;
static int    varcnt=0;
static int    parcnt=0;


void yyerror (char * s)
{
    (void)fprintf (stderr, "%s\n", s ) ;
    if (!isatty(0)) {
	/* input from file, so exit */
	exit (1);
    }
}

%}

/* miscellanious  */

%start statements
%union {
    int    ival;
    float  fval;
    char * string;
    }

/* tokens */

%token FILE_END 0

/* tokens in de file */
%token K_DEF_ K_DELE K_DISP K_EQUA K_FROM
%token K_INDI K_LET_ K_MACR K_MEMO K_MODI
%token K_OFF_ K_ON__ K_ORDE K_PLAY K_RECO
%token K_RELE K_STAT K_TO__ K_TYPE K_USE_
%token K_WRIT K_EOF_ K_QUIT K_HELP K_SHEL
%token K_LBRC K_RBRC K_COMM K_INTG K_SEMI
%token K_SIGN K_EXEC K_TR2W K_MAXM
%token K_MINM K_TRAG IDNTFR K_FLOA K_WITH
%token K_AVG_ K_PARM K_RETN
%token K_ALL_

%token UNKNOWN

%token STAT_END

 /* types in de parse tree */
 %type <ival>	i_intg ind
 %type <fval>	i_floa
 %type <string>	idntfr i_shel i_sign shell_statement


%%

i_intg		: K_INTG { $$ = (BInt4)int_value; }
		;

i_floa		: K_FLOA { $$ = (float)fl_value; }
		;

idntfr		: IDNTFR { if ( yytext[0] == '\'' ) {
			     /* quoted identifier, trailing
				quote already removed in lexer */
			     $$ = GEN_tekst ( &yytext[1] );
			   }
			   else {
			     $$ = GEN_tekst ( yytext );
			   }
			 }
		;

i_shel		: K_SHEL { $$ = GEN_tekst ( yytext ); }
		;

i_sign		: K_SIGN { $$ = GEN_tekst ( yytext ); }
		;

statements	: statement
		  {
		    (void)fprintf ( stderr, "%s", promptstring );
		  }
		| statements statement
		  {
		    (void)fprintf ( stderr, "%s", promptstring );
		  }
		;

statement	: use_statement
		| disp_statement
		| let_statement
		| write_statement
		| relea_statement
		| exec_statement
		| help_statement
		| shell_statement
		| quit_statement
		| STAT_END
			{ yyerrok; }
		/*| UNKNOWN
		    {
			yyerrok;
			yyclearin;
		    }*/
		| FILE_END
			{ return ( 0 ); }
		| error STAT_END
			{ yyerrok; }
		;

use_statement	: K_USE_ STAT_END
		  {
		    FM_close_nefis_files ();
		  }
		| K_USE_ idntfr K_DEF_ idntfr STAT_END
		  {
		    ret_val=FM_open_nefis_files ( $2, $4 );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$4 );
		    if ( ret_val == 0 ) {
		       (void) SM_read_nefis_meta_data ( neffds );
		    }
		  }
		;

disp_statement	: K_DISP K_STAT STAT_END
		  {
		    FM_display_nefis_file_status (NULL);
		  }
		| K_DISP K_STAT K_TO__ idntfr STAT_END
		  {
		    FM_display_nefis_file_status ($4);
		    GEN_free ( (char *)$4 );
		  }
		| K_DISP K_MEMO STAT_END
		  {
		    PR_variables_info_print ();
		  }
		| K_DISP idntfr STAT_END
		  {
		    PR_print_group_info ( NULL,
			     GR_find_group_in_chain (
			             SM_get_group_pointer (), $2 ));
			GEN_free ( (char *)$2 );
		  }
		;


let_statement	: K_LET_ idntfr K_EQUA idntfr indelm K_FROM idntfr indstat
		  {
		    (void)VR_read_var_from_file (
			    $2, $4, $7, elm_uindex, grp_uindex );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$4 );
		    GEN_free ( (char *)$7 );
		  }
		| K_LET_ idntfr K_EQUA idntfr i_sign idntfr STAT_END
		  {
		    (void)FU_basics ( (char *)$4, (char *)$6,
				      (char *)$2, (char *)$5 );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$4 );
		    GEN_free ( (char *)$5 );
		    GEN_free ( (char *)$6 );
		  }
		| K_LET_ idntfr K_EQUA idntfr i_sign i_floa STAT_END
		  {
		    double value;
		    value = $6;
		    (void)FU_simple ( (char *)$4, (char *)$2,
			             &value, (char *)$5 );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$4 );
		    GEN_free ( (char *)$5 );
		  }
		| K_LET_ idntfr K_EQUA idntfr i_sign i_intg STAT_END
		  {
		    double value;
		    value = $6;
		    (void)FU_simple ( (char *)$4, (char *)$2,
			             &value, (char *)$5 );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$4 );
		    GEN_free ( (char *)$5 );
		  }
		| K_LET_ idntfr K_EQUA K_MAXM idntfr STAT_END
		  {
		    (void)FU_intrinsic ( MAXIMUM, (char *)$5,
				         (char *)$2, NULL );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$5 );
		  }
		| K_LET_ idntfr K_EQUA K_MAXM idntfr i_intg STAT_END
		  {
		    double excl_val;
		    excl_val = $6;
		    (void)FU_intrinsic ( MAXIMUM, (char *)$5,
					 (char *)$2, &excl_val );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$5 );
		  }
		| K_LET_ idntfr K_EQUA K_MAXM idntfr i_floa STAT_END
		  {
		    double excl_val;
		    excl_val = $6;
		    (void)FU_intrinsic ( MAXIMUM, (char *)$5,
				         (char *)$2, &excl_val );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$5 );
		  }
		| K_LET_ idntfr K_EQUA K_MINM idntfr STAT_END
		  {
		    (void)FU_intrinsic ( MINIMUM, (char *)$5,
					 (char *)$2, NULL );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$5 );
		  }
		| K_LET_ idntfr K_EQUA K_MINM idntfr i_intg STAT_END
		  {
		    double excl_val;
		    excl_val = $6;
		    (void)FU_intrinsic ( MINIMUM, (char *)$5,
					 (char *)$2, &excl_val );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$5 );
		  }
		| K_LET_ idntfr K_EQUA K_MINM idntfr i_floa STAT_END
		  {
		    double excl_val;
		    excl_val = $6;
		    (void)FU_intrinsic ( MINIMUM, (char *)$5,
					 (char *)$2, &excl_val );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$5 );
		  }
		| K_LET_ idntfr K_EQUA K_AVG_ idntfr STAT_END
		{
		    (void)FU_intrinsic ( AVERAGE, (char *)$5,
					 (char *)$2, NULL );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$5 );
		}
		| K_LET_ idntfr K_EQUA K_AVG_ idntfr i_intg STAT_END
		{
		    double excl_val;
		    excl_val = $6;
		    (void)FU_intrinsic ( AVERAGE, (char *)$5,
					 (char *)$2, &excl_val );
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$5 );
		}
		| K_LET_ idntfr K_EQUA K_AVG_ idntfr i_floa STAT_END
		{
		    double excl_val;
		    excl_val = $6;
		    (void)FU_intrinsic ( AVERAGE, (char *)$5,
					 (char *)$2, &excl_val);
		    GEN_free ( (char *)$2 );
		    GEN_free ( (char *)$5 );
		}
		| K_LET_ idntfr K_EQUA i_intg STAT_END
		  {
		    (void) FU_set_value ( (char *)$2, (BRea4) $4 );
		    GEN_free ( (char *)$2 );
		  }
		| K_LET_ idntfr K_EQUA i_floa STAT_END
		  {
		    (void)FU_set_value ( (char *)$2, $4 );
		    GEN_free ( (char *)$2 );
		  }
		;

write_statement : K_WRIT varnames STAT_END
		  {
		    int i;

		    (void) WR_write_variables ( (char *)NULL, (char *)NULL, varnms );

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		  }
		|
		  K_WRIT varnames K_TO__ idntfr STAT_END
		  {
		    int i;

		    WR_write_variables ( (char *)$4, (char *)NULL,
			                 varnms );

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		    GEN_free ( (char *)$4 );
		  }
		|
		  K_WRIT varnames K_TO__ idntfr idntfr STAT_END
		  {
		    int i;

		    WR_write_variables ( (char *)$4, (char *)$5,
			                 varnms );

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		    GEN_free ( (char *)$4 );
		    GEN_free ( (char *)$5 );
		  }
		;

relea_statement	: K_RELE idntfr STAT_END
		  {
		    VR_release_variable( $2 );
		    GEN_free ( (char *)$2 );
		  }
		|
		  K_RELE K_ALL_ STAT_END
		  {
		    (void)VR_release_all_variables ();
		  }
		;

exec_statement	: K_EXEC idntfr STAT_END
		  {
		    (void)EX_process ( $2 ,0 );
		    GEN_free ( (char *)$2 );
		  }
		| K_EXEC idntfr K_RETN STAT_END
		  {
		    (void)EX_process ( $2 ,1 );
		    GEN_free ( (char *)$2 );
		  }
		| K_EXEC idntfr K_WITH varnames STAT_END
		  {
		    int i;

		    (void)EX_process_with_vars (
			$2, varnms, 0 );

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		    GEN_free ( (char *)$2 );
		  }
		| K_EXEC idntfr K_WITH varnames K_RETN STAT_END
		  {
		    int i;

		    (void)EX_process_with_vars (
			$2, varnms, 1 );

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		    GEN_free ( (char *)$2 );
		  }
		| K_EXEC idntfr K_PARM parnames STAT_END
		  {
		    int i;

		    (void)EX_process_with_parms ( $2, parnms, 0 );

		    i = 0;
		    while ( parnms[i] != NULL ) {
			GEN_free ( parnms[i] );
			i++;
		    }
		    GEN_free ( (char *)$2 );
		  }
		| K_EXEC idntfr K_PARM parnames K_RETN STAT_END
		  {
		    int i;

		    (void)EX_process_with_parms ( $2, parnms, 1 );

		    i = 0;
		    while ( parnms[i] != NULL ) {
			GEN_free ( parnms[i] );
			i++;
		    }
		    GEN_free ( (char *)$2 );
		  }
		| K_EXEC idntfr K_WITH varnames K_PARM parnames STAT_END
		  {
		    int i;

		    (void)EX_process_with_vars_and_parms (
			$2, varnms, parnms, 0 );

		    i = 0;
		    while ( parnms[i] != NULL ) {
			GEN_free ( parnms[i] );
			i++;
		    }

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		    GEN_free ( (char *)$2 );
		  }
		| K_EXEC idntfr K_WITH varnames K_PARM parnames K_RETN STAT_END
		  {
		    int i;

		    (void)EX_process_with_vars_and_parms (
			$2, varnms, parnms, 1 );

		    i = 0;
		    while ( parnms[i] != NULL ) {
			GEN_free ( parnms[i] );
			i++;
		    }

		    i = 0;
		    while ( varnms[i] != NULL ) {
			GEN_free ( varnms[i] );
			i++;
		    }
		    GEN_free ( (char *)$2 );
		  }
		;

varnames	: idntfr
		  {
		    varcnt = 0;
		    varnms[varcnt++] = (char *)$1;
		    varnms[varcnt  ] = NULL;
		  }
		|
		  varnames idntfr
		  {
		    if ( varcnt == MAXNUMBEROFVARS ) {
			yyerror ( "syntax error" );
			YYERROR;
		    }
		    else {
			varnms[varcnt++] = (char *)$2;
			varnms[varcnt  ] = NULL;
		    }
		  }
		;

parnames	: idntfr
		  {
		    parcnt = 0;
		    parnms[parcnt++] = (char *)$1;
		    parnms[parcnt  ] = NULL;
		  }
		|
		  parnames idntfr
		  {
		    if ( parcnt == MAXNUMBEROFPARS ) {
			yyerror ( "syntax error" );
			YYERROR;
		    }
		    else {
			parnms[parcnt++] = (char *)$2;
			parnms[parcnt  ] = NULL;
		    }
		  }
		;

help_statement	: K_HELP STAT_END
		  {
		    (void)GEN_display_help ();
		  }
		;

shell_statement : i_shel STAT_END
		  {
		    (void)system ( (char *)++$$ );
		    GEN_free ((char *) $1);
		  }
		;

indelm		: K_LBRC elm_indices K_RBRC
		| /* empty */
		  {
		    int i;
		    int j;
		    for ( j=0 ; j<5 ; j++ ) {
			for ( i=0 ; i<3 ; i++ ) {
			    elm_uindex[j][i] = -1;
			}
		    }
		  }
		;

indstat		: K_LBRC grp_indices K_RBRC STAT_END
		| STAT_END
		  {
		    int i;
		    int j;
		    for ( j=0 ; j<5 ; j++ ) {
			for ( i=0 ; i<3 ; i++ ) {
			    grp_uindex[j][i] = -1;
			}
		    }
		  }
		;

grp_indices 	: indp
		  {
		    int i;
		    int j;

		    grp_dimcnt=0;
		    for ( i=0 ; i<3 ; i++) {
			grp_uindex[grp_dimcnt][i] = lindex[i];
		    }
		    for ( j=1 ; j<5 ; j++) {
			for ( i=0 ; i<3 ; i++) {
			    grp_uindex[j][i] = -1;
			}
		    }
		  }
		| grp_indices K_SEMI indp
		  {
		    if ( grp_dimcnt == 4 ) {
			yyerror ( "syntax error" );
			YYERROR;
		    }
		    else {
			int i;

		    	grp_dimcnt++;
		    	for ( i=0 ; i<3 ; i++ ) {
			    grp_uindex[grp_dimcnt][i] = lindex[i]; } }
		  }
		;

elm_indices 	: indp
		  {
		    int i;
		    int j;

		    elm_dimcnt = 0;
		    for ( i=0 ; i<3 ; i++ ) {
			elm_uindex[elm_dimcnt][i] = lindex[i];
		    }
		    for ( j=1 ; j<5 ; j++ ) {
			for ( i=0 ; i<3 ; i++ ) {
			    elm_uindex[j][i] = -1;
			}
		    }
		  }
		| elm_indices K_SEMI indp
		  {
		    if ( elm_dimcnt == 4 ) {
			yyerror ( "syntax error" );
			YYERROR;
		    }
		    else {
			int i;

		    	elm_dimcnt++;
		    	for ( i=0 ; i<3 ; i++ ) {
			    elm_uindex[elm_dimcnt][i] = lindex[i]; } }
		  }
		;

indp		: ind
		  {
		    indcnt = 0;
		    lindex[indcnt] = $1;
		    lindex[1] = lindex[2] = -1;
		  }
		| indp K_COMM ind
		  {
		    if ( indcnt == 2 ) {
			yyerror ( "syntax error" );
			YYERROR;
		    }
		    else {
		    	indcnt++;
		    	lindex[indcnt] = $3;
		    }
		  }
		;

ind		: /* empty */
		  { $$ = -1; }
		| i_intg
		;



quit_statement	: K_QUIT STAT_END
		  {
		    return ( 0 );
		  }
		;

%%
