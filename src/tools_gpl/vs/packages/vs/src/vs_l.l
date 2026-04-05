 /*     Copyright (C)  Stichting Deltares, 2011-2026.

 System      : Viewer/Selector

 $Header: /u/cvsroot/viewsel/progsrc/vs/vs.l,v 1.1 1997/01/30 14:31:11 scherjo Exp $

 Programmer  : Abe.Hoekstra - CSO
 Part        : Lexical.Analyzer

 $Log: vs.l,v $
 * Revision 1.1  1997/01/30  14:31:11  scherjo
 * Clean up
 *
 * Revision 1.9  1993/11/22  14:17:48  hoeks_a
 * location of CVSROOT changed
 *
 * Revision 1.8  1993/09/27  15:20:52  hoeks_a
 * HP9000735 added
 *
 * Revision 1.7  1993/07/27  13:11:47  hoeks_a
 * All log messages removed
 *

 @begin@
 @end@
 */

 /* --defintions-- */
%{
static char rcsid[] = "$Id: vs.l,v 1.1 1997/01/30 14:31:11 scherjo Exp $";

#include <stdio.h>
#include <stdlib.h>
#include "vs_y.h"

extern int    int_value;
extern double  fl_value;

void return_actions(int);

#define retrn(t)  {return_actions(t);return(t);}

%}

%START END

%o 5000

 /* definitie lexicale elementen */
k_disp        [Dd][Ii][Ss][Pp]
k_exec        [Ee][Xx][Ee][Cc]
k_help        ([Hh][Ee][Ll][Pp])|"?"
k_let_        [Ll][Ee][Tt]
k_rele        [Rr][Ee][Ll][Ee]
k_shel        [!][^\n]*
k_use_        [Uu][Ss][Ee]
k_writ        [Ww][Rr][Ii][Tt][Ee]
k_quit        ([Qq][Uu][Ii][Tt])|([Ss][Tt][Oo][Pp])|([Ee][Xx][Ii][Tt])

identifier     [A-Za-z0-9][^\ \t\n{(),;]*
quote_ident    \'[^']*

spc            [ \t]+
stat_end       (\n)|(";")
continu        \\[ \t]*\n
empty          [ \t]+\n
any            .
start_comment  "{"

%% /* --rules-- */

<INITIAL>{k_help} { BEGIN END;
                    retrn(K_HELP);
                  }

<INITIAL>{k_shel} { BEGIN END;
                    retrn(K_SHEL);
                  }

<INITIAL>{k_exec} { BEGIN END;
                    retrn(K_EXEC);
                  }

<INITIAL>{k_disp} { BEGIN END;
                    retrn(K_DISP);
                  }

<INITIAL>{k_let_} { BEGIN END;
                    retrn(K_LET_);
                  }

<INITIAL>{k_rele} { BEGIN END;
                    retrn(K_RELE);
                  }

<INITIAL>{k_use_} { BEGIN END;
                    retrn(K_USE_);
                  }

<INITIAL>{k_writ} { BEGIN END;
                    retrn(K_WRIT);
                  }

<INITIAL>{k_quit} { BEGIN END;
                    retrn(K_QUIT);
                  }


([Ss][Tt][Aa][Tt]) { retrn(K_STAT); }

([Mm][Ee][Mm][Oo]) { retrn(K_MEMO); }

([Tt][Oo])         { retrn(K_TO__); }

([Dd][Ee][Ff])     { retrn(K_DEF_); }

([Ff][Rr][Oo][Mm]) { retrn(K_FROM); }

([Aa][Ll][Ll])     { retrn(K_ALL_); }

([Mm][Aa][Xx])     { retrn(K_MAXM); }

([Aa][Vv][Gg])     { retrn(K_AVG_); }

([Mm][Ii][Nn])     { retrn(K_MINM); }

([Ww][Ii][Tt][Hh]) { retrn(K_WITH); }

([Pp][Aa][Rr])     { retrn(K_PARM); }

([Rr][Ee][Tt][Nn]) { retrn(K_RETN); }

"("                { retrn(K_LBRC); }

")"                { retrn(K_RBRC); }

[-+]?[0-9]+"."[0-9]+  { fl_value = atof((char *)yytext);
                        retrn(K_FLOA);
                      }

[0-9]+             { int_value = atoi((char *)yytext);
                     retrn(K_INTG);
                   }

","                { retrn(K_COMM); }

";"                { retrn(K_SEMI); }

"="                { retrn(K_EQUA); }

"+"|"-"|"*"|"/"    { retrn(K_SIGN); }

{identifier}       { retrn(IDNTFR); }

{quote_ident}      { if (yytext[yyleng-1] == '\\') {
                       /* an 'escaped (\') quote,
                         so remove \ */
                       yyleng--;
                       yymore();
                     }
                     else {
                       /* remove trailing quote */
                       input();
                       retrn(IDNTFR);
                     }
                  }


{stat_end}        { BEGIN 0;
                    retrn(STAT_END);
                  }

{start_comment}   {
                    while (input() != '}');
                  }

{empty}          ;

{spc}            ;

{continu}        ;

{any}            { BEGIN END;
                   retrn(UNKNOWN);
                 }

%% /* --user routines-- */

/*  @@
 */

void return_actions(int t)
{
/*    printf("(%s) (%d)\n",yytext,t); */
}

int yywrap()
{
#ifndef MSDOS
    if (isatty(0)) {
    /* end of file from keyboard, so resume input */
    return 0;
    }
#endif
    return 1;
}
