/*------------------------------------------------------------------------------
 *  DelftOnline -- Interactive Client Program
 *  Command Lexical Analyser
 *
 *  Irv.Elshoff@Deltares.NL
 *  24 may 12
 *
 *----------------------------------------------------------------------------*/
/*---- LGPL --------------------------------------------------------------------
 *
 * Copyright (C)  Stichting Deltares, 2011-2013.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation version 2.1.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 *
 * contact: delft3d.support@deltares.nl
 * Stichting Deltares
 * P.O. Box 177
 * 2600 MH Delft, The Netherlands
 *
 * All indications and logos of, and references to, "Delft3D" and "Deltares"
 * are registered trademarks of Stichting Deltares, and remain the property of
 * Stichting Deltares. All rights reserved.
 *
 *------------------------------------------------------------------------------
 * $Id$
 * $HeadURL$
*/

%option noyywrap

%{

#include "dolcli.h"
#include "parse.h"

#define YY_INPUT(BUFFER,SIZE,MAXSIZE) { \
    size_t size = min ((MAXSIZE), (size_t) (Global->input.ep - Global->input.ip)); \
    if (size == 0) { \
        (SIZE) = YY_NULL; \
        } \
    else { \
        (SIZE) = size; \
        strncpy ((BUFFER), Global->input.ip, size); \
        } \
    Global->input.ip += size; \
    }

#define KEYWORD(T) { \
    yytext[yyleng] = '\0'; \
    yylval.ptr = (void *) yytext; \
    return T; \
    }

%}

COMMENT         "#"[^\n]*
DIGITS          -*[0-9]+
STRING          [^ \t\n]+
WHITESPACE      [ \t]+

%%

[\n]            { return TOK_EOL; }
";"             { return TOK_EOL; }

{COMMENT}       /* eat comments */
{WHITESPACE}    /* eat whitespace */ { if (false) unput ('z'); } /* dummy action to prevent warning about unused yyunput function */

    /* Keywords */

ash             { KEYWORD (TOK_ASH); }
call            { KEYWORD (TOK_CALL); }
cd              { KEYWORD (TOK_CD); }
desc            { KEYWORD (TOK_DESC); }
dir             { KEYWORD (TOK_DIR); }
elt             { KEYWORD (TOK_ELT); }
func            { KEYWORD (TOK_FUNC); }
get             { KEYWORD (TOK_GET); }
help            { KEYWORD (TOK_HELP); }
ls              { KEYWORD (TOK_LS); }
put             { KEYWORD (TOK_PUT); }
quit            { KEYWORD (TOK_QUIT); }
sleep           { KEYWORD (TOK_SLEEP); }
start           { KEYWORD (TOK_START); }
status          { KEYWORD (TOK_STATUS); }
step            { KEYWORD (TOK_STEP); }
stop            { KEYWORD (TOK_STOP); }
terminate       { KEYWORD (TOK_TERMINATE); }
thr             { KEYWORD (TOK_THR); }

    /* Keyword shortcuts */

"?"             { KEYWORD (TOK_HELP); }
ZZ              { KEYWORD (TOK_TERMINATE); }
b               { KEYWORD (TOK_STOP); }
g               { KEYWORD (TOK_START); }
n               { KEYWORD (TOK_STEP); }
q               { KEYWORD (TOK_QUIT); }

    /* Variable-valued tokens */

{DIGITS}        { yylval.val = atoi (yytext); return (TOK_INTEGER); }
{STRING}        { yytext[yyleng] = '\0' ; yylval.ptr = (void *) yytext; return (TOK_STRING); }

    /* Miscellaneous */

<<EOF>>         { return TOK_ENDOFINPUT; }
