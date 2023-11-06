//------------------------------------------------------------------------------
//  DelftOnline -- Interactive Client Program
//  Command Parser
//
//  Irv.Elshoff@Deltares.NL
//  24 may 12
//
//------------------------------------------------------------------------------
//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2013.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id:$
// $HeadURL:$

%{
#include "dolcli.h"

#define YYDEBUG 1

void
yyerror (
    const char * str
    );

void
EmitList (
    const char * title,
    DOL::Client::List * list,
    int maxlen
    ) {

    printf ("    %s:\n\t", title);
    int len = 0;
    for (int i = 0 ; i < list->count ; i++) {
        printf (" %s", list->name[i]);
        if ((len += strlen (list->name[i]) + 1) >= maxlen) {
            printf ("\n\t");
            len = 0;
            }
        }

    putchar ('\n');
    }


void
RenderArrayShape (
    DOL::Client::ArrayShape * ash
    ) {

    printf ("ArrayShape: \"%s\" has %d dimension%s: [", ash->pathname, ash->dimensionality, (ash->dimensionality > 1) ? "s" : "");

    long size = 1;
    for (unsigned int i = 0 ; i < ash->dimensionality; i++) {
        printf (" %d", ash->dimensions[i]);
        size *= ash->dimensions[i];
        }

    printf (" ] = %ld element%s\n", size, (size > 1) ? "s" : "");
    }


void
ShowDirectory (
    const char * name
    ) {

    DOL::Client::Directory * dir = Global->dol->GetDirectory (name);
    if (dir == NULL)
        printf ("Directory \"%s\" not found\n", name);

    else {
        printf ("Directory \"%s\":\n", dir->pathname);
        const int maxlen = 60;

        if (dir->subdirs.count > 0)
            EmitList ("Directories", &dir->subdirs, maxlen);
        if (dir->arrays.count > 0)
            EmitList ("ArrayShapes", &dir->arrays, maxlen);
        if (dir->elements.count > 0)
            EmitList ("DataElements", &dir->elements, maxlen);
        if (dir->functions.count > 0)
            EmitList ("Functions", &dir->functions, maxlen);

        delete dir;
        }
    }


//------------------------------------------------------------------------------
%}

%union {
    uint32_t    val;
    void *      ptr;
    }

%type  <ptr> keyword
%type  <ptr> string

%token <ptr> TOK_ASH
%token <ptr> TOK_CALL
%token <ptr> TOK_CD
%token <ptr> TOK_DESC
%token <ptr> TOK_DIR
%token <ptr> TOK_ELT
%token       TOK_ENDOFINPUT
%token       TOK_EOL
%token <ptr> TOK_FUNC
%token <ptr> TOK_GET
%token <ptr> TOK_HELP
%token <val> TOK_INTEGER
%token <ptr> TOK_LS
%token <ptr> TOK_PUT
%token <ptr> TOK_QUIT
%token <ptr> TOK_SLEEP
%token <ptr> TOK_START
%token <ptr> TOK_STATUS
%token <ptr> TOK_STEP
%token <ptr> TOK_STOP
%token <ptr> TOK_STRING
%token <ptr> TOK_TERMINATE
%token <ptr> TOK_THR

%start commands

%%

commands :
    TOK_ENDOFINPUT { return 0; } |
    command TOK_ENDOFINPUT { return 0; } |
    command commands TOK_ENDOFINPUT { return 0; } 
    ;

command:
    TOK_EOL |
    ash_command         TOK_EOL |
    call_command        TOK_EOL |
    cd_command          TOK_EOL |
    desc_command        TOK_EOL |
    dir_command         TOK_EOL |
    elt_command         TOK_EOL |
    func_command        TOK_EOL |
    get_command         TOK_EOL |
    help_command        TOK_EOL |
    ls_command          TOK_EOL |
    put_command         TOK_EOL |
    quit_command        TOK_EOL |
    sleep_command       TOK_EOL |
    start_command       TOK_EOL |
    status_command      TOK_EOL |
    step_command        TOK_EOL |
    stop_command        TOK_EOL |
    terminate_command   TOK_EOL |
    thr_command         TOK_EOL |

    error TOK_ENDOFINPUT {
        yyclearin;
        yyerrok;
        return 0;
        }
    ;

ash_command:
    TOK_ASH string {
        char * name = (char *) $2;
        DOL::Client::ArrayShape * ash = Global->dol->GetArrayShape (name);
        if (ash == NULL)
            printf ("ArrayShape \"%s\" not found\n", name);
        else {
            RenderArrayShape (ash);
            delete ash;
            }
        }
    ;

call_command:
    TOK_CALL string TOK_INTEGER {
        }
    ;

cd_command:
    TOK_CD string {
        char * name = (char *) $2;
        char * dir = Global->dol->ChangeDirectory (name);
        if (dir == NULL)
            printf ("Directory \"%s\" not found\n", (name));
        else {
            printf ("Now in \"%s\"\n", dir);
            free (dir);
            }
        } |
    TOK_CD {
        char * dir = Global->dol->PWD ();
        printf ("Now in \"%s\"\n", dir);
        free (dir);
        }
    ;

desc_command:
    TOK_DESC {
        char * description = Global->dol->GetDescription ();
        if (description == NULL)
            printf ("No description\n");
        else {
            printf ("%s\n", description);
            free (description);
            }
        }
    ;

dir_command:
    TOK_DIR {
        ShowDirectory (".");
        } |
    TOK_DIR string {
        ShowDirectory ((char *) $2);
        }
    ;

elt_command:
    TOK_ELT string {
        char * name = (char *) $2;
        DOL::Client::DataElement * elt = Global->dol->GetDataElement (name);
        if (elt == NULL)
            printf ("DataElement \"%s\" not found\n", name);
        else {
            printf ("DataElement \"%s\" is %s ", elt->pathname, DOL::BaseTypeString (elt->basetype));
            if (elt->arrayshape) {
                printf ("array\n    ");
                RenderArrayShape (elt->arrayshape);
                }
            else
                printf ("scalar\n");
            
            if (elt->description != NULL) printf ("    Description: %s\n", elt->description);
            if (elt->definedon != NULL)   printf ("    Defined on: %s\n", elt->definedon);
            if (elt->units != NULL)       printf ("    Units: %s\n", elt->units);

            delete elt;
            }
        }
    ;

func_command:
    TOK_FUNC string {
        char * name = (char *) $2;
        DOL::Client::Function * func = Global->dol->GetFunction (name);
        if (func == NULL)
            printf ("Function \"%s\" not found\n", name);
        else {
            printf ("Function \"%s\" has context %p\n", func->pathname, func->context);
            if (func->description != NULL) printf ("\tDescription: %s\n", func->description);
            delete func;
            }
        }
    ;

get_command:
    TOK_GET string {
        printf ("1arg\n\"%s\"\n", (char *) $2);
        GetElement ((char *) $2, NULL);
        } |
    TOK_GET string string {
        printf ("2arg\n\"%s\"\n\"%s\"\n", (char *) $2, (char *) $3);
        GetElement ((char *) $2, (char *) $3);
        }
    ;

help_command:
    TOK_HELP {
        printf ("Client commands:  (abbreviation follows /)\n");
        printf ("    ash <name>         Print array shape info\n");
        printf ("    call <func> <arg>  Call a function with the specified integer argument\n");
        printf ("    cd [<dir>]         Change (or print) current directory\n");
        printf ("    desc               Print DOL server description\n");
        printf ("    dir <name>         Print directory info\n");
        printf ("    elt <name>         Print data element info\n");
        printf ("    func <name>        Print function info\n");
        printf ("    get <name> [<f>]   Get data element contents (and possible write to file)\n");
        printf ("    help/?             Print this command summary\n");
        printf ("    ls [<dir>]         List current or specified directory\n");
        printf ("    put <name> <val>   Write a scalar data element\n");
        printf ("    quit/q             Terminate client\n");
        printf ("    sleep <msec>       Pause client for specified number of seconds\n");
        printf ("    sleep <m> <n>      Pause client for random number of milliseconds between <m> and <n>\n");
        printf ("    start/g            Tell server to resume (go)\n");
        printf ("    status             Print server status\n");
        printf ("    step/n [<n>]       Resume server for next <n> milestone units; default 1\n");
        printf ("    stop/b             Pause (brake) server indefinitely\n");
        printf ("    terminate/ZZ       Terminate client and server\n");
        printf ("    thr                Print server threads\n");
        }
    ;

ls_command:
    TOK_LS {
        ListDirectory (".");
        } |
    TOK_LS string {
        ListDirectory ((char *) $2);
        }
    ;

put_command:
    TOK_PUT string {
        }
    ;

quit_command:
    TOK_QUIT {
        Global->input.quit = true;
        return 0;   // cause yyparse to return, exiting the command loop        
        }
    ;

sleep_command:
    TOK_SLEEP TOK_INTEGER {
        usleep (1000 * $2);
        } |
    TOK_SLEEP TOK_INTEGER TOK_INTEGER {
        double r = drand48 ();
        int duration = (int) round ($2 + r * ($3 -$2));
        printf ("Sleeping for %d ms\n", duration);
        usleep (1000 * duration);
        }
    ;

start_command:
    TOK_START {
        bool success = Global->dol->Start ();
        printf ("Start %s\n", success ? "succeeds" : "fails");
        }
    ;

status_command:
    TOK_STATUS {
        DOL::Status * status = Global->dol->ServerStatus ();
        if (status == NULL)
            printf ("Cannot get status\n");
        else {
            printf ("AllowControl: %s\n", status->allowControl ? "true" : "false");
            printf ("Running: %s\n", status->running ? "true" : "false");
            printf ("Milestone: %d\n", status->milestone);
            printf ("Generation: %d\n", status->generation);
            printf ("Distance: %d\n", status->distance);
            delete status;
            }
        }
    ;

step_command:
    TOK_STEP {
        DOL::Milestone milestone = Global->dol->Step (1);
        if (milestone > 0)
            printf ("Stopped at milestone %d\n", milestone);
        else
            printf ("Step fails\n");
        } |
    TOK_STEP TOK_INTEGER {
        DOL::Milestone milestone = Global->dol->Step ($2);
        if (milestone > 0)
            printf ("Stopped at milestone %d\n", milestone);
        else
            printf ("Step fails\n");
        }
    ;

stop_command:
    TOK_STOP {
        DOL::Milestone milestone = Global->dol->Stop ();
        if ((int) milestone > 0)
            printf ("Stopped at milestone %d\n", milestone);
        else {
            DOL::Status * status = Global->dol->ServerStatus ();
            if (status != NULL && status->running == false)
                printf ("Already stopped\n");
            else
                printf ("Stop fails; status fails\n");
            }
        }
    ;

terminate_command:
    TOK_TERMINATE {
        bool success = Global->dol->Terminate ();
        if (success) {
            Global->input.quit = true;
            return 0;   // cause yyparse to return, exiting the command loop        
            }

        printf ("Terminate fails\n");
        }
    ;

thr_command:
    TOK_THR {
        printf ("Threads: ");
        int n = Global->dol->GetThreadCount ();
        printf ("[%d] ", n);

        for (int i = 0 ; i < n ; i++) {
            char * name = Global->dol->GetThreadName (i);
            printf (" %s", name);
            free (name);
            }

        putchar ('\n');
        }
    ;


//------------------------------------------------------------------------------


string:
    TOK_STRING |
    keyword
    ;

keyword:
    TOK_ASH |
    TOK_CALL |
    TOK_CD |
    TOK_DESC |
    TOK_DIR |
    TOK_ELT |
    TOK_FUNC |
    TOK_GET |
    TOK_HELP |
    TOK_LS |
    TOK_PUT |
    TOK_QUIT |
    TOK_SLEEP |
    TOK_START |
    TOK_STATUS |
    TOK_STEP |
    TOK_STOP |
    TOK_TERMINATE |
    TOK_THR
    ;


%%

//------------------------------------------------------------------------------
//  YACC-required functions


void
yyerror (
    const char *  str
    ) {

    printf ("%s on line %d\n", str, Global->input.linenum);
    }
