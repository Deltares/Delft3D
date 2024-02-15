//  Copyright (C)  Stichting Deltares, 2012-2024.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 3,
//  as published by the Free Software Foundation.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program. If not, see <http://www.gnu.org/licenses/>.
//
//  contact: delft3d.support@deltares.nl
//  Stichting Deltares
//  P.O. Box 177
//  2600 MH Delft, The Netherlands
//
//  All indications and logos of, and references to registered trademarks
//  of Stichting Deltares remain the property of Stichting Deltares. All
//  rights reserved.

#include <tchar.h>                   // commentaar voor de FORTRAN programmeerder
#include <string.h>
#include <malloc.h>
#include <stdio.h>
#include <conio.h>
#include <stdlib.h>

enum                                 // automatisch nummertje
{ OUTPUT,
  TITLE,
  COMMENT,
  WATER,
  BOTTOM,
  PARM,
  XT,
  FLOW,
  ZERO,
  FIRST,
  IF,
  ELSE,
  OPEN,
  CLOSE,
  CONTINUE,
} ;

typedef struct                       // derived type
{   char   *tok    ;
    int     ID     ;
    int     type   ;
    char   *name   ;
    char   *trunc  ;
    float   value  ;
    char   *unit   ;
    char   *comment;
} token ;

token t[100000]     ;                // array van 100000 van die derived types
int   it        = 1 ;                // tellertje van ID's ! pas op, tellertje begint bij 1 !!
FILE *infile        ;                // dit is de Duprol- .mod file 'unit'
FILE *fortranfile   ;                //        de FORTRAN .f90 file 'unit'
FILE *asciifile     ;                //        de Delwaq  .asc file 'unit'
int   nowater   = 0 ;                // aantal stoffen in de water fase
int   nobed     = 0 ;                // aantal stoffen op de bodem
int   noitem    = 0 ;                // aantal in en uitvoer items samen
int   noinput   = 0 ;                // aantal invoer items
int   noflux    = 0 ;                // aantal flux items
int   lastinput = 0 ;                // laatste input item (voor mede afdrukken commentaar)
char *modname       ;                // 'naam' in de modfile, voor de naam van de fortran routine
int   iflevel   = 0 ;                // hulpje om nette if-then-else nestings te krijgen

void  parse  ( char * ) ;            // 'C' wil vooraf weten hoe de functies werken
void  header ( FILE * ) ;
void  loop1  ( FILE * ) ;
void  loop2  ( FILE * ) ;
void  loop3  ( FILE * ) ;
int   keyword( char * ) ;
void  breakup( char *, token *, int ) ;
int   findID ( char *, int, int, int ) ;
char *detect ( char * ) ;
void  wfortran () ;
void  wascii   () ;

void main (int argc, char *argv[], char *envp[] )   // hoofdprogramma met run time argumenten
{                                                   // het eerste argument is altijd de naam
    char *modfile = NULL;                           // van deze executable zelf
    char *forfile = NULL;
    char *ascfile = NULL;
    char  line[513] ;                               // dit is de invoer regel, 512 + '\0' is
    char *n         ;                               //           hard gecodeerd !
    int   i         ;
    int   j         ;
    int   maxlen = 0 ;

    switch ( argc )
    {   case 2 :                     // only one argument, should be the .mod file
            modfile = argv[1] ;
            break ;
        case 3 :                     // 2 arguments, first should be -modfile
            if ( strcmp( argv[1], "-modfile"  ) == 0 )
            {   modfile  = argv[2] ;
                break ;
            }
        default:
            cprintf( "use -modfile <file path> to specify DUPROL file\r\n" ) ;
            exit(1) ;
    }
    if ( stricmp( strrchr(modfile,'.'), ".mod" ) != 0 )
    {   cprintf( "the filename of the modfile (%s)should have extension \".mod\"\r\n", modfile ) ;
        exit(1) ;
    }
    
	if ( strrchr(modfile,'\\') != NULL )
    {   i = strrchr(modfile,'.') - strrchr(modfile,'\\') - 1 ;
		if ( i > 10){ i = 10;}
        modname = (char *) malloc ( i + 1 ) ;
        strncpy( modname, strrchr(modfile,'\\') + 1, i ) ;
        modname[i] = '\0' ;
        j = 0; while ( j < i ) {modname[j]=tolower(modname[j]);j++;}
		forfile = (char *) malloc ( strrchr(modfile,'\\') - modfile + 1 + strlen(modname) + 5);
		strncpy( forfile, modfile, strrchr(modfile,'\\') - modfile + 1);
		strncpy( strrchr(forfile,'\\') + 1, modname, i); 
		strcpy(strrchr(forfile,'\\') + 1 + i , ".f90");
		ascfile = (char *) malloc ( strrchr(modfile,'\\') - modfile + 1 + strlen(modname) + 5);
		strncpy( ascfile, modfile, strrchr(modfile,'\\') - modfile + 1);
		strncpy( strrchr(ascfile,'\\') + 1, modname, i); 
		strcpy(strrchr(ascfile,'\\') + 1 + i, ".asc");
    }
    else
    {   i = strrchr(modfile,'.') - modfile ;
		if ( i > 10){i = 10;}
        modname = (char *) malloc ( i + 1 ) ;
        strncpy( modname, modfile, i ) ;
        modname[i] = '\0' ;
        j = 0; while ( j < i ) {modname[j]=tolower(modname[j]);j++;}
		forfile = (char *) malloc ( strlen(modname) + 5);
		strncpy( forfile, modname, i);
		strcpy(forfile + i, ".f90");
		ascfile = (char *) malloc ( strlen(modname) + 5);
		strncpy( ascfile, modname, i);
		strcpy(ascfile + i, ".asc");
    }

    infile      = fopen ( modfile, "r" ) ;
    fortranfile = fopen ( forfile, "w" ) ;
    asciifile   = fopen ( ascfile, "w" ) ;

    n = fgets( line, 512, infile ) ;                                         // Leest een regel Duprol
    while ( n != NULL )                                                      // Doe het volgende tot de file 'op' is
    {   *strchr(line,'\n') = '\0' ;                                          // Haal het 'newline' character weg
        while ( strchr(line,'\x09') != NULL ) *strchr(line,'\x09') = ' ' ;   // Maak ' ' van de tab characters
        for ( i = strlen(line) ; i > 0 ; i-- )
        {   if ( line[i] != ' ' ) { line[i+1] = '\0' ; break ;  }  }         // Haal trailing spaces weg
        for ( i = 0 ; i < (int) strlen(line) ; i++ )
		{   if ( line[i] != ' ' ) break ;  }                                 // Haal leading spaties weg
		if ( strlen(&line[i]) > 2 && line[i] == '}') i=i+2 ;                 // Skip twee tekens } als op een langere regel staat (else-regel)
        if ( strlen(&line[i]) != 0 ) parse ( &line[i] ) ;       // Haal hele spatie regels weg en parse een regel
        n = fgets( line, 512, infile ) ;                                     // Leest een regel Duprol
    }

    wfortran() ;                                                             // write FORTRAN file ;
    wascii  () ;                                                             // write ASCII   file ;
}

void parse ( char *line )
{
    int   i, j, m, n ;
    char *h,*k  ;
    int   end = 0  ;
    char  max[11] ;
    max[10] = '\0' ;

    t[it].name = NULL ;                                           // dit is een null pointer, nog niet gealloceerd
    t[it].ID   = it ;
    t[it].type = keyword(line) ;                                  // deze functie retourneert het type nummer van het keyword
    switch ( t[it].type )
    {   case TITLE:
        case COMMENT:
           t[it].tok = (char *) malloc( 2 ) ;
           strcpy ( t[it].tok, "!" ) ;                            // we gaan '!' in de FORTRAN file zetten
           t[it].comment = (char *) malloc( strlen(line)+1 ) ;    // met daarachter de rest van de regel
           strcpy ( t[it++].comment, line ) ;
           break ;
        case WATER:
           t[it].tok = (char *) malloc( 8 ) ;
           strcpy ( t[it].tok, "real(4)" ) ;                      // we gaan een variabele deklareren
           lastinput = it ;                                       // we zitten nog in het deklaratie deel van DUPROL
           breakup( &line[5], &t[it++], 8 ) ;                     // splitst de deklaratie file in een naam en een comment en
           noitem++ ; noinput++ ; nowater++ ;                     // de default value. Tellertjes verhogen
           break ;                                                // klaar met deze regel
        case BOTTOM:
           t[it].tok = (char *) malloc( 8 ) ;
           strcpy ( t[it].tok, "real(4)" ) ;                      // net als met water, alleen nu met bodem variabele
           lastinput = it ;
           breakup( &line[6], &t[it++], 8 ) ;
           noitem++ ; noinput++ ; nobed++;                        // dus tellertje nobed verhogen
           break ;
        case PARM:
           t[it].tok = (char *) malloc( 8 ) ;                     // net als de vorige 2, maar nu is het geen stof,
           strcpy ( t[it].tok, "real(4)" ) ;                      // maar een input grootheid
           lastinput = it ;
           breakup( &line[4], &t[it++], 10 ) ;
           noitem++ ; noinput++ ;
           break ;
        case XT  :
           t[it].tok = (char *) malloc( 8 ) ;                     // net als de PARM, maar nu een plaats-tijd (XT) functie
           strcpy ( t[it].tok, "real(4)" ) ;
           lastinput = it ;
           breakup( &line[2], &t[it++], 10 ) ;
           noitem++ ; noinput++ ;
           break ;
        case FLOW:
           t[it].tok = (char *) malloc( 8 ) ;                     // of een water flow
           strcpy ( t[it].tok, "real(4)" ) ;
           lastinput = it ;
           breakup( &line[4], &t[it++], 10 ) ;
           noitem++ ; noinput++;
           break ;
        case ZERO:                                                // zero order flux, we zitten nu in de code, dus last input
           t[it].tok = (char *) malloc( 8 ) ;                                      // wordt niet meer verhoogd
           strcpy ( t[it].tok, "real(4)" ) ;
           if ( strchr(line,';') != NULL )                        // one line statement
               *strchr(line,';') = '\0' ;
           else
           {   k = strchr(line,'\0') ;                            // multi-line statement
               strcpy ( k , "   &" ) ;                            // zorg dat in FORTRAN een '&' geprint wordt
           }
           i = strcspn( &line[3],"= ") ;
           t[it].comment = detect ( strchr(line,'=')+1 ) ;        // all after the '=' sign
           t[it].ID    = findID( &line[3], 1, i, 0 )  ;           // find the ID of the state variable
           t[it].name  = (char *) malloc ( strlen(t[t[it].ID].name ) + 3) ;
           strcpy( t[it].name , "D0" ) ; strcpy( &t[it].name[2], t[t[it].ID].name  ) ;
           t[it].trunc = (char *) malloc ( strlen(t[t[it].ID].trunc) + 3) ;
           strcpy( t[it].trunc, "D0" ) ; strcpy( &t[it].trunc[2], t[t[it].ID].trunc ) ;
           if ( findID( t[it].trunc, 0, 10, 1 ) != 0 ) t[it].ID = 0 ;
           else noflux++ ;
           it++ ;
           break ;
        case FIRST:                                               // like with zero order
           t[it].tok = (char *) malloc( 8 ) ;
           strcpy ( t[it].tok, "real(4)" ) ;
           if ( strchr(line,';') != NULL )
               *strchr(line,';') = '\0' ;
           else
           {   k = strchr(line,'\0') ;
               strcpy ( k , "   &" ) ;
               i = strchr(line,'&') - strchr(line,'=') + 1 ;
           }
           i = strcspn( &line[3],"= ") ;
           t[it].comment = detect ( strchr(line,'=')+1 ) ;
           t[it].ID     = findID( &line[3], 1, i, 0 )  ;
           t[it].name  = (char *) malloc ( strlen(t[t[it].ID].name ) + 3) ;
           strcpy( t[it].name , "D1" ) ; strcpy( &t[it].name [2], t[t[it].ID].name  ) ;
           t[it].trunc = (char *) malloc ( strlen(t[t[it].ID].trunc) + 3) ;
           strcpy( t[it].trunc, "D1" ) ; strcpy( &t[it].trunc[2], t[t[it].ID].trunc ) ;
           if ( findID( t[it].trunc, 0, 10, 1 ) != 0 ) t[it].ID = 0 ;
           noflux++ ;
           it++ ;
           break ;
        case IF:                                                  // dit is de DUPROL if .. else constructie
           t[it].tok = (char *) malloc( 3 ) ;                     // wij gaan straks "if ... then ... else ... endif" schrijven
           strcpy ( t[it].tok, "if" ) ;
           t[it++].comment = detect( &line[2] ) ;
           break ;
        case ELSE:
           for ( i = it-1 ; i > 0 ; i-- )
           {   if ( t[i].type == IF ) break ;
               if ( t[i].type == CLOSE )
               {   t[i].type = -1 ;                               // remove the previous close bracket
                   break ;
               }
           }
           t[it].tok = (char *) malloc( 5 ) ;
           strcpy ( t[it].tok, "else" ) ;
           t[it].comment = detect( &line[4] ) ;
           if ( strstr( t[it].comment, " if " ) != NULL ) t[it].ID = -t[it].ID ;  // it is an 'else if', make the ID negative
           it++ ;
           break ;
        case OPEN:                                                // '{' found
           m = (int) strlen(&line[1]) ;
           if ( m == 0 ) break ;                                  // is the only character on the line
           h = line ;
           if ( strchr(line,'}') != NULL )                        // there is a '}' on the same line
           {   *strchr(line,'}') = '\0' ;  end = 1 ;  }           // at the end a separate close item is written
           else
           {   k = strchr(line,'\0') ;                            // the line must be continued
               strcpy ( k , "   &" ) ;                            // on the next line
               m = (int) strlen(&line[1]) ;                       // and has become longer
           }
           k = strchr( line, '=' ) ;
           while ( k != NULL )                                    // loop for multiple instructions on the same line
           {   t[it].type = OUTPUT ;                              // they are split in one instruction per line
               t[it].tok = (char *) malloc( 8 ) ;
               strcpy ( t[it].tok, "real(4)" ) ;
               for ( j = h-line+1 ; j < m ; j++ ) if ( line[j] != ' ' ) break ;  // eliminate spaces after '{' and ';'
               i = strcspn( &line[j],"= ") ;                      // this is the length of the name before '='
               t[it].name = (char *) malloc( i+1 ) ;
               strncpy ( t[it].name, &line[j], i ) ; t[it].name[i] = '\0' ;
               n = findID( t[it].name, 0, i, 0 ) ;                // check the full name ( no bracket, length, look in 'names' )
               if ( n != 0 )                                      // if one is equal, take its trunctation
               {   t[it].trunc = (char *) malloc( i+1 ) ;
                   strcpy ( t[it].trunc, t[n].trunc ) ;           // ID n is location n-1 in array !
                   t[it].ID = n ;
               }
               else                                               // we are unique
               {   if ( i < 11 )                                  // name is short enough
                   {   t[it].trunc = (char *) malloc( i+1 ) ;
                       strcpy ( t[it].trunc, t[it].name ) ;
                   }
                   else
                   {   t[it].trunc = (char *) malloc( 11 ) ;
                       strncpy ( max, &line[j], 10 ) ;
                       if ( findID ( max, 0, 10, 1 ) == 0 )       // check the truncation ( no bracket, length, look in 'truncs' )
                       {   strcpy ( t[it].trunc, max ) ;  }
                       else
                       {   for ( n = 10; n > 0 ; n-- )
                           {   max[n-1] = line[n-1+i-10] ;        // replace from the end the truncation with last characters
                               if ( findID ( max, 0, 10, 1 ) == 0 )
                               {   strcpy ( t[it].trunc, max ) ;
                                   break ;
                   }   }   }   }
                   t[it].ID = it ; noitem++ ;
               }
               h = strchr( &line[j], ';' );                       // this separates multiple instructions on a line
               if ( h != NULL ) *h = '\0' ;                       // close this part of the line
               i = strcspn( &line[j],"=") ;                       // find the '=' sign
               t[it++].comment = detect( &line[i+j+1] ) ;         // evaluate the right hand side in the comment string
               k = NULL ;
               if ( h != NULL ) k = strchr( h+1, '=' ) ;          // look for a further '=' sign after the ';'
           }
           if ( end == 1 )
           {  t[it].name = NULL ;                                 // make an additional close item
              t[it].ID   = it ;                                   // to induce an 'endif' where needed
              t[it++].type = CLOSE ;
              end = 0 ;
           }
           break ;
        case CLOSE:
           it++ ;
           if ( line[1] != '\0' )
           {  if ( keyword( &line[2] ) == ELSE )
              {   t[it].type = ELSE ;
                  for ( i = it-1 ; i > 0 ; i-- )
                  {   if ( t[i].type == IF ) break ;
                      if ( t[i].type == CLOSE )
                      {   t[i].type = -1 ;                        // remove a previous close bracket
                          break ;
                      }
                  }
                  t[it].tok = (char *) malloc( 5 ) ;
                  strcpy ( t[it].tok, "else" ) ;
                  t[it].comment = detect( &line[6] ) ;
                  if ( strstr( t[it].comment, " if " ) != NULL ) t[it].ID = -t[it].ID ;  // it is an 'else if'
                  it++ ;
           }  }
           break ;
        case OUTPUT:                                              // this is like OPEN, but without
           m = (int) strlen(line) ;                               // an opening backet.
           if ( m == 0 ) break ;                                  // NOTE: this can also be a continuation line
           h = line - 1 ;
           k = strchr( line, '=' ) ;                              // in that case the '=' sign is missing
           if ( k == NULL )
           {   t[it].type = CONTINUE ;
               t[it].tok  = (char *) malloc( 1 ) ;
               t[it].tok [0] = '\0' ;
               t[it].name = (char *) malloc( 1 ) ;
               t[it].name[0] = '\0' ;
               if ( strchr(line,';') != NULL )
               {   *strchr(line,';') = '\0' ;
                   end = 1 ;
               }
               else
               {   k = strchr(line,'\0') ;                        // even a continuation line can be continued
                   strcpy ( k , "   &" ) ;
               }
               t[it++].comment = detect( line ) ;
           }
           else                                                   // this is the normal processing like OPEN
           {   if ( strchr(line,'}') != NULL )
               {   *strchr(line,'}') = '\0' ; end = 1 ; }
               else if ( strchr(line,';') == NULL )
               {   k = strchr(line,'\0') ;
                   strcpy ( k , "   &" ) ;
                   m = (int) strlen(line) ;
               }
               while ( k != NULL )
               {   t[it].type = OUTPUT ;
                   t[it].tok = (char *) malloc( 8 ) ;
                   strcpy ( t[it].tok, "real(4)" ) ;              // we will declare this output variable as a real(4)
                   for ( j = h-line+1 ; j < m ; j++ ) if ( line[j] != ' ' ) break ;       // look for first non-blank
                   i = strcspn( &line[j],"= ") ;                  // until next blank or '='
                   if ( i == 2 && strnicmp( &line[j], "fl", 2 ) == 0 )   // we already have a 'FL' variable in Delwaq
                   {   t[it].name = (char *) malloc( 4 ) ; strcpy ( t[it].name , "fl2" ) ; i++ ; } // we name this one 'fl2'
                   else                                           // this is the normal procedure
                   {   t[it].name = (char *) malloc( i+1 ) ;
                       strncpy ( t[it].name, &line[j], i ) ; t[it].name[i] = '\0' ;
                   }
                   n = findID( t[it].name, 0, i, 0 ) ;
                   if ( n != 0 )
                   {   t[it].trunc = (char *) malloc( i+1 ) ;
                       strcpy ( t[it].trunc, t[n].trunc ) ;
                       t[it].ID = n ;
                   }
                   else                                               // we are unique
                   {   if ( i < 11 )
                       {   t[it].trunc = (char *) malloc( i+1 ) ;
                           strcpy ( t[it].trunc, t[it].name ) ;
                       }
                       else
                       {   t[it].trunc = (char *) malloc( 11 ) ;
                           strncpy ( max, &line[j], 10 ) ;
                           if ( findID ( max, 0, 10, 1 ) == 0 )
                           {   strcpy ( t[it].trunc, max ) ;   }
                           else
                           {   for ( n = 10; n > 0 ; n-- )
                               {   max[n-1] = line[n-1+i-10] ;
                                   if ( findID ( max, 0, 10, 1 ) == 0 )
                                   {   strcpy ( t[it].trunc, max ) ;
                                   break ;
                       }   }   }   }
                       t[it].ID = it ; noitem++ ;
                   }
                   h = strchr( &line[j], ';' ) ;
                   if ( h != NULL ) *h = '\0' ;
                   i = strcspn( &line[j],"=") ;
                   t[it++].comment = detect( &line[i+j+1] ) ;
                   k = NULL ;
                   if ( h != NULL ) k = strchr( h+1, '=' ) ;
               }
           }
           if ( end == 1 )
           {  t[it].name = NULL ;
              t[it].ID   = it ;
              t[it++].type = CLOSE ;
              end = 0 ;
           }
    }
}

void wfortran ( )
{   int   i, ifl    ;
    int   iitem     ;
    char  hlp[53]   ;
    int   ipend     ;

    header( fortranfile ) ;
    ipend = 0 ;

//     declarations

    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case TITLE:                         // only the comments of the declaration
            case COMMENT:                       // section ( i < lstinput ) are echoed here
               if ( i < lastinput ) fprintf( fortranfile, "!     %s\n", t[i].comment ) ;
               break ;
            case WATER:
            case BOTTOM:
            case PARM:
            case XT:
            case FLOW:
               if ( t[i].value != -987654.0 )
                   fprintf( fortranfile, "      %s %-17s ! [%10g] %-10s ", t[i].tok, t[i].name, t[i].value, t[i].unit  ) ;
               else
                   fprintf( fortranfile, "      %s %-17s !        %-10s ", t[i].tok, t[i].name,             t[i].unit  ) ;
               fprintf( fortranfile, " %s\n", t[i].comment ) ;
               break ;
            case OUTPUT:
               if ( t[i].ID == i )
               {   fprintf( fortranfile, "      %s %-17s !", t[i].tok, t[i].name ) ;
                   if ( t[i+1].type == TITLE || t[i+1].type == COMMENT ) fprintf( fortranfile, " %s\n", t[i+1].comment ) ;
                   else  fprintf( fortranfile, "\n" ) ;
               }
        }
    }
    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case ZERO:
            case FIRST:
               if ( t[i].ID > 0 )
               fprintf( fortranfile, "      %s %-17s ! flux of %s\n", t[i].tok, t[i].name, t[t[i].ID].name ) ;
        }
    }
    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case ZERO:
            case FIRST:
               if ( t[i].ID > 0 )
               fprintf( fortranfile, "      integer I%-16s ! pointer to flux variable\n", t[i].name ) ;
        }
    }
    loop1 ( fortranfile ) ;

 //      give the flux pointers a sequential value

    iitem = 0 ;
    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case ZERO:
            case FIRST:
               if ( t[i].ID > 0 )
               fprintf( fortranfile, "      I%-16s = %3d\n", t[i].name, ++iitem ) ;
        }
    }

 //      open the loop and give all input variables their value from the pmsa array

    loop2 ( fortranfile ) ;
    iitem = 0 ;
    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case WATER:
            case BOTTOM:
            case PARM:
            case XT:
            case FLOW:
               fprintf( fortranfile, "         %-17s = pmsa( ipnt(%3d) )\n", t[i].name, ++iitem  ) ;
               break ;
        }
    }

 //      start the DUPROL code

    fprintf( fortranfile, "\n!   *****     DUPROL code inserted here    *****\n\n"  ) ;
    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case TITLE:                   // now the comments from the code section are echoed
            case COMMENT:
               if ( i > lastinput )
                  if ( t[i-1].type == TITLE || t[i-1].type == COMMENT ) fprintf( fortranfile, "!     %s\n", t[i].comment ) ;
               break ;
            case OUTPUT:
               for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
               fprintf( fortranfile, "         %-17s = %s\n", t[i].name, t[i].comment  ) ;
               break ;
            case ZERO:
               if ( t[t[i].ID].type != BOTTOM )
               {   for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
                   fprintf( fortranfile, "         %-17s = %s\n", t[i].name, t[i].comment  ) ;
               }
               else
               {   if ( strchr( t[i].comment, '&' ) == NULL )
                   {   for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
                       fprintf( fortranfile, "         %-17s = ( %s ) / Z\n", t[i].name, t[i].comment  ) ;
                   }
                   else
                   {   ipend = 1 ;
                       for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
                       fprintf( fortranfile, "         %-17s = ( %s\n", t[i].name, t[i].comment  ) ;
                   }
               }
               break ;
            case FIRST:
               if ( t[t[i].ID].type != BOTTOM )
               {   if ( strchr( t[i].comment, '&' ) == NULL )
                   {   for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
                       fprintf( fortranfile, "         %-17s = ( %s ) * %s\n", t[i].name, t[i].comment, t[t[i].ID].name  ) ;
                       sprintf( hlp        , "%s * %s", t[i].comment, t[t[i].ID].name  ) ;
                       strcpy ( t[i].comment, hlp ) ;
                   }
                   else
                   {   for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
                       fprintf( fortranfile, "         %-17s = ( %s\n", t[i].name, t[i].comment ) ;
                       sprintf( hlp        , "%s * %s", t[i].comment, t[t[i].ID].name  ) ;
                       strcpy ( t[i].comment, hlp ) ;
                       ipend = 2 ;
               }   }
               else
               {   if ( strchr( t[i].comment, '&' ) == NULL )
                   {   for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
                       fprintf( fortranfile, "         %-17s = ( %s ) * %s / Z\n", t[i].name, t[i].comment, t[t[i].ID].name  ) ;
                       sprintf( hlp        , "%s * %s / Z", t[i].comment, t[t[i].ID].name  ) ;
                       strcpy ( t[i].comment, hlp ) ;
                   }
                   else
                   {   for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
                       fprintf( fortranfile, "         %-17s = ( %s\n", t[i].name, t[i].comment ) ;
                       sprintf( hlp        , "%s * %s / Z", t[i].comment, t[t[i].ID].name  ) ;
                       strcpy ( t[i].comment, hlp ) ;
                       ipend = 3 ;
               }   }
               break ;
            case IF:
               for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
               fprintf( fortranfile, "         %s %s then\n", t[i].tok, t[i].comment ) ;
               iflevel++ ;
               break ;
            case ELSE:
               for ( ifl = 0 ; ifl < iflevel-1 ; ifl++ ) fprintf( fortranfile, "   " ) ;
               fprintf( fortranfile, "         %s %s ", t[i].tok, t[i].comment ) ;
               if ( t[i].ID < 0 ) fprintf( fortranfile, " then\n" ) ;    // it was an 'else if' thus also a 'then'
               else               fprintf( fortranfile, "\n" ) ;
               break ;
            case OPEN:
               if ( t[i].comment[0] != '\0' )
               {   for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
                   fprintf( fortranfile, "         %s\n", t[i].comment ) ;
               }
               break ;
            case CLOSE:
               if ( iflevel > 0 )
               {  for ( ifl = 0 ; ifl < iflevel-1 ; ifl++ ) fprintf( fortranfile, "   " ) ;
                  fprintf( fortranfile, "         endif\n\n" ) ;
                  iflevel-- ;
               }
               break ;
            case CONTINUE :
               {   if ( strchr( t[i].comment, '&' ) != NULL || ipend == 0 )
                   {   for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
                       fprintf( fortranfile, "     &                %s\n", t[i].comment  ) ;
                   }
                   else
                   {   switch ( ipend )
                       {   case 1 :
                              for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
                              fprintf( fortranfile, "     &                %s ) / z\n", t[i].comment  ) ;
                              ipend = 0 ;
                              break ;
                           case 2 :
                              for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
                              fprintf( fortranfile, "                      %s ) * %s\n", t[i].comment, t[t[i].ID].name  ) ;
                              ipend = 0 ;
                              break ;
                           case 3 :
                              for ( ifl = 0 ; ifl < iflevel ; ifl++ ) fprintf( fortranfile, "   " ) ;
                              fprintf( fortranfile, "                      %s ) * %s / Z\n", t[i].comment, t[t[i].ID].name  ) ;
                              ipend = 0 ;
               }   }   }
        }
    }
    fprintf( fortranfile, "\n!   *****     DUPROL code ends here        *****\n\n"  ) ;

 //      give the entries in the flux array their value

    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case ZERO:
            case FIRST:
               fprintf( fortranfile, "         fl  ( I%-16s ) = %s\n", t[i].name, t[i].name ) ;
        }
    }

 //      set the output variables at their location in the pmsa array

    iitem = 0 ;
    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case WATER:
            case BOTTOM:
            case PARM:
            case XT:
            case FLOW:
               ++iitem ;
               break ;
            case OUTPUT:
               if ( t[i].ID == i )
               fprintf( fortranfile, "         pmsa( ipnt(%4d) ) = %s\n", ++iitem, t[i].name  ) ;
        }
    }

 //      increase the flux pointers

    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case ZERO:
            case FIRST:
               fprintf( fortranfile, "         I%-17s = I%-17s + noflux\n", t[i].name, t[i].name ) ;
               fflush ( fortranfile ) ;
        }
    }

 //      end the routine

    loop3 ( fortranfile ) ;
}

void wascii ( )
{   int i ;

    fprintf ( asciifile, "         2\n" ) ;

    fprintf ( asciifile, "HydDuflow                     HydDuflow\n" ) ;
    fprintf ( asciifile, "hyddfl    ; naam module\n" ) ;
    fprintf ( asciifile, "123       ; waarde van TRswitch\n" ) ;
    fprintf ( asciifile, "         7; aantal invoer grootheden op segment niveau\n" ) ;
    fprintf ( asciifile, "Volume         -999.000     x volume of computational cell                           m3\n" ) ;
    fprintf ( asciifile, "Surf           -999.000     x horizontal surface area of a DELWAQ segment            m2\n" ) ;
    fprintf ( asciifile, "DELT           -999.000     x timestep for processes                                 d\n" ) ;
    fprintf ( asciifile, "Width             0.000     x total width                                            m\n" ) ;
    fprintf ( asciifile, "VWind             0.000     x wind speed                                             m/s\n" ) ;
    fprintf ( asciifile, "WindDir           0.000     x actual wind direction                                  degrees\n" ) ;
    fprintf ( asciifile, "Velocity          0.000     x horizontal flow velocity                               m/s\n" ) ;
    fprintf ( asciifile, "         0; aantal invoer items op exchange niveau\n" ) ;
	fprintf ( asciifile, "         8; aantal uitvoer grootheden op segment niveau\n" ) ;
    fprintf ( asciifile, "Z                           x water depth                                            m\n" ) ;
    fprintf ( asciifile, "Q                           x Flow                                                   m3/s\n" ) ;
    fprintf ( asciifile, "As                          x Flow area                                              m2\n" ) ;
    fprintf ( asciifile, "dt                          x Quality time step                                      sec\n" ) ;
    fprintf ( asciifile, "dx                          x Half of the length of section                          m\n" ) ;
    fprintf ( asciifile, "V                           x Half of the volume of section                          m3\n" ) ;
    fprintf ( asciifile, "Wf                          x Wind velocity                                          m/s\n" ) ;
    fprintf ( asciifile, "Wd                          x Wind direction                                         degrees\n" ) ;
    fprintf ( asciifile, "         0; aantal uitvoer items op exchange niveau\n" ) ;
    fprintf ( asciifile, "         0; aantal fluxen\n" ) ;
    fprintf ( asciifile, "         0; aantal basis stochiometrie termen\n" ) ;
    fprintf ( asciifile, "         0 ; aantal basis stochiometrie termen dispersie-array\n" ) ;
    fprintf ( asciifile, "         0 ; aantal basis stochiometrie termen velocity-array\n" ) ;
    fprintf ( asciifile, "END\n" ) ;

    fprintf ( asciifile, "%-30s%s\n", modname, modname ) ;
    fprintf ( asciifile, "%-10s; naam module\n", modname ) ;
    fprintf ( asciifile, "123       ; waarde van TRswitch\n" ) ;

    fprintf ( asciifile, "%10d; aantal invoer grootheden op segment niveau\n", noinput ) ;
    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case WATER:
            case BOTTOM:
            case PARM:
            case XT:
            case FLOW:
               while ( strchr( t[i].comment, '\"' ) != NULL ) *strchr( t[i].comment, '\"' ) = ' ' ;
               while ( strchr( t[i].comment, '\'' ) != NULL ) *strchr( t[i].comment, '\'' ) = ' ' ;
               if ( t[i].value != -987654.0 )
                   fprintf( asciifile, "%-10s%15g   x %-50.50s     %-20.20s\n", t[i].trunc, t[i].value, t[i].comment, t[i].unit ) ;
               else
                   fprintf( asciifile, "%-10s                  x %-50.50s     %-20.20s\n", t[i].trunc, t[i].comment, t[i].unit  ) ;
               break ;
        }
    }
    fprintf ( asciifile, "         0; aantal invoer items op exchange niveau\n" ) ;

    fprintf ( asciifile, "%10d; aantal uitvoer grootheden op segment niveau\n", noitem-noinput ) ;
    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case OUTPUT:
               if ( t[i].ID == i )
                   fprintf( asciifile, "%-10s                  x %-50.50s     %-20.20s\n", t[i].trunc, t[i].comment, t[i].unit  ) ;
        }
    }
    fprintf ( asciifile, "         0; aantal uitvoer items op exchange niveau\n" ) ;

    fprintf ( asciifile, "%10d; aantal fluxen\n", noflux ) ;
    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case ZERO:
            case FIRST:
               if ( t[i].ID > 0 )
                   fprintf( asciifile, "%-10s                  x %-50.50s     %-20.20s\n", t[i].trunc, t[i].comment, t[i].unit  ) ;
        }
    }

    fprintf ( asciifile, "%10d; aantal basis stochiometrie termen\n", noflux ) ;
    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case ZERO:
            case FIRST:
               if ( t[i].ID > 0 )
                   fprintf( asciifile, "%-10s  %-10s     1.0\n", t[t[i].ID].trunc, t[i].trunc  ) ;
        }
    }

    fprintf ( asciifile, "         0 ; aantal basis stochiometrie termen dispersie-array\n" ) ;

    fprintf ( asciifile, "         0 ; aantal basis stochiometrie termen velocity-array\n" ) ;

    fprintf ( asciifile, "END\n" ) ;

    fprintf ( asciifile, "%10d; aantal getransporteerde stoffen\n", nowater ) ;
    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case WATER:
               if ( t[i].ID > 0 )
                   fprintf( asciifile, "%-10s                  x %-50.50s     %-20.20s\n", t[i].trunc, t[i].comment, t[i].unit  ) ;
        }
    }

    fprintf ( asciifile, "%10d; aantal stoffen op de bodem\n", nobed ) ;
    for ( i=1 ; i<it ; i++ )
    {   switch ( t[i].type )
        {   case BOTTOM:
               if ( t[i].ID > 0 )
                   fprintf( asciifile, "%-10s                  x %-50.50s     %-20.20s\n", t[i].trunc, t[i].comment, t[i].unit  ) ;
        }
    }

    fprintf ( asciifile, "END\n" ) ;
}

void header ( FILE *outfile )   // writes the header of the fortran file
{                               // fills in the module name (modname)
                                // fills in the total number of input and output items as array dimensions

    fprintf ( outfile, "%s%-10s%s\n" , "      subroutine ",modname," ( pmsa   , fl     , ipoint , increm, noseg , &" ) ;
    fprintf ( outfile, "%s\n"        , "                              noflux , iexpnt , iknmrk , noq1  , noq2  , &" ) ;
    fprintf ( outfile, "%s\n"        , "                              noq3   , noq4   )" ) ;
    fprintf ( outfile, "%s%s%s%s\n"  , "!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: '",modname,"' :: ",modname ) ;
    fprintf ( outfile, "%s\n"        , "!" ) ;
    fprintf ( outfile, "%s\n"        , "!*******************************************************************************" ) ;
    fprintf ( outfile, "%s\n"        , "!" ) ;
    fprintf ( outfile, "%s\n"        , "      IMPLICIT NONE" ) ;
    fprintf ( outfile, "%s\n"        , "!" ) ;
    fprintf ( outfile, "%s\n"        , "!     Type    Name          I/O Description" ) ;
    fprintf ( outfile, "%s\n"        , "!" ) ;
    fprintf ( outfile, "%s\n"        , "      real(4) pmsa(*)      !I/O Process Manager System Array, window of routine to process library" ) ;
    fprintf ( outfile, "%s\n"        , "      real(4) fl(*)        ! O  Array of fluxes made by this process in mass/volume/time" ) ;
    fprintf ( outfile, "%s%4d%s\n"   , "      integer ipoint(",noitem,") ! I  Array of pointers in pmsa to get and store the data" ) ;
    fprintf ( outfile, "%s%4d%s\n"   , "      integer increm(",noitem,") ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying" ) ;
    fprintf ( outfile, "%s\n"        , "      integer noseg        ! I  Number of computational elements in the whole model schematisation" ) ;
    fprintf ( outfile, "%s\n"        , "      integer noflux       ! I  Number of fluxes, increment in the fl array" ) ;
    fprintf ( outfile, "%s\n"        , "      integer iexpnt(4,*)  ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces" ) ;
    fprintf ( outfile, "%s\n"        , "      integer iknmrk(*)    ! I  Active-Inactive, Surface-water-bottom, see manual for use" ) ;
    fprintf ( outfile, "%s\n"        , "      integer noq1         ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)" ) ;
    fprintf ( outfile, "%s\n"        , "      integer noq2         ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid" ) ;
    fprintf ( outfile, "%s\n"        , "      integer noq3         ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward" ) ;
    fprintf ( outfile, "%s\n"        , "      integer noq4         ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)" ) ;
    fprintf ( outfile, "%s%4d%s\n"   , "      integer ipnt(",noitem,")   !    Local work array for the pointering" ) ;
    fprintf ( outfile, "%s\n"        , "      integer iseg         !    Local loop counter for computational element loop" ) ;
    fprintf ( outfile, "%s\n"        , "!" ) ;
    fprintf ( outfile, "%s\n"        , "!*******************************************************************************" ) ;
    fprintf ( outfile, "%s\n"        , "!" ) ;
    fprintf ( outfile, "%s\n"        , "!     Type    Name          I/O Description                                        Unit" ) ;
    fprintf ( outfile, "%s\n"        , "!" ) ;
}

void loop1  ( FILE *outfile )
{   fprintf ( outfile, "%s\n" , "!" ) ;
    fprintf ( outfile, "%s\n" , "!*******************************************************************************" ) ;
    fprintf ( outfile, "%s\n" , "!" ) ;
    fprintf ( outfile, "%s\n" , "      ipnt        = ipoint") ;
}

void loop2  ( FILE *outfile )
{   fprintf ( outfile, "%s\n" , "!" ) ;
    fprintf ( outfile, "%s\n" , "      do 9000 iseg = 1 , noseg" ) ;
    fprintf ( outfile, "%s\n" , "!" ) ;
}

void loop3  ( FILE *outfile )
{   fprintf ( outfile, "\n%s\n" , "         ipnt        = ipnt        + increm" ) ;
    fprintf ( outfile, "%s\n" , "!" ) ;
    fprintf ( outfile, "%s\n" , " 9000 continue" ) ;
    fprintf ( outfile, "%s\n" , "!" ) ;
    fprintf ( outfile, "%s\n" , "      return" ) ;
    fprintf ( outfile, "%s\n" , "      end subroutine" ) ;
}

int keyword ( char *token )                                           // Identifies Duprol keywords
{   char *test[13] = { "/*"  , "//"   , "WATER ", "BOTTOM ", "PARM ",
                       "XT " , "FLOW ", "k0("   , "k1("    , "IF"   ,
                       "ELSE", "{"    , "}"     }  ;
    int length = 13 ;
    int i ;

    for ( i = 0 ; i < length ; i++ )
    {   if ( strnicmp( token, test[i], strlen(test[i]) ) == 0 ) return i+1 ;
    }

    return 0 ;
}

void breakup( char *line, token *tt, int len )                        // fills in the declaration fields
{   char *n, *p ;
    char  max[11] ;
    int   i, j, k ;

    max[len] = '\0' ;

  // get the name of the variable

    n = strtok( line, " " ) ;
    i = (int) strlen(n) ;
    if ( i == 2 && strnicmp( n, "fl", 2 ) == 0 )                      // we already have a 'FL' variable in Delwaq
    {   tt->name = (char *) malloc( 4 ) ;
        strcpy ( tt->name , "fl2" ) ;                                 // we name this one 'fl2'
    }
    else                                                              // this is the normal procedure
    {   tt->name  = (char *) malloc( i+1 ) ;
        strcpy ( tt->name , n ) ;
    }
    if ( i < len+1 )
    {   tt->trunc = (char *) malloc( i+1 ) ;
        strcpy ( tt->trunc, n ) ;
    }
    else
    {   tt->trunc = (char *) malloc( len+1 ) ;
        strncpy ( max, n, len ) ;
        j = findID ( max, 0, len, 1 )  ;
        if ( j == 0 )
        {   strcpy ( tt->trunc, max ) ;   }
        else
        {   for ( k = len; k > 0 ; k-- )
            {   max[k-1] = n[k-1+i-len] ;
                if ( findID ( max, 0, len, 1 ) == 0 )
                {   strcpy ( tt->trunc, max ) ;
                    break ;
    }   }   }   }

    n += strlen(n)+1 ;

  // gets the default value of the variable

    p = strchr( n , '[' ) ;
    if ( p != NULL )
    {   n = p ;
        n[0] = ' ' ; *strchr(n,']') = ' ' ;
        n = strtok( NULL, " " ) ;
        sscanf ( n, "%f", &(tt->value) ) ;
        n += strlen(n)+1 ;
    }
    else                                                              // this could not be tested, in the
    {   tt->value = -987654.0 ;                                       // examples their never was a missing
    }                                                                 // default value

  // gets the unit of the variable. It may be missing and have embedded blanks

    for ( i = 0 ; i < (int) strlen(n) ; i++ ) if ( n[i] != ' ' ) break ;
    n += i ;
    for ( i = (int) strchr(n,';') ; i > (int) n ; i-- ) if ( (char) i != ' ' &&  (char) i != ';') break ;
    i = i - (int) n ;
    if ( i > 0 )
    {   tt->unit = (char *) malloc( i+1 ) ;
        strncpy ( tt->unit, n, i ) ; tt->unit[i] = '\0' ;
    }
    else
    {   tt->unit = (char *) malloc ( 1 ) ;
       *tt->unit = '\0' ;
    }

  // gets the comment of the declaration after the ; character

    n = strchr(n,';') + 1 ;
    for ( i = 0 ; i < (int) strlen(n) ; i++ ) if ( n[i] != ' ' ) break ;
    n += i ;
    tt->comment = malloc( strlen(n)+1 ) ;
    strcpy( tt->comment, n ) ;

}

int findID ( char *line, int bracket, int len, int mode  )
{
    int  i       ;
    char max[30] ;
    max[len] = '\0' ;

    if ( bracket == 1 ) *strchr(line,')') = '\0' ;           // for the D0 and D1 tokens
    strncpy ( max, line, len ) ;

    if ( mode == 0 )
    {   for ( i = 1 ; i < it ; i++ )
        {   if ( t[i].name != NULL )
            {   if ( strcmp( max, t[i].name ) == 0 ) return i ; }    // note that the ID is never zero
    }   }
    else
    {   for ( i = 1 ; i < it ; i++ )
        {   if ( t[i].trunc != NULL )
            {   if ( strcmp( max, t[i].trunc ) == 0 ) return i ; }
    }   }
    return 0 ;
}

char* detect ( char *line )
{   char *test[16] = { " != "  , " && "    , "!"       , " || "    , "^"     ,
                       " ln("  , "-ln("    , " ln "    , " log("   , " log "   , " rnd(" ,
                       " rnd " , "(ln("    , "*ln("    , " &&("    , " ||("  } ;
    char *solu[16] = { " /= "  , " .and. " , ".not."   , " .or. "  , "**"    ,
                       " log(" , "-log("   , " log "   , " log10(" , " log10 " , " ran(" ,
                       " ran " , "(log("   , "*log("   , " .and.(" , " .or.("} ;
    int   number = 15 ;
    int   i, j, k     ;                                             // replace Duprol arithmetic with
    char *found       ;                                             // FORTRAN arithmetic.
    char *detected    ;
    char *work        ;
		
    detected = malloc( strlen(line) + 50 ) ;
    work     = malloc( strlen(line) + 50 ) ;
	if (strchr(" !(^*", line[0]) == NULL)                
	{
		strcpy( detected, " ");
		strcpy( work    , " ");
		strcpy( detected + 1, line);
		strcpy( work     + 1, line);
	}
	else
	{
		strcpy ( detected, line ) ;
		strcpy ( work    , line ) ;
	}
    for ( i = strlen(detected) ; i > 0 ; i-- ) if ( detected[i] != ' ' ) break ;
    detected[i] = '\0' ; work[i] = '\0' ;
    if ( strchr( detected, '{' ) != NULL ) { *strchr( detected, '{' ) = ' ' ; *strchr( work, '{' ) = ' ' ; }
    if ( strchr( detected, '}' ) != NULL ) { *strchr( detected, '}' ) = ' ' ; *strchr( work, '}' ) = ' ' ; }
    strlwr( detected ) ;
    found = strstr( detected, "fl" ) ;            // if an "fl" is found that is between non-characters/numbers,
    if ( found != NULL )                          // we replace by "fl2" because Delwaq already has a "fl"
    {   if ( ( found == detected || ( ! ( (int) found[-1] > 64 && (int) found[-1] < 91 ) &&
                                      ! ( (int) found[-1] > 96 && (int) found[-1] <123 )     )  )
                 &&                 ( ! ( (int) found[ 2] > 64 && (int) found[ 2] < 91 ) &&
                                      ! ( (int) found[ 2] > 96 && (int) found[ 2] <123 ) &&
                                      ! ( (int) found[ 2] > 47 && (int) found[ 2] < 58 )     )     )  // numbers
        {   for ( j=strlen(detected) ; j > found-detected ; j-- ) detected[j+1] = detected[j] ;
            for ( j=strlen(  work  ) ; j > found-detected ; j-- ) work    [j+1] = work    [j] ;
            strncpy ( &work[found-detected], "fl2", 3 ) ;
    }   }

    for ( i = 0 ; i < number ; i++ )              // these are the replacements of non-FORTRAN intructions
    {   found = strstr( detected, test[i] ) ;     // by FORTRAN instructions (that are sometimes longer).
        while ( found != NULL )
        {   k = strlen(solu[i]) - strlen(test[i]) ;
            if ( k > 0 )
            {   for ( j=strlen(detected) ; j > found-detected ; j-- ) detected[j+k] = detected[j] ;
                for ( j=strlen(  work  ) ; j > found-detected ; j-- ) work    [j+k] = work    [j] ;   }
            strncpy ( &work[found-detected], solu[i], strlen(solu[i]) ) ;
            found = strstr( found+strlen(solu[i]), test[i] ) ;
        }
    }
    free ( detected ) ;
    return work ;
}
