/* testods.c --
       Simple test program for ODS library
*/

#include <stdio.h>
#include "ods.h"
#include <float.h>

TInt4 main( int argc, char *argv[] ) {
    char filename[3][256];

    TInt4 itype;
    char dim[4];
    TInt4 pardep;
    TInt4 timdep;
    TInt4 locdep;
    TInt4 loc_index[3][3];
    TInt4 ndim[5];
    TInt4 ndimt[5];
    TInt4 error;
    TInt4 maxdef;
    TInt4 maxnames;
    TInt4 maxlst;
    TInt4 lang;
    int   i;
    int   j;

    char   *option = NULL;
    char   *name;
    char   *unit;
    char   *locname;
    TInt4  *partyp;
    TInt4  *parcod;
    TInt4  *loctyp;
    TInt4  *locnr;
    TInt4   nrlst;
    TInt4   nrlocs;
    TInt4   maxtimes;
    TReal4 *value;
    TReal4  missing;
    TReal8  timdef[3];
    TReal8 *times;
    TInt4  *timtyp;
    TInt4   nrtimes;
    TInt4   nocell;
    TInt4   igisty;
    TInt4  *indx;

    lang   = 1;

#if 1
    itype  = ODS_TRISULA_HIS_NEFIS;
    pardep = 0;
    timdep = 0;
    locdep = 0;
    strcpy( dim, "PAR" );

//  strcpy( filename[0], "trih-scs.dat" );
//  strcpy( filename[1], "trih-scs.def" );
    strcpy( filename[0], "trih-wheeler.dat" );
    strcpy( filename[1], "trih-wheeler.def" );
    getdim( &filename[0][0], &itype, dim, &pardep, &timdep, &locdep, ndim,
        &error, option );

    printf( "Int: %d\n", sizeof(int) );

    printf( "Ndim:               %d\n", ndim[0] );
    printf( "Number parameters:  %d\n", ndim[1] );
    printf( "Error:              %d\n", error   );

    maxdef   = 1;
    timdep   = 0;
    locdep   = 0;
    maxnames = ndim[1];
    name     = (char *) malloc( maxnames * (PARLEN+1) * sizeof(char) );
    unit     = (char *) malloc( maxnames * (PARLEN+1) * sizeof(char) );
    partyp   = (TInt4 *) malloc( maxnames * sizeof(TInt4) );
    parcod   = (TInt4 *) malloc( maxnames * sizeof(TInt4) );

    getpar( &filename[0][0], &itype, "*", &maxdef, &timdep, &locdep,
            &maxnames, &lang, name, unit, partyp, parcod, &nrlst,
            &error, option );

    printf( "Parameters:\n" );
    for ( i = 0; i < nrlst; i ++ ) {
        printf( "%d %s\n", i, &name[(PARLEN+1)*i]);
    }


    pardep = parcod[1]; /* Parameter 1 is the water level */
    timdep = 0;
    locdep = 0;
    strcpy( dim, "LOC" );

    getdim( &filename[0][0], &itype, dim, &pardep, &timdep, &locdep, ndim,
        &error, option );

    printf( "Error code (reading number of locations): %d\n", error ) ;

    maxnames = ndim[1];
    printf( "Number of locations: %d\n", maxnames );

    locname  = (char *) malloc( maxnames * (PARLEN+1) * sizeof(char) );
    loctyp   = (TInt4 *) malloc( maxnames * sizeof(TInt4) );
    locnr    = (TInt4 *) malloc( maxnames * sizeof(TInt4) );

    getloc( &filename[0][0], &itype, "*", &maxdef, &pardep, &timdep,
            &maxnames, locname, loctyp, locnr, &nrlocs,
            &error, option );

    printf( "Locations:\n" );
    for ( i = 0; i < nrlocs; i ++ ) {
        printf( "%d %s\n", i, &locname[(PARLEN+1)*i]);
    }


    pardep = parcod[1];
    locdep = locnr[0];
    timdep = 0;
    strcpy( dim, "TIM" );

    getdim( &filename[0][0], &itype, dim, &pardep, &timdep, &locdep, ndimt,
        &error, option );

    printf( "Error code (reading number of times): %d\n", error ) ;

    maxtimes = ndimt[1];
    printf( "Number of times (values): %d\n", maxtimes );


    times    = (TReal8 *) malloc( maxtimes * sizeof(TReal8) );
    timtyp   = (TInt4  *) malloc( maxtimes * sizeof(TInt4)  );
    value    = (TReal4 *) malloc( 10 * maxtimes * sizeof(TReal4) );

    timdef[0] = (TReal8)     0.0;
    timdef[1] = (TReal8) FLT_MAX;
    maxdef    = 2;
    gettme( &filename[0][0], &itype, timdef, &maxdef, &pardep, &locdep,
            &maxtimes, times, timtyp, &nrtimes, &error, option );


    missing   = -999.0;
    timdef[0] = times[0];
    timdef[1] = times[nrtimes-1];
 /* timdef[2] = times[1]-times[0]; */


    for ( i = 0; i < maxnames; i ++ ) {
        loc_index[0][0] = locnr[i];
        loc_index[0][1] = locnr[i];
        loc_index[0][2] = 1;
        loc_index[1][0] = 0;
        loc_index[1][1] = 0;
        loc_index[1][2] = 1;
        loc_index[2][0] = 0;
        loc_index[2][1] = 0;
        loc_index[2][2] = 1;

        getmat( &filename[0][0], &itype, &pardep, &loc_index[0][0],
                timdef, &missing, &lang, &maxtimes, &value[maxtimes*i],
                &error, option );

    }

    for ( i = 0; i < maxtimes; i ++ ) {
        printf( "Time: %lf\n", times[i] );
        for ( j = 0; j < maxnames; j ++ ) {
            printf( "%12.4e\n", value[maxtimes*j+i]);
        }
        printf("\n");
    }

/*
    printf( "Values:\n" );
    for ( i = 0; i < maxtimes; i ++ ) {
        printf( " %12.4e", value[i]);
        if ( i % 10 == 9 ) {
            printf( "\n" );
        }
    }
*/

/*
    for ( i = 0; i < maxnames; i ++ ) {
        loc_index[0][0] = locnr[i];
        loc_index[0][1] = locnr[i];
        loc_index[0][2] = 1;
        loc_index[1][0] = 0;
        loc_index[1][1] = 0;
        loc_index[1][2] = 1;
        loc_index[2][0] = 0;
        loc_index[2][1] = 0;
        loc_index[2][2] = 1;

        getmat( &filename[0][0], &itype, &pardep, &loc_index[0][0],
                timdef, &missing, &lang, &maxtimes, value,
                &error, option );

    }
*/
#else
    itype  = ODS_MORSYS_MAP_NEFIS;
    pardep = 0;
    timdep = 0;
    locdep = 0;
    strcpy( dim, "PAR" );

    strcpy( filename[0], "com-f35.dat" );
    strcpy( filename[1], "com-f35.def" );
    getdim( &filename[0][0], &itype, dim, &pardep, &timdep, &locdep, ndim,
        &error, option );

    printf( "Int: %d\n", sizeof(int) );

    printf( "Ndim:               %d\n", ndim[0] );
    printf( "Number parameters:  %d\n", ndim[1] );
    printf( "Error:              %d\n", error   );

    maxdef   = 1;
    timdep   = 0;
    locdep   = 0;
    maxnames = ndim[1];
    name     = (char *) malloc( maxnames * (PARLEN+1) * sizeof(char) );
    unit     = (char *) malloc( maxnames * (PARLEN+1) * sizeof(char) );
    partyp   = (TInt4 *) malloc( maxnames * sizeof(TInt4) );
    parcod   = (TInt4 *) malloc( maxnames * sizeof(TInt4) );

    getpar( &filename[0][0], &itype, "*", &maxdef, &timdep, &locdep,
            &maxnames, &lang, name, unit, partyp, parcod, &nrlst,
            &error, option );

    printf( "Parameters:\n" );
    for ( i = 0; i < nrlst; i ++ ) {
        printf( "%d %s\n", i, &name[(PARLEN+1)*i]);
    }

    pardep = parcod[1]; /* Parameter 1 is the water level */
    timdep = 0;
    locdep = 0;
    strcpy( dim, "LOC" );

    getdim( &filename[0][0], &itype, dim, &pardep, &timdep, &locdep, ndim,
        &error, option );

    printf( "Error code (reading number of locations): %d\n", error ) ;

    printf( "Number of locations: %d x %d x %d -- %d\n", ndim[1], ndim[2], ndim[3], ndim[4] );

    indx = (TInt4 *) malloc( sizeof(TInt4) * ndim[1]*ndim[2] ) ;
    getgrd( &filename[0][0], &itype, &loc_index[0][0], indx, &nocell, &igisty,
            &error );

    printf( "Grid: nocell = %d\n", nocell );

    pardep = parcod[1];
    locdep = 0;
    timdep = 0;
    strcpy( dim, "TIM" );

    getdim( &filename[0][0], &itype, dim, &pardep, &timdep, &locdep, ndimt,
        &error, option );

    printf( "Error code (reading number of times): %d\n", error ) ;

    maxtimes = ndimt[1];
    printf( "Number of times (values): %d\n", maxtimes );

    times    = (TReal8 *) malloc( maxtimes * sizeof(TReal8) );
    timtyp   = (TInt4  *) malloc( maxtimes * sizeof(TInt4)  );
    value    = (TReal4 *) malloc( nocell * sizeof(TReal4) );
    printf( "Value: %p -- %d\n", value, nocell );

    timdef[0] = (TReal8)     0.0;
    timdef[1] = (TReal8) FLT_MAX;
    maxdef    = 2;
    gettme( &filename[0][0], &itype, timdef, &maxdef, &pardep, &locdep,
            &maxtimes, times, timtyp, &nrtimes, &error, option );
    printf( "Value after gettme: %p -- %d\n", value, nocell );

    missing   = -999.0;
 /* timdef[2] = times[1]-times[0]; */


    for ( i = 0; i < 10; i ++ ) {
        printf( "Time = %20.12e:\n", times[i] );
        timdef[0] = times[i];
        timdef[1] = times[i];
        timdef[2] = 1.0;
        loc_index[0][0] = 0;
        loc_index[0][1] = ndim[1]-1;
        loc_index[0][2] = 1;
        loc_index[1][0] = 0;
        loc_index[1][1] = ndim[2]-1;
        loc_index[1][2] = 1;
        loc_index[2][0] = 0;
        loc_index[2][1] = 0;
        loc_index[2][2] = 1;

        getmat( &filename[0][0], &itype, &pardep, &loc_index[0][0],
                timdef, &missing, &lang, &maxtimes, value,
                &error, option );

        for ( j = 0; j < 10; j ++ ) {
            printf( "%12.4e ", value[200+j] );
        }
        printf("\n");
    }

#endif
}
