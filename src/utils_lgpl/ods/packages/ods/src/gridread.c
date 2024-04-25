//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
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
/* @begin@ */
/*
 *  gridread.c   -  Delft3D-WAQ: routines for reading curvilinear and
 *                  FEM grids
 *
 *  Arjen Markus
 */

/*
 * General information:
 * This file contains the following routines:
 * - ReadInteger():                  Read a single (big-endian) integer
 * - ReadReal():                     Read a single (big-endian) real
 * - SkipRecord():                   Skip a full record in the file
 * - ReadRecordIntegers():           Read integers from a record
 * - ReadRecordReals():              Read reals from a record
 * - WQ_ImportGridFEM():             Read the grid from a TELEMAC model
 * - WQ_ImportGridFileCurvilinear()  Read the curvilinear grid files
 *
 */

/* @end@ */

/*
 *  $Author: Markus $
 *  $Date: 6-04-05 10:17 $
 *  $Source$
 *
 */

/*
 * Include files and definitions
 */
#include "d3d_gen.h"
#include "watqual.h"

/*
 * Macros: none
 */

/*
 * Static global data: none
 */

/* @@------------------------------------------
    Function: ReadInteger()
    Author:   Arjen Markus
    Purpose:  Read a (big-endian) integer
    Context:  Used by routines here
    Pseudo Code:
              Read four bytes. Turn the bytes
              around. Return the integer value
 ------------------------------------------------*/
static TInt4 ReadInteger(
                FILE *infile )  /* Input file */
                                /* Returns integer */
{
   char buffer[4] ;
   union _v {
      char buffer2[4] ;
      int  intvalue   ;
   } v ;
   fread( buffer, 4, sizeof(char), infile ) ;

   /* Big endian file! */
   v.buffer2[0] = buffer[3] ;
   v.buffer2[1] = buffer[2] ;
   v.buffer2[2] = buffer[1] ;
   v.buffer2[3] = buffer[0] ;
   return v.intvalue ;
}

/* @@------------------------------------------
    Function: ReadReal()
    Author:   Arjen Markus
    Purpose:  Read a (big-endian) real
    Context:  Used by routines here
    Pseudo Code:
              Read four bytes. Turn the bytes
              around. Return the integer value
 ------------------------------------------------*/
static TReal4 ReadReal(
                FILE *infile )  /* Input file */
                                /* Returns integer */
{
   char buffer[4] ;
   union _v {
      char   buffer2[4] ;
      TReal4 rvalue     ;
   } v ;
   fread( buffer, 4, sizeof(char), infile ) ;

   /* Big endian file! */
   v.buffer2[0] = buffer[3] ;
   v.buffer2[1] = buffer[2] ;
   v.buffer2[2] = buffer[1] ;
   v.buffer2[3] = buffer[0] ;
   return v.rvalue ;
}

/* @@------------------------------------------
    Function: SkipRecord()
    Author:   Arjen Markus
    Purpose:  Skip a full record
    Context:  Used by WQ_ImportGridFEM()
    Pseudo Code:
              Read the four bytes that constitute
              the record length. Then skip to the
              start of the next record
 ------------------------------------------------*/
static TVoid SkipRecord(
                FILE *infile )  /* Input file */
{
   TInt4    reclen ;

   reclen = ReadInteger(infile) ;
   fseek( infile, reclen+4, SEEK_CUR  ) ;
}

/* @@------------------------------------------
    Function: ReadRecordIntegers()
    Author:   Arjen Markus
    Purpose:  Read integers from a record
    Context:  Used by WQ_ImportGridFEM()
    Pseudo Code:
              Successively read the bytes,
              skip to the next record when done
 ------------------------------------------------*/
static TVoid ReadRecordIntegers(
                FILE  *infile,   /* Input file */
                TInt4 *array,    /* Array to be filled */
                TInt4  maxnum )  /* Maximum number */
{
   TInt4    i      ;
   TInt4    reclen ;

   reclen = ReadInteger(infile) ;

   for ( i = 0 ; i < reclen/4 && i < maxnum ; i ++ )
   {
      array[i] = ReadInteger(infile) ;
   }

   if ( maxnum < reclen/4 )
   {
      fseek( infile, reclen-4*maxnum, SEEK_CUR ) ;
   }
   fseek( infile, 4, SEEK_CUR ) ;
}

/* @@------------------------------------------
    Function: ReadRecordReals()
    Author:   Arjen Markus
    Purpose:  Read reals from a record
    Context:  Used by WQ_ImportGridFEM()
    Pseudo Code:
              Successively read the bytes,
              skip to the next record when done
 ------------------------------------------------*/
static TVoid ReadRecordReals(
                FILE   *infile,   /* Input file */
                TReal4 *array,    /* Array to be filled */
                TInt4   maxnum )  /* Maximum number */
{
   TInt4    i      ;
   TInt4    reclen ;

   reclen = ReadInteger(infile) ;

   for ( i = 0 ; i < reclen/4 && i < maxnum ; i ++ )
   {
      array[i] = ReadReal(infile) ;
   }

   if ( maxnum < reclen/4 )
   {
      fseek( infile, reclen-4*maxnum, SEEK_CUR ) ;
   }
   fseek( infile, 4, SEEK_CUR ) ;
}

/* @@------------------------------------------
    Function: ReadRecordString()
    Author:   Leo Postma
    Purpose:  Reads a string record
    Context:  Used by WQ_ImportGridFEM()
    Pseudo Code: just read the string and cut the Fortrans
 ------------------------------------------------*/
static TVoid ReadRecordString(
                FILE   *infile,   /* Input file */
                char*   string,   /* Array to be filled */
                TInt4   maxnum )  /* Maximum number */
{  TInt4    reclen ;

   reclen = ReadInteger(infile) ;

   fread ( string, (int)min(reclen,maxnum), 1, infile ) ;
   if ( reclen <  maxnum ) string[reclen]   = '\0' ;
   if ( reclen == maxnum ) string[reclen-1] = '\0' ;
   if ( reclen >  maxnum )
   {   string[maxnum-1] = '\0' ;
       fseek ( infile, reclen-maxnum, SEEK_CUR ) ;
   }
   fseek( infile, 4, SEEK_CUR ) ;

}

/* @@------------------------------------------
    Function: WQ_ImportGridFEM()
    Author:   Arjen Markus
    Purpose:  Read the grid from a TELEMAC model
    Context:  Used by WQ_ImportGrid()
    Pseudo Code:
              Open the file, read the relevant
              records, store the information in
              the grid structure
 ------------------------------------------------*/
static TInt4 WQ_ImportGridFileFEM(
         WaterQualGeneralPtr   general    , /* I General data for UI */
         WaterQualScenarioPtr  scenario   , /* I Scenario data       */
         TString               file_name  ) /* I Grid file           */
                                    /* Returns if successful or not  */
{
   FILE         *infile                        ;
   TInt4         i , j , nomat  , idim[10]     ;
   TInt4         maxbnd, ibnd, nbnd, idum      ;
   TInt4         maxvalues                     ;
   TInt4         values[10]                    ;
   TInt4         no_elems                      ;
   TInt4         no_nodes                      ;
   TInt4         no_exchs                      ;
   TInt4         has_depth, idepth             ;
   TReal4       *depth                         ;
   GE_GridStruct curv_grid                     ;
   char          title[80]                     ;
   TInt4         nosys                         ;
   TInt4        *femtable                      ;
   TInt4         ityp                          ;
   TInt4         countbnd                      ;
   TInt4         countsect                     ;
   TInt4         counttotal                    ;
   TInt4         startbnd                      ;
   TInt4         firstbnd                      ;
   TReal4        rdum                          ;
   TString       name                          ;

   infile = fopen( file_name, "rb" ) ;

   if ( infile == NULL )
   {
      scenario->grid.ncrd1 = -1 ;
      return ErrorReadingFile ;
   }

   ReadRecordString  ( infile, title , 80 ) ;
   ReadRecordIntegers( infile, values,  2 ) ;
   nosys  = values[0] ;
   has_depth = 0         ;
   for ( i = 0 ; i < nosys ; i ++ )
   {   ReadRecordString ( infile, title, 32 ) ;
       if ( strcmp( title, "HAUTEUR D'EAU" ) == 0 ) has_depth = i+1 ;
   }
   ReadRecordIntegers( infile, values, 10 ) ;
   ReadRecordIntegers( infile, values,  4 ) ;
   no_elems = values[0] ;
   no_nodes = values[1] ;
   /* We need the boundary information as well for the number of
      exchanges - postponed
   no_exchs = scenario->grid.ncrd2 ;
   */

   if ( scenario->grid.xcoord != NULL )
   {
      GE_Free( scenario->grid.xcoord ) ;
      GE_Free( scenario->grid.ycoord ) ;
   }
   if ( scenario->grid.depth != NULL )
   {
      GE_Free( scenario->grid.depth ) ;
      scenario->grid.depth = NULL     ;
   }
   scenario->grid.xcoord     = (TReal4 *) GE_Alloc(  no_nodes    * sizeof(TReal4)     ) ;
   scenario->grid.ycoord     = (TReal4 *) GE_Alloc(  no_nodes    * sizeof(TReal4)     ) ;
   scenario->grid.cell_index = (TInt4  *) GE_Alloc(  no_elems    * sizeof(TInt4 ) * 3 ) ;
   femtable                  = (TInt4  *) GE_Alloc( (no_nodes+1) * sizeof(TInt4 )     ) ;

   ReadRecordIntegers( infile, scenario->grid.cell_index, 3*no_elems ) ;

   maxbnd = 0 ;
   ReadRecordIntegers( infile, &femtable[1], no_nodes ) ;
   for ( i=1 ; i<=no_nodes ; i++ )
   {   if ( femtable[i] != 0 ) maxbnd = max(maxbnd,femtable[i]);
       femtable[i] = i ;
   }

   ReadRecordReals( infile, scenario->grid.xcoord, no_nodes ) ;
   ReadRecordReals( infile, scenario->grid.ycoord, no_nodes ) ;
   if ( has_depth != 0 )
   {
      for ( i=0 ; i<has_depth ; i++ )
      {
         ReadRecordReals( infile, scenario->grid.depth, no_nodes ) ;
      }
   }

   fclose( infile ) ;

   infile = fopen( scenario->wq_files[File_GridCoords], "r" ) ;
   if ( infile == NULL )
   {   scenario->grid.ncrd1 = -1 ;
       return ErrorReadingFile ;
   }

   /* Important:
      The numbering method for the open boundaries has to comply
      to the method in the coupling program!
   */
   GridStartDefaultBoundSects( &scenario->grid ) ;

   nbnd       = -1 ;
   startbnd   =  1 ;
   counttotal =  0 ;
   countbnd   =  0 ;
   firstbnd   =  0 ;
   countsect  =  0 ;
   for ( i=0 ; i<maxbnd ; i++ )
   {
      fscanf ( infile, "%d %d %d %f %f %f %f %d %f %f %f %d %d",
               &ityp, &idum, &idum, &rdum, &rdum,
               &rdum, &rdum, &idum, &rdum, &rdum,
               &rdum, &ibnd, &idum ) ;
      if ( ityp != 2 )
      {
         if ( startbnd == 1 )
         {
            startbnd = 2 ;
            firstbnd = countbnd ; /* we need the whole array! */
         }

         femtable[countbnd] = ibnd ; /* We need the actual node number! */
         countbnd   ++             ;
         counttotal ++             ;
         nbnd --                   ;
      }
      else
      {
         if ( startbnd == 2 )
         {
            startbnd = 1 ;
            countsect ++ ;
            name = (TString) malloc( sizeof(TChar)*40 ) ;
            sprintf( name, "Section %d", countsect ) ;
            GridDefineDefaultBoundSect( name, &femtable[firstbnd],
                                        countbnd-firstbnd,
                                        &scenario->grid           ) ;
         }
      }
   }

   /* Make sure the last boundary section is also added
   */
   if ( startbnd == 2 )
   {
      startbnd = 1 ;
      countsect ++ ;
      name = (TString) malloc( sizeof(TChar)*40 ) ;
      sprintf( name, "Section %d", countsect ) ;
      GridDefineDefaultBoundSect( name, &femtable[firstbnd], countbnd-firstbnd,
                                  &scenario->grid           ) ;
   }


   fclose( infile ) ;

   scenario->wq_model.no_segments_per_layer = no_nodes   ;
   scenario->wq_model.no_bounds_per_layer   = counttotal ; /* No. _open_ boundaries */
   scenario->grid.ncrd3   = scenario->wq_model.no_hyd_layers    ;
   scenario->grid.nocell  = no_nodes * scenario->wq_model.no_layers ;

   scenario->wq_model.no_exchanges[0] =
      (3 * no_nodes + counttotal)/2 * scenario->wq_model.no_layers ;
   scenario->wq_model.no_exchanges[1] = 0 ;
   scenario->wq_model.no_exchanges[2] =
                            no_nodes * (scenario->wq_model.no_layers-1) ;

/* Now, make the graphical editor aware of this grid, i.e.
   fill the grid structure
*/
   curv_grid.no_elems   = no_elems                  ;
   curv_grid.no_nodes   = no_nodes                  ;
   curv_grid.no_bounds  = counttotal                ;
   curv_grid.ncrd1      = 1                         ;
   curv_grid.ncrd2      = 1                         ;
   curv_grid.xcoord     = scenario->grid.xcoord     ;
   curv_grid.ycoord     = scenario->grid.ycoord     ;
   curv_grid.depth      = scenario->grid.depth      ;

   curv_grid.cell_index     = scenario->grid.cell_index ;
   curv_grid.boundary_cells = femtable                  ;

   curv_grid.status     = ( GEdit_CellStatusEnumType *)
      GE_Alloc( sizeof( GEdit_CellStatusEnumType )*(no_nodes+1) ) ;

   for ( i = 0 ; i < no_nodes+1 ; i ++ )
   {
      curv_grid.status[i] = 9 ;
   }

   for ( i = 0 ; i < no_nodes ; i ++ )  /* loop over n */
   {
      curv_grid.status[i] = GEdit_ACTIVE_CELL; /* active cells in GE  */
   }

   curv_grid.type    = GEdit_FEM_GRID   ;
   curv_grid.sysType = Cartesian        ;
   GEdit_CreateGrid( &curv_grid , "" )  ;

   /* NOT YET */
#if 0
   if ( has_depth )
   {
      GEdit_CreateDepth( curv_grid.depth ) ;
      GE_Free( depth ) ;
   }
#endif

   GE_Free( curv_grid.status );
   GE_Free( femtable ) ;

   return EverythingOkay ;
}

/* @@------------------------------------------
    Function: WQ_ImportGridFileCurvilinear()
    Author:   Arjen Markus
    Purpose:  Import the grid file(s)
    Context:  Used by WQ_ImportHydrodynFile() and at start-up
    Pseudo Code:
              Call a FORTRAN routine to actually
              read the file:
              - First get the dimensions
              - Allocate the memory
              - Then get the actual data
 ------------------------------------------------*/
static TInt4 WQ_ImportGridFileCurvilinear(
         WaterQualGeneralPtr   general    , /* I General data for UI */
         WaterQualScenarioPtr  scenario   , /* I Scenario data       */
         TString               file_name  ) /* I Grid file           */
                                    /* Returns if successful or not  */
{
   TInt4         i , j , nomat  , idim[10]     ;
   TChar         filnam[3][256]                ;
   TInt4         itype , indloc[3][3] , nocell ;
   TInt4         igisty , ierror               ;
   TInt4         ipcode , maxdim               ;
   TInt4         indx                          ;
   TInt4         indx_stat                     ;
   TInt4         has_depth                     ;
   TReal4        valmis                        ;
   TReal4       *depth                         ;
   TReal8        time                          ;
   GE_GridStruct curv_grid                     ;
   int           l_filnam                      ;

   for ( j = 0 ; j < 3 ; j ++ )
   {
      for ( i = 0 ; i < 3 ; i ++ )
      {
         indloc[j][i] = 0 ; /* Initialisation necessary! */
      }
   }

/* First determine the dimensions
*/
   strcpy( filnam[0] , file_name ) ;
   itype        =  0   ; /* Ignored by the routine */
   indloc[0][0] = -2   ; /* Get all dimensions     */
   nocell       =  0   ; /* Number of grid cells   */
   igisty       =  0   ; /* Irrelevant here        */
   l_filnam     = 256  ; /* Length of the filename */
   FOR_delwaq_lga( &filnam[0][0] , l_filnam , &itype  , &indloc[0][0] , idim ,
                   &nocell       , &igisty  , &ierror                        ) ;

/* Check for errors
*/
    if ( ierror != 0 )
    {
       return ErrorReadingFile ;
    }

/* Now allocate the memory
   Do not be too smart: things are partially overwritten by the
   input routine WQ_ReadHydrodynFile(). Hence free all arrays,
   then allocate new ones.
*/
   if ( scenario->grid.cell_index != NULL )
   {
      GE_Free( scenario->grid.cell_index ) ;
      scenario->grid.cell_index = NULL     ;
   }
   if ( scenario->grid.xcoord != NULL )
   {
      GE_Free( scenario->grid.xcoord ) ;
      GE_Free( scenario->grid.ycoord ) ;
      scenario->grid.xcoord = NULL     ;
      scenario->grid.ycoord = NULL     ;
   }
   if ( scenario->grid.depth != NULL )
   {
      GE_Free( scenario->grid.depth ) ;
      scenario->grid.depth = NULL     ;
   }

   if ( scenario->grid.cell_index == NULL )
   {
       scenario->grid.cell_index = (TInt4 *)
          GE_Alloc( sizeof( TInt4 ) * idim[3] ) ;
   }
   if ( scenario->grid.xcoord == NULL )
   {
       scenario->grid.xcoord = (TReal4 *)
          GE_Alloc( sizeof( TReal4 ) * idim[0] * idim[1] ) ;
       scenario->grid.ycoord = (TReal4 *)
          GE_Alloc( sizeof( TReal4 ) * idim[0] * idim[1] ) ;
   }
   if ( scenario->grid.depth == NULL )
   {
       scenario->grid.depth = (TReal4 *)
          GE_Alloc( sizeof( TReal4 ) * idim[0] * idim[1] ) ;

       depth        = (TReal4 *)
                          GE_Alloc( sizeof( TReal4 ) * idim[0] * idim[1] ) ;
   }

/* Set the parameters
*/
/* scenario->grid.type    = Grid_Curvilinear  ; */
   scenario->grid.no_indx = idim[3]           ;
   scenario->grid.ncrd1   = idim[0]           ;
   scenario->grid.ncrd2   = idim[1]           ;
   scenario->grid.nocell  = nocell            ;
   nomat                  = idim[0] * idim[1] ;

   if ( idim[2] == 0 )
   {
      idim[2] = 1 ;
   }
   scenario->grid.ncrd3   = idim[2]           ;

/* TODO: consistency check
   This statement led to some awkward inconsistency problems.
   We should check the correctness of the values in the .hyd file
   against the sizes we find here.
   For now: commented out!

   scenario->wq_model.no_layers             = idim[2]    ;
*/
   scenario->wq_model.no_segments_per_layer = nocell     ;
   scenario->wq_model.no_exchanges[0]       = idim[4]    ;
   scenario->wq_model.no_exchanges[1]       = idim[5]    ;
   scenario->wq_model.no_exchanges[2]       = idim[6]    ;

/* Get the index array
*/
   itype        =  0   ; /* Ignored by the routine */
   indloc[0][0] =  0   ; /* Get all dimensions     */
   nocell       =  0   ; /* Number of grid cells   */
   igisty       =  0   ; /* Irrelevant here        */
   FOR_delwaq_lga( &filnam[0][0]  , l_filnam , &itype  , &indloc[0][0] ,
                   scenario->grid.cell_index    ,
                   &nocell        , &igisty  , &ierror          ) ;

   ipcode       =  1      ; /* X-coordinate           */
   time         =  0.0    ; /* Time (ignored)         */
   indloc[0][0] =  0      ; /* Location indices       */
   valmis       = -999.0  ; /* Missing values         */
   maxdim       = nomat   ; /* Missing values         */

   FOR_delwaq_cco( &filnam[0][0] , l_filnam , &itype  , &ipcode  , &time      ,
                   &indloc[0][0] , &valmis  , &maxdim , scenario->grid.xcoord ,
                   &ierror                                                    ) ;

   ipcode       =  2      ; /* Y-coordinate           */
   FOR_delwaq_cco( &filnam[0][0] , l_filnam , &itype  , &ipcode  , &time      ,
                   &indloc[0][0] , &valmis  , &maxdim , scenario->grid.ycoord ,
                   &ierror                                                    ) ;

/* Get the depth array
*/
   has_depth    =  0   ; /* Do we have depth information? */
   itype        =  0   ; /* Ignored by the routine        */
   indloc[0][0] =  0   ; /* Get all dimensions            */
   nocell       =  0   ; /* Number of grid cells          */
   igisty       =  0   ; /* Irrelevant here               */

   FOR_delwaq_dps( filnam  , l_filnam , &itype  , indloc ,
                   idim    , depth    ,
                   &nocell , &igisty  , &ierror          ) ;

/* Fill the area in case of aggregation.

   Copy extra border of depths just around computational
   grid enclosure ; loop thru grid columns
   (mainly for avoiding nasty interpolation problems
   - using zero depths - in boundary corners)
*/
   if ( ierror == 0 )
   {
      has_depth       = 1                    ;
      for ( i = 0 ; i < idim[0] ; i ++ ) /* loop over n */
      {
         for ( j = 0 ; j < idim[1]-1 ; j ++ ) /* loop over m */
         {
            TInt4 indx1,indx2;
            indx1 = j*idim[0]+i;
            indx2 = scenario->grid.cell_index[indx1] - 1 ;

            scenario->grid.depth[indx1] = 0.0 ;
            if ( indx2 > 0 )
            {
               scenario->grid.depth[indx1] = depth[indx2];
            }
         }
      }

      for ( i = 0 ; i < idim[0] ; i ++ ) /* loop over n */
      {
         for ( j = 0 ; j < idim[1]-2 ; j ++ ) /* loop over m; avoid
                                                 array bound overflow */
         {
            TInt4 indx1,indx2;
            indx1 =   j   *idim[0]+i;
            indx2 =  (j+1)*idim[0]+i;

            if ( scenario->grid.depth[indx1] == 0.0f &&
                 scenario->grid.depth[indx2] != 0.0f )
            {
               /* Overwrite zero depth with neighbouring depth value
               */
               scenario->grid.depth[indx1] = scenario->grid.depth[indx2];
            }
         }
      }

/* Copy extra border of depths just around computational
   grid enclosure ; loop thru grid rows
   (mainly for avoiding nasty interpolation problems
   - using zero depths - in boundary corners)
*/
      for ( j = 0 ; j < idim[1] ; j ++ ) /* loop over m */
      {
         for ( i = 0 ; i < idim[0]-1 ; i ++ ) /* loop over n */
         {
            TInt4 indx1,indx2;
            indx1 =  j*idim[0]+i;
            indx2 =  j*idim[0]+i+1;
            if ( scenario->grid.depth[indx1] ==0.0f &&
                 scenario->grid.depth[indx2] !=0.0f )
            {
               /* Overwrite zero depth with neighbouring depth value
               */
               scenario->grid.depth[indx1] = scenario->grid.depth[indx2];
            }
         }
      }
   }

/* Well, now determine the last parameter:
   the number of boundary cells per layer
*/
   scenario->wq_model.no_bounds_per_layer = 0 ;
   for ( i = 0 ; i < nomat ; i ++ )
   {
      if ( scenario->grid.cell_index[i] < 0 )
      {
         if ( scenario->wq_model.no_bounds_per_layer >
                 scenario->grid.cell_index[i]          )
         {
            scenario->wq_model.no_bounds_per_layer =
               scenario->grid.cell_index[i] ;
         }
      }
   }

   scenario->wq_model.no_bounds_per_layer =
      - scenario->wq_model.no_bounds_per_layer ;

/* Now, make the graphical editor aware of this grid, i.e.
   fill the grid structure
*/
   curv_grid.ncrd1      = scenario->grid.ncrd1      ;
   curv_grid.ncrd2      = scenario->grid.ncrd2      ;
   curv_grid.xcoord     = scenario->grid.xcoord     ;
   curv_grid.ycoord     = scenario->grid.ycoord     ;
   curv_grid.depth      = scenario->grid.depth      ;

   curv_grid.cell_index = ( TInt4 * ) GE_Alloc( sizeof( TInt4 ) * idim[3] ) ;
   curv_grid.status     = ( GEdit_CellStatusEnumType *)
      GE_Alloc( sizeof( GEdit_CellStatusEnumType )*(idim[0]+1)*(idim[1]+1) ) ;

   for ( i = 0 ; i < (idim[0]+1)*(idim[1]+1);  i ++ )
   {
      curv_grid.status[i]     = 9 ;
   }

   for ( i = 0 ; i < idim[0] ; i ++ )  /* loop over n */
   {
      for ( j = 0 ; j < idim[1] ; j ++ ) /* loop over m */
      {
         indx      =  j*idim[0]+i;
         indx_stat =  j*(idim[0]+1)+i;

         curv_grid.cell_index[indx] = scenario->grid.cell_index[indx] ;

         if ( scenario->grid.cell_index[indx] < 0 ) /* boundary cells in *.lga */
         {
            curv_grid.status[indx_stat] = GEdit_BOUNDARY_CELL ; /* boundary cells in GE */
         }
         else if ( scenario->grid.cell_index[indx] > 0 ) /* active cells in *.lga */
         {
            curv_grid.status[indx_stat] = GEdit_ACTIVE_CELL; /* active cells in GE  */
         }
         else
         {
            curv_grid.status[indx_stat] = GEdit_INACTIVE_CELL; /* inactive cell in GE */
         }
      }
   }

   curv_grid.sysType = Cartesian        ;
   GEdit_CreateGrid( &curv_grid , "" )  ;

   if ( has_depth )
   {
      GEdit_CreateDepth( curv_grid.depth ) ;
   }

   GE_Free( depth ) ;
   GE_Free( curv_grid.status );
   GE_Free( curv_grid.cell_index ) ;

/* We are done. Return
*/
   return EverythingOkay ;
}

/* @@------------------------------------------
    Function: WQ_ImportGridFile()
    Author:   Arjen Markus
    Purpose:  Import the grid file(s)
    Context:  Used by WQ_ImportHydrodynFile() and at start-up
    Pseudo Code:
              Branch to the correct routine for the
              particular grid type
 ------------------------------------------------*/
TInt4 WQ_ImportGridFile(
         WaterQualGeneralPtr   general    , /* I General data for UI */
         WaterQualScenarioPtr  scenario   , /* I Scenario data       */
         TString               file_name  ) /* I Grid file           */
                                    /* Returns if successful or not  */
{
   switch ( scenario->grid.type )
   {
   case Grid_Curvilinear:
      return WQ_ImportGridFileCurvilinear( general, scenario, file_name ) ;

   case Grid_FEM :
      /* For the moment */
      return WQ_ImportGridFileFEM( general, scenario, file_name ) ;

   default:
      return ErrorFileType ;
   }
}

#ifdef TEST
int main( int argc, char *argv[] )
{
   TReal4 rvalues[10] ;
   TInt4  ivalues[10] ;
   FILE   *infile ;

   infile = fopen( "vlissingen.geo", "rb" ) ;

   SkipRecord(infile);
   SkipRecord(infile);
   SkipRecord(infile);
   SkipRecord(infile);
   ReadRecordIntegers(infile, ivalues, 2);
   printf( "Record 3: %d %d\n", ivalues[0], ivalues[1] ) ;
   ReadRecordIntegers(infile, ivalues, 10);
   printf( "Record 4:\n" ) ;
   printf( "   %d %d %d\n", ivalues[0], ivalues[1], ivalues[2] ) ;
   printf( "   %d %d %d\n", ivalues[3], ivalues[4], ivalues[5] ) ;
   printf( "   %d %d %d\n", ivalues[6], ivalues[7], ivalues[8] ) ;
   SkipRecord(infile);
   ReadRecordReals(infile, rvalues, 10);
   printf( "Record 4:\n" ) ;
   printf( "   %f %f %f\n", rvalues[0], rvalues[1], rvalues[2] ) ;
   printf( "   %f %f %f\n", rvalues[3], rvalues[4], rvalues[5] ) ;
   printf( "   %f %f %f\n", rvalues[6], rvalues[7], rvalues[8] ) ;

   fclose(infile) ;
}

#endif
