! ---- LGPL --------------------------------------------------------------------
!
! Copyright (C)  Stichting Deltares, 2011-2024.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation version 2.1.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
! contact: delft3d.support@deltares.nl
! Stichting Deltares
! P.O. Box 177
! 2600 MH Delft, The Netherlands
!
! All indications and logos of, and references to, "Delft3D" and "Deltares"
! are registered trademarks of Stichting Deltares, and remain the property of
! Stichting Deltares. All rights reserved.
!
!------------------------------------------------------------------------------
! @begin@
!
! getlga.f   -  ODS routine to get the index array for DELWAQ/DELPAR
!               grid files
!
! General information:
! This file contains the following routines:
! - ODS_DELWAQ_UNF_LGA: Read the grid file to return the index array
! - ODS_DELWAQ_UNF_CCO: Read the coordinates file to return the
!                       x- and y-coordinates
!
! $Author: Markus $
! $Date: 11/15/00 3:51p $
! $Source: /u/cvsroot/gpp/libsrc/ods/getlga.f,v $
!
!
! @@------------------------------------------------------------------
!   Subroutine: ODS_DELWAQ_UNF_LGA
!   Author:     Arjen Markus
!   Purpose:    Get dimensions and index array of DELWAQ/DELPAR grids
!   Context:    Used by GETDIM and GETGRD (ODS) via ODSGet*Dlwg()
!   Pseudo Code:
!               construct the proper filenames
!               check whether the files exist
!               read the dimension records and check consistency
!               if that is that is needed (GETDIM) return, otherwise
!               read the index array with special attention to layers
!               (this information is not an integral part of the
!               file, but must be constructed)
!   Note:
!               we make two assumptions:
!               - we always read the whole grid
!               - the array INDLOC is filled in a particular way
!                 (should become a clear part of the ODS documentation)
! --------------------------------------------------------------------
!
SUBROUTINE ODS_DELWAQ_UNF_LGA&
&(FILNAM , ITYPE  , INDLOC , INDX   ,&
&NOCELL , IGISTY , IERROR          )
! --------------------------------------------------------------------
! Subroutine for determining the type of grid files and reading the
! sizes of the grid.
! Note: for DELWAQ/DELPAR grid files (based on TRISULA)
!
! Arguments:
! Name    Type    Dimensions   I/O
! FILNAM  CH*256  3            I/O  name of the grid files:
!                                   1. LGRID file (extension: .lga)
!                                   2. TELMAC file (extension: .cco)
!                                   3. (not used)
! ITYPE   I*4     -             I   type of grid files (ignored)
! INDLOC  I*4     3*3           I   location indices of interest:
!                                   if INDLOC(1,1) = -1: return dimensions
!                                   only.
! INDX    I*4     ?             O   index array to be returned
!                                   (will contain dimensions if INDLOC(1,1)
!                                   equal -1
! NOCELL  I*4     -             O   total number of cells for DELWAQ model
! IGISTY  I*4     -             O   type of grid (always IGCURV
! IERROR  I*4     -             O   ODS error code
! --------------------------------------------------------------------
!
   INTEGER*4 LENODS
   PARAMETER ( LENODS = 256 )
   CHARACTER*(LENODS) FILNAM
   INTEGER*4 ITYPE  , IGISTY , INDLOC , INDX   , NOCELL , IERROR
   DIMENSION FILNAM(*) , INDLOC(3,*) , INDX(*)
!
   INTEGER*4 K1     , K2     , K3     , K4     , K      , I      ,&
   &LUN1   , LUN2   , grid_width     , grid_depth     , NX2    , NY2    ,&
   &NOLAY  , NODATA , NCLAY  , IDUMMY , IDUMM1 , IDUMM2 ,&
   &IDUMM3
   LOGICAL*4 OPEND  , EXIST1 , EXIST2
!
   INCLUDE 'ods.inc'
!
! -------- Extension of the first file: if .dat, .DAT or .def, .DEF
!          assume NEFIS file type.
!
   IERROR = 0
   IGISTY = IGCURV
!
   K1     = INDEX( FILNAM(1) , '.dat' )
   K2     = INDEX( FILNAM(1) , '.DAT' )
   K3     = INDEX( FILNAM(1) , '.def' )
   K4     = INDEX( FILNAM(1) , '.DEF' )
   K      = K1     + K2    + K3    + K4
   IF ( K      .NE. 0 ) THEN
      ITYPE  = 3
!
! Not implemented yet
!
   ELSE
!
! -------- Construct the proper filenames: the first is the LGRID-table
!          with extension ".lga", the second the TELMAC-file with
!          exntension ".cco"
!
      K1     = INDEX( FILNAM(1) , CHAR(0) )
      IF ( K1     .EQ. 0 ) K1     = LENODS - 1
      DO I = K1-1,1,-1
         IF ( FILNAM(1)(I:I) .EQ. '.' ) THEN
            FILNAM(1)(I:) = '.lga'
            FILNAM(2)     = FILNAM(1)
            FILNAM(2)(I:) = '.cco'
            GOTO 120
         ENDIF
      END DO
!
! -------- Check whether the files exist
!
120   CONTINUE
      INQUIRE( FILE = FILNAM(1) , EXIST = EXIST1 )
      INQUIRE( FILE = FILNAM(2) , EXIST = EXIST2 )
      IF ( .NOT. ( EXIST1 .AND. EXIST2 ) ) THEN
         IERROR = IENOFI
         RETURN
      ENDIF
!
! -------- Now read the record with the dimensions
!
!
! -------- Open the files: either binary or unformatted
!          Note:
!          On systems where the usual FORTRAN compiler
!          supports binary files, the files to be read ARE
!          binary. As there is no secure way to identify
!          the character of the files, simply assume the
!          binary file type. If not, we will be notified.
!
!          Note:
!          Read two records to be sure everything is all right
!
      OPEN( NEWUNIT = LUN1    , FILE = FILNAM(1)(1:K1) , STATUS = 'OLD' ,&
      &ACCESS = 'STREAM' , ERR = 210 )
      READ( LUN1   , ERR  = 210 , END = 910 ) grid_width
      READ( LUN1   , ERR  = 210 , END = 910 ) grid_depth
      REWIND( LUN1   )
      GOTO 220
!
210   CONTINUE
      CLOSE( LUN1   )
      OPEN( NEWUNIT = LUN1       , FILE = FILNAM(1)(1:K1) , STATUS = 'OLD' ,&
      &FORM = 'UNFORMATTED' , ERR = 900 )
!
220   CONTINUE
      OPEN( NEWUNIT = LUN2    , FILE = FILNAM(2)(1:K1) , STATUS = 'OLD' ,&
      &ACCESS = 'STREAM' , ERR = 230 )
      READ( LUN2   , ERR  = 230 , END = 910 )
      READ( LUN2   , ERR  = 230 , END = 910 ) NY2
      REWIND( LUN2   )
      GOTO 240
!
230   CONTINUE
      CLOSE( LUN2   )
      OPEN( NEWUNIT = LUN2       , FILE = FILNAM(2)(1:K1) , STATUS = 'OLD' ,&
      &FORM = 'UNFORMATTED' , ERR = 900 )
!
! -------- Read the first record of the LGRID file
!          (Unfortunately there are several variations)
!
240   CONTINUE
      READ( LUN1   , ERR = 300 , END = 910 )&
      &grid_width     , grid_depth     , NOCELL , NOLAY  , IDUMM1 , IDUMM2 , IDUMM3
      GOTO 330
!
! -------- In case of an error, try a shorter record
!
300   CONTINUE
      REWIND( LUN1   )
      READ( LUN1   , ERR = 310 , END = 910 )&
      &grid_width     , grid_depth     , NOCELL , IDUMMY , NOLAY
      GOTO 330
!
310   CONTINUE
      REWIND( LUN1   )
      READ( LUN1   , ERR = 320 , END = 910 )&
      &grid_width     , grid_depth     , NOCELL , NOLAY
      GOTO 330
!
320   CONTINUE
      NOLAY  = 1
      REWIND( LUN1   )
      READ( LUN1   , ERR = 910 , END = 910 )&
      &grid_width     , grid_depth     , NOCELL
      GOTO 330
!
! -------- Skip the first and read the second record of
!          the TELMAC file:
!          it might not be a TELMAC file at all
!
330   CONTINUE
      READ( LUN2   , ERR = 910 , END = 910 )
      READ( LUN2   , ERR = 910 , END = 910 )&
      &NY2    , NX2
      CLOSE( LUN2   )
!
      IF ( grid_width     .NE. NX2    .OR. grid_depth     .NE. NY2    ) GOTO 920
      IF ( grid_width     .LE. 0      .OR. grid_depth     .LE. 0      ) GOTO 920
      IF ( NOCELL .LE. 0                              ) GOTO 920
   ENDIF
!
! -------- Have we read what we need? If so, fill the array and return
!
   IF ( INDLOC(1,1) .EQ. -1 ) THEN
      INDX(1)   = grid_width
      INDX(2)   = grid_depth
      INDX(3)   = NOLAY
      INDX(4)   = grid_width     * grid_depth
      CLOSE( LUN1   )
      RETURN
   ENDIF
!
! -------- Otherwise, read the LGRID file.
!          ASSUMPTION: we read the whole grid (one layer), not more,
!          not less
!
   NODATA = grid_width     * grid_depth
   READ( LUN1   , ERR = 910 , END = 910 ) ( INDX(I)  , I = 1,NODATA )
   CLOSE( LUN1   )
!
! -------- Take care of the layers
!
   IF ( INDLOC(1,3) .GE. 2 ) THEN
      NCLAY  = NOCELL * ( INDLOC(1,3) - 1 )
      DO I = 1,NODATA
         IF ( INDX(I)   .GT. 1 ) INDX(I)   = INDX(I)  + NCLAY
      END DO
   ENDIF
!
   RETURN
!
! -------- Error handling:
!          - file(s) do(es) not exist
!          - read error occurred
!          - files are not consistent or do not contain the right
!            information
!
900 CONTINUE
   IERROR = IENOFI
   CLOSE( LUN1   )
   RETURN
!
910 CONTINUE
   IERROR = IEOTHR
   CLOSE( LUN1   )
   RETURN
!
920 CONTINUE
   IERROR = IEINFO
   RETURN
!
END
!
! @@------------------------------------------------------------------
!   Subroutine: ODS_DELWAQ_UNF_CCO
!   Author:     Arjen Markus
!   Purpose:    Get coordinates from DELWAQ/DELPAR grid files
!   Context:    Used by GETMAT (ODS) via ODSGetMatDlwg()
!   Pseudo Code:
!               read the dimension records
!               skip all intermediate records
!               read the x-coordinate or the y-coordinate
!   Note:
!               as for GETLGA a full grid is returned
! --------------------------------------------------------------------
!
SUBROUTINE ODS_DELWAQ_UNF_CCO&
&(&
&FILNAM , ITYPE  , IPCODE , TIME   , INDLOC ,&
&VALMIS , MAXDIM , DATA   , IERROR          )
! --------------------------------------------------------------------
! Subroutine for determining the type of grid files and reading the
! sizes of the grid.
! Note: for TRISULA grids.
!
! Arguments:
! Name    Type    Dimensions   I/O
! FILNAM  CH*256  3            I/O  name of the grid files:
!                                   1. LGRID file (extension: .lga)
!                                   2. TELMAC file (extension: .cco)
!                                   3. (not used)
! ITYPE   I*4     -             I   type of grid files (ignored)
! IPCODE  I*4     -             I   parameter code (1 or 2)
! TIME    R*8     -             I   time nterval for which data are required
!                                   (ignored)
! INDLOC  I*4     3*3           I   location indices of interest
! VALMIS  R*4     -             I   value reserved for missing value
! MAXDIM  I*4     -             I   maximum number of data
! DATA    R*4     -             O   data (x-coordinate or y-coordinate)
!                                   returned
! IERROR  I*4     -             O   ODS error code
! --------------------------------------------------------------------
!
   INTEGER*4 LENODS
   PARAMETER( LENODS = 256 )
   CHARACTER*(LENODS) FILNAM(*)
!
   INCLUDE 'ods.inc'
!
   INTEGER*4 ITYPE  , INDLOC , MAXDIM , IERROR , IPCODE
   REAL*4    DATA   , VALMIS
   REAL*8    TIME
   DIMENSION INDLOC(3,*) , DATA(*)   , TIME(*)
!
   INTEGER*4 K2     , I      , LUN2   , NX2    , NY2    ,&
   &NPART  , NODATA , NOLAY
   REAL*4    XDUMMY , YDUMMY , ALPHA
   LOGICAL*4 OPEND
   CHARACTER*20 UNFORM , BINARY
   DATA         UNFORM , BINARY / 'UNFORMATTED' , 'BINARY' /
!
   IERROR = IEOK
!
! -------- NEFIS file?
!
!     IF ( ITYPE  .EQ. 3 ) THEN
!
! Not implemented yet
!
!
! -------- Unformatted files:
!          read the record with the dimensions
!
   K2     = INDEX( FILNAM(2) , CHAR(0) )
   IF ( K2     .EQ. 0 ) K2     = LENODS
   OPEN( NEWUNIT = LUN2, FILE = FILNAM(2)(1:K2) , STATUS = 'OLD' ,&
   &FORM = BINARY , ERR = 210 )
   READ( LUN2   , ERR = 210 , END = 910 )
   READ( LUN2   , ERR = 210 , END = 910 ) NY2
   REWIND( LUN2 )
   GOTO 300
!
210 CONTINUE
   CLOSE( LUN2   )
   OPEN( NEWUNIT = LUN2, FILE = FILNAM(2)(1:K2) , STATUS = 'OLD' ,&
   &FORM = UNFORM , ERR = 900 )
!
! -------- Read the second record of the TELMAC file:
!          determine how many records to skip
!          (Check the first actual record!)
!
300 CONTINUE
   READ( LUN2   , ERR = 910 , END = 910 )
   READ( LUN2   , ERR = 310 , END = 910 )&
   &NY2    , NX2    , XDUMMY , YDUMMY , ALPHA  , NPART  , NOLAY
   GOTO 320
!
310 CONTINUE
   REWIND( LUN2   )
   READ( LUN2   , ERR = 910 , END = 910 )
   READ( LUN2   , ERR = 910 , END = 910 )&
   &NY2    , NX2    , XDUMMY , YDUMMY , ALPHA  , NPART
   GOTO 320
!
320 CONTINUE
   DO I = 1,NPART+9
      READ( LUN2   , ERR = 910 , END = 910 ) XDUMMY
   END DO
!
   NODATA = NX2    * NY2
   IF ( NODATA .GT. MAXDIM ) THEN
      IERROR = IELMNY
      CLOSE( LUN2   )
      RETURN
   ENDIF
!
   IF ( IPCODE .EQ. 2 )&
   &READ( LUN2   , ERR = 910 , END = 910 )&
   &( XDUMMY , I = 1,NODATA )
!
   READ( LUN2   , ERR = 910 , END = 910 )&
   &( DATA(I)   , I = 1,NODATA )
!
   CLOSE( LUN2   )
!
! -------- We have read what we need. Return
!
   RETURN
!
! -------- Error handling (any error that occurs)
!
900 CONTINUE
   IERROR = IENOFI
   CLOSE( LUN2   )
   RETURN
910 CONTINUE
   IERROR = IEOTHR
   CLOSE( LUN2   )
   RETURN
!
END
