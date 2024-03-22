!    Copyright 2011 Davide Cesari <dcesari69 at gmail dot com>
!
!    This file is part of FortranGIS.
!
!    FortranGIS is free software: you can redistribute it and/or modify
!    it under the terms of the GNU Lesser General Public License as
!    published by the Free Software Foundation, either version 3 of the
!    License, or (at your option) any later version.
!
!    FortranGIS is distributed in the hope that it will be useful, but
!    WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!    Lesser General Public License for more details.
!
!    You should have received a copy of the GNU Lesser General Public
!    License along with FortranGIS.  If not, see
!    <http://www.gnu.org/licenses/>.

PROGRAM shapelib_test
USE,INTRINSIC :: ISO_C_BINDING
USE shapelib
IMPLICIT NONE

INTEGER,PARAMETER :: lencharattr=40, nshp=4, tshp=shpt_polygonz

TYPE(shpfileobject) :: shphandle
TYPE(shpobject) :: shpobj
INTEGER :: i, j
CHARACTER(len=1024) :: filename

INTEGER :: nshpr, tshpr, nfield, nrec, nd
REAL(kind=c_double) :: minbound(4), maxbound(4)
CHARACTER(len=lencharattr) :: charattrr
INTEGER :: intattrr
REAL(kind=c_double) :: doubleattrr

!CALL get_command_argument(1,filename)
!IF (filename == '') THEN
!  PRINT'(A)','Usage: shape_test <shp_file>'
!  STOP
!ENDIF
filename = 'testshape'

! ==== How to create a shapefile ====

! create a new shapefile object with data of type tshp (polygon)
! and associate it to a file, filename does not include extension
shphandle = shpcreate(TRIM(filename), tshp)
! error check
IF (shpfileisnull(shphandle) .OR. dbffileisnull(shphandle)) THEN
  PRINT*,'Error opening ',TRIM(filename),' for writing'
  STOP 1
ENDIF

! add 3 dbf field, of type character, integer and double respectively
j = dbfaddfield(shphandle, 'name', ftstring, lencharattr, 0)
IF (j /= 0) THEN
  PRINT*,'Error in dbfaddfield',0,j
  STOP 1
ENDIF
j = dbfaddfield(shphandle, 'number', ftinteger, 10, 0)
IF (j /= 1) THEN
  PRINT*,'Error in dbfaddfield',1,j
  STOP 1
ENDIF
j = dbfaddfield(shphandle, 'size', ftdouble, 30, 20)
IF (j /= 2) THEN
  PRINT*,'Error in dbfaddfield',2,j
  STOP 1
ENDIF

! add nshp shapes, of different lengths
DO i = 0, nshp - 1
  PRINT*,'Creating shape',i
! create a shape object with the "simple" method
! for each shape 3 components are added x, y, z
! the type of shape has to be repeated here
! makesimpleshp() is an utility function returning an array
  shpobj = shpcreatesimpleobject(tshp, &
   SIZE(makesimpleshp(i, 0)), &
   makesimpleshp(i, 0), &
   makesimpleshp(i, 1), &
   makesimpleshp(i, 2))

! write the shape object to the shapefile object as i-th element
! -1 = append
  j = shpwriteobject(shphandle, -1, shpobj)
  IF (j /= i) THEN
    PRINT*,'Error in shpwriteobject',i,j
    STOP 1
  ENDIF

! destroy the shape object to avoid memory leaks
  CALL shpdestroyobject(shpobj)

! write the 3 dbf attributes of different types for the i-th shape
! object to the shapefile object
! makechardbf(), makeintdbf() and makedoubledbf() are utility functions
! returning an attribute of the proper type
  j = dbfwriteattribute(shphandle, i, 0, makechardbf(i))
  IF (j /= 1) THEN
    PRINT*,'Error in dbfwriteattribute, char',j
    STOP 1
  ENDIF
  j = dbfwriteattribute(shphandle, i, 1, makeintdbf(i))
  IF (j /= 1) THEN
    PRINT*,'Error in dbfwriteattribute, int',j
    STOP 1
  ENDIF
  j = dbfwriteattribute(shphandle, i, 2, makedoubledbf(i))
  IF (j /= 1) THEN
    PRINT*,'Warning in dbfwriteattribute, double',j
  ENDIF

ENDDO

! close the shapefile object
CALL shpclose(shphandle)


! ==== How to read a shapefile ====

! open an exixting shapefile and associate it to a shapefile object
! filename does not include extension
shphandle = shpopen(TRIM(filename), 'rb')
! error check
IF (shpfileisnull(shphandle) .OR. dbffileisnull(shphandle)) THEN
  PRINT*,'Error opening ',TRIM(filename),' for reading'
  STOP 1
ENDIF

! get general information about the shapefile object
CALL shpgetinfo(shphandle, nshpr, tshpr, minbound, maxbound, nfield, nrec)
IF (nshpr /= nshp) THEN
  PRINT*,'Error in shpgetinfo, wrong number of shapes',nshp,nshpr
  STOP 1
ENDIF
IF (tshpr /= tshp) THEN
  PRINT*,'Error in shpgetinfo, wrong type of shapes',tshp,tshpr
  STOP 1
ENDIF
IF (nfield /= 3) THEN
  PRINT*,'Error in shpgetinfo, wrong number of fields',3,nfield
  STOP 1
ENDIF
IF (nrec /= nshp) THEN
  PRINT*,'Error in shpgetinfo, wrong number of records',nshp,nrec
  STOP 1
ENDIF

! read the nshp shapes
DO i = 0, nshp - 1
  PRINT*,'Checking shape',i
! read the i-th shape from the shapefile object and obtain a shape object
  shpobj = shpreadobject(shphandle, i)
! error check
  IF (shpisnull(shpobj)) THEN
    PRINT*,'Error in shpreadobject',i
    STOP 1
  ENDIF

! now access all the components of the shape object
! number of vertices
  IF (shpobj%nvertices /= SIZE(makesimpleshp(i,0))) THEN
    PRINT*,'Error in shpreadobject, wrong number of vertices',i,&
     SIZE(makesimpleshp(i,0)),shpobj%nvertices
    STOP 1
  ENDIF
! x shape data
  IF (ANY(shpobj%padfx(:) /= makesimpleshp(i,0))) THEN
    PRINT*,'Error in shpreadobject, discrepancies in x',i
    PRINT*,makesimpleshp(i,0)
    PRINT*,shpobj%padfx(:)
    STOP 1
  ENDIF
! y shape data
  IF (ANY(shpobj%padfy(:) /= makesimpleshp(i,1))) THEN
    PRINT*,'Error in shpreadobject, discrepancies in y',i
    PRINT*,makesimpleshp(i,1)
    PRINT*,shpobj%padfy(:)
    STOP 1
  ENDIF
! z shape data
  IF (ANY(shpobj%padfz(:) /= makesimpleshp(i,2))) THEN
    PRINT*,'Error in shpreadobject, discrepancies in z',i
    PRINT*,makesimpleshp(i,2)
    PRINT*,shpobj%padfz(:)
    STOP 1
  ENDIF

! destroy the shape object to avoid memory leaks
! notice that for accessing dbf attributes the shape object is not required
  CALL shpdestroyobject(shpobj)

! get the dbf attributes for the i-th shape in the shapefile object
! first field (character)
  CALL dbfreadattribute(shphandle, i, 0, charattrr)
  IF (charattrr /= makechardbf(i)) THEN
    PRINT*,'Error in dbfreadattribute, discrepancies in char'
    PRINT*,makechardbf(i)
    PRINT*,charattrr
    STOP 1
  ENDIF
! second field (integer)
  CALL dbfreadattribute(shphandle, i, 1, intattrr)
  IF (intattrr /= makeintdbf(i)) THEN
    PRINT*,'Error in dbfreadattribute, discrepancies in int'
    PRINT*,makeintdbf(i)
    PRINT*,intattrr
    STOP 1
  ENDIF
! third field (double)
  CALL dbfreadattribute(shphandle, i, 2, doubleattrr)
  IF (doubleattrr /= makedoubledbf(i)) THEN
! here discreapancies are tolerated
    PRINT*,'Warning in dbfreadattribute, discrepancies in double'
    PRINT*,makedoubledbf(i)
    PRINT*,doubleattrr
  ENDIF

ENDDO

! test whether an attribute is null
IF (dbfisattributenull(shphandle, 0, 0)) THEN
  PRINT*,'Error in dbfisattributenull, non null attribute returned null'
  STOP 1
ENDIF
IF (.NOT.dbfisattributenull(shphandle, 3, 0)) THEN
  PRINT*,'Error in dbfisattributenull, null attribute returned non null'
  STOP 1
ENDIF

! close the shapefile object
CALL shpclose(shphandle)

CONTAINS

! Functions for generating predictable shp and dbf values
FUNCTION makesimpleshp(nshp, ncoord) RESULT(shp)
INTEGER,INTENT(in) :: nshp, ncoord
REAL(kind=c_double) :: shp(nshp+2)

INTEGER :: i

shp(:) = (/(-100.0_c_double + &
 10.0_c_double*i + 100.0_c_double*nshp + 1000.0_c_double*ncoord, &
 i=1, SIZE(shp))/)

END FUNCTION makesimpleshp

FUNCTION makechardbf(nshp) RESULT(dbf)
INTEGER,INTENT(in) :: nshp
CHARACTER(len=lencharattr) :: dbf

INTEGER :: i

IF (nshp == 3) THEN
  dbf= ' '
ELSE
  DO i = 1, LEN(dbf)
    dbf(i:i) = CHAR(32 + MOD(i+2*nshp,32))
  ENDDO
ENDIF


END FUNCTION makechardbf

FUNCTION makeintdbf(nshp) RESULT(dbf)
INTEGER,INTENT(in) :: nshp
INTEGER :: dbf

dbf = -118 + 47*nshp

END FUNCTION makeintdbf

FUNCTION makedoubledbf(nshp) RESULT(dbf)
INTEGER,INTENT(in) :: nshp
REAL(kind=c_double) :: dbf

dbf = -5.894823E+12_c_double + 8.4827943E+11*nshp

END FUNCTION makedoubledbf

END PROGRAM shapelib_test

