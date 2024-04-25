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

PROGRAM shp_fxtr
USE,INTRINSIC :: ISO_C_BINDING
USE shapelib
IMPLICIT NONE

INTEGER,PARAMETER :: lencharattr=40, nshp=4, tshp=shpt_polygonz

TYPE(shpfileobject) :: shphandle
TYPE(shpobject) :: shpobj
INTEGER :: i, j
CHARACTER(len=1024) :: filein, fileout, filereg

INTEGER :: nshpr, tshpr, nfield, nrec, nd
REAL(kind=c_double) :: minbound(4), maxbound(4)
CHARACTER(len=lencharattr) :: charattrr
INTEGER :: intattrr
REAL(kind=c_double) :: doubleattrr

CALL get_command_argument(1,filein)
CALL get_command_argument(2,fileout)
CALL get_command_argument(3,filereg)
IF (filein == '' .OR. fileout == '' .OR. filereg == '') THEN
  PRINT'(A)','Convert a shapefile (with polygons) in the dump format required by'
  PRINT'(A)','the shape2fxtr.pl utilities of fieldextra and directly in the'
  PRINT'(A)','region file format required by fieldextra.'
  PRINT'(A)','Usage: shp_fxtr <shp_file> <dump_file> <reg_file>'
  STOP
ENDIF

! open an exixting shapefile and associate it to a shapefile object
! filename does not include extension
shphandle = shpopen(TRIM(filein), 'rb')
! error check
IF (shpfileisnull(shphandle) .OR. dbffileisnull(shphandle)) THEN
  PRINT*,'Error opening ',TRIM(filein),' for reading'
  STOP 1
ENDIF

! get general information about the shapefile object
CALL shpgetinfo(shphandle, nshpr, tshpr, minbound, maxbound, nfield, nrec)
PRINT*,'number and type of shapes:',nshpr,tshpr

OPEN(10, file=fileout)
OPEN(11, file=filereg)
! read the nshp shapes
DO i = 0, nshpr - 1
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
  PRINT*,'nvertices:',shpobj%nvertices
  IF (ASSOCIATED(shpobj%panpartstart)) THEN
!    PRINT*,'nparts:',SIZE(shpobj%panpartstart)
!    PRINT*,shpobj%panpartstart
    IF (SIZE(shpobj%panpartstart) > 1) THEN
      PRINT*,'Warning, multipart shapes not supported in the conversion'
      PRINT*,'skipping shape ',i+1
      CYCLE
    ENDIF
  ENDIF
  IF (shpobj%nvertices >= 3) THEN
! simple shp dump file
    DO j = 1, shpobj%nvertices
      WRITE(10,'(F0.5,1X,F0.5,1X,I0)')shpobj%padfx(j),shpobj%padfy(j),i+1
    ENDDO
! region file
    WRITE(11,'(I4,1X,I6,1X,''region_'',I0)')0,shpobj%nvertices,i+1
    WRITE(11,'(*(E10.5,1X))')shpobj%padfy(:) ! warning, f2008-style comment
    WRITE(11,'(*(E10.5,1X))')shpobj%padfx(:)
    WRITE(11,'()')
  ENDIF

! destroy the shape object to avoid memory leaks
! notice that for accessing dbf attributes the shape object is not required
  CALL shpdestroyobject(shpobj)

ENDDO

! close the shapefile object
CALL shpclose(shphandle)
CLOSE(10)
CLOSE(11)


END PROGRAM shp_fxtr

