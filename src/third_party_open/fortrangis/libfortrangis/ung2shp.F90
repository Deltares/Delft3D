PROGRAM ung2shp
! Silly application that converts an Arc/Info ungenerate arc file
! into a shapefile, the input file is passed as a command-line argument.
!
! Source of data in this format:
!  http://www.ngdc.noaa.gov/mgg/coast/
! References:
!  http://www.grass-kr.org/html/v.in.arc.html
!  http://www.intevation.de/~jan/gen2shp/gen2shp.html
USE shapelib
IMPLICIT NONE

CHARACTER(len=1024) :: filein, fileout
TYPE(shpobject) :: shapes
TYPE(shpfileobject) :: shapefile
INTEGER,ALLOCATABLE :: shapesize(:)
INTEGER :: i, j, dot, nshape, id
DOUBLE PRECISION,ALLOCATABLE :: shp(:,:)

CALL get_command_argument(1, filein)
dot = INDEX(filein, '.', back=.TRUE.)
IF (dot == 0) THEN
  fileout = TRIM(filein)//'_out'
ELSE IF (dot == 1) THEN
  fileout = 'out'
ELSE
  fileout = filein(:dot-1)
ENDIF

OPEN(10, file=filein)
! count shapes
nshape = 0
DO WHILE(read_ungenerate_shape(10) >= 0)
  nshape = nshape + 1
ENDDO

IF (nshape <= 0) THEN
  PRINT*,'Error, bad number of shapes ',nshape,' in file ',TRIM(filein)
  STOP
ENDIF
PRINT*,'Found ',nshape,' shapes'
ALLOCATE(shapesize(nshape))

REWIND(10)
! count size of each shape
DO i = 1, nshape
  shapesize(i) = read_ungenerate_shape(10)
  IF (shapesize(i) < 0) THEN
    PRINT*,'Error, bad size of shape ',i,shapesize(i),' in file ',TRIM(filein)
    STOP
  ENDIF
ENDDO
PRINT*,'Counted shapes'
ALLOCATE(shp(MAXVAL(shapesize),2))

shapefile = shpcreate(fileout, shpt_arc)
IF (shpfileisnull(shapefile) .OR. dbffileisnull(shapefile)) THEN
  PRINT*,'Error, opening output shapefile ',TRIM(fileout)
  STOP
ENDIF
j = dbfaddfield(shapefile, 'ID', ftinteger, 9, 0)
REWIND(10)
! import each shape
DO i = 1, nshape
  j = read_ungenerate_shape(10, shapeid=id, values=shp)
  shapes = shpcreatesimpleobject(shpt_arc, shapesize(i), shp(1,1), shp(1,2))
  j = shpwriteobject(shapefile, -1, shapes)
  j = dbfwriteattribute(shapefile, i-1, 0, id)
  CALL shpdestroyobject(shapes)
ENDDO

CLOSE(10)
CALL shpclose(shapefile)

CONTAINS

! Reads next shape from ungenerate file.
! Returns the number of points in the shape, -1 for end of file or -2
! in case of error.
FUNCTION read_ungenerate_shape(unit, shapeid, values) RESULT(read_status)
INTEGER,INTENT(in) :: unit
INTEGER,INTENT(out),OPTIONAL :: shapeid
DOUBLE PRECISION,INTENT(out),OPTIONAL :: values(:,:)
INTEGER :: read_status

CHARACTER(len=1024) :: line
INTEGER :: npts, nptsmax, lshapeid
DOUBLE PRECISION :: x(2)

nptsmax = 0
IF (PRESENT(values)) THEN
  IF (SIZE(values,2) >= 2) THEN
    nptsmax = SIZE(values,1)
  ENDIF
ENDIF

READ(unit,'(A)',end=120,err=120)line
IF (line == 'END') THEN ! end of file
  read_status = -1
  RETURN
ENDIF

READ(line,'(I10.0)',end=120,err=120) lshapeid
IF (PRESENT(shapeid)) shapeid = lshapeid
IF (lshapeid < 0) GOTO 120

npts=0
DO WHILE (.TRUE.)
  READ(unit,'(A)',end=120,err=120)line
  IF (line == 'END') THEN ! end of shape
    read_status = npts
    RETURN
  ELSE
    READ(line,*,end=120,err=120)x
    npts = npts + 1
    IF (nptsmax >= npts) values(npts,1:2) = x(:)
  ENDIF
ENDDO
  
RETURN ! never reached

120 read_status = -2
RETURN

END FUNCTION read_ungenerate_shape

END PROGRAM ung2shp

