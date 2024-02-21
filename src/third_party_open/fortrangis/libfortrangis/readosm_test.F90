MODULE readosm_test
USE readosm
USE fortranc
USE,INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

CONTAINS

! subroutine for parsing a file using user-supplied callback
SUBROUTINE readosm_test_setup()

INTEGER :: err
TYPE(c_ptr) :: handle ! readosm file object
CHARACTER(len=512) :: file

CALL get_command_argument(1, file)
IF (LEN_TRIM(file) == 0) file = 'readosm_test.osm'

! open osm file and get a file object
PRINT*,'Opening osm file ',TRIM(file)
err = readosm_open(fchartrimtostr(file), handle)
IF (err /= READOSM_OK) THEN
  PRINT*,'Error opening osm file ',err
  STOP 1
ENDIF

! parse the file passing, as node_fnct argument, a user-defined
! function (see below) which will be called whenever a node
! (georeferenced point) is read from the osm file
PRINT*,'Parsing osm file with user-defined callback'
err = readosm_parse(handle, c_null_ptr, &
 node_fnct=node_callback)
IF (err /= READOSM_OK) THEN
  PRINT*,'Error parsing osm file ',err
  STOP 1
ENDIF
 

PRINT*,'Closing osm file'
err = readosm_close(handle)
IF (err /= READOSM_OK) THEN
  PRINT*,'Error closing osm file ',err
  STOP 1
ENDIF

END SUBROUTINE readosm_test_setup


! definition of the node callback function
FUNCTION node_callback(user_data, node)
TYPE(c_ptr),VALUE :: user_data
TYPE(readosm_node) :: node
INTEGER(kind=c_int) :: node_callback

TYPE(readosm_node_f) :: node_f
INTEGER :: i

! display coordinates
PRINT*,node%longitude,node%latitude

! convert to a more Fortran-friendly object
node_f = readosm_object_f(node)

! scan the attribute list if present
IF (ALLOCATED(node_f%tags)) THEN
  DO i = 1, SIZE(node_f%tags)
!    WRITE(*,'(A,'' = '',A)')TRIM(strtofchar(node_f%tags(i)%key)), &
!     TRIM(strtofchar(node_f%tags(i)%value))
  ENDDO
ENDIF

! set the return code to OK, otherwise parsing will stop
node_callback = READOSM_OK

END FUNCTION node_callback


! subroutine for fully parsing a file using predefined callbacks
SUBROUTINE readosm_test_setup_full()

INTEGER :: err
TYPE(c_ptr) :: handle ! readosm file object
TYPE(readosm_full_f) :: fulldata
CHARACTER(len=512) :: file
INTEGER :: i, j
CHARACTER(len=1),ALLOCATABLE :: key(:), val(:)

CALL getarg(1, file)
IF (LEN_TRIM(file) == 0) file = 'readosm_test.osm'

! open osm file and get a file object
PRINT*,'Opening osm file ',TRIM(file)
err = readosm_open(fchartrimtostr(file), handle)
IF (err /= READOSM_OK) THEN
  PRINT*,'Error opening osm file ',err
  STOP 1
ENDIF

! parse the file with predefined callbacks
PRINT*,'Parsing osm file with predefined callbacks'
err = readosm_parse_full_f(handle, fulldata)
IF (err /= READOSM_OK) THEN
  PRINT*,'Error parsing osm file ',err
  STOP 1
ENDIF

! data has to be used before closing the file, otherwise allocated
! buffers may be lost

DO j = 1, fulldata%nodes%arraysize
  PRINT*,fulldata%nodes%array(j)%longitude,fulldata%nodes%array(j)%latitude
  IF (ALLOCATED(fulldata%nodes%array(j)%tags)) THEN
    DO i = 1, SIZE(fulldata%nodes%array(j)%tags)
      key = fulldata%nodes%array(j)%tags(i)%key
      val = fulldata%nodes%array(j)%tags(i)%value
!      WRITE(*,'(A,''='',A)')strtofchar(key),strtofchar(val)
!      WRITE(*,*)fulldata%nodes%array(j)%tags(i)%key, &
!       fulldata%nodes%array(j)%tags(i)%value
    ENDDO
  ENDIF

ENDDO

PRINT*,'Closing osm file'
err = readosm_close(handle)
IF (err /= READOSM_OK) THEN
  PRINT*,'Error closing osm file ',err
  STOP 1
ENDIF



END SUBROUTINE readosm_test_setup_full


END MODULE readosm_test


PROGRAM readosm_test_main
USE readosm_test
IMPLICIT NONE

CALL readosm_test_setup()
CALL readosm_test_setup_full()

END PROGRAM readosm_test_main
