PROGRAM fortranc_test
USE,INTRINSIC :: ISO_C_BINDING
USE fortranc
IMPLICIT NONE

! interface to functions in c_test.c
INTERFACE
FUNCTION return_null_charp() BIND(C)
IMPORT
TYPE(c_ptr) :: return_null_charp
END FUNCTION return_null_charp

FUNCTION return_empty_charp() BIND(C)
IMPORT
TYPE(c_ptr) :: return_empty_charp
END FUNCTION return_empty_charp

FUNCTION return_8_charp() BIND(C)
IMPORT
TYPE(c_ptr) :: return_8_charp
END FUNCTION return_8_charp

FUNCTION return_c_ptr_ptr() BIND(C)
IMPORT
TYPE(c_ptr) :: return_c_ptr_ptr
END FUNCTION return_c_ptr_ptr
END INTERFACE

TYPE(c_ptr_ptr) :: strarrp
INTEGER :: i


! ==== How to use strlen for a char* null-terminated C string ====

PRINT*,'Testing strlen with C char* argument'
! A NULL pointer should return zero (not obvious, for safety)
IF (strlen(return_null_charp()) /= 0) THEN
  PRINT*,'Error in strlen: a NULL char* does not return zero, ', &
   strlen(return_null_charp())
  STOP 1
ENDIF
! A zero-len string should return zero (obvious)
IF (strlen(return_empty_charp()) /= 0) THEN
  PRINT*,'Error in strlen: a zero len char* does not return zero, ', &
   strlen(return_empty_charp())
  STOP 1
ENDIF
! A 8-char string should return 8 (obvious)
IF (strlen(return_8_charp()) /= 8) THEN
  PRINT*,'Error in strlen: a nonzero len char* does not return expected len (8), ', &
   strlen(return_8_charp())
  STOP 1
ENDIF

PRINT*,'Strlen returns the expected values'


! ==== How to use c_ptr_ptr for decoding a char** object received from C ====

! get a char** object from C function return_c_ptr_ptr(), the function
! result has been declared as:
! char* var[4] = { "first", "segundo", "troisieme", NULL }
PRINT*,'Getting a c_ptr_ptr object from C'
strarrp = c_ptr_ptr_new(return_c_ptr_ptr())
!IF (.NOT.C_ASSOCIATED(strarrp)) THEN
!  PRINT*,'Error in c_ptr_ptr_new, got a NULL pointer'
!ENDIF

! get the number of valid pointers in strarrp
PRINT*,'The object has ',c_ptr_ptr_getsize(strarrp),' elements'
IF (c_ptr_ptr_getsize(strarrp) /= 3) THEN
  PRINT*,'Error in c_ptr_ptr_getsize:',3,c_ptr_ptr_getsize(strarrp)
  STOP 1
ENDIF

! get the content of selected pointers as a Fortran CHARACTER variable
! of the right length, count starts from 1
IF (strtofchar(c_ptr_ptr_getptr(strarrp, 1),100) /= 'first') THEN
  PRINT*,'Error in c_ptr_ptr_getptr:',strtofchar(c_ptr_ptr_getptr(strarrp, 1),100),':first'
  STOP 1
ENDIF
IF (strtofchar(c_ptr_ptr_getptr(strarrp, 2),100) /= 'segundo') THEN
  PRINT*,'Error in c_ptr_ptr_getptr:',strtofchar(c_ptr_ptr_getptr(strarrp, 2),100),':segundo'
  STOP 1
ENDIF
IF (strtofchar(c_ptr_ptr_getptr(strarrp, 3),100) /= 'troisieme') THEN
  PRINT*,'Error in c_ptr_ptr_getptr:',strtofchar(c_ptr_ptr_getptr(strarrp, 3),100),':troisieme'
  STOP 1
ENDIF
IF (strtofchar(c_ptr_ptr_getptr(strarrp, 4),100) /= '') THEN
  PRINT*,'Error in c_ptr_ptr_getptr: out of bound request should return empty string:',strtofchar(c_ptr_ptr_getptr(strarrp, 4),100)
  STOP 1
ENDIF

PRINT*,'The object contains the expected data'


! ==== How to use c_ptr_ptr for creating a char** object and pass it to C ====

! create a char** object from a Fortran array of characters
PRINT*,'Creating a c_ptr_ptr object from a Fortran array of characters'
strarrp = c_ptr_ptr_new((/'first    ','segundo  ','troisieme'/))
! strarrp is now ready to be passed to an interfaced C procedure
! expecting a char** variable

! we check it as before
! get the number of valid pointers in strarrp
PRINT*,'The object has ',c_ptr_ptr_getsize(strarrp),' elements'
IF (c_ptr_ptr_getsize(strarrp) /= 3) THEN
  PRINT*,'Error in c_ptr_ptr_getsize:',3,c_ptr_ptr_getsize(strarrp)
  STOP 1
ENDIF

! get the content of selected pointers as a Fortran CHARACTER variable
! of the right length, count starts from 1
IF (strtofchar(c_ptr_ptr_getptr(strarrp, 1),100) /= 'first') THEN
  PRINT*,'Error in c_ptr_ptr_getptr:',strtofchar(c_ptr_ptr_getptr(strarrp, 1),100),':first'
  STOP 1
ENDIF
IF (strtofchar(c_ptr_ptr_getptr(strarrp, 2),100) /= 'segundo') THEN
  PRINT*,'Error in c_ptr_ptr_getptr:',strtofchar(c_ptr_ptr_getptr(strarrp, 2),100),':segundo'
  STOP 1
ENDIF
IF (strtofchar(c_ptr_ptr_getptr(strarrp, 3),100) /= 'troisieme') THEN
  PRINT*,'Error in c_ptr_ptr_getptr:',strtofchar(c_ptr_ptr_getptr(strarrp, 3),100),':troisieme'
  STOP 1
ENDIF
IF (strtofchar(c_ptr_ptr_getptr(strarrp, 4),100) /= '') THEN
  PRINT*,'Error in c_ptr_ptr_getptr: out of bound request should return empty string:',strtofchar(c_ptr_ptr_getptr(strarrp, 4),100)
  STOP 1
ENDIF

PRINT*,'The object contains the expected data'


END PROGRAM fortranc_test
