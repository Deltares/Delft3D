#ifndef F90TW_DEFS_GTEST_H
#define F90TW_DEFS_GTEST_H

#include "f90tw_defs.h"

/* f90 interface:  gtest assertions one argument [+ message] */
#define F90_GTEST_LEVEL_CHECK1T1A(LEVEL,WHAT,TYPE,CTYPE,NAME1,DUMMY) \
     subroutine CONCAT4(c,LEVEL,WHAT,CTYPE,_) (NAME1) \
                 bind(C,name=TOSTR(CONCAT4(c,LEVEL,WHAT,CTYPE,_))) ; \
         import CTYPE ; \
         TYPE(KIND=CTYPE), intent(in) :: NAME1;  \
     end subroutine CONCAT4(c,LEVEL,WHAT,CTYPE,_)

#define F90_GTEST_LEVEL_CHECK1T1AM(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT5(c,LEVEL,WHAT,CTYPE,M,_) (NAME1, message) \
                 bind(C,name=TOSTR(CONCAT5(c,LEVEL,WHAT,CTYPE,M,_))) ; \
         import CTYPE, C_CHAR ; \
         TYPE(KIND=CTYPE), intent(in) :: NAME1;  \
         character(KIND=C_CHAR), intent(in) :: message ; \
     end subroutine CONCAT5(c,LEVEL,WHAT,CTYPE,M,_)

/* cast to logical(4) interface and implementation  */
#define F90_GTEST_LEVEL_CHECK1T1A_CAST(LEVEL,WHAT,TYPE,CTYPE,NAME1,DUMMY) \
     module procedure CONCAT4(F90,LEVEL,WHAT,TYPE,_)

#define F90_GTEST_LEVEL_CHECK1T1A_CAST_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,DUMMY) \
     subroutine CONCAT4(F90,LEVEL,WHAT,TYPE,_) (NAME1) ; \
         TYPE, intent(in) :: NAME1 ;  \
         TYPE(KIND=CTYPE) :: CONCAT(C,NAME1) ;  \
         CONCAT(C,NAME1) = TYPE( NAME1, KIND=CTYPE) ; \
         call CONCAT4(c,LEVEL,WHAT,CTYPE,_)( CONCAT(C,NAME1)) ; \
     end subroutine CONCAT4(F90,LEVEL,WHAT,TYPE,_)

#define F90_GTEST_LEVEL_CHECK1T1AM_CAST(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT5(F90,LEVEL,WHAT,TYPE,M,_)

#define F90_GTEST_LEVEL_CHECK1T1AM_CAST_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT5(F90,LEVEL,WHAT,TYPE,M,_) (NAME1, message) ; \
         TYPE, intent(in) :: NAME1;  \
         character(LEN=*), intent(in) :: message ; \
         TYPE(KIND=CTYPE) :: CONCAT(C,NAME1) ;  \
         CONCAT(C,NAME1) = TYPE( NAME1, KIND=CTYPE) ; \
         call CONCAT5(c,LEVEL,WHAT,CTYPE,M,_)( CONCAT(C,NAME1), TRIM(message) /&;&/ C_NULL_CHAR ) ; \
     end subroutine CONCAT5(F90,LEVEL,WHAT,TYPE,M,_)

/* dimension(:) argument interface and implementation
// SxS with cast to logical(4) interface and implementation  */
#define F90_GTEST_LEVEL_CHECK1T1ADIM1(LEVEL,WHAT,TYPE,CTYPE,NAME1,DUMMY) \
     module procedure CONCAT5(F90,LEVEL,WHAT,CTYPE,1D,_)

#define F90_GTEST_LEVEL_CHECK1T1ADIM1_CAST(LEVEL,WHAT,TYPE,CTYPE,NAME1,DUMMY) \
     module procedure CONCAT5(F90,LEVEL,WHAT,TYPE,1D,_)

#define F90_GTEST_LEVEL_CHECK1T1ADIM1_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,DUMMY) \
     subroutine CONCAT5(F90,LEVEL,WHAT,CTYPE,1D,_) (NAME1) ; \
         TYPE(KIND=CTYPE), dimension(:), intent(in) :: NAME1;  \
         integer :: i ; \
         do i = 1, size(NAME1) ; \
           call CONCAT4(c,LEVEL,WHAT,CTYPE,_)( NAME1(i)) ; \
         enddo ;  \
     end subroutine CONCAT5(F90,LEVEL,WHAT,CTYPE,1D,_)

#define F90_GTEST_LEVEL_CHECK1T1ADIM1_IMPLEMENT_CAST_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,DUMMY) \
     subroutine CONCAT5(F90,LEVEL,WHAT,TYPE,1D,_) (NAME1) ; \
         TYPE, dimension(:), intent(in) :: NAME1;  \
         TYPE(KIND=CTYPE) :: CONCAT(C,NAME1) ; \
         integer :: i ; \
         do i = 1, size(NAME1) ; \
           CONCAT(C,NAME1) = TYPE( NAME1(i), KIND=CTYPE) ; \
           call CONCAT4(c,LEVEL,WHAT,CTYPE,_)( CONCAT(C,NAME1)) ; \
         enddo ;  \
     end subroutine CONCAT5(F90,LEVEL,WHAT,TYPE,1D,_)

#define F90_GTEST_LEVEL_CHECK1T1AMDIM1(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT6(F90,LEVEL,WHAT,CTYPE,M,1D,_)

#define F90_GTEST_LEVEL_CHECK1T1AMDIM1_CAST(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT6(F90,LEVEL,WHAT,TYPE,M,1D,_)

#define F90_GTEST_LEVEL_CHECK1T1AMDIM1_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT6(F90,LEVEL,WHAT,CTYPE,M,1D,_) (NAME1, message) ; \
         TYPE(KIND=CTYPE), dimension(:), intent(in) :: NAME1;  \
         character(LEN=*), intent(in) :: message ; \
         character(LEN=100) :: str ; \
         integer :: i ; \
         do i = 1, size(NAME1) ; \
           write(str,'(A, " : (", I0, ") INDX", A)') TRIM(message), i, C_NULL_CHAR ; \
           call CONCAT5(c,LEVEL,WHAT,CTYPE,M,_) ( NAME1(i), TRIM(str) ) ; \
         enddo ; \
     end subroutine CONCAT6(F90,LEVEL,WHAT,CTYPE,M,1D,_)

#define F90_GTEST_LEVEL_CHECK1T1AMDIM1_IMPLEMENT_CAST_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT6(F90,LEVEL,WHAT,TYPE,M,1D,_) (NAME1, message) ; \
         TYPE, dimension(:), intent(in) :: NAME1;  \
         character(LEN=*), intent(in) :: message ; \
         TYPE(KIND=CTYPE) :: CONCAT(C,NAME1) ; \
         character(LEN=100) :: str ; \
         integer :: i ; \
         do i = 1, size(NAME1) ; \
           CONCAT(C,NAME1) = TYPE( NAME1(i), KIND=CTYPE) ; \
           write(str,'(A, " : (", I0, ") INDX", A)') TRIM(message), i, C_NULL_CHAR ; \
           call CONCAT5(c,LEVEL,WHAT,CTYPE,M,_) ( CONCAT(C,NAME1), TRIM(str) ) ; \
         enddo ; \
     end subroutine CONCAT6(F90,LEVEL,WHAT,TYPE,M,1D,_)

/* dimension(:,:) argument interface and implementation
// SxS with cast to logical(4) interface and implementation  */
#define F90_GTEST_LEVEL_CHECK1T1ADIM2(LEVEL,WHAT,TYPE,CTYPE,NAME1,DUMMY) \
     module procedure CONCAT5(F90,LEVEL,WHAT,CTYPE,2D,_)

#define F90_GTEST_LEVEL_CHECK1T1ADIM2_CAST(LEVEL,WHAT,TYPE,CTYPE,NAME1,DUMMY) \
     module procedure CONCAT5(F90,LEVEL,WHAT,TYPE,2D,_)

#define F90_GTEST_LEVEL_CHECK1T1ADIM2_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,DUMMY) \
     subroutine CONCAT5(F90,LEVEL,WHAT,CTYPE,2D,_) (NAME1) ; \
         TYPE(KIND=CTYPE), dimension(:,:), intent(in) :: NAME1;  \
         integer :: i, j, n(2) ; \
         n = shape(NAME1) ; \
         do i = 1, n(1) ; \
           do j = 1, n(2) ; \
             call CONCAT4(c,LEVEL,WHAT,CTYPE,_)( NAME1(i,j)) ; \
           enddo ;  \
         enddo ;  \
     end subroutine CONCAT5(F90,LEVEL,WHAT,CTYPE,2D,_)

#define F90_GTEST_LEVEL_CHECK1T1ADIM2_IMPLEMENT_CAST_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,DUMMY) \
     subroutine CONCAT5(F90,LEVEL,WHAT,TYPE,2D,_) (NAME1) ; \
         TYPE, dimension(:,:), intent(in) :: NAME1;  \
         TYPE(KIND=CTYPE) :: CONCAT(C,NAME1) ; \
         integer :: i, j, n(2) ; \
         n = shape(NAME1) ; \
         do i = 1, n(1) ; \
           do j = 1, n(2) ; \
             CONCAT(C,NAME1) = TYPE( NAME1(i,j), KIND=CTYPE) ; \
             call CONCAT4(c,LEVEL,WHAT,CTYPE,_)( CONCAT(C,NAME1)) ; \
           enddo ;  \
         enddo ;  \
     end subroutine CONCAT5(F90,LEVEL,WHAT,TYPE,2D,_)

#define F90_GTEST_LEVEL_CHECK1T1AMDIM2(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT6(F90,LEVEL,WHAT,CTYPE,M,2D,_)

#define F90_GTEST_LEVEL_CHECK1T1AMDIM2_CAST(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT6(F90,LEVEL,WHAT,TYPE,M,2D,_)

#define F90_GTEST_LEVEL_CHECK1T1AMDIM2_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT6(F90,LEVEL,WHAT,CTYPE,M,2D,_) (NAME1, message) ; \
         TYPE(KIND=CTYPE), dimension(:,:), intent(in) :: NAME1;  \
         character(LEN=*), intent(in) :: message ; \
         character(LEN=100) :: str ; \
         integer :: i, j, n(2) ; \
         n = shape(NAME1) ; \
         do i = 1, n(1) ; \
           do j = 1, n(2) ; \
             write(str,'(A, " : (", I0, ",", I0, ") INDX", A)') TRIM(message), i, j, C_NULL_CHAR ; \
             call CONCAT5(c,LEVEL,WHAT,CTYPE,M,_) ( NAME1(i,j), TRIM(str) ) ; \
           enddo ; \
         enddo ; \
     end subroutine CONCAT6(F90,LEVEL,WHAT,CTYPE,M,2D,_)

#define F90_GTEST_LEVEL_CHECK1T1AMDIM2_IMPLEMENT_CAST_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT6(F90,LEVEL,WHAT,TYPE,M,2D,_) (NAME1, message) ; \
         TYPE, dimension(:,:), intent(in) :: NAME1;  \
         character(LEN=*), intent(in) :: message ; \
         TYPE(KIND=CTYPE) :: CONCAT(C,NAME1) ; \
         character(LEN=100) :: str ; \
         integer :: i, j, n(2) ; \
         n = shape(NAME1) ; \
         do i = 1, n(1) ; \
           do j = 1, n(2) ; \
             CONCAT(C,NAME1) = TYPE( NAME1(i,j), KIND=CTYPE) ; \
             write(str,'(A, " : (", I0, ",", I0, ") INDX", A)') TRIM(message), i, j, C_NULL_CHAR ; \
             call CONCAT5(c,LEVEL,WHAT,CTYPE,M,_) ( CONCAT(C,NAME1), TRIM(str) ) ; \
           enddo ; \
         enddo ; \
     end subroutine CONCAT6(F90,LEVEL,WHAT,TYPE,M,2D,_)

/* f90 interface:  gtest assertions two arguments [+ message] */
#define F90_GTEST_LEVEL_CHECK1T2A(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT4(c,LEVEL,WHAT,CTYPE,_) (NAME1, NAME2) \
                 bind(C,name=TOSTR(CONCAT4(c,LEVEL,WHAT,CTYPE,_))) ; \
         import CTYPE ; \
         TYPE(KIND=CTYPE), intent(in) :: NAME1, NAME2;  \
     end subroutine CONCAT4(c,LEVEL,WHAT,CTYPE,_)

#define F90_GTEST_LEVEL_CHECK1T2AM(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT5(c,LEVEL,WHAT,CTYPE,M,_) (NAME1,NAME2,message) \
                 bind(C,name=TOSTR(CONCAT5(c,LEVEL,WHAT,CTYPE,M,_))) ; \
         import CTYPE, C_CHAR ; \
         TYPE(KIND=CTYPE), intent(in) :: NAME1, NAME2;  \
         character(KIND=C_CHAR), intent(in) :: message ; \
     end subroutine CONCAT5(c,LEVEL,WHAT,CTYPE,M,_)

/* cast to logical(4) interface and implementation */
#define F90_GTEST_LEVEL_CHECK1T2A_CAST(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT4(F90,LEVEL,WHAT,TYPE,_)

#define F90_GTEST_LEVEL_CHECK1T2A_CAST_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT4(F90,LEVEL,WHAT,TYPE,_) (NAME1, NAME2) ; \
         TYPE, intent(in) :: NAME1, NAME2;  \
         TYPE(KIND=CTYPE) :: CONCAT(C,NAME1), CONCAT(C,NAME2);  \
         CONCAT(C,NAME1) = TYPE( NAME1, KIND=CTYPE) ; \
         CONCAT(C,NAME2) = TYPE( NAME2, KIND=CTYPE) ; \
         call CONCAT4(c,LEVEL,WHAT,CTYPE,_)(CONCAT(C,NAME1),CONCAT(C,NAME2)) ; \
     end subroutine CONCAT4(F90,LEVEL,WHAT,TYPE,_)

#define F90_GTEST_LEVEL_CHECK1T2AM_CAST(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT5(F90,LEVEL,WHAT,TYPE,M,_)

#define F90_GTEST_LEVEL_CHECK1T2AM_CAST_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT5(F90,LEVEL,WHAT,TYPE,M,_) (NAME1, NAME2, message) ; \
         TYPE, intent(in) :: NAME1, NAME2;  \
         character(LEN=*), intent(in) :: message ; \
         TYPE(KIND=CTYPE) :: CONCAT(C,NAME1), CONCAT(C,NAME2);  \
         CONCAT(C,NAME1) = TYPE( NAME1, KIND=CTYPE) ; \
         CONCAT(C,NAME2) = TYPE( NAME2, KIND=CTYPE) ; \
         call CONCAT5(c,LEVEL,WHAT,CTYPE,M,_)(CONCAT(C,NAME1),CONCAT(C,NAME2), TRIM(message) /&;&/ C_NULL_CHAR ) ; \
     end subroutine CONCAT5(F90,LEVEL,WHAT,TYPE,M,_)

/* dimension(:) argument interface and implementation */
#define F90_GTEST_LEVEL_CHECK1T2ADIM1(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT5(F90,LEVEL,WHAT,CTYPE,1D,_)

#define F90_GTEST_LEVEL_CHECK1T2ADIM1_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT5(F90,LEVEL,WHAT,CTYPE,1D,_) (NAME1, NAME2) ; \
         TYPE(KIND=CTYPE), dimension(:), intent(in) :: NAME1, NAME2;  \
         integer :: i, n ; \
         n = size( NAME1) ; \
         if ( n .ne. size( NAME2) ) then ; \
           call CONCAT3(F90,LEVEL,TRUE,_)( .False., "arrays of different size, abording ...") ; \
           return ; \
         endif ; \
         do i = 1, n ; \
           call CONCAT4(c,LEVEL,WHAT,CTYPE,_)(NAME1(i),NAME2(i)) ; \
         enddo ; \
     end subroutine CONCAT5(F90,LEVEL,WHAT,CTYPE,1D,_)

#define F90_GTEST_LEVEL_CHECK1T2AMDIM1(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT6(F90,LEVEL,WHAT,CTYPE,M,1D,_)

#define F90_GTEST_LEVEL_CHECK1T2AMDIM1_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT6(F90,LEVEL,WHAT,CTYPE,M,1D,_) (NAME1, NAME2, message) ; \
         TYPE(KIND=CTYPE), dimension(:), intent(in) :: NAME1, NAME2;  \
         character(LEN=*), intent(in) :: message ; \
         character(LEN=100) :: str ; \
         integer :: i, n ; \
         n = size( NAME1) ; \
         if ( n .ne. size( NAME2) ) then ; \
           call CONCAT3(F90,LEVEL,TRUE,_)( .False., "arrays of different size, abording ...") ; \
           return ; \
         endif ; \
         do i = 1, n ; \
           write(str,'(A, " : (", I0, ") INDX", A)') TRIM(message), i, C_NULL_CHAR ; \
           call CONCAT5(c,LEVEL,WHAT,CTYPE,M,_)(NAME1(i),NAME2(i), TRIM(str) ) ; \
         enddo ; \
     end subroutine CONCAT6(F90,LEVEL,WHAT,CTYPE,M,1D,_)

/* dimension(:,:) argument interface and implementation */
#define F90_GTEST_LEVEL_CHECK1T2ADIM2(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT5(F90,LEVEL,WHAT,CTYPE,2D,_)

#define F90_GTEST_LEVEL_CHECK1T2ADIM2_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT5(F90,LEVEL,WHAT,CTYPE,2D,_) (NAME1, NAME2) ; \
         TYPE(KIND=CTYPE), dimension(:,:), intent(in) :: NAME1, NAME2;  \
         integer :: i, j, n(2) ; \
         n = shape( NAME1) ; \
         if ( ANY( n .ne. shape( NAME2) ) ) then ; \
           call CONCAT3(F90,LEVEL,TRUE,_)( .False., "arrays of different shape, abording ...") ; \
           return ; \
         endif ; \
         do i = 1, n(1) ; \
           do j = 1, n(2) ; \
             call CONCAT4(c,LEVEL,WHAT,CTYPE,_)(NAME1(i,j),NAME2(i,j)) ; \
           enddo ; \
         enddo ; \
     end subroutine CONCAT5(F90,LEVEL,WHAT,CTYPE,2D,_)

#define F90_GTEST_LEVEL_CHECK1T2AMDIM2(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT6(F90,LEVEL,WHAT,CTYPE,M,2D,_)

#define F90_GTEST_LEVEL_CHECK1T2AMDIM2_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT6(F90,LEVEL,WHAT,CTYPE,M,2D,_) (NAME1, NAME2, message) ; \
         TYPE(KIND=CTYPE), dimension(:,:), intent(in) :: NAME1, NAME2;  \
         character(LEN=*), intent(in) :: message ; \
         character(LEN=100) :: str ; \
         integer :: i, j, n(2) ; \
         n = size( NAME1) ; \
         if ( ANY( n .ne. shape( NAME2) ) ) then ; \
           call CONCAT3(F90,LEVEL,TRUE,_)( .False., "arrays of different shape, abording ...") ; \
           return ; \
         endif ; \
         do i = 1, n(1) ; \
           do j = 1, n(2) ; \
             write(str,'(A, " : (", I0, ",", I0, ") INDX", A)') TRIM(message), i, j, C_NULL_CHAR ; \
             call CONCAT5(c,LEVEL,WHAT,CTYPE,M,_)(NAME1(i,j),NAME2(i,j), TRIM(str) ) ; \
           enddo ; \
         enddo ; \
     end subroutine CONCAT6(F90,LEVEL,WHAT,CTYPE,M,2D,_)

/* f90 interface:  gtest assertions two string arguments [+ message] */
#define F90_GTEST_LEVEL_CHECK1T2STR(LEVEL,WHAT,NAME1,NAME2) \
     subroutine CONCAT3(c,LEVEL,WHAT,_) (NAME1, NAME2) \
                 bind(C,name=TOSTR(CONCAT3(c,LEVEL,WHAT,_))) ; \
         import C_CHAR ; \
         character(KIND=C_CHAR), intent(in) :: NAME1, NAME2;  \
     end subroutine CONCAT3(c,LEVEL,WHAT,_)

#define F90_GTEST_LEVEL_CHECK1T2STRM(LEVEL,WHAT,NAME1,NAME2) \
     subroutine CONCAT4(c,LEVEL,WHAT,M,_) (NAME1,NAME2,message) \
                 bind(C,name=TOSTR(CONCAT4(c,LEVEL,WHAT,M,_))) ; \
         import C_CHAR ; \
         character(KIND=C_CHAR), intent(in) ::  NAME1, NAME2, message ; \
     end subroutine CONCAT4(c,LEVEL,WHAT,M,_)

/* f90 interface:  gtest assertions three arguments [+ message] */
#define F90_GTEST_LEVEL_CHECK1T3A(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT4(c,LEVEL,WHAT,CTYPE,_) (NAME1,NAME2,abs_error) \
                 bind(C,name=TOSTR(CONCAT4(c,LEVEL,WHAT,CTYPE,_))) ; \
         import CTYPE ; \
         TYPE(KIND=CTYPE), intent(in) :: NAME1, NAME2, abs_error;  \
     end subroutine CONCAT4(c,LEVEL,WHAT,CTYPE,_)

#define F90_GTEST_LEVEL_CHECK1T3AM(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT5(c,LEVEL,WHAT,CTYPE,M,_) (NAME1,NAME2,abs_error,message) \
                 bind(C,name=TOSTR(CONCAT5(c,LEVEL,WHAT,CTYPE,M,_))) ; \
         import CTYPE, C_CHAR ; \
         TYPE(KIND=CTYPE), intent(in) :: NAME1, NAME2, abs_error;  \
         character(KIND=C_CHAR), intent(in) :: message ; \
     end subroutine CONCAT5(c,LEVEL,WHAT,CTYPE,M,_)

/* dimension(:) argument interface and implementation */
#define F90_GTEST_LEVEL_CHECK1T3ADIM1(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT5(F90,LEVEL,WHAT,CTYPE,1D,_)

#define F90_GTEST_LEVEL_CHECK1T3ADIM1_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT5(F90,LEVEL,WHAT,CTYPE,1D,_) (NAME1,NAME2,abs_error) ; \
         TYPE(KIND=CTYPE), dimension(:), intent(in) :: NAME1, NAME2 ;  \
         TYPE(KIND=CTYPE), intent(in) :: abs_error; \
         integer :: i, n ; \
         n = size(NAME1) ; \
         if ( n .ne. size( NAME2) ) then ; \
           call CONCAT3(F90,LEVEL,TRUE,_)( .False., "arrays of different size, abording ...") ; \
           return ; \
         endif ; \
         do i = 1, n ; \
           call CONCAT4(c,LEVEL,WHAT,CTYPE,_)(NAME1(i), NAME2(i), abs_error) ; \
         enddo ; \
     end subroutine CONCAT5(F90,LEVEL,WHAT,CTYPE,1D,_)

#define F90_GTEST_LEVEL_CHECK1T3AMDIM1(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT6(F90,LEVEL,WHAT,CTYPE,M,1D,_)

#define F90_GTEST_LEVEL_CHECK1T3AMDIM1_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT6(F90,LEVEL,WHAT,CTYPE,M,1D,_) (NAME1,NAME2,abs_error,message) ; \
         TYPE(KIND=CTYPE), dimension(:), intent(in) :: NAME1, NAME2 ;  \
         TYPE(KIND=CTYPE), intent(in) :: abs_error; \
         character(LEN=*), intent(in) :: message ; \
         character(LEN=100) :: str ; \
         integer :: i, n ; \
         n = size(NAME1) ; \
         if ( n .ne. size( NAME2) ) then ; \
           call CONCAT3(F90,LEVEL,TRUE,_)( .False., "arrays of different size, abording ...") ; \
           return ; \
         endif ; \
         do i = 1, n ; \
           write(str,'(A, " : (", I0, ") INDX", A)') TRIM(message), i, C_NULL_CHAR ; \
           call CONCAT5(c,LEVEL,WHAT,CTYPE,M,_)(NAME1(i), NAME2(i), abs_error, TRIM(str)) ; \
         enddo ; \
     end subroutine CONCAT6(F90,LEVEL,WHAT,CTYPE,M,1D,_)

/* dimension(:,:) argument interface and implementation */
#define F90_GTEST_LEVEL_CHECK1T3ADIM2(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT5(F90,LEVEL,WHAT,CTYPE,2D,_)

#define F90_GTEST_LEVEL_CHECK1T3ADIM2_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT5(F90,LEVEL,WHAT,CTYPE,2D,_) (NAME1,NAME2,abs_error) ; \
         TYPE(KIND=CTYPE), dimension(:,:), intent(in) :: NAME1, NAME2 ;  \
         TYPE(KIND=CTYPE), intent(in) :: abs_error; \
         integer :: i, j, n(2) ; \
         n = shape(NAME1) ; \
         if ( ANY( n .ne. shape( NAME2)) ) then ; \
           call CONCAT3(F90,LEVEL,TRUE,_)( .False., "arrays of different shape, abording ...") ; \
           return ; \
         endif ; \
         do i = 1, n(1) ; \
           do j = 1, n(2) ; \
             call CONCAT4(c,LEVEL,WHAT,CTYPE,_)(NAME1(i,j), NAME2(i,j), abs_error) ; \
           enddo ; \
         enddo ; \
     end subroutine CONCAT5(F90,LEVEL,WHAT,CTYPE,2D,_)

#define F90_GTEST_LEVEL_CHECK1T3AMDIM2(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     module procedure CONCAT6(F90,LEVEL,WHAT,CTYPE,M,2D,_)

#define F90_GTEST_LEVEL_CHECK1T3AMDIM2_IMPLEMENT(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     subroutine CONCAT6(F90,LEVEL,WHAT,CTYPE,M,2D,_) (NAME1,NAME2,abs_error,message) ; \
         TYPE(KIND=CTYPE), dimension(:,:), intent(in) :: NAME1, NAME2 ;  \
         TYPE(KIND=CTYPE), intent(in) :: abs_error; \
         character(LEN=*), intent(in) :: message ; \
         character(LEN=100) :: str ; \
         integer :: i, j, n(2) ; \
         n = shape(NAME1) ; \
         if ( ANY( n .ne. shape( NAME2)) ) then ; \
           call CONCAT3(F90,LEVEL,TRUE,_)( .False., "arrays of different shape, abording ...") ; \
           return ; \
         endif ; \
         do i = 1, n(1) ; \
           do j = 1, n(2) ; \
             write(str,'(A, " : (", I0, ",", I0, ") INDX", A)') TRIM(message), i, j, C_NULL_CHAR ; \
             call CONCAT5(c,LEVEL,WHAT,CTYPE,M,_)(NAME1(i,j), NAME2(i,j), abs_error, TRIM( str)) ; \
           enddo ; \
         enddo ; \
     end subroutine CONCAT6(F90,LEVEL,WHAT,CTYPE,M,2D,_)

/* c wrapper: gtest assertions one argument [+ message] */
#define C_GTEST_LEVEL_CHECK1T1A(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     void CONCAT4(c,LEVEL,WHAT,CTYPE,_) (TYPE* NAME1) { \
         CONCAT2(LEVEL,WHAT,_)(*NAME1); \
     }

#define C_GTEST_LEVEL_CHECK1T1AM(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     void CONCAT5(c,LEVEL,WHAT,CTYPE,M,_) (TYPE* NAME1, const char *message) { \
         CONCAT2(LEVEL,WHAT,_)(*NAME1) << message; \
     }

/* c wrapper: gtest assertions two arguments [+ message] */
#define C_GTEST_LEVEL_CHECK1T2A(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     void CONCAT4(c,LEVEL,WHAT,CTYPE,_) (TYPE* NAME1, TYPE* NAME2) { \
         CONCAT2(LEVEL,WHAT,_)(*NAME1, *NAME2); \
     }

#define C_GTEST_LEVEL_CHECK1T2AM(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     void CONCAT5(c,LEVEL,WHAT,CTYPE,M,_) (TYPE* NAME1, TYPE* NAME2, const char* message) { \
         CONCAT2(LEVEL,WHAT,_)(*NAME1, *NAME2) << message; \
     }

/* c wrapper: gtest assertions two string arguments [+ message] */
#define C_GTEST_LEVEL_CHECK1T2STR(LEVEL,WHAT,NAME1,NAME2) \
     void CONCAT3(c,LEVEL,WHAT,_) ( const char* NAME1,  const char* NAME2) { \
         CONCAT2(LEVEL,WHAT,_)(NAME1, NAME2); \
     }

#define C_GTEST_LEVEL_CHECK1T2STRM(LEVEL,WHAT,NAME1,NAME2) \
     void CONCAT4(c,LEVEL,WHAT,M,_) ( const char* NAME1,  const char* NAME2, const char* message) { \
         CONCAT2(LEVEL,WHAT,_)(NAME1, NAME2) << message; \
     }

/* c wrapper: gtest assertions three arguments [+ message] */
#define C_GTEST_LEVEL_CHECK1T3A(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     void CONCAT4(c,LEVEL,WHAT,CTYPE,_) (TYPE* NAME1, TYPE* NAME2, TYPE* abs_error) { \
         CONCAT2(LEVEL,WHAT,_)(*NAME1, *NAME2, *abs_error); \
     }

#define C_GTEST_LEVEL_CHECK1T3AM(LEVEL,WHAT,TYPE,CTYPE,NAME1,NAME2) \
     void CONCAT5(c,LEVEL,WHAT,CTYPE,M,_) (TYPE* NAME1, TYPE* NAME2, TYPE* abs_error, const char* message) { \
         CONCAT2(LEVEL,WHAT,_)(*NAME1, *NAME2, *abs_error) << message; \
     }

#endif // F90TW_DEFS_GTEST_H