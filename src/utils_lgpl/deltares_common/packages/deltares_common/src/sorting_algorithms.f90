   module sorting_algorithms
   !----- LGPL --------------------------------------------------------------------
   !
   !  Copyright (C)  Stichting Deltares, 2011-2023.
   !
   !  This library is free software; you can redistribute it and/or
   !  modify it under the terms of the GNU Lesser General Public
   !  License as published by the Free Software Foundation version 2.1.
   !
   !  This library is distributed in the hope that it will be useful,
   !  but WITHOUT ANY WARRANTY; without even the implied warranty of
   !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   !  Lesser General Public License for more details.
   !
   !  You should have received a copy of the GNU Lesser General Public
   !  License along with this library; if not, see <http://www.gnu.org/licenses/>.
   !
   !  contact: delft3d.support@deltares.nl
   !  Stichting Deltares
   !  P.O. Box 177
   !  2600 MH Delft, The Netherlands
   !
   !  All indications and logos of, and references to, "Delft3D" and "Deltares"
   !  are registered trademarks of Stichting Deltares, and remain the property of
   !  Stichting Deltares. All rights reserved.
   !
   !-------------------------------------------------------------------------------
   !  
   !  
   !!--description-----------------------------------------------------------------
   !
   ! This module includes sorting algorithms
   !
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------

   !> \page deltares_common
   !! \section sorting Sorting algorithms
   !! The module \em sorting_algorithms provides several basic routines for sorting
   !! arrays, both integer and real (double precision).
   !!
   !! The relevant routine is:
   !! \ref sort
   !!
   contains

   SUBROUTINE INDEXXI(N,ARRIN,INDX)
   implicit none
   integer :: i
   integer :: indxt
   integer :: ir
   integer :: j
   integer :: l
   integer :: q
   integer :: N
   integer :: ARRIN(N)
   integer :: INDX(N)
   DO J=1,N
     INDX(J)=J
   ENDDO
   IF (N == 1) RETURN
   L=N/2+1
   IR=N
10 CONTINUE
     IF(L.GT.1)THEN
       L=L-1
       INDXT=INDX(L)
       Q=ARRIN(INDXT)
     ELSE
       INDXT=INDX(IR)
       Q=ARRIN(INDXT)
       INDX(IR)=INDX(1)
       IR=IR-1
       IF(IR.EQ.1)THEN
         INDX(1)=INDXT
         RETURN
       ENDIF
     ENDIF
     I=L
     J=L+L
20   IF(J.LE.IR)THEN
       IF(J.LT.IR)THEN
         IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
       ENDIF
       IF(Q.LT.ARRIN(INDX(J)))THEN
         INDX(I)=INDX(J)
         I=J
         J=J+J
       ELSE
         J=IR+1
       ENDIF
     GO TO 20
     ENDIF
     INDX(I)=INDXT
   GO TO 10
   END SUBROUTINE INDEXXi

   SUBROUTINE INDEXX(N,ARRIN,INDX)
   implicit none
   integer :: i
   integer :: indxt
   integer :: ir
   integer :: j
   integer :: l
   double precision :: q
   integer :: N
   double precision :: ARRIN(N)
   integer :: INDX(N)
   DO J=1,N
     INDX(J)=J
   ENDDO
   IF (N == 1) RETURN
   L=N/2+1
   IR=N
10 CONTINUE
     IF(L.GT.1)THEN
       L=L-1
       INDXT=INDX(L)
       Q=ARRIN(INDXT)
     ELSE
       INDXT=INDX(IR)
       Q=ARRIN(INDXT)
       INDX(IR)=INDX(1)
       IR=IR-1
       IF(IR.EQ.1)THEN
         INDX(1)=INDXT
         RETURN
       ENDIF
     ENDIF
     I=L
     J=L+L
20   IF(J.LE.IR)THEN
       IF(J.LT.IR)THEN
         IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
       ENDIF
       IF(Q.LT.ARRIN(INDX(J)))THEN
         INDX(I)=INDX(J)
         I=J
         J=J+J
       ELSE
         J=IR+1
       ENDIF
     GO TO 20
     ENDIF
     INDX(I)=INDXT
   GO TO 10
   END SUBROUTINE INDEXX


   !> \anchor sort
   !! Sort a real (double precision) array in ascending order
   !!
   subroutine sort(n,ra,wksp,iwksp)
   ! Sorts an array, routine from Numerical Recipes
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
   !
   implicit none
   !
   ! Global variables
   !
   integer                              :: n          !< Number of elements in the array
   integer, dimension(n)                :: iwksp      !< Integer workspace array
   real*8   , dimension(n)              :: ra         !< Array to be sorted
   real*8   , dimension(n), intent(out) :: wksp       !< Real workspace array
   !
   ! Local variables
   !
   integer :: j
   !
   !! executable statements -------------------------------------------------------
   !
   call indexx(n,ra,iwksp)
   do j = 1, n
      wksp(j) = ra(iwksp(j))
   enddo
   end subroutine sort

   end module sorting_algorithms
