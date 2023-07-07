!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_move

      implicit none

      contains


      SUBROUTINE MOVE ( ARRAY1 , ARRAY2 , NOTOT )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: june  1988 by L.Postma
!
!     FUNCTION            : moves NVAL values from ARRAY1 to ARRAY2
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ARRAY1  REAL      NOTOT     INPUT   array to be copied
!     ARRAY2  REAL      NOTOT     OUTPUT  array to copy to
!     NOTOT   INTEGER     1       INPUT   total number of entries
!
      real    :: ARRAY1(*) , ARRAY2(*)
      integer :: I , NOTOT
!
      DO  10 I = 1,NOTOT
   10 ARRAY2(I) = ARRAY1(I)
!
      RETURN
      END
      subroutine dmove ( array1 , array2 , notot )

      real(8) ::  array1(*)
      real(4) ::  array2(*)
      integer :: I , NOTOT

      do i = 1,notot
         array2(i) = array1(i)
      enddo

      return
      end
      subroutine smove ( array1 , array2 , notot )

      real(4) ::  array1(*)
      real(8) ::  array2(*)
      integer :: I , NOTOT

      do i = 1,notot
         array2(i) = array1(i)
      enddo

      return
      end
      end module m_move
