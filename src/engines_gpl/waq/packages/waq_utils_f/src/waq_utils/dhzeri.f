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
      module m_dhzeri

      implicit none

      contains


      SUBROUTINE DHZERI ( IARRAY , NOTOT  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: june  1988 by L.Postma
!
!     FUNCTION            : utility that zeros an array
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     IARRAY  INTEGER  NOTOT      OUTPUT  array to be zeroed
!     NOTOT   INTEGER     1       INPUT   total number of entries
!
      integer :: NOTOT
      integer :: I

      integer  IARRAY(*)
!
      DO  10 I  = 1,NOTOT
   10 IARRAY(I) = 0
!
      RETURN
      END
      end module m_dhzeri
