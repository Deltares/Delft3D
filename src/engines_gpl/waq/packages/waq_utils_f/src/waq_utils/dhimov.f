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
      module m_dhimov

      implicit none

      contains


      SUBROUTINE DHIMOV ( IARRA1 , IARRA2 , NOTOT )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : december 1994 by Jan van Beek
!
!     FUNCTION            : moves NOTOT values from IARRA1 to IARRA2
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     IARRA1  INTEGER   NOTOT     INPUT   array to be copied
!     IARRA2  INTEGER   NOTOT     OUTPUT  array to copy to
!     NOTOT   INTEGER     1       INPUT   total number of entries
!
      INTEGER     NOTOT, I
      INTEGER     IARRA1(*) , IARRA2(*)
!
      DO  10 I = 1,NOTOT
   10 IARRA2(I) = IARRA1(I)
!
      RETURN
      END
      end module m_dhimov
