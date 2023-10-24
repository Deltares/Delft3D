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
      module m_dlwq67

      implicit none

      contains


      SUBROUTINE DLWQ67 ( AMAT   , NOSEG  , JTRACK )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: june 1988 by L.Postma
!
!     FUNCTION            : updates the diagonal if zero
!
!     LOGICAL UNITNUMBERS : none
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND       LENGTH       FUNCT.  DESCRIPTION
!     ----    -----      ------       ------- -----------
!     AMAT    REAL (JTRACK*2+1)*NOSEG IN/OUT  matrix to invert
!     NOSEG   INTEGER       1         INPUT   number of segments
!     JTRACK  INTEGER       1         INPUT   number of codiagonals
!
      use timers

      real    AMAT(*)
      integer noseg, jtrack
      integer ISEG, ISTEP, ISET
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq67", ithandl )
!
!         set the diagonal
!
      ISTEP = JTRACK*2 + 1
      ISET  = JTRACK + 1
      DO 10 ISEG = 1 , NOSEG
      IF ( ABS(AMAT(ISET)) .LT. 1.0E-35 ) AMAT(ISET) = 1.0
   10 ISET = ISET+ISTEP
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dlwq67
