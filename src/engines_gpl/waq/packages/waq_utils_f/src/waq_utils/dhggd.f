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
      module m_dhggd

      implicit none

      contains


      SUBROUTINE DHGGD( NONUMB, NUMBRS, IGGD  )
!
!     Determine largest common denominator
!
      INTEGER           NONUMB, IGGD, I, IN
      INTEGER           NUMBRS(NONUMB)
      INTEGER           MINNUM

      MINNUM = NUMBRS(1)
      DO I = 2 , NONUMB
         MINNUM = MIN(NUMBRS(I),MINNUM)
      ENDDO

      DO I = MINNUM , 1 , -1
         IGGD = I
         DO IN = 1 , NONUMB
            IF ( MOD(NUMBRS(IN),I) .NE. 0 ) GOTO 50
         ENDDO
         GOTO 100
   50    CONTINUE
      ENDDO
  100 CONTINUE

      RETURN
      END
      end module m_dhggd
