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
      module m_rdwrk3

      implicit none

      contains


      SUBROUTINE RDWRK3 ( LUN   , LCHAR ,ITOTA , ITOTI , ITOTC  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : march 93 by Jan van Beek
!
!     FUNCTION            : Reads DELWAQ boot file
!
!     LOGICAL UNITNUMBERS : LUN(1)   - DELWAQ boot file
!
!     SUBROUTINES CALLED  : open_waq_files, opens files
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUN     INTEGER    *         OUT     Logical unit numbers
!     LCHAR   CHAR*(*)   *         OUT     File names
!     ITOTA   INTEGER    1         OUT     Dimension real array
!     ITOTI   INTEGER    1         OUT     Dimension integer array
!     ITOTC   INTEGER    1         OUT     Dimension character array
!
!     declarations
      use m_open_waq_files
      use m_dhgnam
      use m_sysn          ! System characteristics
      use m_sysi          ! Timer characteristics
!
      INTEGER       ITOTA , ITOTI , ITOTC
      INTEGER       LUN(*)
      CHARACTER*(*) LCHAR(*)
      integer :: INDX, IERR, K, LCHMAX
      
!     input structure for boot-file
!
      INTEGER             LUNIN
!
!         boot the system
!
      LCHMAX = LEN(LCHAR(1))
      CALL DHGNAM(LCHAR(1),' ')
      INDX = INDEX ( LCHAR(1) , ' ' )
      IF ( INDX .EQ. 0 ) INDX = LCHMAX + 1
      LCHAR(1) = LCHAR(1)(1:INDX-1)//'-delwaq03.wrk'
      LUNIN    = 14
      CALL open_waq_files ( LUNIN , LCHAR(1), 1     , 2     , IERR  )
!
      READ  ( LUNIN )   IN
      READ  ( LUNIN )   II
      READ  ( LUNIN )  ITOTA  , ITOTI  , ITOTC
      READ  ( LUNIN ) ( LUN(K) , K = 1,NOLUN )
      READ  ( LUNIN ) ( LCHAR(K) , K=1 , NOLUN )
!
      CLOSE ( LUNIN )
!
      RETURN
      END
      end module m_rdwrk3
