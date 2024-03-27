!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
      module m_dlwqib
      use m_waq_precision


      implicit none

      contains


      SUBROUTINE DLWQIB ( LUN    , LUNUT  , A      , J      , MODE   , & 
                                           IISP   , IRSP   , IERR   )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april 1996 by L. Postma
!
!     FUNCTION            : Initialises the complete boundary subsystem
!
!     LOGICAL UNITNUMBERS : LUN   - binary boundary system file
!                           LUNUT - monitoring file
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUN     INTEGER    1        INPUT   unit number input file
!     A       REAL       ?        IN/OUT  Real      boundary workspace
!     J       INTEGER    ?        IN/OUT  Integer   boundary workspace
!     MODE    INTEGER    1        INPUT   File number involved
!     IISP    INTEGER    1        IN/OUT  Integer array space pointer
!     IRSP    INTEGER    1        IN/OUT  Real array space pointer
!     IERR    INTEGER    1        IN/OUT  error count
!
!     Declaration of arguments
!
      use timers

      real(kind=real_wp) ::A(*)
      integer(kind=int_wp) ::J(*)
      integer(kind=int_wp) ::IISP, IRSP, IERR, MODE, LUN, LUNUT

      integer(kind=int_wp) ::i, ia, ij, k, iopt
      integer(kind=int_wp) ::noitm, nosys, npnt, ndim, ntal, nobrk
      integer(kind=int_wp) ::ithandl = 0
      if ( timon ) call timstrt ( "dlwqib", ithandl )
!
!         initialise the system
!
      IA = 1
      IJ = 1
!
!       Read the system dimensions
!
      READ ( LUN , END=40 , ERR=110 )  J(IJ), J(IJ+1)
      NOITM = J(IJ)
      NOSYS = J(IJ+1)
      IJ = 3
!
!       Read the pointers for this block of data
!
   10 READ ( LUN , END=40 , ERR=110 )  J(IJ), & 
                     NPNT, ( J(IJ+K) , K=2,NPNT+1 )  , & 
                     NDIM, ( J(IJ+NPNT+2+K) , K = 1, NDIM+2 )
      J(IJ     +1) = NPNT
      J(IJ+NPNT+2) = NDIM
      IJ = IJ + NPNT + NDIM + 5
!        default for all items
      IF ( NPNT == 0 ) NPNT = 1
!
!       3 and 4 are harmonics
!
      IOPT = J(IJ-2)
      NTAL = 0
      IF ( IOPT == 3 .OR. IOPT == 4 ) NTAL = 1
!
!       Nr of breakpoints or harmonics
!
      READ ( LUN , END=100 , ERR=110 ) NOBRK
      J(IJ) = NOBRK
      IJ = IJ + 1
!
      DO I=1,NOBRK
         READ ( LUN , END=100 , ERR=110 ) J(IJ) , & 
                    ( A(IA+K) , K=0,NPNT*NDIM-1+NTAL )
         IJ = IJ + 1
         IA = IA + NPNT*NDIM + NTAL
      end do
!
!       Return until finished
!
      GOTO 10
!
!       Update linear pointers in array
!
   40 IISP = IISP + IJ-1
      IRSP = IRSP + IA-1
      goto 9999  !   RETURN
!
  100 WRITE ( LUNUT , '(A,I3)' ) ' END-OF-FILE mode:',MODE
      IERR = IERR + 1
      goto 9999  !   RETURN
  110 WRITE ( LUNUT , '(A,I3)' ) ' ERROR-ON-FILE mode:',MODE
      IERR = IERR + 1
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
!
      END

      end module m_dlwqib
