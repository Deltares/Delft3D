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

!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            :
!
!     V0.02  230395  Jos van Gils  Modify for hybrid coupling
!     V0.01  040894  Jos van Gils  First version
!
!     MODULE              : CHINPU
!
!     FUNCTION            : Reads input Charon
!                           Coupling Charon - Delwaq 4.0
!
!     SUBROUTINES CALLED  :
!
!     FILES               : -
!
!     COMMON BLOCKS       : -
!
      SUBROUTINE CHINP2 (RUNNAM, LUIC  , LUOC  )

      use m_srstop
      use m_charon
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     RUNNAM  C*12     1          I       Filename
!     LUIC    I        1          I       Lu input file
!     LUOC    I        1          I       LU output file
!
!     Declarations
!
      CHARACTER*20    C20
      CHARACTER*12    RUNNAM
      INTEGER         LUIC  , LUOC  , I1 , I2 , I , i3(1)
      REAL            R1(1,12)

!     Initialization of CHARON

      INTITL = 846342

!     Open input files and set LU's for Charon

      NIT    = LUIC
      NOT    = LUOC

      ILEN = LEN(RUNNAM)
!
!     Find last blank
!
      DO INDX = ILEN-4 , 1 , -1
         IF ( RUNNAM(INDX:INDX) .NE. ' ' ) GOTO 5
      ENDDO
!
!     empty string, error
!
      WRITE(*,*) 'ERROR in CHEM coupling, empty CHEM input'
      CALL SRSTOP(1)
    5 CONTINUE
      INDX = INDX + 1
      WRITE (RUNNAM(INDX:),'(''.inp'')')
      OPEN (NIT,FILE=RUNNAM)
      WRITE (RUNNAM(INDX:),'(''.out'')')
      OPEN (NOT,FILE=RUNNAM)

!     Read input file (N.B. Fixed order of input blocks!!!)

      CALL STARTCH
      READ (NIT,*)
      CALL ROWS (0)
      READ (NIT,*)
      CALL MATRIX (0,I1,I1,I2,I2,I1,1,I3,I3,I3,R1)
      READ (NIT,*)
!     (this variable switches on dimensionless input for cjor)
      KA(2) = 'NEW   '
      CALL CJCOR
      ITMAX = 200
      READ (NIT,'(A20)') C20
      IF (C20(1:3).EQ.'MES') THEN
          PF = 0
          WRITE (NOT,*) 'MESSAGES'
      ELSEIF (C20(1:3).EQ.'ALL') THEN
          PF = 1
          WRITE (NOT,*) 'ALL MESSAGES'
      ELSE
          PF = -1
          WRITE (NOT,*) 'NO MESSAGES'
      ENDIF

!     New part to read molar mass components

      read (nit,*)
      read (nit,'(12x,f12.0)') (commas(i),i=1,m)
      read (nit,*)
      write (not,'(''COMPONENT MASSES''/(a6,6x,f12.4))')
     j      (nr(i,1),commas(i),i=1,m)

      WRITE (NOT,*)'STOP READING INPUT FILE COUPLING DELWAQ-CHARON'

      RETURN
      END
