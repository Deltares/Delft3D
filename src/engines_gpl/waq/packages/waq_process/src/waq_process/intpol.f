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

      subroutine intpol ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Depth where wave is created or wind fetch from wind direction

!
!     Description of the module :
!
!        General water quality module for DELWAQ:
!        BLOCK INTERPOLATION
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! Y       R*4 8 I     dependent value pairs
! X       R*4 8 I     independent value pairs
! VALUE   R*4 1 I     independent value
! RESULT  R*4 1 I     resulting dependent value
!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER   MAXPAR,IP, NUMPAR
      PARAMETER (MAXPAR=8)
      DIMENSION X(MAXPAR),Y(MAXPAR),IP(2*MAXPAR+2)

      DO 10 I=1,2*MAXPAR+2
        IP(I) = IPOINT(I)
   10 CONTINUE
!
      DO 9000 ISEG = 1 , NOSEG

      IF (BTEST(IKNMRK(ISEG),0)) THEN

!     fill and count the number of classes

      VALUE = PMSA(IP(1))
      NUMPAR = 1
      DO I=1,MAXPAR
         Y(I) = PMSA(IP(2*I))
         X(I) = PMSA(IP(2*I+1))
         IF (X(I).LT. 0.0) EXIT
         NUMPAR = I
      ENDDO

!*******************************************************************************
!**** RESULT equals the Y corresponding with the interval from the previous
!****        to the current X (assuming the first interval to be 0 - X(1)
!***********************************************************************

      I = 0
   30 I = I + 1
      IF ((VALUE.LT.X(I)).OR.(I.EQ.NUMPAR)) THEN
         RESULT = Y(I)
      ELSE
         GOTO 30
      ENDIF


      PMSA(IP(2*MAXPAR+2)) = RESULT

      ENDIF
!
      DO 40 I=1,2*MAXPAR+2
        IP(I) = IP(I) + INCREM ( I  )
   40 CONTINUE
!
 9000 CONTINUE
!
      RETURN
!
      END
