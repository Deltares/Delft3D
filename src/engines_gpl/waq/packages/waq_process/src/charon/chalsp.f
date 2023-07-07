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
!     V0.01  040894  Jos van Gils  First version
!
!     MODULE              : CHALSP
!
!     FUNCTION            : Process aliasses for species names
!
!     SUBROUTINES CALLED  :
!
!     FILES               : -
!
!     COMMON BLOCKS       : -
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NIN     I        1          I       Lenght of array to be modified
!     NAME    C*(*)    NIN        I/O     Array to be modified
!     NC      I        1          I       Number of relevant characters
!     NALIAS  I        1          I       Number of aliases
!     ALIAS   C*(*)    2,NALIAS   I       Aliases

      SUBROUTINE CHALIA ( NIN , NAME , NC , NALIAS , ALIAS )
      use m_zoek

!
!     Declarations
!
      INTEGER         NIN   , NC    , NALIAS, J     , K     ,
     J                JOLD  , JNEW
      CHARACTER*(*)   NAME(NIN), ALIAS(2,NALIAS)
      DATA JOLD,JNEW /1,2/

!     Local declarations

      DO 200 J = 1,NIN
          DO 100 K = 1,NALIAS
              CALL ZOEK ( NAME(J)(1:NC)      , 1  ,
     +                    ALIAS(JOLD,K)(1:NC), NC ,
     +                    INDX                    )
              IF ( INDX .GT. 0 ) THEN
                  NAME(J)(1:NC) = ALIAS(JNEW,K)(1:NC)
                  GOTO 200
              ENDIF
  100     CONTINUE
  200 CONTINUE

      RETURN
      END
      SUBROUTINE CHALI2 ( NIN , NAME , NC , NALIAS , ALIAS )
      use m_zoek

!
!     Declarations
!
      INTEGER         NIN   , NC    , NALIAS, J     , K     ,
     J                JOLD  , JNEW
      CHARACTER*(*)   NAME(NIN), ALIAS(2,NALIAS)
      DATA JOLD,JNEW /2,1/

!     Local declarations

      DO 200 J = 1,NIN
          DO 100 K = 1,NALIAS
              CALL ZOEK ( NAME(J)(1:NC)      , 1  ,
     +                    ALIAS(JOLD,K)(1:NC), NC ,
     +                    INDX                    )
              IF ( INDX .GT. 0 ) THEN
                  NAME(J)(1:NC) = ALIAS(JNEW,K)(1:NC)
                  GOTO 200
              ENDIF
  100     CONTINUE
  200 CONTINUE

      RETURN
      END
