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

      MODULE DHRALLOC
!
      INTERFACE
      SUBROUTINE DHRALLOC_INT ( PINT  , NEW_LENGTH, OLD_LENGTH )
      INTEGER     , POINTER :: PINT(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
      END SUBROUTINE
      SUBROUTINE DHRALLOC_CH20 ( PCH20 , NEW_LENGTH, OLD_LENGTH )
      CHARACTER*20, POINTER :: PCH20(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
      END SUBROUTINE

      END INTERFACE
!
      END MODULE

      SUBROUTINE DHRALLOC_INT ( PINT  , NEW_LENGTH, OLD_LENGTH )
!
!     Declaration of arguments
!
      INTEGER     , POINTER :: PINT(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
!
      LOGICAL       L_ASSOCIATED
      INTEGER     , POINTER :: P_HULP(:)
!
      MIN_LENGTH = MIN(OLD_LENGTH,NEW_LENGTH)
!
      L_ASSOCIATED = ASSOCIATED(PINT)
      IF (  L_ASSOCIATED ) THEN
         IF ( MIN_LENGTH .GT. 0 ) THEN
            ALLOCATE(P_HULP(NEW_LENGTH))
            P_HULP(1:MIN_LENGTH) = PINT(1:MIN_LENGTH)
            DEALLOCATE(PINT)
            PINT => P_HULP
         ELSE
            DEALLOCATE(PINT)
            ALLOCATE(PINT(NEW_LENGTH))
         ENDIF
      ELSE
         ALLOCATE(PINT(NEW_LENGTH))
      ENDIF
!
      END

      SUBROUTINE DHRALLOC_CH20( PCH20  , NEW_LENGTH, OLD_LENGTH )
!
!     Declaration of arguments
!
      CHARACTER*20, POINTER :: PCH20(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
!
      LOGICAL       L_ASSOCIATED
      CHARACTER*20, POINTER :: P_HULP(:)
!
      MIN_LENGTH = MIN(OLD_LENGTH,NEW_LENGTH)
!
      L_ASSOCIATED = ASSOCIATED(PCH20)
      IF (  L_ASSOCIATED ) THEN
         IF ( MIN_LENGTH .GT. 0 ) THEN
            ALLOCATE(P_HULP(NEW_LENGTH))
            P_HULP(1:MIN_LENGTH) = PCH20(1:MIN_LENGTH)
            DEALLOCATE(PCH20)
            PCH20 => P_HULP
         ELSE
            DEALLOCATE(PCH20)
            ALLOCATE(PCH20(NEW_LENGTH))
         ENDIF
      ELSE
         ALLOCATE(PCH20(NEW_LENGTH))
      ENDIF
!
      END

