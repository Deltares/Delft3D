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
      module m_monsys

      implicit none

      contains


      SUBROUTINE MONSYS(STRING,ILEVEL)
!
!     Arguments
!
      INTEGER       ILEVEL
      CHARACTER*(*) STRING
!
      INTEGER         LUNMON,MONLEV
      COMMON / CMON / LUNMON,MONLEV
      SAVE   / CMON /
!
!
      IF ( ILEVEL .LE. MONLEV ) THEN
         WRITE( LUNMON , '(A)' ) STRING
      ENDIF
!
      RETURN
      END
      SUBROUTINE SETMMO (ILEVEL)
!
!     Arguments
!
      INTEGER       ILEVEL
!
!     Settings
!
      INTEGER         LUNMON,MONLEV
      COMMON / CMON / LUNMON,MONLEV
      SAVE   / CMON /
!
!     Set monitoring level
!
      MONLEV = ILEVEL
!
      RETURN
      END
      SUBROUTINE GETMMO (ILEVEL)
!
!     Arguments
!
      INTEGER    ILEVEL
!
!     Settings
!
      INTEGER         LUNMON,MONLEV
      COMMON / CMON / LUNMON,MONLEV
      SAVE   / CMON /
!
      ILEVEL = MONLEV
!
      RETURN
      END
      SUBROUTINE SETMLU (ILUNMO)
!
!     Arguments
!
      INTEGER       ILUNMO
!
!     Settings
!
      INTEGER         LUNMON,MONLEV
      COMMON / CMON / LUNMON,MONLEV
      SAVE   / CMON /
!
!     Set monitoring level
!
      LUNMON = ILUNMO
!
      RETURN
      END
      SUBROUTINE GETMLU (ILUNMO)
!
!     Arguments
!
      INTEGER    ILUNMO
!
!     Settings
!
      INTEGER         LUNMON,MONLEV
      COMMON / CMON / LUNMON,MONLEV
      SAVE   / CMON /
!
      ILUNMO = LUNMON
!
      RETURN
      END
      end module m_monsys
      BLOCK DATA BDMON
!
      INTEGER         LUNMON,MONLEV
      COMMON / CMON / LUNMON,MONLEV
      SAVE   / CMON /
!
      DATA   LUNMON /   0 /
      DATA   MONLEV /   5 /
!
      END
