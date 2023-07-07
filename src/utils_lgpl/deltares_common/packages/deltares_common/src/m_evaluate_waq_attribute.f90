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
      module m_evaluate_waq_attribute

      implicit none

      contains


      SUBROUTINE evaluate_waq_attribute ( IKNMRK , KENMRK , KNMRKI )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     FUNCTION            : utility that evaluates an attribute (sometimes referred to as kenmerk)
!                           from the "feature" integer. Used for D-Waq
!                           For example: cell is at bottom, surface or in the middle. Cell is active or not
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     IKNMRK  INTEGER     1       INPUT   Index of feature
!     KENMRK  INTEGER     1       INPUT   feature
!     KNMRKI  INTEGER     1       OUTPUT  evaluated feature
!
      INTEGER IKNMRK, KENMRK, KNMRKI
!
!     Local
!
      INTEGER DHIMIS
!
      IF ( IKNMRK .EQ. 1 ) THEN
         KNMRKI = MOD(KENMRK,10)
      ELSEIF ( IKNMRK .EQ. 2 ) THEN
         KNMRKI = KENMRK / 10
         KNMRKI = MOD(KNMRKI,10)
      ELSEIF ( IKNMRK .EQ. 3 ) THEN
         KNMRKI = KENMRK / 100
         KNMRKI = MOD(KNMRKI,10)
      ELSEIF ( IKNMRK .LE. 0 .OR. IKNMRK .GT. 9      ) THEN
         DHIMIS = -999.
         KNMRKI = DHIMIS
      ELSE
         KNMRKI = KENMRK / 10**(IKNMRK-1)
         KNMRKI = MOD(KNMRKI,10)
      ENDIF
!
      RETURN
      END
      end module m_evaluate_waq_attribute
