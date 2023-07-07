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
      module m_dhagkm
      use m_evaluate_waq_attribute


      implicit none

      contains


      SUBROUTINE DHAGKM ( NOSEG , NODIM2, NOGRID, IKNMRK, GRDNOS,
     +                    GRDSEG)
!
!     Deltares
!
!     Created             : Oct. 1998 by Jan van Beek
!
!     Function            : Aggregates kenmerk array
!
!     Subroutines called  : evaluate_waq_attribute, evaluate kenmerk
!
!     Arguments           :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NOSEG   INTEGER  1          INPUT   Number of segments
!     NODIM2  INTEGER  1          INPUT   second dimension kenmerk array
!     NOGRID  INTEGER  1          INPUT   number of grids
!     IKNMRK  INTEGER  *          IN/OUT  kenmerk array
!     GRDNOS  INTEGER  *          INPUT   number of grid cells per grid
!     GRDSEG  INTEGER  *          INPUT   segment pointers
!
!     Declaration of arguments
!
      INTEGER        NOSEG , NODIM2, NOGRID
      INTEGER        GRDNOS(NOGRID)
      INTEGER        GRDSEG(NOSEG,NOGRID)
      INTEGER        IKNMRK(NOSEG,NODIM2,NOGRID)
!
!     Local declaration
!
!     IGRID   INTEGER  1          LOCAL   Grid index
!     ISEG    INTEGER  1          LOCAL   Segment index base grid
!     ISEG2   INTEGER  1          LOCAL   Segment index coarser grid
!     K1_G1   INTEGER  1          LOCAL   Kenmerk 1 base grid
!     K1_G2   INTEGER  1          LOCAL   Kenmerk 1 coarser grid
!     K2_G1   INTEGER  1          LOCAL   Kenmerk 2 base grid
!     K2_G2   INTEGER  1          LOCAL   Kenmerk 2 coarser grid
!
      INTEGER        IGRID , ISEG  , ISEG2 , K1_G1 , K1_G2 ,
     +               K2_G1 , K2_G2
!
!     Set kenmerk array for all coarser grids
!
      DO IGRID = 2 , NOGRID
!
!        Set all first kenmerk inactive, all second kenmerk middle ( 20 )
!
         DO ISEG2 = 1 , GRDNOS(IGRID)
            IKNMRK(ISEG2,1,IGRID) = 20
         ENDDO
!
         DO ISEG = 1 , NOSEG
            ISEG2 = GRDSEG(ISEG,IGRID)
!
!           Kenmerk 1 , 0 = inactive , 1 = active , 2 = GEM bottom
!
            CALL evaluate_waq_attribute(1,IKNMRK(ISEG,1,1)     ,K1_G1)
            CALL evaluate_waq_attribute(1,IKNMRK(ISEG2,1,IGRID),K1_G2)
            IF ( K1_G1 .GT. 0 ) THEN
               K1_G2 = K1_G1
            ENDIF
!
!           Kenmerk 2 , 0 = depth integrated
!                       1 = surface
!                       2 = middle segment
!                       3 = bottom
!
            CALL evaluate_waq_attribute(2,IKNMRK(ISEG,1,1)     ,K2_G1)
            CALL evaluate_waq_attribute(2,IKNMRK(ISEG2,1,IGRID),K2_G2)
            IF ( K2_G1 .EQ. 0 ) THEN
               K2_G2 = 0
            ELSEIF ( K2_G1 .EQ. 1 ) THEN
               IF ( K2_G2 .EQ. 2 ) THEN
                  K2_G2 = 1
               ELSEIF ( K2_G2 .EQ. 3 ) THEN
                  K2_G2 = 0
               ENDIF
            ELSEIF ( K2_G1 .EQ. 3 ) THEN
               IF ( K2_G2 .EQ. 2 ) THEN
                  K2_G2 = 3
               ELSEIF ( K2_G2 .EQ. 1 ) THEN
                  K2_G2 = 0
               ENDIF
            ENDIF
!
            IKNMRK(ISEG2,1,IGRID) = K1_G2 + 10*K2_G2
!
         ENDDO
      ENDDO
!
      RETURN
      END
      end module m_dhagkm
