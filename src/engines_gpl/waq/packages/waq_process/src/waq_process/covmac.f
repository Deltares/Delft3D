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
      module m_covmac

      implicit none

      contains


      SUBROUTINE COVMAC     ( PMSA   , FL     , IPOINT , INCREM, NOSEG ,
     +                        NOFLUX , IEXPNT , IKNMRK , NOQ1  , NOQ2  ,
     +                        NOQ3   , NOQ4   )
!
!*******************************************************************************
!
      use m_evaluate_waq_attribute
      use m_write_error_message
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      REAL(4) PMSA(*)     !I/O Process Manager System Array, window of routine to process library
      REAL(4) FL(*)       ! O  Array of fluxes made by this process in mass/volume/time
      INTEGER IPOINT( 22) ! I  Array of pointers in PMSA to get and store the data
      INTEGER INCREM( 22) ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
      INTEGER NOSEG       ! I  Number of computational elements in the whole model schematisation
      INTEGER NOFLUX      ! I  Number of fluxes, increment in the FL array
      INTEGER IEXPNT(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      INTEGER IKNMRK(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      INTEGER NOQ1        ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      INTEGER NOQ2        ! I  Nr of exchanges in 2nd direction, NOQ1+NOQ2 gives hor. dir. reg. grid
      INTEGER NOQ3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      INTEGER NOQ4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      INTEGER IPNT( 22)   !    Local work array for the pointering
      INTEGER ISEG        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      REAL(4) nMacrophyt  ! I  number of macrophyte species                       (-)
      REAL(4) EM01        ! I  macrophyt emerged 01                               (gC/m2)
      REAL(4) MaxEM01     ! I  maximum biomass for macrophyt emerged 01           (gC/m2)
      REAL(4) EM02        ! I  macrophyt emerged 02                               (gC/m2)
      REAL(4) MaxEM02     ! I  maximum biomass for EM02                           (gC/m2)
      REAL(4) EM03        ! I  macrophyt emerged 03                               (gC/m2)
      REAL(4) MaxEM03     ! I  maximum biomass for EM03                           (gC/m2)
      REAL(4) EM04        ! I  macrophyt emerged 04                               (gC/m2)
      REAL(4) MaxEM04     ! I  maximum biomass for EM04                           (gC/m2)
      REAL(4) EM05        ! I  macrophyt emerged 05                               (gC/m2)
      REAL(4) MaxEM05     ! I  maximum biomass for EM05                           (gC/m2)
      REAL(4) RadIn       ! I  incoming radiation                                 (W/m2)
      REAL(4) fcover      ! O  fraction of water surface covered <0-1>            (-)
      REAL(4) CoverEM01   ! O  covergae with EM01                                 (-)
      REAL(4) CoverEM02   ! O  covergae with EM02                                 (-)
      REAL(4) CoverEM03   ! O  covergae with EM03                                 (-)
      REAL(4) CoverEM04   ! O  covergae with EM04                                 (-)
      REAL(4) CoverEM05   ! O  covergae with EM05                                 (-)
      REAL(4) RadSurf     ! O radiation on top of first water layer               (W/m2)
      INTEGER IQ                      !        Loop counter
      INTEGER Ifrom           !        From Segment
      INTEGER Ito                     !        From Segment
      LOGICAL First
      INTEGER IBotSeg         !        Bottom Segment for Macrophyte

      INTEGER IKMRK1, IKMRK2
      DATA    FIRST /.TRUE./
      SAVE    FIRST
!
!*******************************************************************************
!     Initialise variable indicating BOTTOM SEGMENT

      IF (FIRST) THEN

      IPNT(21) = IPOINT(21)
      DO ISEG = 1,NOSEG
         PMSA( IPNT( 21) ) = -1
         CALL evaluate_waq_attribute(2,IKNMRK(ISEG),IKMRK2)
         IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
            PMSA( IPNT( 21) ) = ISEG
         ENDIF
         IPNT(21) = IPNT(21) + INCREM(21)
      ENDDO
!     Loop to find bottom segment for water segments
      DO IQ = NOQ1+NOQ2+NOQ3, NOQ1 + NOQ2 +1, -1
         Ifrom  = IEXPNT(1,IQ)
         Ito       = IEXPNT(2,IQ)
         if (ifrom.gt.0.and.ito.gt.0) then
            IBOTSEG = nint(PMSA(IPOINT(21)+(ITO-1)*INCREM(21)))
            IF ( IBOTSEG .GT.0 ) THEN
               PMSA(IPOINT(21)+(IFROM-1)*INCREM(21)) = real(IBOTSEG)
            ENDIF
         endif
      ENDDO

      do iq = noq1+noq2+noq3+1, noq1+noq2+noq3+noq4
         ifrom  = iexpnt(1,iq)
         ito    = iexpnt(2,iq)
         if (ifrom.gt.0.and.ito.gt.0) then
            ibotseg = nint(pmsa(ipoint(21)+(ifrom-1)*increm(21)))
            if ( ibotseg .gt.0 ) then
               pmsa(ipoint(21)+(ito-1)*increm(21)) = real(ibotseg)
            endif
         endif
      enddo

      FIRST = .FALSE.
      ENDIF
!
!*******************************************************************************
!
      IPNT        = IPOINT
!
      DO 9000 ISEG = 1 , NOSEG

         CALL evaluate_waq_attribute(1,IKNMRK(ISEG),IKMRK1)

         IF (IKMRK1.EQ.1) THEN
            CALL evaluate_waq_attribute(2,IKNMRK(ISEG),IKMRK2)
            IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.1)) THEN

!           Calculation of fcover for top layer only

               nMacrophyt = PMSA( IPNT(  1) )
               MaxEM01    = PMSA( IPNT(  4) )
               MaxEM02    = PMSA( IPNT(  6) )
               MaxEM03    = PMSA( IPNT(  8) )
               MaxEM04    = PMSA( IPNT( 10) )
               MaxEM05    = PMSA( IPNT( 12) )
               IBotSeg    = nint(PMSA( IPNT( 13) ))
               IF (IBotSeg .le. 0)
     j            CALL write_error_message_with_values('IBotSeg',PMSA( IPNT( 13) ),ISEG,'COVMAC')

               RadIn      = PMSA( IPNT( 14) )
               EM01       = PMSA(IPOINT( 3)+(IBotSeg-1)*INCREM( 3))
               EM02       = PMSA(IPOINT( 5)+(IBotSeg-1)*INCREM( 5))
               EM03       = PMSA(IPOINT( 7)+(IBotSeg-1)*INCREM( 7))
               EM04       = PMSA(IPOINT( 9)+(IBotSeg-1)*INCREM( 9))
               EM05       = PMSA(IPOINT(11)+(IBotSeg-1)*INCREM(11))
!
!   *****     Insert your code here  *****
!
               ! check input


               ! coverage per species

               IF ( MaxEM01 .GT. 1E-20 ) THEN
                  CoverEM01  = EM01/MaxEM01
               ELSE
                  CoverEM01  = 0.0
               ENDIF

               IF ( MaxEM02 .GT. 1E-20 ) THEN
                  CoverEM02  = EM02/MaxEM02
               ELSE
                  CoverEM02  = 0.0
               ENDIF

               IF ( MaxEM03 .GT. 1E-20 ) THEN
                  CoverEM03  = EM03/MaxEM03
               ELSE
                  CoverEM03  = 0.0
               ENDIF

               IF ( MaxEM04 .GT. 1E-20 ) THEN
                  CoverEM04  = EM04/MaxEM04
               ELSE
                  CoverEM04  = 0.0
               ENDIF

               IF ( MaxEM05 .GT. 1E-20 ) THEN
                  CoverEM05  = EM05/MaxEM05
               ELSE
                  CoverEM05  = 0.0
               ENDIF

               ! overall coverage, use emerged only

               fcover     = min (1., (CoverEM01 + CoverEM02 + CoverEM03
     j                    + CoverEM04 + CoverEM05) )
               RadSurf = Radin*(1.-fcover)

            ELSE
!              no cover in other layers
               CoverEM01 = 0.0
               CoverEM02 = 0.0
               CoverEM03 = 0.0
               CoverEM04 = 0.0
               CoverEM05 = 0.0
               fcover = 0.
               RadSurf = 0.
            ENDIF
!
!   *****     End of your code       *****
!
            PMSA( IPNT( 15)   ) = fcover
            PMSA( IPNT( 16)   ) = CoverEM01
            PMSA( IPNT( 17)   ) = CoverEM02
            PMSA( IPNT( 18)   ) = CoverEM03
            PMSA( IPNT( 19)   ) = CoverEM04
            PMSA( IPNT( 20)   ) = CoverEM05
            PMSA( IPNT( 22)   ) = RadSurf

         ENDIF
!
         IPNT        = IPNT        + INCREM
!
 9000 CONTINUE
!
      RETURN
      END

      end module m_covmac
