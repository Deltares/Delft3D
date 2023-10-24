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
      module m_macdis

      implicit none

      contains


      SUBROUTINE MACDIS     ( PMSA   , FL     , IPOINT , INCREM, NOSEG ,
     +                        NOFLUX , IEXPNT , IKNMRK , NOQ1  , NOQ2  ,
     +                        NOQ3   , NOQ4   )
      use m_srstop
      use m_monsys
      use m_evaluate_waq_attribute

!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      REAL(4) PMSA(*)     !I/O Process Manager System Array, window of routine to process library
      REAL(4) FL(*)       ! O  Array of fluxes made by this process in mass/volume/time
      INTEGER IPOINT(14)  ! I  Array of pointers in PMSA to get and store the data
      INTEGER INCREM(14)  ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
      INTEGER NOSEG       ! I  Number of computational elements in the whole model schematisation
      INTEGER NOFLUX      ! I  Number of fluxes, increment in the FL array
      INTEGER IEXPNT(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      INTEGER IKNMRK(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      INTEGER NOQ1        ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      INTEGER NOQ2        ! I  Nr of exchanges in 2nd direction, NOQ1+NOQ2 gives hor. dir. reg. grid
      INTEGER NOQ3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      INTEGER NOQ4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      INTEGER IPNT(14)    !    Local work array for the pointering
      INTEGER ISEG        !    Local loop counter for computational element loop
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      REAL(4) Surf        ! I  surface of segment                            (m2)
      REAL(4) Depth       ! I  depth of segment                               (m)
      REAL(4) TotalDepth  ! I  total depth water column                       (m)
      REAL(4) LocalDepth  ! I  depth from water surface to bottom of segment  (m)
      REAL(4) SM          ! I  macrophyte submerged                       (gC/m2)
      REAL(4) smmax       ! I  Maximum biomass macrophyte submerged       (gC/m2)
      REAL(4) SwDisSM     ! I  macrophyt distr. function (1: lin, 2:Exp)      (-)
      REAL(4) Hmax        ! I  Maxmimum Length Macrophytes                    (m)
      REAL(4) Ffac        ! I  Form factor lin: F = M(mean)/(M/Hmax)          (-)
      REAL(4) BmLaySM     ! O  Biomass Layer macrophyt submerged 01       (gC/m2)
      REAL(4) Hact        ! O  Actual Length Macrophytes                      (m)
      REAL(4) Z2          !    Height Bottom Segment from Bottom              (m)
      REAL(4) Z1          !    Height Top Segment from Bottom                 (m)
      REAL(4) Z2a         !    Height Bottom Segment from Bottom              (m)
      REAL(4) Z1a         !    Height Top Segment from Bottom                 (m)
      REAL(4) Hactd       !    Actual Length Macrophytes - relative to top    (-)
      REAL(4) Z2ad        !    Height Bottom Segment from Bottom - relative   (-)
      REAL(4) Z1ad        !    Height Top Segment from Bottom - relative      (-)
      REAL(4) absHmax     !    Absolute maxmimum Length Macrophytes           (m)
      INTEGER IKMRK1
      INTEGER IKMRK2
      REAL(4) FrBmLay     !    Fraction BM per layer                          (-)
      REAL(4) Zm          !    Watersurface to top Macropyte                  (-)
      REAL(4) A           !    Lineair factor A (Ax + B)                      (-)
      REAL(4) B           !    Lineair factor B (Ax + B)                      (-)
      REAL(4) OriginalDepth
c     INTEGER IQ          !    Loop counter
c     INTEGER Ifrom       !    From Segment
c     INTEGER Ito         !    From Segment
c     LOGICAL First

      INTEGER :: LUNREP

      INTEGER IBotSeg     ! Bottom Segment for Macrophyte
!     INTEGER ITopSeg     ! Top    Segment for Macrophyte
!*******************************************************************************


      IPNT = IPOINT
!     Loop over segments
      DO 9000 ISEG = 1 , NOSEG

!        Check on active segments
         CALL evaluate_waq_attribute(1,IKNMRK(ISEG),IKMRK1)
         IF (IKMRK1.EQ.1) THEN

            Surf        = PMSA( IPNT(  1) )
            Depth       = PMSA( IPNT(  2) )
            TotalDepth  = PMSA( IPNT(  3) )
            LocalDepth  = PMSA( IPNT(  4) )
            SwDisSM     = PMSA( IPNT(  6) )
            Hmax        = PMSA( IPNT(  7) )
            Ffac        = PMSA( IPNT(  8) )
            IBotSeg     = NINT(PMSA( IPNT(  9) ))
            smmax       = PMSA( IPNT( 10) )

            ! get biomass from bottom segment

            !SM          = max( 1.0e-10, PMSA(IPOINT(5)+(IBOTSEG-1)*INCREM(5)) )
            SM          = PMSA(IPOINT(5)+(IBOTSEG-1)*INCREM(5))

!           Limit the maximum height of the plants to the water depth
            absHmax     = min( abs(Hmax), TotalDepth )

            ! actual height is fraction of maximum height

            if ( smmax .gt. 1e-20 ) then
               Hact        = min( absHmax * SM/smmax , TotalDepth - 0.001 )
               Hact        = max(Hact,0.01)
            else
               Hact        = 0.01
            endif
            Hactd = 1.0 ! Represents the entire length of the plants

            !
            ! Hmax > 0: the macrophytes grow from the bottom upwards
            ! Hmax < 0: the macrophytes grow from the surface downwards
            !
            OriginalDepth = TotalDepth
            if ( hmax < 0.0 ) then
               TotalDepth = Hact
            endif

            Zm = TotalDepth - Hact
            Z1 = LocalDepth - Depth
            Z2 = LocalDepth
            Z1a = TotalDepth - LocalDepth
            Z2a = TotalDepth - LocalDepth + Depth

            Z1ad = Z1a / Hact
            Z2ad = Z2a / Hact

!           Switch = 1:  linear Biomass distribution
            If (SwDisSM .EQ. 1 ) Then

!              Check Ffac: 0,1 or 2

               If (Ffac  .LT.  0 .OR. Ffac . GT. 2) Then
                  call getmlu( lunrep )
                  write (lunrep,*) 'MACDIS: Illegal option for Macrophyte form factor - should be between 0 and 2'
                  write (lunrep,*) '   Value now: ', ffac
                  write (lunrep,*) '   Input error (linear biomass distribution)'
                  write (lunrep,*) 'Input error in process MACDIS'
                  write (*,*) 'Input error in process MACDIS'
                  call srstop(1)
               Endif

               A = (SM / Hact) * (2 - (2 * Ffac)) / Hact
               B = (SM / Hact) * (Ffac * (Zm + TotalDepth) -2 * Zm) / Hact

!              Macrophyte is not in segment:
               If (Zm .GT. Z2) Then
                  BmLaySM = 0
!                 Macropyhte is completely in segment:
               Elseif (Zm . LT. Z1 ) Then
                  BmLaySM = (A/2)  * (Z2**2 -Z1**2) + B * (Z2 -Z1)
!                 Macropyhte is partialy in segment: TIP !!!!
               Else
                  BmlaySM = (A/2)  * (Z2**2 -Zm**2) + B * (Z2 -Zm)
!                 For the segment IBotSeg, current segment is ITopSeg!!!
                  PMSA(IPOINT(14)+(IBotSeg-1)*INCREM(14   )) = ISEG
               Endif

!              Switch = 2:  Exponential Biomass distribution

            ElseIf (SwDisSM .EQ. 2) Then

               If (Ffac  .LE.  0 .OR. Ffac .GT. 50.0 ) Then
                  call getmlu( lunrep )
                  write (lunrep,*) 'MACDIS: Incorrect value for Macrophyte form factor - ',
     &                             'should be positive and lower than or equal to 50'
                  write (lunrep,*) '   Value now: ', ffac
                  write (lunrep,*) '   Input error (exponential biomass distribution)'
                  write (lunrep,*) 'Input error in process MACDIS'
                  write (*,*) 'Input error in process MACDIS'
                  call srstop(1)
               Endif

               A = SM / Hactd / ((exp(Ffac * Hactd) - 1.0)/ Ffac - Hactd)
!              Macrophyte is not in segment:
               If (Hact .LT. Z1a) Then
                  BmLaySM = 0
!              Macrophyte is completely in segment:
               Elseif (Hact . GT. Z2a) Then
                  BmLaySM = A * ( (exp(Ffac * Z2ad) - exp(Ffac * Z1ad)) / Ffac - (Z2ad - Z1ad) )
!              Macrophyte is partially in segment: TIP !!!
               Else
                  BmLaySM = A * ((exp(Ffac * Hactd) - exp(Ffac * Z1ad)) / Ffac - (Hactd - Z1ad) )
!                 For the segment IBotSeg, current segment is ITopSeg!!!
                  PMSA(IPOINT(14)+(IBotSeg-1)*INCREM(14   )) = ISEG
               Endif

            ENDIF

            !
            ! One complication: we need to set the top segment explicitly
            ! if we start at the top
            !
            If ( Hmax < 0.0 ) Then
                CALL evaluate_waq_attribute(2,IKNMRK(ISEG),IKMRK2)
                If ( IKMRK2 .EQ. 0 .OR. IKMRK2 .EQ. 1 ) Then
                    PMSA(IPOINT(14)+(IBotSeg-1)*INCREM(14   )) = ISEG
                Endif
            Endif

            If (SM .GT. 0) Then
               FrBmLay = BmLaySm / SM
            Else
               If ( iseg .eq. IBotseg ) Then
                  FrBmLay = 1.0
               Else
                  FrBmLay = 0.0
               Endif
            Endif

!           Return Outputparameters to delwaq

            PMSA( IPNT(11) ) = FrBmLay
            PMSA( IPNT(12) ) = BmLaySM / Depth
            If ( Hmax > 0.0 ) Then
                PMSA( IPNT(13) ) = Hact
            Else
                PMSA( IPNT(13) ) = OriginalDepth
            Endif

        ENDIF

        IPNT = IPNT + INCREM

 9000 CONTINUE
!
      RETURN
      END

      end module m_macdis
