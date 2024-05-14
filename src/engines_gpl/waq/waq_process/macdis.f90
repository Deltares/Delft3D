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
module m_macdis
    use m_waq_precision

    implicit none

contains


    SUBROUTINE MACDIS     (PMSA, FL, IPOINT, INCREM, NOSEG, &
            NOFLUX, IEXPNT, IKNMRK, NOQ1, NOQ2, &
            NOQ3, NOQ4)
        use m_logger, only : terminate_execution, get_log_unit_number
        use m_evaluate_waq_attribute

        !
        !*******************************************************************************
        !
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        REAL(kind = real_wp) :: PMSA(*)      !I/O Process Manager System Array, window of routine to process library
        REAL(kind = real_wp) :: FL(*)        ! O  Array of fluxes made by this process in mass/volume/time
        INTEGER(kind = int_wp) :: IPOINT(14)   ! I  Array of pointers in PMSA to get and store the data
        INTEGER(kind = int_wp) :: INCREM(14)   ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
        INTEGER(kind = int_wp) :: NOSEG        ! I  Number of computational elements in the whole model schematisation
        INTEGER(kind = int_wp) :: NOFLUX       ! I  Number of fluxes, increment in the FL array
        INTEGER(kind = int_wp) :: IEXPNT(4, *)  ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        INTEGER(kind = int_wp) :: IKNMRK(*)    ! I  Active-Inactive, Surface-water-bottom, see manual for use
        INTEGER(kind = int_wp) :: NOQ1         ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
        INTEGER(kind = int_wp) :: NOQ2         ! I  Nr of exchanges in 2nd direction, NOQ1+NOQ2 gives hor. dir. reg. grid
        INTEGER(kind = int_wp) :: NOQ3         ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        INTEGER(kind = int_wp) :: NOQ4         ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        INTEGER(kind = int_wp) :: IPNT(14)     !    Local work array for the pointering
        INTEGER(kind = int_wp) :: ISEG         !    Local loop counter for computational element loop
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        REAL(kind = real_wp) :: Surf         ! I  surface of segment                            (m2)
        REAL(kind = real_wp) :: Depth        ! I  depth of segment                               (m)
        REAL(kind = real_wp) :: TotalDepth   ! I  total depth water column                       (m)
        REAL(kind = real_wp) :: LocalDepth   ! I  depth from water surface to bottom of segment  (m)
        REAL(kind = real_wp) :: SM           ! I  macrophyte submerged                       (gC/m2)
        REAL(kind = real_wp) :: smmax        ! I  Maximum biomass macrophyte submerged       (gC/m2)
        REAL(kind = real_wp) :: SwDisSM      ! I  macrophyt distr. function (1: lin, 2:Exp)      (-)
        REAL(kind = real_wp) :: Hmax         ! I  Maxmimum Length Macrophytes                    (m)
        REAL(kind = real_wp) :: Ffac         ! I  Form factor lin: F = M(mean)/(M/Hmax)          (-)
        REAL(kind = real_wp) :: BmLaySM      ! O  Biomass Layer macrophyt submerged 01       (gC/m2)
        REAL(kind = real_wp) :: Hact         ! O  Actual Length Macrophytes                      (m)
        REAL(kind = real_wp) :: Z2           !    Height Bottom Segment from Bottom              (m)
        REAL(kind = real_wp) :: Z1           !    Height Top Segment from Bottom                 (m)
        REAL(kind = real_wp) :: Z2a          !    Height Bottom Segment from Bottom              (m)
        REAL(kind = real_wp) :: Z1a          !    Height Top Segment from Bottom                 (m)
        REAL(kind = real_wp) :: Hactd        !    Actual Length Macrophytes - relative to top    (-)
        REAL(kind = real_wp) :: Z2ad         !    Height Bottom Segment from Bottom - relative   (-)
        REAL(kind = real_wp) :: Z1ad         !    Height Top Segment from Bottom - relative      (-)
        REAL(kind = real_wp) :: absHmax      !    Absolute maxmimum Length Macrophytes           (m)
        INTEGER(kind = int_wp) :: IKMRK1
        INTEGER IKMRK2
        REAL(kind = real_wp) :: FrBmLay      !    Fraction BM per layer                          (-)
        REAL(kind = real_wp) :: Zm           !    Watersurface to top Macropyte                  (-)
        REAL(kind = real_wp) :: A            !    Lineair factor A (Ax + B)                      (-)
        REAL(kind = real_wp) :: B            !    Lineair factor B (Ax + B)                      (-)
        REAL(kind = real_wp) :: OriginalDepth
        !          INTEGER IQ           !    Loop counter
        !          INTEGER Ifrom       !    From Segment
        !          INTEGER Ito         !    From Segment
        !          LOGICAL First

        INTEGER(kind = int_wp) :: LUNREP

        INTEGER IBotSeg     ! Bottom Segment for Macrophyte
        !     INTEGER ITopSeg     ! Top    Segment for Macrophyte
        !*******************************************************************************

        IPNT = IPOINT
        !     Loop over segments
        DO ISEG = 1, NOSEG

            !        Check on active segments
            CALL evaluate_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            IF (IKMRK1==1) THEN

                Surf = PMSA(IPNT(1))
                Depth = PMSA(IPNT(2))
                TotalDepth = PMSA(IPNT(3))
                LocalDepth = PMSA(IPNT(4))
                SwDisSM = PMSA(IPNT(6))
                Hmax = PMSA(IPNT(7))
                Ffac = PMSA(IPNT(8))
                IBotSeg = NINT(PMSA(IPNT(9)))
                smmax = PMSA(IPNT(10))

                ! get biomass from bottom segment

                !SM          = max( 1.0e-10, PMSA(IPOINT(5)+(IBOTSEG-1)*INCREM(5)) )
                SM = PMSA(IPOINT(5) + (IBOTSEG - 1) * INCREM(5))

                !           Limit the maximum height of the plants to the water depth
                absHmax = min(abs(Hmax), TotalDepth)

                ! actual height is fraction of maximum height

                if (smmax > 1e-20) then
                    Hact = min(absHmax * SM / smmax, TotalDepth - 0.001)
                    Hact = max(Hact, 0.01)
                else
                    Hact = 0.01
                endif
                Hactd = 1.0 ! Represents the entire length of the plants

                !
                ! Hmax > 0: the macrophytes grow from the bottom upwards
                ! Hmax < 0: the macrophytes grow from the surface downwards
                !
                OriginalDepth = TotalDepth
                if (hmax < 0.0) then
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
                If (SwDisSM == 1) Then

                    !              Check Ffac: 0,1 or 2

                    If (Ffac  <  0 .OR. Ffac > 2) Then
                        call get_log_unit_number(lunrep)
                        write (lunrep, *) 'MACDIS: Illegal option for Macrophyte form factor - should be between 0 and 2'
                        write (lunrep, *) '   Value now: ', ffac
                        write (lunrep, *) '   Input error (linear biomass distribution)'
                        write (lunrep, *) 'Input error in process MACDIS'
                        write (*, *) 'Input error in process MACDIS'
                        call terminate_execution(1)
                    Endif

                    A = (SM / Hact) * (2 - (2 * Ffac)) / Hact
                    B = (SM / Hact) * (Ffac * (Zm + TotalDepth) - 2 * Zm) / Hact

                    !              Macrophyte is not in segment:
                    If (Zm > Z2) Then
                        BmLaySM = 0
                        !                 Macropyhte is completely in segment:
                    Elseif (Zm < Z1) Then
                        BmLaySM = (A / 2) * (Z2**2 - Z1**2) + B * (Z2 - Z1)
                        !                 Macropyhte is partialy in segment: TIP !!!!
                    Else
                        BmlaySM = (A / 2) * (Z2**2 - Zm**2) + B * (Z2 - Zm)
                        !                 For the segment IBotSeg, current segment is ITopSeg!!!
                        PMSA(IPOINT(14) + (IBotSeg - 1) * INCREM(14)) = ISEG
                    Endif

                    !              Switch = 2:  Exponential Biomass distribution

                ElseIf (SwDisSM == 2) Then

                    If (Ffac  <=  0 .OR. Ffac > 50.0) Then
                        call get_log_unit_number(lunrep)
                        write (lunrep, *) 'MACDIS: Incorrect value for Macrophyte form factor - ', &
                                'should be positive and lower than or equal to 50'
                        write (lunrep, *) '   Value now: ', ffac
                        write (lunrep, *) '   Input error (exponential biomass distribution)'
                        write (lunrep, *) 'Input error in process MACDIS'
                        write (*, *) 'Input error in process MACDIS'
                        call terminate_execution(1)
                    Endif

                    A = SM / Hactd / ((exp(Ffac * Hactd) - 1.0) / Ffac - Hactd)
                    !              Macrophyte is not in segment:
                    If (Hact < Z1a) Then
                        BmLaySM = 0
                        !              Macrophyte is completely in segment:
                    Elseif (Hact > Z2a) Then
                        BmLaySM = A * ((exp(Ffac * Z2ad) - exp(Ffac * Z1ad)) / Ffac - (Z2ad - Z1ad))
                        !              Macrophyte is partially in segment: TIP !!!
                    Else
                        BmLaySM = A * ((exp(Ffac * Hactd) - exp(Ffac * Z1ad)) / Ffac - (Hactd - Z1ad))
                        !                 For the segment IBotSeg, current segment is ITopSeg!!!
                        PMSA(IPOINT(14) + (IBotSeg - 1) * INCREM(14)) = ISEG
                    Endif

                ENDIF

                !
                ! One complication: we need to set the top segment explicitly
                ! if we start at the top
                !
                If (Hmax < 0.0) Then
                    CALL evaluate_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                    If (IKMRK2 == 0 .OR. IKMRK2 == 1) Then
                        PMSA(IPOINT(14) + (IBotSeg - 1) * INCREM(14)) = ISEG
                    Endif
                Endif

                If (SM > 0) Then
                    FrBmLay = BmLaySm / SM
                Else
                    If (iseg == IBotseg) Then
                        FrBmLay = 1.0
                    Else
                        FrBmLay = 0.0
                    Endif
                Endif

                !           Return Outputparameters to delwaq

                PMSA(IPNT(11)) = FrBmLay
                PMSA(IPNT(12)) = BmLaySM / Depth
                If (Hmax > 0.0) Then
                    PMSA(IPNT(13)) = Hact
                Else
                    PMSA(IPNT(13)) = OriginalDepth
                Endif

            ENDIF

            IPNT = IPNT + INCREM

        end do
        !
        RETURN
    END

end module m_macdis
