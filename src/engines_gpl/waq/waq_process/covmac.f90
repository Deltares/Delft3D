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
module m_covmac
    use m_waq_precision

    implicit none

contains


    SUBROUTINE COVMAC     (process_space_real, FL, IPOINT, INCREM, num_cells, &
            NOFLUX, IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !
        !*******************************************************************************
        !
        use m_extract_waq_attribute
        use m_logger_helper, only : write_error_message_with_values
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        REAL(kind = real_wp) :: process_space_real(*)     !I/O Process Manager System Array, window of routine to process library
        REAL(kind = real_wp) :: FL(*)       ! O  Array of fluxes made by this process in mass/volume/time
        INTEGER(kind = int_wp) :: IPOINT(22) ! I  Array of pointers in process_space_real to get and store the data
        INTEGER(kind = int_wp) :: INCREM(22) ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
        INTEGER(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        INTEGER(kind = int_wp) :: NOFLUX      ! I  Number of fluxes, increment in the FL array
        INTEGER(kind = int_wp) :: IEXPNT(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        INTEGER(kind = int_wp) :: IKNMRK(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        INTEGER(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
        INTEGER(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        INTEGER(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        INTEGER(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        INTEGER(kind = int_wp) :: IPNT(22)   !    Local work array for the pointering
        INTEGER(kind = int_wp) :: ISEG        !    Local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        REAL(kind = real_wp) :: nMacrophyt  ! I  number of macrophyte species                       (-)
        REAL(kind = real_wp) :: EM01        ! I  macrophyt emerged 01                               (gC/m2)
        REAL(kind = real_wp) :: MaxEM01     ! I  maximum biomass for macrophyt emerged 01           (gC/m2)
        REAL(kind = real_wp) :: EM02        ! I  macrophyt emerged 02                               (gC/m2)
        REAL(kind = real_wp) :: MaxEM02     ! I  maximum biomass for EM02                           (gC/m2)
        REAL(kind = real_wp) :: EM03        ! I  macrophyt emerged 03                               (gC/m2)
        REAL(kind = real_wp) :: MaxEM03     ! I  maximum biomass for EM03                           (gC/m2)
        REAL(kind = real_wp) :: EM04        ! I  macrophyt emerged 04                               (gC/m2)
        REAL(kind = real_wp) :: MaxEM04     ! I  maximum biomass for EM04                           (gC/m2)
        REAL(kind = real_wp) :: EM05        ! I  macrophyt emerged 05                               (gC/m2)
        REAL(kind = real_wp) :: MaxEM05     ! I  maximum biomass for EM05                           (gC/m2)
        REAL(kind = real_wp) :: RadIn       ! I  incoming radiation                                 (W/m2)
        REAL(kind = real_wp) :: fcover      ! O  fraction of water surface covered <0-1>            (-)
        REAL(kind = real_wp) :: CoverEM01   ! O  covergae with EM01                                 (-)
        REAL(kind = real_wp) :: CoverEM02   ! O  covergae with EM02                                 (-)
        REAL(kind = real_wp) :: CoverEM03   ! O  covergae with EM03                                 (-)
        REAL(kind = real_wp) :: CoverEM04   ! O  covergae with EM04                                 (-)
        REAL(kind = real_wp) :: CoverEM05   ! O  covergae with EM05                                 (-)
        REAL(kind = real_wp) :: RadSurf     ! O radiation on top of first water layer               (W/m2)
        INTEGER(kind = int_wp) :: IQ                      !        Loop counter
        INTEGER(kind = int_wp) :: Ifrom           !        From Segment
        INTEGER(kind = int_wp) :: Ito                     !        From Segment
        LOGICAL First
        INTEGER(kind = int_wp) :: IBotSeg         !        Bottom Segment for Macrophyte

        INTEGER(kind = int_wp) :: IKMRK1, IKMRK2
        DATA    FIRST /.TRUE./
        SAVE    FIRST
        !
        !*******************************************************************************
        !     Initialise variable indicating BOTTOM SEGMENT

        IF (FIRST) THEN

            IPNT(21) = IPOINT(21)
            DO ISEG = 1, num_cells
                process_space_real(IPNT(21)) = -1
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    process_space_real(IPNT(21)) = ISEG
                ENDIF
                IPNT(21) = IPNT(21) + INCREM(21)
            ENDDO
            !     Loop to find bottom segment for water segments
            DO IQ = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir, num_exchanges_u_dir + num_exchanges_v_dir + 1, -1
                Ifrom = IEXPNT(1, IQ)
                Ito = IEXPNT(2, IQ)
                if (ifrom>0.and.ito>0) then
                    IBOTSEG = nint(process_space_real(IPOINT(21) + (ITO - 1) * INCREM(21)))
                    IF (IBOTSEG >0) THEN
                        process_space_real(IPOINT(21) + (IFROM - 1) * INCREM(21)) = real(IBOTSEG)
                    ENDIF
                endif
            ENDDO

            do iq = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir
                ifrom = iexpnt(1, iq)
                ito = iexpnt(2, iq)
                if (ifrom>0.and.ito>0) then
                    ibotseg = nint(process_space_real(ipoint(21) + (ifrom - 1) * increm(21)))
                    if (ibotseg >0) then
                        process_space_real(ipoint(21) + (ito - 1) * increm(21)) = real(ibotseg)
                    endif
                endif
            enddo

            FIRST = .FALSE.
        ENDIF
        !
        !*******************************************************************************
        !
        IPNT = IPOINT
        !
        DO ISEG = 1, num_cells

            CALL extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)

            IF (IKMRK1==1) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==1)) THEN

                    !           Calculation of fcover for top layer only

                    nMacrophyt = process_space_real(IPNT(1))
                    MaxEM01 = process_space_real(IPNT(4))
                    MaxEM02 = process_space_real(IPNT(6))
                    MaxEM03 = process_space_real(IPNT(8))
                    MaxEM04 = process_space_real(IPNT(10))
                    MaxEM05 = process_space_real(IPNT(12))
                    IBotSeg = nint(process_space_real(IPNT(13)))
                    IF (IBotSeg <= 0) &
                            CALL write_error_message_with_values('IBotSeg', process_space_real(IPNT(13)), ISEG, 'COVMAC')

                    RadIn = process_space_real(IPNT(14))
                    EM01 = process_space_real(IPOINT(3) + (IBotSeg - 1) * INCREM(3))
                    EM02 = process_space_real(IPOINT(5) + (IBotSeg - 1) * INCREM(5))
                    EM03 = process_space_real(IPOINT(7) + (IBotSeg - 1) * INCREM(7))
                    EM04 = process_space_real(IPOINT(9) + (IBotSeg - 1) * INCREM(9))
                    EM05 = process_space_real(IPOINT(11) + (IBotSeg - 1) * INCREM(11))
                    !
                    !   *****     Insert your code here  *****
                    !
                    ! check input


                    ! coverage per species

                    IF (MaxEM01 > 1E-20) THEN
                        CoverEM01 = EM01 / MaxEM01
                    ELSE
                        CoverEM01 = 0.0
                    ENDIF

                    IF (MaxEM02 > 1E-20) THEN
                        CoverEM02 = EM02 / MaxEM02
                    ELSE
                        CoverEM02 = 0.0
                    ENDIF

                    IF (MaxEM03 > 1E-20) THEN
                        CoverEM03 = EM03 / MaxEM03
                    ELSE
                        CoverEM03 = 0.0
                    ENDIF

                    IF (MaxEM04 > 1E-20) THEN
                        CoverEM04 = EM04 / MaxEM04
                    ELSE
                        CoverEM04 = 0.0
                    ENDIF

                    IF (MaxEM05 > 1E-20) THEN
                        CoverEM05 = EM05 / MaxEM05
                    ELSE
                        CoverEM05 = 0.0
                    ENDIF

                    ! overall coverage, use emerged only

                    fcover = min (1., (CoverEM01 + CoverEM02 + CoverEM03 &
                            + CoverEM04 + CoverEM05))
                    RadSurf = Radin * (1. - fcover)

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
                process_space_real(IPNT(15)) = fcover
                process_space_real(IPNT(16)) = CoverEM01
                process_space_real(IPNT(17)) = CoverEM02
                process_space_real(IPNT(18)) = CoverEM03
                process_space_real(IPNT(19)) = CoverEM04
                process_space_real(IPNT(20)) = CoverEM05
                process_space_real(IPNT(22)) = RadSurf

            ENDIF
            !
            IPNT = IPNT + INCREM
            !
        end do
        !
        RETURN
    END

end module m_covmac
