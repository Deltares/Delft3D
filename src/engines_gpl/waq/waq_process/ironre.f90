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
module m_ironre
    use m_waq_precision

    implicit none

contains


    subroutine IRONRE     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !JVB$ ATTRIBUTES DLLEXPORT, ALIAS: 'IRONRE' :: IRONRE
        !
        !*******************************************************************************
        !
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: process_space_real(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(17) ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(17) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(17)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        real(kind = real_wp) :: fes         ! I  iron(II) sulphide                                  (gFe/m3)
        real(kind = real_wp) :: feiiipa     ! I  particulate amorphous oxidizing iron               (gFe/m3)
        real(kind = real_wp) :: feiiipc     ! I  particulate crystalline oxidizing iron             (gFe/m3)
        real(kind = real_wp) :: sud         ! I  total dissolved sulphide (SUD)                     (gS/m3)
        real(kind = real_wp) :: frh2sdis    ! I  fraction of dissolved hydrogen sulphide            (-)
        real(kind = real_wp) :: rcfeah2s20  ! I  rate of amorphous iron red. with H2S               (m3/gS/d)
        real(kind = real_wp) :: rcfech2s20  ! I  rate of crystalline iron red. with H2S             (m3/gS/d)
        real(kind = real_wp) :: rcfeafes20  ! I  rate of amorphous iron red. with FeS               (m3/gFe/d)
        real(kind = real_wp) :: rcfecfes20  ! I  rate of crystalline iron red. with FeS             (m3/gFe/d)
        real(kind = real_wp) :: tcfered     ! I  temperature coeff. for abiotic iron reduction      (-)
        real(kind = real_wp) :: temp        ! I  ambient water temperature                          (oC)
        real(kind = real_wp) :: delt        ! I  timestep for processes                             (d)
        real(kind = real_wp) :: poros       ! I  volumetric porosity                                (-)
        real(kind = real_wp) :: fire1       ! O  rate of amorphous iron red. with H2S               (gFe/m3/d)
        real(kind = real_wp) :: fire2       ! O  rate of crystalline iron red. with H2S             (gFe/m3/d)
        real(kind = real_wp) :: fire3       ! O  rate of amorphous iron red. with FeS               (gFe/m3/d)
        real(kind = real_wp) :: fire4       ! O  rate of crystalline iron red. with FeS             (gFe/m3/d)
        real(kind = real_wp) :: dire1       ! F  rate of amorphous iron red. with H2S               (gFe/m3/d)
        real(kind = real_wp) :: dire2       ! F  rate of crystalline iron red. with H2S             (gFe/m3/d)
        real(kind = real_wp) :: dire3       ! F  rate of amorphous iron red. with FeS               (gFe/m3/d)
        real(kind = real_wp) :: dire4       ! F  rate of crystalline iron red. with FeS             (gFe/m3/d)
        integer(kind = int_wp) :: idire1      !    Pointer to the rate of amorphous iron red. with H2S
        integer(kind = int_wp) :: idire2      !    Pointer to the rate of crystalline iron red. with H2S
        integer(kind = int_wp) :: idire3      !    Pointer to the rate of amorphous iron red. with FeS
        integer(kind = int_wp) :: idire4      !    Pointer to the rate of crystalline iron red. with FeS
        real(kind = real_wp) :: tffered     ! L  temperature function for abiotic iron reduction
        real(kind = real_wp) :: kire1       ! L  rate of amorphous iron red. with H2S
        real(kind = real_wp) :: kire2       ! L  rate of crystalline iron red. with H2S
        real(kind = real_wp) :: kire3       ! L  rate of amorphous iron red. with FeS
        real(kind = real_wp) :: kire4       ! L  rate of crystalline iron red. with FeS

        ! initialise pointering in process_space_real

        ipnt = ipoint
        idire1 = 1
        idire2 = 2
        idire3 = 3
        idire4 = 4

        do iseg = 1, num_cells

            fes = max(process_space_real(ipnt(1)), 0.0)
            feiiipa = max(process_space_real(ipnt(2)), 0.0)
            feiiipc = max(process_space_real(ipnt(3)), 0.0)
            sud = max(process_space_real(ipnt(4)), 0.0)
            frh2sdis = process_space_real(ipnt(5))
            rcfeah2s20 = process_space_real(ipnt(6))
            rcfech2s20 = process_space_real(ipnt(7))
            rcfeafes20 = process_space_real(ipnt(8))
            rcfecfes20 = process_space_real(ipnt(9))
            tcfered = process_space_real(ipnt(10))
            temp = process_space_real(ipnt(11))
            delt = process_space_real(ipnt(12))
            poros = process_space_real(ipnt(13))

            ! temperature function

            tffered = tcfered**(temp - 20.0)

            ! temperature corrected rates

            kire1 = rcfeah2s20 * tffered
            kire2 = rcfech2s20 * tffered
            kire3 = rcfeafes20 * tffered
            kire4 = rcfecfes20 * tffered

            ! fluxes

            dire1 = kire1 * feiiipa * frh2sdis * sud
            dire2 = kire2 * feiiipc * frh2sdis * sud
            dire3 = kire3 * fes * feiiipa
            dire4 = kire4 * fes * feiiipc

            ! maximise fluxes if neccesary

            if (dire1 + dire3 > feiiipa / delt) then
                dire1 = (dire1 / (dire1 + dire3)) * 0.5 * feiiipa / delt
                dire3 = (dire3 / (dire1 + dire3)) * 0.5 * feiiipa / delt
            endif
            if (dire2 + dire4 > feiiipc / delt) then
                dire2 = (dire2 / (dire2 + dire4)) * 0.5 * feiiipc / delt
                dire3 = (dire4 / (dire2 + dire4)) * 0.5 * feiiipc / delt
            endif
            if (dire1 + dire2 > 0.0714 * frh2sdis * sud / delt) then
                dire1 = (dire1 / (dire1 + dire2)) * 0.5 * 0.0714 * frh2sdis * sud / delt
                dire2 = (dire2 / (dire1 + dire2)) * 0.5 * 0.0714 * frh2sdis * sud / delt
            endif
            if (dire3 + dire4 > 0.125 * fes / delt) then
                dire3 = (dire3 / (dire3 + dire4)) * 0.5 * 0.125 * fes / delt
                dire4 = (dire4 / (dire3 + dire4)) * 0.5 * 0.125 * fes / delt
            endif

            fire1 = dire1
            fire2 = dire2
            fire3 = dire3
            fire4 = dire4

            ! store flux and process_space_real

            fl  (idire1) = dire1
            fl  (idire2) = dire2
            fl  (idire3) = dire3
            fl  (idire4) = dire4
            process_space_real(ipnt(14)) = fire1
            process_space_real(ipnt(15)) = fire2
            process_space_real(ipnt(16)) = fire3
            process_space_real(ipnt(17)) = fire4

            idire1 = idire1 + noflux
            idire2 = idire2 + noflux
            idire3 = idire3 + noflux
            idire4 = idire4 + noflux
            ipnt = ipnt + increm

        end do

        return
    end

end module m_ironre
