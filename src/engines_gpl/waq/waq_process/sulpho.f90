!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_sulpho
    use m_waq_precision

    implicit none

contains


    subroutine SULPHO     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !JVB$ ATTRIBUTES DLLEXPORT, ALIAS: 'SULPHO' :: SULPHO
        !
        !*******************************************************************************
        !
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: process_space_real(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(11) ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(11) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(11)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        real(kind = real_wp) :: fes         ! I  iron(II) sulphide                                  (gFe/m3)
        real(kind = real_wp) :: fes2        ! I  pyrite                                             (gFe/m3)
        real(kind = real_wp) :: oxy         ! I  Dissolved Oxygen                                   (g/m3)
        real(kind = real_wp) :: rcfesox20   ! I  specific rate of iron sulphide oxidation           (m3/gO2/d)
        real(kind = real_wp) :: rcfes2ox20  ! I  specific rate of pyrite oxidation                  (m3/gO2/d)
        real(kind = real_wp) :: tcfesox     ! I  temperature coeff. for iron sulphide oxidation     (-)
        real(kind = real_wp) :: temp        ! I  ambient water temperature                          (oC)
        real(kind = real_wp) :: delt        ! I  timestep for processes                             (d)
        real(kind = real_wp) :: poros       ! I  volumetric porosity                                (-)
        real(kind = real_wp) :: ffesox      ! O  rate of iron sulphide oxidation                    (gFe/m3/d)
        real(kind = real_wp) :: ffes2ox     ! O  rate of of pyrite oxidation                        (gFe/m3/d)
        real(kind = real_wp) :: dfesox      ! F  rate of iron sulphide oxidation                    (gFe/m3/d)
        real(kind = real_wp) :: dfes2ox     ! F  rate of of pyrite oxidation                        (gFe/m3/d)
        integer(kind = int_wp) :: idfesox     !    Pointer to the rate of iron sulphide oxidation
        integer(kind = int_wp) :: idfes2ox    !    Pointer to the rate of of pyrite oxidation
        real(kind = real_wp) :: tffesox     ! L  temperature function for iron sulphide oxidation
        real(kind = real_wp) :: rcfesox     ! L  specific rate of iron sulphide oxidation
        real(kind = real_wp) :: rcfes2ox    ! L  specific rate of pyrite oxidation

        ! initialise pointering in process_space_real

        ipnt = ipoint
        idfesox = 1
        idfes2ox = 2

        do iseg = 1, num_cells

            fes = process_space_real(ipnt(1))
            fes2 = process_space_real(ipnt(2))
            oxy = process_space_real(ipnt(3))
            rcfesox20 = process_space_real(ipnt(4))
            rcfes2ox20 = process_space_real(ipnt(5))
            tcfesox = process_space_real(ipnt(6))
            temp = process_space_real(ipnt(7))
            delt = process_space_real(ipnt(8))
            poros = process_space_real(ipnt(9))

            ! no oxidation if no oxygen

            if (oxy <= 0.0) then

                ffesox = 0.0
                ffes2ox = 0.0

            else
                ! temperature function

                tffesox = tcfesox**(temp - 20.0)
                rcfesox = rcfesox20 * tffesox
                rcfes2ox = rcfes2ox20 * tffesox

                ! fluxes

                ffesox = rcfesox * fes * oxy
                ffes2ox = rcfes2ox * fes2 * oxy

                ! maximise fluxes if neccesary

                if (ffesox > fes / delt) then
                    ffesox = 0.5 * fes / delt
                endif
                if (ffes2ox > fes2 / delt) then
                    ffes2ox = 0.5 * fes2 / delt
                endif

            endif

            dfesox = ffesox
            dfes2ox = ffes2ox

            ! store flux and process_space_real

            fl  (idfesox) = dfesox
            fl  (idfes2ox) = dfes2ox
            process_space_real(ipnt(10)) = ffesox
            process_space_real(ipnt(11)) = ffes2ox

            idfesox = idfesox + noflux
            idfes2ox = idfes2ox + noflux
            ipnt = ipnt + increm

        end do

        return
    end

end module m_sulpho
