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
module m_protistdiatsedi
    use m_waq_precision

    implicit none

contains



    ! 6 char name for process mathc with second line of PDF
    subroutine PROSED     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !
        !*******************************************************************************
        !

        use m_extract_waq_attribute
        use protist_constants
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: process_space_real(*)      ! I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)        ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(*)    ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(*)    ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells        ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux       ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *)  ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)    ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir         ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir         ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir         ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir         ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        !     support variables
        integer(kind = int_wp), parameter :: nrIndInp = 6     !   nr of species independent input items
        integer(kind = int_wp), parameter :: nrSpecInp = 7    !   nr of inputs per species
        integer(kind = int_wp), parameter :: nrSpecOut = 6    !   nr of outputs per species
        integer(kind = int_wp) :: nrInputItems     !   nr of input items need for output process_space_real
        integer(kind = int_wp) :: nrOutputItems    !   nr of output items need for output process_space_real
        integer(kind = int_wp) :: ipointLength     !   total length of the process_space_real input and output pointer array
        integer(kind = int_wp), allocatable :: ipnt(:)          !   Local work array for the pointering

        integer(kind = int_wp) :: iseg          ! Local loop counter for computational element loop
        integer(kind = int_wp) :: ioq
        integer(kind = int_wp) :: iflux
        integer(kind = int_wp) :: ikmrk1        ! first segment attribute
        integer(kind = int_wp) :: ikmrk2        ! second segment attribute
        integer(kind = int_wp) :: ikmrkv
        integer(kind = int_wp) :: ikmrkn

        integer(kind = int_wp) :: i_origin, i_dest

        integer(kind = int_wp) :: iSpec         ! local species number counter
        integer(kind = int_wp) :: spInc         ! local species process_space_real/FL number increment
        integer(kind = int_wp) :: inpItems      ! nr of input items need for output process_space_real

        !input parameters
        integer(kind = int_wp) :: nrSpec       ! total nr species implemented in process (from proc_def)
        real(kind = real_wp) :: DELT, MinDepth, TaucSDiat                  ! segment independent input
        real(kind = real_wp) :: Tau, Depth                                 ! segment dependent input
        real(kind = real_wp) :: ZSedDiat, VSedDiat                         ! species dependent input
        real(kind = real_wp) :: Depth2, MinDepth2

        ! input state variables
        real(kind = real_wp) :: diatC, diatChl, diatN, diatP, diatSi        ! protist state variable

        ! auxiliaries
        real(kind = real_wp) :: PSed
        real(kind = real_wp) :: MaxSed_C, MaxSed_Chl, MaxSed_N, MaxSed_P, MaxSed_Si
        real(kind = real_wp) :: PotSed_C, PotSed_Chl, PotSed_N, PotSed_P, PotSed_Si


        !
        !*******************************************************************************
        !
        ! segment and species independent items
        nrSpec = nint(process_space_real(ipoint(1)))   !   nr of species in the interface                                 (-)

        !   nrInputItems  = nrIndInp + nrSpec * nrSpecInp = 6 + 2 * 7 = 20
        !   nrOutputItems = nrSpec * nrSpecOut = 2 * 6 = 12
        !   ipointLength = nrInputItems + nrOutputItems = 20 + 12 = 32
        !   nrFluxes  = nrSpec * (nrSpexFlx + nrPrey * nrLossFluxes) = 2 * 5 = 10

        ! length of the process_space_real input pointer array.
        nrInputItems = nrIndInp + nrSpec * nrSpecInp     ! only input on segments, not on exchanges!
        nrOutputItems = nrSpec * nrSpecOut               ! only output on segments, not on exchanges!
        ipointLength = nrInputItems + nrSpec + nrOutputItems + nrSpec ! includes one input and one output per species on exchanges

        allocate (ipnt(ipointLength))
        ipnt(1:ipointLength) = ipoint(1:ipointLength)
        iflux = 0

        ! segment and species independent items
        DELT = process_space_real(ipnt(2))         !   timestep for processes                                 (d)
        MinDepth = process_space_real(ipnt(3))         !   minimum waterdepth for sedimentation/resuspension      (m)
        TaucSDiat = process_space_real(ipnt(4))         !   critical shear stress for sedimentation Diatoms        (N/m2)

        ! length of the process_space_real input array (for segment and for exchange).
        ! first and second term = input for segment; third term input for exchange
        inpItems = nrIndInp + nrSpec * nrSpecInp

        ! segment loop
        do iseg = 1, num_cells

            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            if (ikmrk1==1) then
                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if ((ikmrk2==0).or.(ikmrk2==3)) then
                    ! species independent items
                    Tau = process_space_real(ipnt(5))  !    total bottom shear stress                              (N/m2)
                    Depth = process_space_real(ipnt(6))  !    depth of segment                                       (m)

                    ! species loop
                    do iSpec = 1, nrSpec

                        spInc = nrIndInp + (iSpec - 1) * nrSpecInp

                        ! species dependent items
                        ! (number of species independent items + location of input item in vector + species loop)
                        ! Protect against negativ values
                        diatC = max(0.0, process_space_real(ipnt(spInc + 1)))  !      C-biomass                                              (gC m-3)
                        diatChl = max(0.0, process_space_real(ipnt(spInc + 2)))  !      Chl-biomass                                            (gChl m-3)
                        diatN = max(0.0, process_space_real(ipnt(spInc + 3)))  !      N-biomass                                              (gN m-3)
                        diatP = max(0.0, process_space_real(ipnt(spInc + 4)))  !      P-biomass                                              (gP m-3)
                        diatSi = max(0.0, process_space_real(ipnt(spInc + 5)))  !      Si-biomass                                             (gSi m-3)
                        ZSedDiat = process_space_real(ipnt(spInc + 6))    !      zeroth-order sedimentation flux Diatoms                (gC/m2/d)
                        VSedDiat = max(0.0, process_space_real(ipnt(spInc + 7)))  !      sedimentation velocity Diatoms                         (m/d)

                        ! Calculate sedimentation probabality-------------------------------------------------------------------------------
                        ! Units: (-)
                        if (Tau == -1.0) then
                            PSed = 1.0
                        elseif (TaucSDiat < 1e-20)  then
                            PSed = 0.0
                        else
                            ! comapre with critical shear stress
                            PSed = max (0.0, (1.0 - Tau / TaucSDiat))
                        endif
                        ! changed PSedMin to 0.0
                        PSed = max(0.0, PSed)

                        ! Calculate potential sediment fluxes-------------------------------------------------------------------------------
                        ! Units:

                        spInc = iflux + nrLossFluxes * (iSpec - 1)

                        if (Depth < MinDepth) then
                            MaxSed_C = 0.0
                            MaxSed_Chl = 0.0
                            MaxSed_N = 0.0
                            MaxSed_P = 0.0
                            MaxSed_Si = 0.0

                            fl(spInc + 1) = 0.0
                            fl(spInc + 2) = 0.0
                            fl(spInc + 3) = 0.0
                            fl(spInc + 4) = 0.0
                            fl(spInc + 5) = 0.0
                        else
                            PotSed_C = ZSedDiat + (VSedDiat * diatC) * PSed
                            PotSed_Chl = ZSedDiat + (VSedDiat * diatChl) * PSed
                            PotSed_N = ZSedDiat + (VSedDiat * diatN) * PSed
                            PotSed_P = ZSedDiat + (VSedDiat * diatP) * PSed
                            PotSed_Si = ZSedDiat + (VSedDiat * diatSi) * PSed

                            ! limit sedimentation to available mass (m/l2/day)
                            MaxSed_C = min (PotSed_C, diatC / DELT * Depth)
                            MaxSed_Chl = min (PotSed_Chl, diatChl / DELT * Depth)
                            MaxSed_N = min (PotSed_N, diatN / DELT * Depth)
                            MaxSed_P = min (PotSed_P, diatP / DELT * Depth)
                            MaxSed_Si = min (PotSed_Si, diatSi / DELT * Depth)

                            ! convert sedimentation to flux
                            ! Units: m/l3/day
                            fl(spInc + 1) = MaxSed_C / Depth
                            fl(spInc + 2) = MaxSed_Chl / Depth
                            fl(spInc + 3) = MaxSed_N / Depth
                            fl(spInc + 4) = MaxSed_P / Depth
                            fl(spInc + 5) = MaxSed_Si / Depth
                        endif

                        ! Output -------------------------------------------------------------------
                        ! (input items + input exchange + position of specific output item in vector + species loop * total number of output)
                        spInc = inpItems + nrSpec + (iSpec - 1) * nrSpecOut

                        process_space_real(ipnt(spInc + 1)) = PSed
                        process_space_real(ipnt(spInc + 2)) = MaxSed_C
                        process_space_real(ipnt(spInc + 3)) = MaxSed_Chl
                        process_space_real(ipnt(spInc + 4)) = MaxSed_N
                        process_space_real(ipnt(spInc + 5)) = MaxSed_P
                        process_space_real(ipnt(spInc + 6)) = MaxSed_Si

                    enddo ! end loop over species

                endif ! end if check for dry cell

            endif ! end if check for dry cell

            !allocate pointers
            iflux = iflux + noflux
            ipnt(1:ipointLength) = ipnt(1:ipointLength) + increm(1:ipointLength)

        enddo ! end loop over segments

        ipnt(1:ipointLength) = ipoint(1:ipointLength)
        ! Exchange loop over horizontal direction -------------------------------------------------------------------
        do ioq = 1, num_exchanges_u_dir + num_exchanges_v_dir
            do iSpec = 1, nrSpec
                ! input+ input exchange + output + exchange + loop over species
                process_space_real(ipnt(inpItems + nrSpec + nrSpec * nrSpecOut + iSpec)) = 0.0
            enddo
            ipnt(1:ipointLength) = ipnt(1:ipointLength) + increm(1:ipointLength)
        enddo

        ! Exchange loop over vertical direction -------------------------------------------------------------------
        do ioq = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir
            i_origin = iexpnt(1, ioq)
            i_dest = iexpnt(2, ioq)

            if (i_origin > 0 .and. i_dest > 0) then
                ! find first index of the origin and destination segment
                call extract_waq_attribute(1, iknmrk(i_origin), ikmrkv)
                call extract_waq_attribute(1, iknmrk(i_dest), ikmrkn)
                if (ikmrkv==1 .and. ikmrkn==1) then
                    ! water-water exchange
                    ! convert value from m/d to m/s
                    Depth = process_space_real(ipnt(6) + (i_origin - 1) * increm(6))
                    Depth2 = process_space_real(ipnt(6) + (i_dest - 1) * increm(6))
                    MinDepth = process_space_real(ipnt(3) + (i_origin - 1) * increm(3))
                    MinDepth2 = process_space_real(ipnt(3) + (i_dest - 1) * increm(3))

                    do iSpec = 1, nrSpec

                        if (Depth > MinDepth .and. Depth2 > MinDepth2) then
                            process_space_real(ipnt(inpItems + nrSpec + nrSpec * nrSpecOut + iSpec)) = process_space_real(ipnt(inpItems + iSpec)) / numSecPerDay
                        else
                            process_space_real(ipnt(inpItems + nrSpec + nrSpec * nrSpecOut + iSpec)) = 0.0
                        endif ! end check if min depth large enough

                    enddo ! end loop over species
                endif ! end check water - water
            endif ! end check boundaries
            ipnt(1:ipointLength) = ipnt(1:ipointLength) + increm(1:ipointLength)
        enddo ! end loop of vertical exchange
        deallocate (ipnt)
        return
    end
    ! end subroutine

end module m_protistdiatsedi
