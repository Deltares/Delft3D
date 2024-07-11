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
module m_veg2dn
    use m_waq_precision

    implicit none

contains


    subroutine veg2dn     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute


        ! function determine nutrient availability for vegetation

        implicit none

        ! arguments          i/o description

        real(kind = real_wp) :: process_space_real(*)      !i/o process manager system array, window of routine to process library
        real(kind = real_wp) :: fl(*)        ! o  array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(*)    ! i  array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(*)    ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells        ! i  number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux       ! i  number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *)  ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)    ! i  active-inactive, surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir         ! i  nr of exchanges in 1st direction, only horizontal dir if irregular mesh
        integer(kind = int_wp) :: num_exchanges_v_dir         ! i  nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir         ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir         ! i  nr of exchanges in the bottom (bottom layers, specialist use only)

        ! from process_space_real array

        real(kind = real_wp) :: depth        ! i  depth of segment                               (m)
        real(kind = real_wp) :: totaldepth   ! i  total depth water column                       (m)
        real(kind = real_wp) :: localdepth   ! i  depth from water surface to bottom of segment  (m)
        real(kind = real_wp) :: volume       ! i  volume                                        (m3)
        real(kind = real_wp) :: surf         ! i  surf                                          (m2)
        real(kind = real_wp) :: hmax         ! i  maxmimum length roots                          (m)
        real(kind = real_wp) :: nh4          ! i  nh4                                         (g/m3)
        real(kind = real_wp) :: aap          ! i  aap                                         (g/m3)
        real(kind = real_wp) :: so4          ! i  so4                                         (g/m3)
        real(kind = real_wp) :: no3          ! i  no3                                         (g/m3)
        real(kind = real_wp) :: po4          ! i  po4                                         (g/m3)
        real(kind = real_wp) :: sud          ! i  sud                                         (g/m3)
        real(kind = real_wp) :: s1_nh4       ! i  nh4 in sediment                             (g/m2)
        real(kind = real_wp) :: s1_aap       ! i  aap in sediment                             (g/m2)
        real(kind = real_wp) :: s1_so4       ! i  so4 in sediment                             (g/m2)
        real(kind = real_wp) :: s1_no3       ! i  no3 in sediment                             (g/m2)
        real(kind = real_wp) :: s1_po4       ! i  po4 in sediment                             (g/m2)
        real(kind = real_wp) :: s1_sud       ! i  sud in sediment                             (g/m2)
        real(kind = real_wp) :: SWRoot       ! I  RootShootModel(y=1,n=0) for F2VB F4VB       (-)
        real(kind = real_wp) :: Vmax         ! I  maximun rate in Michelis/Menten             (-)
        real(kind = real_wp) :: Km           ! I  TIN conc. at half of Vmax                  (gN/m3)
        real(kind = real_wp) :: Vini         ! I  initial rate in Michelis/Menten             (-)
        real(kind = real_wp) :: Poros        ! I  Porosity                                    (-)
        real(kind = real_wp) :: hsed         ! I  sediment layer thickness                    (m)

        real(kind = real_wp) :: vbxxnavail   ! o  available nitrogen                          (g/m2)
        real(kind = real_wp) :: vbxxpavail   ! o  available p                                 (g/m2)
        real(kind = real_wp) :: vbxxsavail   ! o  available s                                 (g/m2)
        real(kind = real_wp) :: porevol      ! o  pore water volume                           (m3)
        real(kind = real_wp) :: F1VB         ! o  allocation factor comp. 1 (stem)             (-)
        real(kind = real_wp) :: F2VB         ! o  allocation factor comp. 2 (foliage)          (-)
        real(kind = real_wp) :: F3VB         ! o  allocation factor comp. 3 (branch)           (-)
        real(kind = real_wp) :: F4VB         ! o  allocation factor comp. 4 (root)             (-)
        real(kind = real_wp) :: F5VB         ! o  allocation factor comp. 5 (fine root)        (-)

        ! local declarations

        integer(kind = int_wp) :: iseg         !    local loop counter for computational element loop
        real(kind = real_wp) :: z2           !    height bottom segment from bottom              (m)
        real(kind = real_wp) :: z1           !    height top segment from bottom                 (m)
        integer(kind = int_wp) :: ikmrk1
        integer ikmrk2
        real(kind = real_wp) :: zm           !    watersurface to top macropyte                  (-)
        real(kind = real_wp) :: frlay        !    fraction witin layer                           (-)
        integer(kind = int_wp) :: iq           !    loop counter
        integer(kind = int_wp) :: ifrom        !    from segment
        integer(kind = int_wp) :: ito          !    from segment
        integer(kind = int_wp) :: iflux        !    index in the fl array

        integer(kind = int_wp), parameter :: npnt = 34         ! number of pointers
        integer(kind = int_wp), parameter :: ioffout = npnt - 9  ! offset for output parameters
        integer(kind = int_wp) :: ipnt(npnt)           ! local work array for the pointering
        integer(kind = int_wp) :: ipb
        integer(kind = int_wp) :: ibotseg              ! bottom segment for macrophyte

        real(kind = real_wp) :: TIN          !    nh4+no2 conc.                                 (g/m3)
        real(kind = real_wp) :: porewater    !    pore water volume                            (m3)
        real(kind = real_wp) :: fsurf        !    auxiliary factor                             (m2)
        ! zero the pool for all segments

        ipnt = ipoint(1:npnt)
        do iseg = 1, num_cells
            process_space_real(ipnt(ioffout + 1)) = 0.0
            process_space_real(ipnt(ioffout + 2)) = 0.0
            process_space_real(ipnt(ioffout + 3)) = 0.0
            process_space_real(ipnt(ioffout + 4)) = 0.0
            process_space_real(ipnt(ioffout + 5)) = 0.0
            process_space_real(ipnt(ioffout + 6)) = 0.0  ! constant value for no vegetation cells
            process_space_real(ipnt(ioffout + 7)) = 0.0
            process_space_real(ipnt(ioffout + 8)) = 0.0
            process_space_real(ipnt(ioffout + 9)) = 0.0  ! constant value for no vegetation cells
            ipnt = ipnt + increm(1:npnt)
        enddo

        ! accumulate mass in the rooting zone in the pool of the bottom segment

        !allocate(poreVol(num_cells),stat=ierr_alloc)

        ipnt = ipoint(1:npnt)
        do iseg = 1, num_cells

            depth = process_space_real(ipnt(1))
            totaldepth = process_space_real(ipnt(2))
            localdepth = process_space_real(ipnt(3))
            volume = process_space_real(ipnt(4))
            surf = process_space_real(ipnt(5))
            ibotseg = NINT(process_space_real(ipnt(6)))
            hmax = process_space_real(ipnt(7))
            nh4 = process_space_real(ipnt(8))
            no3 = process_space_real(ipnt(9))
            aap = process_space_real(ipnt(10))
            po4 = process_space_real(ipnt(11))
            so4 = process_space_real(ipnt(12))
            sud = process_space_real(ipnt(13))
            SWRoot = process_space_real(ipnt(14))
            Vmax = process_space_real(ipnt(15))
            Km = process_space_real(ipnt(16))
            Vini = process_space_real(ipnt(17))
            Poros = process_space_real(ipnt(18))
            s1_nh4 = process_space_real(ipnt(19))
            s1_no3 = process_space_real(ipnt(20))
            s1_aap = process_space_real(ipnt(21))
            s1_po4 = process_space_real(ipnt(22))
            s1_so4 = process_space_real(ipnt(23))
            s1_sud = process_space_real(ipnt(24))
            hsed = process_space_real(ipnt(25))

            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
            if (ikmrk1<3) then ! also when dry!

                ! active water segment

                if (hmax > 0.0) then

                    ! in, partly in or out of the active zone

                    hmax = min(hmax, totaldepth)
                    zm = totaldepth - hmax
                    z1 = localdepth - depth
                    z2 = localdepth

                    if (zm > z2) then
                        ! not in segment:

                    elseif (zm < z1) then
                        ! partialy in segment:
                        frlay = (z2 - zm) / depth
                        ipb = ipoint(ioffout + 1) + (ibotseg - 1) * increm(ioffout + 1)
                        process_space_real(ipb) = process_space_real(ipb) + (nh4 + no3) * volume * frlay

                        ipb = ipoint(ioffout + 2) + (ibotseg - 1) * increm(ioffout + 2)
                        process_space_real(ipb) = process_space_real(ipb) + (aap + po4) * volume * frlay

                        ipb = ipoint(ioffout + 3) + (ibotseg - 1) * increm(ioffout + 3)
                        process_space_real(ipb) = process_space_real(ipb) + (so4 + sud) * volume * frlay

                        ipb = ipoint(ioffout + 4) + (ibotseg - 1) * increm(ioffout + 4)
                        process_space_real(ipb) = process_space_real(ipb) + volume * frlay * Poros
                    else
                        ! completely in segment:
                        ipb = ipoint(ioffout + 1) + (ibotseg - 1) * increm(ioffout + 1)
                        process_space_real(ipb) = process_space_real(ipb) + (nh4 + no3) * volume

                        ipb = ipoint(ioffout + 2) + (ibotseg - 1) * increm(ioffout + 2)
                        process_space_real(ipb) = process_space_real(ipb) + (aap + po4) * volume

                        ipb = ipoint(ioffout + 3) + (ibotseg - 1) * increm(ioffout + 3)
                        process_space_real(ipb) = process_space_real(ipb) + (so4 + sud) * volume

                        ipb = ipoint(ioffout + 4) + (ibotseg - 1) * increm(ioffout + 4)
                        process_space_real(ipb) = process_space_real(ipb) + volume * Poros
                    endif

                endif

                !
                ! Alternative layered sediment approach - assumption: fairly well-mixed sediment layer,
                ! as the DELWAQG module does not export information on the profile.
                !
                if (ikmrk2 == 0 .or. ikmrk2 == 3) then
                    fsurf = min(1.0, -hmax / hsed) * surf

                    ipb = ipoint(ioffout + 1) + (iseg - 1) * increm(ioffout + 1)
                    process_space_real(ipb) = process_space_real(ipb) + (s1_nh4 + s1_no3) * fsurf

                    ipb = ipoint(ioffout + 2) + (iseg - 1) * increm(ioffout + 2)
                    process_space_real(ipb) = process_space_real(ipb) + (s1_aap + s1_po4) * fsurf

                    ipb = ipoint(ioffout + 3) + (iseg - 1) * increm(ioffout + 3)
                    process_space_real(ipb) = process_space_real(ipb) + (s1_so4 + s1_sud) * fsurf

                    ipb = ipoint(ioffout + 4) + (iseg - 1) * increm(ioffout + 4)
                    process_space_real(ipb) = process_space_real(ipb) + volume * Poros
                endif

            elseif (ikmrk1==3) then

                ! delwaq-g segment

                if (hmax < 0.0) then

                    ! distribution over the bottom segments

                    hmax = -hmax
                    hmax = min(hmax, totaldepth)
                    z1 = localdepth - depth

                    if (hmax > localdepth) then
                        ! completely in segment:
                        ipb = ipoint(ioffout + 1) + (iseg - 1) * increm(ioffout + 1)
                        process_space_real(ipb) = process_space_real(ipb) + (nh4 + no3) * volume

                        ipb = ipoint(ioffout + 2) + (iseg - 1) * increm(ioffout + 2)
                        process_space_real(ipb) = process_space_real(ipb) + (aap + po4) * volume

                        ipb = ipoint(ioffout + 3) + (iseg - 1) * increm(ioffout + 3)
                        process_space_real(ipb) = process_space_real(ipb) + (so4 + sud) * volume

                        ipb = ipoint(ioffout + 4) + (iseg - 1) * increm(ioffout + 4)
                        process_space_real(ipb) = process_space_real(ipb) + volume * Poros
                    elseif (hmax > z1) then
                        ! partialy in segment:
                        frlay = (hmax - z1) / depth

                        ipb = ipoint(ioffout + 1) + (iseg - 1) * increm(ioffout + 1)
                        process_space_real(ipb) = process_space_real(ipb) + (nh4 + no3) * volume * frlay

                        ipb = ipoint(ioffout + 2) + (iseg - 1) * increm(ioffout + 2)
                        process_space_real(ipb) = process_space_real(ipb) + (aap + po4) * volume * frlay

                        ipb = ipoint(ioffout + 3) + (iseg - 1) * increm(ioffout + 3)
                        process_space_real(ipb) = process_space_real(ipb) + (so4 + sud) * volume * frlay

                        ipb = ipoint(ioffout + 4) + (iseg - 1) * increm(ioffout + 4)
                        process_space_real(ipb) = process_space_real(ipb) + volume * frlay * Poros
                    else
                        ! not in segment:
                    endif

                endif

            endif

            ipnt = ipnt + increm(1:npnt)

        enddo

        ! express the available pool as g/m2

        ipnt = ipoint(1:npnt)
        do iseg = 1, num_cells
            ibotseg = NINT(process_space_real(ipnt(6)))
            if (ibotseg == iseg) then
                surf = process_space_real(ipnt(5))
                process_space_real(ipnt(ioffout + 1)) = process_space_real(ipnt(ioffout + 1)) / surf
                process_space_real(ipnt(ioffout + 2)) = process_space_real(ipnt(ioffout + 2)) / surf
                process_space_real(ipnt(ioffout + 3)) = process_space_real(ipnt(ioffout + 3)) / surf
                ! RootShoot Model using the Michelis-Menten eq.
                if (Nint(SWRoot) == 1) then
                    porewater = process_space_real(ipnt(ioffout + 4))
                    ! express the availeble nitrogen conc in sediment as g/m3
                    if (porewater > 1.0e-10) then
                        TIN = process_space_real(ipnt(ioffout + 1)) * surf / porewater
                    else
                        TIN = 0.0
                    endif

                    process_space_real(ipnt(ioffout + 5)) = 0.0
                    process_space_real(ipnt(ioffout + 6)) = Vini + (Vmax * TIN) / (Km + TIN)
                    process_space_real(ipnt(ioffout + 7)) = 0.0
                    process_space_real(ipnt(ioffout + 8)) = 0.0
                    process_space_real(ipnt(ioffout + 9)) = 1.0 - process_space_real(ipnt(ioffout + 6))
                endif
            else
                ! Fill all bottom sediment colume
                if (Nint(SWRoot) == 1) then
                    process_space_real(ipnt(ioffout + 5)) = 0.0
                    process_space_real(ipnt(ioffout + 6)) = process_space_real(ipoint(ioffout + 6) + (ibotseg - 1) * increm(ioffout + 6))
                    process_space_real(ipnt(ioffout + 7)) = 0.0
                    process_space_real(ipnt(ioffout + 8)) = 0.0
                    process_space_real(ipnt(ioffout + 9)) = process_space_real(ipoint(ioffout + 9) + (ibotseg - 1) * increm(ioffout + 9))
                endif
            endif
            ipnt = ipnt + increm(1:npnt)
        enddo

        ! in order to avoid the error message from vbupt
        ! the switches should be checked
        if (Nint(SWRoot) == 1) then
            ipnt = ipoint(1:npnt)
            do iseg = 1, num_cells
                if (process_space_real(ipnt(ioffout + 6)) + process_space_real(ipnt(ioffout + 9)) < 2.E-10) then
                    process_space_real(ipnt(ioffout + 5)) = 0.0
                    process_space_real(ipnt(ioffout + 6)) = 0.5
                    process_space_real(ipnt(ioffout + 7)) = 0.0
                    process_space_real(ipnt(ioffout + 8)) = 0.0
                    process_space_real(ipnt(ioffout + 9)) = 0.5
                endif
                ipnt = ipnt + increm(1:npnt)
            enddo
        endif

        return
    end

end module m_veg2dn
