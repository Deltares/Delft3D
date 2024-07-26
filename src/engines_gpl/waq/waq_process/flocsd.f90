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
module m_flocsd
    use m_waq_precision

    implicit none

contains


    subroutine flocsd     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !
        !*******************************************************************************
        !
        use m_extract_waq_attribute
        use flocculation_dwq

        implicit none
        !
        !     type    name         i/o description
        !
        integer(kind = int_wp), parameter :: NOPMSA = 20

        real(kind = real_wp) :: process_space_real(*)        !i/o process manager system array, window of routine to process library
        real(kind = real_wp) :: fl(*)          ! o  array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(NOPMSA) ! i  array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(NOPMSA) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells          ! i  number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux         ! i  number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *)    ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)      ! i  active-inactive, surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir           ! i  nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir           ! i  nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir           ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir           ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(NOPMSA)   ! local work array for the pointering
        integer(kind = int_wp) :: iseg           ! local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     type    name         i/o description                                        unit
        !
        integer(kind = int_wp) :: idflocim1
        integer(kind = int_wp) :: idflocim2

        integer(kind = int_wp) :: ip17, in17, ip18, in18, ipwmac, inwmac, ipwmic, inwmic, iq, num_exchanges, ivan

        real(kind = real_wp) :: cmacro      ! i  inorganic matter (im1; macro flocs)                (gdm/m3)
        real(kind = real_wp) :: cmicro      ! i  inorganic matter (im2; micro flocs)                (gdm/m3)
        real(kind = real_wp) :: tpm         ! i  total particulate matter (including algae)         (gdw/m3)
        real(kind = real_wp) :: tau         ! i  bottom shear stress                                (Pa)
        integer(kind = int_wp) :: swfloform   ! i  1=Manning/Dyer, 2=Chassagne/Safar, 3=NA            (-)
        real(kind = real_wp) :: rcfloc      ! i  flocculation rate                                  (1/d)
        real(kind = real_wp) :: rcbreakup   ! i  floc break-up rate                                 (1/d)
        real(kind = real_wp) :: delt        ! i  timestep for processes                             (d)
        real(kind = real_wp) :: total_depth ! i  total depth for segment (bottom to surface)        (m)
        real(kind = real_wp) :: local_depth ! i  local depth for segment (segment to surface)       (m)
        real(kind = real_wp) :: rho_water   ! i  density of water                                   (kg/m3)
        real(kind = real_wp) :: viscosity   ! i  kinetic viscosity                                  (m/s2)
        real(kind = real_wp) :: spmratioem  ! o  flocculation ratio macro:micro empirical model     (-)
        real(kind = real_wp) :: dflocim1    ! f  flocculation or break-up flux im1                  (g/m3/d)
        real(kind = real_wp) :: dflocim2    ! f  flocculation or break-up flux im2                  (g/m3/d)
        real(kind = real_wp) :: d_micro     ! i  characteristic diameter of micro flocs             (m)
        real(kind = real_wp) :: ustar_macro ! i  characteristic shear velocity of macro flocs       (m/s)
        integer(kind = int_wp) :: ikmrk1    !    first segment attribute
        logical active      !    active segment
        logical bottom      !    sediment bed segment
        real(kind = real_wp) :: macro       !    concentration macro flocs                            (g/m3)
        real(kind = real_wp) :: micro       !    concentration micro flocs                            (g/m3)
        real(kind = real_wp) :: tim         !    total concentration flocs                            (g/m3)
        real(kind = real_wp) :: macroeq     !    concentration macro flocs in equilibrium             (g/m3)
        real(kind = real_wp) :: dfloc       !    flocculation or break-up flux                      (g/m3/d)
        real(kind = real_wp) :: ws_macro    !    fall velocity of macro flocs                       (m/d)
        real(kind = real_wp) :: ws_micro    !    fall velocity of micro flocs                       (m/d)
        !
        !*******************************************************************************
        !
        ipnt = ipoint
        idflocim1 = 1
        idflocim2 = 2

        do iseg = 1, num_cells

            cmacro = process_space_real(ipnt(1))    ! IM1
            cmicro = process_space_real(ipnt(2))    ! IM2
            tpm = process_space_real(ipnt(3))
            tau = process_space_real(ipnt(4))
            swfloform = process_space_real(ipnt(5))
            rcfloc = process_space_real(ipnt(6))
            rcbreakup = process_space_real(ipnt(7))
            rho_water = process_space_real(ipnt(8))
            viscosity = process_space_real(ipnt(9))
            delt = process_space_real(ipnt(10))
            total_depth = process_space_real(ipnt(11))
            local_depth = process_space_real(ipnt(12)) - 0.5 * process_space_real( ipnt( 13) )  ! The "average" depth of the segment,
                                                                    ! not the bottom level
            d_micro     = process_space_real(ipnt(14))
            ustar_macro = process_space_real(ipnt(15))

            ! only for active water segments

            active = btest(iknmrk(iseg), 0)
            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            bottom = ikmrk1==3
            if (active .and. .not. bottom) then
                call flocculate_dwq(swfloform, cmacro, cmicro, tpm, tau, total_depth, local_depth, viscosity, rho_water, &
                         d_micro, ustar_macro, spmratioem, ws_macro, ws_micro)


                ! calculate flocculatio/break-up flux and restrict flux to 50% in one timestep for stability

                tim = cmacro + cmicro
                macroeq = spmratioem * tim / (1. + spmratioem)
                if (macroeq > cmacro) then
                    dfloc = (macroeq - cmacro) * rcfloc
                    dfloc = min(dfloc, 0.5 * cmicro / delt)
                else
                    dfloc = (macroeq - cmacro) * rcbreakup
                    dfloc = max(dfloc, -0.5 * cmacro / delt)
                endif

            else

                spmratioem = -999.
                dfloc = 0.0

            endif

            ! pass values back to the system

            fl  (idflocim1) = dfloc
            fl  (idflocim2) = -dfloc
            process_space_real(ipnt(16)) = spmratioem
            process_space_real(ipnt(17)) = ws_macro
            process_space_real(ipnt(18)) = ws_micro

            idflocim1 = idflocim1 + noflux
            idflocim2 = idflocim2 + noflux
            ipnt = ipnt + increm

        enddo

        !
        ! Now fill in the fall velocities
        !
        num_exchanges = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

        ipwmac = ipoint(19)
        inwmac = increm(19)
        ipwmic = ipoint(20)
        inwmic = increm(20)

        ip17 = ipoint(17)
        ip18 = ipoint(18)
        in17 = increm(17)
        in18 = increm(18)
        !
        ! Horizontal exchanges - set to zero
        !
        do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir

            process_space_real(ipwmac) = 0.0
            process_space_real(ipwmic) = 0.0

            ipwmac = ipwmac + inwmac
            ipwmic = ipwmic + inwmic

        enddo

        do iq = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges
 
            ivan = iexpnt(1,iq)
!           
!           sedimentation velocity from segment to exchange-area
!           
            if ( ivan > 0 ) then
                ip17 = ipoint(17) + (ivan-1) * in17
                ip18 = ipoint(18) + (ivan-1) * in18
                process_space_real(ipwmac) = process_space_real( ip17 )
                process_space_real(ipwmic) = process_space_real( ip18 )
            endif
            
            ipwmac = ipwmac + inwmac
            ipwmic = ipwmic + inwmic

        enddo

    end subroutine flocsd

end module m_flocsd
