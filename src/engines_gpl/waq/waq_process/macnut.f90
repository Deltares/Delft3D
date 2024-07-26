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
module m_macnut
    use m_waq_precision

    implicit none

contains


    subroutine MACNUT     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !
        !*******************************************************************************
        !
        implicit none
        !
        !     type    name         i/o description
        !
        real(kind = real_wp) :: process_space_real(*)     !i/o process manager system array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! o  array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(45) ! i  array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(45) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! i  number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! i  number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! i  nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! i  nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(45)   !    local work array for the pointering
        integer(kind = int_wp) :: iseg        !    local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     type    name         i/o description                                        unit
        !
        real(kind = real_wp) :: depth       ! i  depth of segment                                   (m)
        real(kind = real_wp) :: totaldepth  ! i  total depth water column                           (m)
        real(kind = real_wp) :: locseddept  ! i  sediment layer depth to bottom of segment          (m)
        real(kind = real_wp) :: po4s12      ! i  concentration of PO4 in S12 bottom (pores)         (g/m3)
        real(kind = real_wp) :: nh4s12      ! i  concentration of NH4 in S12 bottom (pores)         (g/m3)
        integer(kind = int_wp) :: ibotseg     ! i  bottom segment number                              (-)
        real(kind = real_wp) :: frbmlay     ! i  fraction in layer of SM biomass in column          (-)
        real(kind = real_wp) :: rootdesm01  ! i  rooting depth sm01                                 (m)
        real(kind = real_wp) :: poros       ! i  volumetric porosity                                (-)
        real(kind = real_wp) :: nh4         ! i  ammonium (nh4)                                     (gn/m3)
        real(kind = real_wp) :: no3         ! i  nitrate (no3)                                      (gn/m3)
        real(kind = real_wp) :: po4         ! i  ortho-phosphate (po4)                              (gp/m3)
        real(kind = real_wp) :: disco2      ! i  concentration of dissolved carbon dioxide          (g/m3)
        real(kind = real_wp) :: dish2co3    ! i  concentration of dissolved true h2co3              (gc/m3)
        real(kind = real_wp) :: dishco3     ! i  concentration of dissolved hco3(-)                 (gc/m3)
        real(kind = real_wp) :: prfnh4sm01  ! i  ammonium preferency over nitrate sm01              (-)
        real(kind = real_wp) :: kmdinsm01w  ! i  half-saturation value n sm01 in water              (gn/m3)
        real(kind = real_wp) :: kmpsm01w    ! i  half-saturation value p sm01 in water              (gp/m3)
        real(kind = real_wp) :: kmco2sm01   ! i  half-saturation value co2 + h2c03 sm01             (gc/m3)
        real(kind = real_wp) :: kmhco3sm01  ! i  half-saturation value hc03 sm01                    (gc/m3)
        real(kind = real_wp) :: kmdinsm01b  ! i  half-saturation value n sm01 in bottom             (gn/m3)
        real(kind = real_wp) :: kmpsm01b    ! i  half-saturation value p sm01 in bottom             (gp/m3)
        real(kind = real_wp) :: temp        ! i  ambient water temperature                         (oC)
        real(kind = real_wp) :: limnsm01w   ! o  nitrogen limitation function sm01 <0-1> water      (-)
        real(kind = real_wp) :: limpsm01w   ! o  phosphorus limitation function sm01 <0-1> water    (-)
        real(kind = real_wp) :: lco2sm01    ! o  co2+h2c03 limitation function sm01 <0-1>           (-)
        real(kind = real_wp) :: limnsm01b   ! o  nitrogen limitation function sm01 <0-1> bottom     (-)
        real(kind = real_wp) :: limpsm01b   ! o  phosphorus limitation function sm01 <0-1>bottom    (-)
        real(kind = real_wp) :: limnutsm01  ! o  nutrient limitation function sm01 <0-1>            (-)
        real(kind = real_wp) :: frootuptn   ! o  fraction root uptake nitrogen sm01                 (-)
        real(kind = real_wp) :: frootuptp   ! o  fraction root uptake phosphorus                    (-)
        real(kind = real_wp) :: cdinsm01w   ! o  average water concentration din for sm01           (m)
        real(kind = real_wp) :: cpo4sm01w   ! o  average water concentration po4 for sm01           (m)
        real(kind = real_wp) :: cco2sm01    ! o  average concentration co2+h2co3 for sm01           (m)
        real(kind = real_wp) :: chco3sm01   ! o  average concentration hco3 for sm01                (m)
        real(kind = real_wp) :: cdinsm01b   ! o  average sediment concentration din for sm01        (m)
        real(kind = real_wp) :: cpo4sm01b   ! o  average sediment concentration po4 for sm01        (m)

        ! local

        integer(kind = int_wp) :: ikmrk1      !    first attribute
        integer(kind = int_wp) :: ikmrk2      !    second attribute
        real(kind = real_wp) :: din         ! l  dissolved inorganic nitrogen, corrected for preference
        real(kind = real_wp) :: z1          ! l  z1
        real(kind = real_wp) :: fr_avg      ! l  fr_avg
        real(kind = real_wp) :: hroot       ! l  hroot
        real(kind = real_wp) :: limn        ! l  limn
        real(kind = real_wp) :: limp        ! l  limp

        ! zero the average concentrations for all segments

        ipnt = ipoint
        do iseg = 1, num_cells
            process_space_real(ipnt(39)) = 0.0
            process_space_real(ipnt(40)) = 0.0
            process_space_real(ipnt(41)) = 0.0
            process_space_real(ipnt(42)) = 0.0
            process_space_real(ipnt(43)) = 0.0
            process_space_real(ipnt(44)) = 0.0
            process_space_real(ipnt(45)) = 0.0
            ipnt = ipnt + increm
        enddo

        ! first loop average concentration over height and rooting depth

        ipnt = ipoint
        do iseg = 1, num_cells

            depth = process_space_real(ipnt(1))
            totaldepth = process_space_real(ipnt(2))
            locseddept = process_space_real(ipnt(4))
            nh4s12 = process_space_real(ipnt(5))
            po4s12 = process_space_real(ipnt(6))
            ibotseg = nint(process_space_real(ipnt(7)))
            FrBmLay = process_space_real(ipnt(8))
            rootdesm01 = process_space_real(ipnt(9))
            poros = process_space_real(ipnt(10))
            nh4 = max(process_space_real(ipnt(11)), 0.0)
            no3 = max(process_space_real(ipnt(12)), 0.0)
            po4 = max(process_space_real(ipnt(13)), 0.0)
            disco2 = max(process_space_real(ipnt(14)), 0.0)
            dish2co3 = max(process_space_real(ipnt(15)), 0.0)
            dishco3 = max(process_space_real(ipnt(16)), 0.0)
            prfnh4sm01 = process_space_real(ipnt(17))
            temp = process_space_real(ipnt(30))

            ! convert co2 to carbon and add h2co3, calculated din with preference
            ! adjust for porosity

            din = (nh4 + no3 / prfnh4sm01) / poros
            po4 = po4 / poros
            disco2 = (disco2 * 12. / 44. + dish2co3) / poros
            dishco3 = dishco3 / poros

            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            if (ikmrk1==1) then

                ! active water segment

                fr_avg = FrBmLay

                process_space_real(botidx(39)) = process_space_real(botidx(39)) + din * fr_avg
                process_space_real(botidx(40)) = process_space_real(botidx(40)) + po4 * fr_avg
                process_space_real(botidx(41)) = process_space_real(botidx(41)) + disco2 * fr_avg
                process_space_real(botidx(42)) = process_space_real(botidx(42)) + dishco3 * fr_avg
                process_space_real(botidx(45)) = process_space_real(botidx(45)) + temp * fr_avg

                ! S12 sediment concentration

                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if ((ikmrk2==0).or.(ikmrk2==3)) then
                    if (nh4s12>0.0) process_space_real(botidx(43)) = nh4s12
                    if (po4s12>0.0) process_space_real(botidx(44)) = po4s12
                endif

            elseif (ikmrk1==3) then

                ! sediment bed segment, distribution of roots in bed

                hroot = min(rootdesm01, totaldepth)
                z1 = locseddept - depth

                if (hroot > locseddept) then
                    ! completely in segment:
                    fr_avg = min(1.0, depth / hroot)
                elseif (hroot > z1) then
                    ! partialy in segment:
                    fr_avg = (hroot - z1) / hroot
                else
                    ! not in segment:
                    fr_avg = 0.0
                endif

                process_space_real(botidx(43)) = process_space_real(botidx(43)) + din * fr_avg
                process_space_real(botidx(44)) = process_space_real(botidx(44)) + po4 * fr_avg
                process_space_real(botidx(45)) = process_space_real(botidx(45)) + temp * fr_avg

            endif

            ipnt = ipnt + increm

        enddo

        ! second loop limiting factors and water / sediment uptake ratio

        ipnt = ipoint
        do iseg = 1, num_cells

            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            if (ikmrk1==1) then
                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if ((ikmrk2==0).or.(ikmrk2==3)) then

                    kmdinsm01w = process_space_real(ipnt(18))
                    kmpsm01w = process_space_real(ipnt(19))
                    kmco2sm01 = process_space_real(ipnt(20))
                    kmhco3sm01 = process_space_real(ipnt(21))
                    kmdinsm01b = process_space_real(ipnt(22))
                    kmpsm01b = process_space_real(ipnt(23))
                    cdinsm01w = process_space_real(ipnt(24))
                    cpo4sm01w = process_space_real(ipnt(25))
                    cco2sm01 = process_space_real(ipnt(26))
                    chco3sm01 = process_space_real(ipnt(27))
                    cdinsm01b = process_space_real(ipnt(28))
                    cpo4sm01b = process_space_real(ipnt(29))

                    ! n limitation

                    if (kmdinsm01b < 1e-20) then
                        ! only in water
                        limnsm01w = cdinsm01w / (cdinsm01w + kmdinsm01w)
                        limnsm01b = -1.
                    else
                        if (kmdinsm01w < 1e-20) then
                            ! only in bottom
                            limnsm01b = cdinsm01b / (cdinsm01b + kmdinsm01b)
                            limnsm01w = -1.
                        else
                            ! both
                            limnsm01w = cdinsm01w / (cdinsm01w + kmdinsm01w)
                            limnsm01b = cdinsm01b / (cdinsm01b + kmdinsm01b)
                        endif
                    endif
                    limn = max(limnsm01w, limnsm01b)
                    if (cdinsm01w > 1e-10) then
                        if (cdinsm01b > 1e-10) then
                            frootuptn = .998 / (1. + 2.66 * (cdinsm01b / cdinsm01w)**(-0.83))
                        else
                            frootuptn = 0.0
                        endif
                    else
                        frootuptn = 1.0
                    endif

                    ! p limitation

                    if (kmpsm01b < 1e-20) then
                        ! only in water
                        limpsm01w = cpo4sm01w / (cpo4sm01w + kmpsm01w)
                        limpsm01b = -1.
                    else
                        if (kmpsm01w < 1e-20) then
                            ! only in bottom
                            limpsm01b = cpo4sm01b / (cpo4sm01b + kmpsm01b)
                            limpsm01w = -1.
                        else
                            ! both
                            limpsm01w = cpo4sm01w / (cpo4sm01w + kmpsm01w)
                            limpsm01b = cpo4sm01b / (cpo4sm01b + kmpsm01b)
                        endif
                    endif
                    limp = max(limpsm01w, limpsm01b)
                    if (cpo4sm01w > 1e-10) then
                        if (cpo4sm01b > 1e-10) then
                            frootuptp = .998 / (1. + 2.66 * (cpo4sm01b / cpo4sm01w)**(-0.83))
                        else
                            frootuptp = 0.0
                        endif
                    else
                        frootuptp = 1.0
                    endif

                    ! c limitation

                    if (kmco2sm01 < 1e-20) then
                        if (kmhco3sm01 < 1e-20) then
                            ! no c limitation calculation
                            lco2sm01 = 1.0
                        else
                            ! only hco3 limitation calculatation
                            lco2sm01 = chco3sm01 / (chco3sm01 + kmhco3sm01)
                        endif
                    else
                        if (kmhco3sm01 < 1e-20) then
                            ! only co2 limitation calculatation
                            lco2sm01 = cco2sm01 / (cco2sm01 + kmco2sm01)
                        else
                            ! both co2 and hco3 limitation calculatation, take the max
                            lco2sm01 = max(cco2sm01 / (cco2sm01 + kmco2sm01), chco3sm01 / (chco3sm01 + kmhco3sm01))
                        endif
                    endif

                    ! overall limitation is minimum of n, p and c

                    limnutsm01 = min(limn, limp, lco2sm01)

                    process_space_real(ipnt(31)) = limnsm01w
                    process_space_real(ipnt(32)) = limpsm01w
                    process_space_real(ipnt(33)) = lco2sm01
                    process_space_real(ipnt(34)) = limnsm01b
                    process_space_real(ipnt(35)) = limpsm01b
                    process_space_real(ipnt(36)) = limnutsm01
                    process_space_real(ipnt(37)) = frootuptn
                    process_space_real(ipnt(38)) = frootuptp

                endif
            endif
            ipnt = ipnt + increm

        enddo

        return
    contains

        ! Auxiliary function to compute the index for the associated bottom segment
        integer function botidx(number)
            integer, intent(in) :: number

            botidx = ipoint(number) + (ibotseg - 1) * increm(number)
        end function botidx

    end subroutine

end module m_macnut
