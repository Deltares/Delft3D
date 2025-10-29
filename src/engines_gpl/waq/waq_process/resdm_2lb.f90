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
module m_resdm_2lb
    use m_waq_precision

    implicit none

contains


    subroutine resdm_2lb ( process_space_real, fl, ipoint, increm, num_cells, &
                        noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                        num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Compute resuspension fluxes from bed (part 4 of the 2-layer bed model)

        !
        !     Description of the module :
        !        General water quality module for DELWAQ:
        !        CALCULATES RESUSPENSION FLUXES FROM BED LAYERS. IT DISTINGUISH TWO SCENARIOS:
        !        NET EROSION - ONLY RESUSPENSION, NO DEPOSITION ALLOWED;
        !        NET DEPOSITION - RESUSPENSION COMPUTED HERE AND DEPOSITION IN SEDIM_2LB.
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! DMS1      R*4 1 I  total amount of dry matter in layer S1      [gDM/m2]
        ! DMS2      R*4 1 I  total amount of dry matter in layer S2      [gDM/m2]
        ! MEROS1    R*4 1 I  erosion parameter of layer S1               [gDM/m2/day]
        ! MEROS2    R*4 1 I  erosion parameter of layer S2               [gDM/m2/day]
        ! TAU       R*4 1 I  total bottom shear stress                   [N/m2]
        ! TCRRS1    R*4 1 I  critical shear stress for erosion in layer S1 [m/s2]
        ! TCRRS2    R*4 1 I  critical shear stress for erosion in layer S2 [m/s2]
        ! DEPTH     R*4 1 I  depth of segment                            [m]
        ! DELT      R*4 1 I  timestep for processes                      [d]
        ! MINDEPTH  R*4 1 I  minimum waterdepth for sedimentation/resuspension [m]
        ! SURF      R*4 1 I  horizontal surface area of a DELWAQ segment [m2]
        ! IM1       R*4 1 I  inorganic matter (IM1)                      [gDM/m3]
        ! IM2       R*4 1 I  inorganic matter (IM2)                      [gDM/m3]
        ! IM3       R*4 1 I  inorganic matter (IM3)                      [gDM/m3]
        ! VSEDIM1   R*4 1 I  sedimentation velocity IM1                  [m/d]
        ! VSEDIM2   R*4 1 I  sedimentation velocity IM2                  [m/d]
        ! VSEDIM3   R*4 1 I  sedimentation velocity IM3                  [m/d]
        ! MGEL      R*4 1 I  erosion parameter at gel concentration      [gDM/m2/day]
        ! TCGEL     R*4 1 I  critical shear stress for erosion at gel conc. [Pa]
        ! FLRES1    R*4 1 O  total resuspension flux DM from layer S1    [g/m2/d]
        ! FLRES2    R*4 1 O  total resuspension flux DM from layer S2    [gDM/m2/d]
        ! PRESS1    R*4 1 O  resuspension probability for layer S1 <0-inf> [-]
        ! PRESS2    R*4 1 O  resuspension probability for layer S2 <0-inf> [-]
        ! FLSEDT    R*4 1 O  sedimentation flux (total) for 2-layer bed model [gDM/m2/d]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE

        real(kind = real_wp)   :: process_space_real(*)      !i/o process manager system array, window of routine to process library
        real(kind = real_wp)   :: fl(*)        ! o  array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(26)   ! i  array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(26)   ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells        ! i  number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux       ! i  number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)    ! i  active-inactive, surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir         ! i  nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir         ! i  nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir         ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir         ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(26)     !    local work array for the pointering
        integer(kind = int_wp) :: iseg         !    local loop counter for computational element loop

        integer(kind = int_wp) :: iflux
        integer(kind = int_wp) :: ikmrk2
        real(kind = real_wp) :: dms1
        real(kind = real_wp) :: dms2
        real(kind = real_wp) :: meros1
        real(kind = real_wp) :: meros2
        real(kind = real_wp) :: tau
        real(kind = real_wp) :: tcrrs1
        real(kind = real_wp) :: tcrrs2
        real(kind = real_wp) :: depth
        real(kind = real_wp) :: delt
        real(kind = real_wp) :: mindep
        real(kind = real_wp) :: surf
        real(kind = real_wp) :: im1
        real(kind = real_wp) :: im2
        real(kind = real_wp) :: im3
        real(kind = real_wp) :: vsedim1
        real(kind = real_wp) :: vsedim2
        real(kind = real_wp) :: vsedim3
        real(kind = real_wp) :: flsed_im1
        real(kind = real_wp) :: flsed_im2
        real(kind = real_wp) :: flsed_im3

        real(kind = real_wp) :: press1
        real(kind = real_wp) :: press2
        real(kind = real_wp) :: rfdms1
        real(kind = real_wp) :: rfdms2
        real(kind = real_wp) :: flres1
        real(kind = real_wp) :: flres2
        real(kind = real_wp) :: mrdms1
        real(kind = real_wp) :: mrdms2
        real(kind = real_wp) :: flsedt
        real(kind = real_wp) :: flrest
        real(kind = real_wp) :: mgel
        real(kind = real_wp) :: tcgel
        real(kind = real_wp) :: delts2
        
        integer(kind = int_wp) :: isbeds1
        integer(kind = int_wp) :: isbeds2

        ipnt = ipoint

        iflux = 0
        do iseg = 1, num_cells

            if (btest(iknmrk(iseg), 0)) then
                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if ((ikmrk2==0).or.(ikmrk2==3)) then
                
                    dms1    = process_space_real(ipnt (1))
                    dms2    = process_space_real(ipnt (2))
                    meros1  = process_space_real(ipnt (3))
                    meros2  = process_space_real(ipnt (4))
                    tau     = process_space_real(ipnt (5))
                    tcrrs1  = process_space_real(ipnt (6))
                    tcrrs2  = process_space_real(ipnt (7))
                    depth   = process_space_real(ipnt (8))
                    delt    = process_space_real(ipnt (9))
                    mindep  = process_space_real(ipnt (10))
                    surf    = process_space_real(ipnt (11))
                    im1     = max(0.0, process_space_real(ipnt (12)))
                    im2     = max(0.0, process_space_real(ipnt (13)))
                    im3     = max(0.0, process_space_real(ipnt (14)))
                    vsedim1 = max(0.0, process_space_real(ipnt (15)))
                    vsedim2 = max(0.0, process_space_real(ipnt (16)))
                    vsedim3 = max(0.0, process_space_real(ipnt (17)))
                    mgel    = process_space_real(ipnt (18)) ! erosion parameter of gel (g/m2/day)
                    tcgel   = process_space_real(ipnt (19)) ! critical shear stress for erosion of gel (Pa)
                    isbeds1 = nint(process_space_real(ipnt (20)))
                    isbeds2 = nint(process_space_real(ipnt (21)))
                
                    !***********************************************************************
                    !**** Processes connected to the RESUSENSION
                    !***********************************************************************
                
                    press1 = 0.0
                    press2 = 0.0
                    flres1 = 0.0
                    flres2 = 0.0
                    flsedt = 0.0
                
                    ! Calculate resuspension probability in S1
                    if (tau == -1.0) then
                        press1 = 1.0
                    else
                        ! Compare with critical shear stress
                        press1 = max (0.0, (tau / tcrrs1 - 1.0))
                    endif
                
                    ! Calculate resuspension probability in S2
                    if (tau == -1.0) then
                        press2 = 1.0
                    else
                        ! Compare with critical shear stress
                        press2 = max (0.0, (tau / tcrrs2 - 1.0))
                    endif
                    
                    ! No resuspension when depth below min depth
                    if (depth < mindep) then
                        flres1 = 0.0
                        flres2 = 0.0
                        flsedt = 0.0
                    else
                        ! Compute potential deposition flux
                        flsed_im1 = min(im1 * vsedim1, im1 / delt * depth)
                        flsed_im2 = min(im2 * vsedim2, im2 / delt * depth)
                        flsed_im3 = min(im3 * vsedim3, im3 / delt * depth)
                        flsedt = flsed_im1 + flsed_im2 + flsed_im3
                        
                        rfdms1 = meros1
                        rfdms2 = meros2
                
                        if (flsedt >= max(0.0, mgel * (tau / tcgel - 1.0))) then 
                            ! Net deposotion scenario: 
                            flres1  = max(0.0, mgel * (tau / tcgel - 1.0))
                            flres2  = 0.0
                        else  
                            ! Net erosion scenario                            
                            ! Resuspension from S1
                            flrest = (1.0 - flsedt / max(0.0, mgel * (tau / tcgel - 1.0))) * (rfdms1 * press1)
                            mrdms1 = max (0.0, dms1 / delt) ! available material in S1
                
                            if (flrest <= mrdms1) then       
                                ! enough S1 layer for erosion
                                flres1 = flrest
                                flres2 = 0.0
                            else
                                ! If first layer is exhausted then resuspension from the second layer
                                flres1 = mrdms1
                                delts2 = max(0.0, (1.0 - flres1 / flrest) * delt)
                                ! Limit resuspension to available material in S2
                                mrdms2 = max (0.0, dms2 / delt)
                                flres2 = min (rfdms2 * press2 * delts2 / delt, mrdms2)
                            endif
                        endif
                    endif
                
                    process_space_real (ipnt (22)) = flres1
                    process_space_real (ipnt (23)) = flres2
                    process_space_real (ipnt (24)) = press1
                    process_space_real (ipnt (25)) = press2
                    process_space_real (ipnt (26)) = flsedt
                
                endif
            endif
            !
            iflux = iflux + noflux
            ipnt = ipnt + increm

        end do
        !
        return
        !
    end

end module m_resdm_2lb
