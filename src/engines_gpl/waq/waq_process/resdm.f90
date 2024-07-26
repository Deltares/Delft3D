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
module m_resdm
    use m_waq_precision

    implicit none

contains


    subroutine resdm  (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Resuspension total bottom material (dry mass)

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! DM1     R*4 1 I  amount dry matter in layer S1                     [gDM/m2]
        ! DM2     R*4 1 I  amount dry matter in layer S2                     [gDM/m2]
        ! DELT    R*4 1 I  DELWAQ timestep                                   [scu]
        ! DEPTH   R*4 1 I  depth water column                                  [m]
        ! FLRES1  R*4 1 O  resuspension flux DM from layer S1           [gDM/m2/d]
        ! FLRES2  R*4 1 O  resuspension flux DM from layer S2           [gDM/m2/d]
        ! IAUSYS  R*4 1 I  ratio between auxiliary and system clock unit       [-]
        ! MRDMS1  R*4 1 L  max. res. flux (contents of layer S1)        [gDM/m2/d]
        ! MRDMS2  R*4 1 L  max. res. flux (contents of layer S2)        [gDM/m2/d]
        ! MINDEP  R*4 1 I  minimal depth for resuspension                      [m]
        ! PRESS1  R*4 1 L  resuspension probability from S1 (0 - endless)      [-]
        ! PRESS2  R*4 1 L  resuspension probability from S2 (0 - endless)      [-]
        ! POTRES  R*4 1 L  potential resuspension flux                  [gDM/m2/d]
        ! FLRES1  R*4 1 L  resuspension flux DM from layer S1           [gDM/m2/d]
        ! FLRES2  R*4 1 L  resuspension flux DM from layer S2           [gDM/m2/d]
        ! TAU     R*4 1 I  calculated sheerstress                        [kg/m/s2]
        ! TAUVEL  R*4 1 I  total velocity calcualted from tau                [m/s]
        ! TCRRS1  R*4 1 I  critical sheerstress resuspension S1          [kg/m/s2]
        ! TCRRS2  R*4 1 I  critical sheerstress resuspension S2          [kg/m/s2]
        ! VCRRS1  R*4 1 I  critical velocity resuspension S1                 [m/s]
        ! VCRRS2  R*4 1 I  critical velocity resuspension S2                 [m/s]
        ! VRES    R*4 1 I  first order resuspensionrate constant             [1/d]
        ! VOLUME  R*4 1 I  volume computed by DELWAQ                          [m3]
        ! ZRES    R*4 1 I  zeroth order resuspension flux               [gDM/m2/d]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE

        real(kind = real_wp) :: process_space_real(*)     !i/o process manager system array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! o  array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(16) ! i  array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(16) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! i  number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! i  number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! i  nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! i  nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(16)   !    local work array for the pointering
        integer(kind = int_wp) :: iseg        !    local loop counter for computational element loop

        integer(kind = int_wp) :: iflux
        integer(kind = int_wp) :: ikmrk2
        real(kind = real_wp) :: dms1
        real(kind = real_wp) :: dms2
        real(kind = real_wp) :: zres
        real(kind = real_wp) :: vres
        real(kind = real_wp) :: tau
        real(kind = real_wp) :: tcrrs1
        real(kind = real_wp) :: tcrrs2
        real(kind = real_wp) :: depth
        real(kind = real_wp) :: delt
        real(kind = real_wp) :: mindep
        real(kind = real_wp) :: surf
        integer(kind = int_wp) :: isw_zf
        real(kind = real_wp) :: press1
        real(kind = real_wp) :: press2
        real(kind = real_wp) :: flres1
        real(kind = real_wp) :: flres2
        real(kind = real_wp) :: rfdms1
        real(kind = real_wp) :: mrdms1
        real(kind = real_wp) :: delts2
        real(kind = real_wp) :: rfdms2
        real(kind = real_wp) :: mrdms2

        ipnt = ipoint

        iflux = 0
        do iseg = 1, num_cells
            if (btest(iknmrk(iseg), 0)) then
                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if ((ikmrk2==0).or.(ikmrk2==3)) then

                    dms1 = process_space_real(ipnt (1))
                    dms2 = process_space_real(ipnt (2))
                    zres = process_space_real(ipnt (3))
                    vres = process_space_real(ipnt (4))
                    tau = process_space_real(ipnt (5))
                    tcrrs1 = process_space_real(ipnt (6))
                    tcrrs2 = process_space_real(ipnt (7))
                    depth = process_space_real(ipnt (8))
                    delt = process_space_real(ipnt (9))
                    mindep = process_space_real(ipnt (10))
                    surf = process_space_real(ipnt (11))
                    isw_zf = nint(process_space_real(ipnt (12)))

                    !***********************************************************************
                    !**** Processes connected to the RESUSENSION
                    !***********************************************************************

                    press1 = 0.0
                    press2 = 0.0

                    !     Calculate resuspension probability in S1
                    if (tau == -1.0) then
                        press1 = 1.0
                    else
                        !         Compare with critical shear stress
                        press1 = max (0.0, (tau / tcrrs1 - 1.0))
                    endif

                    !     Calculate resuspension probability in S2
                    if (tau == -1.0) then
                        press2 = 1.0
                    else
                        !        Compare with critical shear stress
                        press2 = max (0.0, (tau / tcrrs2 - 1.0))
                    endif

                    !     No resuspension when depth below min depth
                    if (depth < mindep) then
                        flres1 = 0.0
                        flres2 = 0.0
                    else
                        !        Resuspension from S1
                        if (isw_zf == 0) then
                            ! add zero and first order resuspension
                            rfdms1 = zres + (vres * dms1)
                        else
                            ! take the minimum of the first order and second order
                            rfdms1 = min(zres, (vres * dms1))
                        endif

                        !        Limit resuspension to available material in S1
                        mrdms1 = max (0.0, dms1 / delt)
                        flres1 = min (rfdms1 * press1, mrdms1)

                        !        If first layer is exhausted then resuspension from the second layer for the remaining of the timestep (DELTS2)
                        if (rfdms1 * press1 > 1e-20) then
                            delts2 = max(0.0, (1. - flres1 / (rfdms1 * press1)) * delt)
                        else
                            delts2 = 0.0
                        endif

                        if (isw_zf == 0) then
                            rfdms2 = zres + (vres * dms2)
                        else
                            rfdms2 = min(zres, (vres * dms2))
                        endif

                        !        Limit resuspension to available material in S2
                        mrdms2 = max (0.0, dms2 / delt)
                        flres2 = min (rfdms2 * press2 * delts2 / delt, mrdms2)
                    endif

                    process_space_real (ipnt (13)) = flres1
                    process_space_real (ipnt (14)) = flres2
                    process_space_real (ipnt (15)) = press1
                    process_space_real (ipnt (16)) = press2

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

end module m_resdm
