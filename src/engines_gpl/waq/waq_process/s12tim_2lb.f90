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
module m_s12tim_2lb
    use m_waq_precision

    implicit none

contains


    subroutine s12tim_2lb (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute


        !>\file
        !>       Update the mass in bed considering resuspension and burial flux
        !        (part 7 of the 2-layer bed model)

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        CALCULATE THE CHANGES IN MASS IN BED DUE TO RESUSPENSION AND BURIAL
        !        RESUSPENSION AND BURIAL FLUXES ARE REDISTRIBUTED BASED ON MASS FRACTION 
        !        OF EACH SEDIMENT CLASS, THIS MEANS BURIAL IS ONLY ALLOWED WHEN THERE
        !        IS SEDIMENT MASS IN BED.
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! FRACS1    R*4 1 I  fraction IMX in layer S1                   [gDM/gDM]
        ! FRACS2    R*4 1 I  fraction IMX in layer S2                   [gDM/gDM]
        ! FRESS1    R*4 1 I  total resuspension flux DM from layer S1   [g/m2/d]
        ! FRESS2    R*4 1 I  total resuspension flux DM from layer S2   [gDM/m2/d]
        ! FBURS1    R*4 1 I  total burial flux DM from layer S1         [gDM/m2/d]
        ! FBURS2    R*4 1 I  total burial flux DM from layer S2         [gDM/m2/d]
        ! FSED      R*4 1 I  sedimentation flux IM1 towards S1          [g/m2/d]
        ! DEPTH     R*4 1 I  depth of segment                           [m]
        ! IMS1      R*4 1 I  IMX in layer S1                            [gDM/m2]
        ! IMS2      R*4 1 I  IMX in layer S2                            [gDM/m2]
        ! DELT      R*4 1 I  timestep for processes                     [d]
        ! RES1      R*4 1 O  resuspension flux IM1 from layer S1        [g/m2/d]
        ! RES2      R*4 1 O  resuspension flux IM1 from layer S2        [g/m2/d]
        ! BUR1      R*4 1 O  burial flux of IM1 from layer S1           [g/m2/d]
        ! BUR2      R*4 1 O  burial flux IM1 from layer S2              [g/m2/d]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        implicit none

        real(kind = real_wp) :: process_space_real  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(17), increm(17), num_cells, noflux, &
                iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        integer(kind = int_wp) :: ip(17), iflux, iseg, ikmrk2
        
        real(kind = real_wp) :: fracs1
        real(kind = real_wp) :: fracs2
        real(kind = real_wp) :: fress1
        real(kind = real_wp) :: fress2
        real(kind = real_wp) :: fburs1
        real(kind = real_wp) :: fburs2
        real(kind = real_wp) :: fsed
        real(kind = real_wp) :: depth
        real(kind = real_wp) :: ims1
        real(kind = real_wp) :: ims2
        real(kind = real_wp) :: delt
        real(kind = real_wp) :: b1, b2, r1, r2
        real(kind = real_wp) :: bur1, bur2, res1, res2
        integer(kind = int_wp) :: isbeds1, isbeds2

        ip = ipoint
        !
        iflux = 0
        do iseg = 1, num_cells

            if (btest(iknmrk(iseg), 0)) then
                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if ((ikmrk2==0).or.(ikmrk2==3)) then
                    !
                    fracs1 = process_space_real(ip( 1))
                    fracs2 = process_space_real(ip( 2))
                    fress1 = process_space_real(ip( 3))
                    fress2 = process_space_real(ip( 4))
                    fburs1 = process_space_real(ip( 5))
                    fburs2 = process_space_real(ip( 6))
                    fsed   = process_space_real(ip( 7))
                    depth  = process_space_real(ip( 8))
                    ims1   = process_space_real(ip( 9))
                    ims2   = process_space_real(ip(10))
                    delt   = process_space_real(ip(11))
                    isbeds1 = nint(process_space_real(ip(12)))
                    isbeds2 = nint(process_space_real(ip(13)))
                
                    !***********************************************************************
                    !**** Processes connected to the RESUSPENSION and BURIAL
                    !***********************************************************************
                
                    ! resuspension
                    r1 = 0.0
                    r2 = 0.0
                    if (fracs1 > 0.0) r1 = fress1 * fracs1
                    if (fracs2 > 0.0) r2 = fress2 * fracs2
                
                    ! burial (only happens when there is net deposition)
                    b1 = 0.0
                    b2 = 0.0
                    if (fracs1 > 0.0) b1 = fburs1 * fracs1
                    if (fracs2 > 0.0) b2 = fburs2 * fracs2
                
                    ! check conservation of bed mass (resuspension and burial)
                    ! sedimentation only towards S1
                    res1 = min(r1, max(0.0, ims1 / delt + fsed)) ! r1
                    res2 = min(r2, max(0.0, ims2 / delt)) ! r2
                    bur1 = min(b1, max(0.0, ims1 / delt + fsed - r1)) ! b1
                    bur2 = b2 ! no burial from S2 to beneath layer
                    
                    ! store results
                    process_space_real(ip(14)) = res1
                    process_space_real(ip(15)) = res2
                    process_space_real(ip(16)) = bur1
                    process_space_real(ip(17)) = bur2
                
                    fl(1 + iflux) = res1 / depth
                    fl(2 + iflux) = res2 / depth
                    fl(3 + iflux) = bur1 / depth
                    fl(4 + iflux) = bur2 / depth
                
                endif
            endif
            !
            iflux = iflux + noflux
            ip = ip + increm
            !
        end do
        !
        return
        !
    end

end module m_s12tim_2lb
