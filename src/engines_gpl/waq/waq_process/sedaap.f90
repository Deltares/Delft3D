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
module m_sedaap
    use m_waq_precision

    implicit none

contains


    subroutine sedaap (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Sedimentation flux and velocity for PAP and AAP (adsorbed PO4)

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !
        ! Name    T   L I/O   Description                                    Uni
        ! ----    --- -  -    -------------------                            ---
        ! SFL1    R*4 1 I  sedimentation flux carriers                 [gC/m2/d]
        ! Q1      R*4 1 I  quality of carrier                          [gOMV/gC]
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        implicit none

        real(kind = real_wp) :: process_space_real  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(20), increm(20), num_cells, noflux, &
                iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        integer(kind = int_wp) :: ipnt(20)

        integer(kind = int_wp) :: iflux, iseg, ikmrk2, iq, ifrom

        real(kind = real_wp) :: sfl1, sfl2, sfl3
        real(kind = real_wp) :: sfl1s2, sfl2s2, sfl3s2
        real(kind = real_wp) :: q1, q2, q3, depth
        real(kind = real_wp) :: fpim1, fpim2, fpim3
        real(kind = real_wp) :: vsim1, vsim2, vsim3

        ipnt = ipoint
        iflux = 0

        do iseg = 1, num_cells
            if (btest(iknmrk(iseg), 0)) then
                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if ((ikmrk2==0).or.(ikmrk2==3)) then
                    !

                    sfl1 = process_space_real(ipnt (1))
                    sfl2 = process_space_real(ipnt (2))
                    sfl3 = process_space_real(ipnt (3))
                    sfl1s2 = process_space_real(ipnt (4))
                    sfl2s2 = process_space_real(ipnt (5))
                    sfl3s2 = process_space_real(ipnt (6))
                    q1 = process_space_real(ipnt (7))
                    q2 = process_space_real(ipnt (8))
                    q3 = process_space_real(ipnt (9))
                    depth = process_space_real(ipnt (13))
                    !      switch = process_space_real(ipnt (14) ) ignore SWITCH

                    !***********************************************************************
                    !**** Processes connected to the SEDIMENTATION of AAP
                    !***********************************************************************

                    !     Sedimentation to S1/S2
                    process_space_real(ipnt(18)) = sfl1 * q1 + sfl2 * q2 + sfl3 * q3
                    fl(1 + iflux) = process_space_real(ipnt(18)) / depth

                    process_space_real(ipnt(19)) = sfl1s2 * q1 + sfl2s2 * q2 + sfl3s2 * q3
                    fl(2 + iflux) = process_space_real(ipnt(19)) / depth

                endif
            endif
            !
            iflux = iflux + noflux
            ipnt = ipnt + increm
            !
        end do

        !.....Reset pointers
        ipnt = ipoint

        !.....Exchangeloop over horizontal direction
        do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir

            !........Set VxSedAAP to zero
            process_space_real(ipnt(20)) = 0.0
            ipnt(20) = ipnt(20) + increm(20)

        end do

        !.....Entery point in process_space_real for VxSedIMX in the vertical direction
        ipnt(15) = ipnt(15) + (num_exchanges_u_dir + num_exchanges_v_dir) * increm(15)
        ipnt(16) = ipnt(16) + (num_exchanges_u_dir + num_exchanges_v_dir) * increm(16)
        ipnt(17) = ipnt(17) + (num_exchanges_u_dir + num_exchanges_v_dir) * increm(17)

        !.....Exchange loop over the vertical direction
        do iq = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

            ifrom = iexpnt(1, iq)

            if (ifrom > 0) then
                fpim1 = process_space_real(ipnt(10) + (ifrom - 1) * increm(10))
                fpim2 = process_space_real(ipnt(11) + (ifrom - 1) * increm(11))
                fpim3 = process_space_real(ipnt(12) + (ifrom - 1) * increm(12))
                vsim1 = process_space_real(ipnt(15))
                vsim2 = process_space_real(ipnt(16))
                vsim3 = process_space_real(ipnt(17))
                !...........calculate VxSedAAP
                process_space_real(ipnt(20)) = fpim1 * vsim1 + fpim2 * vsim2 + fpim3 * vsim3
            endif

            !........Exchangepointers increment
            ipnt(15) = ipnt(15) + increm(15)
            ipnt(16) = ipnt(16) + increm(16)
            ipnt(17) = ipnt(17) + increm(17)
            ipnt(20) = ipnt(20) + increm(20)

        end do

        return
    end

end module m_sedaap
