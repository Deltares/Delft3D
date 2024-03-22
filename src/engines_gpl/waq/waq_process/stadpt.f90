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
module m_stadpt
    use m_waq_precision

    implicit none

contains


    subroutine stadpt (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_evaluate_waq_attribute

        !>\file
        !>       Depth-averaged, max and min value per timestep

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !
        ! Name    T   L I/O   Description                                  Units
        ! ----    --- -  -    -------------------                          -----
        !
        ! CONC           I    Concentration of the substance              1
        ! VOLUME         I    Volume of the computational cells           2
        !
        ! DPTAVG         O    Average over depth                          3
        ! DPTMAX         O    Maximum over depth                          4
        ! DPTMIN         O    Minimum over depth                          5
        !

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        implicit none

        real(kind = real_wp) :: pmsa  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(*), increm(*), noseg, noflux
        integer(kind = int_wp) :: iexpnt(4, *), iknmrk(*), noq1, noq2, noq3, noq4
        !
        integer(kind = int_wp) :: ip1, ip2, ip3, ip4, ip5
        integer(kind = int_wp) :: in1, in2, in3, in4, in5
        integer(kind = int_wp) :: ikmrk
        real(kind = real_wp) :: volume

        !     work arrays
        real(kind = real_wp), allocatable :: cdepsum(:)
        real(kind = real_wp), allocatable :: vdepsum(:)
        real(kind = real_wp), allocatable :: cdepavg(:)
        real(kind = real_wp), allocatable :: cdepmax(:)
        real(kind = real_wp), allocatable :: cdepmin(:)

        integer(kind = int_wp) :: iseg, ifrom, ito, ik1from, ik1to, iq

        ip1 = ipoint(1)
        ip2 = ipoint(2)
        ip3 = ipoint(3)
        ip4 = ipoint(4)
        ip5 = ipoint(5)

        in1 = increm(1)
        in2 = increm(2)
        in3 = increm(3)
        in4 = increm(4)
        in5 = increm(5)

        !
        !     The averaging is independent of a time interval, as the outcome
        !     is itself time-dependent (there is only a reduction in the
        !     spatial coordinates)
        !

        !     This method also works when the vertical is unstructured
        !     The only assumption is that the direction in which the exchanges are
        !     defined is the same as the order that they were arranged in.
        !
        !     so:  from 1 to 2, from 2 to 3, from 3 to 4
        !     or:  from 4 to 3, from 3 to 2, from 2 to 1
        !     not: from 2 to 1, from 3 to 2, from 4 to 3
        !
        !     this assumtion is made elsewhere in Delwaq
        !

        !     allocate and initialise work arrays
        allocate (cdepsum(noseg))
        allocate (vdepsum(noseg))
        allocate (cdepavg(noseg))
        allocate (cdepmax(noseg))
        allocate (cdepmin(noseg))
        cdepsum = 0.0
        vdepsum = 0.0
        cdepavg = 0.0
        cdepmax = 0.0
        cdepmin = 0.0

        !     default output is the value from the segment itself
        do iseg = 1, noseg
            call evaluate_waq_attribute(1, iknmrk(iseg), ikmrk)
            if (ikmrk /= 0) then
                cdepsum(iseg) = pmsa(ip1) * pmsa(ip2)
                vdepsum(iseg) = pmsa(ip2)
                cdepavg(iseg) = pmsa(ip1)
                cdepmax(iseg) = pmsa(ip1)
                cdepmin(iseg) = pmsa(ip1)
            endif
            ip1 = ip1 + in1
            ip2 = ip2 + in2
        end do

        !     first loop forwards
        do iq = noq1 + noq2 + 1, noq1 + noq2 + noq3
            ifrom = iexpnt(1, iq)
            ito = iexpnt(2, iq)
            if (ifrom > 0 .and. ito > 0) then
                call evaluate_waq_attribute(1, iknmrk(ifrom), ik1from)
                call evaluate_waq_attribute(1, iknmrk(ito), ik1to)
                if (ik1from == 1 .and. ik1to == 1) then
                    cdepsum(ito) = cdepsum(ito) + cdepsum(ifrom)
                    vdepsum(ito) = vdepsum(ito) + vdepsum(ifrom)
                    if (vdepsum(ito)>0.0) then
                        cdepavg(ito) = cdepsum(ito) / vdepsum(ito)
                    endif
                    cdepmax(ito) = max(cdepmax(ifrom), cdepmax(ito))
                    cdepmin(ito) = min(cdepmin(ifrom), cdepmin(ito))
                endif
            endif
        enddo

        !     second loop backwards
        do iq = noq1 + noq2 + noq3, noq1 + noq2 + 1, -1
            ifrom = iexpnt(1, iq)
            ito = iexpnt(2, iq)
            if (ifrom > 0 .and. ito > 0) then
                call evaluate_waq_attribute(1, iknmrk(ifrom), ik1from)
                call evaluate_waq_attribute(1, iknmrk(ito), ik1to)
                if (ik1from == 1 .and. ik1to == 1) then
                    cdepavg(ifrom) = cdepavg(ito)
                    cdepmax(ifrom) = cdepmax(ito)
                    cdepmin(ifrom) = cdepmin(ito)
                endif
            endif
        enddo

        !     copy final result back into pmsa array
        do iseg = 1, noseg
            call evaluate_waq_attribute(1, iknmrk(iseg), ikmrk)
            if (ikmrk /= 0) then
                pmsa(ip3) = cdepavg(iseg)
                pmsa(ip4) = cdepmax(iseg)
                pmsa(ip5) = cdepmin(iseg)
            endif
            ip3 = ip3 + in3
            ip4 = ip4 + in4
            ip5 = ip5 + in5
        end do

        !     deallocate work arrays
        deallocate (cdepsum)
        deallocate (vdepsum)
        deallocate (cdepavg)
        deallocate (cdepmax)
        deallocate (cdepmin)

        return
    end

end module m_stadpt
