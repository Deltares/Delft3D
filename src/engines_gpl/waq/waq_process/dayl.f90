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
module m_dayl
    use m_waq_precision

    implicit none

contains


    subroutine dayl   (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Daylength calculation in hours

        !
        !     Description of the module :
        !
        !        Computes daylength in hours.
        !        Formulea 6.2.7 from "Zonnestraling in Nederland",
        !        C.A.Velds, Thieme/KNMI, 1992, 1st imp., ISBN 90-5210-140-X
        !        General water quality module for DELWAQ:
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! TIME    R*4 1 I  DELWAQ time in scu                              [scu]
        ! TREF    R*4 1 I  Reference time in days                            [d]
        ! RLAT    R*4 1 I  Latitude, north pos., south neg. [ radians ]   [grad]
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        implicit none
        real(kind = real_wp) :: process_space_real  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(*), increm(*), num_cells, noflux, &
                iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        integer(kind = int_wp) :: ip1, ip2, ip3, ip4, ip5
        integer(kind = int_wp) :: in1, in2, in3, in4, in5
        real(kind = real_wp) :: time, tref, auxsys
        real(kind = real_wp) :: daynr
        double precision latitu, declin, temp, daylength
        double precision sin50m, e, pi
        parameter (sin50m = -1.454389765d-2)
        parameter (e = 1.721420632d-2)
        parameter (pi = 3.141592654d0)
        logical  varflg
        integer(kind = int_wp) :: iseg

        in1 = increm(1)
        in2 = increm(2)
        in3 = increm(3)
        in4 = increm(4)
        in5 = increm(5)

        ip1 = ipoint(1)
        ip2 = ipoint(2)
        ip3 = ipoint(3)
        ip4 = ipoint(4)
        ip5 = ipoint(5)

        varflg = .true.

        if (in1 == 0 .and. in2 == 0 .and. in3 == 0 .and. &
                in4 == 0) then

            !        Only constant inputs, so only single calculation of daylength needed to be set to all segments
            varflg = .false.
            time = process_space_real(ip1)
            !        Conversion Latitude to rads
            latitu = process_space_real(ip2) / 360 * 2 * pi
            tref = process_space_real(ip3)
            auxsys = process_space_real(ip4)

            !        Conversion time to daynumbers relative to tref
            daynr = mod (time / auxsys + tref, 365.)

            !        Computes declination of sun on day DAYNR.
            if ((daynr < 0.) .or. (daynr > 365.)) then
                declin = 9.9999d9
            else
                declin = 6.918d-3 - &
                        3.99912d-1 * cos (e * daynr) - &
                        6.758d-3 * cos (2.0d0 * e * daynr) - &
                        2.697d-3 * cos (3.0d0 * e * daynr) + &
                        7.0257d-2 * sin (e * daynr) + &
                        9.07d-4 * sin (2.0d0 * e * daynr) + &
                        1.480d-3 * sin (3.0d0 * e * daynr)
            endif

            !       Computes daylenth

            temp = ((sin50m - sin (declin) * sin (latitu)) / &
                    (cos (declin) * cos (latitu)))

            if (temp > 1.0) then
                temp = 0.0
            elseif (temp < -1.0) then
                temp = 24.0
            else
                temp = 7.639437268d0 * acos (temp)
            endif
            daylength = temp / 24.0
        endif

        do iseg = 1, num_cells
            if (varflg) then
                time = process_space_real(ip1)
                !           Conversion Latitude to rads
                latitu = process_space_real(ip2) / 360 * 2 * pi
                tref = process_space_real(ip3)
                auxsys = process_space_real(ip4)

                !           Conversion time to daynumbers relative to tref
                daynr = mod (time / auxsys + tref, 365.)

                !           Computes declination of sun on day DAYNR.
                if ((daynr < 0) .or. (daynr > 365.)) then
                    declin = 9.9999d9
                else
                    declin = 6.918d-3 - &
                            3.99912d-1 * cos (e * daynr) - &
                            6.758d-3 * cos (2.0d0 * e * daynr) - &
                            2.697d-3 * cos (3.0d0 * e * daynr) + &
                            7.0257d-2 * sin (e * daynr) + &
                            9.07d-4 * sin (2.0d0 * e * daynr) + &
                            1.480d-3 * sin (3.0d0 * e * daynr)
                endif

                !           Computes daylenth

                temp = ((sin50m - sin (declin) * sin (latitu)) / &
                        (cos (declin) * cos (latitu)))

                if (temp > 1.0) then
                    temp = 0.0
                elseif (temp < -1.0) then
                    temp = 24.0
                else
                    temp = 7.639437268d0 * acos (temp)
                endif
                daylength = temp / 24.0
            endif
            process_space_real (ip5) = daylength

            ip1 = ip1 + in1
            ip2 = ip2 + in2
            ip3 = ip3 + in3
            ip4 = ip4 + in4
            ip5 = ip5 + in5

        end do

        return
    end

end module m_dayl
