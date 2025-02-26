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
module m_rfpart
    use m_waq_precision
    use chemical_utils, only: chlorinity_from_sal

    implicit none
    private
    public :: rfpart

contains


    subroutine rfpart (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper, only : stop_with_error, get_log_unit_number

        !>\file
        !>       Reprofunctions for heavy metal partition coefficients

        implicit none
        !
        !     declaration of arguments
        !
        integer(kind = int_wp) :: num_cells, noflux, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        integer(kind = int_wp) :: ipoint(*), increm(*), &
                iexpnt(4, *), iknmrk(*)
        real(kind = real_wp) :: process_space_real(*), fl(*)
        !
        !     local declarations
        !
        integer(kind = int_wp) :: ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, ip9, ip10, &
                ip11, ip12, ip13, ip14, ip15, ip16, ip17, ip18, &
                ip19, ip20, ip21
        integer(kind = int_wp) :: iflux, iseg

        integer(kind = int_wp) :: lunrep

        real(kind = real_wp)   :: alk         ! alkalinity                                 [mole/m3]
        real(kind = real_wp)   :: sal         ! salinity                                      [g/kg]
        real(kind = real_wp)   :: temp        ! temperature                                     [oC]
        real(kind = real_wp)   :: cecim1      ! cation exchange capacity of IM1            [eq/kgDW]
        real(kind = real_wp)   :: cecim2      ! cation exchange capacity of IM2            [eq/kgDW]
        real(kind = real_wp)   :: cecim3      ! cation exchange capacity of IM3            [eq/kgDW]
        real(kind = real_wp)   :: doc         ! dissolve organic carbon concentration        [gC/m3]
        real(kind = real_wp)   :: kpim1       ! partition coefficient for IM1              [m3/kgDW]
        real(kind = real_wp)   :: kpim2       ! partition coefficient for IM2              [m3/kgDW]
        real(kind = real_wp)   :: kpim3       ! partition coefficient for IM3              [m3/kgDW]
        real(kind = real_wp)   :: kp0         ! reference partition coefficient            [m3/kgDW]
        real(kind = real_wp)   :: ph          ! acidity                                          [-]
        real(kind = real_wp)   :: ac          ! coefficient a for reprofunction            [various]
        real(kind = real_wp)   :: bc          ! coefficient b for reprofunction            [various]
        real(kind = real_wp)   :: cc          ! coefficient c for reprofunction            [various]
        real(kind = real_wp)   :: dc          ! coefficient d for reprofunction            [various]
        real(kind = real_wp)   :: gc          ! coefficient g for reprofunction            [various]
        real(kind = real_wp)   :: lc          ! coefficient l for reprofunction            [various]
        real(kind = real_wp)   :: mc          ! coefficient m for reprofunction            [various]
        real(kind = real_wp)   :: nc          ! coefficient n for reprofunction            [various]
        real(kind = real_wp)   :: oc          ! coefficient o for reprofunction            [various]
        integer(kind = int_wp) :: iversn      ! option parameter for reprofunction               [-]
                                              ! (0=no repro, 1=Rine repro, 2=North Sea repro)

        real(kind = real_wp) :: ccl, logalk, logccl, logdoc, logkp0

        ip1 = ipoint(1)
        ip2 = ipoint(2)
        ip3 = ipoint(3)
        ip4 = ipoint(4)
        ip5 = ipoint(5)
        ip6 = ipoint(6)
        ip7 = ipoint(7)
        ip8 = ipoint(8)
        ip9 = ipoint(9)
        ip10 = ipoint(10)
        ip11 = ipoint(11)
        ip12 = ipoint(12)
        ip13 = ipoint(13)
        ip14 = ipoint(14)
        ip15 = ipoint(15)
        ip16 = ipoint(16)
        ip17 = ipoint(17)
        ip18 = ipoint(18)
        ip19 = ipoint(19)
        ip20 = ipoint(20)
        ip21 = ipoint(21)

        iflux = 0

        do iseg = 1, num_cells

            if (btest(iknmrk(iseg), 0)) then

                ph = process_space_real(ip1)
                alk = process_space_real(ip2)
                sal = process_space_real(ip3)
                doc = process_space_real(ip4)
                cecim1 = process_space_real(ip5)
                cecim2 = process_space_real(ip6)
                cecim3 = process_space_real(ip7)
                ac = process_space_real(ip8)
                bc = process_space_real(ip9)
                cc = process_space_real(ip10)
                dc = process_space_real(ip11)
                gc = process_space_real(ip13)
                lc = process_space_real(ip12)
                mc = process_space_real(ip14)
                nc = process_space_real(ip15)
                oc = process_space_real(ip16)
                iversn = nint (process_space_real(ip17))
                temp = process_space_real(ip18)
                ccl = chlorinity_from_sal( sal, temp )

                if (alk < 1.0) alk = 1.0
                if (ccl < 1.0) ccl = 1.0
                if (doc < 0.1) doc = 0.1
                !
                !           calculation of partition coefficients depending on switch
                !
                if (iversn == 1) then

                    !              "rhine" function

                    logalk = log10(alk)
                    logccl = log10(ccl)
                    logdoc = log10(doc)
                    logkp0 = ac + bc * ph + cc * (ph**2) + dc * logalk + gc * logccl + &
                            lc * logdoc + mc * ph * logalk + nc * ph * logalk * logalk + &
                            oc * ph * ph * logalk
                    kp0 = 10.0**logkp0

                    kpim1 = kp0 * (cecim1 / 0.0002)
                    kpim2 = kp0 * (cecim2 / 0.0002)
                    kpim3 = kp0 * (cecim3 / 0.0002)

                elseif (iversn == 2) then

                    !              "north sea" function

                    kp0 = (10.0**ac) * (10.0**(bc * ph)) * &
                            ((1800. * ccl + cc)**dc)
                    kpim1 = kp0 * cecim1 * 1000.
                    kpim2 = kp0 * cecim2 * 1000.
                    kpim3 = kp0 * cecim3 * 1000.
                    !
                else

                    !              switch for function out of range

                    call get_log_unit_number(lunrep)
                    write(lunrep, *) 'Error in rfpart'
                    write(lunrep, *) 'Invalid option for repro function partition coefficient'
                    write(lunrep, *) 'Option in input:', iversn
                    write(*, *) ' Error in rfpart'
                    write(*, *) ' Invalid option for repro function partition coefficient'
                    write(*, *) ' Option in input:', iversn
                    call stop_with_error()

                endif
                !
                !           output of module
                !
                process_space_real(ip18) = kpim1
                process_space_real(ip19) = kpim2
                process_space_real(ip20) = kpim3
                !
                !        end active cells block
                !
            endif

            iflux = iflux + noflux
            ip1 = ip1 + increm (1)
            ip2 = ip2 + increm (2)
            ip3 = ip3 + increm (3)
            ip4 = ip4 + increm (4)
            ip5 = ip5 + increm (5)
            ip6 = ip6 + increm (6)
            ip7 = ip7 + increm (7)
            ip8 = ip8 + increm (8)
            ip9 = ip9 + increm (9)
            ip10 = ip10 + increm (10)
            ip11 = ip11 + increm (11)
            ip12 = ip12 + increm (12)
            ip13 = ip13 + increm (13)
            ip14 = ip14 + increm (14)
            ip15 = ip15 + increm (15)
            ip16 = ip16 + increm (16)
            ip17 = ip17 + increm (17)
            ip18 = ip18 + increm (18)
            ip19 = ip19 + increm (19)
            ip20 = ip20 + increm (20)
            ip21 = ip21 + increm (21)

        enddo

        return

    end subroutine rfpart

end module m_rfpart
