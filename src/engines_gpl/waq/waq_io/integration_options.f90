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
module integration_options
    use m_waq_precision
    use m_string_utils

    implicit none

    private
    public :: check_integration_option

contains

    subroutine check_integration_option(keynam, intopt, file_unit, ierr2)

        !! Checks for integration option keywords
        !!
        !!    Supported keywords are:
        !!    \li NODISP-AT-NOFLOW      - Diffusion is not applied if Q equals zero (thin dams)
        !!    \li NODISP-AT-BOUND       - Diffusion is not applied accross open boundaries
        !!    \li LOWER-ORDER-AT-BOUND  - Use first order upwind scheme at open boundaries
        !!    \li BALANCES-OLD-STYLE    - Balances information in its basic form only
        !!    \li BALANCES-GPP-STYLE    - Basic balances including enhancements for GPP
        !!    \li BALANCES-SOBEK-STYLE  - Basic balances including SOBEK and GPP enhancements
        !!    \li FORESTER              - Apply Forester filter against overshoots in the vertical
        !!    \li ANTICREEP             - Apply anticreep horizontal diffusion for integration options 19 & 20
        !!    \li BAL_NOLUMPPROCESSES   - Do not lump all processes in one balance term but split
        !!    \li BAL_NOLUMPLOADS       - Do not lump all loads in one balance term but split
        !!    \li BAL_NOLUMPTRANSPORT   - Do not lump all transport terms in one balance term but split
        !!    \li BAL_UNITAREA          - Make balances per m2 rather than per total volume
        !!    \li BAL_UNITVOLUME        - Make balances per m3 rather than per total volume
        !!    \li BAL_NOSUPPRESSSPACE   - ??? to be clarified
        !!    \li BAL_NOSUPPRESSTIME    - ??? to be clarified
        !!    \li ANTIDIFFUSION         - An option of integration methods 21 and 22
        !!    \li PARTICLE_TRACKING     - The Delwaq simulation will also use particle tracking
        !!
        !!    Also the negation of the keywords are valid but they will invoke default behavior

        use timers       !   performance timers


        !     Logical units     : file_unit  = report file

        character(len=*), intent(in) :: keynam            !< string to test
        integer(kind = int_wp), intent(inout) :: intopt             !< integration option
        integer(kind = int_wp), intent(in) :: file_unit              !< unit number report file
        integer(kind = int_wp), intent(out) :: ierr2              !< 0 if keyword found

        integer(kind = int_wp), parameter :: nokey = 19
        character(len=40)  lockey
        character(len=40), save :: keywords(nokey)
        character(len=40), save :: defkeys(nokey)
        data keywords / 'NODISP-AT-NOFLOW          ', 'NODISP-AT-BOUND           ', &
                'LOWER-ORDER-AT-BOUND      ', 'BALANCES-OLD-STYLE        ', &
                'BALANCES-GPP-STYLE        ', 'BALANCES-SOBEK-STYLE      ', &
                'FORESTER                  ', 'ANTICREEP                 ', &
                'BAL_NOLUMPPROCESSES       ', 'BAL_NOLUMPLOADS           ', &
                'BAL_NOLUMPTRANSPORT       ', 'BAL_UNITAREA              ', &
                'BAL_UNITVOLUME            ', 'BAL_NOSUPPRESSSPACE       ', &
                'BAL_NOSUPPRESSTIME        ', 'SCHEME15_UNSTRUCTURED     ', &
                'ANTIDIFFUSION             ', 'PARTICLE_TRACKING         ', &
                'SCHEME24_VERTICAL_CENTRAL ' /
        data defkeys   /'DISP-AT-NOFLOW            ', 'DISP-AT-BOUND             ', &
                'HIGHER-ORDER-AT-BOUND     ', 'NO-BALANCES               ', &
                'x xxxxxxxxxxxxxxxxxxxxxxxx', 'x xxxxxxxxxxxxxxxxxxxxxxxx', &
                'NO-FORESTER               ', 'NO-ANTICREEP              ', &
                'BAL_LUMPPROCESSES         ', 'BAL_LUMPLOADS             ', &
                'BAL_LUMPTRANSPORT         ', 'x xxxxxxxxxxxxxxxxxxxxxxxx', &
                'x xxxxxxxxxxxxxxxxxxxxxxxx', 'BAL_SUPPRESSSPACE         ', &
                'BAL_SUPPRESSTIME          ', 'SCHEME15_STRUCTURED       ', &
                'NO-ANTIDIFFUSION          ', 'x xxxxxxxxxxxxxxxxxxxxxxxx', &
                'SCHEME24_VERTICAL_UPWIND  ' /
        integer(kind = int_wp) :: ikey                 ! number of the found key
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("check_integration_option", ithndl)

        !     watch out BTEST, IBSET en IBCLR start counting at 0, so IKEY-1 should be used

        ierr2 = 0
        lockey = keynam
        ikey = index_in_array(lockey, keywords)   ! look in the keywords
        if (ikey > 0) then
            write (file_unit, 1000) ikey, lockey
            intopt = ibset(intopt, ikey - 1)
            select case (ikey)
            case (4)            !  if old style then don't set gpp style and sobek style
                intopt = ibclr(intopt, 4)
                intopt = ibclr(intopt, 5)
            case (5)            !  gpp balances also set fourth bit, don't set sobek balances
                intopt = ibset(intopt, 3)
                intopt = ibclr(intopt, 5)
            case (6)            !  sobek balances, then also gpp balances, an general balances
                intopt = ibset(intopt, 3)
                intopt = ibset(intopt, 4)
            end select
        else
            ikey = index_in_array(lockey, defkeys)  ! look in the defaults
            if (ikey > 0) then
                write (file_unit, 1000) ikey, lockey
                intopt = ibclr(intopt, ikey - 1)
            else
                ierr2 = 1
            endif
        endif

        if (timon) call timstop(ithndl)
        return

        1000 format (' Keyword (', i2, ') detected: ', a)

    end subroutine check_integration_option

end module integration_options
