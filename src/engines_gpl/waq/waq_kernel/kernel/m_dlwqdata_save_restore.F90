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

!     Restore the local persistent variables from the derived type
!
module m_dlwqdata_save_restore

    use m_waq_precision
    use delwaq2_data
    use m_waq_data_structure
    use variable_declaration
    use m_timer_variables
    use m_waq_memory_dimensions

    implicit none

    !type(delwaq_data), target :: dlwqd                !< derived type for persistent storage
    integer(kind = int_wp) :: itime
    integer(kind = int_wp) :: ifflag
    integer(kind = int_wp) :: iaflag
    integer(kind = int_wp) :: ibflag
    integer(kind = int_wp) :: nddim
    integer(kind = int_wp) :: nvdim
    integer(kind = int_wp) :: nosss
    integer(kind = int_wp) :: noqtt
    integer(kind = int_wp) :: noqt
    integer(kind = int_wp) :: nopred
    integer(kind = int_wp) :: ithandl
    logical :: litrep
    logical :: ldummy
    integer(kind = int_wp) :: inwtyp
    integer(kind = int_wp) :: nowarn
    integer(kind = int_wp) :: ioptzb
    logical :: forester
    logical :: updatr
    integer(kind = int_wp) :: lleng
    real(kind = dp) :: tol
    logical :: lstrec
    integer(kind = int_wp) :: itimel

contains

    !> Saves data from the (global) individual separate variables into the DELWAQ data structure
    subroutine dlwqdata_save(dlwqd)

        type(delwaq_data), target, intent(inout) :: dlwqd                !< derived type for persistent storage

        dlwqd%ii = ii
        dlwqd%in = in
        dlwqd%itime = itime
        dlwqd%ifflag = ifflag
        dlwqd%iaflag = iaflag
        dlwqd%ibflag = ibflag
        dlwqd%nddim = nddim
        dlwqd%nvdim = nvdim
        dlwqd%nosss = nosss
        dlwqd%noqtt = noqtt
        dlwqd%noqt = noqt
        dlwqd%nopred = nopred
        dlwqd%itimel = itimel
        dlwqd%lstrec = lstrec
        dlwqd%ithandl = ithandl
        dlwqd%litrep = litrep
        dlwqd%ldummy = ldummy
        dlwqd%inwtyp = inwtyp

        dlwqd%otime = otime
        dlwqd%deltim = deltim
        dlwqd%tscale = tscale

        dlwqd%nowarn = nowarn
        dlwqd%ioptzb = ioptzb
        dlwqd%forester = forester
        dlwqd%updatr = updatr

        dlwqd%lleng = lleng

        dlwqd%tol = tol

        dlwqd%iwstkind => iwstkind
        dlwqd%iexseg => iexseg
        dlwqd%iknmkv => iknmkv

        !     DLWQD%GRIDPS    - no need!

        call copy_time_data(dlwqd, .true.)

    end subroutine dlwqdata_save


    !> Restores the data from the DELWAQ data structure into the (global) individual separate variables
    subroutine dlwqdata_restore(dlwqd)

        type(delwaq_data), target, intent(inout) :: dlwqd                !< derived type for persistent storage

        in = dlwqd%in
        ii = dlwqd%ii
        itime = dlwqd%itime
        ifflag = dlwqd%ifflag
        iaflag = dlwqd%iaflag
        ibflag = dlwqd%ibflag
        nddim = dlwqd%nddim
        nvdim = dlwqd%nvdim
        nosss = dlwqd%nosss
        noqtt = dlwqd%noqtt
        noqt = dlwqd%noqt
        nopred = dlwqd%nopred
        itimel = dlwqd%itimel
        ithandl = dlwqd%ithandl
        litrep = dlwqd%litrep
        lstrec = dlwqd%lstrec
        ldummy = dlwqd%ldummy
        inwtyp = dlwqd%inwtyp

        otime = dlwqd%otime
        deltim = dlwqd%deltim
        tscale = dlwqd%tscale

        nowarn = dlwqd%nowarn
        ioptzb = dlwqd%ioptzb
        forester = dlwqd%forester
        lleng = dlwqd%lleng
        updatr = dlwqd%updatr

        tol = dlwqd%tol

        iwstkind => dlwqd%iwstkind
        iexseg => dlwqd%iexseg
        iknmkv => dlwqd%iknmkv

        call copy_time_data(dlwqd, .false.)

    end subroutine dlwqdata_restore

end module m_dlwqdata_save_restore
