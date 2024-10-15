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
module m_exchange_values
    use m_waq_precision
    use m_string_utils
    use timers

    implicit none

contains


    subroutine exchange_values(name, nosss, value, num_constants, num_spatial_parameters, &
            num_time_functions, num_spatial_time_fuctions, const, coname, param, &
            paname, funcs, funame, sfuncs, sfname, &
            lget, ierr)

        character(len = 20) :: name, coname(*), paname(*), funame(*), sfname(*)
        real(kind = real_wp) :: value(nosss), const(num_constants), param (num_spatial_parameters, nosss), &
                funcs(num_time_functions), sfuncs(nosss, num_spatial_time_fuctions)
        logical :: lget
        integer(kind = int_wp) :: nosss, num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, ierr

        ! local
        integer(kind = int_wp) :: indx
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("values", ithandl)

        ierr = 1
        indx = index_in_array(name, sfname(:num_spatial_time_fuctions))
        if (indx > 0) then
            if (lget) then
                value(1:nosss) = sfuncs(1:nosss, indx)
            else
                sfuncs(1:nosss, indx) = value(1:nosss)
            endif
            ierr = 0
            goto 100
        endif
        indx = index_in_array(name, paname (:num_spatial_parameters))
        if (indx > 0) then
            if (lget) then
                value(1:nosss) = param(indx, 1:nosss)
            else
                param(indx, 1:nosss) = value(1:nosss)
            endif
            ierr = 0
            goto 100
        endif
        indx = index_in_array(name, funame (:num_time_functions))
        if (indx > 0) then
            if (lget) then
                value(1:nosss) = funcs(indx)
                ierr = 0
            endif
            goto 100
        endif
        indx = index_in_array(name, coname (:num_constants))
        if (indx > 0) then
            if (lget) then
                value(1:nosss) = const(indx)
                ierr = 0
            endif
            goto 100
        endif

        100 continue
        if (timon) call timstop (ithandl)

    end subroutine exchange_values

end module m_exchange_values
