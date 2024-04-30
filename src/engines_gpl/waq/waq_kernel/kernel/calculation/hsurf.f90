!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!
module m_hsurf
    use m_waq_precision
    use m_string_utils

    implicit none

contains

    subroutine hsurf    (noseg, nopa, paname, param, nosfun, &
            sfname, segfun, surface, file_unit_list)

        !     Deltares Software Centre

        !>\File
        !>           Set values of horizontal surface array.

        !     Created             :    September 2012 by Christophe Thiange

        !     Logical unitnumbers : file_unit_list     = number of monitoring file

        !     Subroutines called  : none

        use timers
        implicit none

        !     Parameters          :
        !     type     kind  function         name                      description

        integer(kind = int_wp), intent(in) :: noseg                   !< number of computational volumes
        integer(kind = int_wp), intent(in) :: nopa                    !< number of parameters
        character(20), intent(in) :: paname(nopa)          !< names of the parameters
        real(kind = real_wp), intent(in) :: param (nopa, noseg)    !< parameter values
        integer(kind = int_wp), intent(in) :: nosfun                  !< number of segment functions
        character(20), intent(in) :: sfname(nosfun)          !< names of the segment functions
        real(kind = real_wp), intent(in) :: segfun(noseg, nosfun)   !< segment function values
        real(kind = real_wp), intent(inout) :: surface(noseg)          !< horizontal surface
        integer(kind = int_wp), intent(in) :: file_unit_list                     !< logical unit number monitoring file


        !     local variables

        logical, save :: first = .true.  ! true if first time step
        integer(kind = int_wp), save :: indx            ! index of the surf variable in the array
        integer(kind = int_wp), save :: mode            ! -1 segment functions, +1 parameters, 0 none
        integer(kind = int_wp), save :: ithandl         ! timer handle
        data       ithandl  /0/
        if (timon) call timstrt ("hsurf", ithandl)

        !         see if the surface is available

        if (first) then
            first = .false.
            indx = index_in_array('SURF      ', paname)
            if (indx > 0) then                           ! SURF is found
                mode = 1
                surface(:) = param(indx, 1:noseg)
            else
                indx = index_in_array('SURF      ', sfname)
                if (indx > 0) then
                    mode = -1
                else
                    surface = 1.0
                    write(file_unit_list, 2000)
                endif
            endif
        endif
        if (mode ==  -1) then
            surface(:) = segfun(1:noseg, indx)
        endif

        if (timon) call timstop (ithandl)
        return
        2000 format (' WARNING  : could not find horizontal surface; using value of 1.0 m.')
    end

end module m_hsurf
