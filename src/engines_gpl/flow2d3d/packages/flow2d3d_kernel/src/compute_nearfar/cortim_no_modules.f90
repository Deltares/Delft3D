subroutine cortim_no_modules(lun    ,no_modules,no_val)
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
!  $Id: cortim_no_modules.f90 5888 2016-02-24 10:14:54Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/cortim_no_modules.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Determine the number of cormix modules used
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!
! Global variables
!
    integer                                                    , intent(in)  :: lun
    integer                                                    , intent(out) :: no_modules
    integer                                                    , intent(out) :: no_val
!
! Local variables
!
    integer                                     :: ix1
    integer                                     :: ix2
    integer                                     :: ix3
    integer                                     :: iocond
    real(fp)                                    :: rdummy
    character*256                               :: record
    logical                                     :: found
!
!! executable statements -------------------------------------------------------
!
    no_modules = 0
    no_val     = 0
    iocond     = 0

    do while (iocond == 0)
       read (lun,'(a256)',iostat = iocond) record
       call small(record,len(record))

       ix1 = index (record,'begin')
       ix2 = index (record,'mod')

       if (ix1 /= 0 .and. ix2 /= 0) then
          found = .false.
          do while (.not. found)
             read (lun,'(a256)',iostat = iocond) record
             call small(record,len(record))
             ix1 = index (record,'x        y       z')
             ix2 = index (record,'end')
             ix3 = index (record,'mod')
             if (ix1 /= 0) then
                found = .true.
                no_modules = no_modules + 1
                ix2 = 0
                ix3 = 0
                do while (ix2 == 0 .and. ix3 == 0)
                   read (lun,'(a256)',iostat = iocond) record
                   call small (record,len(record))
                   ix2 = index(record,'end')
                   ix3 = index(record,'mod')
                   read (record,*,iostat = iocond) rdum, rdum, rdum, rdum
                   if (iocond == 0) then
                      no_val = no_val + 1
                   endif
                enddo
                iocond = 0
             elseif (ix2 /= 0 .and. ix3 /=0) then
                found = .true.
             endif
          enddo
       endif
    enddo
end subroutine cortim_no_modules
