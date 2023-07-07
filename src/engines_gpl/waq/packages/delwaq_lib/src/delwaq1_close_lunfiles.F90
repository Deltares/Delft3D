!!  Copyright (C)  Stichting Deltares, 2012-2023.
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


!>\file
!>                    delwaq1_close_lunfiles

subroutine delwaq1_close_lunfiles()
    use m_delwaq1_data
      
    implicit none
    
    !
    ! Close all open LUN files
    !
    do i = 1, nlun
        inquire (unit=lun(i), opened=unitop)
        if (unitop) then
            close (unit = lun(i))
        endif
    end do

    if ( timon ) then
      call timstop ( ithndl )
      call timdump ( TRIM(RUNID)//'-delwaq1-timers.out' )
    endif

end subroutine delwaq1_close_lunfiles