subroutine timdat(julday    ,timsec    ,idate    ,itime     )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
!!--description-----------------------------------------------------------------
!
!    Function:  returns date and time according actual time
!               in minutes and itdate (julian day)
!               in the form yyyymmdd  hhmmss
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_basics
    implicit none
!
! Global variables
!
    integer              :: idate  !!  Absolute date related to ITDATE and TIMSEC
    integer,  intent(out):: itime  !!  Absolute time related to ITDATE and TIMSEC
    integer,  intent(in) :: julday !  Description and declaration in inttim.igs
    real(hp), intent(in) :: timsec !  Description and declaration in inttim.igs
!
! Local variables
!
    integer :: icurtm
    integer :: iday
    integer :: ihou
    integer :: imin
    integer :: imo
    integer :: isec
    integer :: iy
    integer :: l
    integer :: localjulday
    integer :: n
    real(hp):: localtimsec
!
!! executable statements -------------------------------------------------------
!
    if (timsec < 0.0) then
       iday        = floor(timsec/86400.0_hp)
       !
       ! iday is negative
       ! decrease julday
       ! make timsec positive
       !
       localjulday = julday + iday
       localtimsec = timsec - iday * 86400.0_hp
    else
       localjulday = julday
       localtimsec = timsec
    endif
    !-----Define number of days, hours, minutes and seconds from TIMSEC
    !
    icurtm = nint(localtimsec)
    iday = icurtm/86400
    icurtm = icurtm - iday*86400
    ihou = icurtm/3600
    icurtm = icurtm - ihou*3600
    imin = icurtm/60
    icurtm = icurtm - imin*60
    isec = icurtm
    !
    !-----Convert julian day-number
    !
    l = localjulday + iday + 68569
    n = 4*l/146097
    l = l - (146097*n + 3)/4
    iy = 4000*(l + 1)/1461001
    l = l - 1461*iy/4 + 31
    imo = 80*l/2447
    iday = l - 2447*imo/80
    l = imo/11
    imo = imo + 2 - 12*l
    iy = 100*(n - 49) + iy + l
    !
    !-----Define integer values for time and date
    !
    itime = ihou*10000 + imin*100 + isec
    idate = abs(iy)*10000 + imo*100 + iday
    idate = sign(idate, iy)
end subroutine timdat
