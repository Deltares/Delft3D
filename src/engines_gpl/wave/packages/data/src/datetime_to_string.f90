function datetime_to_string(date, time) result (dtstring)
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
!!--description-----------------------------------------------------------------
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
use time_module, only : ymd2jul
use precision_basics
implicit none
!
! Global variables
!
character(15)             :: dtstring ! [yyyymmdd.hhmmss] julian date
integer      , intent(in) :: date     ! [yyyymmdd]
real(hp)     , intent(in) :: time     ! [sec]      seconds since date, 0:00:00 h
!
! Local variables
!
integer       :: idate
integer       :: iday
integer       :: ihour
integer       :: imin
integer       :: imon
integer       :: isec
integer       :: itime
integer       :: iyear
integer       :: julday
!
!! executable statements -------------------------------------------------------
!
   julday = ymd2jul(date)
   call timdat(julday, time, idate, itime)
   iyear = idate/10000
   imon  = (idate - iyear*10000)/100
   iday  = idate - iyear*10000 - imon*100
   ihour = itime/10000
   imin  = (itime - ihour*10000)/100
   isec  = itime - ihour*10000 - imin*100
   write (dtstring, '(I4,2I2.2,A1,3I2.2)') iyear, imon, iday, '.', ihour, imin, isec
end function datetime_to_string
