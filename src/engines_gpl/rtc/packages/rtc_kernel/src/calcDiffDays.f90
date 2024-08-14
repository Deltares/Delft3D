!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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

! function returns the number of days between two dates
! uses function julian()
! precondition: firstEvent occurs before secondEvent

Double Precision function calcDiffDays(firstEvent, secondEvent, evstrt, Nevnt)

  implicit none

  Integer firstEvent, secondEvent, date1, date2, time1, time2, NEvnt
  INTEGER      EVSTRT(NEVNT,6)

  double precision julian, jDate1, jDate2, diff

  date1 = (evstrt(firstEvent, 1) * 10000) + &
          (evstrt(firstEvent, 2) * 100) + &
           evstrt(firstEvent, 3)
  time1 =  evstrt(firstEvent, 4) * 10000. + &
           evstrt(firstEvent, 5) * 100.

  date2 = (evstrt(secondEvent, 1) * 10000) + &
          (evstrt(secondEvent, 2) * 100) + &
           evstrt(secondEvent, 3)
  time2 =  evstrt(secondEvent, 4) * 10000. + &
           evstrt(secondEvent, 5) * 100.

  jDate1 = julian(date1, time1)
  jDate2 = julian(date2, time2)
  diff = jDate2 - jDate1
! write(*,*) ' CalcdiffDays', firstevent, secondevent, diff
! write(*,*) ' CalcdiffDays', DATE1, DATE2

  calcDiffDays = diff
  return

end function calcDiffDays
