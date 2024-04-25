subroutine datumi(jaar      ,jdate    ,t         )
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
!    Function: Calculates the number of hours referred to
!              January 1, 00:00 of the year 'JAAR' from a given
!              date/time
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Global variables
!
    integer              , intent(in)  :: jaar   !!  Year
    integer, dimension(6), intent(in)  :: jdate  !!  Date and time
    real(hp)                           :: t      !!  Time in hours referred to January 1,
                                                 !!  00:00 of the year 'JAAR'
!
! Local variables
!
    integer                 :: i     ! Help var. 
    integer                 :: jhulp ! Help var. 
    integer                 :: mnd   ! Help var. for the month 
    real(hp)                :: rlen  ! Length of a year in hours 
    real(hp), dimension(12) :: rmd   ! The number of days of the cumulated counted months 
!
!! executable statements -------------------------------------------------------
!
    rmd(1) = 0.0d0
    rmd(2) = 31.0d0
    rmd(3) = 59.0d0
    rmd(4) = 90.0d0
    rmd(5) = 120.0d0
    rmd(6) = 151.0d0
    rmd(7) = 181.0d0
    rmd(8) = 212.0d0
    rmd(9) = 243.0d0
    rmd(10) = 273.0d0
    rmd(11) = 304.0d0
    rmd(12) = 334.0d0
    !
    jhulp = jdate(1)
    !
    ! Compute month definitions for leap-years
    ! Year divisible by 4, minus those centuries that are not divisible by 4. 
    !
    if (mod(jhulp, 4) == 0) then
       if (mod(jhulp, 100)/=0 .or. mod(jhulp, 400)==0) then
          do i = 3, 12
             rmd(i) = rmd(i) + 1.0d0
          enddo
       endif
    endif
    !
    mnd = jdate(2)
    t = rmd(mnd)*24.0d0 + real(jdate(3) - 1, hp)*24.0d0 + real(jdate(4), hp)          &
      & + real(jdate(5), hp)/60.0d0 + real(jdate(6), hp)/3600.0d0
    !
    ! Hypothetical case (jhulp = jdate(1) and jaar = jdate(1))
    !
    if (jhulp /= jaar) then
       rlen = 8760.0d0
       if (jhulp <= jaar) then
          if (mod(jhulp, 4) == 0) rlen = 8784.0d0
          t = t - rlen
       else
          if (mod(jaar, 4) == 0) rlen = 8784.0d0
          t = t + rlen
       endif
    endif
end subroutine datumi
