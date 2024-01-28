subroutine varcon(fname     ,timmin    ,result    ,isdir     ,nres      , &
                & gdp       )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer  , pointer :: lundia
!
! Local parameters
!
    integer, parameter :: nresmx = 100
!
! Global variables
!
    integer                 , intent(in)  :: nres
    integer, dimension(nres), intent(in)  :: isdir
    real(fp)                , intent(in)  :: timmin
    real(fp), dimension(nres)             :: result
    character(*)            , intent(in)  :: fname
!
! Local variables
!
    integer                 :: i
    integer                 :: it
    integer                 :: iuntim
    integer                 :: ncol
    integer                 :: nt
    integer, external       :: newlun
    real(fp)                :: a
    real(fp)                :: b
    real(fp)                :: c
    real(fp)                :: c1
    real(fp)                :: c2
    real(fp)                :: s
    real(fp)                :: s1
    real(fp)                :: s2
    real(fp)                :: t1
    real(fp)                :: t2
    real(fp), dimension(nresmx) :: buff1
    real(fp), dimension(nresmx) :: buff2
    character(4)            :: blname
    character(256)          :: message
!
!! executable statements -------------------------------------------------------
!
    lundia    => gdp%gdinout%lundia
    !
    !     VARCON interpolates record of values from time series file
    !     TEKAL format; first column time in minutes
    !
    !     GLOBAL DATA
    !
    !     global data structure definition and access functions
    !
    !
    !     fname    input     filename time series file
    !     timmin   input     time in minutes
    !     result   output    array containing interpolated values
    !     isdir    input     mask array, 0 if scalar, 1 if direction (deg)
    !     nres     input     number of values on record
    !
    open (newunit=iuntim, file = fname, err = 999)
  100 continue
    read (iuntim, '(A)') blname
    if (blname(1:1)=='*') goto 100
    read (iuntim, *) nt, ncol
    if (ncol < nres + 1) then
       write (message,'(a,a)') 'Not enough columns in file ',trim(fname)
       call prterr(lundia, 'U021', message)
       call d3stop(1, gdp)          
    endif
    read (iuntim, *, err = 998) t1, (buff1(i), i = 1, nres)
    !
    do it = 1, nt - 1
       read (iuntim, *, err = 998) t2, (buff2(i), i = 1, nres)
       if (timmin <= t1) then
          !
          ! before start of series; take first value
          !
          do i = 1, nres
             result(i) = buff1(i)
          enddo
          goto 500
       elseif (timmin >= t2) then
          t1 = t2
          do i = 1, nres
             buff1(i) = buff2(i)
          enddo
       else
          if (t2 <= t1) then
             write (message,'(a,a)') 'Time series not monotonous in file ',trim(fname)
             call prterr(lundia, 'U021', message)
             call d3stop(1, gdp)          
          endif
          b = (timmin - t1)/(t2 - t1)
          a = 1.0 - b
          do i = 1, nres
             if (isdir(i) == 1) then
                c1 = cos(buff1(i)*degrad)
                c2 = cos(buff2(i)*degrad)
                s1 = sin(buff1(i)*degrad)
                s2 = sin(buff2(i)*degrad)
                c = c1*a + c2*b
                s = s1*a + s2*b
                result(i) = atan2(s, c)/degrad
                if (result(i) < 0.0) result(i) = result(i) + 360.0
             else
                result(i) = buff1(i)*a + buff2(i)*b
             endif
          enddo
          goto 500
       endif
    enddo
    !
    ! after end of series or only one record; take last value
    !
    do i = 1, nres
       result(i) = buff1(i)
    enddo
    !
  500 continue
    !
    close (iuntim)
    return
    !
  998 continue
    write (message,'(a,a)') 'Error reading file ',trim(fname)
    call prterr(lundia, 'U021', message)
    call d3stop(1, gdp)          
  999 continue
    write (message,'(a,a)') 'Error opening file ',trim(fname)
    call prterr(lundia, 'U021', message)
    call d3stop(1, gdp)          
end subroutine varcon
