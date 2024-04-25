subroutine setcurrentdatetime(timnow, gdp)
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
! Print the time varying history data
! Selection is done using PRSHIS. For elements like
! ZCURW where KMAX must be > 1 this coupling between
! KMAX and PRSHIS is done in subroutine RDPRFL
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
!
! Global variables
!
    type(globdat),target :: gdp
    real(fp)             :: timnow
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)              , pointer :: hdt
    real(fp)              , pointer :: timsec
    real(fp)              , pointer :: timmin
    real(hp)              , pointer :: dtimmin
    real(fp)              , pointer :: timhr       ! Current timestep (in hours) TIMNOW * 2 * HDT / 3600.
    integer               , pointer :: julday
    character(24)         , pointer :: date_time
    integer, dimension(:) , pointer :: i_date_time
!
!
! Local variables
!
    integer       :: idate   ! Current date, based on JULDAY and TIMSEC
    integer       :: iday    ! Days in IDATE
    integer       :: ifrac   ! Seconds fraction (4 digits) in TIMSEC
    integer       :: ihour   ! Hours in ITIME
    integer       :: imin    ! Minutes in ITIME
    integer       :: imon    ! Months in IDATE
    integer       :: isec    ! Seconds in ITIME
    integer       :: itime   ! Current time without seconds fraction, based on JULDAY and TIMSEC
    integer       :: iyear   ! Years in IDATE
    integer(long) :: itimsec ! int(TIMSEC), + 1 when rounding the fraction to 4 digits results in exactly one second
!
!! executable statements -------------------------------------------------------
!
    hdt         => gdp%gdnumeco%hdt
    timsec      => gdp%gdinttim%timsec
    timmin      => gdp%gdinttim%timmin
    dtimmin     => gdp%gdinttim%dtimmin
    timhr       => gdp%gdinttim%timhr
    julday      => gdp%gdinttim%julday
    date_time   => gdp%gdinttim%date_time
    i_date_time => gdp%gdinttim%i_date_time
    !
    timsec  = timnow * 2.0_fp * hdt
    timmin  = timsec / 60.0_fp
    dtimmin = real(timsec, hp) / 60.0_hp
    timhr   = timsec / 3600.0_fp
    !
    itimsec = int(timsec,long)
    ifrac   = nint((timsec - real(itimsec,fp))*10000.0_fp)
    if (ifrac > 9999) then
       ifrac   = ifrac - 10000
       itimsec = itimsec + 1
    endif
    call timdat(julday, real(itimsec, fp), idate, itime)
    iyear = idate/10000
    imon  = (idate - iyear*10000)/100
    iday  = idate - iyear*10000 - imon*100
    ihour = itime/10000
    imin  = (itime - ihour*10000)/100
    isec  = itime - ihour*10000 - imin*100
    write (date_time, '(i4.4,2(a1,i2.2),a1,3(i2.2,a1),i4.4)')  &
        & iyear, '-', imon, '-', iday, ' ', ihour, ':', imin, ':', isec, '.', ifrac
    i_date_time(1) = iyear
    i_date_time(2) = imon
    i_date_time(3) = iday
    i_date_time(4) = ihour
    i_date_time(5) = imin
    i_date_time(6) = isec
end subroutine setcurrentdatetime
