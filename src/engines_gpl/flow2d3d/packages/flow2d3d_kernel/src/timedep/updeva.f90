subroutine updeva(luneva    ,timnow    ,dt        ,inteva    ,first     , &
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
!
!    Function: Reads the time dependent rain/evaporation pars.
!              FROM FILE (if FLEVA = true)
! Method used: - Time dependent data is read and stored
!              - The first time (FIRST = true) data are read and
!                stored depending on value of KEVA. In the times
!                that follows data are read and stored in
!                D"value".
!              - Subsequently time increment values are calcula-
!                ted (if INTEVA = true) and restored in arrays
!                D"value".
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: it0eva
    integer                , pointer :: it1eva
    integer                , pointer :: keva
    integer                , pointer :: lundia
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: evapor
    real(fp)               , pointer :: devapo
    real(fp)               , pointer :: precipt
    real(fp)               , pointer :: dpreci
    real(fp)               , pointer :: train
    real(fp)               , pointer :: dtrain
!
! Global variables
!
    integer  , intent(in)  :: luneva !  Description and declaration in luntmp.igs
    logical                :: first  !!  Flag = TRUE in case a time-dependent file is read for the 1-st time
    logical  , intent(in)  :: inteva !!  Interpolation method between consecutive data:
                                     !!    N = No     interpolation.
                                     !!    Y = Linear interpolation.
    real(fp)               :: dt     !  Description and declaration in esm_alloc_real.f90
    real(fp) , intent(in)  :: timnow !!  Current timestep (multiples of dt)
!
! Local variables
!
    integer                        :: i
    integer                        :: iocond      ! Flag for reading errors = 0 No error < 0 End-Of-File reached > 0 Reading error 
    integer                        :: it
    integer                        :: iteva       ! Read timestep number = multiple of DT The checking of RTEVA = DT * ITEVA is performed in RDEVA or RDTDF (TDATOM) 
    logical                        :: dtn
    real(fp)                       :: rinc        ! Increment values at each timestep for the wind data 
    real(fp)                       :: rteva       ! Time read from the rain/evaporation data 
    real(fp)                       :: t
    real(fp)                       :: tlread      ! Last time read from file (in minutes) 
    real(fp), dimension(3)         :: rval        ! Help array to read values 
    character(200)                 :: errmsg      ! String containing error message 
!
!! executable statements -------------------------------------------------------
!
    evapor      => gdp%gdheat%evapor
    devapo      => gdp%gdheat%devapo
    precipt     => gdp%gdheat%precipt
    dpreci      => gdp%gdheat%dpreci
    train       => gdp%gdheat%train
    dtrain      => gdp%gdheat%dtrain
    rhow        => gdp%gdphysco%rhow
    it0eva      => gdp%gdinttim%it0eva
    it1eva      => gdp%gdinttim%it1eva
    keva        => gdp%gdtricom%keva
    lundia      => gdp%gdinout%lundia
    !
    do i = 1, 3
       rval(i) = 0.0_fp
    enddo
    !
    ! Read evaperature input for the consecutive times
    !
    ! -->
    !
    ! When End-Of-File or error occures for reading TMP_eva file
    ! stop program. In the future the program should be able
    ! to go on and using latest read value as end value
    !
  100 continue
    tlread = real(it0eva,fp)*dt
    read (luneva, iostat = iocond) rteva, (rval(i), i = 1, 3)
    if (iocond/=0) call eoferr('eva', lundia, iocond, tlread, gdp)
    !
    ! Calculate timestep number
    !
    iteva = nint(rteva/dt)
    if (dtn(iteva, rteva, dt)) then
       write (errmsg, '(a,f10.4)') 'Timeva = ', rteva
       call prterr(lundia, 'S044', trim(errmsg))
    endif
    !
    if (real(iteva,fp)>timnow) first = .false.
    !
    if (first) then
       it0eva = iteva
       !
       ! Calculate evaperature parameters
       ! transform unit from mm/hour to m/sec
       !
       precipt = rval(1) / 3600000.0_fp
       if (rval(2) < -998.0_fp) then
          keva = 1
          call prterr(lundia, 'G051', 'Missing value found for evaporation in input file. The evaporative mass flux will be calculated internally.')
       else
          evapor = rval(2) * rhow / 3600000.0_fp
       endif
       train = rval(3)
       goto 100
    ! <--
    !
    else
       it1eva = iteva
       !
       ! Calculate evaperature parameters
       ! transform unit from mm/hour to m/sec
       !
       dpreci = rval(1) / 3600000.0_fp
       if (rval(2) < -998.0_fp) then
          if (keva /= 1) then
             write (errmsg, '(a,f10.4)') 'Cannot handle evaporation switching from a real value to a missing value'
             call prterr(lundia, 'P004', trim(errmsg))
             call d3stop(1, gdp)
          endif
       else
          if (keva == 1) then
             write (errmsg, '(a,f10.4)') 'Cannot handle evaporation switching from a missing value to a real value'
             call prterr(lundia, 'P004', trim(errmsg))
             call d3stop(1, gdp)
          endif
          devapo = rval(2) * rhow / 3600000.0_fp
       endif
       dtrain = rval(3)
       !
       ! Interpolate in time
       !
       if (inteva) then
          rinc = (it1eva-it0eva) * 2.00_fp
          dpreci = (dpreci - precipt) / rinc
          devapo = (devapo - evapor) / rinc
          dtrain = (dtrain - train)  / rinc
       endif
    endif
end subroutine updeva
