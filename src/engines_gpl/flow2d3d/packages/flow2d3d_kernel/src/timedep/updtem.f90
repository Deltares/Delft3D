subroutine updtem(luntem    ,ktemp     ,timnow    ,dt        ,inttem    , &
                & first     ,gdp       )
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
!    Function: Reads the time dependent temperature parameters
!              from file (if FLTEM = true)
! Method used: - Time dependent data is read and stored
!              - The first time (FIRST = true) data are read and
!                stored depending on value of KTEMP. In the times
!                that follows data are read and stored in
!                D"value".
!              - Subsequently time increment values are calcula-
!                ted (if INTTEM = true) and restored in arrays
!                D"value".
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
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: fclou
    real(fp)               , pointer :: rhum
    real(fp)               , pointer :: rhum0
    real(fp)               , pointer :: rhum1
    real(fp)               , pointer :: tdryb
    real(fp)               , pointer :: tdryb0
    real(fp)               , pointer :: tdryb1
    real(fp)               , pointer :: qsun
    real(fp)               , pointer :: qsun0
    real(fp)               , pointer :: qsun1
    real(fp)               , pointer :: qradin
    real(fp)               , pointer :: qradin0
    real(fp)               , pointer :: qradin1
    real(fp)               , pointer :: tback
    real(fp)               , pointer :: tback0
    real(fp)               , pointer :: tback1
    real(fp)               , pointer :: tair
    real(fp)               , pointer :: tair0
    real(fp)               , pointer :: tair1
    real(fp)               , pointer :: cfclou
    real(fp)               , pointer :: cfclou0
    real(fp)               , pointer :: cfclou1
    real(fp)               , pointer :: vapres
    real(fp)               , pointer :: vapres0
    real(fp)               , pointer :: vapres1
    integer                , pointer :: ivapop
    logical                , pointer :: solrad_read
    integer                , pointer :: lundia
    integer                , pointer :: it0tem
    integer                , pointer :: it1tem
!
! Global variables
!
    integer , intent(in)  :: ktemp  !  Description and declaration in tricom.igs
    integer , intent(in)  :: luntem !  Description and declaration in luntmp.igs
    logical               :: first  !!  Flag = TRUE in case a time-dependent
                                    !!  file is read for the 1-st time
    logical , intent(in)  :: inttem !!  Interpolation method between consecu-
                                    !!  tive temperature  data:
                                    !!    N = No     interpolation.
                                    !!    Y = Linear interpolation.
    real(fp)              :: dt     !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: timnow !!  Current timestep (multiples of dt)
!
! Local variables
!
    integer                :: i
    integer                :: iocond  ! Flag for reading errors = 0 No error < 0 End-Of-File reached > 0 Reading error 
    integer                :: it
    integer                :: ittem   ! Read timestep number = multiple of DT The checking of RTTEM = DT * ITTEM is performed in RDHEAT or RDTDF (TDATOM) 
    integer                :: nval
    logical                :: dtn
    real(fp)               :: rinc    ! Increment values at each timestep for the wind data 
    real(fp)               :: rttem   ! Time read from the temperature data file (real time related to ITDATE and in same units as DT re-defined in multiples of DT) 
    real(fp)               :: t
    real(fp)               :: tlread  ! Last time read from file (in minutes) 
    real(fp), dimension(4) :: rval    ! Help array to read values 
    character(20)          :: errmsg  ! String containing error message 
!
!! executable statements -------------------------------------------------------
!
    fclou       => gdp%gdheat%fclou
    rhum        => gdp%gdheat%rhum
    rhum0       => gdp%gdheat%rhum0
    rhum1       => gdp%gdheat%rhum1
    tdryb       => gdp%gdheat%tdryb
    tdryb0      => gdp%gdheat%tdryb0
    tdryb1      => gdp%gdheat%tdryb1
    qsun        => gdp%gdheat%qsun
    qsun0       => gdp%gdheat%qsun0
    qsun1       => gdp%gdheat%qsun1
    qradin      => gdp%gdheat%qradin
    qradin0     => gdp%gdheat%qradin0
    qradin1     => gdp%gdheat%qradin1
    tback       => gdp%gdheat%tback
    tback0      => gdp%gdheat%tback0
    tback1      => gdp%gdheat%tback1
    tair        => gdp%gdheat%tair
    tair0       => gdp%gdheat%tair0
    tair1       => gdp%gdheat%tair1
    cfclou      => gdp%gdheat%cfclou
    cfclou0     => gdp%gdheat%cfclou0
    cfclou1     => gdp%gdheat%cfclou1
    vapres      => gdp%gdheat%vapres
    vapres0     => gdp%gdheat%vapres0
    vapres1     => gdp%gdheat%vapres1
    ivapop      => gdp%gdheat%ivapop
    solrad_read => gdp%gdheat%solrad_read
    lundia      => gdp%gdinout%lundia
    it0tem      => gdp%gdinttim%it0tem
    it1tem      => gdp%gdinttim%it1tem
    !
    do i = 1, 4
       rval(i) = 0.0_fp
    enddo
    !
    nval = 3
    if (ktemp  == 3) then
       nval = 1
    endif
    if (ivapop == 1 .or. solrad_read) then
       nval = 4
    endif
    !
    ! Read temperature input for the consecutive times
    !
    ! -->
    !
    ! When End-Of-File or error occures for reading TMP_tem file
    ! stop program. In the future the program should be able
    ! to go on and using latest read value as end value
    !
  100 continue
    tlread = real(it0tem,fp) * dt
    read (luntem, iostat = iocond) rttem, (rval(i), i = 1, nval)
    if (iocond /= 0) call eoferr('tem', lundia, iocond, tlread, gdp)
    !
    ! Calculate timestep number
    !
    ittem = nint(rttem/dt)
    if (dtn(ittem, rttem, dt)) then
       write (errmsg, '(a,f10.4)') 'Timtem = ', rttem
       call prterr(lundia    ,'S044'    ,errmsg    )
    endif
    !
    if (real(ittem,fp) > timnow) first = .false.
    !
    if (first) then
       it0tem = ittem
       !
       ! Define temperature parameters
       !
       if (ktemp==1) then
          rhum0   = rval(1)
          tdryb0  = rval(2)
          qsun0   = rval(3)
       elseif (ktemp==2) then
          rhum0   = rval(1)
          tdryb0  = rval(2)
          qradin0 = rval(3)
       elseif (ktemp==3) then
          tback0  = rval(1)
       elseif (ktemp==4) then
          rhum0   = rval(1)
          tair0   = rval(2)
          qsun0   = rval(3)
          vapres0 = rval(4)
       elseif (ktemp==5) then
          rhum0   = rval(1)
          tair0   = rval(2)
          cfclou0 = rval(3)/100.0_fp
          if (solrad_read) then
             qradin0 = rval(4)
          endif
       else
       endif
       goto 100
    ! <-- read next record in file
    else
       it1tem = ittem
       !
       ! Define temperature parameters
       !
       if (ktemp==1) then
          rhum1   = rval(1)
          tdryb1  = rval(2)
          qsun1   = rval(3)
       elseif (ktemp==2) then
          rhum1   = rval(1)
          tdryb1  = rval(2)
          qradin1 = rval(3)
       elseif (ktemp==3) then
          tback1  = rval(1)
       elseif (ktemp==4) then
          rhum1   = rval(1)
          tair1   = rval(2)
          qsun1   = rval(3)
          vapres1 = rval(4)
       elseif (ktemp==5) then
          rhum1   = rval(1)
          tair1   = rval(2)
          cfclou1 = rval(3)/100.0_fp
          if (solrad_read) then
             qradin1 = rval(4)
          endif
       else
       endif
    endif
end subroutine updtem
