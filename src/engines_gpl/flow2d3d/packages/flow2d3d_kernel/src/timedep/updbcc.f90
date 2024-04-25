subroutine updbcc(lunbcc    ,lundia    ,first     ,itbcc     ,ito       , &
                & istsc     ,timnow    ,itfinish  ,timscl    , &
                & nto       ,kmax      ,lstsc     ,procbc    ,tprofc    , &
                & zstep     ,gdp       )
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
!    Function: Reads the time dependent constituent BC from file
!              (if flbcc = TRUE / LSTSC > 0)
! Method used: - Time dependent value is read and   stored in
!                array PROCBC (K,N,L).
!              - The first time (FIRST = TRUE) data are read for
!                K=1--4. In the following times data are read for
!                K=5--8.
!              - Subsequently time increment values are calcula-
!                ted and restored in PROCBC (K,N,L) with K=5--8
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
    real(fp) , pointer :: scalef
    real(fp) , pointer :: tstop
    real(fp) , pointer :: dt
    real(fp) , pointer :: tunit
!
! Global variables
!
    integer                             , intent(in)  :: istsc    !!  Index number of constituent
    integer                             , intent(in)  :: ito      !!  Index number of open boundary loc.
    integer                             , intent(in)  :: itfinish !  Description and declaration in inttim.igs
    integer                             , intent(in)  :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                             , intent(in)  :: lstsc    !  Description and declaration in dimens.igs
    integer                             , intent(in)  :: lunbcc   !  Description and declaration in luntmp.igs
    integer                                           :: lundia   !  Description and declaration in inout.igs
    integer                             , intent(in)  :: nto      !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, nto, lstsc)                 :: itbcc    !  Description and declaration in esm_alloc_int.f90
    logical                                           :: first    !!  Flag = TRUE in case a time-dependent
                                                                  !!  file is read for the 1-st time
    real(fp)                            , intent(in)  :: timnow   !!  Current timestep (multiples of dt)
    real(fp)                            , intent(in)  :: timscl   !!  Multiple factor to create minutes
                                                                  !!  from read times
    real(fp), dimension(2, nto, lstsc)                :: zstep    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(4, nto, kmax, lstsc)          :: procbc   !  Description and declaration in esm_alloc_real.f90
    character(10), dimension(nto, lstsc), intent(in)  :: tprofc   !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer       :: iocond ! Flag for reading errors = 0 No error < 0 End-Of-File reached > 0 Reading error 
    integer       :: ittime ! Help variable containing timestep number RTBCC * SCALEF / SDT 
    integer       :: k      ! Loop counter over KMAX 
    integer       :: nread  ! Number of boundary values to read depending on TPROFC 
    logical       :: last
    real(fp)      :: rinc   ! Increment values at each timestep for the open boundary conditions 
    real(fp)      :: rtbcc  ! Time read from the open boundary file (multiples of sdt) 
    real(fp)      :: rttime ! Help variable containing time RTBCC * SCALEF 
    real(fp)      :: sdt    ! dt*tunit/60
    real(fp)      :: tlread ! Last time read from file (in minutes) 
    character(16) :: fmtbcc
    character(20) :: errmsg ! String containing error message 
!
!! executable statements -------------------------------------------------------
!
    scalef  => gdp%gdupdbcc%scalef
    tstop   => gdp%gdexttim%tstop
    dt      => gdp%gdexttim%dt
    tunit   => gdp%gdexttim%tunit
    !
    !
    ! function definition
    !     DISABLED FOR DD
    !
    !      dtn (it,t ,dt)= abs (it * dt - t) .gt. (0.1 * dt)
    !
    ! Initialization local parameters
    !
    sdt  = dt*(tunit/60.0)
    last = .false.
    if (first) scalef = timscl
    !
    ! Define reading format
    ! Time is read with format F12.4 rest with G14.6
    !
    fmtbcc = '(f16.4,...g14.6)'
    if (tprofc(ito, istsc)(:7)=='uniform') then
       nread = 2
    elseif (tprofc(ito, istsc)(:6)=='linear') then
       nread = 4
    elseif (tprofc(ito, istsc)(:4)=='step') then
       nread = 5
    else
       nread = 2*kmax
    endif
    write (fmtbcc(8:10), '(i3.3)') nread
    !
    ! Read amplitudes at end A and B for the consecutive times
    ! as long as time read < time now
    ! Note: The process input will always be interpolated between two
    !       consecutive times. For the profile and depth (in M) the
    !       latest read values will be used !!
    !
    !==>
   10 continue
    itbcc(5, ito, istsc) = itbcc(5, ito, istsc) + 1
    !
    ! End of file and not yet end of computation
    !
    if (itbcc(5, ito, istsc)>itbcc(4, ito, istsc)) then
       last = .true.
       itbcc(5, ito, istsc) = itbcc(4, ito, istsc)
       errmsg = 'open bound = '
       write (errmsg(14:16), '(i3)') ito
       call prterr(lundia    ,'V098'    ,errmsg(:16)          )
    !
    endif
    !
    ! When End-Of-File or error occurs for reading TMP_bcc file:
    ! stop program. In the future the program should be able
    ! to go on and using latest read value as end value
    !
    if (first) then
       tlread = real(itbcc(1, ito, istsc),fp)*sdt/scalef
    else
       tlread = real(itbcc(2, ito, istsc),fp)*sdt/scalef
    endif
    !
    ! Read boundary values depending on value of TPROFC
    !
    if (tprofc(ito, istsc)(:7)=='uniform') then
       read (lunbcc, fmtbcc, rec = itbcc(5, ito, istsc), iostat = iocond) &
          & rtbcc, procbc (3, ito, 1, istsc) , procbc (4, ito, 1, istsc)
    elseif (tprofc(ito, istsc)(:6)=='linear') then
       read (lunbcc, fmtbcc, rec = itbcc(5, ito, istsc), iostat = iocond) &
          & rtbcc, procbc (3, ito, 1, istsc) , procbc (3, ito, kmax, istsc) , procbc  &
          & (4, ito, 1, istsc) , procbc (4, ito, kmax, istsc)
    elseif (tprofc(ito, istsc)(:4)=='step') then
       read (lunbcc, fmtbcc, rec = itbcc(5, ito, istsc), iostat = iocond) &
          & rtbcc, procbc (3, ito, 1, istsc) , procbc (3, ito, kmax, istsc) , procbc  &
          & (4, ito, 1, istsc) , procbc (4, ito, kmax, istsc) , zstep (2, ito, istsc)
    else
       read (lunbcc, fmtbcc, rec = itbcc(5, ito, istsc), iostat = iocond) &
          & rtbcc, ( procbc (3, ito, k, istsc) , k = 1, kmax), ( procbc (4, ito,  &
          & k, istsc) , k = 1, kmax)
    endif
    if (iocond/=0) call eoferr('bcc', lundia, iocond, tlread, gdp)
    !
    ! Test read time RTBCC as multiple of SDT (/ SCALEF)
    !
    rttime = real(rtbcc*scalef,fp)
    ittime = nint(rttime/sdt)
    if (abs(ittime*sdt - rttime)>(0.1*sdt)) then
       write (errmsg, '(a,f10.4)') 'Timbcc = ', rtbcc
       call prterr(lundia    ,'S044'    ,errmsg    )
    endif
    !
    ! Define Time as multiple of SDT, where RTBCC is in SCALEF minutes
    !
    rtbcc = real(nint(rtbcc*scalef/sdt),fp)
    !
    ! Redefine time if LAST = .true.
    ! TIMNOW is per definition < ITFINISH
    !
    if (last) rtbcc = real(itfinish,fp)
    !
    ! Test if ITBCT (1,ITO) or ITBCT (2,ITO) part
    ! should be executed
    !
    if (rtbcc>timnow) first = .false.
    !
    if (first) then
       !
       ! Define time step for ITBCC (1,ITO,ISTSC)
       !
       itbcc(1, ito, istsc) = nint(rtbcc)
       !
       ! Copy open boundary values read to "partner" array
       !
       zstep(1, ito, istsc) = zstep(2, ito, istsc)
       do k = 1, kmax
          procbc(1, ito, k, istsc) = procbc(3, ito, k, istsc)
          procbc(2, ito, k, istsc) = procbc(4, ito, k, istsc)
       enddo
       !
       ! Read new time step
       !
       goto 10
    !<==
    else
       !
       ! Define time step for ITBCC (2,ITO,ISTSC)
       !
       itbcc(2, ito, istsc) = nint(rtbcc)
       !
       ! Calculate increment with linear interpolation
       !
       rinc = (itbcc(2, ito, istsc) - itbcc(1, ito, istsc))*2.0
       do k = 1, kmax
          procbc(3, ito, k, istsc) = (procbc(3, ito, k, istsc) - procbc(1, ito, &
                                   & k, istsc))/rinc
          procbc(4, ito, k, istsc) = (procbc(4, ito, k, istsc) - procbc(2, ito, &
                                   & k, istsc))/rinc
       enddo
    endif
end subroutine updbcc
