subroutine upddis(lundis    ,lundia    ,sferic    ,itdis     , &
                & isrc      ,nm        ,grdang    ,timnow    ,dt        , &
                & itfinish  ,timscl    ,nsrc      ,lstsc     ,j         , &
                & nmmaxj    ,dismmt    ,alfas     , &
                & disch0    ,disch1    ,rint0     ,rint1     , &
                & umdis0    ,umdis1    ,vmdis0    ,vmdis1    ,gdp       )
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
!    Function: Reads the time dependent discharges from FILE
!              for discharge N (if fldis = TRUE / NSRC > 0)
! Method used: - Time dependent value is read and stored.
!
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
    real(fp) , pointer :: scalef
!
! Global variables
!
    integer                                   , intent(in)  :: isrc     !!  Index number of discharge location
    integer                                   , intent(in)  :: itfinish !  Description and declaration in inttim.igs
    integer                                                 :: j        !!  Begin pointer for arrays which have
                                                                        !!  been transformed into 1D arrays.
                                                                        !!  Due to the shift in the 2nd (M-)
                                                                        !!  index, J = -2*NMAX + 1
    integer                                   , intent(in)  :: lstsc    !  Description and declaration in dimens.igs
    integer                                                 :: lundia   !  Description and declaration in inout.igs
    integer                                   , intent(in)  :: lundis   !  Description and declaration in luntmp.igs
    integer                                   , intent(in)  :: nm       !!  N,M index for discharge location
    integer                                                 :: nmmaxj   !  Description and declaration in dimens.igs
    integer                                   , intent(in)  :: nsrc     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, nsrc)                             :: itdis    !  Description and declaration in esm_alloc_int.f90
    logical                                                 :: sferic   !  Description and declaration in tricom.igs
    real(fp)                                  , intent(in)  :: dt       !  Description and declaration in esm_alloc_real.f90
    real(fp)                                  , intent(in)  :: grdang   !  Description and declaration in tricom.igs
    real(fp)                                  , intent(in)  :: timnow   !!  Current timestep (multiples of dt)
    real(fp)                                  , intent(in)  :: timscl   !!  Multiple factor to create minutes
                                                                        !!  from read times
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: alfas    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsc, nsrc)                        :: rint0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsc, nsrc)                        :: rint1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                               :: disch0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                               :: disch1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                               :: umdis0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                               :: umdis1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                               :: vmdis0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                               :: vmdis1   !  Description and declaration in esm_alloc_real.f90
    character(1), dimension(nsrc)             , intent(in)  :: dismmt   !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer          :: iocond      ! Flag for reading errors = 0 No error < 0 End-Of-File reached > 0 Reading error 
    integer          :: ittime      ! Help variable containing timestep number RTDIS * SCALEF / DT 
    integer          :: l           ! Loop variable for nr. of constituents 
    integer          :: nread       ! Number of boundary values to read depending on DISMMT and LSTSC 
    logical          :: last        ! Flag to see of last record to be read is reached without having reached and of computaion 
    real(fp)         :: alpha       ! Linear interpolation factor
    real(fp)         :: anglen      ! The edge between X- and Y- ax calcu- lated from WINDD (wind direction from north) 
    real(fp)         :: rinc        ! Increment values at each timestep for the open boundary conditions 
    real(fp)         :: rtdis       ! Time read from the discharge file (multiples of DT / SCALEF) 
    real(fp)         :: rttime      ! Help variable containing time RTDIS * SCALEF 
    real(fp)         :: tlread      ! Last time read from file (in minutes) 
    real(fp)         :: uvdir       ! Velocity direction read from file given as wind from north 
    real(fp)         :: uvmagn      ! Velocity manitude read from file 
    real(fp)         :: workx       ! Work X 
    real(fp)         :: worky       ! Work Y 
    character(16)    :: fmtdis
    character(20)    :: errmsg      ! String containing error message 
!
!! executable statements -------------------------------------------------------
!
    scalef    => gdp%gdupddis%scalef
    last = .false.
    !
    ! Copy discharge values 
    !
    itdis(1,isrc) = itdis(2,isrc)
    disch0(isrc)  = disch1(isrc)
    umdis0(isrc)  = umdis1(isrc)
    vmdis0(isrc)  = vmdis1(isrc)
    do l = 1, lstsc
        rint0(l, isrc) = rint1(l, isrc)
    enddo
    !
    ! End of simulation
    !
    if (nint(timnow)==itfinish) return
    !
    ! Define reading format
    !
    fmtdis = '(f16.4,...g14.6)'
    if (dismmt(isrc)=='Y') then
       nread = 3 + lstsc
    else
       nread = 1 + lstsc
    endif
    write (fmtdis(8:10), '(i3.3)') nread
    !
    ! Read discharges and concentrations 
    ! as long as time read < time now
    !
    !==>
   10 continue
    itdis(5, isrc) = itdis(5, isrc) + 1 ! next record will be read
    !
    ! End of file and not yet end of computation
    !
    if (itdis(5, isrc)>itdis(4, isrc)) then
       last           = .true.
       itdis(5, isrc) = itdis(4, isrc)
       errmsg         = 'discharge = '
       write (errmsg(13:15), '(i3)') isrc
       call prterr(lundia    ,'V098'    ,errmsg(:15)          )
    endif
    !
    ! Read the time dependent data
    !
    if (dismmt(isrc)=='Y') then
       !
       ! Read discharge values including magnitude and direction
       !
       read (lundis, fmtdis, rec = itdis(5, isrc), iostat = iocond) &
          & rtdis, disch1(isrc) , (rint1(l, isrc), l = 1, lstsc), uvmagn, uvdir
       if (iocond/=0) call eoferr('dis', lundia, iocond, tlread, gdp)
       !
       ! direction is given relative to the north, positive
       ! clockwise the velocity is coming from that direction.
       ! angle to east-axis
       !   alpha = 90-uvdir
       ! velocity streams to direction
       !   beta  = 90-uvdir+180 = 270-uvdir
       ! velocity adjusted to grdang
       !   gamma = 270-uvdir+grdang
       !
       anglen       =  (270.0_fp - uvdir + grdang)*degrad
       workx        =  uvmagn*cos(anglen)
       worky        =  uvmagn*sin(anglen)
       umdis1(isrc) =  workx*cos(alfas(nm)*degrad) + worky*sin(alfas(nm)*degrad)
       vmdis1(isrc) = -workx*sin(alfas(nm)*degrad) + worky*cos(alfas(nm)*degrad)
    else
       !
       ! Read discharge values, set magnitude and direction to zero
       !
       read (lundis, fmtdis, rec = itdis(5, isrc), iostat = iocond) &
          & rtdis, disch1(isrc) , (rint1(l, isrc), l = 1, lstsc)
       if (iocond/=0) call eoferr('dis', lundia, iocond, tlread, gdp)
       umdis1(isrc) = 0.0_fp
       vmdis1(isrc) = 0.0_fp
    endif
    !
    ! Test read time RTDIS as multiple of DT (/ SCALEF)
    !
    rttime = real(rtdis*scalef,fp)
    ittime = nint(rttime/dt)
    if (abs(ittime*dt - rttime)>(0.1_fp*dt)) then
       write (errmsg, '(a,f10.4)') 'Timdis = ', rtdis
       call prterr(lundia    ,'S044'    ,errmsg    )
    endif
    !
    ! Define RTDIS as multiplication factor of DT, where RTDIS is in SCALEF minutes
    !
    rtdis = real(nint(rtdis*scalef/dt),fp)
    if (last) then
       rtdis = real(itfinish,fp)
    endif
    itdis(2, isrc) = nint(rtdis)
    !
    ! rtdis < timnow: read data of next time point
    ! rtdis = timnow: if timnow=itstart, a second set of data is needed
    !                 => read data of next time point
    !                 if timnow=tend, no error is generated; rtdis is set to tstop
    ! rtdis > timnow: finished reading data
    !
    if (comparereal(rtdis,timnow) /= 1 .and. .not. last) then
       !
       ! Copy discharge values read to "partner" array
       !
       itdis(1,isrc) = itdis(2,isrc)
       disch0(isrc)  = disch1(isrc)
       umdis0(isrc)  = umdis1(isrc)
       vmdis0(isrc)  = vmdis1(isrc)
       do l = 1, lstsc
          rint0(l, isrc) = rint1(l, isrc)
       enddo
       !
       ! Read new time step from file
       !
       goto 10
       !<==
    endif
end subroutine upddis
