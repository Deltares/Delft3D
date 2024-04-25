subroutine rdmeteo(gdp, ecwind)
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
!    Function: - Read meteo related items
!              - Initialize meteo module
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use meteo
   !use ec_module
   use precision
   use properties
   !
   use dfparall
   use globaldata
   !
   implicit none
   !
   ! Global variables
   !
   type(globdat),target :: gdp
   logical              :: ecwind  ! Temporary flag to switch between meteo and ec module
   !
   ! The following list of pointer parameters is used to point inside the gdp structure
   !
   integer                  , pointer :: nmax
   integer                  , pointer :: mmax
   integer                  , pointer :: mlb
   integer                  , pointer :: mub
   integer                  , pointer :: nmlb
   integer                  , pointer :: nmub
   integer                  , pointer :: nmaxus
   integer                  , pointer :: itdate
   integer                  , pointer :: nmaxgl
   integer                  , pointer :: mmaxgl
   real(fp)                 , pointer :: tstop
   real(fp)                 , pointer :: tzone
   real(fp)                 , pointer :: rhum
   real(fp)                 , pointer :: tair
   real(fp)                 , pointer :: mulsd
   real(fp)                 , pointer :: betasd
   real(fp)                 , pointer :: albedo
   real(fp), dimension(:)   , pointer :: rhumarr
   real(fp), dimension(:)   , pointer :: tairarr
   real(fp), dimension(:)   , pointer :: clouarr
   real(fp), dimension(:)   , pointer :: swrfarr
   real(fp), dimension(:)   , pointer :: secchi
   logical                  , pointer :: rhum_file
   logical                  , pointer :: tair_file
   logical                  , pointer :: clou_file
   logical                  , pointer :: prcp_file
   logical                  , pointer :: swrf_file
   logical                  , pointer :: scc_file
   logical                  , pointer :: solrad_read
   type (gd_heat)           , pointer :: gdheat
   integer                  , pointer :: lundia
   real(fp)                 , pointer :: paver
   logical                  , pointer :: lfsdu
   logical                  , pointer :: lfsdus1
   logical                  , pointer :: pcorr
   logical                  , pointer :: wind
   logical                  , pointer :: temp
   integer                  , pointer :: ktemp
   logical                  , pointer :: sferic
   integer                  , pointer :: patmECItemId
   integer                  , pointer :: uwindECItemId
   integer                  , pointer :: vwindECItemId
   integer                  , pointer :: gridECItemId
!
! Local variables
!
   integer                              :: istat
   integer                              :: qId
   integer                              :: provid
   logical                              :: ex
   logical                              :: error       !!  Flag=TRUE if an error is encountered
   logical                              :: success
   logical                              :: interpolate
   logical                              :: p_file
   logical                              :: u_file
   logical                              :: v_file
   real(fp)                             :: rdef        ! Help var. containing default value(s) for real variable 
   real(fp)                             :: extinc      ! Coefficient determining the penetration of the solar ray in the water column; also known as extinction coefficient 
   real(fp)  , dimension(:,:) , pointer :: ectmp1
   real(fp)  , dimension(:,:) , pointer :: ectmp2
   real(fp)  , dimension(:,:) , pointer :: ectmp3
   character(10)                        :: value
   character(11)                        :: fmttmp      ! Help var. for the attribute file formats
   character(256)                       :: filename
   character(500)                       :: message
   type(tmeteoitem)           , pointer :: meteoitem
   !type(tECHandle)            , pointer :: ECHandle
!
!! executable statements -------------------------------------------------------
!
   nmaxgl        => gdp%gdparall%nmaxgl
   mmaxgl        => gdp%gdparall%mmaxgl
   nmax          => gdp%d%nmax
   mmax          => gdp%d%mmax
   mlb           => gdp%d%mlb
   mub           => gdp%d%mub
   nmlb          => gdp%d%nmlb
   nmub          => gdp%d%nmub
   nmaxus        => gdp%d%nmaxus
   itdate        => gdp%gdexttim%itdate
   tstop         => gdp%gdexttim%tstop
   tzone         => gdp%gdexttim%tzone
   rhum          => gdp%gdheat%rhum
   tair          => gdp%gdheat%tair
   mulsd         => gdp%gdheat%mulsd
   betasd        => gdp%gdheat%betasd
   albedo        => gdp%gdheat%albedo
   rhumarr       => gdp%gdheat%rhumarr
   tairarr       => gdp%gdheat%tairarr
   clouarr       => gdp%gdheat%clouarr
   swrfarr       => gdp%gdheat%swrfarr
   rhum_file     => gdp%gdheat%rhum_file
   tair_file     => gdp%gdheat%tair_file
   clou_file     => gdp%gdheat%clou_file
   prcp_file     => gdp%gdheat%prcp_file
   swrf_file     => gdp%gdheat%swrf_file
   scc_file      => gdp%gdheat%scc_file
   solrad_read   => gdp%gdheat%solrad_read
   gdheat        => gdp%gdheat
   lfsdu         => gdp%gdprocs%lfsdu 
   lfsdus1       => gdp%gdprocs%lfsdus1
   lundia        => gdp%gdinout%lundia
   paver         => gdp%gdnumeco%paver
   pcorr         => gdp%gdnumeco%pcorr
   wind          => gdp%gdprocs%wind
   temp          => gdp%gdprocs%temp
   ktemp         => gdp%gdtricom%ktemp
   sferic        => gdp%gdtricom%sferic
   !ECHandle      => gdp%gd_ECHandle
   !gridECItemId  => gdp%gridECItemId
   !patmECItemId  => gdp%patmECItemId
   !uwindECItemId => gdp%uwindECItemId
   !vwindECItemId => gdp%vwindECItemId
   !
   error     = .false.
   success   = initmeteo(gdp%runid)
   call checkmeteoresult(success, gdp)
   !
   ! All different wind options, only if wind == true
   !
   if (wind) then 
      p_file    = .false.
      u_file    = .false.
      v_file    = .false.
      !
      if (ecwind) then
         !qId      = addQuantity(ECHandle, 'air_pressure')
         !patmECItemId = addECItem(ECHandle, qId, gridECItemId)
         !allocate(ectmp1(nmax,mmax))
         !success   = addECItemField(ECHandle, patmECItemId, ectmp1)
         !!
         !qId       = addQuantity(ECHandle, 'x_wind')
         !uwindECItemId = addECItem(ECHandle, qId, gridECItemId)
         !allocate(ectmp2(nmax,mmax))
         !success   = addECItemField(ECHandle, uwindECItemId, ectmp2)
         !!
         !qId       = addQuantity(ECHandle, 'y_wind')
         !vwindECItemId = addECItem(ECHandle, qId, gridECItemId)
         !allocate(ectmp3(nmax,mmax))
         !success   = addECItemField(ECHandle, vwindECItemId, ectmp3)
      endif
      !
      filename = ' '
      call prop_get_string(gdp%mdfile_ptr,'*','Filwnd',filename)
      if (filename /= ' ') then
         value = ' '
         call prop_get_string(gdp%mdfile_ptr,'*','Wnsvwp',value)
         if (index(value,'y')>=1 .or. index(value,'Y')>=1) then
            value = ' '
            call prop_get_string(gdp%mdfile_ptr,'*','Wndgrd',value)
            if ( index(value,'a')==0 .and. index(value,'A')==0 ) then
               !
               ! flow dimensions needed here
               !
               !if (patmECItemId == -1) then
                  success = addmeteoitem(gdp%runid, filename, sferic, mmaxgl, nmaxgl)
                  call checkmeteoresult(success, gdp)
                  call prterr(lundia, 'G051', 'Wind and pressure specified on the hydrodynamic grid')
                  p_file = .true.
                  u_file = .true.
                  v_file = .true.
               !else
                  ! the same but using ec-module
                  !
               !   provId   = addProvider(ECHandle, provType_file, filename, provFile_svwp)
               !   if (provId == 0) call checkResult(ECHandle)
               !   success  = initProvider(ECHandle, provId)
               !   call checkResult(ECHandle, success)
               !endif
            else
               !
               ! Wind on a separate equidistant grid specified in Filwu, Filwv, Filwp
               ! or wind on a separate curvilinear grid specified in Fwndgp, Fwndgu, Fwndgv
               !
               ! do not use the file specified in Filwnd
               !
               write(message,'(3a)') 'File ',trim(filename),' is not used'
               call prterr(lundia, 'Z013', trim(message))
            endif
         elseif (index(value,'n')>=1 .or. index(value,'N')>=1) then
            success = addmeteoitem(gdp%runid, filename)
            call checkmeteoresult(success, gdp)
            call prterr(lundia, 'G051', 'Uniform wind and pressure specified')
            p_file = .true.
            u_file = .true.
            v_file = .true.
         endif
      endif
      !
      filename = ' '
      call prop_get_string(gdp%mdfile_ptr,'*','Filspv',filename)
      if (filename /= ' ') then
         !
         ! flow dimensions needed here
         !
         success = addmeteoitem(gdp%runid, filename, sferic, mmaxgl, nmaxgl)
         call checkmeteoresult(success, gdp)
         if (p_file .and. u_file .and. v_file) then
            call prterr(lundia, 'G051', 'Multiple input for wind and pressure, using input specified on the hydrodynamic grid')
         else
            call prterr(lundia, 'G051', 'Wind and pressure specified on the hydrodynamic grid')
            p_file = .true.
            u_file = .true.
            v_file = .true.
         endif
      endif
      !
      filename = ' '
      call prop_get_string(gdp%mdfile_ptr,'*','Filwu',filename)
      if (filename /= ' ') then
         success = addmeteoitem(gdp%runid, filename, sferic)
         call checkmeteoresult(success, gdp)
         if (u_file) then
            call prterr(lundia, 'G051', 'Multiple input for wind (U component), using input specified on a separate equidistant grid')
         else
            call prterr(lundia, 'G051', 'Wind (U component) specified on a separate equidistant grid')
            u_file = .true.
         endif
      endif
      !
      filename = ' '
      call prop_get_string(gdp%mdfile_ptr,'*','Filwv',filename)
      if (filename /= ' ') then
         success = addmeteoitem(gdp%runid, filename, sferic)
         call checkmeteoresult(success, gdp)
         if (v_file) then
            call prterr(lundia, 'G051', 'Multiple input for wind (V component), using input specified on a separate equidistant grid')
         else
            call prterr(lundia, 'G051', 'Wind (V component) specified on a separate equidistant grid')
            v_file = .true.
         endif
      endif
      !
      filename = ' '
      call prop_get_string(gdp%mdfile_ptr,'*','Filwp',filename)
      if (filename /= ' ') then
         success = addmeteoitem(gdp%runid, filename, sferic)
         call checkmeteoresult(success, gdp)
         if (p_file) then
            call prterr(lundia, 'G051', 'Multiple input for air pressure, using input specified on a separate equidistant grid')
         else
            call prterr(lundia, 'G051', 'Air pressure specified on a separate equidistant grid')
            p_file = .true.
         endif
      endif
      !
      filename = ' '
      call prop_get_string(gdp%mdfile_ptr,'*','Fwndgu',filename)
      if (filename /= ' ') then
         success = addmeteoitem(gdp%runid, filename, sferic, mmaxgl, nmaxgl)
         call checkmeteoresult(success, gdp)
         if (u_file) then
            call prterr(lundia, 'G051', 'Multiple input for wind (U component), using input specified on a separate curvilinear grid')
         else
            call prterr(lundia, 'G051', 'Wind (U component) specified on a separate curvilinear grid')
            u_file = .true.
         endif
      endif
      !
      filename = ' '
      call prop_get_string(gdp%mdfile_ptr,'*','Fwndgv',filename)
      if (filename /= ' ') then
         success = addmeteoitem(gdp%runid, filename, sferic, mmaxgl, nmaxgl)
         call checkmeteoresult(success, gdp)
         if (v_file) then
            call prterr(lundia, 'G051', 'Multiple input for wind (V component), using input specified on a separate curvilinear grid')
         else
            call prterr(lundia, 'G051', 'Wind (V component) specified on a separate curvilinear grid')
            v_file = .true.
         endif
      endif
      !
      filename = ' '
      call prop_get_string(gdp%mdfile_ptr,'*','Fwndgp',filename)
      if (filename /= ' ') then
         success = addmeteoitem(gdp%runid, filename, sferic, mmaxgl, nmaxgl)
         call checkmeteoresult(success, gdp)
         if (p_file) then
            call prterr(lundia, 'G051', 'Multiple input for air pressure, using input specified on a separate curvilinear grid')
         else
            call prterr(lundia, 'G051', 'Air pressure specified on a separate curvilinear grid')
            p_file = .true.
         endif
         message = getmeteomessage()
      endif
      !
      filename = ' '
      call prop_get_string(gdp%mdfile_ptr,'*','Filweb',filename)
      if (filename /= ' ') then
         success = addmeteoitem(gdp%runid, filename, sferic)
         call checkmeteoresult(success, gdp)
         if (p_file .and. u_file .and. v_file) then
            call prterr(lundia, 'G051', 'Wind and pressure specified on a Spiderweb grid added to meteo input')
         else
            call prterr(lundia, 'G051', 'Wind and pressure specified on a Spiderweb grid')
            p_file = .true.
            u_file = .true.
            v_file = .true.
         endif
      endif
      !
      ! If p, u or v specified on file, then all three must be specified on file
      !
      if (p_file .or. u_file .or. v_file) then
         if (.not. p_file) then
            write(message,'(a)') 'Wind specification is incomplete; air pressure is missing'
            call prterr(lundia, 'U021', trim(message))
            call d3stop(1, gdp)
         endif
         if (.not. u_file) then
            write(message,'(a)') 'Wind specification is incomplete; U component is missing'
            call prterr(lundia, 'U021', trim(message))
            call d3stop(1, gdp)
         endif
         if (.not. v_file) then
            write(message,'(a)') 'Wind specification is incomplete; V component is missing'
            call prterr(lundia, 'U021', trim(message))
            call d3stop(1, gdp)
         endif
      endif   
   endif
   !
   ! Cloudiness, relative air humidity and temperature
   !
   clou_file = .false.
   rhum_file = .false.
   tair_file = .false. 
   scc_file  = .false. 
   !
   filename = ' '
   call prop_get_string(gdp%mdfile_ptr,'*','Fwndgr',filename)
   if (filename /= ' ') then
      success = addmeteoitem(gdp%runid, filename, sferic, mmaxgl, nmaxgl)
      call prterr(lundia, 'G051', 'Relative air humidity specified on a separate curvilinear grid')
   else
      call prop_get_string(gdp%mdfile_ptr,'*','Filwr',filename)
      if (filename /= ' ') then
         success = addmeteoitem(gdp%runid, filename, sferic)
         call prterr(lundia, 'G051', 'Relative air humidity specified on a separate equidistant grid')
      endif
   endif
   if (filename /= ' ') then
      rhum_file = .true.
      call checkmeteoresult(success, gdp)
      allocate (gdp%gdheat%rhumarr(gdp%d%nmlb:gdp%d%nmub), stat = istat)
      if (istat/=0) then
         call prterr(lundia, 'U021', 'Rdmeteo: memory alloc error')
         call d3stop(1, gdp)
      endif
   else
      rhum_file = .false.
   endif
   !
   ! Air Temperature
   !
   filename = ' '
   call prop_get_string(gdp%mdfile_ptr,'*','Fwndgt',filename)
   if (filename /= ' ') then
      success = addmeteoitem(gdp%runid, filename, sferic, mmaxgl, nmaxgl)
      call prterr(lundia, 'G051', 'Air temperature specified on a separate curvilinear grid')
   else
      call prop_get_string(gdp%mdfile_ptr,'*','Filwt',filename)
      if (filename /= ' ') then
         success = addmeteoitem(gdp%runid, filename, sferic)
         call prterr(lundia, 'G051', 'Air temperature specified on a separate equidistant grid')
      endif
   endif
   if (filename /= ' ') then
      tair_file = .true.
      call checkmeteoresult(success, gdp)
      allocate (gdp%gdheat%tairarr(gdp%d%nmlb:gdp%d%nmub), stat = istat)
      if (istat/=0) then
         call prterr(lundia, 'U021', 'Rdmeteo: memory alloc error')
         call d3stop(1, gdp)
      endif
   else
      tair_file = .false.
   endif
   !
   ! Cloudiness
   !
   filename = ' '
   call prop_get_string(gdp%mdfile_ptr,'*','Fwndgc',filename)
   if (filename /= ' ') then
      success = addmeteoitem(gdp%runid, filename, sferic, mmaxgl, nmaxgl)
      call prterr(lundia, 'G051', 'Air cloudiness specified on a separate curvilinear grid')
   else
      call prop_get_string(gdp%mdfile_ptr,'*','Filwc',filename)
      if (filename /= ' ') then
         success = addmeteoitem(gdp%runid, filename, sferic)
         call prterr(lundia, 'G051', 'Air cloudiness specified on a separate equidistant grid')
      endif
   endif
   if (filename /= ' ') then
      clou_file = .true.
      call checkmeteoresult(success, gdp)
      allocate (gdp%gdheat%clouarr(gdp%d%nmlb:gdp%d%nmub), stat = istat)
      if (istat/=0) then
         call prterr(lundia, 'U021', 'Rdmeteo: memory alloc error')
         call d3stop(1, gdp)
      endif
   else
      clou_file = .false.
   endif
   !
   ! Precipitation
   !
   filename = ' '
   call prop_get_string(gdp%mdfile_ptr,'*','Fwndgpr',filename)
   if (filename /= ' ') then
      success = addmeteoitem(gdp%runid, filename, sferic, mmaxgl, nmaxgl)
      call prterr(lundia, 'G051', 'Precipitation specified on a separate curvilinear grid')
   else
      call prop_get_string(gdp%mdfile_ptr,'*','Filwpr',filename)
      if (filename /= ' ') then
         success = addmeteoitem(gdp%runid, filename, sferic)
         call prterr(lundia, 'G051', 'Precipitation specified on a separate equidistant grid')
      endif
   endif
   if (filename /= ' ') then
      prcp_file = .true.
      call checkmeteoresult(success, gdp)
   else
      prcp_file = .false.
   endif
   !
   ! Short-wave radiation flux
   !
   filename = ' '
   call prop_get_string(gdp%mdfile_ptr,'*','Fwndgs',filename)
   if (filename /= ' ') then
      success = addmeteoitem(gdp%runid, filename, sferic, mmaxgl, nmaxgl)
      call prterr(lundia, 'G051', 'Short-wave solar radiation specified on a separate curvilinear grid')
   else
      call prop_get_string(gdp%mdfile_ptr,'*','Filws',filename)
      if (filename /= ' ') then
         success = addmeteoitem(gdp%runid, filename, sferic)
         call prterr(lundia, 'G051', 'Short-wave solar radiation specified on a separate equidistant grid')
      endif
   endif
   if (filename /= ' ') then
      swrf_file = .true.
      call checkmeteoresult(success, gdp)
      allocate (gdp%gdheat%swrfarr(gdp%d%nmlb:gdp%d%nmub), stat = istat)
      if (istat/=0) then
         call prterr(lundia, 'U021', 'Rdmeteo: memory alloc error')
         call d3stop(1, gdp)
      endif
   else
      swrf_file = .false.
   endif
   !
   !   Time and space varying Secchi depth
   !
   filename = ' '
   call prop_get_string(gdp%mdfile_ptr,'*','Filscc',filename)
   if (filename /= ' ') then
      scc_file  = .true.
      if (.not.associated(gdp%gdheat%secchi)) then 
          allocate (gdp%gdheat%secchi(gdp%d%nmlb:gdp%d%nmub), stat = istat)
          if (istat /= 0) then
             call prterr(lundia, 'P004', 'memory alloc error for Secchi depth')
             call d3stop(1, gdp)
          endif    
      endif          
      call prterr(lundia, 'G051', 'Secchi depth specified on a separate equidistant grid')
      success = addmeteoitem(gdp%runid, filename, sferic, mmaxgl, nmaxgl)
      call checkmeteoresult(success, gdp)
   else
      scc_file = .false.
   endif
   !
   !  Subsidence/Uplift 
   !
   filename = ' '
   call prop_get_string(gdp%mdfile_ptr,'*','Filsdu',filename)
   if (filename /= ' ') then
      if (.not.associated(gdp%gdsdu%sdu_t0)) then 
          allocate (gdp%gdsdu%sdu_t0(gdp%d%nmlb:gdp%d%nmub), stat = istat)
          if (istat == 0) allocate(gdp%gdsdu%sdu_tp(gdp%d%nmlb:gdp%d%nmub), stat=istat)
          if (istat == 0) allocate(gdp%gdsdu%sdu_tn(gdp%d%nmlb:gdp%d%nmub), stat=istat)
          if (istat /= 0) then
             call prterr(lundia, 'P004', 'memory alloc error for subsidence/uplift')
             call d3stop(1, gdp)
          endif    
      endif          
      call prterr(lundia, 'G051', 'Reading subsidence/uplift input on computational grid')
      success = addmeteoitem(gdp%runid, filename, sferic, mmaxgl, nmaxgl)
      call checkmeteoresult(success, gdp)
      if (lfsdus1) then 
         call prterr(lundia, 'G051', '   Subsidence/uplift effect on water level included.')
      endif    
   endif
   !
   ! If block 'interpolation', notify meteo module
   !
   interpolate = .true.
   call prop_get_logical(gdp%mdfile_ptr,'*','Wndint',interpolate)
   if (.not. interpolate) then
      call meteoblockint()
      call prterr(lundia, 'G051', 'No interpolation on meteo data')
   endif
   !
   ! Check whether meteo data is available for the complete time interval
   !
   success = checkmeteo(gdp%runid, itdate, tzone, tstop)
   call checkmeteoresult(success,gdp)
   !
   ! Relative humidity is only used for ktemp is 4 or 5
   !
   if (rhum_file) then
      if (ktemp/=4 .and. ktemp/=5) then
         write(message,'(a,i2)') 'Relative humidity is not used in heat model (ktemp) = ',ktemp
         call prterr(lundia, 'U021', trim(message))
         call d3stop(1, gdp)
      endif
   endif
   !
   ! Air temperature is only used for ktemp is 4 or 5
   !
   if (tair_file) then
      if (ktemp/=4 .and. ktemp/=5) then
         write(message,'(a,i2)') 'Air temperature is not used in heat model (ktemp) = ',ktemp
         call prterr(lundia, 'U021', trim(message))
         call d3stop(1, gdp)
      endif
   endif
   !
   ! Cloudiness is only used for ktemp is 5
   !
   if (clou_file) then
      if (ktemp /= 5) then
         write(message,'(a,i2)') 'Cloudiness is not used in heat model (ktemp) = ',ktemp
         call prterr(lundia, 'U021', trim(message))
         call d3stop(1, gdp)
      endif
   endif
   !
   ! Short-wave radiation flux is only used for ktemp is 5
   !
   if (swrf_file) then
      if (ktemp /= 5) then
         write(message,'(a,i2)') 'Short-wave radiation is not used in heat model (ktemp) = ',ktemp
         call prterr(lundia, 'U021', trim(message))
         call d3stop(1, gdp)
      endif
   endif
   !
   ! Time and space varying Secchi depth is only used for ktemp is 4 or 5
   !
   if (scc_file) then
      if (ktemp /= 5) then
         write(message,'(a,i2)') 'Time and space varying Secchi depth is not used in heat model (ktemp) = ',ktemp
         call prterr(lundia, 'U021', trim(message))
         call d3stop(1, gdp)
      endif
   endif
   !
   ! If rhum, clou or tair specified on file, then all two (ktemp=4)/three(ktemp=5)
   ! must be specified on file
   !
   if (rhum_file .or. clou_file .or. tair_file) then
      if (solrad_read) then
         write(message,'(2a)') 'Meteo input on a separate grid ', &
                             & 'is not allowed in combination with input of solar radiation in .tem file.'
         call prterr(lundia, 'U021', trim(message))
         call d3stop(1, gdp)
      endif
      if (.not. rhum_file) then
         write(message,'(a)') 'Air specification is incomplete; relative humidity is missing'
         call prterr(lundia, 'U021', trim(message))
         call d3stop(1, gdp)
      endif
      if (.not. tair_file) then
         write(message,'(a)') 'Air specification is incomplete; temperature is missing'
         call prterr(lundia, 'U021', trim(message))
         call d3stop(1, gdp)
      endif
      if (ktemp==5 .and. .not. clou_file) then
         write(message,'(a)') 'Air specification is incomplete; cloudiness is missing'
         call prterr(lundia, 'U021', trim(message))
         call d3stop(1, gdp)
      endif
   endif
   !
   ! For "Murakami" and "Ocean" model
   !
   if (ktemp==4 .or. ktemp==5) then
      if (.not. scc_file) then
         allocate (gdp%gdheat%secchi(gdp%d%nmlb:gdp%d%nmub), stat = istat)
         if (istat/=0) then
            call prterr(lundia, 'U021', 'Rdproc: memory alloc error')
            call d3stop(1, gdp)
         endif
         !
         secchi      => gdp%gdheat%secchi
         !
         secchi      = -1.0
         extinc      = -1.0
         !
         ! NEW INPUT:
         ! Locate and read 'Secchi' Secchidepth; default value allowed
         !
         filename = ' '
         call prop_get_string(gdp%mdfile_ptr, '*', 'Secchi', filename)
         if (filename == ' ') filename = 'dummyname'
         inquire (file = trim(filename), exist = ex)
         if (ex) then
            fmttmp = 'formatted'
         call depfil(lundia    ,error     ,filename  ,fmttmp    , &
                   & secchi    ,1         ,1         ,gdp%griddim)
         if (error) call d3stop(1, gdp)
         else
            filename = ' '
            rdef = secchi(1)
            call prop_get(gdp%mdfile_ptr, '*', 'Secchi', rdef)
            secchi = rdef
         endif
         !
         ! ORIGINAL INPUT:
         ! Locate and read 'Extinc' extiction coefficient for the solar
         ! insolation; default value allowed
         !
         call prop_get(gdp%mdfile_ptr, '*', 'Extinc', extinc)
         !
         if (secchi(1) == - 1.0_fp) then
            if (extinc == - 1.0_fp) then
               !
               ! both secchi and extinc not defined by user;
               ! secchi = default
               !
               ! old parametrisation: secchi = 1.7/0.127
               secchi = 2.0_fp
               write(message, '(a,f12.7,a)') 'Heat model: Default value used for Secchi depth: ', secchi(1), ' m'
               call prterr(lundia, 'G051', trim(message))
            else
               !
               ! secchi not defined by user;
               ! secchi = f(extinc)
               !
               secchi = 1.7_fp/extinc
               write(message, '(a,f12.7)') 'Heat model: Extinc specified to be ', extinc
               call prterr(lundia, 'G051', trim(message))
            endif
         !
         ! secchi is defined by user;
         ! warning when extinc is defined too
         !
         elseif (extinc /= - 1.0_fp) then
            if (filename == ' ') then
               write(message, '(a,f12.7,a)') 'Heat model: Secchi depth used instead of Extinc, with value ', secchi(1), ' m'
            else
               write(message, '(a,a)') 'Heat model: Extinc value skipped; Secchi depth read from ', trim(filename)
            endif
            call prterr(lundia, 'G051', trim(message))
            extinc = -1.0_fp
         else
            if (filename == ' ') then
               write(message, '(a,f12.7,a)') 'Heat model: Secchi depth specified to be ', secchi(1), ' m'
            else
               write(message, '(a,a)') 'Heat model: Secchi depth read from ', trim(filename)
            endif
            call prterr(lundia, 'G051', trim(message))
         endif
      endif
      !
      ! Locate and read optional real 'mulsd': The multiplier of the Secchi depth to obtain the shallow-Secchi depth (used in heatu)
      !
      rdef = -999.0_fp
      call prop_get(gdp%mdfile_ptr, '*', 'mulsd', rdef)
      if (comparereal(rdef, -999.0_fp) /= 0) then
         mulsd = rdef
         if (comparereal(mulsd, 0.01_fp) == -1 .or. comparereal(mulsd, 1.0_fp) == 1) then
            write(message,'(a,f12.3)') 'Secchi depth multiplication factor MulSD (for shallow Secchi depth) has a value outside the region [0.01,1.0]: ', mulsd
            call prterr(lundia, 'P004', trim(message))
            call d3stop(1, gdp)
         else
            write(message,'(a,f12.3)') 'Secchi multiplication factor MulSD for shallow Secchi depth: ', mulsd
            call prterr(lundia, 'G051', trim(message))
         endif
      else
          !
          ! Use default value mulsd = 0.0 (only the normal Secchi depth is used)
          !
          mulsd = 0.0_fp
      endif      
      !
      ! Locate and read optional real 'betasd': The fraction of solar radiation that penetrates deep (used in heatu)
      !
      rdef = -999.0_fp
      call prop_get(gdp%mdfile_ptr, '*', 'betasd', rdef)
      if (comparereal(rdef, -999.0_fp) /= 0) then
         betasd = rdef
         if (comparereal(betasd, 0.0_fp) == -1 .or. comparereal(betasd, 1.0_fp) == 1) then
            write(message,'(a,f12.3)') 'The fraction of solar radiation that penetrates deep BetaSD has a value outside the region [0.0,1.0]: ', betasd
            call prterr(lundia, 'P004', trim(message))
            call d3stop(1, gdp)
         else
            write(message,'(a,f12.3)') 'The fraction of solar radiation that penetrates deep BetaSD: ', betasd
            call prterr(lundia, 'G051', trim(message))
         endif
      else
          !
          ! Use default value betasd = 1.0 (Solar radiation penetrates completely over the Secchi depth)
          !
          betasd = 1.0_fp
      endif
      !
      ! Locate and read optional real 'albedo': The Albedo coefficient for reflection of solar radiation (used in heatu)
      !
      rdef = -999.0_fp
      call prop_get(gdp%mdfile_ptr, '*', 'albedo', rdef)
      if (comparereal(rdef, -999.0_fp) /= 0) then
         albedo = rdef
         if (comparereal(albedo, 0.0_fp) == -1 .or. comparereal(albedo, 1.0_fp) == 1) then
            write(message,'(a,f12.3)') 'The Albedo coefficient has a value outside the region [0.0,1.0]: ', albedo
            call prterr(lundia, 'P004', trim(message))
            call d3stop(1, gdp)
         else
            write(message,'(a,f12.3)') 'The Albedo coefficient for reflection of solar radiation: ', albedo
            call prterr(lundia, 'G051', trim(message))
         endif
      else
          !
          ! Use default value for Albedo = 0.09, or Albedo = 0.06 for temperature model 5 (Murakami)
          !
          if (ktemp /= 5) then
             albedo = 0.09_fp
          else
             albedo = 0.06_fp
          endif
      endif
   endif
end subroutine rdmeteo
