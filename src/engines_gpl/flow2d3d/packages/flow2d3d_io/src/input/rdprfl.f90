subroutine rdprfl(lunmd     ,lundia    ,nrrec     ,mdfrec    ,tstprt    , &
                & kmax      ,lstsci    ,ltur      ,lsal      ,ltem      , &
                & nostat    ,ntruv     ,prsmap    ,prshis    ,selmap    , &
                & selhis    ,lsed      ,gdp       )
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
!    Function: - Reads the output selection options from the MD-
!                file :
!              - Writes the flags to a character string SELHIS
!                (char*23) and SELMAP (char*21).
!                Default SELHIS = 'YYYYYYYYYYYYYYYYYYYYYYY'
!                Default SELMAP = 'YYYYYYYYYYYYYYYYYYYYN'
!              - Reads the output print options from the MD-
!                file :
!              - Writes the flags to a character string PRSHIS
!                (char*23) and PRSMAP (char*19).
!                Default = 'YYYYYYYYYYYYYYYYYYYYYYY'
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use globaldata
    use netcdf
    use dfparall, only: parll
    use string_module, only: str_lower
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer,               pointer :: itis
    integer, dimension(:), pointer :: smlay
    integer, dimension(:), pointer :: shlay
    logical,               pointer :: htur2d
    integer,               pointer :: io_fp
    integer,               pointer :: io_prec
    integer,               pointer :: nc_deflate
    integer,               pointer :: nc_mode
    logical,               pointer :: mergemap
!
! Global variables
!
    integer      , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer      , intent(in)  :: lsal   !  Description and declaration in dimens.igs
    integer      , intent(in)  :: lsed   !  Description and declaration in dimens.igs
    integer      , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer      , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer      , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                    :: lundia !  Description and declaration in inout.igs
    integer                    :: lunmd  !  Description and declaration in inout.igs
    integer      , intent(in)  :: nostat !  Description and declaration in dimens.igs
    integer                    :: nrrec  !!  Pointer to the record number in the MD-file
    integer      , intent(in)  :: ntruv  !  Description and declaration in dimens.igs
    logical      , intent(out) :: tstprt !  Description and declaration in tricom.igs
    character(*)               :: mdfrec !!  Standard rec. length in MD-file (300)
    character(19)              :: prsmap !  Description and declaration in tricom.igs
    character(21)              :: selmap !  Description and declaration in tricom.igs
    character(23)              :: prshis !  Description and declaration in tricom.igs
    character(23)              :: selhis !  Description and declaration in tricom.igs
!
! Local variables
!
    logical                            :: any_netcdf
    integer                            :: i
    integer                            :: icount
    integer                            :: istat
    integer                            :: j
    integer                            :: kmaxout
    integer                            :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                            :: lkw    ! Length (in characters) of keyword 
    integer                            :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                            :: ntrec  ! Help. var to keep track of NRREC 
    integer, dimension(:), allocatable :: ival
    logical                            :: found  ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                            :: lerror ! Flag=TRUE if an error is encountered 
    logical                            :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    character(10)                      :: cdef   ! Default value when CVAR not found 
    character(10)                      :: chulp  ! Help var. 
    character(6)                       :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM)
    character(256)                     :: inputstring
    character(256)                     :: message
!
!! executable statements -------------------------------------------------------
!
    htur2d     => gdp%gdprocs%htur2d
    itis       => gdp%gdrdpara%itis
    io_fp      => gdp%gdpostpr%io_fp
    io_prec    => gdp%gdpostpr%io_prec
    nc_deflate => gdp%gdpostpr%nc_deflate
    nc_mode    => gdp%gdpostpr%nc_mode
    mergemap   => gdp%gdpostpr%mergemap
    newkw = .true.
    cdef  = 'YYYYYYYYYY'
    any_netcdf = .false.
    !
    ! initialize parameters that are to be read
    !
    prshis = 'YYYYYYYYYYYYYYYYYYYYYYY'
    prsmap = 'YYYYYYYYYYYYYYYYYYY'
    selhis = 'YYYYYYYYYYYYYYYYYYYYYYY'
    selmap = 'YYYYYYYYYYYYYYYYYYYYN'
    tstprt = .false.
    !
    ! locate 'FLNcdf' record for print flag of output in NETCDF format
    !
    inputstring = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'FLNcdf', inputstring)
    call str_lower(inputstring)
    if (index(inputstring,'map') > 0) then
       gdp%iofiles(FILOUT_MAP)%filetype = FTYPE_NETCDF
       any_netcdf = .true.
       write (lundia, '(a)') '*** MESSAGE map-file format is NetCDF'
    endif
    if (index(inputstring,'fou') > 0) then
       gdp%iofiles(FILOUT_FOU)%filetype = FTYPE_NETCDF
       any_netcdf = .true.
       write (lundia, '(a)') '*** MESSAGE fourier-file format is NetCDF'
    endif
    if (index(inputstring,'his') > 0) then
       gdp%iofiles(FILOUT_HIS)%filetype = FTYPE_NETCDF
       any_netcdf = .true.
       write (lundia, '(a)') '*** MESSAGE history-file format is NetCDF'
    endif
    if (index(inputstring,'dro') > 0) then
       gdp%iofiles(FILOUT_DRO)%filetype = FTYPE_NETCDF
       any_netcdf = .true.
       write (lundia, '(a)') '*** MESSAGE drogue-file format is NetCDF'
    endif
    if (index(inputstring,'com') > 0) then
       call prterr(lundia, 'U021', "Com-file in NetCDF format is currently not supported.")
       call d3stop(1, gdp)
    endif
    if (index(inputstring,'all') > 0) then
       call prterr(lundia, 'U021', "All files in NetCDF format is currently not supported.")
       call d3stop(1, gdp)
    endif
    !
    ! set cmode flag for netCDF nf90_create calls
    !
    i = 3 ! by default netCDF3 format
    if (any_netcdf) call prop_get(gdp%mdfile_ptr, '*', 'ncFormat', i)
    if (i==3) then
        nc_mode = NF90_64BIT_OFFSET
        if (any_netcdf) write (lundia, '(a)') '*** MESSAGE Creating output files in NetCDF3 format'
    elseif (i==4) then
        nc_mode = NF90_NETCDF4
        if (any_netcdf) write (lundia, '(a)') '*** MESSAGE Creating output files in NetCDF4 format'
    else
        call prterr(lundia,'U021', "Unknown netCDF format version specified. ncFormat should equal 3 or 4.")
        call d3stop(1, gdp)
    endif
    !
    nc_deflate = 0
    if (any_netcdf .and. i==4) then
        call prop_get(gdp%mdfile_ptr, '*', 'ncDeflate', nc_deflate)
        write (lundia, '(a,i0)') '*** MESSAGE Using deflation level ',nc_deflate
    endif
    !
    ! set the numerical precision of the output to the map and his files.
    !
    i = 4
    call prop_get(gdp%mdfile_ptr, '*', 'PrecOut', i)
    if (fp==sp) then
        io_fp = IO_REAL4
    else
        io_fp = IO_REAL8
    endif
    select case (i)
    case (4)
        io_prec= IO_REAL4
        if (fp==sp) then
            write (lundia, '(a)') '*** MESSAGE His, map, drogue, and fourier files written in single precision.'
        else
            write (lundia, '(a)') '*** MESSAGE His, map, drogue, and fourier files written in single precision (except for time and horizontal coordinates).'
        endif
    case (8)
        if (fp==sp) then
            io_prec= IO_REAL4
            write (lundia, '(a)') '*** WARNING Double precision output requested for single precision simulation. His, map, drogue, and fourier files still written in single precision.'
        else
            io_prec= IO_REAL8
            write (lundia, '(a)') '*** MESSAGE His, map, drogue, and fourier files written in double precision.'
        endif
    case default
       call prterr(lundia, 'U021', "Invalid output precision specified. PrecOut should be set to 4 or 8.")
       call d3stop(1, gdp)
    end select
    !
    ! merge map files into one trim-file in case of parallel simulation (default: true)
    ! mergemap should be true for serial simulations
    !
    mergemap = .true.
    call prop_get(gdp%mdfile_ptr, '*', 'MergeMap', mergemap)
    if (.not.parll) mergemap = .true.
    !
    ! locate 'PHhydr' record for print flag History hydrodynamic
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'PHhydr', chulp)
    prshis(1:6) = chulp(1:6)
    !
    ! locate 'PHproc' record for print flag History processes
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'PHproc', chulp)
    prshis(7:16) = chulp(1:10)
    !
    ! locate 'PHderv' record for print flag History derivitives
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'PHderv', chulp)
    prshis(17:19) = chulp(1:3)
    !
    ! locate 'PHflux' record for print flag History fluxes (cross-sections)
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'PHflux', chulp)
    prshis(20:23) = chulp(1:4)
    !
    ! test for 'N' (default is 'Y')
    !
    do i = 1, 23
       if (prshis(i:i) == 'n')       prshis( i: i) = 'N'
       if (prshis(i:i) /= 'N')       prshis( i: i) = 'Y'
    enddo
    if (kmax   == 1)                 prshis( 6: 6) = 'N'
    if (lstsci == 0)                 prshis( 7:14) = 'NNNNNNNN'
    if (ltur   == 0)                 prshis(15:16) = 'NN'
    if (kmax   == 1)                 prshis(17:18) = 'NN'
    if (max(lsal,ltem) == 0)         prshis(19:19) = 'N'
    if (nostat==0)                   prshis( 1:19) = 'NNNNNNNNNNNNNNNNNNN'
    if (lstsci == 0)                 prshis(22:23) = 'NN'
    if (ntruv==0)                    prshis(20:23) = 'NNNN'
    !
    ! locate 'PMhydr' record for print flag Map hydrodynamic
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'PMhydr', chulp)
    prsmap(1:6) = chulp(1:6)
    !
    ! locate 'PMproc' record for print flag Map processes
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'PMproc', chulp)
    prsmap(7:16) = chulp(1:10)
    !
    ! locate 'PMderv' record for print flag Map derivitives
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'PMderv', chulp)
    prsmap(17:19) = chulp(1:3)
    !
    ! test for 'N' (default is 'Y')
    !
    do i = 1, 19
       if (prsmap(i:i) == 'n') prsmap( i: i) = 'N'
       if (prsmap(i:i) /= 'N') prsmap( i: i) = 'Y'
    enddo
    if (kmax   == 1)           prsmap( 6: 6) = 'N'
    if (lstsci == 0)           prsmap( 7:14) = 'NNNNNNNN'
    if (ltur   == 0)           prsmap(15:16) = 'NN'
    if (kmax   == 1)           prsmap(17:18) = 'NN'
    if (max(lsal, ltem)==0)    prsmap(19:19) = 'N'
    !
    ! locate 'SHhydr' record for print flag History hydrodynamic
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'SHhydr', chulp)
    selhis(1:4) = chulp(1:4)
    !
    ! locate 'SHproc' record for print flag History processes
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'SHproc', chulp)
    selhis(5:14) = chulp(1:10)
    !
    ! locate 'SHderv' record for print flag History derivitives
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'SHderv', chulp)
    selhis(15:19) = chulp(1:5)
    !
    ! locate 'SHflux' record for print flag History fluxes (stations
    ! and cross-sections)
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'SHflux', chulp)
    selhis(20:23) = chulp(1:4)
    !
    ! test for 'N' (default is 'Y')
    !
    do i = 1, 23
       if (selhis(i:i)=='n')           selhis(i : i) = 'N'
       if (selhis(i:i)/='N')           selhis(i : i) = 'Y'
    enddo
    !
    if (kmax   == 1)                   selhis(4 : 4) = 'N'
    if (lstsci == 0)                   selhis(5 :12) = 'NNNNNNNN'
    if (ltur   == 0)                   selhis(13:14) = 'NN'
    if (kmax   == 1)                   selhis(17:18) = 'NN'
    if (max(lsal, ltem, lsed) == 0)    selhis(19:19) = 'N'
    if (lstsci == 0)                   selhis(22:23) = 'NN'
    if (nostat==0) then
                                       selhis( 1:19) = 'NNNNNNNNNNNNNNNNNNN'
       if (ntruv==0)                   selhis(20:23) = 'NNNN'
    else
       if (ntruv==0)                   selhis(21:23) = 'NNN'
    endif
    !
    ! locate 'SMhydr' record for print flag Map hydrodynamic
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'SMhydr', chulp)
    selmap(1:5) = chulp(1:5)
    !
    ! locate 'SMproc' record for print flag Map processes
    !
    chulp = cdef
    call prop_get(gdp%mdfile_ptr, '*', 'SMproc', chulp)
    selmap(6:15) = chulp(1:10)
    !
    ! locate 'SMderv' record for print flag Map derivitives
    !
    chulp = cdef
    chulp(6:6) = 'N'
    call prop_get(gdp%mdfile_ptr, '*', 'SMderv', chulp)
    if (chulp(6:6) == ' ') chulp(6:6) = 'N'
    selmap(16:21) = chulp(1:6)
    !
    ! test for 'N' (default is 'Y')
    !
    do i = 1, 21
       if (selmap(i:i) == 'n') selmap(i:i) = 'N'
       if (selmap(i:i) /= 'N') selmap(i:i) = 'Y'
    enddo
    !
    if (kmax   == 1)                selmap( 4: 5) = 'NN'
    if (lstsci == 0)                selmap( 6:13) = 'NNNNNNNN'
    if (ltur   == 0)                selmap(14:15) = 'NN'
    if (kmax   == 1)                selmap(18:19) = 'NN'
    if (max(lsal, ltem, lsed) == 0) selmap(20:20) = 'N'
    !
    ! Always write turbulence info when HLES is active
    !
    if (htur2d) selmap(21:21) = 'Y'
    !
    ! Layer specified output
    !
    !
    ! SMlay
    !
                  allocate (gdp%gdpostpr%smlay(kmax+1), stat = istat)
    if (istat==0) allocate (gdp%gdpostpr%shlay(kmax+1), stat = istat)
    if (istat==0) allocate (ival(kmax+1), stat = istat)
    !
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'RDPRFL: memory alloc error')
       call d3stop(1, gdp)
    endif
    smlay => gdp%gdpostpr%smlay
    shlay => gdp%gdpostpr%shlay
    ival = -999
    call prop_get(gdp%mdfile_ptr, '*', 'SMlay', ival, kmax+1)
    if (ival(1) == -999) then
       !
       ! No layers specified for map-output
       ! Default: all layers to output, including zero: 0, 1, 2, ..., kmax
       !
       do i = 1, kmax+1
          smlay(i) = i-1
       enddo
    else
       !
       ! layers specified for map-output
       ! Detect the number of output layers(kmaxout):
       !   Search for the first i for which ival(i) == -999. Then kmaxout = i-1
       ! On the flow: check that the specified layers are valid
       ! 
       kmaxout = kmax + 1
       do i = 1, kmax+1
          if (ival(i) == -999) then
             kmaxout = i-1
             exit
          endif
          if (ival(i)<0 .or. ival(i)>kmax) then
             write (message,'(a,i0,a)') "Invalid layer specification '", ival(i), "' in keyword SMlay"
             call prterr(lundia, 'U021', trim(message))
             call d3stop(1, gdp)
          endif
       enddo
       !
       ! Adapt the size of array smlay in case kmaxout<kmax
       !
       if (kmaxout /= kmax+1) then
          deallocate (gdp%gdpostpr%smlay, stat = istat)
          allocate (gdp%gdpostpr%smlay(kmaxout), stat = istat)
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'RDPRFL: memory alloc error')
             call d3stop(1, gdp)
          endif
          smlay => gdp%gdpostpr%smlay
       endif
       !
       ! Fill smlay with the values of ival in increasing order
       !
       icount = 1
       do i = 0, kmax
          do j = 1, kmaxout
             if (ival(j) == i) then
                smlay(icount) = i
                icount        = icount + 1
                exit
             endif
          enddo
       enddo
       write(message,'(a)') "Map output is written for layer(s)"
       do i=1, size(smlay)
          write(message,'(a,a,i0)') trim(message), ' ', smlay(i)
       enddo
       call prterr(lundia, 'G051',trim(message))
    endif
    !
    ! SHlay
    !
    ival = -999
    call prop_get(gdp%mdfile_ptr, '*', 'SHlay', ival, kmax+1)
    if (ival(1) == -999) then
       !
       ! No layers specified for his-output
       ! Default: all layers to output, including zero: 0, 1, 2, ..., kmax
       !
       do i = 1, kmax+1
          shlay(i) = i-1
       enddo
    else
       !
       ! layers specified for map-output
       ! Detect the number of output layers(kmaxout):
       !   Search for the first i for which ival(i) == -999. Then kmaxout = i-1
       ! On the flow: check that the specified layers are valid
       ! 
       kmaxout = kmax + 1
       do i = 1, kmax+1
          if (ival(i) == -999) then
             kmaxout = i-1
             exit
          endif
          if (ival(i)<0 .or. ival(i)>kmax) then
             write (message,'(a,i0,a)') "Invalid layer specification '", ival(i), "' in keyword SHlay"
             call prterr(lundia, 'U021', trim(message))
             call d3stop(1, gdp)
          endif
       enddo
       !
       ! Adapt the size of array shlay in case kmaxout<kmax
       !
       if (kmaxout /= kmax) then
          deallocate (gdp%gdpostpr%shlay, stat = istat)
          allocate (gdp%gdpostpr%shlay(kmaxout), stat = istat)
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'RDPRFL: memory alloc error')
             call d3stop(1, gdp)
          endif
          shlay => gdp%gdpostpr%shlay
       endif
       !
       ! Fill shlay with the values of ival in increasing order
       !
       icount = 1
       do i = 0, kmax
          do j = 1, kmaxout
             if (ival(j) == i) then
                shlay(icount) = i
                icount        = icount + 1
                exit
             endif
          enddo
       enddo
       write(message,'(a)') "History output is written for layer(s)"
       do i=1, size(shlay)
          write(message,'(a,a,i0)') trim(message), ' ', shlay(i)
       enddo
       call prterr(lundia, 'G051',trim(message))
    endif
    !
    ! Read flag for test results in zsol file
    !
    call prop_get(gdp%mdfile_ptr, '*', 'Tstprt', tstprt)
    deallocate(ival, stat=istat)
end subroutine rdprfl
