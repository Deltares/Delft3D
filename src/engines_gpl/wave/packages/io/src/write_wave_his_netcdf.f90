subroutine write_wave_his_netcdf (sg, sof, n_swan_grids, i_swan, wavedata)
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
    use wave_data
    use swan_flow_grid_maps
    use swan_input
    use netcdf
    use dwaves_version_module
    !
    implicit none
!
! Parameters
!
    integer, parameter :: SCANMODE    = 1
    integer, parameter :: READMODE    = 2
    integer, parameter :: COL_XP      = 1
    integer, parameter :: COL_YP      = 2
    integer, parameter :: COL_DEPTH   = 3
    integer, parameter :: COL_HSIG    = 4
    integer, parameter :: COL_DIR     = 5
    integer, parameter :: COL_RTPEAK  = 6
    integer, parameter :: COL_TM01    = 7
    integer, parameter :: COL_DSPR    = 8
    integer, parameter :: COL_UBOT    = 9
    integer, parameter :: COL_XWINDV  = 10
    integer, parameter :: COL_YWINDV  = 11
    integer, parameter :: COL_XVEL    = 12
    integer, parameter :: COL_YVEL    = 13
    integer, parameter :: VALSPERLINE = 13
    integer, parameter :: NAMLEN      = 40
!
! Global variables
!
    integer     , intent(in)  :: n_swan_grids ! number of swan grids
    integer     , intent(in)  :: i_swan       ! current swan grid number
    type (grid)               :: sg           ! swan grid
    type (output_fields)      :: sof          ! output fields defined on swan grid
    type (wave_data_type)     :: wavedata
!
! Local variables
!
    integer                                            :: fillun
    integer                                     , save :: hisoutputcount = 1
    integer                                            :: i
    integer                                            :: idfile
    integer                                            :: iddim_statnamlen
    integer                                            :: iddim_nstat
    integer                                            :: iddim_time
    integer                                            :: idvar_depth
    integer                                            :: idvar_hsig
    integer                                            :: idvar_dir
    integer                                            :: idvar_rtpeak
    integer                                            :: idvar_tm01
    integer                                            :: idvar_dspr
    integer                                            :: idvar_ubot
    integer                                            :: idvar_xwindv
    integer                                            :: idvar_ywindv
    integer                                            :: idvar_xvel
    integer                                            :: idvar_yvel
    integer                                            :: idvar_statid
    integer                                            :: idvar_statnam
    integer                                            :: idvar_time
    integer                                            :: idvar_x
    integer                                            :: idvar_y
    integer                                            :: ierror
    integer                                            :: ind
    integer                                            :: istat
    integer                                            :: loc
    integer                                            :: mode
    integer                                     , save :: nstat
    integer                                            :: precision
    integer                                            :: year
    integer                                            :: month
    integer                                            :: day
    integer, external                                  :: nc_def_var
    real(hp), dimension(:,:), allocatable       , save :: rval
    real(hp), dimension(:)  , allocatable       , save :: xcor
    real(hp), dimension(:)  , allocatable       , save :: ycor
    character(8)                                       :: cdate
    character(10)                                      :: ctime
    character(5)                                       :: czone
    character(256)                                     :: filename
    character(256)                                     :: gridnam
    character(500)                                     :: line
    character(NAMLEN), dimension(:), allocatable, save :: statnam
    character(NAMLEN), dimension(:), allocatable, save :: statid
    character(100)                                     :: string
    character(50)                                      :: tabfil
    character(256)                                     :: full_version
    logical                                            :: formatok
!
!! executable statements -------------------------------------------------------
!
    if (hisoutputcount == 1) then
       !
       ! Read the pntfilnam files
       ! SCANMODE: count the total number of stations
       ! end of SCANMODE: allocate needed arrays
       ! READMODE: read for each station: x coordinate, y coordinate, (optional) stationName
       !           no stationName present: stationName is "Station"
       !           stationId = istat + stationName
       !
       nstat = 0
       istat = 0
       do mode=SCANMODE, READMODE
          do loc=1,swan_run%nloc
             open (newunit = fillun, file = trim(swan_run%pntfilnam(loc)), status = 'old', iostat = ierror)
             if (ierror /= 0) then
                write(*,'(3a)') "ERROR: Write wavh-file: unable to open file '",trim(swan_run%pntfilnam(loc)), "' for reading."
                return
             endif
             !
             do
                read (fillun, '(a)', iostat = ierror) line
                if (ierror/=0 .or. len_trim(line)==0) exit
                if (mode == SCANMODE) then
                   nstat = nstat + 1
                else
                   istat = istat + 1
                   read(line,*, iostat = ierror) xcor(istat), ycor(istat), statnam(istat)
                   if ( statnam(istat) == ' ') then
                      statnam(istat) = "Station"
                   endif
                   write(statid(istat), '(i0,a)') istat, trim(statnam(istat))
                endif
             enddo
             close (fillun)
          enddo
          if (mode == SCANMODE) then
             !
             ! TO DO: deallocate these arrays at the end of a simulation
             !
             allocate(xcor(nstat), stat=ierror)
             allocate(ycor(nstat), stat=ierror)
             allocate(statnam(nstat), stat=ierror)
             allocate(statid(nstat), stat=ierror)
             if (ierror/=0) write(*,*) "ERROR allocating arrays in write_wave_his_netcdf"
             xcor    = -999.0_hp
             ycor    = -999.0_hp
             statnam = ' '
             statid  = ' '
          endif
       enddo
       !
       call getfullversionstring_dwaves(full_version)
       call date_and_time(cdate, ctime, czone)
       year  = wavedata%time%refdate / 10000
       month = (wavedata%time%refdate - year*10000) / 100
       day   = wavedata%time%refdate - year*10000 - month*100
    endif
    !
    ! Read the values from the tab file(s) and put them in array rval
    !
    allocate(rval(nstat,VALSPERLINE), stat=ierror)
    if (ierror/=0) write(*,*) "ERROR allocating arrays in write_wave_his_netcdf"
    rval    = -999.0_hp
    tabfil = ' '
    istat  = 1
    do loc=1,swan_run%nloc
       if (swan_run%pntfilnamtab(loc) == tabfil) then
          !
          ! Several pntfilnam data will appear in one tab file
          ! This file is already read
          !
          cycle
       else
          tabfil = swan_run%pntfilnamtab(loc)
       endif
       open (newunit = fillun, file = trim(tabfil), status = 'old', iostat = ierror)
       if (ierror /= 0) then
          write(*,'(3a)') "ERROR: Write wavh-file: unable to open file '",trim(tabfil), "' for reading."
          return
       endif
       !
       ! The tabfile is assumed to be in a fixed format, defined by a header line
       ! If this header line is not found, an error is generated
       ! TO DO: Currently only the first header of the tab file is checked,
       ! but a tab file may contain several headers
       !
       formatok = .false.
       do
          read (fillun, '(a)', iostat = ierror) line
          if (ierror/=0 .or. len_trim(line)==0) exit
          if (line(1:179) == "%       Xp            Yp            Depth         Hsig          Dir           RTpeak        Tm01          Dspr          Ubot        X-Windv       Y-Windv       X-Vel         Y-Vel") then
              !
              ! Correct header line found
              !
             formatok = .true.
          endif
          if (line(1:1) == '%') cycle
          if (.not.formatok) then
             write(*,'(3a)') "ERROR: Write wavh-file: unexpected format in file '",trim(tabfil), "'."
             return
          endif
          read(line,*, iostat = ierror) (rval(istat,i), i = 1, VALSPERLINE)
          if (ierror/=0) cycle
          istat = istat + 1
       enddo
       close(fillun, status='delete', iostat = ierror)
    enddo
    !
    ! Check the number of data fields read
    !
    if (istat-1 /= nstat) then
       write(*,'(2(a,i0))') "ERROR: Write wavh-file: Expecting ", nstat, " data lines but found ", istat-1
    endif
    !
    ! Check the x-y-coordinates. Unfortunately, the values in the tab file are rounded
    ! Unfortunately this check can only be done the first time (the xy cordinates are not stored for all SWAN grids)
    !
    if (hisoutputcount == 1) then
       do istat=1,nstat
          if (abs(rval(istat,COL_XP)-xcor(istat)) > 1.0_hp &
            & .or.  abs(rval(istat,COL_YP)-ycor(istat)) > 1.0_hp) then
             write(*,'(3a,2e20.10)') "ERROR: Unexpected x-y-coordinates found for station '", &
                  & trim(statid(istat)), "':", rval(istat,COL_XP), rval(istat,COL_YP)
          endif
       enddo
    endif
    !
    if (swan_run%netcdf_sp) then
       precision = nf90_float
       write(*,*) "Writing data to netcdf file in single precision (except the grid)"
    else
       ! default
       precision = nf90_double
    endif
    !
    ! define name of output file
    !
    if (n_swan_grids == 1) then
       write(filename,'(3a)')'wavh-', trim(swan_run%casl), '.nc'
    else
       gridnam = sg%grid_name
       ind = index(gridnam, '/', back = .true.)
       if (ind > 0) gridnam = gridnam(ind+1:)
       ind = index(gridnam, '\', back = .true.)
       if (ind > 0) gridnam = gridnam(ind+1:)
       ind = index(gridnam, '.', back = .true.)
       if (ind > 0) gridnam = gridnam(:ind-1)
       write(filename,'(5a)')'wavh-', trim(swan_run%casl), '-', trim(gridnam), '.nc'
    endif
    !
    if (hisoutputcount == 1) then
       !
       ! create file
       !
       ierror = nf90_create(filename, wavedata%output%ncmode, idfile); call nc_check_err(ierror, "creating file", filename)
       !
       ! global attributes
       !
       ierror = nf90_put_att(idfile, nf90_global,  'institution', trim(company)); call nc_check_err(ierror, "put_att global institution", filename)
       ierror = nf90_put_att(idfile, nf90_global,  'references', trim(company_url)); call nc_check_err(ierror, "put_att global references", filename)
       ierror = nf90_put_att(idfile, nf90_global,  'source', trim(full_version)); call nc_check_err(ierror, "put_att global source", filename)
       ierror = nf90_put_att(idfile, nf90_global,  'history', &
              'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
              ', '//trim(product_name)); call nc_check_err(ierror, "put_att global history", filename)
       !
       ! dimensions
       !
       ierror = nf90_def_dim(idfile, 'station_name_len', NAMLEN, iddim_statnamlen); call nc_check_err(ierror, "def_dim statnamlen", filename)
       ierror = nf90_def_dim(idfile, 'stations', nstat, iddim_nstat); call nc_check_err(ierror, "def_dim nstat", filename)
       ierror = nf90_def_dim(idfile, 'time', nf90_unlimited, iddim_time); call nc_check_err(ierror, "def_dim time", filename)
       !
       ! define vars
       !
       if (sg%sferic) then
          string = 'deg'
       else
          string = 'm'
       endif
       ! name, type, dims, standardname, longname, unit, xycoordinates
       idvar_x       = nc_def_var(idfile, 'station_x_coordinate'       , nf90_double, 1, (/iddim_nstat/), '', 'x coordinate station', trim(string), .false., filename)
       idvar_y       = nc_def_var(idfile, 'station_y_coordinate'       , nf90_double, 1, (/iddim_nstat/), '', 'y coordinate station', trim(string), .false., filename)
       write(string,'(a,i0.4,a,i0.2,a,i0.2,a)') 'seconds since ', year, '-', month, '-', day,' 00:00:00'
       idvar_time    = nc_def_var(idfile, 'time'    , nf90_double, 1, (/iddim_time/)                        , 'time', 'time'            , trim(string), .false., filename)
       idvar_statnam = nc_def_var  (idfile, 'station_name', nf90_char, 2, (/iddim_statnamlen, iddim_nstat/), ''    , 'Observation station name', ''  , .false., filename)
       ierror        = nf90_put_att(idfile, idvar_statnam , 'cf_role', 'timeseries_id')
       idvar_statid  = nc_def_var  (idfile, 'station_id'  , nf90_char, 2, (/iddim_statnamlen, iddim_nstat/), ''    , 'Observation station identifier', ''  , .false., filename)
       ierror        = nf90_put_att(idfile, idvar_statid  , 'cf_role', 'timeseries_id')
       idvar_depth   = nc_def_var(idfile, 'Depth'  , precision, 2, (/iddim_nstat, iddim_time/), ''    , 'Water depth', 'm'  , .true., filename)
       idvar_hsig    = nc_def_var(idfile, 'Hsig'   , precision, 2, (/iddim_nstat, iddim_time/), ''    , 'Significant wave height', 'm'  , .true., filename)
       idvar_dir     = nc_def_var(idfile, 'Dir'    , precision, 2, (/iddim_nstat, iddim_time/), ''    , 'Mean wave direction', 'degree'  , .true., filename)
       idvar_rtpeak  = nc_def_var(idfile, 'RTpeak' , precision, 2, (/iddim_nstat, iddim_time/), ''    , 'Peak period', 's'  , .true., filename)
       idvar_tm01    = nc_def_var(idfile, 'Tm01'   , precision, 2, (/iddim_nstat, iddim_time/), ''    , 'Mean absolute wave period', 's'  , .true., filename)
       idvar_dspr    = nc_def_var(idfile, 'Dspr'   , precision, 2, (/iddim_nstat, iddim_time/), ''    , 'Directional spreading of the waves', 'degree'  , .true., filename)
       idvar_ubot    = nc_def_var(idfile, 'Ubot'   , precision, 2, (/iddim_nstat, iddim_time/), ''    , 'Rms-value of the maxima of the orbital velocity near the bottom', 'm s-1'  , .true., filename)
       idvar_xwindv  = nc_def_var(idfile, 'X-Windv', precision, 2, (/iddim_nstat, iddim_time/), ''    , 'X component of the wind velocity', 'm s-1'  , .true., filename)
       idvar_ywindv  = nc_def_var(idfile, 'Y-Windv', precision, 2, (/iddim_nstat, iddim_time/), ''    , 'Y component of the wind velocity', 'm s-1'  , .true., filename)
       idvar_xvel    = nc_def_var(idfile, 'X-Vel'  , precision, 2, (/iddim_nstat, iddim_time/), ''    , 'X component of the current velocity', 'm s-1'  , .true., filename)
       idvar_yvel    = nc_def_var(idfile, 'Y-Vel'  , precision, 2, (/iddim_nstat, iddim_time/), ''    , 'Y component of the current velocity', 'm s-1'  , .true., filename)
       !
       ierror = nf90_enddef(idfile); call nc_check_err(ierror, "enddef", filename)
       !
       ! put vars (time independent)
       !
       ierror = nf90_put_var(idfile, idvar_x  , xcor  , start=(/ 1 /), count = (/ nstat /)); call nc_check_err(ierror, "put_var x", filename)
       ierror = nf90_put_var(idfile, idvar_y  , ycor  , start=(/ 1 /), count = (/ nstat /)); call nc_check_err(ierror, "put_var y", filename)
       do istat=1,nstat
          ierror = nf90_put_var(idfile, idvar_statnam, trim(statnam(istat)), start=(/ 1,istat /)); call nc_check_err(ierror, "put_var statnam", filename)
          ierror = nf90_put_var(idfile, idvar_statid , trim(statid (istat)), start=(/ 1,istat /)); call nc_check_err(ierror, "put_var statid", filename)
       enddo
       !
       ! The following allocated arrays are only needed when creating the output file and can be deallocated
       !
       deallocate(xcor   , stat=ierror)
       deallocate(ycor   , stat=ierror)
       deallocate(statnam, stat=ierror)
       deallocate(statid , stat=ierror)
    else
       !
       ! open file
       !
       ierror = nf90_open(filename, NF90_WRITE, idfile); call nc_check_err(ierror, "opening file", filename)
       !
       ierror = nf90_inq_varid(idfile, 'time'   , idvar_time  ); call nc_check_err(ierror, "inq_varid time   ", filename)
       ierror = nf90_inq_varid(idfile, 'Depth'  , idvar_depth ); call nc_check_err(ierror, "inq_varid Depth  ", filename)
       ierror = nf90_inq_varid(idfile, 'Hsig'   , idvar_hsig  ); call nc_check_err(ierror, "inq_varid Hsig   ", filename)
       ierror = nf90_inq_varid(idfile, 'Dir'    , idvar_dir   ); call nc_check_err(ierror, "inq_varid Dir    ", filename)
       ierror = nf90_inq_varid(idfile, 'RTpeak' , idvar_rtpeak); call nc_check_err(ierror, "inq_varid RTpeak ", filename)
       ierror = nf90_inq_varid(idfile, 'Tm01'   , idvar_tm01  ); call nc_check_err(ierror, "inq_varid Tm01   ", filename)
       ierror = nf90_inq_varid(idfile, 'Dspr'   , idvar_dspr  ); call nc_check_err(ierror, "inq_varid Dspr   ", filename)
       ierror = nf90_inq_varid(idfile, 'Ubot'   , idvar_ubot  ); call nc_check_err(ierror, "inq_varid Ubot   ", filename)
       ierror = nf90_inq_varid(idfile, 'X-Windv', idvar_xwindv); call nc_check_err(ierror, "inq_varid X-Windv", filename)
       ierror = nf90_inq_varid(idfile, 'Y-Windv', idvar_ywindv); call nc_check_err(ierror, "inq_varid Y-Windv", filename)
       ierror = nf90_inq_varid(idfile, 'X-Vel'  , idvar_xvel  ); call nc_check_err(ierror, "inq_varid X-Vel  ", filename)
       ierror = nf90_inq_varid(idfile, 'Y-Vel'  , idvar_yvel  ); call nc_check_err(ierror, "inq_varid Y-Vel  ", filename)
    endif
    !
    ! put vars (time dependent)
    !
    ierror = nf90_put_var(idfile, idvar_time   , wavedata%time%calctimtscale * wavedata%time%tscale, start=(/ hisoutputcount /)); call nc_check_err(ierror, "put_var time", filename)
    do istat=1,nstat
       ierror = nf90_put_var(idfile, idvar_depth , rval(istat,COL_DEPTH ), start=(/ istat, hisoutputcount /)); call nc_check_err(ierror, "put_var Depth  ", filename)
       ierror = nf90_put_var(idfile, idvar_hsig  , rval(istat,COL_HSIG  ), start=(/ istat, hisoutputcount /)); call nc_check_err(ierror, "put_var Hsig   ", filename)
       ierror = nf90_put_var(idfile, idvar_dir   , rval(istat,COL_DIR   ), start=(/ istat, hisoutputcount /)); call nc_check_err(ierror, "put_var Dir    ", filename)
       ierror = nf90_put_var(idfile, idvar_rtpeak, rval(istat,COL_RTPEAK), start=(/ istat, hisoutputcount /)); call nc_check_err(ierror, "put_var RTpeak ", filename)
       ierror = nf90_put_var(idfile, idvar_tm01  , rval(istat,COL_TM01  ), start=(/ istat, hisoutputcount /)); call nc_check_err(ierror, "put_var Tm01   ", filename)
       ierror = nf90_put_var(idfile, idvar_dspr  , rval(istat,COL_DSPR  ), start=(/ istat, hisoutputcount /)); call nc_check_err(ierror, "put_var Dspr   ", filename)
       ierror = nf90_put_var(idfile, idvar_ubot  , rval(istat,COL_UBOT  ), start=(/ istat, hisoutputcount /)); call nc_check_err(ierror, "put_var Ubot   ", filename)
       ierror = nf90_put_var(idfile, idvar_xwindv, rval(istat,COL_XWINDV), start=(/ istat, hisoutputcount /)); call nc_check_err(ierror, "put_var X-Windv", filename)
       ierror = nf90_put_var(idfile, idvar_ywindv, rval(istat,COL_YWINDV), start=(/ istat, hisoutputcount /)); call nc_check_err(ierror, "put_var Y-Windv", filename)
       ierror = nf90_put_var(idfile, idvar_xvel  , rval(istat,COL_XVEL  ), start=(/ istat, hisoutputcount /)); call nc_check_err(ierror, "put_var X-Vel  ", filename)
       ierror = nf90_put_var(idfile, idvar_yvel  , rval(istat,COL_YVEL  ), start=(/ istat, hisoutputcount /)); call nc_check_err(ierror, "put_var Y-Vel  ", filename)
    enddo
    ierror = nf90_close(idfile); call nc_check_err(ierror, "closing file", filename)
    deallocate(rval, stat=ierror)
    !
    ! Prepare hisoutputcount for next visit
    !
    if (i_swan == n_swan_grids) then
       hisoutputcount = hisoutputcount + 1
    endif
end subroutine write_wave_his_netcdf
