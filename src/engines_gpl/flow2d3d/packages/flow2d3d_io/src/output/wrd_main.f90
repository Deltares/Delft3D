subroutine wrd_main(lundia    ,error     ,ndro      ,itdroc    ,runtxt    , &
                  & trifil    ,dtsec     ,gdp       )
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
!    Function: Main routine for writing the FLOW HIS file.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall, only: inode, master, parll
    use datagroups
    use netcdf
    use flow2d3d_version_module
    !
    use globaldata
    !
    implicit none
    !
    include 'fsm.i'
    include 'tri-dyn.igd'
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer(pntrsize)                    , pointer :: namdro
    integer(pntrsize)                    , pointer :: mndro
    integer(pntrsize)                    , pointer :: itdro
    integer(pntrsize)                    , pointer :: ibuff
    integer(pntrsize)                    , pointer :: dxydro
    integer(pntrsize)                    , pointer :: xydro
    integer                              , pointer :: itdrof
    integer                              , pointer :: itdroi
    integer                              , pointer :: itdate
    real(fp)                             , pointer :: dt
    real(fp)                             , pointer :: tunit
    real(fp)                             , pointer :: tzone
    logical                              , pointer :: sferic
    !
    type (datagroup)                     , pointer :: group
    logical                              , pointer :: first
!
! Global variables
!
    integer                                                             , intent(in)  :: ndro   !  Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: itdroc !  Current time counter for the drogue data file
    integer                                                                           :: lundia !  Description and declaration in inout.igs
    logical                                                                           :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                                                          :: dtsec  !!  Integration time step [in seconds]
    character(30) , dimension(10)                                       , intent(in)  :: runtxt !!  Textual description of model input
    character(*)                                                        , intent(in)  :: trifil !  File name for FLOW NEFIS output
                                                                                                !  files (tri"h/m"-"casl""labl".dat/def)
!
! Local variables
!
    integer                                           :: irequest
    integer                                           :: ierror
    integer                                           :: filetype
    integer                                           :: fds
    integer                                , external :: open_datdef
    integer                                , external :: clsnef
    character(256)                                    :: filename
    !
    character(256)                                    :: full_version
    character(8)                                      :: cdate
    character(10)                                     :: ctime
    character(5)                                      :: czone
    character(1024)                                   :: error_string
    !
    character(16) :: simdat  ! Simulation date representing the flow condition at this date
    character(20) :: rundat  ! Execution date of the simulation
    character(6)  :: ftype   ! String containing to which output file version group should be written

!
!! executable statements -------------------------------------------------------
!
    namdro              => gdp%gdr_i_ch%namdro
    mndro               => gdp%gdr_i_ch%mndro
    itdro               => gdp%gdr_i_ch%itdro
    ibuff               => gdp%gdr_i_ch%ibuff
    dxydro              => gdp%gdr_i_ch%dxydro
    xydro               => gdp%gdr_i_ch%xydro
    itdrof              => gdp%gdinttim%itdrof
    itdroi              => gdp%gdinttim%itdroi
    itdate              => gdp%gdexttim%itdate
    dt                  => gdp%gdexttim%dt
    tunit               => gdp%gdexttim%tunit
    tzone               => gdp%gdexttim%tzone
    sferic              => gdp%gdtricom%sferic
    !
    call getdatagroup(gdp, FILOUT_DRO, 'dro-const', group)
    first               => group%first
    !
    call dattim(rundat    )
    simdat(1:16)  = 'yyyymmdd  hhmmss'
    simdat(1:4)   = rundat(1:4)
    simdat(5:6)   = rundat(6:7)
    simdat(7:8)   = rundat(9:10)
    simdat(11:12) = rundat(12:13)
    simdat(13:14) = rundat(15:16)
    simdat(15:16) = rundat(18:19)
    !
    call getfullversionstring_flow2d3d(full_version)
    call date_and_time(cdate, ctime, czone)
    !
    filename = trifil(1:3) // 'd' // trifil(5:)
    filetype = getfiletype(gdp, FILOUT_DRO)
    if (filetype == FTYPE_NETCDF) filename = trim(filename)//'.nc'
    !
    ierror = 0
    do irequest = REQUESTTYPE_DEFINE, REQUESTTYPE_WRITE
       !
       ! request REQUESTTYPE_DEFINE: define all groups, dimensions, and elements
       !         REQUESTTYPE_WRITE : write the data
       !
       if (irequest == REQUESTTYPE_DEFINE) then
          if (.not.first) cycle
          if (inode /= master) cycle
          call delnef(filename,gdp       )
       endif
       !
       ! create or open the file
       !
       if (inode /= master) then
          ! only the master needs to open the file
       elseif (first .and. irequest == REQUESTTYPE_WRITE) then
          ! the file has already been opened in step 1
       elseif (filetype == FTYPE_NEFIS) then
          if (first .and. irequest == REQUESTTYPE_DEFINE) then              
             write(lundia,*) 'Creating new '//trim(filename)//'.dat'
             write(lundia,*) 'Creating new '//trim(filename)//'.def'
          endif
          ierror = open_datdef(filename ,fds      , .false.)
          if (ierror /= 0) then
             write(error_string,'(2a)') 'While trying to open dat/def-file ',trim(filename)
             call prtnefiserr(trim(error_string), gdp)
          endif
       elseif (filetype == FTYPE_NETCDF) then
          if (first .and. irequest == REQUESTTYPE_DEFINE) then              
             write(lundia,*) 'Creating new '//trim(filename)
             ierror = nf90_create(filename, gdp%gdpostpr%nc_mode, fds); call nc_check_err(lundia, ierror, "creating file", filename)
             !
             ! global attributes
             !
             ierror = nf90_put_att(fds, nf90_global,  'Conventions', 'CF-1.6'); call nc_check_err(lundia, ierror, "put_att global Conventions", filename)
             ierror = nf90_put_att(fds, nf90_global,  'institution', trim('Deltares')); call nc_check_err(lundia, ierror, "put_att global institution", filename)
             ierror = nf90_put_att(fds, nf90_global,  'references', trim('www.deltares.nl')); call nc_check_err(lundia, ierror, "put_att global references", filename)
             ierror = nf90_put_att(fds, nf90_global,  'source', trim(full_version)); call nc_check_err(lundia, ierror, "put_att global source", filename)
             ierror = nf90_put_att(fds, nf90_global,  'history', &
                    'This file is created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
                    ', '//trim('Delft3D')); call nc_check_err(lundia, ierror, "put_att global history", filename)
          else
             ierror = nf90_open(filename, NF90_WRITE, fds); call nc_check_err(lundia, ierror, "opening file", filename)
          endif
       endif
       if (ierror/=0) goto 9999
       !
       ! time independent data
       !
       if (first) then
          call wridro(lundia    ,error     ,trifil    ,ndro      ,itdrof    , &
                    & itdroi    ,simdat    ,itdate    ,tunit     ,dt        , &
                    & ch(namdro),i(mndro)  ,i(itdro)  ,i(ibuff)  ,r(dxydro) , &
                    & irequest  ,fds       ,gdp       )
          if (error) goto 9999
       endif
       !
       ! data per time step
       !
       if (irequest==REQUESTTYPE_DEFINE .or. .not. first) then
          call wrtdro(lundia    ,error     ,trifil    ,itdroc    ,itdrof    , &
                    & itdroi    ,ndro      ,r(xydro)  ,sferic    ,irequest  , &
                    & fds       ,itdate    ,dtsec     ,gdp       )
          if (error) goto 9999
       endif
       !
       if (irequest == REQUESTTYPE_DEFINE) then
          !
          ! upon first request: define all groups, dimensions, and elements on file
          !
          call defnewgrp(fds, FILOUT_DRO, gdp, filename)
          !
          if (filetype == FTYPE_NETCDF) then
             !
             ierror = nf90_enddef(fds); call nc_check_err(lundia, ierror, "enddef", filename)
             if (ierror/=0) goto 9999
             !
          endif
       endif
    enddo
    !
9999 continue
    if (ierror/= 0) error = .true.
    if (inode == master) then
       if (filetype == FTYPE_NETCDF) then
          ierror = nf90_sync(fds); call nc_check_err(lundia, ierror, "sync file", filename)
          ierror = nf90_close(fds); call nc_check_err(lundia, ierror, "closing file", filename)
       elseif (filetype == FTYPE_NEFIS) then
          ierror = clsnef(fds)
          !
          ! wridoc needs to be called AFTER defnewgrp because this routine will
          ! add the dro-version not just to the file, but also to the group
          ! administration in memory and calling defnewgrp afterwards would thus
          ! duplicate definition of the dro-version group.
          !
          if (first) then
             ftype = 'dro'
             call wridoc(error     ,trifil    ,ftype     ,simdat    ,runtxt    , &
                       & .false.   ,''        ,gdp       )
          endif
       endif
       if (ierror/= 0) error = .true.
    endif
    first = .false.
end subroutine wrd_main
