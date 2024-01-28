subroutine wrparm(comfil    ,lundia    ,error     , &
                & dt        ,nfltyp    ,tscale    ,itlen     ,it01      , &
                & it02      ,tzone     ,gdp       )
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
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    use datagroups
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical                  , pointer :: first
    type (datagroup)         , pointer :: group
    real(fp)                 , pointer :: rhow
    real(fp)                 , pointer :: ag
!
! Global variables
!
    integer     , intent(in)  :: it01   !  Description and declaration in esm_alloc_int.f90
    integer     , intent(in)  :: it02   !  Description and declaration in esm_alloc_int.f90
    integer     , intent(in)  :: itlen  !  Description and declaration in esm_alloc_int.f90
    integer                   :: lundia !  Description and declaration in inout.igs
    integer     , intent(in)  :: nfltyp !  Description and declaration in esm_alloc_int.f90
    logical     , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)    , intent(in)  :: dt     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(in)  :: tscale !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(in)  :: tzone  !  Description and declaration in exttim.igs
    character(*)              :: comfil !!  Name for communication file
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: i       ! Help variable
    integer                                       :: ierror  ! Flag for error when writing to Communication file 
    integer    , dimension(1)                     :: idummy  ! Help array to write integers
    integer    , dimension(3,5)                   :: uindex
    integer                        , external     :: putelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(16)                                 :: grpnam  ! Data-group name defined for the COM-files 
    character(256)                                :: errmsg  ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
! Data statements
!
    data grpnam/'PARAMS'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_COM, grpnam, group)
    first    => group%first
    rhow     => gdp%gdphysco%rhow
    ag       => gdp%gdphysco%ag
    !
    ! Set up the element dimensions
    !
    if (first) then
       !
       ! Set up the element chracteristics
       !
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'AG', ' ', IO_REAL4, 1, (/1/), ' ', 'Acceleration of gravity', '[  M/S2 ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'RHOW', ' ', IO_REAL4, 1, (/1/), ' ', 'Density of water', '[ KG/M3 ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'DT', ' ', IO_REAL4, 1, (/1/), ' ', 'Timestep FLOW', '[ TUNIT ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'NFLTYP', ' ', IO_INT4, 1, (/1/), ' ', 'Dry point proc. 0 = NO  1 = MEAN  2 = MAX  3 = MIN', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'TSCALE', ' ', IO_REAL4, 1, (/1/), ' ', 'Basic unit of time, expressed in seconds', '[   S   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'IT01', ' ', IO_INT4, 1, (/1/), ' ', 'Reference date', '[YYYYMMDD]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'IT02', ' ', IO_INT4, 1, (/1/), ' ', 'Reference time', '[ HHMMSS]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'TZONE', ' ', IO_REAL4, 1, (/1/), ' ', 'Local time zone', '[ HOUR  ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'ITLEN', ' ', IO_INT4, 1, (/1/), ' ', 'Length of tide cycle ; stand alone and no wave 0', '[ TSCALE]')
    endif
    !
    ierror = open_datdef(comfil   ,fds      , .false.)
    if (ierror /= 0) goto 9999
    !
    if (first) then
       call defnewgrp(fds, FILOUT_COM, grpnam, gdp, comfil, errlog=ERRLOG_NONE)
       first = .false.
    endif
    !
    ! initialize group index
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! element 'AG'
    !
    call sbuff_checksize(1)
    sbuff(1) = ag
    ierror = putelt(fds, grpnam, 'AG', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'RHOW'
    ! 
    sbuff(1) = rhow
    ierror = putelt(fds, grpnam, 'RHOW', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'DT'
    !
    sbuff(1) = dt
    ierror = putelt(fds, grpnam, 'DT', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'NFLTYP'
    !
    idummy(1) = nfltyp
    ierror = putelt(fds, grpnam, 'NFLTYP', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'TSCALE'
    !
    sbuff(1) = tscale
    ierror = putelt(fds, grpnam, 'TSCALE', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'IT01'
    !
    idummy(1) = it01
    ierror = putelt(fds, grpnam, 'IT01', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'IT02'
    !
    idummy(1) = it02
    ierror = putelt(fds, grpnam, 'IT02', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'TZONE'
    !
    sbuff(1) = tzone
    ierror = putelt(fds, grpnam, 'TZONE', uindex, 1, sbuff)
    if (ierror==9002) ierror=0 ! TZONE didn't exist on old files. Accept error for backward compatibility.
    if (ierror/= 0) goto 9999
    !
    ! element 'ITLEN'
    !
    idummy(1) = itlen
    ierror = putelt(fds, grpnam, 'ITLEN', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ierror = clsnef(fds)
    !
    ! write error message if error occured and set error= .true.
    !
9999   continue
    if (ierror /= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error= .true.
    endif
end subroutine wrparm
