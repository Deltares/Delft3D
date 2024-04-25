subroutine wrkent(comfil    ,lundia    ,error     ,itcur     ,ntcur     , &
                & itimc     ,mmax      ,nmax      ,nmaxus    ,kfu       , &
                & kfv       ,ibuff     ,gdp       )
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
    integer                  , pointer :: celidt
    type (datagroup)         , pointer :: group
!
! Global variables
!
    integer                                                     , intent(in)  :: itcur  !!  Current time counter for the communication file, where starting point depend on CYCLIC
    integer                                                     , intent(in)  :: itimc  !!  Current time step counter for 2D system
    integer                                                                   :: lundia !  Description and declaration in inout.igs
    integer                                                                   :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                   :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                   :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                     , intent(in)  :: ntcur  !!  Total number of timesteps on communication file (to write to)
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmaxus, mmax)                                          :: ibuff  !  Description and declaration in esm_alloc_int.f90
    logical                                                     , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)                                                              :: comfil !!  Name for communication file
                                                                                        !!  com-<case><label>
!
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: ierror ! Flag for error when writing to Communication file 
    integer                                       :: m
    integer                                       :: n
    integer      , dimension(1)                   :: idummy ! Help array to read/write Nefis files 
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: putelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(16)                                 :: grnam1 ! Data-group name defined for the COM-files (KENMNT) 
    character(16)                                 :: grnam2 ! Data-group name defined for the COM-files (KENMTIM) 
    character(256)                                :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
! Data statements
!
    data grnam1/'KENMNT'/
    data grnam2/'KENMTIM'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_COM, grnam1, group)
    first   => group%first
    celidt  => group%celidt
    !
    !-----Set up the element dimensions
    !
    if (first) then
       !
       ! Set up the element chracteristics
       !
       call addelm(gdp, lundia, FILOUT_COM, grnam1, 'NTCUR', ' ', IO_INT4, 1, (/1/), ' ', 'Number of current fields in groups CURTIM and KENMTIM', '[   -   ]')
       !
       call addelm(gdp, lundia, FILOUT_COM, grnam2, 'TIMCUR', ' ', IO_INT4, 1, (/1/), ' ', 'Time of current field rel.to reference date/time', '[ TSCALE]')
       call addelm(gdp, lundia, FILOUT_COM, grnam2, 'KFU', ' ', IO_INT4, 2, (/nmaxus, mmax/), ' ', '0/1 Non-active/Active u-point', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grnam2, 'KFV', ' ', IO_INT4, 2, (/nmaxus, mmax/), ' ', '0/1 Non-active/Active v-point', '[   -   ]')
    endif
    !
    ierror = open_datdef(comfil   ,fds      , .false.)
    if (ierror /= 0) goto 9999
    !
    if (first) then
       call defnewgrp(fds, FILOUT_COM, grnam1, gdp, comfil, errlog=ERRLOG_NONE)
       call defnewgrp(fds, FILOUT_COM, grnam2, gdp, comfil, errlog=ERRLOG_NONE)
       first = .false.
    endif
    !
    ! initialize group index
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! element 'NTCUR'
    !
    idummy(1) = ntcur
    ierror = putelt(fds, grnam1, 'NTCUR', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! initialize group index
    !
    celidt = itcur
    !
    uindex (1,1) = celidt ! start index
    uindex (2,1) = celidt ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! element 'TIMCUR'
    !
    idummy(1) = itimc
    ierror = putelt(fds, grnam2, 'TIMCUR', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'KFU'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          ibuff(n, m) = kfu(n, m)
       enddo
    enddo
    ierror = putelt(fds, grnam2, 'KFU', uindex, 1, ibuff)
    if (ierror/= 0) goto 9999
    !
    ! element 'KFV'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          ibuff(n, m) = kfv(n, m)
       enddo
    enddo
    ierror = putelt(fds, grnam2, 'KFV', uindex, 1, ibuff)
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
end subroutine wrkent
