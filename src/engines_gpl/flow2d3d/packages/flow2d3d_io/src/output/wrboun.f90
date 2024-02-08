subroutine wrboun(comfil    ,lundia    ,error     ,norow     ,nocol     , &
                & noroco    ,nrob      ,nto       ,irocol    ,mnbnd     , &
                & nob       ,gdp       )
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
    type (datagroup)         , pointer :: group
!
! Global variables
!
    integer                                    :: lundia !  Description and declaration in inout.igs
    integer, intent(in)                        :: nocol  !  Description and declaration in esm_alloc_int.f90
    integer                                    :: noroco !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                        :: norow  !  Description and declaration in esm_alloc_int.f90
    integer                                    :: nrob   !  Description and declaration in esm_alloc_int.f90
    integer                                    :: nto    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, nto)                 :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, noroco)              :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(8, nrob)                :: nob    !  Description and declaration in esm_alloc_int.f90
    logical                      , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)                               :: comfil !!  Name for communication file
                                                         !!  com-<case><label>
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: ierror  ! Flag for error when writing to Communication file 
    integer      , dimension(1)                   :: idummy  ! Help array to read/write Nefis files 
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: putelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(16)                                 :: grpnam  ! Data-group name defined for the COM-files 
    character(256)                                :: errmsg  ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
! Data statements
!
    data grpnam/'BOUNDCNST'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_COM, grpnam, group)
    first   => group%first
    !
    if (first) then
       !
       ! Set up the element chracteristics
       !
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'NOROW', ' ', IO_INT4, 1, (/1/), ' ', 'Number of computational grid rows in IROCOL table', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'NOCOL', ' ', IO_INT4, 1, (/1/), ' ', 'Number of computational grid columns in IROCOL table', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'NOROCO', ' ', IO_INT4, 1, (/1/), ' ', 'NOROW+NOCOL', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'IROCOL', ' ', IO_INT4, 2, (/5, noroco/), ' ', 'Administration of zeta-points, IROCOL-table', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'NTO', ' ', IO_INT4, 1, (/1/), ' ', 'Number of open boundaries', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'MNBND', ' ', IO_INT4, 2, (/6, nto/), ' ', 'Open boundary begin and end points (ml, mh, nl, nh, kl, kh)', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'NROB', ' ', IO_INT4, 1, (/1/), ' ', 'Number of open boundary points', '[   -   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'NOB', ' ', IO_INT4, 2, (/8, nrob/), ' ', 'Administration of open boundary points', '[   -   ]')
    endif
    !
    ierror = open_datdef(comfil, fds, .false.)
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
    ! element 'NOROW'
    !
    idummy(1) = norow
    ierror = putelt(fds, grpnam, 'NOROW', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'NOCOL'
    !
    idummy(1) = nocol
    ierror = putelt(fds, grpnam, 'NOCOL', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'NOROCO'
    !
    idummy(1) = noroco
    ierror = putelt(fds, grpnam, 'NOROCO', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'IROCOL'
    !
    ierror = putelt(fds, grpnam, 'IROCOL', uindex, 1, irocol)
    if (ierror/= 0) goto 9999
    !
    ! element 'NTO'
    !
    idummy(1) = nto
    ierror = putelt(fds, grpnam, 'NTO', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'MNBND'
    !
    if (nto>0) then
       ierror = putelt(fds, grpnam, 'MNBND', uindex, 1, mnbnd(1:6,:))
       if (ierror/= 0) goto 9999
    endif
    !
    ! element 'NROB'
    !
    idummy(1) = nrob
    ierror = putelt(fds, grpnam, 'NROB', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! element 'NOB'
    !
    if (nrob>0) then
       ierror = putelt(fds, grpnam, 'NOB', uindex, 1, nob)
       if (ierror/= 0) goto 9999
    endif
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
end subroutine wrboun
