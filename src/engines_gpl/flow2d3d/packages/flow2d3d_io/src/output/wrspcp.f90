subroutine wrspcp(comfil    ,lundia    ,error     ,nsrc      ,namsrc    , &
                & mnksrc    ,xyzsrc    ,gdp       )
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
!
! Global variables
!
    integer                                        :: lundia !  Description and declaration in inout.igs
    integer                                        :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(7, nsrc)              :: mnksrc !  Description and declaration in esm_alloc_int.f90
    logical                          , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)     , dimension(3, nsrc)              :: xyzsrc !  Description and declaration in esm_alloc_real.f90
    character(*)                                   :: comfil !!  Name for communication file com-<case><label>
    character(20), dimension(nsrc)                 :: namsrc !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: i
    integer                                       :: isrc
    integer                                       :: ierror ! Flag for error when writing to Communication file 
    integer    , dimension(1)                     :: idummy ! Help array to write integers
    integer    , dimension(3,5)                   :: uindex
    integer                        , external     :: putels
    integer                        , external     :: putelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(16)                                 :: grpnam ! Data-group name defined for the COM-files 
    character(256)                                :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
! Data statements
!
    data grpnam/'SPECPOINTS'/
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
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'NSRC', ' ', IO_INT4, 1, (/1/), ' ', 'Number of sources', '[   -   ]')
       if (nsrc>0) then
          call addelm(gdp, lundia, FILOUT_COM, grpnam, 'NAMSRC', ' ', 20, 1, (/nsrc/), ' ', 'Name of discharge sources', '[   -   ]') !CHARACTER
          call addelm(gdp, lundia, FILOUT_COM, grpnam, 'MNKSRC', ' ', IO_INT4, 2, (/7, nsrc/), ' ', '(M,N,K) indices of discharge sources and time dep. location', '[   -   ]')
          call addelm(gdp, lundia, FILOUT_COM, grpnam, 'XYZSRC', ' ', IO_INT4, 2, (/3, nsrc/), ' ', 'Exact X,Y,Z-position', '[   M   ]')
       endif
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
    ! element 'NSRC'
    !
    idummy(1) = nsrc
    ierror = putelt(fds, grpnam, 'NSRC', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    if (nsrc>0) then
       !
       ! element 'NAMSRC'
       !
       ierror = putels(fds, grpnam, 'NAMSRC', uindex, 1, namsrc)
       if (ierror/= 0) goto 9999
       !
       ! element 'MNKSRC'
       !
       ierror = putelt(fds, grpnam, 'MNKSRC', uindex, 1, mnksrc)
       if (ierror/= 0) goto 9999
       !
       ! element 'XYZSRC'
       !
       call sbuff_checksize(3*nsrc)
       do isrc = 1, nsrc
          do i = 1, 3
             sbuff(i+(isrc-1)*3) = xyzsrc(i,isrc)
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'XYZSRC', uindex, 1, sbuff)
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
end subroutine wrspcp
