subroutine rdtimw(comfil    ,lundia    ,error     ,ntwav     , &
                & waverd    ,nmaxus    ,mmax      ,gdp       )
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
    use globaldata
    use m_alloc
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer                                   :: lundia !  Description and declaration in inout.igs
    integer                                   :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                   :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                   :: ntwav  !!  Total number of timesteps on comm. file (to read from)
    logical                     , intent(out) :: error  !!  Flag = TRUE if an error is encountered
    logical                                   :: waverd !!  Flag = TRUE if wave process and communication file exist
    character(*)                              :: comfil !!  Name for communication file com-<case><label>
!
! Local variables
!
    integer , dimension(:)         , pointer      :: timwav  ! Array with time steps on comm. file for wave results
    integer                                       :: fds
    integer                                       :: i
    integer                                       :: ierror  ! Flag for error when writing to Communication file 
    integer      , dimension(1)                   :: idummy  ! Help array to read integers
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: clsnef
    integer                        , external     :: getelt
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    character(16)                                 :: grnam1  ! Data-group name defined for the COM-files (CURNT) 
    character(16)                                 :: grnam2  ! Data-group name defined for the COM-files (CURTIM) 
    character(256)                                :: errmsg  ! Character var. containing the errormessage to be written to file. 
                                                             ! The message depends on the error. 
!
! Data statements
!
    data grnam1/'WAVNT'/
    data grnam2/'WAVTIM'/
!
!! executable statements -------------------------------------------------------
!
    timwav => gdp%gdtricom%timwav
    ierror = open_datdef(comfil, fds, .true.)
    if (ierror /= 0) goto 8888
    !
    ! initialize group index
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! Read the number of timesteps available at the file
    !
    idummy(1) = 0
    ierror = getelt(fds, grnam1, 'NTWAV', uindex, 1, 4, idummy)
    if (ierror/=0) goto 8888
    !
    ! Test if number of time steps on file are inside defined array boundary.
    ! If not then increase array size.
    !
    if (idummy(1)>size(timwav)) then
       call reallocP(gdp%gdtricom%timwav,max(2*size(timwav),idummy(1)))
       timwav => gdp%gdtricom%timwav
    endif
    !
    ! Read the array with time information (only read the new value(s))
    ! The time associated with NTWAV may have been updated (AppendCOM = false in MDW).
    !
    do i = max(1,ntwav), idummy(1)
       uindex (1,1) = i ! start index
       uindex (2,1) = i ! end index
       ierror = getelt(fds, grnam2, 'TIMWAV', uindex, 1, 4, timwav(i))
       if (ierror/=0) exit
    enddo
    !
    ierror = clsnef(fds)
    if (ierror/= 0) goto 9999
    !
 8888 continue
    if (ierror /= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
       ierror = clsnef(fds)
    endif
    !
9999 continue
    !
    ! Finally, update the number of time steps read.
    !
    ntwav = idummy(1)
end subroutine rdtimw
