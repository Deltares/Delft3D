subroutine urdbcc(lundia    ,error     ,filusr    ,ltur      ,kmax      , &
                & nto       ,ubnd      ,gdp       )
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
!    Function: - Test existence of user defined file for
!                boundary conditions
!              - Read of input from user defined files and
!                initialize UBND array for 2 directions, LTUR
!                constituents/turbulent quantities, KMAX+1 layers
!                2 boundary ends & NTO open boundary sections
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                     , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                   :: lundia !  Description and declaration in inout.igs
    integer                                     , intent(in)  :: nto    !  Description and declaration in esm_alloc_int.f90
    logical                                     , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(2, ltur, 0:kmax, 2, nto), intent(out) :: ubnd   !  Description and declaration in trisol.igs
    character(256)                                            :: filusr !  Description and declaration in usrpar.igs
!
! Local variables
!
    integer                :: into    ! Initial boundary number to test read input 
    integer                :: iocond  ! IO status output: > 0 error < 0 end-of-file = 0 ok 
    integer                :: k       ! Loop variable 
    integer                :: l       ! Loop variable 
    integer                :: laag    ! Read layer number to test sequen- tial reading 
    integer                :: lfile   ! Length of file name 
    integer                :: luntmp  ! Unit number for attribute file 
    integer                :: n       ! Loop variable 
    integer                :: newlun
    integer                :: nrand   ! Read boundary number 
    logical                :: ex      ! Logical flag for file existence 
    real(fp)               :: rdef    ! Default value for boundary condition input for turbulent quantities should be < 0 !! 
    real(fp), dimension(2) :: rval    ! Help array to read boundary condi- tion input for turbulent quantities 
!
!! executable statements -------------------------------------------------------
!
    !-----Initialize local parameters
    !
    rdef = -999.999
    !
    !-----Initialize array
    !
    do l = 1, ltur
       do k = 0, kmax
          do n = 1, nto
             ubnd(1, l, k, 1, n) = rdef
             ubnd(1, l, k, 2, n) = rdef
             ubnd(2, l, k, 1, n) = rdef
             ubnd(2, l, k, 2, n) = rdef
          enddo
       enddo
    enddo
    !
    !-----test file existence, if so read
    !
    lfile = index(filusr, ' ')
    if (lfile==0) lfile = 13
    lfile = lfile - 1
    !
    inquire (file = filusr(1:lfile), exist = ex)
    if (ex) then
       !
       !--------file = exist
       !
       open (newunit=luntmp, file = filusr(1:lfile), form = 'formatted', status = 'old')
       into = 0
       !
       !-->     read boundary condition input (each boundary is optional)
       !
 1000  continue
       read (luntmp, *, iostat = iocond) nrand, laag, (rval(l), l = 1, ltur)
       if (iocond/=0) then
          !
          !--------------reading error?
          !
          if (iocond>0) then
             call prterr(lundia    ,'G007'    ,filusr(1:lfile)      )
             !
             error = .true.
          endif
          goto 1100
       endif
       !
       !-----------Read boundary not succeeding the previous read boundary ?
       !              into+1 < nrand skipp in between boundaries
       !              into+1 > nrand write sequence error in file
       !
       if (into + 1>nrand) then
          call prterr(lundia    ,'G007'    ,filusr(1:lfile)      )
          !
          error = .true.
          goto 1100
       endif
       !
       into = nrand
       !
       !-----------Read boundary layer not first one to read (:= 0)
       !
       if (laag/=0) then
          call prterr(lundia    ,'G007'    ,filusr(1:lfile)      )
          !
          error = .true.
          goto 1100
       endif
       !
       !-----------Define boundary coniditions for boundary nrand and layer 0
       !
       do l = 1, ltur
          ubnd(1, l, 0, 1, into) = rval(l)
          ubnd(1, l, 0, 2, into) = rval(l)
          ubnd(2, l, 0, 1, into) = rval(l)
          ubnd(2, l, 0, 2, into) = rval(l)
       enddo
       !
       !-----------Read boundary coniditions for next kmax layers
       !
       do k = 1, kmax
          read (luntmp, *, iostat = iocond) nrand, laag, (rval(l), l = 1, ltur)
          if (iocond/=0) then
             !
             !-----------------reading error / end-of-file (both not permitted)
             !
             call prterr(lundia    ,'G007'    ,filusr(1:lfile)      )
             !
             error = .true.
             goto 1100
          endif
          !
          !--------------Read boundary number not expected ? (should be into)
          !
          if (nrand/=into) then
             call prterr(lundia    ,'G007'    ,filusr(1:lfile)      )
             !
             error = .true.
             goto 1100
          endif
          !
          !--------------Read boundary layer not expected ? (should be k)
          !
          if (laag/=k) then
             call prterr(lundia    ,'G007'    ,filusr(1:lfile)      )
             !
             error = .true.
             goto 1100
          endif
          do l = 1, ltur
             ubnd(1, l, k, 1, into) = rval(l)
             ubnd(1, l, k, 2, into) = rval(l)
             ubnd(2, l, k, 1, into) = rval(l)
             ubnd(2, l, k, 2, into) = rval(l)
          enddo
       enddo
       !
       goto 1000
       !
       ! <--    next boundary number
       !
       !
       !--------close file
       !
 1100  continue
       close (luntmp)
    else
       !
       !--------file = not exist
       !
       call prterr(lundia    ,'G004'    ,filusr(1:lfile)      )
       !
       error = .true.
    endif
end subroutine urdbcc
