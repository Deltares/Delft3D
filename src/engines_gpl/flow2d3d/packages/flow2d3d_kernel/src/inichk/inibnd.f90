subroutine inibnd(lundia    ,error      ,nto       ,nopest    ,nrob      , &
                & mnbnd     ,nob       ,typbnd    ,gdp       )
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
!    Function: - Fills and checks the open boundary type
!              - Initialises NOB-array
!              - Checks whether boundary sections are defined
!                according to the rules (multiples of 45 degrees)
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
    include 'pardef.igd'
    logical      , pointer :: bndneu
!
! Global variables
!
    integer                                         :: lundia !  Description and declaration in inout.igs
    integer                           , intent(in)  :: nopest !  Description and declaration in dimens.igs
    integer                                         :: nrob   !  Description and declaration in esm_alloc_int.f90
    integer                           , intent(in)  :: nto    !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(7, nto)   , intent(in)  :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(8, nopest), intent(out) :: nob    !  Description and declaration in esm_alloc_int.f90
    logical                                         :: error   !!  Flag=TRUE if an error is encountered
    character(1), dimension(nto)      , intent(in)  :: typbnd !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                   :: incx   ! Increment step calculated from two x-coordinates (= 0, -1 or 1) 
    integer                   :: incy   ! Increment step calculated from two Y-coordinates (= 0, -1 or 1) 
    integer                   :: ix     ! Help var. 
    integer                   :: iy     ! Help var. 
    integer                   :: m      ! Current M-index of the active point in the current computational ROW 
    integer                   :: maxinc ! Maximum of (INCXA,INCYA) 
    integer                   :: n      ! Current N-index of the active point in the current computational COLUMN 
    integer                   :: nrobx  ! Help var. 
    integer                   :: nx1    ! M-coord. of the first  point 
    integer                   :: nx2    ! M-coord. of the second point 
    integer                   :: ny1    ! N-coord. of the first  point 
    integer                   :: ny2    ! N-coord. of the second point 
    integer, dimension(mxnto) :: ityp   ! Help var. for open boundary type 
    character(4)              :: errornr ! Number of the error message which will be printed in case of error 
!
!! executable statements -------------------------------------------------------
!
    bndneu  => gdp%gdnumeco%bndneu
    !
    ! initialisation local array (NTO < MXNTO see RDBNDD)
    !
    do n = 1, nto
       ityp(n) = 0
    enddo
    !
    ! fill ITYP array, depends on value of TYPBND
    !
    do n = 1, nto
       if (typbnd(n) == 'Z') then
          ityp(n) = 2
       elseif (typbnd(n) == 'C') then
          ityp(n) = 3
       elseif (typbnd(n) == 'Q') then
          ityp(n) = 5
       elseif (typbnd(n) == 'R') then
          if (bndneu) then
             ityp(n) = 8
          else
             ityp(n) = 6
          endif
       elseif (typbnd(n) == 'T') then
          ityp(n) = 7
       elseif (typbnd(n) == 'N') then
          ityp(n) = 8
       else
          error = .true.
          errornr = 'U047'
          goto 9999
       endif
    enddo
    !
    ! initialize NOB array (highest dimension here nopest, all next
    ! calls will be with NROB)
    !
    do n = 1, nto
       nx1 = mnbnd(1, n)
       ny1 = mnbnd(2, n)
       nx2 = mnbnd(3, n)
       ny2 = mnbnd(4, n)
       call increm(nx1       ,ny1       ,nx2       ,ny2       ,incx      , &
                 & incy      ,maxinc    ,error      )
       !
       ! if error then no multiple of 45 degrees
       !
       if (error) then
          errornr = 'V025'
          goto 9999
       endif
       !
       ix = nx1 - incx
       iy = ny1 - incy
       do m = 1, maxinc + 1
          nrob          = nrob + 1
          nrobx         = min(nrob, nopest)
          ix            = ix + incx
          iy            = iy + incy
          nob(1, nrobx) = ix
          nob(2, nrobx) = iy
          nob(3, nrobx) = ityp(n)
          nob(8, nrobx) = n
       enddo
    enddo
    !
    ! check array boundary of NOB array (NROB .gt. NOPEST)
    ! if NROB .gt. NOPEST then NOB will get out of bound
    !
    if (nrob > nopest) then
       error   = .true.
       errornr = 'V026' 
       goto 9999
    endif
   
 9999 continue
    if (error) call prterr(lundia, errornr, ' ')
end subroutine inibnd
