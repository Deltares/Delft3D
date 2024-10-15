subroutine fbcorr(lundia, nto, nambnd, typbnd, gdp)
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
!    Function: Reads flow boundary corrections file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use flow_tables
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    type (handletype)                  , pointer :: fbcrfile
    type (fbcrbndtype)  , dimension(:) , pointer :: fcrbnd
    logical                            , pointer :: fbccorrection
    character(256)                     , pointer :: fbcrfilnam
    integer                            , pointer :: julday
!
! Global variables
!
    integer                      , intent(in) :: lundia  !  Description and declaration in inout.igs 
    integer                      , intent(in) :: nto
    character(20), dimension(nto), intent(in) :: nambnd  !  Description and declaration in esm_alloc_char.f90
    character(1) , dimension(nto), intent(in) :: typbnd  !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                           :: istat
    integer                                           :: j        ! loop counter 
    character(20)                                     :: parname 
    character(20)                                     :: txtput2
    character(40)                                     :: txtput1 
    character(MAXTABLECLENGTH), dimension(:), pointer :: parnames 
!
!! executable statements -------------------------------------------------------
!
    fbcrfile       => gdp%gdflwpar%fbcrfile
    fcrbnd         => gdp%gdflwpar%fcrbnd
    fbccorrection  => gdp%gdflwpar%fbccorrection
    fbcrfilnam     => gdp%gdflwpar%fbcrfilnam
    julday         => gdp%gdinttim%julday
    !
    ! Corrective boundary conditions
    !
    fbcrfilnam = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Filbc0', fbcrfilnam)
    if (fbcrfilnam == ' ') then
       !
       ! No file specified, try old keyword Corbnd
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Corbnd', fbcrfilnam)
    endif
    if (fbcrfilnam == ' ') then
       !
       ! No file specified
       !
       return
    else
       fbccorrection = .true.
       txtput1       = 'Corrective boundary conditions file'
       write (lundia, '(3a)') txtput1, ': ', trim(fbcrfilnam)
       call flw_readtable(fbcrfile ,fbcrfilnam ,julday ,gdp)
                       allocate(parnames(2)         , stat = istat)
       if (istat == 0) allocate(gdp%gdflwpar%fcrbnd(nto), stat = istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'FBCORR: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       ! update local pointer
       !
       fcrbnd         => gdp%gdflwpar%fcrbnd
       !
       do j = 1, nto
          fcrbnd(j)%ibct  = 0
          fcrbnd(j)%btype = ' '
       enddo
       !
       do j = 1, nto
          txtput1 = 'Boundary name'
          write (lundia, '(2a,a20)') txtput1, ':', trim(nambnd(j))
          !
          ! Display boundary conditions
          !
          if (typbnd(j) == 'Z') then
             parname = 'water elevation (z) '
          elseif (typbnd(j) == 'C') then
             parname = 'current         (c) '
          elseif (typbnd(j) == 'Q') then
             parname = 'flux/discharge  (q) '
          elseif (typbnd(j) == 'R') then
             parname = 'riemann         (r) '
          elseif (typbnd(j) == 'T') then
             parname = 'total discharge (t) '
          elseif (typbnd(j) == 'N') then
             parname = 'neumann         (n) '
          endif
          fcrbnd(j)%btype = typbnd(j)
          !
          ! Check boundary conditions
          !
          ! Find entries in table
          !
          call flw_gettable(fbcrfile , nambnd(j) ,parname   , &
             & fcrbnd(j)%ibct(1)     , fcrbnd(j)%ibct(2)    , &
             & fcrbnd(j)%ibct(3)     , 0         ,gdp       )
          fcrbnd(j)%ibct(4) = 1
          txtput1           = '  Correction'
          !
          ! Check entries in table
          !
          if (fcrbnd(j)%ibct(3) == 0) then
             txtput2 = '                none'
          elseif (fcrbnd(j)%ibct(3) == 1) then
             !
             ! Uniform values
             !
             txtput2 = '             uniform'
             parnames(1) = trim(parname)
             !
          elseif (fcrbnd(j)%ibct(3) == 2) then
             !
             ! Values at "end A" and "end B"
             !
             txtput2     = '(end A,end B) linear'
             parnames(1) = parname // ' end A'
             parnames(2) = parname // ' end B'
             !
          endif
          !
          write (lundia, '(3a)') txtput1, ':', txtput2
          !
          if (fcrbnd(j)%ibct(3) > 0) then
             call flw_checktableparnames(fbcrfile  , parnames  , &
                & fcrbnd(j)%ibct(1)    , fcrbnd(j)%ibct(2)     , &
                & fcrbnd(j)%ibct(3)    , gdp       )
          endif
       enddo
       deallocate(parnames, stat = istat)
    endif
end subroutine fbcorr
