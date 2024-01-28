subroutine chktra(lundia    ,error     ,nmax      ,mmax      ,ittdef    , &
                & ntrt      ,ittaru    ,nttaru    ,cdir      , &
                & flsedprop_rqrd       ,gdp       )
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
! - Checks used trachytopes are available in
! trachytope definitions.
! Called for U/V-directions
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
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
    integer                                    :: lundia
    integer                      , intent(in)  :: mmax
    integer                      , intent(in)  :: nmax
    integer                      , intent(in)  :: ntrt
    integer                      , intent(in)  :: nttaru
    integer, dimension(ntrt, 2)  , intent(in)  :: ittdef
    integer, dimension(nttaru, 3), intent(in)  :: ittaru
    logical                      , intent(out) :: error
    logical                                    :: flsedprop_rqrd
    character(1)                               :: cdir
!
! Local variables
!
    integer                     :: i
    integer                     :: itd
    integer                     :: itt
    integer                     :: nmsgnm
    integer                     :: nmsgtd
    integer, dimension(2)       :: numlen
    logical                     :: lfound
    character(12), dimension(2) :: cnum
    character(132)              :: cmsg
!
!! executable statements -------------------------------------------------------
!
    ! Set message counters
    !
    nmsgnm = 0
    nmsgtd = 0
    !
    ! Perform the checks.
    !
    do itt = 1, nttaru
       !
       ! Skip block separator
       !
       if (ittaru(itt, 1)== - 1 .and. ittaru(itt, 2)== - 1 .and. ittaru(itt, 3) &
         & == - 1) then
          goto 100
       endif
       !
       ! Check N,M values
       !
       if (ittaru(itt, 1)<=0 .or. ittaru(itt, 1)>nmax .or. ittaru(itt, 2)       &
         & <=0 .or. ittaru(itt, 2)>mmax) then
          !
          ! N or M out of range, produce message
          !
          nmsgnm = nmsgnm + 1
          error  = .true.
          !
          ! Break off the check if 10 messages are exceeded
          !
          if (nmsgnm>10) then
             call prterr(lundia    ,'J010'    ,cdir      )
             exit
          endif
          write (cnum(1), '(i12)') ittaru(itt, 1)
          call remove_leading_spaces(cnum(1)   ,numlen(1) )
          write (cnum(2), '(i12)') ittaru(itt, 2)
          call remove_leading_spaces(cnum(2)   ,numlen(2) )
          cmsg = 'Trachytopes in ' // cdir // '-Direction, ' //                 &
               & 'N, M out of Range: N = ' // cnum(1)(1:numlen(1))              &
               & // ', M = ' // cnum(2)(1:numlen(2))
          call prterr(lundia    ,'J001'    ,cmsg      )
       endif
       !
       ! Check if trachytope has been defined
       !
       lfound = .false.
       do itd = 1, ntrt
          if (ittaru(itt, 3)==ittdef(itd, 1)) then
             ! alternative without searching
             ! ittaru(itt,4) = itd
             !
             ! Check if grain sizes are required.
             !
             if (ittdef(itd, 2)==103 .or. ittdef(itd, 2)==104) then
                flsedprop_rqrd = .true.
             endif
             if (ittdef(itd, 2)==105 .or. ittdef(itd, 2)==106) then
                gdp%gdbedformpar%lfbedfrmrou = .true.
             endif
             lfound = .true.
             exit
          endif
       enddo
       if (.not.lfound) then
          !
          ! Check if this trachytope was used before.
          ! If so go to next item
          !
          do i = 1, itd - 1
             if (ittaru(itt, 3)==ittaru(i, 3)) goto 100
          enddo
          !
          ! Trachytope was not used before, produce message
          !
          nmsgtd = nmsgtd + 1
          error  = .true.
          !
          ! Break off the check if 10 messages are exceeded
          !
          if (nmsgtd==10) then
             call prterr(lundia    ,'J011'    ,cdir      )
             exit
          endif
          write (cnum(1), '(i12)') ittaru(itt, 3)
          call remove_leading_spaces(cnum(1)   ,numlen(1) )
          cmsg = 'Trachytope in ' // cdir // '-Direction ' //                   &
               & 'not defined, Number: ' // cnum(1)(1:numlen(1))
          call prterr(lundia    ,'J001'    ,cmsg      )
       endif
  100  continue
    enddo
end subroutine chktra
