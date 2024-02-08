subroutine expblk(lundia    ,ittaru    ,rttaru    ,nttaru    ,iblbeg    , &
                & nblcks    ,nmpblk    ,error     ,gdp       )
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
! Expands block trachytope data to
! N, M data
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
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
    integer                       , intent(in)  :: iblbeg
    integer                                     :: lundia
    integer                       , intent(in)  :: nblcks
    integer                       , intent(in)  :: nttaru
    integer , dimension(4)        , intent(in)  :: nmpblk
    integer , dimension(nttaru, 3)              :: ittaru
    logical                       , intent(out) :: error
    real(fp), dimension(nttaru)                 :: rttaru
!
! Local variables
!
    integer :: ib
    integer :: icur
    integer :: im
    integer :: in
    integer :: m1
    integer :: m2
    integer :: n1
    integer :: n2
!
!! executable statements -------------------------------------------------------
!
    ! Determine N.M data
    !
    n1 = min(nmpblk(1), nmpblk(3))
    m1 = min(nmpblk(2), nmpblk(4))
    n2 = max(nmpblk(1), nmpblk(3))
    m2 = max(nmpblk(2), nmpblk(4))
    !
    ! Set the current index
    !
    icur = iblbeg + nblcks
    !
    ! The expansion
    !
    ! First finalize M1
    !
    do in = n1 + 1, n2
       do ib = 1, nblcks
          !
          ! Increase index and check it.
          !
          icur = icur + 1
          if (icur>nttaru) then
             call prterr(lundia    ,'J007'    ,' '       )
             error = .true.
             goto 9999
          endif
          !
          ! Store the data
          !
          ittaru(icur, 1) = in
          ittaru(icur, 2) = m1
          ittaru(icur, 3) = ittaru(iblbeg + ib, 3)
          rttaru(icur) = rttaru(iblbeg + ib)
       enddo
    enddo
    !
    ! The rest from M1+1 to M2
    !
    do im = m1 + 1, m2
       do in = n1, n2
          do ib = 1, nblcks
             !
             ! Increase index and check it.
             !
             icur = icur + 1
             if (icur>nttaru) then
                call prterr(lundia    ,'J007'    ,' '       )
                error = .true.
                goto 9999
             endif
             !
             ! Store the data
             !
             ittaru(icur, 1) = in
             ittaru(icur, 2) = im
             ittaru(icur, 3) = ittaru(iblbeg + ib, 3)
             rttaru(icur) = rttaru(iblbeg + ib)
          enddo
       enddo
    enddo
    !
 9999 continue
end subroutine expblk
