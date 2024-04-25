subroutine get_swan_depth (sif,botfil)
!
! Head routine for calling read_bot
!
use swan_flow_grid_maps
implicit none
type(input_fields)          :: sif
character(*)                :: botfil
real                        :: fac =1.
   call read_bot (sif%dps     ,sif%mmax  ,sif%nmax  ,botfil    ,fac )
end subroutine get_swan_depth


subroutine read_bot(dpb       ,mb        ,nb        ,botfil    ,fac  )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer                , intent(in)  :: mb
    integer                , intent(in)  :: nb
    real                   , intent(in)  :: fac
    real, dimension(mb, nb)              :: dpb
    character(*)           , intent(in)  :: botfil
!
! Local variables
!
    integer           :: i
    integer           :: j
    integer           :: lunbot
    real              :: dummy
!
!! executable statements -------------------------------------------------------
!
    open (newunit = lunbot, file = botfil, status = 'unknown')
    do j = 1, nb
       read (lunbot, *, end = 999) (dpb(i, j), i = 1, mb), dummy
    enddo
    close (lunbot)
    do j = 1, nb
       do i = 1, mb
          dpb(i, j) = dpb(i, j)*fac
       enddo
    enddo
    return
  999 continue
    write (*, '('' Premature end of file while reading file: '',A)') botfil
    close (lunbot)
    call wavestop(1, ' Premature end of file while reading file: '//trim(botfil))
end subroutine read_bot 
