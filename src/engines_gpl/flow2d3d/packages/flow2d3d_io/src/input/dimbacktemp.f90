subroutine dimbacktemp(lundia    ,lconst    ,lstsci    ,gdp       )
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
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
! - Reads dimension background temperature constituents from an attribute file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use string_module
    use globaldata
    use message_module
    use m_rdsed
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical  , dimension(:) , pointer :: flbcktemp
!
! Global variables
!
    integer , intent(in)  :: lconst
    integer , intent(in)  :: lstsci    
    integer               :: lundia  ! Description and declaration in inout.igs
!
! Local variables
!
    integer                                                :: i
    integer                                                :: icount
    integer                                                :: istat
    integer                                                :: j
    character(6)                                           :: keyword
    character(11)                                          :: keyword2
    character(20)   , dimension(:) , allocatable           :: tempbackconst ! Names of the constituents as read in md-file
!
!! executable statements -------------------------------------------------------
!
    flbcktemp   => gdp%gdheat%flbcktemp
    !
    ! allocate dynamic memory
    ! Allocate using the gdp structure itself instead of the local pointers
    ! 
    allocate (gdp%gdheat%flbcktemp(lstsci)  , stat=istat)
    !  
    ! Check for background temperature
    call prop_get(gdp%mdfile_ptr, '*', 'Bcktem' , gdp%gdheat%back_temp)
    ! 
    icount = lstsci
    allocate(tempbackconst(icount))
    tempbackconst = ' '
    
    if (.not. gdp%gdheat%back_temp) then
       do i = 1, lstsci
          gdp%gdheat%flbcktemp(i) = .false.
       enddo
       goto 9999
    endif
    !
    ! populate tempbackconst array
    !
    keyword= 'Namc  '
    i = 0
    do j = lstsci-lconst +1, lstsci
       i = i + 1
       if (i<10) then
          write (keyword(5:5), '(i1)') i
       else
          write (keyword(5:6), '(i2)') i
       endif
       call prop_get(gdp%mdfile_ptr, '*', keyword, tempbackconst(j))
       call small(tempbackconst(j) ,999 )
    enddo
    !
    ! locate 'temperature' record; file containing background temperature constituents
    !
    do i = 1, lstsci
       gdp%gdheat%flbcktemp(i) = .false.
       keyword2 = tempbackconst(i)(1:11)
       if (keyword2 == 'temperature') then
          gdp%gdheat%flbcktemp(i) = .true.
       endif
    enddo       
    !
 9999 continue
end subroutine dimbacktemp
