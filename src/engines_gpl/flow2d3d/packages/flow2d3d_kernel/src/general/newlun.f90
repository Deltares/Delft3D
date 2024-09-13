function newlun(gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: newlun.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/general/newlun.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: This routine gets an available unit specifier. It
!              returns an error if it didn't succeed.
! Method used:
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
    integer         :: newlun
                                   !!  Ineteger function used to attach
                                   !!  a new unit number to a var. LUN...
!
!
! Local variables
!
    integer                        :: iexit                ! Exit return value 
    integer                        :: ierr
    integer                        :: lunit                ! Help var. 
    logical                        :: opened               ! Logical flag = TRUE if the test file is already opened 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    call psemlun
    lunit = 31
    opened = .true.
    !
    !-----get unit specifier
    !
    !-->
    !
   10 continue
    if (opened .and. lunit<999) then
       lunit = lunit + 1
       inquire (unit = lunit, iostat = ierr, opened = opened)
       if (ierr /= 0) opened = .true.
       goto 10
    !
    ! <--
    !
    endif
    !
    !-----test if unit number is available
    !
    if (opened) then
       newlun = 0
       write (*, *) ' *** FATAL ERROR - New unit number not available'
       write (*, *) ' Abnormal end'
       !
       !--------stop routine for DELFT3D
       !
       iexit = 2
       call d3stop(iexit     ,gdp       )
    else
       newlun = lunit
    endif
    call vsemlun
end function newlun
