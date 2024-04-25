subroutine dfexchg ( iptr, ks, ke, itype, nm_pos, gdp)
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
!   Exchanges halo values of field array between neighbouring subdomains
!
!!--pseudo code and references--------------------------------------------------
!
!   if not parallel, return
!   actual update of field array based on its type
!
!
!!--declarations----------------------------------------------------------------
    use flow2d3d_timers
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    integer, intent(inout)        :: iptr      ! pointer to first element of field array
    integer, intent(in)           :: itype     ! type of data
    integer, intent(in)           :: ke        ! last index in vertical direction
    integer, intent(in)           :: ks        ! first index in vertical direction
    integer, intent(in)           :: nm_pos    ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
! Local variables
!
    integer, pointer :: lundia
    character(80)    :: msgstr ! string to pass message
!
!! executable statements -------------------------------------------------------
!
    !
    ! if not parallel, return
    !
    if (.not.parll) return
    !
    call timer_start(timer_dfexchg, gdp)
    !
    lundia => gdp%gdinout%lundia
    !
    ! actual update of field array based on its type
    !
    if ( itype == dfint ) then
        if (nm_pos==1) then
           call dfupdi_nm_pos1 ( iptr, ks, ke, gdp )
        else
           call dfupdi_nm_pos2( iptr, ks, ke, gdp )
        endif 
    elseif ( itype == dfreal ) then
       if (nm_pos==1) then
          call dfupdr_nm_pos1 ( iptr, ks, ke, gdp )
       else
          call dfupdr_nm_pos2( iptr, ks, ke, gdp )
       endif
    elseif ( itype == dfdble ) then
       if (nm_pos==1) then
          call dfupdd_nm_pos1 ( iptr, ks, ke, gdp )
       else
          call dfupdd_nm_pos2( iptr, ks, ke, gdp )
       endif
    else
       write (msgstr,'(a,i3)') 'Unknown type of field array to be update: ',itype
       call prterr(lundia, 'U021', trim(msgstr))
       call d3stop(1, gdp)
    endif
    call timer_stop(timer_dfexchg, gdp)
end subroutine dfexchg
