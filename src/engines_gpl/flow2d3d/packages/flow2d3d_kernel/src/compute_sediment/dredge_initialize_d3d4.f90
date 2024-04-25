subroutine dredge_initialize_d3d4(gdp)
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
!!--declarations----------------------------------------------------------------
    use dfparall, only: parll, inode, nproc
    use dredge_comm, only: dredgecommunicate
    use dredge_data_module, only: dredge_type
    use m_dredge_initialize, only: dredge_initialize
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer            , pointer :: lundia
    type (dredge_type) , pointer :: gddredge
!
! Global variables
!
! NONE
!
! Local variables
!
    logical                                  :: dredge_parll
    logical                                  :: error
    integer                        , pointer :: dredge_domainnr
    integer                        , pointer :: dredge_ndomains
!
!! executable statements -------------------------------------------------------
!
    gddredge            => gdp%gddredge
    lundia              => gdp%gdinout%lundia
    dredge_domainnr     => gddredge%dredge_domainnr
    dredge_ndomains     => gddredge%dredge_ndomains
    
    if (parll) then
       dredge_parll    = .true.
       dredge_domainnr = inode
       dredge_ndomains = nproc
    elseif (gdp%gdprognm%numdomains > 1) then
       dredge_parll    = .true.
       call start_dd_dredgecommunication (dredge_domainnr, dredge_ndomains)
       dredge_domainnr = dredge_domainnr+1
    else
       dredge_parll    = .false.
       dredge_domainnr = 1
       dredge_ndomains = 1
    endif
    !
    call dredge_initialize(gddredge, dredge_domainnr, dredge_ndomains, lundia, error, dredgecommunicate)
    !
    if (error) call d3stop(1, gdp)
end subroutine dredge_initialize_d3d4
