subroutine initdredge(gddredge)
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
!  $Id: initdredge.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/data/src/gdp/initdredge.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata, only: sv_dredge
    !
    implicit none
    !
    type(sv_dredge) :: gddredge
    !
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    nullify(gddredge%link_percentage)
    nullify(gddredge%link_distance)
    nullify(gddredge%link_sum)
    nullify(gddredge%dzdred)
    nullify(gddredge%refplane)
    nullify(gddredge%voldred)
    nullify(gddredge%totvoldred)
    nullify(gddredge%globalareadred)
    nullify(gddredge%voldune)
    nullify(gddredge%percsupl)
    nullify(gddredge%totvoldump)
    nullify(gddredge%localareadump)
    nullify(gddredge%globalareadump)
    nullify(gddredge%globaldumpcap)
    nullify(gddredge%voldump)
    !
    gddredge%dredge_domainnr = 0
    gddredge%dredge_ndomains = 0
    gddredge%nadred          = 0
    gddredge%nadump          = 0
    gddredge%nasupl          = 0
    gddredge%nalink          = 0
    gddredge%ntimaccum       = 0
    !
    nullify(gddredge%link_def)
    nullify(gddredge%ndredged)
    nullify(gddredge%nploughed)
    !
    gddredge%tsmortime       = .false.
    gddredge%firstdredge     = .true.
    !
    nullify(gddredge%dredge_areas)
    nullify(gddredge%dump_areas)
    gddredge%dredgefile      = ' '
    !
    nullify(gddredge%dredge_prop)
    nullify(gddredge%dump_prop)
end subroutine initdredge
