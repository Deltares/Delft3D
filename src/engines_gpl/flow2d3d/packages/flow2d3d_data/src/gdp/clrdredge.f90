subroutine clrdredge(istat, gddredge)
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
!  $Id: clrdredge.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/data/src/gdp/clrdredge.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow_tables, only: cleartable
    !
    use globaldata, only: sv_dredge
    !
    implicit none
    !
    type(sv_dredge) :: gddredge
!
! Global variables
!
    integer,intent(out) :: istat
!
! Local variables
!
    integer :: i
!
!! executable statements -------------------------------------------------------
!
    if (associated(gddredge%link_percentage)) deallocate (gddredge%link_percentage, STAT = istat)
    if (associated(gddredge%link_distance))   deallocate (gddredge%link_distance  , STAT = istat)
    if (associated(gddredge%link_sum))        deallocate (gddredge%link_sum       , STAT = istat)
    if (associated(gddredge%dzdred))          deallocate (gddredge%dzdred         , STAT = istat)
    if (associated(gddredge%refplane))        deallocate (gddredge%refplane       , STAT = istat)
    if (associated(gddredge%voldred))         deallocate (gddredge%voldred        , STAT = istat)
    if (associated(gddredge%totvoldred))      deallocate (gddredge%totvoldred     , STAT = istat)
    if (associated(gddredge%globalareadred))  deallocate (gddredge%globalareadred , STAT = istat)
    if (associated(gddredge%voldune))         deallocate (gddredge%voldune        , STAT = istat)
    if (associated(gddredge%percsupl))        deallocate (gddredge%percsupl       , STAT = istat)
    if (associated(gddredge%totvoldump))      deallocate (gddredge%totvoldump     , STAT = istat)
    if (associated(gddredge%localareadump))   deallocate (gddredge%localareadump  , STAT = istat)
    if (associated(gddredge%globalareadump))  deallocate (gddredge%globalareadump , STAT = istat)
    if (associated(gddredge%globaldumpcap))   deallocate (gddredge%globaldumpcap  , STAT = istat)
    if (associated(gddredge%voldump))         deallocate (gddredge%voldump        , STAT = istat)
    !
    if (associated(gddredge%link_def))        deallocate (gddredge%link_def       , STAT = istat)
    if (associated(gddredge%ndredged))        deallocate (gddredge%ndredged       , STAT = istat)
    if (associated(gddredge%nploughed))       deallocate (gddredge%nploughed      , STAT = istat)
    !
    if (associated(gddredge%dredge_areas))    deallocate (gddredge%dredge_areas   , STAT = istat)
    if (associated(gddredge%dump_areas))      deallocate (gddredge%dump_areas     , STAT = istat)
    !
    if (associated(gddredge%dredge_prop)) then
       do i = 1, gddredge%nadred
          if (associated(gddredge%dredge_prop(i)%nm))             deallocate (gddredge%dredge_prop(i)%nm                  , STAT = istat)
          if (associated(gddredge%dredge_prop(i)%inm))            deallocate (gddredge%dredge_prop(i)%inm                 , STAT = istat)
          if (associated(gddredge%dredge_prop(i)%area))           deallocate (gddredge%dredge_prop(i)%area                , STAT = istat)
          if (associated(gddredge%dredge_prop(i)%hdune))          deallocate (gddredge%dredge_prop(i)%hdune               , STAT = istat)
          if (associated(gddredge%dredge_prop(i)%dz_dredge))      deallocate (gddredge%dredge_prop(i)%dz_dredge           , STAT = istat)
          if (associated(gddredge%dredge_prop(i)%dunetoplevel))   deallocate (gddredge%dredge_prop(i)%dunetoplevel        , STAT = istat)
          if (associated(gddredge%dredge_prop(i)%triggerlevel))   deallocate (gddredge%dredge_prop(i)%triggerlevel        , STAT = istat)
          if (associated(gddredge%dredge_prop(i)%bedlevel))       deallocate (gddredge%dredge_prop(i)%bedlevel            , STAT = istat)
          if (associated(gddredge%dredge_prop(i)%troughlevel))    deallocate (gddredge%dredge_prop(i)%troughlevel         , STAT = istat)
          if (associated(gddredge%dredge_prop(i)%sedimentdepth))  deallocate (gddredge%dredge_prop(i)%sedimentdepth       , STAT = istat)
          if (associated(gddredge%dredge_prop(i)%sortvar))        deallocate (gddredge%dredge_prop(i)%sortvar             , STAT = istat)
          if (associated(gddredge%dredge_prop(i)%triggered))      deallocate (gddredge%dredge_prop(i)%triggered           , STAT = istat)
          if (associated(gddredge%dredge_prop(i)%reflevel))       deallocate (gddredge%dredge_prop(i)%reflevel            , STAT = istat)
       enddo
       deallocate (gddredge%dredge_prop    , STAT = istat)
    endif
    !
    if (associated(gddredge%dump_prop)) then
       do i = 1, gddredge%nadump
          if (associated(gddredge%dump_prop(i)%nm))               deallocate (gddredge%dump_prop(i)%nm                    , STAT = istat)
          if (associated(gddredge%dump_prop(i)%inm))              deallocate (gddredge%dump_prop(i)%inm                   , STAT = istat)
          if (associated(gddredge%dump_prop(i)%area))             deallocate (gddredge%dump_prop(i)%area                  , STAT = istat)
          if (associated(gddredge%dump_prop(i)%hdune))            deallocate (gddredge%dump_prop(i)%hdune                 , STAT = istat)
          if (associated(gddredge%dump_prop(i)%bedlevel))         deallocate (gddredge%dump_prop(i)%bedlevel              , STAT = istat)
          if (associated(gddredge%dump_prop(i)%dz_dump))          deallocate (gddredge%dump_prop(i)%dz_dump               , STAT = istat)
          if (associated(gddredge%dump_prop(i)%sortvar))          deallocate (gddredge%dump_prop(i)%sortvar               , STAT = istat)
          if (associated(gddredge%dump_prop(i)%reflevel))         deallocate (gddredge%dump_prop(i)%reflevel              , STAT = istat)
       enddo
       deallocate (gddredge%dump_prop      , STAT = istat)
    endif
    !
    call cleartable(gddredge%tseriesfile)
end subroutine clrdredge
