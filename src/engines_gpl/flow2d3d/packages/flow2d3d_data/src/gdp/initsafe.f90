subroutine initsafe(gdp)
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
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    use message_module
    use bedcomposition_module
    use morphology_data_module
    use dredge_data_module, only: initdredge
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    istat = 0
    call tree_create("Delft3D-FLOW input", gdp%input_tree)
    nullify (gdp%mdfile_ptr)
    !
    call initadv2d     (gdp)
    call initbcdat     (gdp)
    call initbedformpar(gdp)
    call initbubble    (gdp)
    call initcline     (gdp)
    call initculver    (gdp)
    call initcoup      (gdp)
    call initdischarge (gdp)
    call initveg3d     (gdp)
    call initdredge    (gdp%gddredge)
    call nullsedtra    (gdp%gderosed)
    call initf0isf1    (gdp)
    call initflwpar    (gdp)
    call initfmtbcc    (gdp)
    call initfmtbct    (gdp)
    call initfmtdis    (gdp)
    call initfourier   (gdp)
    call initheat      (gdp)
    call initincbc     (gdp)
    call initincbcc    (gdp)
    call initincwav    (gdp)
    call initkeywtd    (gdp)
    call initmassbal   (gdp)
    call nullmorpar    (gdp%gdmorpar)
    call nullsedpar    (gdp%gdsedpar)
    call nulltrapar    (gdp%gdtrapar)
    istat = initmorlyr (gdp%gdmorlyr)
    call initstack     (gdp%messages)
    call initpostpr    (gdp)
    call initrestart   (gdp)
    call initprocs     (gdp)
    call initrtc       (gdp)
    call initscour     (gdp)
    call initsnel      (gdp)
    call initsobek     (gdp)
    call initstations  (gdp)
    call inittimers    (gdp)
    call inittrachy    (gdp)
    call inittrisol    (gdp)
    call initupdbcc    (gdp)
    call initupdbct    (gdp)
    call initupddis    (gdp)
    call initu_ppr     (gdp)
    call initwaqpar    (gdp)
    call initwrirst    (gdp)
    call initwrline    (gdp)
    call initz_initcg  (gdp)
    call initzmodel    (gdp)
    call initsdu       (gdp)
    !
    call sbuff_init
    !
    call initdfparall  (gdp%gdparall) 
    call initdfparall  (gdp%iopartit) 
    ! 
    ! Since GDP allocation has not yet succeeded, calling prterr(...,gdp) and d3stop(...) does not work
    !
    if (istat /= 0) then
       write(*,*) 'ERROR during initialization of GDP structure'
       call throwexception()
    endif
end subroutine initsafe
