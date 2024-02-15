subroutine gdp_alloc(gdp)
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
    !
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
!
!
! Local variables
!
!
!
!! executable statements -------------------------------------------------------
!
    call iofiles_alloc(gdp)
    !
    allocate (gdp%gdadv2d)
    allocate (gdp%gdaddress)
    allocate (gdp%gdbedformpar)
    allocate (gdp%gdbubble)
    allocate (gdp%gdconst)
    allocate (gdp%gdconstd)
    allocate (gdp%gdcoup)
    allocate (gdp%gddatusr)
    allocate (gdp%gddischarge)
    allocate (gdp%d)
    allocate (gdp%griddim)
    allocate (gdp%gdveg3d)
    allocate (gdp%gdexttim)
    allocate (gdp%gdfmtbcc)
    allocate (gdp%gdfmtbct)
    allocate (gdp%gdfmtdis)
    allocate (gdp%gdfourier)
    allocate (gdp%gdheat)
    allocate (gdp%gdhtur2d)
    allocate (gdp%gdinout)
    allocate (gdp%gdinttim)
    allocate (gdp%gdiwearr)
    allocate (gdp%gdiwepar)
    allocate (gdp%gdkeywtd)
    allocate (gdp%gdluntmp)
    allocate (gdp%gdflwpar)
    allocate (gdp%gdsedpar)
    allocate (gdp%gdmassbal)
    allocate (gdp%gdmorpar)
    allocate (gdp%messages)
    allocate (gdp%gdmorlyr)
    allocate (gdp%gdmudcoe)
    allocate (gdp%gdnfl)
    allocate (gdp%gdnumeco)
    allocate (gdp%gdphysco)
    allocate (gdp%gdpointrs)
    allocate (gdp%gdprocs)
    allocate (gdp%gdprognm)
    allocate (gdp%gdr_i_ch)
    allocate (gdp%gdrdpara)
    allocate (gdp%gdrivpro)
    allocate (gdp%gdsobek)
    allocate (gdp%gdstations)
    allocate (gdp%gdtfzeta)
    allocate (gdp%gdtmpfil)
    allocate (gdp%gdtrachy)
    allocate (gdp%gdturcoe)
    allocate (gdp%gdusrpar)
    allocate (gdp%gdzmodel)
    allocate (gdp%gdnonhyd)
    !
    allocate (gdp%gdbetaro)
    allocate (gdp%gdbcdat)
    allocate (gdp%gdcline)
    allocate (gdp%gdculver)
    allocate (gdp%gddredge)
    allocate (gdp%gdtrapar)
    allocate (gdp%gderosed)
    allocate (gdp%gdf0isf1)
    allocate (gdp%gdincbc)
    allocate (gdp%gdincbcc)
    allocate (gdp%gdincwav)
    allocate (gdp%gdinibcc)
    allocate (gdp%gdinibct)
    allocate (gdp%gdinidis)
    allocate (gdp%gdpostpr)
    allocate (gdp%gdrestart)
    allocate (gdp%gdrtc)
    allocate (gdp%gdscour)
    allocate (gdp%gdsdu)
    allocate (gdp%gdsnel)
    allocate (gdp%gdtimers)
    allocate (gdp%gdtricom)
    allocate (gdp%gdtricom%timwav(1500))
    allocate (gdp%gdtrisol)
    allocate (gdp%gdu_ppr)
    allocate (gdp%gdupdbcc)
    allocate (gdp%gdupdbct)
    allocate (gdp%gdupddis)
    allocate (gdp%gdwaqpar)
    allocate (gdp%gdwrirst)
    allocate (gdp%gdwrline)
    allocate (gdp%gdz_initcg)
    !
    allocate (gdp%dd)
    !
    ! runid may be already allocated. See d3df_dll.F90, module gdp_entry, subroutine gdpAlloc
    !
    allocate (gdp%gdparall)
    allocate (gdp%iopartit)
    !
    if (.not. associated(gdp%runid)) then
       allocate (gdp%runid)
    endif
    allocate (gdp%uniqueid)
    !
    allocate (gdp%arch)
    allocate (gdp%errorcode)
    !
end subroutine gdp_alloc
