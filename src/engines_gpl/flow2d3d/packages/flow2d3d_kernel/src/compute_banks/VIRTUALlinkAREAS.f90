subroutine VIRTUALlinkAREAS(kfs,agsqs_link,agsqs,gsqs,NMlistMERGED,Nmerged,thresMERGE,icx,icy,nmmax,nmlb,nmub,nst, dim_nmlist, gdp)
!
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function:   compute "spreaded"  values of areas
!
!  Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    logical, pointer :: linkMINarea
!
! global variables
!
    integer                                              , intent(in)    :: dim_nmlist
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: agsqs_link
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: agsqs
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: gsqs
    integer, dimension(nmlb:nmub)                        , intent(in)    :: Nmerged
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kfs
!    integer, dimension(nmlb:nmub)                        , intent(in)    :: MERGEDwith
    integer, dimension(dim_NMlist,nmlb:nmub)             , intent(in)    :: NMlistMERGED
    real(fp)                                             , intent(in)    :: thresMERGE
    integer                                              , intent(in)    :: nst
    integer                                              , intent(in)    :: nmlb   
    integer                                              , intent(in)    :: nmub
    integer                                              , intent(in)    :: icx   
    integer                                              , intent(in)    :: icy
    integer                                              , intent(in)    :: nmmax
!
!
! local variables
!
    integer                    :: nm  
    integer                    :: nmn
    integer                    :: N
    real(fp)                   :: Aeach
    real(fp)                   :: Abig
    real(fp)                   :: AsmallTOT
    real(fp)                   :: Atot 
    real(fp)                   :: MINperc
!
! executable statements -------------------------------------------------------
! 
    linkMINarea => gdp%gdimbound%linkMINarea
!     initialize agsqs_link to agsqs
!
      do nm=1,nmmax          
         if (kfs(nm)==1) then
            agsqs_link(nm) = agsqs(nm)
         else
            agsqs_link(nm) = 1._fp !if zero sud gives NaN cause of zero division where kfs=0.
         endif
      enddo
!
!     change values of agsqs_link wherever linked
!
      do nm=1,nmmax 
         if (Nmerged(nm)>1) then
            Atot = 0._fp
            DO N = 1,Nmerged(nm)  
               nmN = NMlistMERGED(N,nm)  
               Atot = Atot + agsqs(nmN)*gsqs(nmN)          
            ENDDO
            IF (linkMINarea) then !small cells have minimum Area, the rest goes to the big cell
               AsmallTOT = 0._fp
               DO N = 2,Nmerged(nm)  
                  nmN = NMlistMERGED(N,nm)  
                  MINperc = thresMERGE
                  AsmallTOT = AsmallTOT + MINperc*gsqs(nmN)     
                  agsqs_link(nmN) = MINperc          
               ENDDO
               Abig = Atot - AsmallTOT
               agsqs_link(nm) = Abig/gsqs(nm)
            else !divide Area by total numnber of cells
               Aeach = Atot/Nmerged(nm)
               DO N = 1,Nmerged(nm)  
                  nmN = NMlistMERGED(N,nm)  
                  agsqs_link(nmN) = Aeach/gsqs(nmN)
               ENDDO
            endif
         endif
      enddo
!
!
RETURN
end subroutine VIRTUALlinkAREAS
