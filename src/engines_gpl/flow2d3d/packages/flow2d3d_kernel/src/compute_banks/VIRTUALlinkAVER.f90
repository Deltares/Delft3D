subroutine VIRTUALlinkAVER(agsqs_loc,agsqs,gsqs,s1,NMlistMERGED,Nmerged,icx,icy,nmmax,nmlb,nmub,nst, dim_nmlist, gdp)
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
!   Function:   AVERAGE out water surface in linked cells
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
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: agsqs_loc
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: agsqs
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: gsqs
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: s1
    integer, dimension(nmlb:nmub)                        , intent(in)    :: Nmerged
!    integer, dimension(nmlb:nmub)                        , intent(in)    :: MERGEDwith
    integer, dimension(dim_NMlist,nmlb:nmub)             , intent(in)    :: NMlistMERGED
   ! real(fp)                                             , intent(in)    :: thresMERGE
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
    real(fp)                   :: Atot
    real(fp)                   :: s1_av
    real(fp)                   :: Area_spread
!
! executable statements -------------------------------------------------------
! 
    linkMINarea => gdp%gdimbound%linkMINarea
      do nm=1,nmmax          
!          
         if (Nmerged(nm)>1) then
            Atot = 0._fp
            DO N = 1,Nmerged(nm)  
               nmN = NMlistMERGED(N,nm)  
               Atot = Atot + agsqs(nmN)*gsqs(nmN)      
            ENDDO
            if (linkMINarea) then
               s1_av = 0._fp
               DO N = 1,Nmerged(nm)  
                  nmN = NMlistMERGED(N,nm)  
                  Area_spread = agsqs_loc(nmN)*gsqs(nmN)
                  s1_av = s1_av  + s1(nmN) *Area_spread 
               ENDDO
               DO N = 1,Nmerged(nm)  
                  nmN = NMlistMERGED(N,nm)   
                  s1(nmN) = s1_av/Atot 
               ENDDO
            else
               s1_av = 0._fp
               DO N = 1,Nmerged(nm)  
                  nmN = NMlistMERGED(N,nm)  
                 ! Area_spread = agsqs_loc(nmN)*gsqs(nmN)
                  s1_av = s1_av  + s1(nmN) !*Area_spread 
               ENDDO
               DO N = 1,Nmerged(nm)  
                  nmN = NMlistMERGED(N,nm)   
                  s1(nmN) = s1_av/Nmerged(nm)    ! DIV 3 SINCE Area_spread are the same
               ENDDO
            endif
            
         endif
      enddo
!
!
RETURN
end subroutine VIRTUALlinkAVER
