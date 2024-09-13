subroutine VIRTUALlinkAVER_s1(volum0,s0,s1,dps,agsqs,gsqs,thick,NMlistMERGED,Nmerged,icx,icy,nmmax,nmlb,nmub,nst,kmax, dim_nmlist, gdp)
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
!   Function:   compute "spreaded"  values of s1
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
    real(fp), dimension(:), pointer :: agsqs_link
!
! global variables
!
    integer                                              , intent(in)    :: dim_nmlist
   ! real(fp), dimension(nmlb:nmub)                       , intent(inout) :: agsqs_loc
   ! real(fp), dimension(nmlb:nmub)                       , intent(inout) :: agsqs
    real(fp), dimension(nmlb:nmub, kmax)                 , intent(out)   :: volum0
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: agsqs
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: gsqs
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: s1
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: s0
    real(prec), dimension(nmlb:nmub)                     , intent(in)    :: dps
    real(fp), dimension(kmax)                            , intent(in)    :: thick 
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
    integer                                              , intent(in)    :: kmax
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
    real(fp)                   :: AA
!
! executable statements -------------------------------------------------------
! 
    agsqs_link => gdp%gdimbound%agsqs_link
      do nm=1,nmmax          
!          
         if (Nmerged(nm)>1) then
            s1_av = 0._fp
            Atot = 0._fp
            DO N = 1,Nmerged(nm)  
               nmN = NMlistMERGED(N,nm)  
               AA = agsqs_link(nmN)*gsqs(nmN)
               s1_av = s1_av  + s1(nmN)*AA
               Atot = Atot + AA
            ENDDO
            DO N = 1,Nmerged(nm)  
               nmN = NMlistMERGED(N,nm)   
               s1(nmN) = s1_av/Atot
               volum0(nmN,1:kmax) = thick(1:kmax)*(s0(nmN) + real(dps(nmN),fp))*agsqs_link(nmN)*gsqs(nmN)
            ENDDO
            
         endif
      enddo
!
!
RETURN
end subroutine VIRTUALlinkAVER_s1
