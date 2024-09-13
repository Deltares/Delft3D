subroutine VIRTUALlinkAVER_s1bis(volum1,s1,dps,gsqsR,thick,NMlistMERGED,Nmerged,icx,icy,nmmax,nmlb,nmub,nst,kmax, dim_nmlist)
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
  use precision
!
  implicit none
!
! global variables
!
    integer                                              , intent(in)    :: dim_nmlist
   ! real(fp), dimension(nmlb:nmub)                       , intent(inout) :: agsqs_loc
   ! real(fp), dimension(nmlb:nmub)                       , intent(inout) :: agsqs
    real(fp), dimension(nmlb:nmub, kmax)                 , intent(out)   :: volum1
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: gsqsR
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: s1
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
!
! executable statements -------------------------------------------------------
! 
      do nm=1,nmmax          
!          
         if (Nmerged(nm)>1) then
            s1_av = 0._fp
            Atot = 0._fp
            DO N = 1,Nmerged(nm)  
               nmN = NMlistMERGED(N,nm)  
               s1_av = s1_av  + s1(nmN)*gsqsR(nmN)
               Atot = Atot + gsqsR(nmN)
            ENDDO
            DO N = 1,Nmerged(nm)  
               nmN = NMlistMERGED(N,nm)   
               s1(nmN) = s1_av/Atot
             !  volum1(nmN,1:kmax) = thick(1:kmax)*(s1(nmN) + real(dps(nmN),fp))*gsqsR(nmN)
            ENDDO
            
         endif
      enddo
!
!
RETURN
end subroutine VIRTUALlinkAVER_s1bis
