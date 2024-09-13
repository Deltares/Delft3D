subroutine VIRTUALlink_gsqsR(gsqsR,gsqs,volum0,volum1,volum0L,volum1L,NMlistMERGED,Nmerged,thresMERGE,icx,icy,nmmax,nmlb,nmub,nst,kmax, dim_nmlist, gdp)
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
!   Function:   compute "spreaded"  values of gsqsR
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
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: gsqsR
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: gsqs
    real(fp), dimension(nmlb:nmub,1:kmax)                , intent(inout) :: volum0
    real(fp), dimension(nmlb:nmub,1:kmax)                , intent(inout) :: volum0L
    real(fp), dimension(nmlb:nmub,1:kmax)                , intent(inout) :: volum1L
    real(fp), dimension(nmlb:nmub,1:kmax)                , intent(inout) :: volum1
    integer, dimension(nmlb:nmub)                        , intent(in)    :: Nmerged
!    integer, dimension(nmlb:nmub)                        , intent(in)    :: MERGEDwith
    integer, dimension(dim_NMlist,nmlb:nmub)             , intent(in)    :: NMlistMERGED
    real(fp)                                             , intent(in)    :: thresMERGE
    integer                                              , intent(in)    :: nst
    integer                                              , intent(in)    :: kmax
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
    real(fp)                   :: Atot
    real(fp)                   :: vol0(1:kmax)
    real(fp)                   :: vol1(1:kmax)
    real(fp)                   :: Amin,AsmallTOT,Abig
!
! executable statements -------------------------------------------------------
! 
    linkMINarea => gdp%gdimbound%linkMINarea
      volum0L(nmlb:nmub,1:kmax) = volum0(nmlb:nmub,1:kmax)
      volum1L(nmlb:nmub,1:kmax) = volum1(nmlb:nmub,1:kmax)
!
      do nm=1,nmmax          
!          
         if (Nmerged(nm)>1) then
            Atot = 0._fp
            vol0(1:kmax) = 0._fp
            vol1(1:kmax) = 0._fp
            DO N = 1,Nmerged(nm)  
               nmN = NMlistMERGED(N,nm)  
               Atot = Atot +  gsqsR(nmN)     !NOTE: HERE gsqsR ALREADY INCLUDES agsqs
               vol0(1:kmax) = vol0(1:kmax) +  volum0(nmN,1:kmax) 
               vol1(1:kmax) = vol1(1:kmax) +  volum1(nmN,1:kmax) 
            ENDDO
            IF (linkMINarea) then !small cells have minimum Area, the rest goes to the big cell
               AsmallTOT = 0._fp
               DO N = 2,Nmerged(nm)  
                  nmN = NMlistMERGED(N,nm)  
                  Amin = thresMERGE*gsqs(nmN)         
                  AsmallTOT = AsmallTOT +  Amin 
                  gsqsR(nmN) = Amin       
               ENDDO
               Abig = Atot - AsmallTOT
               gsqsR(nm) = Abig 
            else
               Atot = Atot/Nmerged(nm)
               vol0 = vol0/Nmerged(nm)
               vol1 = vol1/Nmerged(nm)
               DO N = 1,Nmerged(nm)  
                  nmN = NMlistMERGED(N,nm)  
                  gsqsR(nmN)  = Atot 
          !        volum0L(nmN,1:kmax) = vol0(1:kmax)
          !        volum1L(nmN,1:kmax) = vol1(1:kmax)
               ENDDO
            endif

         endif
      enddo

       
!
!
RETURN
end subroutine VIRTUALlink_gsqsR
