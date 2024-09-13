subroutine VIRTUALlinkCONC(gsqsR,r1,volum1L,NMlistMERGED,Nmerged,lstsci,icx,icy,nmmax,nmlb,nmub,nst,kmax, dim_nmlist, gdp)
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
!   Function:   compute "spreaded"  values of concentration
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
    integer, pointer :: idebugCUThardINI
    integer, pointer :: idebugCUThardFIN
!
! global variables
!
    integer                                              , intent(in)    :: dim_nmlist
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: gsqsR
    real(fp), dimension(nmlb:nmub, kmax, lstsci)         , intent(inout) :: r1   
    real(fp), dimension(nmlb:nmub, kmax)                 , intent(inout) :: volum1L
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
    integer                                              , intent(in)    :: lstsci
!
!
! local variables
!
    integer                         :: nm  
    integer                         :: nmn
    integer                         :: N
    integer                         :: k
    real(fp)                        :: Aeach
    real(fp)                        :: Atot
    real(fp)                        :: TOTvol1(1:kmax)
    real(fp), dimension( kmax, lstsci)     :: r1_av
!
! executable statements -------------------------------------------------------
! 
    idebugCUThardINI => gdp%gdimbound%idebugCUThardINI
    idebugCUThardFIN => gdp%gdimbound%idebugCUThardFIN
      do nm=1,nmmax          
!          
         if (Nmerged(nm)>1) then
            Atot = 0._fp
         !   Vol1 = 0._fp
            DO N = 1,Nmerged(nm)  
               nmN = NMlistMERGED(N,nm)  
               Atot = Atot + gsqsR(nmN)     !NOTE: HERE gsqsR ALREADY INCLUDES agsqs
               !Vol1 = Vol1 + Volum1L(nm)
            ENDDO
            r1_av(1:kmax,1:lstsci) = 0._fp
            TOTvol1(1:kmax)  = 0._fp
            DO N = 1,Nmerged(nm)  
               nmN = NMlistMERGED(N,nm)  
               TOTvol1(1:kmax) = TOTvol1(1:kmax)  + Volum1L(nmN,1:kmax)
               do k=1,kmax
                  r1_av(k,1:lstsci) = r1_av(k,1:lstsci)  + r1(nmN,k,1:lstsci)*Volum1L(nmN,k) !*Area_spread 
               enddo
            ENDDO
            DO N = 1,Nmerged(nm)  
               nmN = NMlistMERGED(N,nm)   
               DO K=1,kmax
                  r1(nmN,k,1:lstsci)  = r1_av(k,1:lstsci)/ TOTvol1(k)   ! if not linkMINarea I have DIV 3 SINCE Volum1L(nm) are the same
               enddo
            ENDDO
            
         endif
      enddo

      if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
         do k =1,nmmax
           write(9891999,'(2i6,15f21.15)') nst,k,r1(k,1,1) 
         enddo
      endif
!
!
RETURN
end subroutine VIRTUALlinkCONC
