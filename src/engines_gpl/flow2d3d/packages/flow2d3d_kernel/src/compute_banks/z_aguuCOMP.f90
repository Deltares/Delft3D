subroutine z_aguuCOMP(EDGElenWET,zk,kfu,z_aguu,aguu,dpL,dpH,icx,icy,lunscr,nst,nmmax,nmlb,nmub,kmax)
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
!  Ndryact: delft3d.support@deltares.nl                                         
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
!   Function: Z-layer only. Compute z_aguu, k-dependent active edge for z-layer computation
! 
!   Author: Alberto Canestrelli
!             
!!--declarations----------------------------------------------------------------
!
  use precision
!
  implicit none
!
! global variables
!
    real(fp), dimension(nmlb:nmub,4)                                    , intent(in)    :: EDGElenWET
    real(fp), dimension(0:kmax)                                         , intent(in)    :: zk
    real(fp), dimension(nmlb:nmub,1:kmax)                               , intent(out)   :: z_aguu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: aguu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: dpL
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: dpH
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfu
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmmax
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: icx
    integer                                                             , intent(in)    :: icy
    integer                                                             , intent(in)    :: kmax
    integer                                                             , intent(in)    :: lunscr
!
! local variables
!
  integer                    :: nmu
  integer                    :: nmH  
  integer                    :: nmL 
  integer                    :: nm
  integer                    :: k   
  integer                    :: Lad
  integer                    :: L 
  integer                    :: kH
  integer                    :: kL
  real(fp)                   :: dpuLOC
  real(fp)                   :: zkm
  real(fp)                   :: dpHH
  real(fp)                   :: dpHL
  real(fp)                   :: aguuH  
  real(fp)                   :: aguuL

  
!
! executable statements -------------------------------------------------------
!     
   if (icy==1) then !along x
      L = 2
      Lad = 4
   else !along y
      L = 3
      Lad = 1
   endif 
   do nm=1,nmmax      
      if (kfu(nm) ==0) then
         z_aguu(1:nmmax,1:kmax) = 0
      else
         if (comparereal(aguu(nm),0._fp).gt.0._fp) then
            nmu = nm + icx
            !dpuLOC not needed i think
            dpuLOC = min(dpL(nm),dpL(nmu)) ! (i.e. max elevation) !maybe I can use dpu(nm) if it is always the minimum
            IF (dpH(nm)>dpH(nmu)) then
               dpHH = dpH(nm)
               dpHL = dpH(nmu)
               nmH = nm
               nmL = nmu
               kH = L
               kL = Lad
               aguuH = EDGElenWET(nm,kH)
               aguuL = EDGElenWET(nm,kL)
            ELSE
               dpHH = dpH(nmu)
               dpHL = dpH(nm)
               nmH = nmu
               nmL = nm
               kH = Lad
               kL = L
               aguuL = EDGElenWET(nm,kH)
               aguuH = EDGElenWET(nm,kL)
            ENDIF
            do k=1,kmax !kfumin(nm),kfumax(nm)
               if (zk(k).lt.dpHL) then
                  !if (zk(k).gt.dpuLOC) then not needed I think
                  z_aguu(nm,k) = aguu(nm)
               elseif (zk(k).gt.dpHL .and. zk(k).lt.dpHH)then !double check if its k
                  zkm = zk(k-1)
                  if (zkm.gt.dpHL) then
                     z_aguu(nm,k) =  aguuH
                  else
                     z_aguu(nm,k) = ((zk(k)-dpHL)*aguuH + (dpHL-zkm)*aguuL)/(zk(k)-zkm) !maybe replace (zk(k)-zkm) with fixed thickness if we have it
                  endif
               elseif (zk(k).gt.dpHH) then
                  zkm = zk(k-1)
                  if (zkm.gt.dpHH)then  
                     z_aguu(nm,k) = 1._fp
                  elseif(zkm.lt.dpHH.and.zkm.gt.dpHL)then  
                     z_aguu(nm,k) = ((zk(k)-dpHH)*aguu(nm) + (dpHH-dpHL)*aguuH)/(zk(k)-zkm)
                  else ! if(zkm.lt.dpHL)then  
                     z_aguu(nm,k) = ((zk(k)-dpHH)*aguu(nm) + (dpHH-dpHL)*aguuH + (dpHL-zkm)*aguuL)/(zk(k)-zkm)
                  endif
               endif
            enddo    
         endif
      endif
!
   enddo

   RETURN
end subroutine z_aguuCOMP
