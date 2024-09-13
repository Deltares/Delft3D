subroutine z_defineKGP(GHOSTu1,GHOSTv1,kfumn0,kfvmn0,kfumx0,kfvmx0,kfu,kfv,icx,icy,lunscr,nst,nmmax,nmlb,nmub, gdp)
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
!   Function: Z-layer only. Find the lowermost and uppermost vertical layer for which ghost points are still present (kGPumin,kGPumax) (kGPvmin,kGPvmax)
! 
!   Author: Alberto Canestrelli
!             
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer, dimension(:), pointer :: kGPumin
    integer, dimension(:), pointer :: kGPvmin
    integer, dimension(:), pointer :: kGPumax
    integer, dimension(:), pointer :: kGPvmax
!
! global variables
!
!
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: GHOSTu1 
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: GHOSTv1 
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfumn0
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfvmn0
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfumx0
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfvmx0
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfu
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfv
    integer                                                             , intent(in)    :: lunscr
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmmax
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: icx
    integer                                                             , intent(in)    :: icy
!
! local variables
!
  integer                    :: nm
  integer                    :: nmj
  integer                    :: k   
  integer                    :: nmk(4)
  
!
! executable statements -------------------------------------------------------
!     
    kGPumin => gdp%gdimbound%kGPumin
    kGPvmin => gdp%gdimbound%kGPvmin
    kGPumax => gdp%gdimbound%kGPumax
    kGPvmax => gdp%gdimbound%kGPvmax
!   define 
! 
!      kmin = max(kfsmin(n,m),kfsmax(n,m))
!      do k = kmax, 0, -1
!         kc = min(k + 1, kmax)
!         if (k >= kmin) then
!            zkt(n,m,k) = s1(n,m)
!         elseif (k < kfsmin(n,m)) then
!            zkt(n,m,k) = - real(dps(n,m),fp)
!         else
!            zkt(n,m,k) = zkt(n,m,k+1) - dzs1(n,m,kc)
!         endif
!      enddo
!
!   Define kGPmin (flag for minimum ghost point needed for adjacent cells
!
       DO nm = 1,nmmax
          !u point
          if (GHOSTu1(nm)==1) THEN  
             kGPumin(nm) = 9999999
             nmk(k) = nm + icx
             nmk(k) = nm - icx
             nmk(k) = nm + icy
             nmk(k) = nm - icy
             do k=1,4
                nmj = nmk(k)
                if (GHOSTu1(nmj)==0) THEN !
                   kGPumin(nm) = min(kGPumin(nm),kfumn0(nmj))
                endif    
             enddo  
          endif
          !v point
          if (GHOSTv1(nm)==1) THEN  
             kGPvmin(nm) = 9999999
             nmk(k) = nm + icx
             nmk(k) = nm - icx
             nmk(k) = nm + icy
             nmk(k) = nm - icy
             do k=1,4
                nmj = nmk(k)
                if (GHOSTv1(nmj)==0) THEN !
                   kGPvmin(nm) = min(kGPvmin(nm),kfvmn0(nmj))
                endif    
             enddo  
          endif
       ENDDO
!
!   Define kGPmax (flag for maximum ghost point needed for adjacent cells)
!
       DO nm = 1,nmmax
          !u point
          if (kfu(nm)==0) then
             kGPumax(nm) = -1
             nmk(k) = nm + icx
             nmk(k) = nm - icx
             nmk(k) = nm + icy
             nmk(k) = nm - icy
             do k=1,4
                nmj = nmk(k)
                if (GHOSTu1(nmj)==0) THEN !
                   kGPumax(nm) = max(kGPumax(nm),kfumx0(nmj))
                endif    
             enddo  
          endif
          !v point
          if (kfv(nm)==0) then
             kGPvmax(nm) = -1
             nmk(k) = nm + icx
             nmk(k) = nm - icx
             nmk(k) = nm + icy
             nmk(k) = nm - icy
             do k=1,4
                nmj = nmk(k)
                if (GHOSTv1(nmj)==0) THEN !
                   kGPvmax(nm) = max(kGPvmax(nm),kfvmx0(nmj))
                endif    
             enddo  
          endif
       ENDDO
RETURN
END
