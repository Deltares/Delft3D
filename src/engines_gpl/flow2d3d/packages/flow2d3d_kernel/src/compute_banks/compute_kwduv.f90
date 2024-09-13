subroutine compute_kwduv(icx,icy,kWDu,ghostu1,totGHOSTu1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddb,gdp)
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
!   Function:  compute kwdu and kwdv for masking of ghost points in the floodplains in wetting and drying situations                         
!
!
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
    integer, dimension(:), pointer :: mGPu1
    integer, dimension(:), pointer :: nGPu1
    integer, dimension(:), pointer :: mGPv1
    integer, dimension(:), pointer :: nGPv1
!
! global variables
!
    integer, dimension(nmlb:nmub,4)                                    , intent(out)   :: kWDu
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: ghostu1
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfs_cc
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lunscr   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: icx
    integer                                                             , intent(in)    :: icy
    integer                                                             , intent(in)    :: totGHOSTu1
!
!
! local variables
!
  integer                    :: nm
  integer                    :: numu 
  integer                    :: ndmu
  integer                    :: I
  integer                    :: k
  integer                    :: icxy
  integer                    :: icxOK
  integer                    :: icyOK
  integer                    :: kOK(4)
  integer                    :: kGHOS(4)
  integer                    :: kDRYnoGH(4)
  integer                    :: kNNG(4)
  integer                    :: nmK(4)
  integer                    :: kk
  integer                    :: m
  integer                    :: n
  Integer                    :: mGP
  integer                    :: nGP
  integer                    :: ndm
  integer                    :: num
  integer                    :: nmd
  integer                    :: nmu
  integer                    :: ddb
  integer                    :: nL(4)
  integer                    :: mL(4)
  integer                    :: contFLUID
  integer                    :: contGHOST
  integer                    :: contDRYnoGH  
  integer                    :: contNNghostWD 
  real(fp)                   :: velK(1:kmax,4)
  real(fp)                   :: vel(1:kmax)
!
! executable statements -------------------------------------------------------
!  
    mGPu1 => gdp%gdimbound%mGPu1
    nGPu1 => gdp%gdimbound%nGPu1
    mGPv1 => gdp%gdimbound%mGPv1
    nGPv1 => gdp%gdimbound%nGPv1
   icxy   = max(icx, icy)    
!
   do i = 1,totGHOSTu1       
!       
      if (icy==1) then  
         icxOK = icx
         icyOK = icy
         mGP = mGPu1(i)
         nGP = nGPu1(i)
      else
         icxOK = icy
         icyOK = icx
         mGP = mGPv1(i)
         nGP = nGPv1(i)
      endif
      nm = (nGP   + ddb)  *icyOK + (mGP   + ddb)  *icxOK - icxy 
!
      !right cell 
      nmu  = nm+icx 
      !upper right
      numu = nmu + icy
      !lower right
      ndmu = nmu - icy
      !left cell 
      ! nm
      !upper left
      num = nm + icy
      !lower left
      ndm = nm - icy
!
!     define masking for along m terms (advection and shear)
!
      !negative m
      if (kfs_cc(nm).eq.0.or.kfs_cc(nm).eq.3) then
         kWDu(nm,1) = 1
      else
         kWDu(nm,1) = 0
      endif
      !positive m
      if (kfs_cc(nmu).eq.0.or.kfs_cc(nmu).eq.3) then
         kWDu(nm,2) = 1
      else
         kWDu(nm,2) = 0
      endif
!
!     define masking for along n terms (advection and shear)
!    
!     negative n      
      if ( (kfs_cc(ndm).eq.0.or.kfs_cc(ndm).eq.1.or.kfs_cc(ndm).eq.3) .and. (kfs_cc(ndmu).eq.0.or.kfs_cc(ndmu).eq.1.or.kfs_cc(ndmu).eq.3) ) then
         kWDu(nm,3) = 1
      else
         kWDu(nm,3) = 0 !0 also if .not.(kfs_cc(ndm).gt.0.and.kfs_cc(ndmu).gt.0) 
      endif
!
!     positive n      
      if ( (kfs_cc(num).eq.0.or.kfs_cc(num).eq.1.or.kfs_cc(num).eq.3) .and. (kfs_cc(numu).eq.0.or.kfs_cc(numu).eq.1.or.kfs_cc(numu).eq.3) ) then
         kWDu(nm,4) = 1
      else
         kWDu(nm,4) = 0 !0 also if .not.(kfs_cc(num).gt.0.and.kfs_cc(numu).gt.0) 
      endif    
!
   enddo
   ! REMOVE IT FOR NOW
    kWDu(:,:) = 1
!
RETURN
END
!
! 
