   subroutine kfuvGHOST3(nst,kcs,kfu,kfv,umean,vmean,hu,hv,u1,v1,u0,v0,qxk,qyk,thick,mmax,nmax,kmax,nmaxus,nlb,nub,mlb,mub,nmlb,nmub,Irov, gdp)
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
!   Function: set  GHOSTu1=3 as inactive 
!
!   Author: Alberto Canestrelli
!
!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                   , pointer :: cutcell
    integer, dimension(:,:)   , pointer :: GHOSTu1
    integer, dimension(:,:)   , pointer :: GHOSTv1
    real(fp), dimension(:,:)  , pointer :: aguu
    real(fp), dimension(:,:)  , pointer :: agvv
    real(fp), dimension(:,:,:), pointer :: qxk_tinyCUT
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: umean
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: vmean
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: v1
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: u0
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: v0 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inOUT) :: kfu 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inOUT) :: kfv
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: hv 
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(in)    :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(in)    :: qyk   
    real(fp), dimension(kmax)                                           , intent(in)    :: thick 
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: Irov
    integer                                                             , intent(in)    :: nst
!
!
! local variables
!
    integer                    :: I,mGP,nGP,k,m,n,nad,mad
    logical                    :: OUTbound
    real(fp)                   :: Lpart,Lad,Ltot,Uvertex(1:kmax),Vvertex(1:kmax),aguuOK,agvvOK

!
! executable statements -------------------------------------------------------
!
    cutcell     => gdp%gdimbound%cutcell
    GHOSTu1     => gdp%gdimbound%GHOSTu1
    GHOSTv1     => gdp%gdimbound%GHOSTv1
    aguu        => gdp%gdimbound%aguu
    agvv        => gdp%gdimbound%agvv
    qxk_tinyCUT => gdp%gdimbound%qxk_tinyCUT
!  CONTROLLARE IF DOUBLE VALUED FLUID-GHOST (IN THAT CASE hu hv might have changed but should be fine)
!
!  define qxk_tinyCUT at the befinning =0 for non cutcell method
!

      do m=1,mmax           
         do n=1,nmaxus
            if (GHOSTu1(n,m).eq.3) then !it is at a wet/dry interface
            !   if ((comparereal(aguu(n,m),0.5_fp).lt.0).and.(comparereal(aguu(n,m),0._fp).ge.0)) then            
                  kfu(n,m) = 0
                  u1(n,m,1:kmax) = 0._fp
                  u0(n,m,1:kmax) = 0._fp     
            !   endif
            endif
            if (GHOSTv1(n,m).eq.3) then !it is at a wet/dry interface
              ! if ((comparereal(agvv(n,m),0.5_fp).lt.0).and.(comparereal(agvv(n,m),0._fp).ge.0)) then            
                  kfv(n,m) = 0
                  v1(n,m,1:kmax) = 0._fp  
                  v0(n,m,1:kmax) = 0._fp  
              ! endif
            endif
         enddo
      enddo 


   RETURN
end subroutine kfuvGHOST3
