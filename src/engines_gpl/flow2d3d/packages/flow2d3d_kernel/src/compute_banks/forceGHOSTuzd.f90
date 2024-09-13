subroutine forceGHOSTuzd(icx        ,icy        ,u          ,&
                       & aak        ,bbk        ,cck        ,ddk        ,&
                       & buux       ,bux        ,bdx        ,&
                       & bddx       ,buuy       ,buy        ,bdy        ,bddy       ,&
                       & mmax       ,nmax       ,kmax       ,&
                       & nst        ,nlb        ,nub        ,mlb        ,mub        ,&
                       & nmlb       ,nmub       ,iter       ,zmodel     ,gdp)
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
!   Function: Call subroutine that are needed for cutcells before sud computation (stage 2)
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
    integer              , pointer :: totGHOSTu1
    integer              , pointer :: totGHOSTv1
    integer, dimension(:), pointer :: mGPv1
    integer, dimension(:), pointer :: nGPv1
    integer, dimension(:), pointer :: mGPu1
    integer, dimension(:), pointer :: nGPu1
!
! global variables 
!
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout) :: u
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: aak     !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: bbk     !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: cck     !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: ddk     !!  Internal work array, diagonal space at (N,M,K) 
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: buux    
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: bux        
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: bdx        
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: bddx       
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: buuy       
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: buy        
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: bdy        
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: bddy       
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: icx
    integer                                                             , intent(in)    :: icy
    integer                                                             , intent(in)    :: mmax
    integer                                                             , intent(in)    :: nmax
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: kmax
    integer                                                             , intent(in)    :: iter
    integer                                                             , intent(in)    :: nst
    logical                                                             , intent(in)    :: zmodel
!
! local variables
!
   integer :: i
   integer :: k
   integer :: mGP
   integer :: nGP
   integer :: nm
   integer :: nmaxOK
!
! executable statements -------------------------------------------------------
!   
    totGHOSTu1 => gdp%gdimbound%totGHOSTu1
    totGHOSTv1 => gdp%gdimbound%totGHOSTv1
    mGPv1      => gdp%gdimbound%mGPv1
    nGPv1      => gdp%gdimbound%nGPv1
    mGPu1      => gdp%gdimbound%mGPu1
    nGPu1      => gdp%gdimbound%nGPu1
      nmaxOK = nmax  !note in this subroutine nmax is always nmax and never mmax!
      if (icx.eq.1) then ! along v     
         do i = 1,totGHOSTv1   
            mGP = mGPv1(i)
            nGP = nGPv1(i) 
            nm = (mGP-1)*nmaxOK+nGP
            aak (nm, 1:kmax) = 0.0
            bbk (nm, 1:kmax) = 1.0 !/hdt 
            bux (nm, 1:kmax) = 0.0
            bdx (nm, 1:kmax) = 0.0
            buy (nm, 1:kmax) = 0.0
            bdy (nm, 1:kmax) = 0.0
            cck (nm, 1:kmax) = 0.0
            ddk (nm, 1:kmax) = u(nm, 1:kmax) !/hdt !note: I interpolated u0 not u1, then u1 becomes u0 before iterative loop
            if (.not.zmodel) then
               bddx(nm, 1:kmax) = 0.0
               bddy(nm, 1:kmax) = 0.0
               buux(nm, 1:kmax) = 0.0
               buuy(nm, 1:kmax) = 0.0
            endif
              !  write(3333300,'(6i6,15f21.15)') nst,nm,mGP,nGP,i,k,u0(nm, k)
         enddo
      else
         do i = 1,totGHOSTu1   
            mGP = mGPu1(i)
            nGP = nGPu1(i) 
            nm = (mGP-1)*nmaxOK+nGP
            aak (nm, 1:kmax) = 0.0
            bbk (nm, 1:kmax) = 1.0 !/hdt 
            bux (nm, 1:kmax) = 0.0
            bdx (nm, 1:kmax) = 0.0
            buy (nm, 1:kmax) = 0.0
            bdy (nm, 1:kmax) = 0.0
            cck (nm, 1:kmax) = 0.0
            ddk (nm, 1:kmax) = u(nm, 1:kmax) !/hdt !note: I interpolated u0 not u1, then u1 becomes u0 before iterative loop
            if (.not.zmodel) then
               bddx(nm, 1:kmax) = 0.0
               bddy(nm, 1:kmax) = 0.0
               buux(nm, 1:kmax) = 0.0
               buuy(nm, 1:kmax) = 0.0
            endif
             !   write(3333300,'(6i6,15f21.15)') nst,nm,mGP,nGP,i,k,u0(nm, k)
         enddo
      endif
!
   RETURN 
end subroutine forceGHOSTuzd
