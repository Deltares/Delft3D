subroutine InterpGhost_s_u_v(gsqs,kfs,kcs,s1,u1,v1,dps,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub, gdp)
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
!   Function:   Interpolate the values at the Image points
!
!   NOTE: Vandermonde matrices are notoriously ill-conditioned. The inverses of large floating-point
! Vandermonde matrices are subject to severe round-off effects.fROM http://www.mathworks.com/help/symbolic/mupad_ref/linalg-invvandermonde.html
! 
!               
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                 , pointer :: totGHOSTs1
    integer, dimension(:,:) , pointer :: kfs_cc
    real(fp), dimension(:,:), pointer :: dpL
    real(fp), dimension(:,:), pointer :: PSIx
    real(fp), dimension(:,:), pointer :: PSIy
    real(fp), dimension(:,:), pointer :: ETAx
    real(fp), dimension(:,:), pointer :: ETAy
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: s1 
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: v1
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: gsqs
    real(prec), dimension(nlb:nub,mlb:mub)                              , intent(inout) :: dps
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kfs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs 
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lunscr   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: irov   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
!
!
! local variables
!
  integer                    :: i
  integer                    :: m
  integer                    :: n
  integer                    :: mGP
  integer                    :: nGP
  integer                    :: k
  integer                    :: kk
  integer                    :: kkk
  integer                    :: kfl
  integer                    :: Kdry
  integer                    :: k1
  integer                    :: k2
  integer                    :: k3
  integer                    :: k4
  integer                    :: mBI
  integer                    :: nBI
  integer                    :: mBI1
  integer                    :: nBI1
  integer                    :: mBI2
  integer                    :: nBI2
  integer                    :: iG1
  integer                    :: iG2
  integer                    :: nL(4)
  integer                    :: mL(4)
  integer                    :: INFO
  integer                    :: contFLUID
  integer                    :: contGHOST
  integer                    :: contDRYnoGH   
  integer                    :: nDRY   
  integer                    :: mDRY  
  integer                    :: signINT
  real(fp)                   :: kOK(4)
  real(fp)                   :: kGHOS(4)
  real(fp)                   :: kDRYnoGH(4)
  real(fp)                   :: nxDRY1
  real(fp)                   :: nyDRY1
  real(fp)                   :: nxDRY2
  real(fp)                   :: nyDRY2
  real(fp)                   :: signREAL
  real(fp)                   :: xBI_DRY1
  real(fp)                   :: yBI_DRY1
  real(fp)                   :: xBI_DRY2
  real(fp)                   :: yBI_DRY2
  real(fp)                   :: halfPSI
  real(fp)                   :: halfETA
!
  real(fp)                   :: butta
  real(fp)                   :: A(4,4)
  real(fp)                   :: A3(3,3)
  real(fp)                   :: B(4)
  real(fp)                   :: B3(3)
!
! executable statements -------------------------------------------------------
!  
    totGHOSTs1 => gdp%gdimbound%totGHOSTs1
    kfs_cc     => gdp%gdimbound%kfs_cc
    dpL        => gdp%gdimbound%dpL
    PSIx       => gdp%gdimbound%PSIx
    PSIy       => gdp%gdimbound%PSIy
    ETAx       => gdp%gdimbound%ETAx
    ETAy       => gdp%gdimbound%ETAy
!   
!    DONEinterpS1(1:totGHOSTs1) =.FALSE. !for image point with common interpolation stencil (contFLUID==2)
!
!   FARE un test con una bank che parte a met`a cella su tutta l ' orizzontale (cio`e tutte ghost cell adiacenti). Vedere se l' interpolazione con contFLUID==2 visto che defgenera in 4 punti a 2 a 2 coincidenti funziona
!
!   interpolate depth and water surface at s1 ghost point
!
    CALL interpG_ATs1LOCATION(s1,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub, gdp)
    CALL interpG_ATs1LOCATION(dps,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub, gdp) !note only values af dps at fluid surface points are used, and that are already set = to dpL in checkDRY.f90
!
!   interpolate u1 at u1 ghost point
!
    do k=1,kmax
      CALL interpG_ATu1LOCATION(u1(nlb,mlb,k),kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub,1._fp, gdp)
    enddo
!
!   interpolate v1 and dpU at v1 ghost point
!
    do k=1,kmax
      CALL interpG_ATv1LOCATION(v1(nlb,mlb,k),kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub,1._fp, gdp)
    enddo
!

!
    do m=1,mmax
       do n=1,nmaxus
        !  write(981234,'(5i7,4f25.15)') m,n,kfs_cc(n,m),1,1,dps(n,m),s1(n,m),u1(n,m,1),v1(n,m,1)
       enddo
    enddo
!
RETURN
END
!
!
!             else
!                !first (and only) boundary dry cell not ghost
!                k1 = kDRYnoGH(1)   
!                nDRY = nL(K1)
!                mDRY = mL(K1) 
!                do kfl=1,2 !cycle of the fluid cell
!                   kkk = kOK(kfl)
!                   if (nDRY==nL(kkk)) then !lay on the same vertical => normal is vertical
!                      signREAL = real(sign(1,mL(kkk)-mDRY),fp)
!                      nxDRY1 = signREAL*PSIx(2,2) !Normal that points toward the fluid cell
!                      nyDRY1 = signREAL*PSIy(2,2) !Normal that points toward the fluid cell
!                   else !if(mDRY==mL(kkk)) then !lays on the same horizontal => normal is horizontal   
!                      signREAL = real(sign(1,nL(kkk)-nDRY),fp)                 
!                      nxDRY1 = signREAL*ETAx(2,2) !Normal that points toward the fluid cell
!                      nyDRY1 = signREAL*ETAy(2,2) !Normal that points toward the fluid cell
!                   endif    
!                 enddo  
!             endif
