subroutine interpG_ATv1LOCATION(VAR,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,IsignBC,nlb,nub,mlb,mub,nmlb,nmub,MAXagvv, gdp)
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
!   Function:   Interpolate the values at the Image points. IsignBC=1 enforces a free-slip. IsignBC=-1
!               let Irov to decide
!
!   NOTE: Vandermonde matrices are notoriously ill-conditioned. The inverses of large floating-point
! Vandermonde matrices are subject to severe round-off effects.fROM http://www.mathworks.com/help/symbolic/mupad_ref/linalg-invvandermonde.html
!
!   Author: Alberto Canestrelli 
!               
!!--declarations----------------------------------------------------------------
!
    use globaldata
    use mathconsts, only: pi
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp)                , pointer :: DISSghost
    integer                 , pointer :: totGHOSTv1
    integer                 , pointer :: typeEXTRAPstencil
    integer, dimension(:,:) , pointer :: FROMmnTOghostV1
    integer, dimension(:)   , pointer :: nGPv1
    integer, dimension(:)   , pointer :: mGPv1
    integer, dimension(:)   , pointer :: mIPv1
    integer, dimension(:)   , pointer :: nIPv1
    integer, dimension(:)   , pointer :: mBIv1
    integer, dimension(:)   , pointer :: nBIv1
    integer, dimension(:,:) , pointer :: GHOSTv1
    real(fp), dimension(:,:), pointer :: PSIx
    real(fp), dimension(:,:), pointer :: PSIy
    real(fp), dimension(:,:), pointer :: ETAx
    real(fp), dimension(:,:), pointer :: ETAy
    real(fp), dimension(:,:), pointer :: aguu
    real(fp), dimension(:,:), pointer :: agvv
    real(fp), dimension(:,:), pointer :: xcorU1
    real(fp), dimension(:,:), pointer :: ycorU1
    real(fp), dimension(:,:), pointer :: xcorV1
    real(fp), dimension(:,:), pointer :: xG_V1
    real(fp), dimension(:,:), pointer :: xG_U1
    real(fp), dimension(:)  , pointer :: xIPv1
    real(fp), dimension(:)  , pointer :: yIPv1
    real(fp), dimension(:)  , pointer :: xBIv1
    real(fp), dimension(:)  , pointer :: yBIv1
    real(fp), dimension(:)  , pointer :: v1IP
    real(fp), dimension(:)  , pointer :: nxG_V1
    real(fp), dimension(:)  , pointer :: nyG_V1
    real(fp), dimension(:,:), pointer :: shiftBIv_x
    real(fp), dimension(:,:), pointer :: shiftBIv_y
    logical                 , pointer :: periodSURFACE
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: VAR
    real(fp)                                                            , intent(in)    :: MAXagvv !max value of aguu for which I prescribe ghost cells
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lunscr   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: irov   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: IsignBC   !  Description and declaration in iidim.f90
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
  integer                    :: j
  integer                    :: mu
  integer                    :: nd
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
  integer                    :: contNNghostWD
  integer                    :: nDRY   
  integer                    :: mDRY  
  integer                    :: signINT
  integer                    :: IrovLOC
!
  integer                    :: kOK(4)
  integer                    :: kGHOS(4)
  integer                    :: kDRYnoGH(4)
  integer                    :: kNNG(4)
  integer                    :: iwork(4)
  integer                    :: IPIV(4) 
!
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
  real(fp)                   :: RsignBC
  real(fp)                   :: angleADJ
  real(fp)                   :: dx
  real(fp)                   :: dy
  real(fp)                   :: x1
  real(fp)                   :: y1
  real(fp)                   :: rcond
  real(fp)                   :: colsum
  real(fp)                   :: anorm
!
  logical                    :: REDO
  logical                    :: DRYUadj
  logical                    :: setTOzero
!
  real(fp)                   :: butta
  real(fp)                   :: A(4,4)
  real(fp)                   :: A3(3,3)
  real(fp)                   :: B(4)
  real(fp)                   :: B3(3)
  real(fp)                   :: work(16)
!
  character, dimension(1)    :: norm
!
! executable statements -------------------------------------------------------
!  
    DISSghost         => gdp%gdimbound%DISSghost
    totGHOSTv1        => gdp%gdimbound%totGHOSTv1
    typeEXTRAPstencil => gdp%gdimbound%typeEXTRAPstencil
    FROMmnTOghostV1   => gdp%gdimbound%FROMmnTOghostV1
    nGPv1             => gdp%gdimbound%nGPv1
    mGPv1             => gdp%gdimbound%mGPv1
    mIPv1             => gdp%gdimbound%mIPv1
    nIPv1             => gdp%gdimbound%nIPv1
    mBIv1             => gdp%gdimbound%mBIv1
    nBIv1             => gdp%gdimbound%nBIv1
    GHOSTv1           => gdp%gdimbound%GHOSTv1
    PSIx              => gdp%gdimbound%PSIx
    PSIy              => gdp%gdimbound%PSIy
    ETAx              => gdp%gdimbound%ETAx
    ETAy              => gdp%gdimbound%ETAy
    aguu              => gdp%gdimbound%aguu
    agvv              => gdp%gdimbound%agvv
    xcorU1            => gdp%gdimbound%xcorU1
    ycorU1            => gdp%gdimbound%ycorU1
    xcorV1            => gdp%gdimbound%xcorV1
    xG_V1             => gdp%gdimbound%xG_V1
    xG_U1             => gdp%gdimbound%xG_U1
    xIPv1             => gdp%gdimbound%xIPv1
    yIPv1             => gdp%gdimbound%yIPv1
    xBIv1             => gdp%gdimbound%xBIv1
    yBIv1             => gdp%gdimbound%yBIv1
    v1IP              => gdp%gdimbound%v1IP
    nxG_V1            => gdp%gdimbound%nxG_V1
    nyG_V1            => gdp%gdimbound%nyG_V1
    shiftBIv_x        => gdp%gdimbound%shiftBIv_x
    shiftBIv_y        => gdp%gdimbound%shiftBIv_y
    periodSURFACE     => gdp%gdimbound%periodSURFACE
!
!  extrapolate at the boundary, in order to have some sort of tranmissive behaviour
!
    if (typeEXTRAPstencil.ge.1.and..not.periodSURFACE) then ! if periodic tangential velocity is already extrapolated
       do m = 1, mmax - 1
          mu = m+1
          do n = 1, nmaxus
             nd = n - 1
             !if (cstbnd.and.m.lt.mmax) then  
   !         extrapolate horizontally 
             if (kcs(n, m) == 2 .and. kcs(n, mu) == 1 ) then  
                if (ghostv1(n , m)/=0) var(n, m)  = var(n, mu) !ghostv1 needed for staircase boundary
                if (ghostv1(nd, m)/=0) var(nd, m) = var(nd, mu) !ghostv1 needed for staircase boundary
             elseif (kcs(n, m) == 1 .and. kcs(n, mu) == 2) then 
                if (ghostv1(n , mu)/=0) var(n, mu)  = var(n, m) !ghostv1 needed for staircase boundary
                if (ghostv1(nd, mu)/=0) var(nd, mu) = var(nd, m)!ghostv1 needed for staircase boundary
             endif 
          enddo
       enddo
    endif
!
    RsignBC = REAL(IsignBC,fp)
    IrovLOC = Irov
    IF (IrovLOC==3) IrovLOC=0 ! IT SHOULD NEVER HAPPEN
    SELECT CASE(IsignBC)
    CASE(1)
       IrovLOC = 0 ! I prescribe depth at U and V point as if it was a free slip
     CASE(-1)
       SELECT CASE(Irov)
       CASE(0,3) !FREE SLIP,free slip+HLES
         RsignBC = 1._fp
       CASE(2) !no SLIP
         RsignBC = -1._fp
       CASE default !partial SLIP or new options
         WRITE(*,*) 'Irov value not compatible with cut cells'
         call d3stop(1, gdp)
       END SELECT
    CASE DEFAULT
       WRITE(*,*) 'Wrong RsignBC value' 
       call d3stop(1, gdp)
    END SELECT
!
    do i = 1,totGHOSTv1       
!        
       m = mIPv1(i)
       n = nIPv1(i)
       mGP = mGPv1(i)
       nGP = nGPv1(i)
       if  (comparereal(agvv(nGP,mGP),MAXagvv).gt.0) cycle
       mBI = mBIv1(i)
       nBI = nBIv1(i)
       setTOzero = .FALSE.
!
!      if n equal to 1 do an extrapolation from the stencil corresponding to n=2, otherwise 2 nodes of the stencil are outside the domain
!
       if (n.eq.1) then
          n = n + 1
       elseif (n.eq.nmaxus) then
          n = n - 1
       endif
       
!
       contFLUID = 0
       contGHOST = 0
       contDRYnoGH = 0
       contNNghostWD = 0
!
         ! lower left  
         nL(1)=n-1
         mL(1)=m-1 
         ! lower right
         nL(2)=n-1
         mL(2)=m
         ! upper right
         nL(3)=n
         mL(3)=m  
         ! upper left 
         nL(4)=n 
         mL(4)=m-1
!
         Do K=1,4
            IF (GHOSTv1(nL(K),mL(K)+1) == 0) then !fluid cell ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
               contFLUID = contFLUID + 1
               kOK(contFLUID) = K
            ELSEIF (GHOSTv1(nL(K),mL(K)+1) == 1) then ! dry ghost cell ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
               contGHOST = contGHOST + 1
               kGHOS(contGHOST) = K
            ELSEIF (GHOSTv1(nL(K),mL(K)+1) == 2) then  ! dry NOT ghost  
               contDRYnoGH = contDRYnoGH + 1
               kDRYnoGH(contDRYnoGH) = K
            ELSEIF (GHOSTv1(nL(K),mL(K)+1) >=3) then  !bank/wet channel interface or orthogonal to it
               contNNghostWD = contNNghostWD + 1
               kNNG(contNNghostWD) = K
            endif
         ENDDO   
         !write(80808080,'(12i11)') nst,m,n,mgp,ngp,mbi,nbi,contFLUID,contGHOST,contDRYnoGH,contNNghostWD
       !
       ! note: Bilinear interpolation for a rectangle has a simple form (Nasr-Azadani, E. Meiburg 2009)
       ! to espress the inverse so  I think it should not be solved the linear system. However, 
       ! if the rectangle is not with edges along x and y teh form is not that simple so I prefer to always
       ! solve the linear system. Moreover the form is not simple when I use the boundary condition itself for
       ! the interpolation, even for dirichlet BC (its not a rectangle anymore)
       !
       !write(3434343,'(5i6)')  nst,n,m,contFLUID,contGHOST
       IF (contFLUID==4) then      
!
!         simplest case: all the four corner nodes of the interpolation cell are on the fluid
!         
          A(1,1) = xcorU1(nL(1),mL(1))*ycorU1(nL(1),mL(1)) 
          A(2,1) = xcorU1(nL(2),mL(2))*ycorU1(nL(2),mL(2))
          A(3,1) = xcorU1(nL(3),mL(3))*ycorU1(nL(3),mL(3))
          A(4,1) = xcorU1(nL(4),mL(4))*ycorU1(nL(4),mL(4))
          A(1,2) = xcorU1(nL(1),mL(1))  
          A(2,2) = xcorU1(nL(2),mL(2)) 
          A(3,2) = xcorU1(nL(3),mL(3)) 
          A(4,2) = xcorU1(nL(4),mL(4)) 
          A(1,3) = ycorU1(nL(1),mL(1)) 
          A(2,3) = ycorU1(nL(2),mL(2))
          A(3,3) = ycorU1(nL(3),mL(3))
          A(4,3) = ycorU1(nL(4),mL(4))
          A(1,4) = 1._fp
          A(2,4) = 1._fp
          A(3,4) = 1._fp
          A(4,4) = 1._fp
          B(1) = var(nL(1),mL(1)+1)   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
          B(2) = var(nL(2),mL(2)+1)   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
          B(3) = var(nL(3),mL(3)+1)   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
          B(4) = var(nL(4),mL(4)+1)   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
!
         !No pivoting means no row interchanges.  It can be done only if Gaussian elimination never run into zeros on the diagonal. 
         !see here for how to prescribe IPIV for a good pivot http://math.fullerton.edu/mathews/n2003/PivotingMod.html
          CALL DGETRF( contFLUID, contFLUID, A, contFLUID, IPIV, INFO ) !compute LU
          CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
          CALL DGETRS('N', contFLUID, 1, A, contFLUID, IPIV, B, contFLUID, INFO ) !solve linear system
          CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)   
          v1IP(i) = B(1)*xIPv1(i)*yIPv1(i)+B(2)*xIPv1(i)+B(3)*yIPv1(i)+B(4)
          if (IrovLOC==2) then  ! no-slip
             var(nGP,mGP) = RsignBC * v1IP(i) ! DIRICHLET condition for velocity
          ELSEIF(IrovLOC==0) then ! free slip
             var(nGP,mGP) = RsignBC * v1IP(i) ! DIRICHLET condition for velocity           
          ELSEIF(IrovLOC==1) then ! partial slip
            WRITE(*,*) 'Partial slip not compatible with cut cells'
            call d3stop(1, gdp)
          ENDIF 
          if (V1IP(i).lt.-1000.or.V1IP(i).gt.1000) then
            write(*,*) ' wrong value of v1ip in n,m,nst,irov', n,m,nst,irov
            call d3stop(1, gdp)
          endif  
       ELSEIF (contFLUID==3) then      
!         three of the  four corner nodes of the interpolation cell are on the fluid
!         
          k1 = kOK(1)  
          k2 = kOK(2) 
          k4 = kOK(3)  !! I put the BC on the third row so the diagonal term is not going to be zero (for zero-velocity prescribed) 
         
          if (contGHOST.eq.1) then
            !note its not necessarely the same ghost cell i!
             k3 = kGHOS(1)
             iG1  = FROMmnTOghostV1(nL(k3),mL(k3)+1)   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
             mBI1 = mBIv1(iG1) !not needed for no slip
             nBI1 = nBIv1(iG1) !not needed for no slip
             nxDRY1 = nxG_V1(iG1) 
             nyDRY1 = nyG_V1(iG1) 
             xBI_DRY1 = xBIv1(iG1) + shiftBIv_x(nL(k3),mL(k3)+1) 
             yBI_DRY1 = yBIv1(iG1) + shiftBIv_y(nL(k3),mL(k3)+1) 
!
             if (IrovLOC==2) then  ! no-slip
                A(1,1) = xcorU1(nL(k1),mL(k1))*ycorU1(nL(k1),mL(k1)) 
                A(2,1) = xcorU1(nL(k2),mL(k2))*ycorU1(nL(k2),mL(k2))
                A(3,1) = xBI_DRY1*yBI_DRY1
                A(4,1) = xcorU1(nL(k4),mL(k4))*ycorU1(nL(k4),mL(k4))
                A(1,2) = xcorU1(nL(k1),mL(k1))  
                A(2,2) = xcorU1(nL(k2),mL(k2)) 
                A(3,2) = xBI_DRY1
                A(4,2) = xcorU1(nL(k4),mL(k4)) 
                A(1,3) = ycorU1(nL(k1),mL(k1)) 
                A(2,3) = ycorU1(nL(k2),mL(k2))
                A(3,3) = yBI_DRY1 
                A(4,3) = ycorU1(nL(k4),mL(k4))
                A(1,4) = 1._fp
                A(2,4) = 1._fp
                A(3,4) = 1._fp
                A(4,4) = 1._fp
                B(1) = var(nL(k1),mL(k1)+1)   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
                B(2) = var(nL(k2),mL(k2)+1)   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
                B(3) = 0._fp
                B(4) = var(nL(k4),mL(k4)+1)   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
             ELSEIF(IrovLOC==0) then ! free slip
                A(1,1) = xcorU1(nL(k1),mL(k1))*ycorU1(nL(k1),mL(k1)) 
                A(2,1) = xcorU1(nL(k2),mL(k2))*ycorU1(nL(k2),mL(k2))
                A(3,1) = + xBI_DRY1 * nyDRY1 + yBI_DRY1 * nxDRY1 !minus since the normal goes toward the dry cell
                A(4,1) = xcorU1(nL(k4),mL(k4))*ycorU1(nL(k4),mL(k4))
                A(1,2) = xcorU1(nL(k1),mL(k1))  
                A(2,2) = xcorU1(nL(k2),mL(k2)) 
                A(3,2) = nxDRY1  !minus since the normal goes toward the dry cell  
                A(4,2) = xcorU1(nL(k4),mL(k4)) 
                A(1,3) = ycorU1(nL(k1),mL(k1)) 
                A(2,3) = ycorU1(nL(k2),mL(k2))
                A(3,3) = nyDRY1  !minus since the normal goes toward the dry cell  
                A(4,3) = ycorU1(nL(k4),mL(k4))
                A(1,4) = 1._fp
                A(2,4) = 1._fp
                A(3,4) = 0._fp
                A(4,4) = 1._fp
                B(1) = var(nL(k1),mL(k1)+1)   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
                B(2) = var(nL(k2),mL(k2)+1)   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
                B(3) = 0._fp
                B(4) = var(nL(k4),mL(k4)+1)   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)                 
             ELSEIF(IrovLOC==1) then ! partial slip
               WRITE(*,*) 'Partial slip not compatible with cut cells'
               call d3stop(1, gdp)
             ENDIF 
   !
             CALL DGETRF( 4, 4, A, 4, IPIV, INFO ) !compute LU
             CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)        
             CALL DGETRS('N', 4, 1, A, 4, IPIV, B, 4, INFO ) !solve linear system
             CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)     
             v1IP(i) = B(1)*xIPv1(i)*yIPv1(i)+B(2)*xIPv1(i)+B(3)*yIPv1(i)+B(4)
             var(nGP,mGP) = RsignBC * v1IP(i) ! DIRICHLET condition for velocity         
             if (V1IP(i).lt.-1000.or.V1IP(i).gt.1000) then
               write(*,*) ' wrong value of v1ip in n,m,nst,irov', n,m,nst,irov
               call d3stop(1, gdp)
             endif    
!
          else !if contghost=0, i.e. contDRYnoGH=1  or contNNghostWD=1
!
             !i use only the three fluids for simplicity
             k1 = kOK(1)   
             k2 = kOK(2)   
             k3 = kOK(3)     
! 
             A3(1,1) = xcorU1(nL(k1),mL(k1))
             A3(2,1) = xcorU1(nL(k2),mL(k2))
             A3(3,1) = xcorU1(nL(k3),mL(k3))
             A3(1,2) = ycorU1(nL(k1),mL(k1)) 
             A3(2,2) = ycorU1(nL(k2),mL(k2)) 
             A3(3,2) = ycorU1(nL(k3),mL(k3))
             A3(1,3) = 1._fp
             A3(2,3) = 1._fp
             A3(3,3) = 1._fp
             B3(1) = var(nL(k1),mL(k1)+1)
             B3(2) = var(nL(k2),mL(k2)+1)
             B3(3) = var(nL(k3),mL(k3)+1)
!
             CALL DGETRF( 3, 3, A3, 3, IPIV, INFO ) !compute LU
             CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
             CALL DGETRS('N', 3, 1, A3, 3, IPIV, B3, 3, INFO ) !solve linear system
             CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI) 
             v1IP(i) = B3(1)*xIPv1(i)+B3(2)*yIPv1(i)+B3(3) 
             var(nGP,mGP) = RsignBC * v1IP(i)
! 
             if (IrovLOC==2) then  ! no-slip
                var(nGP,mGP) = RsignBC * v1IP(i) ! DIRICHLET condition for velocity
             ELSEIF(IrovLOC==0) then ! free slip
                var(nGP,mGP) = RsignBC * v1IP(i) ! DIRICHLET condition for velocity           
             ELSEIF(IrovLOC==1) then ! partial slip
               WRITE(*,*) 'Partial slip not compatible with cut cells'
               call d3stop(1, gdp)
             ENDIF 
             if (V1IP(i).lt.-1000.or.V1IP(i).gt.1000) then
               write(*,*) ' wrong value of v1ip in n,m,nst,irov', n,m,nst,irov
               call d3stop(1, gdp)
             endif     
!                          
          endif
!
       ELSEIF (contFLUID==2) then      
!         two of the  four corner nodes of the interpolation stencil are on the same 
!         side of the cut-cell boundary  (or in general: i have only 2 fluid points)
!         
         k1 = kOK(1)  
         k4 = kOK(2)  !! I put the BCs on the second and third row so the diagonal term is not going to be zero (for zero-velocity prescribed) 
!
         if (contGHOST.eq.2) then
          !first ghost (note iG1 and iG2 can in general be different from i)
            k2 = kGHOS(1)
            iG1  = FROMmnTOghostV1(nL(k2),mL(k2)+1)   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
            mBI1 = mBIv1(iG1) !not needed for no slip
            nBI1 = nBIv1(iG1) !not needed for no slip
            nxDRY1 = nxG_V1(iG1) 
            nyDRY1 = nyG_V1(iG1) 
            xBI_DRY1 = xBIv1(iG1) + shiftBIv_x(nL(k2),mL(k2)+1) 
            yBI_DRY1 = yBIv1(iG1) + shiftBIv_y(nL(k2),mL(k2)+1) 
          !second ghost
            k3 = kGHOS(2)
            iG2 = FROMmnTOghostV1(nL(k3),mL(k3)+1)   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
            mBI2 = mBIv1(iG2) !not needed for no slip
            nBI2 = nBIv1(iG2) !not needed for no slip
            nxDRY2 = nxG_V1(iG2) 
            nyDRY2 = nyG_V1(iG2) 
            xBI_DRY2 = xBIv1(iG2) + shiftBIv_x(nL(k3),mL(k2)+1) 
            yBI_DRY2 = yBIv1(iG2) + shiftBIv_y(nL(k3),mL(k2)+1) 
         elseif ((contGHOST.eq.1).and.(contNNghostWD.EQ.1)) then
          !first ghost (note iG1 and iG2 can in general be different from i)
            k2 = kGHOS(1)
            iG1  = FROMmnTOghostV1(nL(k2),mL(k2)+1)   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
            mBI1 = mBIv1(iG1) !not needed for no slip
            nBI1 = nBIv1(iG1) !not needed for no slip
            nxDRY1 = nxG_V1(iG1) 
            nyDRY1 = nyG_V1(iG1) 
            xBI_DRY1 = xBIv1(iG1) + shiftBIv_x(nL(k2),mL(k2)+1) 
            yBI_DRY1 = yBIv1(iG1) + shiftBIv_y(nL(k2),mL(k2)+1) 
          !contNNghostWD
            k3 = kNNG(1)
            nBI2 = nL(k3)          !n of the non ghost BC
            mBI2 = mL(k3)+1        !m of the non ghost BC
            if (ghostV1(nL(k3),mL(k3)+1).eq.3)  then !on the wet/dry interface
               nxDRY2 =   ETAx(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY2 =   ETAy(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction         
               xBI_DRY2 = xcorU1(nL(k3),mL(k3)) + shiftBIv_x(nL(k3),mL(k3)+1) 
               yBI_DRY2 = ycorU1(nL(k3),mL(k3)) + shiftBIv_y(nL(k3),mL(k3)+1) 
            else !if (ghostV1(nL(k3)+1,mL(k3).eq.4)  then  !orthogonal to the wet/dry interface
               !check at which side is the image point (it could be a double-valued point, orthogonal to an exact wet-dry interface above and below tiself
               nxDRY2 =   PSIx(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY2 =   PSIy(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction         
               x1 = xcorU1(nL(k3),mL(k3))
               y1 = ycorU1(nL(k3),mL(k3))
               dx = xIPv1(i) - x1  
               dy = yIPv1(i) - y1    
               angleADJ= atan2(dx*PSIy(nBI2,mBI2)-dy*PSIx(nBI2,mBI2),dx*PSIx(nBI2,mBI2)+dy*PSIy(nBI2,mBI2)) 
               DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
               if (DRYUadj) then  
                  xBI_DRY2 = xcorU1(nL(k3),mL(k3)) + nxDRY2 + shiftBIv_x(nL(k3),mL(k3)+1) 
                  yBI_DRY2 = ycorU1(nL(k3),mL(k3)) + nyDRY2 + shiftBIv_y(nL(k3),mL(k3)+1) 
               else                  
                  xBI_DRY2 = xcorU1(nL(k3),mL(k3)) - nxDRY2 + shiftBIv_x(nL(k3),mL(k3)+1) 
                  yBI_DRY2 = ycorU1(nL(k3),mL(k3)) - nyDRY2 + shiftBIv_y(nL(k3),mL(k3)+1) 
               endif              
            endif
!
         elseif (contNNghostWD.EQ.2) then ! unlucky case (a) on my scanned notes
!
           !first contNNghostWD  
            k2 = kNNG(1)
            nBI1 = nL(k2)      !n of the non ghost BC
            mBI1 = mL(k2)+1    !m of the non ghost BC
            if (ghostV1(nL(k2),mL(k2)+1).eq.3)  then !on the wet/dry interface
               nxDRY1 = -PSIy(nBI1,mBI1) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY1 =  PSIx(nBI1,mBI1) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               xBI_DRY1 = xcorU1(nL(k2),mL(k2)) + shiftBIv_x(nL(k2),mL(k2)+1) 
               yBI_DRY1 = ycorU1(nL(k2),mL(k2)) + shiftBIv_y(nL(k2),mL(k2)+1) 
            else !if (ghostV1(nL(k3)+1,mL(k3).eq.4)  then  !orthogonal to the wet/dry interface
               !check at which side is the image point (it could be a double-valued point, orthogonal to an exact wet-dry interface above and below tiself
               nxDRY1 =   PSIx(nBI1,mBI1) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY1 =   PSIy(nBI1,mBI1) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction         
               x1 = xcorU1(nL(k2),mL(k2))
               y1 = ycorU1(nL(k2),mL(k2))
               dx = xIPv1(i) - x1  
               dy = yIPv1(i) - y1    
               angleADJ= atan2(dx*PSIy(nBI1,mBI1)-dy*PSIx(nBI1,mBI1),dx*PSIx(nBI1,mBI1)+dy*PSIy(nBI1,mBI1)) 
               DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
               if (DRYUadj) then  
                  xBI_DRY1 = xcorU1(nL(k2),mL(k2)) + nxDRY1 + shiftBIv_x(nL(k2),mL(k2)+1) 
                  yBI_DRY1 = ycorU1(nL(k2),mL(k2)) + nyDRY1 + shiftBIv_y(nL(k2),mL(k2)+1) 
               else                  
                  xBI_DRY1 = xcorU1(nL(k2),mL(k2)) - nxDRY1 + shiftBIv_x(nL(k2),mL(k2)+1) 
                  yBI_DRY1 = ycorU1(nL(k2),mL(k2)) - nyDRY1 + shiftBIv_y(nL(k2),mL(k2)+1) 
               endif              
            endif     
           !second contNNghostWD
            k3 = kNNG(2)
            nBI2 = nL(k3)      !n of the non ghost BC
            mBI2 = mL(k3)+1    !m of the non ghost BC
            if (ghostV1(nL(k3),mL(k3)+1).eq.3)  then !on the wet/dry interface
               nxDRY2 = -PSIy(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY2 =  PSIx(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               xBI_DRY2 = xcorU1(nL(k3),mL(k3)) + shiftBIv_x(nL(k3),mL(k3)+1) 
               yBI_DRY2 = ycorU1(nL(k3),mL(k3)) + shiftBIv_y(nL(k3),mL(k3)+1)     
            else !if (ghostV1(nL(k3)+1,mL(k3).eq.4)  then  !orthogonal to the wet/dry interface
               !check at which side is the image point (it could be a double-valued point, orthogonal to an exact wet-dry interface above and below tiself
               nxDRY2 =   PSIx(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY2 =   PSIy(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction         
               x1 = xcorU1(nL(k3),mL(k3))
               y1 = ycorU1(nL(k3),mL(k3))
               dx = xIPv1(i) - x1  
               dy = yIPv1(i) - y1    
               angleADJ= atan2(dx*PSIy(nBI2,mBI2)-dy*PSIx(nBI2,mBI2),dx*PSIx(nBI2,mBI2)+dy*PSIy(nBI2,mBI2)) 
               DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
               if (DRYUadj) then  
                  xBI_DRY2 = xcorU1(nL(k3),mL(k3)) + nxDRY2 + shiftBIv_x(nL(k3),mL(k3)+1) 
                  yBI_DRY2 = ycorU1(nL(k3),mL(k3)) + nyDRY2 + shiftBIv_y(nL(k3),mL(k3)+1) 
               else                  
                  xBI_DRY2 = xcorU1(nL(k3),mL(k3)) - nxDRY2 + shiftBIv_x(nL(k3),mL(k3)+1) 
                  yBI_DRY2 = ycorU1(nL(k3),mL(k3)) - nyDRY2 + shiftBIv_y(nL(k3),mL(k3)+1) 
               endif              
            endif                    
         else !!case (b) excluded above by do while
            if (mod(nst,80).eq.0) write(*,*) 'Warning: velocity set to zero since 2 cells in the stencil are dry non-vegetated/dry bank non-ghost cells for u1 in cell (m,n)', mGP,ngp  
            !SHOULD not occur often, only in the case depicted in singlurCASE_2FLUIDSoneGHOST.bmp
            setTOzero = .true.
            !call d3stop(1, gdp)
         endif
!
         if (.not.setTOzero) then
         IF (IrovLOC==2) then ! no slip
!
            A(1,1) = xcorU1(nL(k1),mL(k1))*ycorU1(nL(k1),mL(k1)) 
            A(2,1) = xBI_DRY1*yBI_DRY1
            A(3,1) = xBI_DRY2*yBI_DRY2
            A(4,1) = xcorU1(nL(k4),mL(k4))*ycorU1(nL(k4),mL(k4))
            A(1,2) = xcorU1(nL(k1),mL(k1))  
            A(2,2) = xBI_DRY1
            A(3,2) = xBI_DRY2 
            A(4,2) = xcorU1(nL(k4),mL(k4)) 
            A(1,3) = ycorU1(nL(k1),mL(k1)) 
            A(2,3) = yBI_DRY1
            A(3,3) = yBI_DRY2
            A(4,3) = ycorU1(nL(k4),mL(k4))
            A(1,4) = 1._fp
            A(2,4) = 1._fp
            A(3,4) = 1._fp
            A(4,4) = 1._fp
            B(1) = var(nL(k1),mL(k1)+1) ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
            B(2) = 0._fp
            B(3) = 0._fp
            B(4) = var(nL(k4),mL(k4)+1) ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
!
            CALL DGETRF( 4, 4, A, 4, IPIV, INFO ) !compute LU
            CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
            CALL DGETRS('N', 4, 1, A, 4, IPIV, B, 4, INFO ) !solve linear system
            CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)   
            v1IP(i) = B(1)*xIPv1(i)*yIPv1(i)+B(2)*xIPv1(i)+B(3)*yIPv1(i)+B(4)
            var(nGP,mGP) = RsignBC * v1IP(i) ! DIRICHLET condition for velocity    
            if (V1IP(i).lt.-1000.or.V1IP(i).gt.1000) then
               write(*,*) ' wrong value of v1ip in n,m,nst,irov', n,m,nst,irov
               call d3stop(1, gdp)
            endif  
!
         ELSEIF(IrovLOC==0) then ! free slip
!
            A(1,1) = xcorU1(nL(k1),mL(k1))*ycorU1(nL(k1),mL(k1)) 
            A(2,1) = + xBI_DRY1  * nyDRY1 + yBI_DRY1  * nxDRY1  
            A(3,1) = + xBI_DRY2  * nyDRY2 + yBI_DRY2  * nxDRY2  
            A(4,1) = xcorU1(nL(k4),mL(k4))*ycorU1(nL(k4),mL(k4))
            A(1,2) = xcorU1(nL(k1),mL(k1))  
            A(2,2) = nxDRY1   
            A(3,2) = nxDRY2 
            A(4,2) = xcorU1(nL(k4),mL(k4)) 
            A(1,3) = ycorU1(nL(k1),mL(k1)) 
            A(2,3) = nyDRY1
            A(3,3) = nyDRY2
            A(4,3) = ycorU1(nL(k4),mL(k4))
            A(1,4) = 1._fp
            A(2,4) = 0._fp
            A(3,4) = 0._fp
            A(4,4) = 1._fp
            B(1) = var(nL(k1),mL(k1)+1) ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
            B(2) = 0._fp
            B(3) = 0._fp
            B(4) = var(nL(k4),mL(k4)+1) ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)                               
!
            CALL DGETRF( 4, 4, A, 4, IPIV, INFO ) !compute LU
            !compute 1-norm needed for condition number
            anorm = 0.d0
            do j=1,4
               colsum = 0.d0
               do k=1,4
                  colsum = colsum + abs(A(k,j))
               enddo
               anorm = max(anorm, colsum)
            enddo
            norm = '1'  ! use 1-norm to compute condition number with dgecon
            CALL DGECON(norm,4,a,4,anorm,rcond,work,iwork,info) !http://www.mathworks.com/support/solutions/en/data/1-3KL67Y/?product=SL&solution=1-3KL67Y
            if (rcond.gt.0.000000000000001_fp) then ! 
               CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
               CALL DGETRS('N', 4, 1, A, 4, IPIV, B, 4, INFO ) !solve linear system
               CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)   
               v1IP(i) = B(1)*xIPv1(i)*yIPv1(i)+B(2)*xIPv1(i)+B(3)*yIPv1(i)+B(4)
               var(nGP,mGP) = RsignBC * v1IP(i) ! DIRICHLET condition for velocity    
               if (V1IP(i).lt.-1000.or.V1IP(i).gt.1000) then
                  write(*,*) ' wrong value of v1ip in n,m,nst,irov', n,m,nst,irov
                  call d3stop(1, gdp)
               endif  
            else !The matrix for v1 is singular at machine precision just compute the average of the fluid cell points
               write(*,*) 'Matrix for v1 is singular, rcond = ',rcond
               var(nGP,mGP) = (B(1)+B(4))*0.5_fp               
            endif

         ELSEIF(IrovLOC==1) then ! partial slip
            WRITE(*,*) 'Partial slip not compatible with cut cells'
            call d3stop(1, gdp)
         ENDIF 
         ELSE ! IF(setTOzero) 
          !  var(nGP,mGP) = 0._fp
          !average of fluid points
            var(nGP,mGP) = (var(nL(k1),mL(k1)+1) + var(nL(k4),mL(k4)+1) ) *0.5_fp

         ENDIF
!           
       ELSEIF (contFLUID==1) then      
!         one of the  four corner nodes of the interpolation cell are on the fluid 
!         
         !first fluid cell
         k3 = kOK(1)  !! I put the BCs on the first  and second row so the diagonal term is not going to be zero (for zero-velocity prescribed) 
 
         if (contGHOST.ge.2) then !even if its 3 I just use the first 2 (rare case, see CASEwith3GHOSTSand1FLUIDpoint.bmp mine)
!
            !first ghost (note iG1 and iG2 can in general be different from i)
             k1 = kGHOS(1)
             iG1  = FROMmnTOghostV1(nL(k1),mL(k1)+1)    ! note here n is decreased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
             mBI1 = mBIv1(iG1) !not needed for no slip
             nBI1 = nBIv1(iG1) !not needed for no slip
             xBI_DRY1 = xBIv1(iG1) + shiftBIv_x(nL(k1),mL(k1)+1)  
             yBI_DRY1 = yBIv1(iG1) + shiftBIv_y(nL(k1),mL(k1)+1)   
             nxDRY1 = nxG_V1(iG1) 
             nyDRY1 = nyG_V1(iG1) 
             !second ghost
             k2 = kGHOS(2)
             iG2 = FROMmnTOghostV1(nL(k2),mL(k2)+1)    ! note here n is decreased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
             mBI2 = mBIv1(iG2) !not needed
             nBI2 = nBIv1(iG2) !not needed for no slip
             xBI_DRY2 = xBIv1(iG2) + shiftBIv_x(nL(k2),mL(k2)+1)   
             yBI_DRY2 = yBIv1(iG2) + shiftBIv_y(nL(k2),mL(k2)+1)  
             nxDRY2 = nxG_V1(iG2) 
             nyDRY2 = nyG_V1(iG2) 
! 
          elseif(contGHOST.eq.1.and.contNNghostWD.ge.1) then  
! 
            !first ghost (note iG1 and iG2 can in general be different from i)
             k1 = kGHOS(1) 
             iG1  = FROMmnTOghostV1(nL(k1),mL(k1)+1) 
             mBI1 = mBIv1(iG1)
             nBI1 = nBIv1(iG1)
             xBI_DRY1 = xBIv1(iG1) + shiftBIv_x(nL(k1),mL(k1)+1)   
             yBI_DRY1 = yBIv1(iG1) + shiftBIv_y(nL(k1),mL(k1)+1)   
             nxDRY1 = nxG_V1(iG1) 
             nyDRY1 = nyG_V1(iG1) 
             !look for the second dry point at the wet/dry interface
             k2 = kNNG(1)
             nBI2 = nL(k2)      !n of the non ghost BC
             mBI2 = mL(k2)+1    !m of the non ghost BC
             if (ghostV1(nL(k2),mL(k2)+1).eq.3)  then !on the wet/dry interface
               nxDRY2 =  ETAx(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY2 =  ETAy(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               xBI_DRY2 = xcorU1(nL(k2),mL(k2)) + shiftBIv_x(nL(k2),mL(k2)+1)  
               yBI_DRY2 = ycorU1(nL(k2),mL(k2)) + shiftBIv_y(nL(k2),mL(k2)+1) 
             else !if (ghostV1(nL(k3)+1,mL(k3).eq.4)  then  !orthogonal to the wet/dry interface
               !check at which side is the image point (it could be a double-valued point, orthogonal to an exact wet-dry interface above and below tiself
               nxDRY2 =  PSIx(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY2 =  PSIy(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction         
               x1 = xcorU1(nL(k2),mL(k2))
               y1 = ycorU1(nL(k2),mL(k2))
               dx = xIPv1(i) - x1  
               dy = yIPv1(i) - y1    
               angleADJ= atan2(dx*PSIy(nBI2,mBI2)-dy*PSIx(nBI2,mBI2),dx*PSIx(nBI2,mBI2)+dy*PSIy(nBI2,mBI2)) 
               DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
               if (DRYUadj) then  
                  xBI_DRY2 = xcorU1(nL(k2),mL(k2)) + nxDRY2 + shiftBIv_x(nL(k2),mL(k2)+1) 
                  yBI_DRY2 = ycorU1(nL(k2),mL(k2)) + nyDRY2 + shiftBIv_y(nL(k2),mL(k2)+1) 
               else                  
                  xBI_DRY2 = xcorU1(nL(k2),mL(k2)) - nxDRY2 + shiftBIv_x(nL(k2),mL(k2)+1) 
                  yBI_DRY2 = ycorU1(nL(k2),mL(k2)) - nyDRY2 + shiftBIv_y(nL(k2),mL(k2)+1) 
               endif              
             endif   
                  
          else              
             if (mod(nst,40).eq.0) WRITE(*,*) 'Error for interpolation for v1: only one fluid cell and less then two ghosts/boundary points. BI in cell',mBI,nBI
             setTOzero = .true.
             !call d3stop(1, gdp)
          endif         
   !
          if (.not.setTOzero) then
          IF (IrovLOC==2) then ! no slip  
!
             A3(1,1) = xBI_DRY1
             A3(2,1) = xBI_DRY2
             A3(3,1) = xcorU1(nL(k3),mL(k3)) 
             A3(1,2) = yBI_DRY1 
             A3(2,2) = yBI_DRY2
             A3(3,2) = ycorU1(nL(k3),mL(k3))  
             A3(1,3) = 1._fp
             A3(2,3) = 1._fp
             A3(3,3) = 1._fp
             B3(1) = 0._fp
             B3(2) = 0._fp
             B3(3) = var(nL(k3),mL(k3)+1) ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
!
          ELSEIF(IrovLOC==0) then ! free slip !this basically gives a constant value everywhere
!
             A3(1,1) = nxDRY1
             A3(2,1) = nxDRY2
             A3(3,1) = xcorU1(nL(k3),mL(k3)) 
             A3(1,2) = nyDRY1
             A3(2,2) = nyDRY2
             A3(3,2) = ycorU1(nL(k3),mL(k3))  
             A3(1,3) = 0._fp
             A3(2,3) = 0._fp
             A3(3,3) = 1._fp
             B3(1) = 0._fp
             B3(2) = 0._fp
             B3(3) = var(nL(k3),mL(k3)+1) ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)                                          
!
          ELSEIF(IrovLOC==1) then ! partial slip
             WRITE(*,*) 'Partial slip not compatible with cut cells'
             call d3stop(1, gdp)
          ENDIF 
            
          CALL DGETRF( 3, 3, A3, 3, IPIV, INFO ) !compute LU
          if ((IrovLOC.eq.2).or.((info==0).and.IrovLOC==0)) then
             CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
             CALL DGETRS('N', 3, 1, A3, 3, IPIV, B3, 3, INFO ) !solve linear system
             CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)   
             v1IP(i) = B3(1)*xIPv1(i)+B3(2)*yIPv1(i)+B3(3) 
             var(nGP,mGP) = RsignBC * v1IP(i) ! DIRICHLET condition for velocity    
             if (V1IP(i).lt.-1000.or.V1IP(i).gt.1000) then
               write(*,*) ' wrong value of v1ip in n,m,nst,irov', n,m,nst,irov
               call d3stop(1, gdp)
             endif  
          else !((info/=0).and.IrovLOC==0)))then
             write(*,*) 'The two normals for v1 are coincident for ghost point (mGP,nGP)=,',mGP,nGP,'at time step ',nst
             !call d3stop(1,gdp)  !uncomment this to set the value constant in the cell
             var(nGP,mGP) = var(nL(k3)+1,mL(k3)) 
          endif
          else !if settozero=true
             var(nGP,mGP) = var(nL(k3)+1,mL(k3)) 
          endif
!
       ELSE
          WRITE(515151,*) 'Error for interpolation: NO fluid cells found for v1. BI in cell',mBI,nBI,i
          WRITE(*,*) 'Error for interpolation: NO fluid cells found for v1.  BI in cell',mBI,nBI,i
          call d3stop(1, gdp)
       ENDIF          
       !var(nGP,mGP) = var(nGP,mGP)*DISSghost    
    enddo
!
RETURN
END
!
! 
