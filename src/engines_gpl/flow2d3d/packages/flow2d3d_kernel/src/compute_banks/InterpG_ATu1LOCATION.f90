subroutine interpG_ATu1LOCATION(VAR,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,IsignBC,nlb,nub,mlb,mub,nmlb,nmub,MAXaguu, gdp)
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
    use mathconsts, only: pi
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp)                , pointer :: DISSghost
    integer                 , pointer :: totGHOSTu1
    integer                 , pointer :: typeEXTRAPstencil
    integer, dimension(:,:) , pointer :: FROMmnTOghostU1
    integer, dimension(:)   , pointer :: nGPu1
    integer, dimension(:)   , pointer :: mGPu1
    integer, dimension(:)   , pointer :: mIPu1
    integer, dimension(:)   , pointer :: nIPu1
    integer, dimension(:)   , pointer :: mBIu1
    integer, dimension(:)   , pointer :: nBIu1
    integer, dimension(:,:) , pointer :: GHOSTu1
    real(fp), dimension(:,:), pointer :: PSIx
    real(fp), dimension(:,:), pointer :: PSIy
    real(fp), dimension(:,:), pointer :: ETAx
    real(fp), dimension(:,:), pointer :: ETAy
    real(fp), dimension(:,:), pointer :: aguu
    real(fp), dimension(:,:), pointer :: xcorU1
    real(fp), dimension(:,:), pointer :: xcorV1
    real(fp), dimension(:,:), pointer :: ycorV1
    real(fp), dimension(:,:), pointer :: Nx
    real(fp), dimension(:,:), pointer :: Ny
    real(fp), dimension(:,:), pointer :: xG_V1
    real(fp), dimension(:,:), pointer :: xG_U1
    real(fp), dimension(:)  , pointer :: xIPu1
    real(fp), dimension(:)  , pointer :: yIPu1
    real(fp), dimension(:)  , pointer :: xBIu1
    real(fp), dimension(:)  , pointer :: yBIu1
    real(fp), dimension(:)  , pointer :: u1IP
    real(fp), dimension(:)  , pointer :: nxG_U1
    real(fp), dimension(:)  , pointer :: nyG_U1
    real(fp), dimension(:,:), pointer :: shiftBIu_x
    real(fp), dimension(:,:), pointer :: shiftBIu_y
    logical                 , pointer :: periodSURFACE
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: VAR
    real(fp)                                                            , intent(in)    :: MAXaguu !max value of aguu for which I prescribe ghost cells
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lunscr   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: irov   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: IsignBC
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
  integer                    :: nu
  integer                    :: md
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
    totGHOSTu1        => gdp%gdimbound%totGHOSTu1
    typeEXTRAPstencil => gdp%gdimbound%typeEXTRAPstencil
    FROMmnTOghostU1   => gdp%gdimbound%FROMmnTOghostU1
    nGPu1             => gdp%gdimbound%nGPu1
    mGPu1             => gdp%gdimbound%mGPu1
    mIPu1             => gdp%gdimbound%mIPu1
    nIPu1             => gdp%gdimbound%nIPu1
    mBIu1             => gdp%gdimbound%mBIu1
    nBIu1             => gdp%gdimbound%nBIu1
    GHOSTu1           => gdp%gdimbound%GHOSTu1
    PSIx              => gdp%gdimbound%PSIx
    PSIy              => gdp%gdimbound%PSIy
    ETAx              => gdp%gdimbound%ETAx
    ETAy              => gdp%gdimbound%ETAy
    aguu              => gdp%gdimbound%aguu
    xcorU1            => gdp%gdimbound%xcorU1
    xcorV1            => gdp%gdimbound%xcorV1
    ycorV1            => gdp%gdimbound%ycorV1
    Nx                => gdp%gdimbound%Nx
    Ny                => gdp%gdimbound%Ny
    xG_V1             => gdp%gdimbound%xG_V1
    xG_U1             => gdp%gdimbound%xG_U1
    xIPu1             => gdp%gdimbound%xIPu1
    yIPu1             => gdp%gdimbound%yIPu1
    xBIu1             => gdp%gdimbound%xBIu1
    yBIu1             => gdp%gdimbound%yBIu1
    u1IP              => gdp%gdimbound%u1IP
    nxG_U1            => gdp%gdimbound%nxG_U1
    nyG_U1            => gdp%gdimbound%nyG_U1
    shiftBIu_x        => gdp%gdimbound%shiftBIu_x
    shiftBIu_y        => gdp%gdimbound%shiftBIu_y
    periodSURFACE     => gdp%gdimbound%periodSURFACE
!
!  extrapolate at the boundary, in order to have some sort of tranmissive behaviour
!
    if (typeEXTRAPstencil.ge.1.and..not.periodSURFACE) then ! if periodic tangential velocity is already extrapolated
       do m = 1, mmax
          md = m-1
          do n = 1, nmaxus - 1
             nu = n + 1
   !         extrapolate vertically 
             if (kcs(n, m) == 2 .and. (kcs(nu, m) == 1.and.ghostu1(nu, m) /= 0)) then 
                if (ghostu1(n , m )/=0) var(n, m)  = var(nu, m)   !ghostu1 needed for staircase boundary
                if (ghostu1(n , md)/=0) var(n, md) = var(nu, md)   !ghostu1 needed for staircase boundary
             elseif ((kcs(n, m) == 1.and.ghostu1(n, m) /= 0) .and. kcs(nu, m) == 2) then !ghostu1 needed for staircase boundary
                if (ghostu1(nu, m )/=0) var(nu, m)  = var(n, m)    !ghostu1 needed for staircase boundary
                if (ghostu1(nu, md)/=0) var(nu, md) = var(n, md)  !ghostu1 needed for staircase boundary
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
       WRITE(*,*) 'Wrong IsignBC value' 
       call d3stop(1, gdp)
    END SELECT
!
    do i = 1,totGHOSTu1       
!        
       m = mIPu1(i)
       n = nIPu1(i)
       mGP = mGPu1(i)
       nGP = nGPu1(i)
       if  (comparereal(aguu(nGP,mGP),MAXaguu).gt.0) cycle
       mBI = mBIu1(i)
       nBI = nBIu1(i)
       setTOzero = .false.
!
!      if m equal to 1 do an extrapolation from the stencil corresponding to n=2, otherwise 2 nodes of the stencil are outside the domain
!
       if (m.eq.1) then 
          m = m + 1
       elseif (m.eq.mmax) then
          m = m - 1
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

       Do K=1,4
         IF (GHOSTu1(nL(K)+1,mL(K)) == 0) then !fluid cell ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
            contFLUID = contFLUID + 1
            kOK(contFLUID) = K
         ELSEIF (GHOSTu1(nL(K)+1,mL(K)) == 1) then ! dry ghost cell ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
            contGHOST = contGHOST + 1
            kGHOS(contGHOST) = K
         ELSEIF (GHOSTu1(nL(K)+1,mL(K)) == 2) then ! dry NOT ghost cell 
            contDRYnoGH = contDRYnoGH + 1
            kDRYnoGH(contDRYnoGH) = K
         ELSEIF (GHOSTu1(nL(K)+1,mL(K)) >= 3) then ! bank/wet channel interface or orthogonal to it
            contNNghostWD = contNNghostWD + 1
            kNNG(contNNghostWD) = K
         endif
      ENDDO   
       !
       ! note: Bilinear interpolation for a rectangle has a simple form (Nasr-Azadani, E. Meiburg 2009)
       ! to espress the inverse so  I think it should not be solved the linear system. However, 
       ! if the rectangle is not with edges along x and y teh form is not that simple so I prefer to always
       ! solve the linear system. Moreover the form is not simple when I use the boundary condition itself for
       ! the interpolation, even for dirichlet BC (its not a rectangle anymore)
       !
      ! write(3434343,'(5i6)')  nst,n,m,contFLUID,contGHOST
       IF (contFLUID==4) then      
!
!         simplest case: all the four corner nodes of the interpolation cell are on the fluid
!         
          A(1,1) = xcorV1(nL(1),mL(1))*ycorV1(nL(1),mL(1)) 
          A(2,1) = xcorV1(nL(2),mL(2))*ycorV1(nL(2),mL(2))
          A(3,1) = xcorV1(nL(3),mL(3))*ycorV1(nL(3),mL(3))
          A(4,1) = xcorV1(nL(4),mL(4))*ycorV1(nL(4),mL(4))
          A(1,2) = xcorV1(nL(1),mL(1))  
          A(2,2) = xcorV1(nL(2),mL(2)) 
          A(3,2) = xcorV1(nL(3),mL(3)) 
          A(4,2) = xcorV1(nL(4),mL(4)) 
          A(1,3) = ycorV1(nL(1),mL(1)) 
          A(2,3) = ycorV1(nL(2),mL(2))
          A(3,3) = ycorV1(nL(3),mL(3))
          A(4,3) = ycorV1(nL(4),mL(4))
          A(1,4) = 1._fp
          A(2,4) = 1._fp
          A(3,4) = 1._fp
          A(4,4) = 1._fp
          B(1) = var(nL(1)+1,mL(1))   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
          B(2) = var(nL(2)+1,mL(2))   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
          B(3) = var(nL(3)+1,mL(3))   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
          B(4) = var(nL(4)+1,mL(4))   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
!
      !No pivoting means no row interchanges.  It can be done only if Gaussian elimination never run into zeros on the diagonal. 
      !see here for how to prescribe IPIV for a good pivot http://math.fullerton.edu/mathews/n2003/PivotingMod.html
          CALL DGETRF( contFLUID, contFLUID, A, contFLUID, IPIV, INFO ) !compute LU
          CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
          CALL DGETRS('N', contFLUID, 1, A, contFLUID, IPIV, B, contFLUID, INFO ) !solve linear system
          CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)   
          u1IP(i) = B(1)*xIPu1(i)*yIPu1(i)+B(2)*xIPu1(i)+B(3)*yIPu1(i)+B(4)
          if (IrovLOC==2) then  ! no-slip
             var(nGP,mGP) = RsignBC * u1IP(i) ! DIRICHLET condition for velocity
          ELSEIF(IrovLOC==0) then ! free slip
             var(nGP,mGP) = RsignBC * u1IP(i) ! DIRICHLET condition for velocity           
          ELSEIF(IrovLOC==1) then ! partial slip
            WRITE(*,*) 'Partial slip not compatible with cut cells'
            call d3stop(1, gdp)
          ENDIF 
          if (U1IP(i).lt.-1000.or.U1IP(i).gt.1000) then
             write(*,*) ' wrong value of u1ip in n,m,nst,irov', n,m,nst,irov
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
            iG1  = FROMmnTOghostU1(nL(k3)+1,mL(k3))   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
            mBI1 = mBIu1(iG1) !not needed for no slip
            nBI1 = nBIu1(iG1) !not needed for no slip
            nxDRY1 = nxG_U1(iG1)
            nyDRY1 = nyG_U1(iG1) 
            xBI_DRY1 = xBIu1(iG1) + shiftBIu_x(nL(k3)+1,mL(k3))   !
            yBI_DRY1 = yBIu1(iG1) + shiftBIu_y(nL(k3)+1,mL(k3))  
!
             if (IrovLOC==2) then  ! no-slip
                A(1,1) = xcorV1(nL(k1),mL(k1))*ycorV1(nL(k1),mL(k1)) 
                A(2,1) = xcorV1(nL(k2),mL(k2))*ycorV1(nL(k2),mL(k2))
                A(3,1) = xBI_DRY1*yBI_DRY1 !xBIu1(i)*yBIu1(i) 
                A(4,1) = xcorV1(nL(k4),mL(k4))*ycorV1(nL(k4),mL(k4))
                A(1,2) = xcorV1(nL(k1),mL(k1))  
                A(2,2) = xcorV1(nL(k2),mL(k2)) 
                A(3,2) = xBI_DRY1 !xBIu1(i)
                A(4,2) = xcorV1(nL(k4),mL(k4)) 
                A(1,3) = ycorV1(nL(k1),mL(k1)) 
                A(2,3) = ycorV1(nL(k2),mL(k2))
                A(3,3) = yBI_DRY1 !yBIu1(i) 
                A(4,3) = ycorV1(nL(k4),mL(k4))
                A(1,4) = 1._fp
                A(2,4) = 1._fp
                A(3,4) = 1._fp
                A(4,4) = 1._fp
                B(1) = var(nL(k1)+1,mL(k1))   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
                B(2) = var(nL(k2)+1,mL(k2))   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
                B(3) = 0._fp
                B(4) = var(nL(k4)+1,mL(k4))   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
             ELSEIF(IrovLOC==0) then ! free slip
                A(1,1) = xcorV1(nL(k1),mL(k1))*ycorV1(nL(k1),mL(k1)) 
                A(2,1) = xcorV1(nL(k2),mL(k2))*ycorV1(nL(k2),mL(k2))
                A(3,1) = xBI_DRY1*nyDRY1 + yBI_DRY1*nxDRY1 !- xBIu1(i)* Ny(nBI,mBI) - yBIu1(i)* Nx(nBI,mBI) !minus since the normal goes toward the dry cell
                A(4,1) = xcorV1(nL(k4),mL(k4))*ycorV1(nL(k4),mL(k4))
                A(1,2) = xcorV1(nL(k1),mL(k1))  
                A(2,2) = xcorV1(nL(k2),mL(k2)) 
                A(3,2) = nxDRY1 !- Nx(nBI,mBI)  !minus since the normal goes toward the dry cell  
                A(4,2) = xcorV1(nL(k4),mL(k4)) 
                A(1,3) = ycorV1(nL(k1),mL(k1)) 
                A(2,3) = ycorV1(nL(k2),mL(k2))
                A(3,3) = nyDRY1 !- Ny(nBI,mBI)  !minus since the normal goes toward the dry cell  
                A(4,3) = ycorV1(nL(k4),mL(k4))
                A(1,4) = 1._fp
                A(2,4) = 1._fp
                A(3,4) = 0._fp
                A(4,4) = 1._fp
                B(1) = var(nL(k1)+1,mL(k1))   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
                B(2) = var(nL(k2)+1,mL(k2))   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
                B(3) = 0._fp
                B(4) = var(nL(k4)+1,mL(k4))   ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)          
             ELSEIF(IrovLOC==1) then ! partial slip
               WRITE(*,*) 'Partial slip not compatible with cut cells'
               call d3stop(1, gdp)
             ENDIF 
   !
             CALL DGETRF( 4, 4, A, 4, IPIV, INFO ) !compute LU
             CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)        
             CALL DGETRS('N', 4, 1, A, 4, IPIV, B, 4, INFO ) !solve linear system
             CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)     
             u1IP(i) = B(1)*xIPu1(i)*yIPu1(i)+B(2)*xIPu1(i)+B(3)*yIPu1(i)+B(4)
             var(nGP,mGP) = RsignBC * u1IP(i) ! DIRICHLET condition for velocity   
             if (U1IP(i).lt.-1000.or.U1IP(i).gt.1000) then
               write(*,*) ' wrong value of u1ip in n,m,nst,irov', n,m,nst,irov
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
            A3(1,1) = xcorV1(nL(k1),mL(k1))
            A3(2,1) = xcorV1(nL(k2),mL(k2))
            A3(3,1) = xcorV1(nL(k3),mL(k3))
            A3(1,2) = ycorV1(nL(k1),mL(k1))
            A3(2,2) = ycorV1(nL(k2),mL(k2))
            A3(3,2) = ycorV1(nL(k3),mL(k3))
            A3(1,3) = 1._fp
            A3(2,3) = 1._fp
            A3(3,3) = 1._fp
            B3(1) = var(nL(k1)+1,mL(k1))
            B3(2) = var(nL(k2)+1,mL(k2))
            B3(3) = var(nL(k3)+1,mL(k3))            
!
            CALL DGETRF( 3, 3, A3, 3, IPIV, INFO ) !compute LU
            CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
            CALL DGETRS('N', 3, 1, A3, 3, IPIV, B3, 3, INFO ) !solve linear system
            CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)  
!             
            u1IP(i) = B3(1)*xIPu1(i)+B3(2)*yIPu1(i)+B3(3) 
!
            if (IrovLOC==2) then  ! no-slip
               var(nGP,mGP) = RsignBC * u1IP(i) ! DIRICHLET condition for level 
            ELSEIF(IrovLOC==0) then ! free slip
               var(nGP,mGP) = RsignBC * u1IP(i) ! DIRICHLET condition for level        
            ELSEIF(IrovLOC==1) then ! partial slip
               WRITE(*,*) 'Partial slip not compatible with cut cells'
               !pause
               stop
            ENDIF 
            if (U1IP(i).lt.-1000.or.U1IP(i).gt.1000) then
               write(*,*) ' wrong value of u1ip in n,m,nst,irov', n,m,nst,irov
               call d3stop(1, gdp)
            endif     
         endif
                       
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
            iG1  = FROMmnTOghostU1(nL(k2)+1,mL(k2))   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
            mBI1 = mBIu1(iG1) !not needed for no slip
            nBI1 = nBIu1(iG1) !not needed for no slip
            nxDRY1 = nxG_U1(iG1)
            nyDRY1 = nyG_U1(iG1) 
            xBI_DRY1 = xBIu1(iG1) + shiftBIu_x(nL(k2)+1,mL(k2))   !
            yBI_DRY1 = yBIu1(iG1) + shiftBIu_y(nL(k2)+1,mL(k2))   !
          !second ghost
            k3 = kGHOS(2)
            iG2 = FROMmnTOghostU1(nL(k3)+1,mL(k3))   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
            mBI2 = mBIu1(iG2) !not needed for no slip
            nBI2 = nBIu1(iG2) !not needed for no slip
            nxDRY2 = nxG_U1(iG2)
            nyDRY2 = nyG_U1(iG2) 
            xBI_DRY2 = xBIu1(iG2) + shiftBIu_x(nL(k3)+1,mL(k3))   !
            yBI_DRY2 = yBIu1(iG2) + shiftBIu_y(nL(k3)+1,mL(k3))   !
         elseif ((contGHOST.eq.1).and.(contNNghostWD.EQ.1)) then
          !first ghost (note iG1 and iG2 can in general be different from i)
            k2 = kGHOS(1)
            iG1  = FROMmnTOghostU1(nL(k2)+1,mL(k2))   ! note here m is increased by one because the location of xcorU1(n,m) is the location of xG_v1(n+1,m)
            mBI1 = mBIu1(iG1) !not needed for no slip
            nBI1 = nBIu1(iG1) !not needed for no slip
            nxDRY1 = nxG_U1(iG1)
            nyDRY1 = nyG_U1(iG1) 
            xBI_DRY1 = xBIu1(iG1) + shiftBIu_x(nL(k2)+1,mL(k2))   !
            yBI_DRY1 = yBIu1(iG1) + shiftBIu_y(nL(k2)+1,mL(k2))   !
          !contNNghostWD
            k3 = kNNG(1)
            nBI2 = nL(k3)+1      !n of the non ghost BC
            mBI2 = mL(k3)        !m of the non ghost BC            
            if (ghostU1(nL(k3)+1,mL(k3)).eq.3)  then !on the wet/dry interface
               nxDRY2 =  PSIx(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY2 =  PSIy(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction         
               xBI_DRY2 = xcorV1(nL(k3),mL(k3)) + shiftBIu_x(nL(k3)+1,mL(k3))   !
               yBI_DRY2 = ycorV1(nL(k3),mL(k3)) + shiftBIu_y(nL(k3)+1,mL(k3))  
            else !if (ghostU1(nL(k3)+1,mL(k3).eq.4)  then !orthogonal to the wet/dry interface
               !check at which side is the image point (it could be a double-valued point, orthogonal to an exact wet-dry interface above and below tiself
               nxDRY2 =  ETAx(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY2 =  ETAy(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction               
               x1 = xcorV1(nL(k3),mL(k3))
               y1 = ycorV1(nL(k3),mL(k3))
               dx = xIPu1(i) - x1  
               dy = yIPu1(i) - y1    
               angleADJ= atan2(dx*PSIy(nBI2,mBI2)-dy*PSIx(nBI2,mBI2),dx*PSIx(nBI2,mBI2)+dy*PSIy(nBI2,mBI2)) 
               DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
               if (DRYUadj) then  
                  xBI_DRY2 = xcorV1(nL(k3),mL(k3)) + nxDRY2 + shiftBIu_x(nL(k3)+1,mL(k3))   !
                  yBI_DRY2 = ycorV1(nL(k3),mL(k3)) + nyDRY2 + shiftBIu_y(nL(k3)+1,mL(k3))   !
               else                  
                  xBI_DRY2 = xcorV1(nL(k3),mL(k3)) - nxDRY2 + shiftBIu_x(nL(k3)+1,mL(k3))   !
                  yBI_DRY2 = ycorV1(nL(k3),mL(k3)) - nyDRY2 + shiftBIu_y(nL(k3)+1,mL(k3))   !
               endif              
            endif
         elseif (contNNghostWD.EQ.2) then ! unlucky case (a) on my scanned notes
           !first contNNghostWD  
            k2 = kNNG(1)
            nBI1 = nL(k2)+1      !n of the non ghost BC
            mBI1 = mL(k2)        !m of the non ghost BC
            if (ghostU1(nL(k2)+1,mL(k2)).eq.3)  then !on the wet/dry interface
               nxDRY1 =  PSIx(nBI1,mBI1) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY1 =  PSIy(nBI1,mBI1) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               xBI_DRY1 = xcorV1(nL(k2),mL(k2)) + shiftBIu_x(nL(k2)+1,mL(k2))   !
               yBI_DRY1 = ycorV1(nL(k2),mL(k2)) + shiftBIu_y(nL(k2)+1,mL(k2))  
            else
               !check at which side is the image point (it could be a double-valued point, orthogonal to an exact wet-dry interface above and below tiself
               nxDRY1 =  ETAx(nBI1,mBI1) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY1 =  ETAy(nBI1,mBI1) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction               
               x1 = xcorV1(nL(k2),mL(k2))
               y1 = ycorV1(nL(k2),mL(k2))
               dx = xIPu1(i) - x1  
               dy = yIPu1(i) - y1    
               angleADJ= atan2(dx*ETAy(nBI1,mBI1)-dy*ETAx(nBI1,mBI1),dx*ETAx(nBI1,mBI1)+dy*ETAy(nBI1,mBI1)) 
               DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
               if (DRYUadj) then  
                  xBI_DRY1 = xcorV1(nL(k2),mL(k2)) + nxDRY1 + shiftBIu_x(nL(k2)+1,mL(k2))   !
                  yBI_DRY1 = ycorV1(nL(k2),mL(k2)) + nyDRY1 + shiftBIu_y(nL(k2)+1,mL(k2))   !
               else                  
                  xBI_DRY1 = xcorV1(nL(k2),mL(k2)) - nxDRY1 + shiftBIu_x(nL(k2)+1,mL(k2))   !
                  yBI_DRY1 = ycorV1(nL(k2),mL(k2)) - nyDRY1 + shiftBIu_y(nL(k2)+1,mL(k2))   !
               endif    
            endif
           !second contNNghostWD
            k3 = kNNG(2)
            nBI2 = nL(k3)+1      !n of the non ghost BC
            mBI2 = mL(k3)        !m of the non ghost BC
            if (ghostU1(nL(k3)+1,mL(k3)).eq.3)  then !on the wet/dry interface
               nxDRY2 =  PSIx(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY2 =  PSIy(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction         
               xBI_DRY2 = xcorV1(nL(k3),mL(k3)) + shiftBIu_x(nL(k3)+1,mL(k3))   !
               yBI_DRY2 = ycorV1(nL(k3),mL(k3)) + shiftBIu_y(nL(k3)+1,mL(k3))   !
            else !if (ghostU1(nL(k3)+1,mL(k3).eq.4)  then !orthogonal to the wet/dry interface
               !check at which side is the image point of the i^th GP (it could be a double-valued point, orthogonal to an exact wet-dry interface above and below itself
               nxDRY2 =  ETAx(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY2 =  ETAy(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction               
               x1 = xcorV1(nL(k3),mL(k3))
               y1 = ycorV1(nL(k3),mL(k3))
               dx = xIPu1(i) - x1  
               dy = yIPu1(i) - y1    
               angleADJ= atan2(dx*ETAy(nBI2,mBI2)-dy*ETAx(nBI2,mBI2),dx*ETAx(nBI2,mBI2)+dy*ETAy(nBI2,mBI2)) 
               DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
               if (DRYUadj) then  
                  xBI_DRY2 = xcorV1(nL(k3),mL(k3)) + nxDRY2 + shiftBIu_x(nL(k3)+1,mL(k3))   !
                  yBI_DRY2 = ycorV1(nL(k3),mL(k3)) + nyDRY2 + shiftBIu_y(nL(k3)+1,mL(k3))   !
               else                  
                  xBI_DRY2 = xcorV1(nL(k3),mL(k3)) - nxDRY2 + shiftBIu_x(nL(k3)+1,mL(k3))   !
                  yBI_DRY2 = ycorV1(nL(k3),mL(k3)) - nyDRY2 + shiftBIu_y(nL(k3)+1,mL(k3))   !
               endif              
            endif    
         else
            if (mod(nst,80).eq.0) write(*,*) 'Warning: velocity set to the average since 2 cells in the stencil are dry non-vegetated/dry bank non-ghost cells for u1 in cell (m,n)', mGP,ngp  
            !SHOULD not occur often, only in the case depicted in singlurCASE_2FLUIDSoneGHOST.bmp
            setTOzero = .true.
            !call d3stop(1, gdp)
         endif
!
         if (.not.setTOzero) then
         IF(IrovLOC==2) then ! no slip
!
            A(1,1) = xcorV1(nL(k1),mL(k1))*ycorV1(nL(k1),mL(k1)) 
            A(2,1) = xBI_DRY1*yBI_DRY1
            A(3,1) = xBI_DRY2*yBI_DRY2
            A(4,1) = xcorV1(nL(k4),mL(k4))*ycorV1(nL(k4),mL(k4))
            A(1,2) = xcorV1(nL(k1),mL(k1))  
            A(2,2) = xBI_DRY1
            A(3,2) = xBI_DRY2
            A(4,2) = xcorV1(nL(k4),mL(k4)) 
            A(1,3) = ycorV1(nL(k1),mL(k1)) 
            A(2,3) = yBI_DRY1 
            A(3,3) = yBI_DRY2
            A(4,3) = ycorV1(nL(k4),mL(k4))
            A(1,4) = 1._fp
            A(2,4) = 1._fp
            A(3,4) = 1._fp
            A(4,4) = 1._fp
            B(1) = var(nL(k1)+1,mL(k1)) ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
            B(2) = 0._fp
            B(3) = 0._fp
            B(4) = var(nL(k4)+1,mL(k4)) ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
!
            CALL DGETRF( 4, 4, A, 4, IPIV, INFO ) !compute LU
            CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
            CALL DGETRS('N', 4, 1, A, 4, IPIV, B, 4, INFO ) !solve linear system
            CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)   
            u1IP(i) = B(1)*xIPu1(i)*yIPu1(i)+B(2)*xIPu1(i)+B(3)*yIPu1(i)+B(4)
            var(nGP,mGP) = RsignBC * u1IP(i)   
            if (U1IP(i).lt.-1000.or.U1IP(i).gt.1000) then
               write(*,*) ' wrong value of u1ip in n,m,nst,irov', n,m,nst,irov
               call d3stop(1, gdp)
            endif  
!
        ELSEIF(IrovLOC==0) then ! free slip
 
            A(1,1) = xcorV1(nL(k1),mL(k1))*ycorV1(nL(k1),mL(k1)) 
            A(2,1) = + xBI_DRY1  * nyDRY1 + yBI_DRY1  * nxDRY1  
            A(3,1) = + xBI_DRY2  * nyDRY2 + yBI_DRY2  * nxDRY2  
            A(4,1) = xcorV1(nL(k4),mL(k4))*ycorV1(nL(k4),mL(k4))
            A(1,2) = xcorV1(nL(k1),mL(k1))  
            A(2,2) = nxDRY1   
            A(3,2) = nxDRY2 
            A(4,2) = xcorV1(nL(k4),mL(k4)) 
            A(1,3) = ycorV1(nL(k1),mL(k1)) 
            A(2,3) = nyDRY1
            A(3,3) = nyDRY2
            A(4,3) = ycorV1(nL(k4),mL(k4))
            A(1,4) = 1._fp
            A(2,4) = 0._fp
            A(3,4) = 0._fp
            A(4,4) = 1._fp
            B(1) = var(nL(k1)+1,mL(k1)) ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
            B(2) = 0._fp
            B(3) = 0._fp
            B(4) = var(nL(k4)+1,mL(k4)) ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)                        
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
               u1IP(i) = B(1)*xIPu1(i)*yIPu1(i)+B(2)*xIPu1(i)+B(3)*yIPu1(i)+B(4)
               var(nGP,mGP) = RsignBC * u1IP(i)   
               if (U1IP(i).lt.-1000.or.U1IP(i).gt.1000) then
                  write(*,*) ' wrong value of u1ip in n,m,nst,irov', n,m,nst,irov
                  call d3stop(1, gdp)
               endif   
            else !The matrix for u1 is singular at machine precision just compute the average of the fluid cell points
               write(*,*) 'Matrix for u1 is singular, rcond = ',rcond
               var(nGP,mGP) = (B(1)+B(4))*0.5_fp               
            endif
         ELSEIF(IrovLOC==1) then ! partial slip
            WRITE(*,*) 'Partial slip not compatible with cut cells'
            call d3stop(1, gdp)
         ENDIF 
         ELSE ! IF(setTOzero) 
           ! var(nGP,mGP) = 0._fp
            var(nGP,mGP) =  (var(nL(k1)+1,mL(k1))+ var(nL(k4)+1,mL(k4)))*0.5_fp
         ENDIF
!
! 
       ELSEIF (contFLUID==1) then      
!         one of the  four corner nodes of the interpolation cell are on the fluid 
!         
          !first fluid cell
          k3 = kOK(1)  
          if (contGHOST.ge.2) then !even if its 3 I just use the first 2 (rare case, see CASEwith3GHOSTSand1FLUIDpoint.bmp mine)
!                       
             !first ghost (note iG1 and iG2 can in general be different from i)
             k1 = kGHOS(1)
             iG1  = FROMmnTOghostU1(nL(k1)+1,mL(k1)) 
             mBI1 = mBIu1(iG1)
             nBI1 = nBIu1(iG1)
             xBI_DRY1 = xBIu1(iG1) + shiftBIu_x(nL(k1)+1,mL(k1))  
             yBI_DRY1 = yBIu1(iG1) + shiftBIu_y(nL(k1)+1,mL(k1)) 
             nxDRY1 = nxG_U1(iG1)
             nyDRY1 = nyG_U1(iG1)
             !second ghost
             k2 = kGHOS(2)  
             iG2 = FROMmnTOghostU1(nL(k2)+1,mL(k2)) 
             mBI2 = mBIu1(iG2)
             nBI2 = nBIu1(iG2) 
             xBI_DRY2 = xBIu1(iG2) + shiftBIu_x(nL(k2)+1,mL(k2)) 
             yBI_DRY2 = yBIu1(iG2) + shiftBIu_y(nL(k2)+1,mL(k2))  
             nxDRY2 = nxG_U1(iG2)
             nyDRY2 = nyG_U1(iG2)
!
          elseif(contGHOST.eq.1.and.contNNghostWD.ge.1) then  
!  
            !first ghost (note iG1 and iG2 can in general be different from i)
             k1 = kGHOS(1) 
             iG1  = FROMmnTOghostU1(nL(k1)+1,mL(k1)) 
             mBI1 = mBIu1(iG1)
             nBI1 = nBIu1(iG1)
             xBI_DRY1 = xBIu1(iG1) + shiftBIu_x(nL(k1)+1,mL(k1))  
             yBI_DRY1 = yBIu1(iG1) + shiftBIu_y(nL(k1)+1,mL(k1))  
             nxDRY1 = nxG_U1(iG1)
             nyDRY1 = nyG_U1(iG1)
             !look for the second dry point at the wet/dry interface
             k2 = kNNG(1)
             nBI2 = nL(k2)+1      !n of the non ghost BC
             mBI2 = mL(k2)        !m of the non ghost BC
             if (ghostU1(nL(k2)+1,mL(k2)).eq.3)  then
               nxDRY2 =  PSIx(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY2 =  PSIy(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction         
               xBI_DRY2 = xcorV1(nL(k2),mL(k2)) + shiftBIu_x(nL(k2)+1,mL(k2))  
               yBI_DRY2 = ycorV1(nL(k2),mL(k2)) + shiftBIu_y(nL(k2)+1,mL(k2)) 
             else !if (ghostU1(nL(k2)+1,mL(k2).eq.4)  then
               !check at which side is the image point (it could be a double-valued point, orthogonal to an exact wet-dry interface above and below tiself
               nxDRY2 =  ETAx(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction 
               nyDRY2 =  ETAy(nBI2,mBI2) ! I figured out that clearly the orientation (verso) does not matter to impose the derivative equal to zero, only the direction               
               x1 = xcorV1(nL(k2),mL(k2))
               y1 = ycorV1(nL(k2),mL(k2)) 
               dx = xIPu1(i) - x1  
               dy = yIPu1(i) - y1    
               angleADJ= atan2(dx*ETAy(nBI2,mBI2)-dy*ETAx(nBI2,mBI2),dx*ETAx(nBI2,mBI2)+dy*ETAy(nBI2,mBI2)) 
               DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
               if (DRYUadj) then  
                  xBI_DRY2 = xcorV1(nL(k2),mL(k2)) + nxDRY2 + shiftBIu_x(nL(k2)+1,mL(k2)) 
                  yBI_DRY2 = ycorV1(nL(k2),mL(k2)) + nyDRY2 + shiftBIu_y(nL(k2)+1,mL(k2)) 
               else                  
                  xBI_DRY2 = xcorV1(nL(k2),mL(k2)) - nxDRY2 + shiftBIu_x(nL(k2)+1,mL(k2)) 
                  yBI_DRY2 = ycorV1(nL(k2),mL(k2)) - nyDRY2 + shiftBIu_y(nL(k2)+1,mL(k2)) 
               endif              
             endif
                  
          else 
             if (mod(nst,40).eq.0) WRITE(*,*) 'Error for interpolation for u1: only one fluid cell and less then two ghosts/boundary points. BI in cell',mBI,nBI
             setTOzero = .true.
             !    pause
             !stop
          endif    
!
          if (.not.setTOzero) then
          IF (IrovLOC==2) then ! no slip   
!                         
             A3(1,1) = xBI_DRY1
             A3(2,1) = xBI_DRY2
             A3(3,1) = xcorV1(nL(k3),mL(k3)) 
             A3(1,2) = yBI_DRY1
             A3(2,2) = yBI_DRY2
             A3(3,2) = ycorV1(nL(k3),mL(k3))  
             A3(1,3) = 1._fp
             A3(2,3) = 1._fp
             A3(3,3) = 1._fp
             B3(1) = 0._fp
             B3(2) = 0._fp
             B3(3) = var(nL(k3)+1,mL(k3)) ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)  
!
          ELSEIF(IrovLOC==0) then ! free slip !this basically gives a constant value everywhere
!
             A3(1,1) = nxDRY1
             A3(2,1) = nxDRY2
             A3(3,1) = xcorV1(nL(k3),mL(k3)) 
             A3(1,2) = nyDRY1
             A3(2,2) = nyDRY2
             A3(3,2) = ycorV1(nL(k3),mL(k3))  
             A3(1,3) = 0._fp
             A3(2,3) = 0._fp
             A3(3,3) = 1._fp
             B3(1) = 0._fp
             B3(2) = 0._fp
             B3(3) = var(nL(k3)+1,mL(k3)) ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)                       
!
          ELSEIF(IrovLOC==1) then ! partial slip
             WRITE(*,*) 'Partial slip not compatible with cut cells'
             call d3stop(1, gdp)
          ENDIF 
!
          CALL DGETRF( 3, 3, A3, 3, IPIV, INFO ) !compute LU
          if ((IrovLOC.eq.2).or.((info==0).and.IrovLOC==0)) then
             CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
             CALL DGETRS('N', 3, 1, A3, 3, IPIV, B3, 3, INFO ) !solve linear system
             CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)   
             u1IP(i) = B3(1)*xIPu1(i)+B3(2)*yIPu1(i)+B3(3) 
             var(nGP,mGP) = RsignBC * u1IP(i) ! DIRICHLET condition for velocity              
             if (U1IP(i).lt.-1000.or.U1IP(i).gt.1000) then
               write(*,*) ' wrong value of u1ip in n,m,nst,irov', n,m,nst,irov
               call d3stop(1, gdp)
             endif  
          else !((info/=0).and.IrovLOC==0)))then
             write(*,*) 'The two normals for u1 are coincident for ghost point (mGP,nGP)=,',mGP,nGP,'at time step ',nst
             !call d3stop(1,gdp)  !uncomment this to set the value constant in the cell
             var(nGP,mGP) = var(nL(k3)+1,mL(k3)) 
          endif
          else !if settozero=true
             var(nGP,mGP) = var(nL(k3)+1,mL(k3)) 
          endif
       ELSE
          WRITE(515151,*) 'Error for interpolation: NO fluid cells found for u1. BI in cell',mBI,nBI,i
          WRITE(*,*) 'Error for interpolation: NO fluid cells found for u1.  BI in cell',mBI,nBI,i
          call d3stop(1, gdp)
       ENDIF       
       !var(nGP,mGP) = var(nGP,mGP)*DISSghost   
    enddo
!
RETURN
END
!
! 
