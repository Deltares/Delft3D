subroutine interpG_ATs1LOCATION(VAR,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub, gdp)
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
    integer, dimension(:,:) , pointer :: FROMmnTOghostS1
    integer, dimension(:)   , pointer :: nGPs1
    integer, dimension(:)   , pointer :: mGPs1
    integer, dimension(:)   , pointer :: mIPs1
    integer, dimension(:)   , pointer :: nIPs1
    integer, dimension(:)   , pointer :: mBIs1
    integer, dimension(:)   , pointer :: nBIs1
    integer, dimension(:,:) , pointer :: GHOSTs1
    real(fp), dimension(:,:), pointer :: PSIx
    real(fp), dimension(:,:), pointer :: PSIy
    real(fp), dimension(:,:), pointer :: ETAx
    real(fp), dimension(:,:), pointer :: ETAy
    real(fp), dimension(:,:), pointer :: xG
    real(fp), dimension(:,:), pointer :: yG
    real(fp), dimension(:,:), pointer :: Nx
    real(fp), dimension(:,:), pointer :: Ny
    real(fp), dimension(:)  , pointer :: xIPs1
    real(fp), dimension(:)  , pointer :: yIPs1
    real(fp), dimension(:)  , pointer :: xBIs1
    real(fp), dimension(:)  , pointer :: yBIs1
    real(fp), dimension(:)  , pointer :: s1IP
    real(fp), dimension(:)  , pointer :: nxG_S1
    real(fp), dimension(:)  , pointer :: nyG_S1
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: VAR
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
  integer                    :: cont
  integer                    :: i
  integer                    :: j
  integer                    :: m
  integer                    :: n
  integer                    :: km
  integer                    :: kn
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
  integer                    :: contFLUID_contFLUIDdryNNveg
  integer                    :: contFLUIDdryNNveg
  integer                    :: nDRY   
  integer                    :: mDRY  
  integer                    :: signINT
  integer                    :: IPIV(4) 
!
  integer                    :: kOK(4)
  integer                    :: kGHOS(4)
  integer                    :: kDRYnoGH(4)
  integer                    :: iwork(4)
  integer                    :: iwork3(3)
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
  real(fp)                   :: rcond
  real(fp)                   :: colsum
  real(fp)                   :: anorm
  real(fp)                   :: averVAR
!
  real(fp)                   :: butta
  real(fp)                   :: A(4,4)
  real(fp)                   :: A3(3,3)
  real(fp)                   :: B(4)
  real(fp)                   :: B3(3)
  real(fp)                   :: work(16)
  real(fp)                   :: work3(9)
  real(fp)                   :: VARstorage(4)
!
  character, dimension(1)    :: norm
!
! executable statements -------------------------------------------------------
!  
    totGHOSTs1      => gdp%gdimbound%totGHOSTs1
    kfs_cc          => gdp%gdimbound%kfs_cc
    FROMmnTOghostS1 => gdp%gdimbound%FROMmnTOghostS1
    nGPs1           => gdp%gdimbound%nGPs1
    mGPs1           => gdp%gdimbound%mGPs1
    mIPs1           => gdp%gdimbound%mIPs1
    nIPs1           => gdp%gdimbound%nIPs1
    mBIs1           => gdp%gdimbound%mBIs1
    nBIs1           => gdp%gdimbound%nBIs1
    GHOSTs1         => gdp%gdimbound%GHOSTs1
    PSIx            => gdp%gdimbound%PSIx
    PSIy            => gdp%gdimbound%PSIy
    ETAx            => gdp%gdimbound%ETAx
    ETAy            => gdp%gdimbound%ETAy
    xG              => gdp%gdimbound%xG
    yG              => gdp%gdimbound%yG
    Nx              => gdp%gdimbound%Nx
    Ny              => gdp%gdimbound%Ny
    xIPs1           => gdp%gdimbound%xIPs1
    yIPs1           => gdp%gdimbound%yIPs1
    xBIs1           => gdp%gdimbound%xBIs1
    yBIs1           => gdp%gdimbound%yBIs1
    s1IP            => gdp%gdimbound%s1IP
    nxG_S1          => gdp%gdimbound%nxG_S1
    nyG_S1          => gdp%gdimbound%nyG_S1
!   
!    DONEinterpS1(1:totGHOSTs1) =.FALSE. !for image point with common interpolation stencil (contFLUID==2)
!
!   FARE un test con una bank che parte a met`a cella su tutta l ' orizzontale (cio`e tutte ghost cell adiacenti). Vedere se l' interpolazione con contFLUID==2 visto che defgenera in 4 punti a 2 a 2 coincidenti funziona
!
!
    do i = 1,totGHOSTs1       
!     
       m = mIPs1(i)
       n = nIPs1(i)
       mGP = mGPs1(i)
       nGP = nGPs1(i)
       mBI = mBIs1(i)
       nBI = nBIs1(i)
 
!    this modification should not be needed in general if on the corners kfs=0. If 2 I could have this problem: problemOFghostCELLatTHEcornerIFkfc=2 there.bmp
!
       if (m.eq.1) then 
          m = m + 1
       endif
       if (n.eq.1) then
          n = n + 1
       endif
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
       contFLUID = 0
       contGHOST = 0
       contDRYnoGH = 0
       contFLUIDdryNNveg = 0
       averVAR = 0._fp
!
       Do K=1,4
          IF (GHOSTs1(nL(K),mL(K)) == 0) then !fluid cell
             contFLUID = contFLUID + 1
             averVAR = averVAR + var(nL(K),mL(K))
             kOK(contFLUID) = K
          ELSEIF (GHOSTs1(nL(K),mL(K)) == 1) then ! dry ghost cell
             contGHOST = contGHOST + 1
             kGHOS(contGHOST) = K
          ELSEIF (GHOSTs1(nL(K),mL(K)) == 2) then ! dry NOT ghost cell ! It is used at the boundary!!
             contDRYnoGH = contDRYnoGH + 1
             kDRYnoGH(contDRYnoGH) = K
          ELSEIF (GHOSTs1(nL(K),mL(K)) == -1) then !dry not-vegetated cell (I have to assing it a meaningful value of surface)
             contFLUID = contFLUID + 1
             contFLUIDdryNNveg = contFLUIDdryNNveg + 1
             kOK(contFLUID) = K
          endif
       ENDDO   
!  
!      assign value of water surface to dry not-vegetated cell (if existing). I assign the average of all the fluid cells
!
       contFLUID_contFLUIDdryNNveg = contFLUID-contFLUIDdryNNveg
       if (contFLUID_contFLUIDdryNNveg.gt.0) then
          averVAR = averVAR/(contFLUID_contFLUIDdryNNveg)
       elseif ((contFLUID_contFLUIDdryNNveg.eq.0).and.(contFLUID.ne.0)) then
          write(*,*) 'Warning: contFLUID-contFLUIDdryNNveg is zero. (contFLUID,contFLUIDdryNNveg,mgp,ngp)=',contFLUID,contFLUIDdryNNveg,mgp,ngp  ! If there is only 1 or more contFLUIDdryNNveg and no fluid cells  I CANNOT define averVAR for the cells FLUIDdryNNveg. If both zero even worst I cannot interpolate
          cont = 0
          ! I do the average of the wet adjacent nodes and I return
          do km=max(mGP-1,1),min(mGP+1,mmax)
             do kn=max(nGP-1,1),min(nGP+1,nmaxus)
               if (kfs_cc(kn,km).ge.1) then ! any active cell with water surface larger then upper bed elevation
                  cont = cont+1
                  averVAR = averVAR + var(kn,km) 
               endif
             enddo
          enddo
          var(nGP,mGP) = averVAR/cont
          if (cont ==0) then
              write(*,*) 'contFLUID-contFLUIDdryNNveg is zero and no adjacent is wet . (contFLUID,contFLUIDdryNNveg,mgp,ngp)=',contFLUID,contFLUIDdryNNveg,mgp,ngp  ! If there is only 1 or more contFLUIDdryNNveg and no fluid cells  I CANNOT define averVAR for the cells FLUIDdryNNveg. If both zero even worst I cannot interpolate
              !pause  
              stop
          endif
          return
        !  pause !it should never occur. It it occurs, that means that it uses the value of water surface on the dry not-vegetated cell that corresponds to bed elevation (not very good but it should not crash)
        !  stop
       endif
       DO K =contFLUID_contFLUIDdryNNveg+1,contFLUID
          kk = kOK(K) 
          VARstorage(K) = var(nL(KK),mL(KK)) ! I store the original water surfaces here and I put them back in s1 (i.e. var) at the end of the subr
          var(nL(KK),mL(KK)) = averVAR
       ENDDO
!
       !
       ! note: Bilinear interpolation for a rectangle has a simple form (Nasr-Azadani, E. Meiburg 2009)
       ! to espress the inverse so  I think it should not be solved the linear system. However, 
       ! if the rectangle is not with edges along x and y teh form is not that simple so I prefer to always
       ! solve the linear system. Moreover the form is not simple when I use the boundary condition itself for
       ! the interpolation, even for dirichlet BC (its not a rectangle anymore)
       !
      IF (contFLUID==4) then      
!
!         simplest case: all the four corner nodes of the interpolation cell are on the fluid
!         
         A(1,1) = xG(nL(1),mL(1))*yG(nL(1),mL(1)) 
         A(2,1) = xG(nL(2),mL(2))*yG(nL(2),mL(2))
         A(3,1) = xG(nL(3),mL(3))*yG(nL(3),mL(3))
         A(4,1) = xG(nL(4),mL(4))*yG(nL(4),mL(4))
         A(1,2) = xG(nL(1),mL(1))  
         A(2,2) = xG(nL(2),mL(2)) 
         A(3,2) = xG(nL(3),mL(3)) 
         A(4,2) = xG(nL(4),mL(4)) 
         A(1,3) = yG(nL(1),mL(1)) 
         A(2,3) = yG(nL(2),mL(2))
         A(3,3) = yG(nL(3),mL(3))
         A(4,3) = yG(nL(4),mL(4))
         A(1,4) = 1._fp
         A(2,4) = 1._fp
         A(3,4) = 1._fp
         A(4,4) = 1._fp
         B(1) = var(nL(1),mL(1))
         B(2) = var(nL(2),mL(2))
         B(3) = var(nL(3),mL(3))
         B(4) = var(nL(4),mL(4))
!
      !No pivoting means no row interchanges.  It can be done only if Gaussian elimination never run into zeros on the diagonal. 
      !see here for how to prescribe IPIV for a good pivot http://math.fullerton.edu/mathews/n2003/PivotingMod.html
         CALL DGETRF( contFLUID, contFLUID, A, contFLUID, IPIV, INFO ) !compute LU
         IF (INFO/=0) THEN
            IF (INFO<0) THEN 
               WRITE(*,'(a,i2,100a)') 'DGETRF error: the ', -INFO,'-th argument has an illegal value'
               !pause
               stop
            ELSE
               WRITE(*,'(a,i1,a,i1,a)') 'U(',INFO,',',INFO,') is exactly zero. The factorization &
               has been completed, but the factor U is exactly &
               singular, and division by zero will occur if it &
               is used to solve a system of equations'
               !PAUSE 
               STOP
            ENDIF
         ENDIF
         CALL DGETRS('N', contFLUID, 1, A, contFLUID, IPIV, B, contFLUID, INFO ) !solve linear system
         s1IP(i) = B(1)*xIPs1(i)*yIPs1(i)+B(2)*xIPs1(i)+B(3)*yIPs1(i)+B(4)
         var(nGP,mGP) = s1IP(i) ! Neumann condition for level
         if (s1IP(i).lt.-1000.or.s1IP(i).gt.1000) then
            write(*,*) ' wrong value of s1ip in n,m', n,m
            !pause
            stop            
         endif    
      ELSEIF (contFLUID==3) then      
!         three of the  four corner nodes of the interpolation cell are on the fluid
!         
         IF (contDRYnoGH.EQ.0) THEN !this means that the forth is a ghost cell. NOT SURE actually if IT IS ever POSSIBLE THAT contDRYnoGH IS 1
            !3 fluid cells
            k1 = kOK(1)  
            k2 = kOK(2) 
            k4 = kOK(3)  !! I put the BC on the third row so the diagonal term is not going to be zero (for zero-velocity prescribed) 
            !1 ghost cell
            k3 = kGHOS(1)
            iG1  = FROMmnTOghostS1(nL(k3),mL(k3)) 
            nxDRY1 = nxG_S1(iG1) !- Nx(nBI1,mBI1)
            nyDRY1 = nyG_S1(iG1) !- Ny(nBI1,mBI1)

            A(1,1) = xG(nL(k1),mL(k1))*yG(nL(k1),mL(k1)) 
            A(2,1) = xG(nL(k2),mL(k2))*yG(nL(k2),mL(k2))
            A(3,1) = + xBIs1(i)* nyDRY1 + yBIs1(i)* nxDRY1   !- xBIs1(i)* Ny(nBI,mBI) - yBIs1(i)* Nx(nBI,mBI) !minus since the normal goes toward the dry cell
            A(4,1) = xG(nL(k4),mL(k4))*yG(nL(k4),mL(k4))
            A(1,2) = xG(nL(k1),mL(k1))  
            A(2,2) = xG(nL(k2),mL(k2)) 
            A(3,2) = + nxDRY1 !- Nx(nBI,mBI)  !minus since the normal goes toward the dry cell  
            A(4,2) = xG(nL(k4),mL(k4)) 
            A(1,3) = yG(nL(k1),mL(k1)) 
            A(2,3) = yG(nL(k2),mL(k2))
            A(3,3) = + nyDRY1 !- Ny(nBI,mBI)  !minus since the normal goes toward the dry cell  
            A(4,3) = yG(nL(k4),mL(k4))
            A(1,4) = 1._fp
            A(2,4) = 1._fp
            A(3,4) = 0._fp
            A(4,4) = 1._fp
            B(1) = var(nL(k1),mL(k1))
            B(2) = var(nL(k2),mL(k2))
            B(3) = 0._fp
            B(4) = var(nL(k4),mL(k4))
   !
            CALL DGETRF( 4, 4, A, 4, IPIV, INFO ) !compute LU
            CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
            CALL DGETRS('N', 4, 1, A, 4, IPIV, B, 4, INFO ) !solve linear system
            CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)   
            s1IP(i) = B(1)*xIPs1(i)*yIPs1(i)+B(2)*xIPs1(i)+B(3)*yIPs1(i)+B(4)
            var(nGP,mGP) = s1IP(i) ! Neumann condition for level      
            if (s1IP(i).lt.-1000.or.s1IP(i).gt.1000) then
             write(*,*) ' wrong value of s1ip in n,m', n,m
             !pause
             stop    
            endif                   
         ELSE
            WRITE(*,*) 'Error for interpolation: case not considered: THREE fluid cells and 1 dry  NOT ghost cell in cell',mBI,nBI
            !pause
            stop
         ENDIF
      ELSEIF (contFLUID==2) then      
!         two of the  four corner nodes of the interpolation cell are on the same 
!         side of the cut-cell boundary as the computed cell.
!       
         if (contGHOST.eq.2) then
            k1 = kOK(1)  
            k2 = kGHOS(1)
            k3 = kGHOS(2)
            k4 = kOK(2)  !! I put the BCs on the second and third row so the diagonal term is not going to be zero (for zero-velocity prescribed) 
            !first ghost (note iG1 and iG2 can in general be different from i)
            iG1  = FROMmnTOghostS1(nL(k2),mL(k2)) 
            mBI1 = mBIs1(iG1)
            nBI1 = nBIs1(iG1)
            nxDRY1 = nxG_S1(iG1) !- Nx(nBI1,mBI1)
            nyDRY1 = nyG_S1(iG1) !- Ny(nBI1,mBI1)
         !second ghost
            iG2 = FROMmnTOghostS1(nL(k3),mL(k3)) 
            mBI2 = mBIs1(iG2)
            nBI2 = nBIs1(iG2)
            nxDRY2 = nxG_S1(iG2) !- Nx(nBI2,mBI2)
            nyDRY2 = nyG_S1(iG2) !- Ny(nBI2,mBI2)

            A(1,1) = xG(nL(k1),mL(k1))*yG(nL(k1),mL(k1)) 
            A(2,1) = xBIs1(iG1)* nyDRY1 + yBIs1(iG1)* nxDRY1 !- xBIs1(iG1)* Ny(nBI1,mBI1) - yBIs1(iG1)* Nx(nBI1,mBI1) !minus since the normal goes toward the dry cell
            A(3,1) = xBIs1(iG2)* nyDRY2 + yBIs1(iG2)* nxDRY2 !minus since the normal goes toward the dry cell
            A(4,1) = xG(nL(k4),mL(k4))*yG(nL(k4),mL(k4))
            A(1,2) = xG(nL(k1),mL(k1))  
            A(2,2) = nxDRY1 !- Nx(nBI1,mBI1)  !minus since the normal goes toward the dry cell  
            A(3,2) = nxDRY2 !- Nx(nBI2,mBI2)  !minus since the normal goes toward the dry cell  
            A(4,2) = xG(nL(k4),mL(k4)) 
            A(1,3) = yG(nL(k1),mL(k1)) 
            A(2,3) = nyDRY1 !- Ny(nBI1,mBI1)  !minus since the normal goes toward the dry cell  
            A(3,3) = nyDRY2 !- Ny(nBI2,mBI2)  !minus since the normal goes toward the dry cell  
            A(4,3) = yG(nL(k4),mL(k4))
            A(1,4) = 1._fp
            A(2,4) = 0._fp
            A(3,4) = 0._fp
            A(4,4) = 1._fp
            B(1) = var(nL(k1),mL(k1))
            B(2) = 0._fp
            B(3) = 0._fp
            B(4) = var(nL(k4),mL(k4))
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
               s1IP(i) = B(1)*xIPs1(i)*yIPs1(i)+B(2)*xIPs1(i)+B(3)*yIPs1(i)+B(4)
               var(nGP,mGP) = s1IP(i) ! Neumann condition for level 
               if (s1IP(i).lt.-1000.or.s1IP(i).gt.1000) then
                write(*,*) ' wrong value of s1ip in n,m', n,m
                !pause
                stop    
               endif     
            else !the matrix is singular at machine precision just compute the average of the fluid cell points
               write(*,*) 'Matrix is singular, rcond = ',rcond
               var(nGP,mGP) = (B(1)+B(4))*0.5_fp               
            endif               
         elseif (contGHOST.eq.1) then   
!
            k1 = kGHOS(1)  
            k2 = kOK(1)  !! I put the BCs on the first  and second row so the diagonal term is not going to be zero (for zero-velocity prescribed) 
            k3 = kOK(2)  !! I put the BCs on the first  and second row so the diagonal term is not going to be zero (for zero-velocity prescribed) 
!                    
            !first ghost (note iG1 and iG2 can in general be different from i)
            iG1  = FROMmnTOghostS1(nL(k1),mL(k1)) 
            mBI1 = mBIs1(iG1)
            nBI1 = nBIs1(iG1)     
            nxDRY1 = nxG_S1(iG1) !- Nx(nBI1,mBI1)
            nyDRY1 = nyG_S1(iG1) !- Ny(nBI1,mBI1)
! 
            A3(1,1) = nxDRY1 !- Nx(nBI1,mBI1) !minus since the normal goes toward the dry cell 
            A3(2,1) = xG(nL(k2),mL(k2)) 
            A3(3,1) = xG(nL(k3),mL(k3)) 
            A3(1,2) = nyDRY1 !- Ny(nBI1,mBI1)  !minus since the normal goes toward the dry cell
            A3(2,2) = yG(nL(k2),mL(k2))  
            A3(3,2) = yG(nL(k3),mL(k3))  
            A3(1,3) = 0._fp
            A3(2,3) = 1._fp
            A3(3,3) = 1._fp
            B3(1) = 0._fp
            B3(2) = var(nL(k2),mL(k2))
            B3(3) = var(nL(k3),mL(k3))
!
            CALL DGETRF( 3, 3, A3, 3, IPIV, INFO ) !compute LU
            !compute 1-norm needed for condition number
            anorm = 0.d0
            do j=1,3
               colsum = 0.d0
               do k=1,3
                  colsum = colsum + abs(A3(k,j))
               enddo
               anorm = max(anorm, colsum)
            enddo
            norm = '1'  ! use 1-norm to compute condition number with dgecon
            CALL DGECON(norm,3,A3,3,anorm,rcond,work3,iwork3,info) !http://www.mathworks.com/support/solutions/en/data/1-3KL67Y/?product=SL&solution=1-3KL67Y
            if (rcond.gt.0.000000000000001_fp) then ! 
               CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
               CALL DGETRS('N', 3, 1, A3, 3, IPIV, B3, 3, INFO ) !solve linear system
               CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)   
               s1IP(i) = B3(1)*xIPs1(i)+B3(2)*yIPs1(i)+B3(3) 
               var(nGP,mGP) = s1IP(i) ! Neumann condition for level  
               if (s1IP(i).lt.-1000.or.s1IP(i).gt.1000) then
                write(*,*) ' wrong value of s1ip in n,m', n,m
                !pause
                stop    
               endif          
            else !the matrix is singular at machine precision just compute the average of the fluid cell points
               write(*,*) 'Matrix is singular, rcond = ',rcond
               var(nGP,mGP) = (B(1)+B(4))*0.5_fp               
            endif                    
!
         elseif (contDRYnoGH.ge.1) then  !if multiple dry no ghost cells choose the one that has a vertical or horizontal adjacent wet
!
            k2 = kOK(1)  !! I put the BCs on the first  and second row so the diagonal term is not going to be zero (for zero-velocity prescribed) 
            k3 = kOK(2)  !! I put the BCs on the first  and second row so the diagonal term is not going to be zero (for zero-velocity prescribed) 
!     
            do kk=1,contDRYnoGH  
               Kdry = kDRYnoGH(kk)   
               do kkk=1,2 !cycle of the fluid cell
                  kfl = kOK(kkk)
                  if (nL(Kdry)==nL(kfl)) then !lay on the same vertical => normal is vertical
                     k1 = Kdry
                     nDRY = nL(k1)
                     mDRY = mL(k1)
                     signREAL = real(sign(1,mL(kfl)-mDRY),fp)
                     nxDRY1 = signREAL*PSIx(2,2) !Normal that points toward the fluid cell
                     nyDRY1 = signREAL*PSIy(2,2) !Normal that points toward the fluid cell
                     exit
                  elseif (mL(Kdry)==mL(kfl)) then !lays on the same horizontal => normal is horizontal    
                     k1 = Kdry
                     nDRY = nL(k1)
                     mDRY = mL(k1)
                     signREAL = real(sign(1,nL(kfl)-nDRY),fp)
                     nxDRY1 = signREAL*ETAx(2,2) !Normal that points toward the fluid cell
                     nyDRY1 = signREAL*ETAy(2,2) !Normal that points toward the fluid cell
                     exit
                  endif  
               enddo    
            enddo          
            A3(1,1) = nxDRY1 !minus since the normal goes toward the dry cell 
            A3(2,1) = xG(nL(k2),mL(k2)) 
            A3(3,1) = xG(nL(k3),mL(k3)) 
            A3(1,2) = nyDRY1  !minus since the normal goes toward the dry cell
            A3(2,2) = yG(nL(k2),mL(k2))  
            A3(3,2) = yG(nL(k3),mL(k3))  
            A3(1,3) = 0._fp
            A3(2,3) = 1._fp
            A3(3,3) = 1._fp
            B3(1) = 0._fp
            B3(2) = var(nL(k2),mL(k2))
            B3(3) = var(nL(k3),mL(k3))
! 
            CALL DGETRF( 3, 3, A3, 3, IPIV, INFO ) !compute LU
            !compute 1-norm needed for condition number
            anorm = 0.d0
            do j=1,3
               colsum = 0.d0
               do k=1,3
                  colsum = colsum + abs(A3(k,j))
               enddo
               anorm = max(anorm, colsum)
            enddo
            norm = '1'  ! use 1-norm to compute condition number with dgecon
            CALL DGECON(norm,3,A3,3,anorm,rcond,work3,iwork3,info) !http://www.mathworks.com/support/solutions/en/data/1-3KL67Y/?product=SL&solution=1-3KL67Y
            if (rcond.gt.0.000000000000001_fp) then ! 
               CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
               CALL DGETRS('N', 3, 1, A3, 3, IPIV, B3, 3, INFO ) !solve linear system
               CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)   
               s1IP(i) = B3(1)*xIPs1(i)+B3(2)*yIPs1(i)+B3(3) 
               var(nGP,mGP) = s1IP(i) ! Neumann condition for level  
               if (s1IP(i).lt.-1000.or.s1IP(i).gt.1000) then
                write(*,*) ' wrong value of s1ip in n,m', n,m
                !pause
                stop    
               endif          
            else !the matrix is singular at machine precision just compute the average of the fluid cell points
               write(*,*) 'Matrix is singular, rcond = ',rcond
               var(nGP,mGP) = (B(1)+B(4))*0.5_fp               
            endif        
         else 
            WRITE(515151,*) 'Error for interpolation: only two fluid cell and zero ghosts/boundary points. BI in cell',mBI,nBI
            WRITE(*,*) 'Error for interpolation: only two fluid cell and zero ghosts/boundary points. BI in cell',mBI,nBI
            !pause
            stop
         endif            
!             
      ELSEIF (contFLUID==1) then      
!         one of the  four corner nodes of the interpolation cell are on the fluid 
!         
         if (contGHOST.ge.2) then !even if its 3 I just use the first 2 (rare case, see CASEwith3GHOSTSand1FLUIDpoint.bmp mine)
            k1 = kGHOS(1)
            k2 = kGHOS(2)    
            k3 = kOK(1)                       
            !first ghost (note iG1 and iG2 can in general be different from i)
            iG1  = FROMmnTOghostS1(nL(k1),mL(k1)) 
            mBI1 = mBIs1(iG1)
            nBI1 = nBIs1(iG1)
            nxDRY1 = nxG_S1(iG1) !- Nx(nBI1,mBI1)
            nyDRY1 = nyG_S1(iG1) !- Ny(nBI1,mBI1)
            !second ghost
            iG2 = FROMmnTOghostS1(nL(k2),mL(k2)) 
            mBI2 = mBIs1(iG2)
            nBI2 = nBIs1(iG2) 
            nxDRY2 = nxG_S1(iG2) !- Nx(nBI2,mBI2)
            nyDRY2 = nyG_S1(iG2) !- Ny(nBI2,mBI2)
         elseif(contGHOST.eq.1.and.contDRYnoGH.ge.1) then  
           
         !first ghost (note iG1 and iG2 can in general be different from i)
            k1 = kGHOS(1) 
            iG1  = FROMmnTOghostS1(nL(k1),mL(k1)) 
            mBI1 = mBIs1(iG1)
            nBI1 = nBIs1(iG1)
            nxDRY1 = nxG_S1(iG1) !- Nx(nBI1,mBI1)
            nyDRY1 = nyG_S1(iG1) !- Ny(nBI1,mBI1)

            k3 = kOK(1)  !! I put the BCs on the first  and second row so the diagonal term is not going to be zero (for zero-velocity prescribed)  
            !look for the second dry point (its a non-ghost dry point)
!             if (contDRYnoGH.gt.1) then  !if multiple dry no ghost cells choose the one that has a vertical or horizontal adjacent wet
            do kk=1,contDRYnoGH  
               Kdry = kDRYnoGH(kk)   
               if (nL(Kdry)==nL(k3)) then !lay on the same vertical => normal is vertical
                  k2 = Kdry
                  nDRY = nL(K2)
                  mDRY = mL(K2)
                  signREAL = real(sign(1,mL(k3)-mDRY),fp)
                  nxDRY2 = signREAL*PSIx(2,2) !Normal that points toward the fluid cell
                  nyDRY2 = signREAL*PSIy(2,2) !Normal that points toward the fluid cell
                  exit
               elseif (mL(Kdry)==mL(k3)) then !lays on the same horizontal => normal is horizontal    
                  k2 = Kdry
                  nDRY = nL(K2)
                  mDRY = mL(K2)
                  signREAL = real(sign(1,nL(k3)-nDRY),fp)
                  nxDRY2 = signREAL*ETAx(2,2) !Normal that points toward the fluid cell
                  nyDRY2 = signREAL*ETAy(2,2) !Normal that points toward the fluid cell
                  exit
               endif      
            enddo          
         else 
            WRITE(515151,*) 'Error for interpolation for s1: only one fluid cell and less then two ghosts/boundary points. BI in cell',mBI,nBI
            WRITE(*,*) 'Error for interpolation for s1: only one fluid cell and less then two ghosts/boundary points. BI in cell',mBI,nBI
            !pause
            stop
         endif            
!
         A3(1,1) = nxDRY1  
         A3(2,1) = nxDRY2
         A3(3,1) = xG(nL(k3),mL(k3)) 
         A3(1,2) = nyDRY1   
         A3(2,2) = nyDRY2
         A3(3,2) = yG(nL(k3),mL(k3))  
         A3(1,3) = 0._fp
         A3(2,3) = 0._fp
         A3(3,3) = 1._fp
         B3(1) = 0._fp
         B3(2) = 0._fp
         B3(3) = var(nL(k3),mL(k3))
! 
         CALL DGETRF( 3, 3, A3, 3, IPIV, INFO ) !compute LU
         CALL ERROR_DGETRF(INFO,contFLUID,mBI,nBI)   
         CALL DGETRS('N', 3, 1, A3, 3, IPIV, B3, 3, INFO ) !solve linear system
         CALL ERROR_DGETRS(INFO,contFLUID,mBI,nBI)   
         s1IP(i) = B3(1)*xIPs1(i)+B3(2)*yIPs1(i)+B3(3) 
       !  write(*,*) 'interpolato',  s1IP(i) 
         if (s1IP(i).lt.-1000.or.s1IP(i).gt.1000) then
             write(*,*) ' wrong value of s1ip in n,m', n,m
             ! pause
             stop   
         endif
         var(nGP,mGP) = s1IP(i) ! Neumann condition for level      
!
      ELSE
         WRITE(515151,*) 'Error for interpolation: NO fluid cells found for s1. BI in cell',mBI,nBI,i
         WRITE(*,*) 'Error for interpolation: NO fluid cells found for s1.  BI in cell',mBI,nBI,i
         !pause
         stop
      ENDIF     
!       
 !      I store back in s1 (i.e. var) the original water surfaces since I modified them 
       DO K =contFLUID-contFLUIDdryNNveg+1,contFLUID
          kk = kOK(K) 
          var(nL(KK),mL(KK)) = VARstorage(K)  
       ENDDO
!
   enddo    
!
RETURN
END
!
! 
