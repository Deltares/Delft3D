subroutine interpG_ATu1LOCATION_Hart(icx,icy,u1,ghostu1,totGHOSTu1,Nx,Ny,xG_U1,yG_U1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddb,MAXaguu, gdp)
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
!   Function:   Interpolate the values at the ghost points by using the simple 
!               Hartmann approx along Cartesian lines. No image points needed
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
    integer, dimension(:)   , pointer :: mGPu1
    integer, dimension(:)   , pointer :: nGPu1
    integer, dimension(:)   , pointer :: mGPv1
    integer, dimension(:)   , pointer :: nGPv1
    integer                 , pointer :: typeHART
    real(fp), dimension(:,:), pointer :: aguu
    real(fp), dimension(:,:), pointer :: agvv
    real(fp)                , pointer :: DISSghost
!
! global variables
!
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout) :: u1
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: Nx
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: Ny
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: xG_U1
    real(fp)                                                            , intent(in)    :: MAXaguu !max value of aguu for which I prescribe ghost cells
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: yG_U1
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
  integer                    :: MAXad
  integer                    :: kMIN
  integer                    :: nm
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
  integer                    :: nmG
  integer                    :: ddb
  integer                    :: nL(4)
  integer                    :: mL(4)
  integer                    :: contFLUID
  integer                    :: contGHOST
  integer                    :: contDRYnoGH  
  integer                    :: contNNghostWD 
  real(fp)                   :: velK(1:kmax,4)
  real(fp)                   :: vel(1:kmax)
  real(fp)                   :: angle
  real(fp)                   :: minANGLE
  real(fp)                   :: normX
  real(fp)                   :: normY
  real(fp)                   :: dx
  real(fp)                   :: dy
  real(fp)                   :: ABSangle
  real(fp)                   :: maxANGLE
  real(fp)                   :: aguuLOC
!
! executable statements -------------------------------------------------------
!  
    mGPu1     => gdp%gdimbound%mGPu1
    nGPu1     => gdp%gdimbound%nGPu1
    mGPv1     => gdp%gdimbound%mGPv1
    nGPv1     => gdp%gdimbound%nGPv1
    typeHART  => gdp%gdimbound%typeHART
    aguu      => gdp%gdimbound%aguu
    agvv      => gdp%gdimbound%agvv
    DISSghost => gdp%gdimbound%DISSghost
!
!  extrapolate at the boundary, in order to have some sort of tranmissive behaviour
!
   icxy   = max(icx, icy)    
!
   do i = 1,totGHOSTu1       
!       
      if (icy==1) then  
         icxOK = icx
         icyOK = icy
         mGP = mGPu1(i)
         nGP = nGPu1(i)
         aguuLOC = aguu(nGP,mGP)
      else
         icxOK = icy
         icyOK = icx
         mGP = mGPv1(i)
         nGP = nGPv1(i)
         aguuLOC = agvv(nGP,mGP)
      endif
      if  (comparereal(aguuLOC,MAXaguu).gt.0) cycle
      nmG = (nGP   + ddb)  *icyOK + (mGP   + ddb)  *icxOK - icxy 
!
      ! lower  (it is always in the direction of the vel vector)
      nmK(1) =   nmG-icy
      ! right  it is always perpendicular to the direction of the vel vector)
      nmK(2) =   nmG+icx 
      ! upper 
      nmK(3) =   nmG+icy 
      ! left  
      nmK(4) =   nmG-icx 
!
      contFLUID = 0
      contGHOST = 0
      contDRYnoGH = 0
      contNNghostWD = 0
!
      Do K=1,4
        nm = nmK(K)
        IF (GHOSTu1(nm) == 0) then !fluid cell ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
           contFLUID = contFLUID + 1
           kOK(contFLUID) = K
           velK(1:kmax,contFLUID) = u1(nm,1:kmax)
        ELSEIF (GHOSTu1(nm) == 1) then ! dry ghost cell ! note here n is increased by one because the location of xcorV1(n,m) is the location of xG_u1(n+1,m)
           contGHOST = contGHOST + 1
           kGHOS(contGHOST) = K
        ELSEIF (GHOSTu1(nm) == 2) then ! dry NOT ghost cell 
           contDRYnoGH = contDRYnoGH + 1
           kDRYnoGH(contDRYnoGH) = K
        ELSEIF (GHOSTu1(nm) >= 3) then ! bank/wet channel interface or orthogonal to it
           contNNghostWD = contNNghostWD + 1
           kNNG(contNNghostWD) = K
        endif
     ENDDO   
!
     vel = 0._fp
     IF (contFLUID==4) then 
        !rare case in which a small island is present  
        !vel stays zero
     ELSEIF ((contFLUID==3).or.(contFLUID ==2)) then 
        SELECT CASE(typeHART)
        CASE(1) !average of neighbours !best result
           vel = 0._fp
           do k=1,contFLUID
              vel(:) = vel(:) + velK(:,k)
           enddo
           vel(:) = vel(:)/contFLUID
        CASE(2) !closest to the direction of the normal to the bank
        !not the best result. If small cut with no upwind poins, it only has cross advection that is sent close to zero cause of hertmann laterally
           minANGLE = 9999999999999._fp
           if (kfs_cc(nmG)==0) then
              Normx = - nx(nmG) !minus => toward water !WRONG!!! I SHOULD USE nxG_U1 AND nyG_U1. To simplify, for Hartmenn I can do average of normals on 9 cell stencil so I can remove find_BI_PI
              Normy = - ny(nmG) !minus => toward water !WRONG!!! I SHOULD USE nxG_U1 AND nyG_U1. To simplify, for Hartmenn I can do average of normals on 9 cell stencil so I can remove find_BI_PI
           else
              Normx = - nx(nmG+icx) !minus => toward water !WRONG!!! I SHOULD USE nxG_U1 AND nyG_U1. To simplify, for Hartmenn I can do average of normals on 9 cell stencil so I can remove find_BI_PI
              Normy = - ny(nmG+icx) !minus => toward water!WRONG!!! I SHOULD USE nxG_U1 AND nyG_U1. To simplify, for Hartmenn I can do average of normals on 9 cell stencil so I can remove find_BI_PI
           endif
           do k=1,contFLUID
              nm = nmK(kOK(k))
              dx = xG_U1(nm)-xG_U1(nmG)
              dy = yG_U1(nm)-yG_U1(nmG)
              angle = atan2(dx*Normy-dy*Normx,dx*Normx+dy*Normy) 
              ABSangle = abs(angle) 
              if (ABSangle.lt.minANGLE) then  
                 minANGLE = ABSangle
                 kMIN = k
              endif
           enddo
           vel(:) = velK(:,kMIN)
           if (minANGLE>99999999._fp) then
              WRITE(*,*) 'minANGLE too large something is wrong in Hartmann'
              call d3stop(1, gdp)
           endif
        CASE(3) !FARTHEST from the direction of the normal to the bank. Terrible results
           maxANGLE = 0._fp
           if (kfs_cc(nmG)==0) then
              Normx = - nx(nmG) !minus => toward water !WRONG!!! I SHOULD USE nxG_U1 AND nyG_U1. To simplify, for Hartmenn I can do average of normals on 9 cell stencil so I can remove find_BI_PI
              Normy = - ny(nmG) !minus => toward water !WRONG!!! I SHOULD USE nxG_U1 AND nyG_U1. To simplify, for Hartmenn I can do average of normals on 9 cell stencil so I can remove find_BI_PI
           else
              Normx = - nx(nmG+icx) !minus => toward water !WRONG!!! I SHOULD USE nxG_U1 AND nyG_U1. To simplify, for Hartmenn I can do average of normals on 9 cell stencil so I can remove find_BI_PI
              Normy = - ny(nmG+icx) !minus => toward water !WRONG!!! I SHOULD USE nxG_U1 AND nyG_U1. To simplify, for Hartmenn I can do average of normals on 9 cell stencil so I can remove find_BI_PI
           endif
           do k=1,contFLUID
              nm = nmK(kOK(k))
              dx = xG_U1(nm)-xG_U1(nmG)
              dy = yG_U1(nm)-yG_U1(nmG)
              angle = atan2(dx*Normy-dy*Normx,dx*Normx+dy*Normy) 
              ABSangle = abs(angle) 
              if (ABSangle.gt.maxANGLE) then  
                 maxANGLE = ABSangle
                 MAXad = k
              endif
           enddo
           vel(:) = velK(:,MAXad)
!
        CASE DEFAULT 
           WRITE(*,*) 'typeHART not possible'
           call d3stop(1, gdp)
        END SELECT
     ELSEIF (contFLUID ==1) THEN 
!
        vel(:) = velK(:,1)
!
     ENDIF
     u1(nmG,:) = vel(:)*DISSghost  
!
   enddo
!
RETURN
END
!
! 
