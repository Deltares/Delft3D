subroutine drotim(nst       ,j         ,nmmaxj    ,kmax      ,ndro      , &
                & icx       ,icy       ,windxt    ,windyt    ,windft    , &
                & kcu       ,kcv       ,kcs       ,kfu       ,kfv       , &
                & mndro     ,itdro     ,u1        ,v1        ,xcor      , &
                & ycor      ,guu       ,gvv       ,guv       ,gvu       , &
                & dxydro    ,xydro     ,hu        ,hv        ,s1        , &
                & dpu       ,dpv       ,thick     ,drodep    , &
                & kfumin    ,kfumax    ,kfvmin    ,kfvmax    , &
                & dzu1      ,dzv1      ,zk        ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: - calculates drogues if release time < NST
!                if stop time > NST and drogue on active point
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)     , pointer :: hdt
!
! Global variables
!
    integer                                               :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                    !!  then computation proceeds in the X-
                                                                    !!  dir. If icx=1 then computation pro-
                                                                    !!  ceeds in the Y-dir.
    integer                                               :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                               :: j      !!  Begin pointer for arrays which have
                                                                    !!  been transformed into 1D arrays.
                                                                    !!  Due to the shift in the 2nd (M-)
                                                                    !!  index, J = -2*NMAX + 1
    integer                                               :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                 , intent(in)  :: ndro   !  Description and declaration in dimens.igs
    integer                                               :: nmmaxj !  Description and declaration in dimens.igs
    integer                                 , intent(in)  :: nst    !!  Time step number
    integer, dimension(2, ndro)             , intent(in)  :: itdro  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(2, ndro)                           :: mndro  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfvmin !  Description and declaration in esm_alloc_int.f90
    real(fp)                                              :: windft !  Description and declaration in trisol.igs
    real(fp)                                              :: windxt !  Description and declaration in trisol.igs
    real(fp)                                              :: windyt !  Description and declaration in trisol.igs
    real(fp), dimension(2, ndro)                          :: dxydro !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(2, ndro)                          :: xydro  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: dpv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: dzv1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                             :: thick  ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)             , intent(in)  :: zk
    real(fp), dimension(ndro)                             :: drodep
!
! Local variables
!
    integer  :: ddb
    integer  :: i     ! Hulp var. 
    integer  :: icxy  ! Max (ICX  ,ICY ) 
    integer  :: mf    ! M-coordinate at start of calculation and after calculation (-><- Cline) 
    integer  :: nf    ! N coordinate at start of calculation and after calculation (-><- Cline) 
    integer  :: nm    ! N,M array index 
    real(fp) :: dxf   ! Delta X at start of calculation and after calculation (-><- Cline) 
    real(fp) :: dyf   ! Delta Y at start of calculation and after calculation (-><- Cline) 
    real(fp) :: xf    ! X coordinate at start of calculation and after calculation (-><- Cline) 
    real(fp) :: yf    ! Y coordinet at start of calculation and after calculation (-><- Cline) 
!
!! executable statements -------------------------------------------------------
!
    hdt     => gdp%gdnumeco%hdt
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ! calculate starting point of streamlines
    !
    do i = 1, ndro
       nm = (mndro(2, i) + ddb)*icy + (mndro(1, i) + ddb)*icx - icxy
       !
       ! drogue only calculated if a) drogue released, and still
       !                              to be calculated
       !                           b) drogue on active point
       ! not for itdro(2,i) = nst (info that is calculated in step
       ! nst will be written to file as coordinates for nst+1)
       !
       if (itdro(1, i)<=nst .and. itdro(2, i)>nst .and. kcs(nm)==1) then
          mf  = mndro(1, i)
          nf  = mndro(2, i)
          dxf = dxydro(1, i)
          dyf = dxydro(2, i)
          xf  = xydro(1, i)
          yf  = xydro(2, i)
          !
          ! calculate composed line
          !
          call cline(hdt       ,j         ,nmmaxj    ,kmax      ,icx       , &
                   & icy       ,icxy      ,mf        ,nf        ,dxf       , &
                   & dyf       ,xf        ,yf        ,u1        ,v1        , &
                   & xcor      ,ycor      ,guu       ,gvv       ,guv       , &
                   & gvu       ,kcu       ,kcv       ,kcs       ,kfu       , &
                   & kfv       ,windxt    ,windyt    ,windft    , &
                   & s1        ,dpu       ,dpv       ,thick     ,drodep(i) , &
                   & kfumin    ,kfumax    ,kfvmin    ,kfvmax    , &
                   & dzu1      ,dzv1      ,zk        ,gdp       )
          !
          ! rewrite n,m and dx,dy and write x,y point of drogue
          ! in arrays
          !
          mndro (1, i) = mf
          mndro (2, i) = nf
          dxydro(1, i) = dxf
          dxydro(2, i) = dyf
          xydro (1, i) = xf
          xydro (2, i) = yf
       endif
    enddo
end subroutine drotim
