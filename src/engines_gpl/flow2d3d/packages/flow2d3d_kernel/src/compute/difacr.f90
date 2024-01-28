subroutine difacr(icx       ,icy       ,j         ,nmmaxj    ,nmmax     , &
                & kmax      ,lstsci    ,lsal      ,ltem      ,kcs       , &
                & kfu       ,kfv       ,kadu      ,kadv      ,s0        , &
                & dps       ,r0        ,ddkl      ,guu       ,gvv       , &
                & guv       ,gvu       ,thick     ,sig       ,dicuv     , &
                & sigdif    ,dsdksi    ,dtdksi    ,dsdeta    ,dtdeta    , &
                & gdp       )
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
!    Function: Computes horizontal diffusion along Z-planes.
!              Explicit in u-direction, explicit in v-
!              direction.
!              Horizontal gradients salinity and temperature
!              are computed in subroutine dengra.
!              Only if KMAX > 1 and Anti Creep
! Method used: Reference : On the approximation of horizontal
!              gradients in sigma co-ordinates for bathymetry
!              with steep bottom slopes (G.S. Stelling and J.
!              van Kester - International Journal for Methods
!              in Fluids, Vol. 18 1994)
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
!
! Global variables
!
    integer                                     , intent(in)  :: icx    !!  Increment in the x-dir., if icx= nmax
                                                                        !!  then computation proceeds in the x-
                                                                        !!  dir. if icx=1 then computation pro-
                                                                        !!  ceeds in the y-dir.
    integer                                     , intent(in)  :: icy    !!  Increment in the y-dir. (see icx)
    integer                                                   :: j      !!  Begin pointer for arrays which have
                                                                        !!  been transformed into 1d arrays.
                                                                        !!  due to the shift in the 2nd (m-)
                                                                        !!  index, j = -2*nmax + 1
    integer                                                   :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                   :: lsal   !  Description and declaration in dimens.igs
    integer                                                   :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                   :: ltem   !  Description and declaration in dimens.igs
    integer                                     , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                   :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)           :: kadu   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)           :: kadv   !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)            :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dsdeta !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dsdksi !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dtdeta !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dtdksi !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)      :: ddkl   !!  Internal work array, diagonal space
                                                                        !!  at (n,m,k,l)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)      :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                     :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                     :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsci)                                   :: sigdif !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: k
    integer :: nm
    integer :: nmu
    integer :: num
    real(fp):: dpnm
    real(fp):: dpnmu
    real(fp):: dpnum
    real(fp):: sepnm
    real(fp):: sepnmu
    real(fp):: sepnum
!
!! executable statements -------------------------------------------------------
!
    ! horizontal diffusion in both u- and v-diffusion
    ! artificial creep is avoided by use
    ! of a special limiter
    !
    ! calibration for SAL / TEMP in routine DENGRA
    !
    if (lsal/=0) then
       do nm = 1, nmmax
          do k = 1, kmax
             ddkl(nm, k, lsal) = ddkl(nm, k, lsal) + dsdksi(nm, k)              &
                               & + dsdeta(nm, k)
          enddo
       enddo
    endif
    if (ltem/=0) then
       do nm = 1, nmmax
          do k = 1, kmax
             ddkl(nm, k, ltem) = ddkl(nm, k, ltem) + dtdksi(nm, k)              &
                               & + dtdeta(nm, k)
          enddo
       enddo
    endif
    !
    if (lstsci==max(lsal, ltem)) goto 1000
    !
    ! calibration for all other constituents
    ! horizontal diffusion in u-diffusion
    ! artificial creep is avoided by use
    ! of a special limiter
    !
    nmu = icx
    do nm = 1, nmmax
       nmu = nmu + 1
       if (kfu(nm)/=0) then
          sepnm  = s0(nm)
          sepnmu = s0(nmu)
          dpnm   = real(dps(nm),fp)
          dpnmu  = real(dps(nmu),fp)
          call difhor(nm        ,nmu       ,j         ,nmmaxj    ,kmax      , &
                    & lstsci    ,lsal      ,ltem      ,kcs       ,kadu      , &
                    & sepnm     ,sepnmu    ,dpnm      ,dpnmu     ,guu       , &
                    & gvu       ,r0        ,ddkl      ,thick     ,sig       , &
                    & dicuv     ,sigdif    ,gdp       )
       endif
    enddo
    !
    ! calibration for all other constituents
    ! horizontal diffusion in v-direction
    ! artificial creep is avoided by use
    ! of a special limiter
    !
    num = icy
    do nm = 1, nmmax
       num = num + 1
       if (kfv(nm)/=0) then
          sepnm  = s0(nm)
          sepnum = s0(num)
          dpnm   = real(dps(nm),fp)
          dpnum  = real(dps(num),fp)
          call difhor(nm        ,num       ,j         ,nmmaxj    ,kmax      , &
                    & lstsci    ,lsal      ,ltem      ,kcs       ,kadv      , &
                    & sepnm     ,sepnum    ,dpnm      ,dpnum     ,gvv       , &
                    & guv       ,r0        ,ddkl      ,thick     ,sig       , &
                    & dicuv     ,sigdif    ,gdp       )
       endif
    enddo
 1000 continue
end subroutine difacr
