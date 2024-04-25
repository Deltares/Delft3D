subroutine z_difhor_nhfull(j         ,nmmaxj    ,kmax      ,lstsci    ,nmmax     , &
                  & icx       ,icy       ,kcs       ,kfuz1     ,kfvz1     , &
                  & kfsz1     ,kfumin    ,kfumx0    ,kfvmin    ,kfvmx0    , &
                  & dicuv     ,sigdif    ,dzu0      ,dzv0      ,areau     , &
                  & areav     ,guv       ,gvu       ,r0        ,ddkl      , &
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
!    Function: Computes horizontal diffusion. Explicit in u-direction,
!              explicit in v-direction.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow2d3d_timers
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
    integer                                                 , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                 , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                               :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer                                                 , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                               :: nmmaxj !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfvmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfvmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: kfsz1  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: areau  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: areav  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)      , intent(in)  :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzv0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: ddkl   !! Internal work array, diagonal space at (N,M,K,L)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsci)                             , intent(in)  :: sigdif !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: k
    integer :: kenu
    integer :: kenv
    integer :: l
    integer :: nm
    integer :: nmu
    integer :: num
    real(fp):: cl
    real(fp):: cr
    real(fp):: difl
    real(fp):: difr
    real(fp):: flux
!
!! executable statements -------------------------------------------------------
!
    ! HORIZONTAL DIFFUSION IN U-DIFFUSION
    !
    do l = 1, lstsci
       !
       ! CONTRIBUTION TO VOLUME NM
       !
       do nm = 1, nmmax
          nmu = nm + icx
          do k = kfumin(nm), kfumx0(nm)
             if (kfuz1(nm,k)==1 .and. kfsz1(nm,k)*kfsz1(nmu,k)/=0) then
                cl            = r0(nm,k,l)
                difl          = dicuv(nm,k)
                cr            = r0(nmu,k,l)
                difr          = dicuv(nmu,k)
                flux          = 0.5_fp * (cr-cl) * (difl+difr) / (sigdif(l)*gvu(nm))
                kenu          = max(0 , 2-kcs(nmu))
                ddkl(nm ,k,l) = ddkl(nm ,k,l) + areau(nm,k)*flux*kenu
                kenu          = max(0 , 2-kcs(nm))
                ddkl(nmu,k,l) = ddkl(nmu,k,l) - areau(nm,k)*flux*kenu
             endif
          enddo
       enddo
    enddo
    !
    ! HORIZONTAL DIFFUSION IN V-DIRECTION
    !
    do l = 1, lstsci
       !
       ! LOOPS ARE SPLIT TO AVOID INTERNAL RECURRENCE WHICH
       ! OBSTRUCTS VECTORISATION
       !
       ! CONTRIBUTION TO VOLUME NM
       !
       do nm = 1, nmmax
          num = nm + icy
          do k = kfvmin(nm), kfvmx0(nm)
             if (kfvz1(nm,k)==1 .and. kfsz1(nm,k)*kfsz1(num,k)/=0) then
                cl            = r0(nm,k,l)
                difl          = dicuv(nm,k)
                cr            = r0(num,k,l)
                difr          = dicuv(num,k)
                flux          = 0.5_fp * (cr-cl) * (difl+difr) / (sigdif(l)*guv(nm))
                kenv          = max(0 , 2-kcs(num))
                ddkl(nm ,k,l) = ddkl(nm ,k,l) + areav(nm,k)*flux*kenv
                kenv          = max(0, 2 - kcs(nm))
                ddkl(num,k,l) = ddkl(num,k,l) - areav(nm,k)*flux*kenv
             endif
          enddo
       enddo
    enddo
end subroutine z_difhor_nhfull
