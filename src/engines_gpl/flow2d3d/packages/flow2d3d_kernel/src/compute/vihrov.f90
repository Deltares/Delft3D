  subroutine vihrov(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                & icy       ,kcs       ,kfu       ,kfv       ,kfs       , &
                & u0        ,v         ,vicuv     ,vnu2d     ,guu       , &
                & gvv       ,gvu       ,ddk       ,rxx       ,rxy       , &
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
!    Function:
! Method used: - roughness (partial slip) of rigid walls
!              - blockage flow by rigid sheets
!              - 2D turbulence model at depth points
!              Special approximation pressure term, based
!              on limiter to avoid artificial flow.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: z0
    real(fp)               , pointer :: z0v
    real(fp)               , pointer :: vonkar
    integer                , pointer :: iro
    integer                , pointer :: irov
    real(fp)               , pointer :: hdt
!
! Global variables
!
    integer                                     , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                        !!  then computation proceeds in the X-
                                                                        !!  dir. If icx=1 then computation pro-
                                                                        !!  ceeds in the Y-dir.
    integer                                     , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                   :: j      !!  Begin pointer for arrays which have
                                                                        !!  been transformed into 1D arrays.
                                                                        !!  Due to the shift in the 2nd (M-)
                                                                        !!  index, J = -2*NMAX + 1
    integer                                     , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                   :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: vnu2d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: ddk    !!  Internal work array, diagonal space
                                                                        !!  at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: rxx    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: rxy    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: v      !!  V-velocities at new time level
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)            :: vicuv  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: k      ! Loop variable 
    integer :: ki     ! Mask value 
    integer :: ndm    ! NM - ICY 
    integer :: ndmd   ! NM - ICX - ICY 
    integer :: nm     ! Loop variable 
    integer :: nmd    ! NM - ICX 
    integer :: nmu    ! NM + ICX 
    integer :: num    ! NM + ICY 
    integer :: numu   ! NM + ICX + ICY 
    real(fp):: dx     ! Distance in X dir. in depth point 
    real(fp):: dy     ! Distance in Y dir. in depth point 
    real(fp):: s
    real(fp):: vi     ! Viscosity in U-point 
    real(fp):: vicmax
!
!! executable statements -------------------------------------------------------
!
    z0       => gdp%gdphysco%z0
    z0v      => gdp%gdphysco%z0v
    vonkar   => gdp%gdphysco%vonkar
    iro      => gdp%gdphysco%iro
    irov     => gdp%gdphysco%irov
    hdt      => gdp%gdnumeco%hdt
    !
    ! Upper bound for VICUV in coupling points is removed. See chkvic.
    !
    do nm = 1, nmmax
       do k = 1, kmax
          rxy(nm, k) = 0.0
          rxx(nm, k) = 0.0
       enddo
    enddo
    !
    ! roughness rigid walls (partial slip IROV = 1 and Z0V <> 0.)
    !
    if (irov==1) then
       do nm = 1, nmmax
          num = nm + icy
          !
          ! rxy in depth point
          !
          if (kfu(nm)==1 .or. kfu(num)==1) then
             nmu = nm + icx
             nmd = nm - icx
             ndm = nm - icy
             numu = nm + icx + icy
             ki = kfu(num) + 2*kfv(nmu) + 4*kfu(nm) + 8*kfv(nm)
             !
             ! rxy no walls
             !
             if (ki==15) then
                dy = 0.5*(guu(nm) + guu(num))
                dx = 0.5*(gvv(nm) + gvv(nmu))
                do k = 1, kmax
                   vi = 0.25*(vicuv(nm, k) + vicuv(nmu, k) + vicuv(num, k)      &
                      & + vicuv(numu, k)) + vnu2d(nm)
                   rxy(nm, k) = vi*((u0(num, k) - u0(nm, k))/dy + (v(nmu, k) - v&
                              & (nm, k))/dx)
                enddo
             elseif (ki==12 .or. ki==6 .or. ki==4) then
                !
                ! rxy for u(nm); rough wall, flow below wall
                !
                do k = 1, kmax
                   s = vonkar*u0(nm, k)/log((0.5*guu(nm) + z0v)/z0v)
                   rxy(nm, k) = -s*abs(s)
                enddo
             elseif (ki==9 .or. ki==3 .or. ki==1) then
                !
                ! rxy for u(num); rough wall, flow above wall
                !
                do k = 1, kmax
                   s = vonkar*u0(num, k)/log((0.5*guu(num) + z0v)/z0v)
                   rxy(nm, k) = s*abs(s)
                enddo
             else
             endif
          endif
       enddo
    !
    ! roughness rigid walls (no slip IROV = 2 and Z0V = 0.)
    !
    elseif (irov==2) then
       do nm = 1, nmmax
          num = nm + icy
          !
          ! rxy in depth point
          !
          if (kfu(nm)==1 .or. kfu(num)==1) then
             nmu = nm + icx
             nmd = nm - icx
             ndm = nm - icy
             numu = nm + icx + icy
             ki = kfu(num) + 2*kfv(nmu) + 4*kfu(nm) + 8*kfv(nm)
             !
             ! rxy no walls
             !
             if (ki==15) then
                dy = 0.5*(guu(nm) + guu(num))
                dx = 0.5*(gvv(nm) + gvv(nmu))
                do k = 1, kmax
                   vi = 0.25*(vicuv(nm, k) + vicuv(nmu, k) + vicuv(num, k)      &
                      & + vicuv(numu, k)) + vnu2d(nm)
                   rxy(nm, k) = vi*((u0(num, k) - u0(nm, k))/dy + (v(nmu, k) - v&
                              & (nm, k))/dx)
                enddo
             elseif (ki==12 .or. ki==6 .or. ki==4) then
                !
                ! rxy for u(nm); rough wall, flow below wall
                !
                dy = guu(nm)
                do k = 1, kmax
                   vi = 0.5*(vicuv(nm, k) + vicuv(nmu, k))                      &
                      & + 0.5*(vnu2d(nm) + vnu2d(ndm))
                   rxy(nm, k) = vi*( - u0(nm, k))/dy/2
                enddo
             elseif (ki==9 .or. ki==3 .or. ki==1) then
                !
                ! rxy for u(num); rough wall, flow above wall
                !
                dy = guu(num)
                do k = 1, kmax
                   vi = 0.5*(vicuv(num, k) + vicuv(numu, k))                    &
                      & + 0.5*(vnu2d(nm) + vnu2d(num))
                   rxy(nm, k) = vi*(u0(num, k))/dy/2
                enddo
             else
             endif
          endif
       enddo
    !
    ! roughness rigid walls (free slip IROV = 3 and Z0V = 0.)
    !
    elseif (irov==3) then
       do nm = 1, nmmax
          num = nm + icy
          !
          ! rxy in depth point
          !
          if (kfu(nm)==1 .or. kfu(num)==1) then
             nmu = nm + icx
             nmd = nm - icx
             ndm = nm - icy
             numu = nm + icx + icy
             ki = kfu(num) + 2*kfv(nmu) + 4*kfu(nm) + 8*kfv(nm)
             !
             ! rxy no walls
             !
             if (ki==15) then
                dy = 0.5*(guu(nm) + guu(num))
                dx = 0.5*(gvv(nm) + gvv(nmu))
                do k = 1, kmax
                   vi = 0.25*(vicuv(nm, k) + vicuv(nmu, k) + vicuv(num, k)      &
                      & + vicuv(numu, k)) + vnu2d(nm)
                   rxy(nm, k) = vi*((u0(num, k) - u0(nm, k))/dy + (v(nmu, k) - v&
                              & (nm, k))/dx)
                enddo
             else
                do k = 1, kmax
                   rxy(nm, k) = 0.
                enddo
             endif
          endif
       enddo
    else
    endif
    !
    ! rxx in zeta point (active point, on open boundary rxx = 0.)
    !
    do nm = 1, nmmax
       if ( (kcs(nm)==1 .or. kcs(nm)==3) .and. kfs(nm)==1 ) then
          ndm = nm - icy
          nmd = nm - icx
          ndmd = nm - icx - icy
          dx = 0.5*(gvv(nm) + gvv(ndm))
          do k = 1, kmax
             vi = vicuv(nm, k) + 0.25*(vnu2d(nm) + vnu2d(nmd) + vnu2d(ndm)      &
                & + vnu2d(ndmd))
             rxx(nm, k) = 2*vi*(u0(nm, k) - u0(nmd, k))/dx
          enddo
       endif
    enddo
    !
    ! roughness rigid walls
    ! explicit term
    !
    do k = 1, kmax
       nmu = icx
       ndm = -icy
       do nm = 1, nmmax
          ndm = ndm + 1
          nmu = nmu + 1
          if (kfu(nm)==1) then
             ddk(nm, k) = ddk(nm, k) + (rxx(nmu, k) - rxx(nm, k))/gvu(nm)       &
                        & + (rxy(nm, k) - rxy(ndm, k))/guu(nm)
          endif
       enddo
    enddo
end subroutine vihrov
