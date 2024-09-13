subroutine z_vermom_horadv_mdue(kmax  , icx   , icy    , icxy   , kcs   , &
                              & kfs   , guu   , guv    , gvu    , u0    , & 
                              & v0    , kfsmin, kfsmx0 , kfuz0  , kfsz0 , &
                              & kfvz0 , w0    , ddk    , gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: z_vermom_horadv_mdue.f90 1044 2011-11-21 21:22:12Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20111115_13532_z-model_improvements_oss-merge/engines_gpl/flow2d3d/packages/kernel/src/nonhydro/z_vermom_horadv_mdue.f90 $
!!--description-----------------------------------------------------------------
!
!
!!--pseudo code and references--------------------------------------------------
!
! "A comparison of two 3D shallow water models using sigma coordinates and
!  z-coordinates in the vertical direction."
! Bijvelds, Van Kester and Stelling.
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer , pointer :: m1_nhy
    integer , pointer :: m2_nhy
    integer , pointer :: n1_nhy
    integer , pointer :: n2_nhy
!
! Global variables
!
    integer                                                         :: icx    !!  Increment in the X-dir., if ICX= NMAX
    integer                                            , intent(in) :: icy
    integer                                            , intent(in) :: icxy
    integer                                            , intent(in) :: kmax
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfuz0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfvz0  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: ddk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: v0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax) , intent(in) :: w0     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer            :: ddb
    integer            :: ikenx
    integer            :: ikeny
    integer            :: k
    integer            :: kd
    integer            :: ku
    integer            :: m
    integer            :: nm
    integer            :: nmst
    integer            :: nmstart
    integer            :: ndelta
    integer            :: ndm
    integer            :: ndmd
    integer            :: ndmu
    integer            :: nmd
    integer            :: nmu
    integer            :: num
    integer            :: numd
    integer            :: numu
    real(fp)           :: advecx
    real(fp)           :: advecy
    real(fp)           :: gksi
    real(fp)           :: geta
    real(fp)           :: cuu
    real(fp)           :: cvv
    real(fp)           :: uuu
    real(fp)           :: vvv
!
!! executable statements -------------------------------------------------------
!
    m1_nhy   => gdp%gdnonhyd%m1_nhy
    m2_nhy   => gdp%gdnonhyd%m2_nhy
    n1_nhy   => gdp%gdnonhyd%n1_nhy
    n2_nhy   => gdp%gdnonhyd%n2_nhy
    !
    ddb     = gdp%d%ddbound
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy + ddb) + (m1_nhy - 1 + ddb)*icxy
    !
    ! horizontal advection: u dw/dx + v dw/dy
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kfs(nm)*kcs(nm) == 1) then
             nmd  = nm - icx
             ndm  = nm - icy
             ndmd = nm - icx - icy
             nmu  = nm + icx
             num  = nm + icy
             numu = nm + icx + icy
             ndmu = nm + icx - icy
             numd = nm - icx + icy
             gksi = gvu(nm)
             geta = guu(nm)
             !
             !  loop over internal layers
             !
             do k = kfsmin(nm), kfsmx0(nm)-1
                ku     = k + 1
                kd     = k - 1
                vvv    = 0.25_fp * (v0(ndm,k) + v0(nm,k) + v0(ndm,ku) + v0(nm,ku))
                uuu    = 0.25_fp * (u0(nmd,k) + u0(nm,k) + u0(nmd,ku) + u0(nm,ku))
                cvv    = vvv / geta
                cuu    = uuu / gksi
                advecx = 0.0_fp
                advecy = 0.0_fp
                if (uuu>=0.0_fp .and. vvv>=0.0_fp) then
                   if (cuu > cvv) then
                      ikenx = kfuz0(nmd ,k) * kfuz0(nmd ,ku) * kfsz0(nm ,k) * kfsz0(nmd ,k)
                      ikeny = kfvz0(ndmd,k) * kfvz0(ndmd,ku) * kfsz0(nmd,k) * kfsz0(ndmd,k)
                      if (ikenx /= 0) then
                         advecx = ikenx * uuu * (w0(nm ,k)-w0(nmd ,k)) / gvu(nmd)
                      endif
                      if (ikeny /= 0) then
                         advecy = ikeny * vvv * (w0(nmd,k)-w0(ndmd,k)) / guv(ndmd)
                      endif
                   else
                      ikenx = kfuz0(ndmd,k) * kfuz0(ndmd,ku) * kfsz0(ndm,k) * kfsz0(ndmd, k)
                      ikeny = kfvz0(ndm ,k) * kfvz0(ndm ,ku) * kfsz0(nm ,k) * kfsz0(ndm , k)
                      if (ikenx /= 0) then
                         advecx = ikenx * uuu * (w0(ndm,k)-w0(ndmd,k)) / gvu(ndmd)
                      endif
                      if (ikeny /= 0) then
                         advecy = ikeny * vvv * (w0(nm,k)-w0(ndm,k)) / guv(ndm)
                      endif
                   endif
                elseif (uuu<0.0_fp .and. vvv>=0.0_fp) then
                   if ( -cuu > cvv) then
                      ikenx = kfuz0(nm  ,k) * kfuz0(nm  ,ku) * kfsz0(nmu,k) * kfsz0(nm  ,k)
                      ikeny = kfvz0(ndmu,k) * kfvz0(ndmu,ku) * kfsz0(nmu,k) * kfsz0(ndmu,k)
                      if (ikenx /= 0) then
                         advecx = ikenx * uuu * (w0(nmu,k)-w0(nm,k)) / gvu(nm)
                      endif
                      if (ikeny /= 0) then
                         advecy = ikeny * vvv * (w0(nmu,k)-w0(ndmu,k)) / guv(ndmu)
                      endif
                   else
                      ikenx = kfuz0(ndm,k) * kfuz0(ndm,ku) * kfsz0(ndmu,k) * kfsz0(ndm,k)
                      ikeny = kfvz0(ndm,k) * kfvz0(ndm,ku) * kfsz0(nm  ,k) * kfsz0(ndm,k)
                      if (ikenx /= 0) then
                         advecx = ikenx * uuu * (w0(ndmu,k)-w0(ndm,k)) / gvu(ndm)
                      endif
                      if (ikeny /= 0) then
                         advecy = ikeny * vvv * (w0(nm,k)-w0(ndm,k)) / guv(ndm)
                      endif
                   endif
                elseif (uuu>=0.0_fp .and. vvv<0.0_fp) then
                   if (cuu > -cvv) then
                      ikenx = kfuz0(nmd,k) * kfuz0(nmd,ku) * kfsz0(nm  ,k) * kfsz0(nmd,k)
                      ikeny = kfvz0(nmd,k) * kfvz0(nmd,ku) * kfsz0(numd,k) * kfsz0(nmd,k)
                      if (ikenx /= 0) then
                         advecx = ikenx * uuu * (w0(nm,k)-w0(nmd,k)) / gvu(nmd)
                      endif
                      if (ikeny /= 0) then
                         advecy = ikeny * vvv * (w0(numd,k)-w0(nmd,k)) / guv(nmd)
                      endif
                   else
                      ikenx = kfuz0(numd,k) * kfuz0(numd,ku) * kfsz0(num,k) * kfsz0(numd,k)
                      ikeny = kfvz0(nm  ,k) * kfvz0(nm  ,ku) * kfsz0(nm ,k) * kfsz0(num ,k)
                      if (ikenx /= 0) then
                         advecx = ikenx * uuu * (w0(num,k)-w0(numd,k)) / gvu(numd)
                      endif
                      if (ikeny /= 0) then
                         advecy = ikeny * vvv * (w0(num,k)-w0(nm,k)) / guv(nm)
                      endif
                   endif
                elseif (uuu<0.0_fp .and. vvv<0.0_fp) then
                   if ( -cuu > -cvv) then
                      ikenx = kfuz0(nm ,k) * kfuz0(nm ,ku) * kfsz0(nmu,k) * kfsz0(nm  ,k)
                      ikeny = kfvz0(nmu,k) * kfvz0(nmu,ku) * kfsz0(nmu,k) * kfsz0(numu,k)
                      if (ikenx /= 0) then
                         advecx = ikenx * uuu * (w0(nmu,k)-w0(nm,k)) / gvu(nm)
                      endif
                      if (ikeny /= 0) then
                         advecy = ikeny * vvv * (w0(numu,k)-w0(nmu,k)) / guv(nmu)
                      endif
                   else
                      ikenx = kfuz0(numd,k) * kfuz0(numd,ku) * kfsz0(num,k) * kfsz0(numd,k)
                      ikeny = kfvz0(nm  ,k) * kfvz0(nm  ,ku) * kfsz0(nm ,k) * kfsz0(num ,k)
                      if (ikenx /= 0) then
                         advecx = ikenx * uuu * (w0(numu,k)-w0(num,k)) / gvu(num)
                      endif
                      if (ikeny /= 0) then
                         advecy = ikeny * vvv * (w0(num,k)-w0(nm,k)) / guv(nm)
                      endif
                   endif
                else
                endif
                ddk (nm, k) = ddk(nm, k) - advecx - advecy
             enddo
          endif
       enddo
    enddo
    !
end subroutine z_vermom_horadv_mdue
