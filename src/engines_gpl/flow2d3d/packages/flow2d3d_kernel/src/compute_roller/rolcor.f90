subroutine rolcor(hrms      ,tp        ,theta     ,hu        ,hv         , &
                & guu       ,gvv       ,qxk       ,qyk       ,eulerisoglm, &
                & grmasu    ,grmasv    ,grfacu    ,grfacv    ,grmsur     , &
                & grmsvr    ,nmax      ,mmax      ,kmax      ,thick      , &
                & dzs1      ,kfsmin    ,kfsmax    ,sig       ,sw         , &
                & kfu       ,kcu       ,kfv       ,kcv       ,gdp        )
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
! Computes correction for mass fluxes (used in difu.f90) due to roller mass flux
! return flow and breaker delay. The former is now taken care of within euler.f90
! and no longer required in this routine (when using the default settings,
! i.e. eulerisoglm = false)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: rolcorr
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: rhow
    logical                , pointer :: zmodel
!
! Global variables
!
    integer                                                             , intent(in)  :: mmax
    integer                                                             , intent(in)  :: nmax
    integer                                                             , intent(in)  :: kmax
    logical                                                             , intent(in)  :: eulerisoglm !  Flag for using eulerian velocities for suspended transports
    integer,  dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kcu         !  Description and declaration in esm_alloc_int.f90
    integer,  dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kcv         !  Description and declaration in esm_alloc_int.f90
    integer,  dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kfsmax      !  Description and declaration in esm_alloc_int.f90
    integer,  dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kfsmin      !  Description and declaration in esm_alloc_int.f90
    integer,  dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kfu         !  Description and declaration in esm_alloc_int.f90
    integer,  dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kfv         !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: hrms
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: tp
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: hu
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: hv
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: theta
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: guu
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: gvv
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: grmsur
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: grmsvr
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: grmasu
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: grmasv
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: grfacu
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: grfacv
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub , kmax), intent(in)  :: dzs1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub , kmax)              :: qxk
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub , kmax)              :: qyk
    real(fp), dimension(kmax)                                           , intent(in)  :: thick
    real(fp), dimension(kmax)                                           , intent(in)  :: sig 
    real(fp)                                                            , intent(in)  :: sw
!
! Local variables
!
    integer :: m
    integer :: n
    integer :: k
    integer :: kk
    real(fp):: ampu
    real(fp):: costu
    real(fp):: f1u
    real(fp):: f2u
    real(fp):: f3u
    real(fp):: kwavu
    real(fp):: omegau
    real(fp):: tpu
    real(fp):: ustokes
    real(fp):: zu
    real(fp):: ampv
    real(fp):: f1v
    real(fp):: f2v
    real(fp):: f3v
    real(fp):: kwavv
    real(fp):: omegav
    real(fp):: sintv
    real(fp):: tpv
    real(fp):: vstokes
    real(fp):: zv
!
!! executable statements -------------------------------------------------------
!
    rolcorr   => gdp%gdbetaro%rolcorr
    ag        => gdp%gdphysco%ag
    rhow      => gdp%gdphysco%rhow
    zmodel    => gdp%gdprocs%zmodel 
    !
    do n=1,nmax
       do m=1,mmax
          if (eulerisoglm) then
             !
             ! Use Eulerian velocities for fluxes
             !
             if (kfu(n,m)==1 .and. kcu(n,m)<=2) then
                !
                ! u-direction
                !
                tpu = (tp(n,m)+tp(n,m+1)) / 2.0_fp
                if (tpu > 0.5_fp) then
                   !
                   ! only where there are waves
                   !
                   if (kmax > 1) then
                      !
                      ! 3D
                      !
                      costu  = 0.5_fp * (cos(degrad*theta(n,m))+cos(degrad*theta(n,m+1)))
                      ampu   = (hrms(n,m)+hrms(n,m+1)) / 4.0_fp
                      omegau = 2._fp * pi / tpu
                      call wavenr(hu(n,m), tpu, kwavu, ag)
                      f1u    = omegau * kwavu * ampu**2
                      f3u    = (1.0_fp - exp(-2.0_fp*kwavu*hu(n,m)))**2
                      do k=1,kmax
                         if (.not.zmodel) then
                            zu    = (1.0_fp+sig(k)) * hu(n,m)
                         else
                            zu    = 0.0_fp
                            do kk = kfsmin(n,m), kfsmax(n,m)
                               zu = zu + dzs1(n,m,kk)
                            enddo
                         endif
                         f2u        = exp(2.0_fp*kwavu*(zu-hu(n,m))) * (1.0_fp+exp(-4.0_fp*kwavu*zu))
                         ustokes    = f1u * (f2u / f3u) * costu
                         qxk(n,m,k) = qxk(n,m,k) - sw*(ustokes*hu(n,m) + grmsur(n,m) + grfacu(n,m))*guu(n,m)*thick(k)
                      enddo
                   else 
                      !
                      ! 2DH
                      !
                      qxk(n,m,1) = qxk(n,m,1) - sw*(grmasu(n,m) + grfacu(n,m))*guu(n,m)*thick(k)
                   endif
                endif ! end of tpu check
             endif ! end u-direction
             if (kfv(n,m)==1 .and. kcv(n,m)<=2) then
                !
                ! v-direction
                !
                tpv = (tp(n,m) + tp(n+1,m))/2.0_fp
                if (tpv > 0.5_fp) then
                   !
                   ! only where there are waves
                   !
                   if (kmax > 1) then
                      !
                      ! 3D
                      !
                      sintv  = 0.5_fp * (sin(degrad*theta(n,m))+sin(degrad*theta(n+1,m)))
                      ampv   = (hrms(n,m)+hrms(n+1,m)) / 4.0_fp
                      omegav = 2.0_fp * pi / tpv
                      call wavenr(hv(n,m), tpv, kwavv, ag)
                      f1v    = omegav * kwavv * ampv**2
                      f3v    = (1.0_fp - exp(-2.0_fp*kwavv*hv(n,m)))**2
                      do k=1,kmax
                         if (.not.zmodel) then
                            zv    = (1.0_fp+sig(k)) * hv(n,m)
                         else
                            zv    = 0.0_fp
                            do kk = kfsmin(n,m), kfsmax(n,m)
                               zv = zv + dzs1(n,m,kk)
                            enddo
                         endif
                         f2v        = exp(2.0_fp*kwavv*(zv-hv(n,m))) * (1.0_fp+exp(-4.0_fp*kwavv*zv))
                         vstokes    = f1v * (f2v / f3v) * sintv
                         qyk(n,m,k) = qyk(n,m,k) - sw*(vstokes*hv(n,m)+grmsvr(n,m)+grfacv(n,m))*gvv(n,m)*thick(k)
                      enddo
                   else
                      !
                      ! 2DH
                      !
                      qyk(n,m,1) = qyk(n,m,1) - sw*(grmasv(n,m)+grfacv(n,m))*gvv(n,m)*thick(k)
                   endif
                endif ! end of tpv check
             endif ! end v-direction
          else 
             !
             ! Use GLM velocities for fluxes (default)
             !
             if (rolcorr==1) then
                 !
                 ! Correction to fluxes uniform over water depth
                 !
                 do k = 1, kmax
                    qxk(n,m,k) = qxk(n,m,k) - sw*(grmsur(n,m))*guu(n,m)*thick(k)
                    qyk(n,m,k) = qyk(n,m,k) - sw*(grmsvr(n,m))*gvv(n,m)*thick(k)
                 enddo
                 !
             elseif (rolcorr==2) then
                !
                ! Correction for massflux due to roller is taken care of in euler.f90 (default)
                !
             endif
             !
             ! Apply correction for breaker delay             
             !
             do k = 1, kmax
               qxk(n,m,k) = qxk(n,m,k) - sw*(grfacu(n,m))*guu(n,m)*thick(k)
               qyk(n,m,k) = qyk(n,m,k) - sw*(grfacv(n,m))*gvv(n,m)*thick(k)
             enddo
             !
          endif
       enddo
    enddo
end subroutine rolcor
