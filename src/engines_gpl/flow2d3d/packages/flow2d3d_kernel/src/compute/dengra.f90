subroutine dengra(icreep    ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                & icx       ,icy       ,lstsci    ,lsts      ,lsal      , &
                & ltem      ,lsed      ,saleqs    ,temeqs    ,rhosol    , &
                & kcs       ,kfu       ,s0        ,dps       ,hu        , &
                & thick     ,sig       ,guu       ,gvu       ,r0        , &
                & dicuv     ,dpu       ,dpdksi    ,dsdksi    ,dtdksi    , &
                & dldksi    ,gdp       )
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
!    Function: Computes horizontal pressure gradient due
!              to density gradients, along strictly
!              horizontal planes, using a special limiter,
!              to avoid 'artificial flow'. Recently the
!              limiter was improved. The original minimum
!              limiter leads to underestimation of the
!              pressure gradient. The limiter is following
!              Van Leer and preserves monotony.
!              Horizontal diffusive fluxes of salt and
!              temperature are stored in arrays for
!              for transport equation.
!              Also in case of sediment; but only in baroclinic
!              density term (no correction for diffusive flux
!              for sediment; i.e. water is assumed homogeneous
!              HOWEVER array DLDKSI for this purpose is already
!              RESERVED!)
!
! Method used: Reference : On the approximation of horizontal
!              gradients in sigma co-ordinates for bathymetry
!              with steep bottom slopes (G.S. Stelling and J.
!              van Kester - International Journal for Methods
!              in Fluids, Vol. 18 1994) and the discussion
!              in "A comparison of two 3D shallow
!              water models using sigma-coordinates and
!              z-coordinates in the vertical direction."
!              (M.D. Bijvelds, J. van Kester and G.S. Stelling -
!               Proceedings 6-th International Conference on
!               estuarine and coastal modelling, New Orleans,
!               september 1999)
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
    include 'pardef.igd'
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    integer                , pointer :: idensform
!
! Global variables
!
    integer                                                 , intent(in)  :: icreep !  Description and declaration in tricom.igs
    integer                                                 , intent(in)  :: icx    !!  Increment in the x-dir., if icx= nmax
                                                                                    !!  then computation proceeds in the x-
                                                                                    !!  dir. if icx=1 then computation pro-
                                                                                    !!  ceeds in the y-dir.
    integer                                                               :: icy    !!  Increment in the y-dir. (see icx)
    integer                                                               :: j      !!  Begin pointer for arrays which have
                                                                                    !!  been transformed into 1d arrays.
                                                                                    !!  due to the shift in the 2nd (m-)
                                                                                    !!  index, j = -2*nmax + 1
    integer                                                 , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: lsal   !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: lsed   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: lsts   !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                               :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                , intent(in)  :: saleqs !  Description and declaration in tricom.igs
    real(fp)                                                , intent(in)  :: temeqs !  Description and declaration in tricom.igs
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)    , intent(in)  :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: dldksi !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dpdksi !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dsdksi !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dtdksi !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                               , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                               , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lsed)                               , intent(in)  :: rhosol !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                              :: k
    integer                              :: k1
    integer                              :: k2
    integer                              :: kf
    integer                              :: kflux
    integer                              :: kll
    integer                              :: kpoint
    integer                              :: krr
    integer                              :: l      ! Constituent number
    integer                              :: lst
    integer                              :: nm     ! Index gridpoint
    integer                              :: nmu    ! Index neighbour (M+1)
    integer, dimension(2*kmax + 1)       :: kicol  ! K-index concentration point left for flux kf
    integer, dimension(2*kmax + 1)       :: kicor  ! K-index concentration point right for flux kf
    real(fp)                             :: alph0
    real(fp)                             :: cl
    real(fp)                             :: cladt
    real(fp)                             :: cr
    real(fp)                             :: darea
    real(fp)                             :: difco
    real(fp)                             :: dpnm
    real(fp)                             :: dpnmu
    real(fp)                             :: drho
    real(fp)                             :: dsal
    real(fp)                             :: dtem
    real(fp)                             :: dummy
    real(fp)                             :: flux
    real(fp)                             :: grad
    real(fp)                             :: grad1
    real(fp)                             :: grad2
    real(fp)                             :: grmax
    real(fp)                             :: grmin
    real(fp)                             :: h0
    real(fp)                             :: rhods
    real(fp)                             :: rhodt
    real(fp)                             :: sal
    real(fp)                             :: sepnm
    real(fp)                             :: sepnmu
    real(fp)                             :: temp
    real(fp)                             :: zubot
    real(fp)                             :: zuk
    real(fp), dimension(0:2*kmax + 1)    :: point  ! Merge arrays polal and polar
    real(fp), dimension(0:kmax)          :: polal  ! Z-coordinate horizontal layers in nm
    real(fp), dimension(0:kmax)          :: polar  ! Z-coordinate horizontal layers in nmu
    real(fp), dimension(2*kmax + 1)      :: poflu  ! Z-coordinate gradient flux
    real(fp), dimension(kmax)            :: pocol  ! Z-coordinate concentr. point nm ,k
    real(fp), dimension(kmax)            :: pocor  ! Z-coordinate concentr. point nmu,k
!
!! executable statements -------------------------------------------------------
!
    rhow      => gdp%gdphysco%rhow
    ag        => gdp%gdphysco%ag
    idensform => gdp%gdphysco%idensform
    !
    alph0 = 0.6980
    !
    ! Initialise for all (nm, k)
    !
    dldksi = 0.0
    dpdksi = 0.0
    dsdksi = 0.0
    dtdksi = 0.0
    !
    ! pressure gradient; only for salinity and/or temperature and/or
    ! sediment and for anti creep
    !
    if (lsts==0 .or. icreep==0) goto 9999
    !
    nmu = icx
    do nm = 1, nmmax
       nmu = nmu + 1
       !
       ! In the SED version the following line read:
       ! if (kfu(nm).eq.1 .and. kcs(nm)*kcs(nmu).ne.2) then
       ! The latter check was not introduced here on ground of
       ! backward compatibility for non-sed quantities.
       !
       if (kfu(nm)==1) then
          sepnm  = s0(nm)
          sepnmu = s0(nmu)
          dpnm   = real(dps(nm) ,fp)
          dpnmu  = real(dps(nmu),fp)
          !
          ! position horizontal interfaces left
          !
          polal(0) = sepnm
          h0 = max(sepnm + dpnm, 0.01_fp)
          do k = 1, kmax
             polal(k) = (sig(k) - 0.5*thick(k))*h0 + sepnm
             pocol(k) = 0.5*(polal(k - 1) + polal(k))
          enddo
          !
          ! position horizontal interfaces right
          !
          polar(0) = sepnmu
          h0 = max(sepnmu + dpnmu, 0.01_fp)
          do k = 1, kmax
             polar(k) = (sig(k) - 0.5*thick(k))*h0 + sepnmu
             pocor(k) = 0.5*(polar(k - 1) + polar(k))
          enddo
          !
          ! merge polal and polar
          !
          kll = 0
          krr = 0
          do k = 0, 2*kmax + 1
             if (polal(kll)>polar(krr)) then
                point(k) = polal(kll)
                kll = kll + 1
                if (kll>kmax) then
                   kpoint = k + 1
                   point(kpoint) = polar(krr)
                   goto 140
                endif
             else
                point(k) = polar(krr)
                krr = krr + 1
                if (krr>kmax) then
                   kpoint = k + 1
                   point(kpoint) = polal(kll)
                   goto 140
                endif
             endif
          enddo
          kpoint = 2*kmax + 1
          !
          ! position flux points
          !
  140     continue
          kflux = kpoint
          do k = 1, kflux
             poflu(k) = 0.5*(point(k) + point(k - 1))
          enddo
          !
          ! k-index concentration points left and right for flux point
          !
          kll = 1
          krr = 1
          do kf = 1, kflux
             kicol(kf) = 0
             kicor(kf) = 0
             do k = kll, kmax
                if (poflu(kf)<=polal(k - 1) .and. poflu(kf)>=polal(k)) then
                   kicol(kf) = k
                   kll = k
                   exit
                endif
             enddo
             do k = krr, kmax
                if (poflu(kf)<=polar(k - 1) .and. poflu(kf)>=polar(k)) then
                   kicor(kf) = k
                   krr = k
                   exit
                endif
             enddo
          enddo
          !
          ! computation concentration flux
          ! using limiter
          !
          lst = max(lsal, ltem)
          do l = 1, lsts
             !
             ! computation of flux
             !
             do kf = 1, kflux
                kll = kicol(kf)
                krr = kicor(kf)
                if (kll/=0 .and. krr/=0) then
                   !
                   ! interpolation left point
                   !
                   if (pocor(krr)>pocol(kll)) then
                      k1 = 0
                      do k = kll - 1, 1, -1
                         if (pocol(k)>pocor(krr)) then
                            k1 = k
                            exit
                         endif
                      enddo
                      k2 = k1 + 1
                      if (k1<1) then
                         cl = r0(nm, k2, l)
                      else
                         cl = ((pocor(krr) - pocol(k2))/(pocol(k1) - pocol(k2)))&
                            & *r0(nm, k1, l)                                    &
                            & + ((pocor(krr) - pocol(k1))/(pocol(k2) - pocol(k1)&
                            & ))*r0(nm, k2, l)
                      endif
                   else
                      k1 = kmax + 1
                      do k = kll + 1, kmax, 1
                         if (pocol(k)<pocor(krr)) then
                            k1 = k
                            exit
                         endif
                      enddo
                      k2 = k1 - 1
                      if (k1>kmax) then
                         cl = r0(nm, k2, l)
                      else
                         cl = ((pocor(krr) - pocol(k2))/(pocol(k1) - pocol(k2)))&
                            & *r0(nm, k1, l)                                    &
                            & + ((pocor(krr) - pocol(k1))/(pocol(k2) - pocol(k1)&
                            & ))*r0(nm, k2, l)
                      endif
                   endif
                   !
                   ! interpolation right point
                   !
                   if (pocol(kll)>pocor(krr)) then
                      k1 = 0
                      do k = krr - 1, 1, -1
                         if (pocor(k)>pocol(kll)) then
                            k1 = k
                            exit
                         endif
                      enddo
                      k2 = k1 + 1
                      if (k1<1) then
                         cr = r0(nmu, k2, l)
                      else
                         cr = ((pocol(kll) - pocor(k2))/(pocor(k1) - pocor(k2)))&
                            & *r0(nmu, k1, l)                                   &
                            & + ((pocol(kll) - pocor(k1))/(pocor(k2) - pocor(k1)&
                            & ))*r0(nmu, k2, l)
                      endif
                   else
                      k1 = kmax + 1
                      do k = krr + 1, kmax, 1
                         if (pocor(k)<pocol(kll)) then
                            k1 = k
                            exit
                         endif
                      enddo
                      k2 = k1 - 1
                      if (k1>kmax) then
                         cr = r0(nmu, k2, l)
                      else
                         cr = ((pocol(kll) - pocor(k2))/(pocor(k1) - pocor(k2)))&
                            & *r0(nmu, k1, l)                                   &
                            & + ((pocol(kll) - pocor(k1))/(pocor(k2) - pocor(k1)&
                            & ))*r0(nmu, k2, l)
                      endif
                   endif
                   grad1 = r0(nmu, krr, l) - cl
                   grad2 = cr - r0(nm, kll, l)
                   grmax = max(grad1, grad2)
                   grmin = min(grad1, grad2)
                   !
                   ! new limiter following Van Leer
                   !
                   if (grmax>=0.0 .and. grmin<=0.0) then
                      grad = 0.0
                   else
                      grad = 2.*grad1*grad2/(grad1 + grad2)
                   endif
                   !
                   ! flux
                   !
                   if (l==lsal) then
                      sal = 0.5*(r0(nm, kll, lsal) + r0(nmu, krr, lsal))
                      if (ltem==0) then
                         temp = temeqs
                      else
                         temp = 0.5*(r0(nm, kll, ltem) + r0(nmu, krr, ltem))
                      endif
                      select case(idensform)
                         case( dens_Eckart )
                            call dens_eck ( temp, sal, dummy, rhods, dummy )
                         case( dens_UNESCO )
                            call dens_unes( temp, sal, dummy, rhods, dummy )
                         case( dens_NaClSol)
                            call dens_nacl( temp, sal, dummy, rhods, dummy )
                      end select
                      drho = rhods*grad
                      dsal = grad
                      dtem = 0.0
                   elseif (l==ltem) then
                      temp = 0.5*(r0(nm, kll, ltem) + r0(nmu, krr, ltem))
                      if (lsal==0) then
                         sal = saleqs
                      else
                         sal = 0.5*(r0(nm, kll, lsal) + r0(nmu, krr, lsal))
                      endif
                      select case(idensform)
                         case( dens_Eckart )
                            call dens_eck ( temp, sal, dummy, dummy, rhodt )
                         case( dens_UNESCO )
                            call dens_unes( temp, sal, dummy, dummy, rhodt )
                         case( dens_NaClSol)
                            call dens_nacl( temp, sal, dummy, dummy, rhodt )
                      end select
                      drho = rhodt*grad
                      dsal = 0.0
                      dtem = grad
                   else
                      drho = grad*(1.0 - rhow/rhosol(l - lst))
                      dsal = 0.0
                      dtem = 0.0
                   endif
                   darea = (point(kf - 1) - point(kf))*guu(nm)/gvu(nm)
                   difco = 0.5*(dicuv(nm, kll) + dicuv(nmu, krr))/0.7
                   !
                   ! Bottom in U-point dpu(nm) may differ from MIN(dps(nm),dps(nmu))
                   ! formula should be based on dpu
                   !
                   zubot = -dpu(nm)
                   if (zubot<point(kf)) then
                      dsdksi(nm, kll) = dsdksi(nm, kll)                         &
                                      & + darea*difco*dsal*abs(2 - abs(kcs(nmu)))
                      dsdksi(nmu, krr) = dsdksi(nmu, krr)                       &
                                       & - darea*difco*dsal*abs(2 - abs(kcs(nm)))
                      dtdksi(nm, kll) = dtdksi(nm, kll)                         &
                                      & + darea*difco*dtem*abs(2 - abs(kcs(nmu)))
                      dtdksi(nmu, krr) = dtdksi(nmu, krr)                       &
                                       & - darea*difco*dtem*abs(2 - abs(kcs(nm)))
                      flux = ag*(point(kf - 1) - point(kf))*drho/(gvu(nm)*rhow)
                      do k = kmax, 1, -1
                         zuk = (1.0 + sig(k))*hu(nm) - dpu(nm)
                         if (zuk<point(kf)) then
                            dpdksi(nm, k) = dpdksi(nm, k) + flux
                         else
                            if (zuk<point(kf - 1)) then
                               flux = ag*(point(kf - 1) - zuk)                  &
                                    & *drho/(gvu(nm)*rhow)
                               dpdksi(nm, k) = dpdksi(nm, k) + flux
                            endif
                            exit
                         endif
                      enddo
                   else
                      !
                      ! No more flux points above bottom
                      !
                      exit
                   endif
                endif
             enddo
          enddo
       endif
    enddo
    !
    !
 9999 continue
end subroutine dengra
