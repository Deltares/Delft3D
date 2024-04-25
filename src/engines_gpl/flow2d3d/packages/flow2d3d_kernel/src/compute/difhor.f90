subroutine difhor(nm        ,nmu       ,j         ,nmmaxj    ,kmax      , &
                & lstsci    ,lsal      ,ltem      ,kcs       ,kadu      , &
                & sepnm     ,sepnmu    ,dpnm      ,dpnmu     ,guu       , &
                & gvu       ,r0        ,ddkl      ,thick     ,sig       , &
                & dicuv     ,sigdif    ,gdp       )
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
!    Function: computes horizontal diffusion along strict
!              horizontal planes, using a special limiter,
!              to avoid 'artificial vertical diffusion'.
!              Recently the limiter was improved.The new
!              limiter is following Van Leer's scheme for
!              advection and preserves monotony.
!              explicit in u-direction, explicit in v-
!              direction. Fluxes computed in subroutine
!              dengra are used.
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
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'pardef.igd'
!
! Global variables
!
    integer                                                               :: j      !!  Begin pointer for arrays which have
                                                                                    !!  been transformed into 1d arrays.
                                                                                    !!  due to the shift in the 2nd (m-)
                                                                                    !!  index, j = -2*nmax + 1
    integer                                                 , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: lsal   !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: nm
    integer                                                               :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: nmu
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: kadu   !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                , intent(in)  :: dpnm
    real(fp)                                                , intent(in)  :: dpnmu
    real(fp)                                                , intent(in)  :: sepnm
    real(fp)                                                , intent(in)  :: sepnmu
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)      , intent(in)  :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: ddkl   !!  Internal work array, right hand side at (n,m,k,l)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                               , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                               , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsci)                             , intent(in)  :: sigdif !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                             :: k
    integer                             :: k1
    integer                             :: k2
    integer                             :: kf
    integer                             :: kflux
    integer                             :: kll
    integer                             :: kpoint
    integer                             :: krr
    integer                             :: l       ! Constituent number 
    integer, dimension(2*kmax + 1)      :: kicol   ! K-index concentration point left for flux kf 
    integer, dimension(2*kmax + 1)      :: kicor   ! K-index concentration point right for flux kf 
    real(fp)                            :: cl
    real(fp)                            :: cr
    real(fp)                            :: difl
    real(fp)                            :: difr
    real(fp)                            :: flux
    real(fp)                            :: grad
    real(fp)                            :: grad1
    real(fp)                            :: grad2
    real(fp)                            :: grmax
    real(fp)                            :: grmin
    real(fp)                            :: h0
    real(fp), dimension(0:2*kmax + 1)   :: point   ! Merge arrays polal and polar 
    real(fp), dimension(0:kmax)         :: polal   ! Z-coordinate horizontal layers in nm 
    real(fp), dimension(0:kmax)         :: polar   ! Z-coordinate horizontal layers in nmu 
    real(fp), dimension(2*kmax + 1)     :: poflu   ! Z-coordinate gradient flux 
    real(fp), dimension(kmax)           :: pocol   ! Z-coordinate concentr. point nm ,k 
    real(fp), dimension(kmax)           :: pocor   ! Z-coordinate concentr. point nmu,k 
!
!! executable statements -------------------------------------------------------
!
    !***position horizontal interfaces left
    !
    polal(0) = sepnm
    h0 = max(sepnm + dpnm, 0.01_fp)
    do k = 1, kmax
       polal(k) = (sig(k) - 0.5*thick(k))*h0 + sepnm
       pocol(k) = 0.5*(polal(k - 1) + polal(k))
    enddo
    !
    !***position horizontal interfaces right
    !
    polar(0) = sepnmu
    h0 = max(sepnmu + dpnmu, 0.01_fp)
    do k = 1, kmax
       polar(k) = (sig(k) - 0.5*thick(k))*h0 + sepnmu
       pocor(k) = 0.5*(polar(k - 1) + polar(k))
    enddo
    !
    !***merge polal and polar
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
             goto 41
          endif
       else
          point(k) = polar(krr)
          krr = krr + 1
          if (krr>kmax) then
             kpoint = k + 1
             point(kpoint) = polal(kll)
             goto 41
          endif
       endif
    enddo
    kpoint = 2*kmax + 1
    !
    !***position flux points
    !
   41 continue
    kflux = kpoint
    do k = 1, kflux
       poflu(k) = 0.5*(point(k) + point(k - 1))
    enddo
    !
    !***k-index concentration points left and right for flux point
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
    !***computation diffusive flux using
    !   limiter
    !
    do l = max(lsal, ltem) + 1, lstsci
       !
       !***computation of flux
       !
       do kf = 1, kflux
          kll = kicol(kf)
          krr = kicor(kf)
          if (kll/=0 .and. krr/=0) then
             if (kadu(nm, kll)*kadu(nm, krr)/=0) then
                !
                !***interpolation left point
                !
                k2 = kll
                if (pocor(krr)>pocol(kll)) then
                   k1 = k2 - 1
                   if (k1<1) then
                      cl = r0(nm, k2, l)
                   else
                      cl = ((pocor(krr) - pocol(k2))/(pocol(k1) - pocol(k2)))   &
                         & *r0(nm, k1, l)                                       &
                         & + ((pocor(krr) - pocol(k1))/(pocol(k2) - pocol(k1))) &
                         & *r0(nm, k2, l)
                   endif
                else
                   k1 = k2 + 1
                   if (k1>kmax) then
                      cl = r0(nm, k2, l)
                   else
                      cl = ((pocor(krr) - pocol(k2))/(pocol(k1) - pocol(k2)))   &
                         & *r0(nm, k1, l)                                       &
                         & + ((pocor(krr) - pocol(k1))/(pocol(k2) - pocol(k1))) &
                         & *r0(nm, k2, l)
                   endif
                endif
                !
                !***interpolation left point
                !
                k2 = krr
                if (pocol(kll)>pocor(k2)) then
                   k1 = k2 - 1
                   if (k1<1) then
                      cr = r0(nmu, k2, l)
                   else
                      cr = ((pocol(kll) - pocor(k2))/(pocor(k1) - pocor(k2)))   &
                         & *r0(nmu, k1, l)                                      &
                         & + ((pocol(kll) - pocor(k1))/(pocor(k2) - pocor(k1))) &
                         & *r0(nmu, k2, l)
                   endif
                else
                   k1 = k2 + 1
                   if (k1>kmax) then
                      cr = r0(nmu, k2, l)
                   else
                      cr = ((pocol(kll) - pocor(k2))/(pocor(k1) - pocor(k2)))   &
                         & *r0(nmu, k1, l)                                      &
                         & + ((pocol(kll) - pocor(k1))/(pocor(k2) - pocor(k1))) &
                         & *r0(nmu, k2, l)
                   endif
                endif
                grad1 = r0(nmu, krr, l) - cl
                grad2 = cr - r0(nm, kll, l)
                grmax = max(grad1, grad2)
                grmin = min(grad1, grad2)
                !
                !***new limiter following Van Leer
                !
                if (grmax>=0.0 .and. grmin<=0.0) then
                   grad = 0.0
                else
                   grad = 2.*grad1*grad2/(grad1 + grad2)
                endif
                !
                !***flux
                !
                difl = dicuv(nm, kll)
                difr = dicuv(nmu, krr)
                flux = 0.5*(point(kf - 1) - point(kf))*grad*guu(nm)             &
                     & *(difl + difr)/sigdif(l)/gvu(nm)
                ddkl(nm, kll, l) = ddkl(nm, kll, l) + flux*abs(2 - kcs(nmu))
                ddkl(nmu, krr, l) = ddkl(nmu, krr, l) - flux*abs(2 - kcs(nm))
             endif
          endif
       enddo
    enddo
end subroutine difhor
