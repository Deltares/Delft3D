module m_umod
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
!-------------------------------------------------------------------------------
!
! functions and subroutines
!
implicit none 



private

public compute_umod, find_cell_centre_index

contains
    
subroutine compute_umod(nmmax , kmax , icx       , &
                      & kcs   , kfu  , kfv       , kcu    , kcv, &
                      & dps   , s1   , deltau    , deltav , &
                      & u     , v    , sig, &
                      & gdp   , &
                      !output
                      & umod   , uuu  , vvv) 
!
use precision, only: fp, prec
!
use globaldata, only: globdat
!
implicit none
!
type(globdat),target :: gdp
!
! Global variables
!
integer                                           , intent(in)  :: nmmax !  Description and declaration in dimens.igs
integer                                           , intent(in)  :: kmax  !  Description and declaration in esm_alloc_int.f90
integer                                           , intent(in)  :: icx
integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs   !  Description and declaration in esm_alloc_int.f90
integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcu   !  Description and declaration in esm_alloc_int.f90
integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcv   !  Description and declaration in esm_alloc_int.f90
integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu   !  Description and declaration in esm_alloc_int.f90
integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv   !  Description and declaration in esm_alloc_int.f90
real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dps   !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s1    !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: deltau !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: deltav !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: u
real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: v
real(fp)  , dimension(kmax)                       , intent(in)  :: sig   !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: umod
real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: uuu
real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: vvv
!
! Local variables
!
integer  :: kmx
integer  :: nm
integer  :: nm_u1
integer  :: nm_u2
integer  :: nm_v1
integer  :: nm_v2
real(fp) :: ufac
real(fp) :: vfac
!
!! executable statements -------------------------------------------------------
!
do nm=1,nmmax !loop on cell-centres

   if (kcs (nm) == 0) then
      uuu  (nm) = 0.0_fp
      vvv  (nm) = 0.0_fp
      umod (nm) = 0.0_fp
      cycle
   endif
       
   call find_cell_centre_index(gdp,nm,icx,s1,dps,kcs,kcu,kmax,kcv,kfu,kfv,deltau,deltav,sig,&
                            & nm_u1,nm_u2,nm_v1,nm_v2,kmx,ufac,vfac)
          
   uuu(nm) = ufac * (  kfu(nm_u1)*u(nm_u1, kmx) &
      &         + kfu(nm_u2)*u(nm_u2, kmx)  )
   vvv(nm) = vfac * (  kfv(nm_v1)*v(nm_v1, kmx) &
      &         + kfv(nm_v2)*v(nm_v2, kmx)  )
   
   umod(nm) = (uuu(nm)*uuu(nm) + vvv(nm)*vvv(nm))**0.5
enddo !kn

end subroutine compute_umod
       
subroutine find_cell_centre_index(gdp,nm,icx,s1,dps,kcs,kcu,kmax,kcv,kfu,kfv,deltau,deltav,sig,&
                                & nm_u1,nm_u2,nm_v1,nm_v2,kmx,ufac,vfac)
!
use precision, only: fp, prec
use globaldata, only: globdat
!
implicit none
!
type(globdat),target :: gdp
!
! The following list of pointer parameters is used to point inside the gdp structure
!
real(fp)                             , pointer :: eps
logical                              , pointer :: v2dwbl
!
! Global variables
!
integer                                           , intent(in)  :: nm
integer                                           , intent(in)  :: icx
integer                                           , intent(in)  :: kmax  !  Description and declaration in esm_alloc_int.f90
integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs   !  Description and declaration in esm_alloc_int.f90
integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcu   !  Description and declaration in esm_alloc_int.f90
integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcv   !  Description and declaration in esm_alloc_int.f90
integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu   !  Description and declaration in esm_alloc_int.f90
integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv   !  Description and declaration in esm_alloc_int.f90
!
real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dps   !  Description and declaration in esm_alloc_real.f90
!
real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: deltau !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: deltav !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s1    !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(kmax)                       , intent(in)  :: sig   !  Description and declaration in esm_alloc_real.f90
!
integer, intent(out) :: nm_u1
integer, intent(out) :: nm_u2
integer, intent(out) :: nm_v1
integer, intent(out) :: nm_v2
integer, intent(out) :: kmx
!
real(fp), intent(out) :: vfac
real(fp), intent(out) :: ufac    
!
! Local variables
!
integer  :: k
integer  :: ndm
integer  :: ndmd
integer  :: ndmu
integer  :: nmd
integer  :: nmu
integer  :: num
integer  :: numd
real(fp) :: cc
real(fp) :: fact
real(fp) :: maxdepfrac
real(fp) :: h1
real(fp) :: deltas
!
!! executable statements -------------------------------------------------------
!
eps                 => gdp%gdconst%eps
v2dwbl              => gdp%gdnumeco%v2dwbl
!
nmd  = nm  - icx
numd = nmd + 1
ndmd = nmd - 1
num  = nm  + 1
ndm  = nm  - 1
nmu  = nm  + icx
ndmu = nmu - 1
!
h1 = s1(nm) + real(dps(nm),fp)
!
if (v2dwbl) then
   fact   = max(kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm), 1)
   deltas = (deltau(nm) + deltau(nmd) + deltav(nm) + deltav(ndm)) / fact
   maxdepfrac = 0.5_fp
else
   deltas = 0.05_fp
   maxdepfrac = 0.05_fp
endif       
!
do k = kmax, 1, -1
   cc  = (1.0 + sig(k))*h1
   kmx = k
   if (cc>=maxdepfrac*h1 .or. cc>=deltas) then
      exit
   endif         
enddo
!
ufac = 0.5_fp
vfac = 0.5_fp
if (abs(kcs(nm)) == 1) then
   !
   ! Internal point
   ! Set velocity in U direction.
   !
   nm_u1 = nm
   nm_u2 = nmd
   !
   ! Set velocity in V direction.
   !
   nm_v1 = nm
   nm_v2 = ndm
elseif (kcu(nm) + kcu(nmd) == 1) then
   !
   ! Open boundary (kcs(nm)==2) in v-direction
   !
   ! Set velocity in U direction.
   !
   nm_u1 = nm
   nm_u2 = nmd
   ufac  = 1.0_fp
   !
   ! Set velocity in V direction.
   !
   if (kcu(nm) == 1) then
      !
      ! Open boundary at left-hand side
      !
      nm_v1 = nmu
      nm_v2 = ndmu
   else
      !
      ! Open boundary at right-hand side
      !
      nm_v1 = nmd
      nm_v2 = ndmd
   endif
else
   !
   ! Open boundary (kcs(nm)==2) in u-direction
   !
   ! Set velocity in U direction.
   !
   if (kcv(nm) == 1) then
      !
      ! Open boundary at lower side
      !
      nm_u1 = num
      nm_u2 = numd
   else
      !
      ! Open boundary at upper side
      !
      nm_u1 = ndm
      nm_u2 = ndmd
   endif
   !
   ! Set velocity in V direction.
   !
   nm_v1 = nm
   nm_v2 = ndm
   vfac  = 1.0_fp
endif

end subroutine find_cell_centre_index
                
end module m_umod