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

public compute_umod

contains
    
function compute_umod(u,v,huv,deltadir,sig,kmax,gdp) result(umod)

use precision, only: fp 
use globaldata, only: globdat

!
! Global variables
!
type(globdat),target :: gdp
!
!
integer, intent(in)  :: kmax
!
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: u
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: v
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: huv
real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: deltadir  
real(fp), dimension(kmax)                                          , intent(in)  :: sig

real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax) :: umod !cell centres!!!
!
! Local variables
!
integer :: kmaxx
integer :: k
integer :: nc, nm, kc, mc, mu, nd !
!
real(kind=fp) :: ucm !cell center velocity in m-direction [m/s]
real(kind=fp) :: ucn !cell center velocity in n-direction [m/s]
real(kind=fp) :: depth
real(kind=fp) :: zcc

!
! Initialize locals
!
ucm   = 0.0_fp
ucn   = 0.0_fp
depth = 0.0_fp



do kc=1,nc !loop on cell-centres

associate(v2dwbl => gdp%gdnumeco%v2dwbl, trtminh => gdp%gdtrachy%trtminh)
depth      = max(trtminh , huv(nc, mc))
!
kmaxx = kmax
!

if (v2dwbl) then !  Flag for the use of velocity above wave boundary layer for computing 2d representative velocity
   do k = kmax, 1, -1
      zcc = (1.0 + sig(k))*depth
      if (zcc>deltadir(nc, mc) .or. zcc>0.5*depth) then
         kmaxx = k
         exit
      endif
   enddo
endif
end associate      

!v2dwbl          => gdp%gdnumeco%v2dwbl
!trtminh         => gdp%gdtrachy%trtminh

!idir 1
!uvdir=r(u1)     ,uvperp=r(v1) 

!idir 2
!uvdir=r(v1)     ,uvperp=r(u1) 

!
! Average perpendicular velocity
!
!if (idir==1) then
!   call ! For Villemonte and Tabellen(nc, mc, nm, gdp) 
!   call n_and_m_to_nm(nc, mu, nmu, gdp) 
!
!   vvv = 0.25_fp*(  uvperp(nc, mc, kmaxx) + uvperp(nc, mu, kmaxx)  &
!       &          + uvperp(nd, mc, kmaxx) + uvperp(nd, mu, kmaxx))
!else
!   call n_and_m_to_nm(nc, mc, nm, gdp) 
!   call n_and_m_to_nm(nu, mc, num, gdp) 
!
!   vvv = 0.25_fp*(  uvperp(nc, mc, kmaxx) + uvperp(nc, md, kmaxx)  &
!       &          + uvperp(nu, mc, kmaxx) + uvperp(nu, md, kmaxx))
!endif
!          
!uuu  = uvdir(nc, mc, kmaxx)
!umod = sqrt(uuu**2 + vvv**2)

ucm = 0.25_fp*(  u(nc, mc, kmaxx) + u(nc, mu, kmaxx)  &
    &          + u(nd, mc, kmaxx) + u(nd, mu, kmaxx))

ucn = 0.25_fp*(  v(nc, mc, kmaxx) + v(nc, mu, kmaxx)  &
    &          + v(nd, mc, kmaxx) + v(nd, mu, kmaxx))

umod = sqrt(ucm**2 + ucn**2)

enddo !kn

end function compute_umod

end module m_umod