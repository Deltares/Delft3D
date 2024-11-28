!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

module m_setrho

implicit none

private

public :: setrho, setrhofixedp, setrhokk

contains

!> fill rho of one column
subroutine setrhokk(kk) 
   use m_flow, only: rho, density_is_pressure_dependent, kmxn
   use m_get_kbot_ktop

   integer :: kk
   integer :: kb, kt, k

   double precision :: p0

   call getkbotktop(kk, kb, kt)
   if (kt < kb) return

   if (.not. density_is_pressure_dependent()) then
      do k = kb, kt
         rho(k) = setrho(k, p0)
      end do
   else
      p0 = 0d0 ! surface value is 0 bar in unesco, not 1 bar
      do k = kt, kb, -1
         rho(k) = setrho(k, p0)
      end do
   end if

   do k = kt + 1, kb + kmxn(kk) - 1
      rho(k) = rho(kt)
   end do

end subroutine setrhokk

!> set density in a cell
double precision function setrho(cell, p0)

   use m_flow
   use m_sediment
   use sediment_basics_module, only: has_advdiff
   use m_transport
   use m_turbulence, only: rhowat
   use unstruc_messages, only: mess, LEVEL_ERROR
   use m_densfm, only: densfm

   implicit none

   integer, intent(in) :: cell !< cell number
   double precision, intent(inout) :: p0 !< in as cell ceiling pressure, out as cell floorpressure (pascal)
   double precision :: rhok !< in as previous density, reduces required nr of iterations
   double precision, parameter :: rhom_min = 990d0 !< lower limit of rhom [kg/m3]
   double precision, parameter :: rhom_max = 1250d0 !< upper limit of rhom [kg/m3]
   integer :: i
   double precision :: sal, temp, p1, dzz

   call getsaltemk(cell, sal, temp)

   if (.not. density_is_pressure_dependent()) then
      setrho = densfm(sal, temp, p0)
   else
      dzz = zws(cell) - zws(cell - 1)
      rhok = rho(cell)
      do i = 1, Maxitpresdens
         p1 = p0 + ag * dzz * rhok
         rhok = densfm(sal, temp, 0.5d0 * (p1 + p0))
      end do
      setrho = rhok
      p0 = p1
   end if

   call add_sediment_effect_to_density(setrho, cell)

   setrho = min(setrho, rhom_max) ! check overshoots at thin water layers
   setrho = max(setrho, rhom_min) !

end function setrho

double precision function setrhofixedp(k, p0)
   use m_densfm, only: densfm

   implicit none

   integer, intent(in) :: k !< cell number
   double precision, intent(in) :: p0 !< some given pressure

   double precision :: sal, temp

   call getsaltemk(k, sal, temp)

   setrhofixedp = densfm(sal, temp, p0)

   call add_sediment_effect_to_density(setrhofixedp, k)

end function setrhofixedp

subroutine getsaltemk(k, sal, temp)
   use m_flow
   use m_transport

   implicit none
   integer :: k
   double precision :: sal, temp

   if (jasal > 0) then
      saL = max(0d0, constituents(isalt, k))
   else
      saL = backgroundsalinity
   end if

   if (jatem > 0) then
      temp = max(-5d0, constituents(itemp, k))
   else
      temp = backgroundwatertemperature
   end if
end subroutine getsaltemk

!> Adds the effect of sediment on the density of a cell
subroutine add_sediment_effect_to_density(rho, cell)
   use m_sediment, only: jased, jaseddenscoupling, jasubstancedensitycoupling, mxgr, rhosed, sed, stmpar, stm_included
   use m_transport, only: constituents, ised1, itra1, itran
   use m_turbulence, only: rhowat
   use sediment_basics_module, only: has_advdiff
   use unstruc_messages, only: LEVEL_ERROR, mess
   use unstruc_model, only: check_positive_value

   implicit none

   double precision, intent(inout) :: rho !< density in a cell [kg/m3]
   integer, intent(in) :: cell !< cell index
   double precision, parameter :: rhom_min = 990d0 !< lower limit of rhom [kg/m3]
   double precision, parameter :: rhom_max = 1250d0 !< upper limit of rhom [kg/m3]
   double precision, parameter :: SEDIMENT_DENSITY = 2600d0 !< default/typical sediment density [kg/m3]
   double precision :: rhom !< density in a cell [kg/m3] before adding sediment effects
   integer :: i, lsed !< loop indices

   if (jased > 0 .and. stm_included) then
      rhom = rho ! UNST-5170 for mor, only use salt+temp, not sediment effect
      rhom = min(rhom, rhom_max) ! check overshoots at thin water layers
      rhom = max(rhom, rhom_min) !
      rhowat(cell) = rhom
      if (stmpar%morpar%densin) then ! sediment density effects
         i = ised1
         rhom = rho
         do lsed = 1, stmpar%lsedtot
            if (has_advdiff(stmpar%sedpar%tratyp(lsed))) then ! has suspended component
               rho = rho + constituents(i, cell) * (stmpar%sedpar%rhosol(lsed) - rhom) / stmpar%sedpar%rhosol(lsed)
               i = i + 1
            end if
         end do
      end if
   else if (jasubstancedensitycoupling > 0) then ! for now, only works for DELWAQ sediment fractions (concentrations in g/m3 and density of SEDIMENT_DENSITY)
      if (itra1 == 0) then
         call mess(LEVEL_ERROR, 'SubstanceDensityCoupling was set to 1, but there are no substances.')
      end if
      rhom = rho
      do i = itra1, itran
         rho = rho + (1d-3) * constituents(i, cell) * (SEDIMENT_DENSITY - rhom) / SEDIMENT_DENSITY
      end do
   else if (jaseddenscoupling > 0) then ! jased < 4
      rhom = rho
      do i = 1, mxgr
         call check_positive_value('rhosed', rhosed(i))
         rho = rho + sed(i, cell) * (rhosed(i) - rhom) / rhosed(i)
      end do

   end if
end subroutine add_sediment_effect_to_density

end module m_setrho
