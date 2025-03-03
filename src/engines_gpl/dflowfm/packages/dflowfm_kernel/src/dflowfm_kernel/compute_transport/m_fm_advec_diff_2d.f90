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

module m_fm_advec_diff_2d

   implicit none

   private

   public :: fm_advec_diff_2d
   
    contains
    
    subroutine fm_advec_diff_2d(var, uadv, qadv, sour, sink, limityp, ierror)
      use m_transport, only: dxiau
      use m_flowgeom, only: ndx, lnx, ln, ba ! static mesh information
      use m_flow, only: ndkx, lnkx, kbot, ktop, lbot, ltop, kmxn, kmxL
      use m_alloc, only: realloc
      use precision, only: dp
      use m_solve_2d, only: solve_2d
      use m_comp_sumhorflux, only: comp_sumhorflux
      use m_comp_fluxhor3d, only: comp_fluxhor3d
      use m_comp_dxiau, only: comp_dxiau
   
      implicit none

      integer, parameter :: NSUBSTEPS = 1 !< number of substeps
      integer, parameter :: NUMCONST = 1 !< number of constituents
      integer, parameter, dimension(NUMCONST) :: JAUPDATECONST = 1 !< update constituent (1) or not (0)
      
      real(kind=dp), dimension(1, ndx), intent(inout) :: var !< variable to be tranported
      real(kind=dp), dimension(lnx), intent(in) :: uadv
      real(kind=dp), dimension(lnx), intent(in) :: qadv
      real(kind=dp), dimension(ndx), intent(in) :: sour
      real(kind=dp), dimension(ndx), intent(in) :: sink
      integer, intent(in) :: limityp !< limiter type (>0) or upwind (0)
      integer, intent(out) :: ierror !< error (1) or not (0)

      integer :: k1, k2

      real(kind=dp), dimension(:, :), allocatable :: fluxhor ! horizontal fluxes
      real(kind=dp), dimension(:, :), allocatable :: fluxver ! vertical   fluxes

      real(kind=dp), dimension(:), allocatable :: dif ! sum of molecular and user-specified diffusion coefficient
      real(kind=dp), dimension(:), allocatable :: sigdif

      real, dimension(:), allocatable :: duml
      real(kind=dp), dimension(:), allocatable :: sqi

      real(kind=dp), dimension(:, :), allocatable :: const_sour ! sources in transport, dim(NUMCONST,ndkx)
      real(kind=dp), dimension(:, :), allocatable :: const_sink ! linear term of sinks in transport, dim(NUMCONST,ndkx)

!  work arrays
      real(kind=dp), dimension(:, :), allocatable :: rhs ! right-hand side, dim(NUMCONST,ndkx)
      integer, dimension(:), allocatable :: jaupdate
      integer, dimension(:), allocatable :: jahorupdate
      integer, dimension(:), allocatable :: ndeltasteps
      real(kind=dp), dimension(:), allocatable :: sumhorflux, dumx, dumy

      integer :: L

      ierror = 1

!  allocate
      call realloc(jaupdate, ndx, keepExisting=.true., fill=1) !Mask array for the 2D part, true for all.
      call realloc(jahorupdate, lnx, keepExisting=.true., fill=1)
      call realloc(ndeltasteps, ndx, keepExisting=.true., fill=1) !It is only used if NSUBSTEPS>1, which is not the case.
      call realloc(sqi, ndx, keepExisting=.true., fill=0d0)

      call realloc(fluxhor, (/1, lnx/), keepExisting=.true., fill=0d0)
      call realloc(fluxver, (/1, ndx/), keepExisting=.true., fill=0d0)

      call realloc(dif, 1, keepExisting=.true., fill=0d0)
      call realloc(sigdif, 1, keepExisting=.true., fill=0d0)

      allocate (duml(1:lnkx), stat=ierror); duml = 0.0

      call realloc(rhs, (/1, ndx/), keepExisting=.true., fill=0d0)

      call realloc(sumhorflux, ndx, keepExisting=.true., fill=0d0)
      call realloc(dumx, ndx, keepExisting=.true., fill=0d0)
      call realloc(dumy, ndx, keepExisting=.true., fill=0d0)

!  construct advective velocity field --> uadv, qadv, mind the orientation (>0 from ln(1,L) to ln(2,L))
      do L = 1, lnx
         k1 = ln(1, L)
         k2 = ln(2, L)
         sqi(k1) = sqi(k1) - min(qadv(L), 0d0)
         sqi(k2) = sqi(k2) + max(qadv(L), 0d0)
      end do

      const_sour=RESHAPE(sour,shape=(/1, ndx/))
      const_sink=RESHAPE(sink,shape=(/1, ndx/))

!  compute horizontal fluxes, explicit part
      call comp_dxiAu()
      call comp_fluxhor3D(NUMCONST, limityp, ndx, lnx, uadv, qadv, sqi, ba, kbot, lbot, ltop, kmxn, kmxL, var, dif, sigdif, duml, NSUBSTEPS, jahorupdate, ndeltasteps, jaupdateconst, fluxhor, dumx, dumy, 1, dxiAu)
      call comp_sumhorflux(1, 0, lnkx, ndkx, lbot, ltop, fluxhor, sumhorflux)
      call solve_2D(1, ndx, ba, kbot, ktop, sumhorflux, fluxver, const_sour, const_sink, 1, jaupdate, ndeltasteps, var, rhs)
      ierror = 0
1234  continue
      return
   end subroutine fm_advec_diff_2d
end module m_fm_advec_diff_2d