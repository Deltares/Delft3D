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

module m_ade2d

   implicit none

   private

   public :: fm_ade2d
   
    contains
    
    subroutine fm_ade2d(thevar, uadv, qadv, sour, sink, limityp, ierror)
      use m_transport, only: dxiau
      use m_flowgeom, only: Ndx, Lnx, ln, ba ! static mesh information
      use m_flow, only: Ndkx, Lnkx, kbot, ktop, Lbot, Ltop, kmxn, kmxL
      use m_alloc, only: realloc
      use precision, only: dp
      use m_solve_2d, only: solve_2d
      use m_comp_sumhorflux, only: comp_sumhorflux
      use m_comp_fluxhor3d, only: comp_fluxhor3d
      use m_comp_dxiau, only: comp_dxiau
   
      implicit none

      integer, parameter :: BFNSUBSTEPS = 1
      integer, parameter :: NUMCONST = 1
      integer, parameter, dimension(NUMCONST) :: JAUPDATECONST = 1 !< update constituent (1) or not (0)
      
      real(kind=dp), dimension(1, ndx), intent(inout) :: thevar !< variable to be tranported
      real(kind=dp), dimension(lnx), intent(in) :: uadv
      real(kind=dp), dimension(lnx), intent(in) :: qadv
      real(kind=dp), dimension(ndx), intent(in) :: sour
      real(kind=dp), dimension(ndx), intent(in) :: sink
      integer, intent(in) :: limityp !< limiter type (>0) or upwind (0)
      integer, intent(out) :: ierror !< error (1) or not (0)

      integer :: k1, k2

      real(kind=dp), dimension(:, :), allocatable :: fluxhorbf ! horizontal fluxes
      real(kind=dp), dimension(:, :), allocatable :: fluxverbf ! vertical   fluxes

      real(kind=dp), dimension(:), allocatable :: difsedubf ! sum of molecular and user-specified diffusion coefficient
      real(kind=dp), dimension(:), allocatable :: sigdifibf

      real, dimension(:), allocatable :: dumL
      real(kind=dp), dimension(:), allocatable :: bfsqi

      real(kind=dp), dimension(:, :), allocatable :: const_sourbf ! sources in transport, dim(NUMCONST,Ndkx)
      real(kind=dp), dimension(:, :), allocatable :: const_sinkbf ! linear term of sinks in transport, dim(NUMCONST,Ndkx)

!  work arrays
      real(kind=dp), dimension(:, :), allocatable :: rhsbf ! right-hand side, dim(NUMCONST,Ndkx)
      integer, dimension(:), allocatable :: jabfupdate
      integer, dimension(:), allocatable :: jabfhorupdate
      integer, dimension(:), allocatable :: nbfdeltasteps
      real(kind=dp), dimension(:), allocatable :: bfsumhorflux, dumx, dumy

      integer :: L

      ierror = 1

!  allocate
      call realloc(jabfupdate, ndx, keepExisting=.true., fill=1) !Mask array for the 2D part, true for all.
      call realloc(jabfhorupdate, lnx, keepExisting=.true., fill=1)
      call realloc(nbfdeltasteps, ndx, keepExisting=.true., fill=1) !It is only used if NSUBSTEPS>1, which is not the case.
      call realloc(bfsqi, ndx, keepExisting=.true., fill=0d0)

      call realloc(fluxhorbf, (/1, Lnx/), keepExisting=.true., fill=0d0)
      call realloc(fluxverbf, (/1, Ndx/), keepExisting=.true., fill=0d0)

      call realloc(difsedubf, 1, keepExisting=.true., fill=0d0)
      call realloc(sigdifibf, 1, keepExisting=.true., fill=0d0)

      allocate (dumL(1:lnkx), stat=ierror); dumL = 0.0

      call realloc(const_sourbf, (/1, Ndx/), keepExisting=.true., fill=0d0)
      call realloc(const_sinkbf, (/1, Ndx/), keepExisting=.true., fill=0d0)
      call realloc(rhsbf, (/1, Ndx/), keepExisting=.true., fill=0d0)

      call realloc(bfsumhorflux, Ndx, keepExisting=.true., fill=0d0)
      call realloc(dumx, Ndx, keepExisting=.true., fill=0d0)
      call realloc(dumy, Ndx, keepExisting=.true., fill=0d0)

!  construct advective velocity field --> uadv, qadv, mind the orientation (>0 from ln(1,L) to ln(2,L))
      !qadv=uadv
      do L = 1, Lnx
         k1 = ln(1, L)
         k2 = ln(2, L)
         bfsqi(k1) = bfsqi(k1) - min(qadv(L), 0d0)
         bfsqi(k2) = bfsqi(k2) + max(qadv(L), 0d0)
      end do

      const_sourbf=RESHAPE(sour,shape=(/1, ndx/))
      const_sinkbf=RESHAPE(sink,shape=(/1, ndx/))

!  compute horizontal fluxes, explicit part
      call comp_dxiAu()
      call comp_fluxhor3D(NUMCONST, limityp, Ndx, Lnx, uadv, qadv, bfsqi, ba, kbot, Lbot, Ltop, kmxn, kmxL, thevar, difsedubf, sigdifibf, dumL, BFNSUBSTEPS, jabfhorupdate, nbfdeltasteps, jaupdateconst, fluxhorbf, dumx, dumy, 1, dxiAu)
      call comp_sumhorflux(1, 0, Lnkx, Ndkx, Lbot, Ltop, fluxhorbf, bfsumhorflux)
      call solve_2D(1, Ndx, ba, kbot, ktop, bfsumhorflux, fluxverbf, const_sourbf, const_sinkbf, 1, jabfupdate, nbfdeltasteps, thevar, rhsbf)
      ierror = 0
1234  continue
      return
   end subroutine fm_ade2d
end module m_ade2d