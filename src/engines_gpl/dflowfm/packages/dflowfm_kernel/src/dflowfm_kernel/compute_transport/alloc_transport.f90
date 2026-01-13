!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

!> allocate transport arrays
module m_alloc_transport

   use precision, only: dp
   implicit none

   private

   public :: alloc_transport

contains

   subroutine alloc_transport(Keepexisting)
      use m_flowgeom, only: Ndx, Lnx
      use m_flow, only: Lnkx, Ndkx, kmx, sigdifi, wsf
      use m_fm_wq_processes
      use m_transport
      use m_alloc
      use m_meteo, only: numtracers, numfracs, item_sourcesink_constituent_delta
      use fm_external_forcings_data, only: numsrc, qcsrc, vcsrc, wstracers
      use m_sediment, only: stm_included
      use m_ec_module, only: ec_undef_int
      implicit none

      logical, intent(in) :: KeepExisting !< keep existing data (true) or not (false)

!  allocate and initialize fluxes

      call realloc(fluxhor, [NUMCONST, Lnkx], keepExisting=KeepExisting, fill=0.0_dp)
      call realloc(fluxver, [NUMCONST, Ndkx], keepExisting=KeepExisting, fill=0.0_dp)

      call realloc(fluxhortot, [NUMCONST, Lnkx], keepExisting=KeepExisting, fill=0.0_dp)
      call realloc(sinksetot, [NUMCONST, Ndx], keepExisting=KeepExisting, fill=0.0_dp)
      call realloc(sinkftot, [NUMCONST, Ndx], keepExisting=KeepExisting, fill=0.0_dp)

      call realloc(difsedu, NUMCONST, keepExisting=KeepExisting, fill=0.0_dp)
      call realloc(molecular_diffusion_coeff, NUMCONST, keepExisting=KeepExisting, fill=0.0_dp)
      call realloc(sigdifi, NUMCONST, keepExisting=KeepExisting, fill=0.0_dp)
      call realloc(wsf, NUMCONST, keepExisting=.true., fill=0.0_dp)

      call realloc(constituents, [NUMCONST, Ndkx], keepExisting=KeepExisting, fill=0.0_dp)

      call realloc(const_sour, [NUMCONST, Ndkx], keepExisting=KeepExisting, fill=0.0_dp)
      call realloc(const_sink, [NUMCONST, Ndkx], keepExisting=KeepExisting, fill=0.0_dp)

      call realloc(dsedx, [NUMCONST, Ndkx], keepExisting=KeepExisting, fill=0.0_dp)
      call realloc(dsedy, [NUMCONST, Ndkx], keepExisting=KeepExisting, fill=0.0_dp)

      call realloc(thetavert, NUMCONST, keepExisting=KeepExisting, fill=0.0_dp)
      !call realloc(wstracers, NUMCONST, keepExisting=KeepExisting, fill=0d0)
      call realloc(wstracers, NUMCONST, keepExisting=.true., fill=0.0_dp)

      call realloc(const_names, NUMCONST, keepExisting=KeepExisting, fill='')
      call realloc(const_units, NUMCONST, keepExisting=KeepExisting, fill='')

      call realloc(id_const, [2, NUMCONST], keepExisting=KeepExisting, fill=0)

      call realloc(sumhorflux, [NUMCONST, Ndkx], keepExisting=.false., fill=0.0_dp)
      call realloc(ndeltasteps, Ndx, keepExisting=.false., fill=1)
      call realloc(jaupdate, Ndx, keepExisting=.false., fill=1)
      call realloc(jaupdatehorflux, Lnx, keepExisting=.false., fill=1)
      call realloc(dtmax, Ndx, keepExisting=.false., fill=0.0_dp)

      if (stm_included) then
         call realloc(u1sed, Lnkx, keepExisting=.false., fill=0.0_dp)
         call realloc(q1sed, Lnkx, keepExisting=.false., fill=0.0_dp)
      end if

      if (jalimitdtdiff == 1) then
         call realloc(sumdifflim, Ndkx, keepExisting=.false., fill=0.0_dp)
      end if
      call realloc(dxiAu, Lnkx, keepExisting=.false., fill=0.0_dp)

      call realloc(jaupdateconst, NUMCONST, keepExisting=.false., fill=1)
      if (stm_included) then
         call realloc(noupdateconst, NUMCONST, keepExisting=.false., fill=0)
      end if

!  work arrays
      if (allocated(rhs)) then
         deallocate (rhs)
      end if
      allocate (rhs(NUMCONST, Ndkx))

      if (kmx > 0) then ! 3D
         if (allocated(a)) then
            deallocate (a, b, c, d, e, sol)
         end if
         allocate (a(kmx, NUMCONST), b(kmx, NUMCONST), c(kmx, NUMCONST), d(kmx, NUMCONST), e(kmx), sol(kmx))
         a = 0.0_dp
         b = 0.0_dp
         c = 0.0_dp
         d = 0.0_dp
         e = 0.0_dp
      end if

!  tracer boundary condition
      call realloc(itrac2const, numtracers, keepExisting=KeepExisting, fill=0)
      call realloc(ifrac2const, numfracs, keepExisting=KeepExisting, fill=0)

      call realloc(qcsrc, [NUMCONST, numsrc], keepExisting=.false., fill=0.0_dp)
      call realloc(vcsrc, [2 * NUMCONST, numsrc], keepExisting=.false., fill=0.0_dp)

      if (jawaqproc > 0) then
!     WAQ
         call realloc(isys2const, num_substances_total, keepExisting=.true., fill=0)
      end if
      call realloc(iconst2sys, NUMCONST, keepExisting=.true., fill=0)
      call realloc(item_sourcesink_constituent_delta, NUMCONST, keepExisting=.true., fill=ec_undef_int)
   end subroutine alloc_transport

end module m_alloc_transport
