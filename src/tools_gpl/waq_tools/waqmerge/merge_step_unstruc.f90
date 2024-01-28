!!  Copyright (C)  Stichting Deltares, 2021-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine merge_step_unstruc( hyd, domain_hyd_coll)

      ! function : merges a step in the hydrodynamics

      ! (c) Deltares

      ! global declarations

      use hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hyd)                            :: hyd                   ! description of the hydrodynamics
      type(t_hyd_coll)                       :: domain_hyd_coll       ! description of the domain hydrodynamics

      ! local declarations

      type(t_hyd), pointer                   :: domain_hyd            ! description of one domain hydrodynamics
      integer                                :: n_domain              ! number of domains
      integer                                :: i_domain              ! domain index
      integer                                :: idmn                  ! flow like domain index (0:n_domain-1)
      integer                                :: dmn                   ! segment flow like domain index (0:n_domain-1)
      integer                                :: nolay                 ! number of layers
      integer                                :: ilay                  ! layer index
      integer                                :: iseg                  ! segment index
      integer                                :: isegl                 ! segment index
      integer                                :: iseg_domain           ! segment index
      integer                                :: iseg_glob             ! segment index
      integer                                :: iq                    ! exchange index
      integer                                :: iq_domain             ! exchange index
      integer                                :: iq_glob               ! exchange index
      integer                                :: iq_global             ! exchange index
      integer                                :: noq1_domain           ! number of exchanges

      ! copy to locals for convenience

      n_domain  = hyd%domain_coll%cursize
      nolay     = hyd%nolay

      do i_domain = 1 , n_domain
         idmn = i_domain - 1
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do isegl = 1 , domain_hyd%nosegl
            iseg_glob = domain_hyd%iglobal(isegl)
            dmn = domain_hyd%idomain(isegl)
            if ( iseg_glob .gt. 0 .and. dmn .eq. idmn) then
               do ilay = 1, nolay
                  iseg_domain = (ilay-1)*domain_hyd%nosegl + isegl
                  iseg        = (ilay-1)*hyd%nosegl + iseg_glob
                  hyd%volume(iseg) = domain_hyd%volume(iseg_domain)
                  if ( hyd%sal_present ) hyd%sal(iseg) = domain_hyd%sal(iseg_domain)
                  if ( hyd%tem_present ) hyd%tem(iseg) = domain_hyd%tem(iseg_domain)
                  if ( hyd%tau_present ) hyd%tau(iseg) = domain_hyd%tau(iseg_domain)
                  if ( hyd%vdf_present ) hyd%vdf(iseg) = domain_hyd%vdf(iseg_domain)
                  if ( ilay .ne. nolay ) then
                     iq_domain = domain_hyd%noq1 + iseg_domain
                     iq_glob   = hyd%noq1        + iseg
                     hyd%area(iq_glob) = domain_hyd%area(iq_domain)
                     hyd%flow(iq_glob) = domain_hyd%flow(iq_domain)
                  endif
               enddo
            endif
         enddo
         noq1_domain = domain_hyd%noq1
         do iq = 1, noq1_domain
            iq_global = domain_hyd%iglobal_link(iq)
            if ( iq_global .gt. 0 ) then
               hyd%area(iq_global) = domain_hyd%area(iq)
               hyd%flow(iq_global) = domain_hyd%flow(iq)
            endif
         enddo
      enddo

      return
      end
