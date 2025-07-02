!!  Copyright (C)  Stichting Deltares, 2021-2025.
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

      module merge_step_mod
      contains
      subroutine merge_step( hyd, domain_hyd_coll,maxseg,maxnoq,n_domain,ipnew,iqnew)

      ! function : merges a step in the hydrodynamics

      ! global declarations

      use m_hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)                            :: hyd                   ! description of the hydrodynamics
      type(t_hydrodynamics_collection)                 :: domain_hyd_coll       ! description of the domain hydrodynamics
      integer                                :: maxbnd                ! maximum boundary id
      integer                                :: maxseg                ! maximum segment id
      integer                                :: maxnoq                ! maximum exchange id
      integer                                :: n_domain              ! number of domains
      integer                                :: ipnew(:,:)            ! renumber table segments
      integer                                :: iqnew(maxnoq,n_domain) ! renumber table exchanges

      ! local declarations

      type(t_hydrodynamics), pointer                   :: domain_hyd            ! description of one domain hydrodynamics
      integer                                :: i_domain              ! domain index
      integer                                :: num_layers                 ! number of layers
      integer                                :: ilay                  ! layer index
      integer                                :: iseg                  ! segment index
      integer                                :: isegl                 ! segment index
      integer                                :: iseg_domain           ! segment index
      integer                                :: iq                    ! exchange index
      integer                                :: iq_new                ! exchange index in overall domain

      ! copy to locals for convenience

      do i_domain = 1 , n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iq = 1 , domain_hyd%num_exchanges
            iq_new = iqnew(iq, i_domain)
            if (iq_new.gt.0) then
               hyd%area(iq_new) = domain_hyd%area(iq)
               hyd%flow(iq_new) = domain_hyd%flow(iq)
            endif
         enddo
         do iseg_domain = 1 , domain_hyd%num_cells
            iseg = ipnew(iseg_domain,i_domain)
            hyd%volume(iseg) = domain_hyd%volume(iseg_domain)
            if ( hyd%sal_present ) hyd%sal(iseg) = domain_hyd%sal(iseg_domain)
            if ( hyd%tem_present ) hyd%tem(iseg) = domain_hyd%tem(iseg_domain)
            if ( hyd%tau_present ) hyd%tau(iseg) = domain_hyd%tau(iseg_domain)
            if ( hyd%vdf_present ) hyd%vdf(iseg) = domain_hyd%vdf(iseg_domain)
         enddo
      enddo

      return
      end subroutine merge_step
      end module merge_step_mod
