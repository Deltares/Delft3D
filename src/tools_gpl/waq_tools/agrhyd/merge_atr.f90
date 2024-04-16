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

      subroutine merge_atr( hyd, domain_hyd_coll)

      ! function : merges the attributes

      ! (c) DELFT HYDRAULICS

      ! global declarations

      use m_hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)                            :: hyd                   ! description of the hydrodynamics
      type(t_hydrodynamics_collection)                 :: domain_hyd_coll       ! description of the domain hydrodynamics

      ! local declarations

      type(t_hydrodynamics), pointer                   :: domain_hyd            ! description of one domain hydrodynamics
      integer                                :: n_domain              ! number of domains
      integer                                :: i_domain              ! domain index
      integer                                :: nolay                 ! number of layers
      integer                                :: ilay                  ! layer index
      integer                                :: iseg                  ! segment index
      integer                                :: isegl                 ! segment index
      integer                                :: iseg_domain           ! segment index
      integer                                :: isoff                 ! segment offset

      ! copy to locals for convenience

      n_domain  = hyd%domain_coll%current_size
      nolay     = hyd%nolay

      isoff  = 0
      do i_domain = 1 , n_domain

         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do ilay = 1 , nolay
            do isegl = 1 , domain_hyd%nosegl
               iseg_domain = (ilay-1)*domain_hyd%nosegl + isegl
               iseg        = (ilay-1)*hyd%nosegl + isegl + isoff
               hyd%attributes(iseg) = domain_hyd%attributes(iseg_domain)
            enddo
         enddo

         isoff  = isoff  + domain_hyd%nosegl

      enddo

      return
      end
