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

      subroutine merge_atr( hyd, domain_hyd_coll,maxbnd,maxseg,n_domain,ipnew)

      ! function : merges the attributes

      ! global declarations

      use dlwq_hyd_data                   ! module contains everything for the hydrodynamics
      use hydmod

      implicit none

      ! declaration of the arguments

      type(t_hyd)                            :: hyd                   ! description of the hydrodynamics
      type(t_hyd_coll)                       :: domain_hyd_coll       ! description of the domain hydrodynamics
      integer                                :: maxbnd                ! maximum boundary id
      integer                                :: maxseg                ! maximum segment id
      integer                                :: n_domain              ! number of domains
      integer                                :: ipnew(-2*maxbnd:maxseg,n_domain)

      ! local declarations

      type(t_hyd), pointer                   :: domain_hyd            ! description of one domain hydrodynamics
      integer                                :: i_domain              ! domain index
      integer                                :: iseg                  ! segment index
      integer                                :: isegl                 ! segment index
      integer                                :: iseg_domain           ! segment index

      do i_domain = 1 , n_domain

         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iseg_domain = 1 , domain_hyd%noseg
            iseg = ipnew(iseg_domain,i_domain)
            hyd%attributes(iseg) = domain_hyd%attributes(iseg_domain)
         enddo
      enddo

      return
      end
