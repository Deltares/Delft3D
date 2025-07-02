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

      subroutine from_ddb2(hyd,domain_hyd_coll,parallel,n_mode)

      ! function : set overall hyd with info from the domains

      ! global declarations

      use m_hydmod
      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)               :: hyd                    ! description of the hydrodynamics
      type(t_hydrodynamics_collection)    :: domain_hyd_coll        ! description of all domain hydrodynamics
      logical                   :: parallel               ! parallel option, extra m lines are removed
      logical                   :: n_mode                 ! stack domains in the n direction

      ! local declarations

      integer                   :: n_domain               ! number of domains
      integer                   :: i_domain               ! index in collection
      type(t_hydrodynamics), pointer      :: domain_hyd             ! description of one domain hydrodynamics
      integer                   :: ilay                   ! index in layers
      integer                   :: iwaste                 ! wasteload index
      integer                   :: i_wasteload            ! index in collection
      type(t_wasteload)         :: wasteload              ! one wasteload description
      integer                   :: moffset                ! offset in m direction
      integer                   :: noffset                ! offset in n direction

      type(t_domain)            :: domain                 ! one domain description
      integer                   :: n_dd_bound             ! number of dd-boundaries
      integer                   :: i_dd_bound             ! index in collection
      type(t_dd_bound),pointer  :: dd_bound               ! one dd_bound description

      ! some init

      if ( parallel ) then
         hyd%description(1) = 'merged MPI-based parallel Delft3D FLOW'
      else
         hyd%description(1) = 'merged domains from ddcouple'
      end if
      hyd%description(2) = 'info derived from ddbound file: '
      hyd%horizontal_aggregation  = 1  ! not defiend yet
      hyd%file_dwq%name  = ' '
      hyd%sal_present    = .true.
      hyd%tem_present    = .true.
      hyd%tau_present    = .true.
      hyd%vdf_present    = .true.
      hyd%wasteload_coll%current_size = 0
      hyd%wasteload_coll%maxsize = 0

      ! get properties from the domains

      n_domain = hyd%domain_coll%current_size
      moffset  = 0
      noffset  = 0
      do i_domain = 1 , n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)

         ! some stuff from the first domain

         if ( i_domain .eq. 1 ) then
            hyd%geometry           = domain_hyd%geometry
            hyd%minimum_vdf_used   = domain_hyd%minimum_vdf_used
            hyd%vertical_diffusion = domain_hyd%vertical_diffusion
            hyd%hyd_ref            = domain_hyd%hyd_ref
            hyd%hyd_start          = domain_hyd%hyd_start
            hyd%hyd_stop           = domain_hyd%hyd_stop
            hyd%hyd_step           = domain_hyd%hyd_step
            hyd%cnv_ref            = domain_hyd%cnv_ref
            hyd%cnv_start          = domain_hyd%cnv_start
            hyd%cnv_stop           = domain_hyd%cnv_stop
            hyd%cnv_step           = domain_hyd%cnv_step
            hyd%cnv_step_sec       = domain_hyd%cnv_step_sec

            hyd%num_layers_grid               = domain_hyd%num_layers_grid
            allocate(hyd%hyd_layers(hyd%num_layers_grid))
            do ilay = 1 , hyd%num_layers_grid
               hyd%hyd_layers(ilay) = domain_hyd%hyd_layers(ilay)
            enddo
            hyd%num_layers              = domain_hyd%num_layers
            allocate(hyd%waq_layers(hyd%num_layers))
            do ilay = 1 , hyd%num_layers
               hyd%waq_layers(ilay) = domain_hyd%waq_layers(ilay)
            enddo

         endif

         ! fill in the domain specifics in the overall hyd

         hyd%domain_coll%domain_pnts(i_domain)%num_columns = domain_hyd%num_columns
         hyd%domain_coll%domain_pnts(i_domain)%num_rows = domain_hyd%num_rows
         hyd%domain_coll%domain_pnts(i_domain)%aggr = domain_hyd%file_dwq%name

         ! waste loads

         do iwaste = 1, domain_hyd%wasteload_coll%current_size
            wasteload   = domain_hyd%wasteload_coll%wasteload_pnts(iwaste)
            wasteload%m = wasteload%m + moffset
            wasteload%n = wasteload%n + noffset
            i_wasteload = hyd%wasteload_coll%add(wasteload)
         enddo


         if ( .not. domain_hyd%sal_present ) hyd%sal_present = .false.
         if ( .not. domain_hyd%tem_present ) hyd%tem_present = .false.
         if ( .not. domain_hyd%tau_present ) hyd%tau_present = .false.
         if ( .not. domain_hyd%vdf_present ) hyd%vdf_present = .false.

         if ( n_mode ) then
            if ( parallel ) then
               noffset = noffset + domain_hyd%num_rows - 6
            else
               noffset = noffset + domain_hyd%num_rows
            endif
         else
            if ( parallel ) then
               moffset = moffset + domain_hyd%num_columns - 6
            else
               moffset = moffset + domain_hyd%num_columns
            endif
         endif

      enddo

      ! write the overall hyd file

      hyd%file_hyd%type   = FT_ASC
      hyd%file_hyd%status = 0
      call write_hyd(hyd, parallel)

      return
      end
