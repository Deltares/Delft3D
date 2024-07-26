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

      subroutine overall_hyd(waq_output_dir,hyd,n_domain)

      ! function : create overall hyd structure

      ! (c) Deltares

      ! global declarations

      use m_hydmod
      use system_utils, only: makedir
      implicit none

      ! declaration of the arguments

      character(len=256)        :: waq_output_dir         ! directory of output for WAQ
      type(t_hydrodynamics)               :: hyd                    ! description of the hydrodynamics
      integer                   :: n_domain               ! number of domains

      ! local declarations

      character(len=256)        :: name                   ! base name
      character(len=256)        :: name_path              ! base name with path
      character(len=4)          :: sdnm                   ! domain string
      integer                   :: i_domain               ! index in collection
      integer                   :: i_domain1              ! index in collection
      type(t_domain)            :: domain                 ! one domain description
      logical                   :: result                 ! result

      ! set the names
      name              = hyd%file_hyd%name
      name_path         = trim(waq_output_dir)//'/'//trim(name)
      call makedir(trim(waq_output_dir))
      
      hyd%file_com%name = name
      hyd%file_hyd%name = name_path//'.hyd'
      hyd%geometry      = HYD_GEOM_UNSTRUC

      ! get the domains


      hyd%domain_coll%current_size = 0
      hyd%domain_coll%maxsize = 0
      do i_domain = 0, n_domain-1

         write(sdnm, '(i4.4)') i_domain
         domain%name = trim(waq_output_dir)//'/'//trim(name)//'_'//sdnm//'.hyd'
         domain%num_columns = -999
         domain%num_rows = -999
         domain%aggr = ' '
         i_domain1 = hyd%domain_coll%add(domain)
      enddo

      ! some prelim initialisation of hyd

      call set_hyd(hyd,name_path)
      hyd%file_dps%name = ' '
      hyd%task = HYD_TASK_FULL

      return
   end
