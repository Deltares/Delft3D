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

      subroutine sing_dis(hyd)

      ! function : rename discharge names singapore models

      ! (c) DELFT HYDRAULICS

      ! global declarations

      use m_hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)                            :: hyd                   ! description of the hydrodynamics

      ! local declarations

      integer                                :: nowast                ! number of wasteloads
      integer                                :: iwaste                ! wasteload index
      character(len=3)                       :: c3                    ! prefix
      character(len=20)                      :: org_name              ! original discharge name which equals type

      ! some init

      nowast = hyd%wasteload_coll%current_size
      if ( nowast .le. 0 ) return

      ! loop over the wasteloads

      do iwaste = 1 , nowast

         ! add the layer to the name as prefix, this is to make them unique, this will be the name in the hyd file

         write(c3,'(i2.2,a1)') hyd%wasteload_coll%wasteload_pnts(iwaste)%k,'_'
         org_name = hyd%wasteload_coll%wasteload_pnts(iwaste)%name
         hyd%wasteload_coll%wasteload_pnts(iwaste)%name = c3//org_name
         hyd%wasteload_coll%wasteload_pnts(iwaste)%waqtype = org_name

      enddo

      return
      end
