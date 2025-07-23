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

      subroutine agr_exp(input_hyd, output_hyd, ipnt   )

      use m_hydmod
      implicit none

      type(t_hydrodynamics)          :: input_hyd                           ! description of the input hydrodynamics
      type(t_hydrodynamics)          :: output_hyd                          ! description of the output hydrodynamics
      integer              :: ipnt(input_hyd%num_rows,input_hyd%num_columns) ! aggregation pointer

      ! local declarations

      integer              :: num_columns          ! num_columns
      integer              :: num_rows          ! num_rows
      integer              :: m             ! m index
      integer              :: n             ! n index
      integer              :: iseg          ! segment index
      integer              :: iseg_new      ! segment index in the new grid


      num_columns = input_hyd%num_columns
      num_rows = input_hyd%num_rows

      do m = 1 , num_columns
         do n = 1 , num_rows
            iseg     = input_hyd%lgrid(n,m)
            if ( iseg .gt. 0 ) then
               iseg_new  = (m-1)*num_rows + n
               ipnt(n,m) = iseg_new
            else
               ipnt(n,m) = iseg
            endif
         enddo
      enddo

      return
      end
