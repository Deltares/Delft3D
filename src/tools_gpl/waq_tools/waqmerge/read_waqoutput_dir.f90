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

      subroutine read_waqoutput_dir(hyd, waq_output_dir)

      ! function : read WAQOutputDir from mdu file

      ! global declarations

      use m_hydmod                   ! module contains everything for the hydrodynamics
      use properties
      implicit none

      ! declaration of the arguments
      type(t_hydrodynamics), intent(in)               :: hyd                  ! description of the overall hydrodynamics
      type(tree_data) , pointer             :: mdu_ptr              ! tree for mdu file

      ! local declarations
      character(len=256), intent(out)       :: waq_output_dir       ! WAQ directory
      logical                               :: waq_output_dir_found ! WAQ directory specified
      integer                               :: istat                ! reading parameter
      character(len=500)                    :: error_message


      waq_output_dir = ''

      nullify(mdu_ptr)
      call tree_create('waqmerge-input', mdu_ptr)

      istat = 0
      call prop_file('ini',trim(hyd%file_hyd%name)//'.mdu', mdu_ptr, istat, error_message)
      if (istat /= 0) then
         select case (istat)
            case(1)
               write(*     ,'(a,a)') '*** ERROR File: '//trim(hyd%file_hyd%name)//'.mdu'//' not found'
            case(3)
               write(*     ,'(a,a)') '*** ERROR Premature EOF in file: '//trim(hyd%file_hyd%name)//'.mdu'
            case default
               write(*     ,'(a,a)') '*** ERROR Read error from file: '//trim(hyd%file_hyd%name)//'.mdu'
         endselect
         write(*,'(a,a)') '*** Error message: ', trim(error_message)

      endif
      call prop_get(mdu_ptr, 'output', 'WAQOutputDir', waq_output_dir,waq_output_dir_found)



      if (.not. waq_output_dir_found) then
         waq_output_dir = 'DFM_DELWAQ_'//trim(hyd%file_hyd%name)
      end if

      write(*,'(a,a)') '*** Directory containing the flow files: ', trim(waq_output_dir)

      return
    end subroutine
