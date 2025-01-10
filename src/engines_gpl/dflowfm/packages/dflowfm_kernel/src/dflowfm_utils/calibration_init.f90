!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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

! Initialise calibration factors
module m_calibration_init

   implicit none

   private

   public :: calibration_init

contains

   subroutine calibration_init()
      use m_calibration
      use unstruc_messages
      use unstruc_model, only: md_cldfile, md_cllfile
      use m_flowgeom, only: lnx, lnx1d
      use m_flow, only: ifrcutp, ifrctypuni
      !
      integer :: LF
      logical :: error

      ! Skip 1d friction types. For now the ifrcutp is always equal to 1, while FRCU contains the actual (calculated) Chezy value
      do LF = lnx1D + 1, lnx
         if (ifrcutp(LF) /= ifrctypuni) then
            error = .true.
            call mess(LEVEL_ERROR, 'Only uniform background roughness definition supported in combination with [calibration]DefinitionFile')
         end if
      end do

      call read_cldfile(md_cldfile, clddata, GET_DIMENSIONS)
      call read_cldfile(md_cldfile, clddata, FILL_DATA)
      !
      call read_cllfile(md_cllfile, clddata, GET_DIMENSIONS)
      call read_cllfile(md_cllfile, clddata, FILL_DATA)
      !
      ! TO DO: check for obs en crs in domain (similar to trachy code below)

      ! ! TO DO: Update to allow cross-sections for parts outside domain which are inactive
                    !!itrt = trachy_fl%gen%ittdef(trachy_fl%gen%crs(itrtcrs)%itrt,1)
      !
                    !!cld_in_arl =  .false.
                    !!itt = 0
                    !!do while ((.not. trt_in_arl) .and. (itt < trachy_fl%dir(1)%nttaru))
                    !!   itt = itt + 1
                    !!   if (trachy_fl%dir(1)%ittaru(itt,3) == itrt) then    ! if trachytope is included in .arl file
                    !!      trt_in_arl = .true.
                    !!   end if
                    !!enddo
                    !!
                    !! TO DO: move to after reading cll ?
      !if ((clddata%crs(icldcrs) == intmiss) .and. cld_in_arl) then
      !    call mess(LEVEL_ERROR, 'Error reading calibration definition file: Cross-section does not exist in "'//trim(md_cldfile)//'": '//rec132)
      !    error = .true.
      !    goto 9999
      !end if
      !
      !
   end subroutine calibration_init

end module m_calibration_init
