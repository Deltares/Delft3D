!!  Copyright (C)  Stichting Deltares, 2012-2024.
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

! test_waq_model_run.f90 --
!     Test program to check that DELWAQ2 works correctly via the OpenMI/Delta-Shell
!     using preprocessed files from DELWAQ1
!
program test_waq_model_run

   use waq_omi_interface

   implicit none

   integer :: dummy
   logical :: success

   character(len=20) :: runid = 'arjen-ws'

   integer, parameter      :: nosegm = 1736
   real, dimension(nosegm) :: salvalue
   real, dimension(nosegm) :: ctrvalue
   real, dimension(nosegm) :: dtrvalue

   integer                 :: i
   integer, dimension(7)   :: cells = (/56, 230, 514, 722, 946, 1153, 1322/)

   dummy = ModelInitialize_By_Id(runid)

   do i = 1, 10
      dummy = ModelPerformTimeStep()
      success = GetCurrentValue('Salinity', salvalue)
      success = GetCurrentValue('cTr1', ctrvalue)
      success = GetCurrentValue('dTr1', dtrvalue)
      write (*, '(i5,10e12.4)') i, salvalue(cells)
      write (*, '(5x,10e12.4)') ctrvalue(cells)
      write (*, '(5x,10e12.4)') dtrvalue(cells)
   end do

   dummy = ModelFinalize()

end program test_waq_model_run
