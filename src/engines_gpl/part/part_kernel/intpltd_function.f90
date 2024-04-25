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

module intpltd_function_mod
!
!  data definition module(s)
!
use m_waq_precision          ! single/double precision
use timers
!
!  module procedure(s)
!
!
implicit none

contains
    subroutine intpltd_function( lunrep, xData, yData, xVal, yVal )
    ! Inputs: xData = a vector of the x-values of the data to be interpolated
    !         yData = a vector of the y-values of the data to be interpolated
    !         xVal  = a vector of the x-values where interpolation should be performed
    ! Output: yVal  = a vector of the resulting interpolated values
    ! taken from: https://scicomp.stackexchange.com/questions/20960/linear-interpolation-in-fortran

      implicit none

      integer(int_wp ), intent(in)    :: lunrep              ! report file
      real, intent(in)           :: xData(:), yData(:), xVal
      real, intent(out)          :: yVal
      integer                    :: inputIndex, dataIndex
      real                       :: minXData,maxXData, minYdata, xRange, weight

      ! Possible checks on inputs could go here
      ! Things you may want to check:
      ! monotonically increasing xData
      IF(size(xData) .ne. size(yData)) THEN
        write(lunrep,*) 'ERROR wrong X and Y data length for interpolation. Please check!'
        write(*,*) 'ERROR wrong X and Y data length for interpolation. Please check!'
      ENDIF

      minXData = xData(1)
      maxXData = xData(size(xData))
      xRange = maxXData - minXData

      ! possible checks for out of range xVal could go here

      ! this will work if x is incremental, otherwise function for uniformly spaced
      dataIndex = 1
      DO WHILE ((xData(dataIndex+1) < xVal) .and. (xData(dataIndex+1) .le. maxXData))
            dataIndex = dataIndex + 1
      ENDDO

      weight = (xVal - xData(dataIndex))/(xData(dataIndex+1)-xData(dataIndex));
      yVal = (1.0-weight)*yData(dataIndex) + weight*yData(dataIndex+1);

end subroutine
end module
