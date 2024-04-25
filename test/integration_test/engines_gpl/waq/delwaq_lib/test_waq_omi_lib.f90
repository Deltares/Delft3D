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

! test_waq_omi_lib.f90 --
!     Test program to check that DELWAQ2 works correctly via the OpenMI/Delta-Shell
!
program test_waq_omi_lib
   use waq_omi_interface

   implicit none

   logical :: success
   logical :: varying_volume

   integer :: integration_method
   integer :: i

   integer, dimension(4, 4) :: pointers = &
                               reshape((/-1, 1, 0, 2, &
                                         1, 2, 3, -2, &
                                         2, 3, 1, -2, &
                                         3, -2, 2, 0/), (/4, 4/))
   integer, dimension(4)   :: number_exchanges = (/4, 0, 0, 0/)
   real, dimension(3)      :: salvalue
   real, dimension(3)      :: tmpvalue
   real, dimension(3)      :: oxyvalue
   real, dimension(3)      :: volume
   real, dimension(4)      :: flow
   real, dimension(4)      :: area
   real                    :: dvol_dt
   real                    :: deltt

   varying_volume = .false.
   integration_method = 5

   success = SetIntegrationOptions(integration_method, .true., .true., .true., .false., .false.)

   !
   ! Schematisation:
   ! Three segments of 10x1 km, 10 m deep - volume = 1.0e8 m3
   ! Cross sections: 1 km x 10 m - area = 1.0e4 m2
   ! Flow rate: 0.2 m/s x area = 0.2e4 m3/s
   ! Lengths: 5 km on both sides
   ! Residence time: 1.0e8/0.2e4 = 50000 s
   !
   deltt = 10000.0
   success = SetSimulationTimes(0, 1000000, int(deltt))

   success = DefineWQSchematisation(3, pointers, number_exchanges)

   success = DefineWQDispersion((/100.0, 0.0, 0.0/), &
                                (/5000.0, 5000.0, 5000.0, 5000.0, 5000.0, 5000.0, 5000.0, 5000.0/))

   success = DefineWQProcesses((/'Salinity', 'Temperature', 'OXY'/), 3, 3, &
                               (/'OXYSAT'/), 1, &
                               (/'SaturOXY', 'ReaerOXY'/), 2)
   success = DefineWQExtraOutputParameters((/'OXYSAT'/), 1)

   success = SetCurrentValueScalarInit('Salinity', 30.0)
   success = SetCurrentValueScalarInit('Temperature', 20.0)
   success = SetCurrentValueScalarInit('OXY', 3.0)

   success = DefineDischargeLocations((/1, 2/), 2)

   success = DefineMonitoringLocations((/1, 2, 3/), (/"1", "2", "3"/), 3)
   success = SetOutputTimers(1, 0, 1000000, int(deltt))
   success = SetOutputTimers(2, 0, 1000000, int(deltt))
   success = SetOutputTimers(3, 0, 1000000, int(deltt))

   ! Important: for initialising the mass per segment
   volume = 1.0e8
   success = SetInitialVolume(volume)

   success = ModelInitialize()

   ! Now we can define the volumes, flows and areas
   ! As well as boundary conditions and waste loads
   area = 1.0e4
   flow = 0.2e4

   if (varying_volume) then
      dvol_dt = 0.2e4
      flow = (/0.8e4, 0.6e4, 0.4e4, 0.2e4/)
   end if

   success = SetFlowData(volume, area, flow)
   success = SetWasteLoadValues(1, (/1.0, 0.0, 1000000.0, 0.0/))
!   success = SetWasteLoadValues( 2, (/ 1.0, 1000000.0, 0.0, 0.0 /) )

   success = SetBoundaryConditions(1, (/30.0, 10.0, 3.0/))
   success = SetBoundaryConditions(2, (/0.0, 20.0, 3.0/))

!   success = SetBoundaryConditions( 1, (/ 30.0, 20.0, 3.0 /) )
!   success = SetBoundaryConditions( 2, (/ 30.0, 20.0, 3.0 /) )

   do i = 1, 10
      success = ModelPerformTimeStep()
      success = GetCurrentValue('Salinity', salvalue)
      success = GetCurrentValue('Temperature', tmpvalue)
      success = GetCurrentValue('OXY', oxyvalue)
      write (*, '(i5,3e12.4)') i, salvalue
      write (*, '(5x,3e12.4)') tmpvalue
      write (*, '(5x,3e12.4)') oxyvalue

      if (varying_volume) then
         volume = volume + dvol_dt*deltt
         success = SetFlowData(volume, area, flow)
      end if
   end do

   success = ModelFinalize()

end program test_waq_omi_lib
