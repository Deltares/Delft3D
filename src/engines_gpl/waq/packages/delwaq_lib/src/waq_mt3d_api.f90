!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

!!  Interface to access Delwaq from MT3D
   
   module MT3D
      logical :: MT3D_debug = .false.
   end module

   logical function MT3D_set_debug_status (set_MT3D_debug)
      !DEC$ ATTRIBUTES DLLEXPORT::MT3D_set_debug_status
      !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MT3D_SET_DEBUG_STATUS' :: MT3D_set_debug_status
      
      use MT3D

      implicit none
      
      logical :: set_MT3D_debug
      
      MT3D_debug = set_MT3D_debug

      MT3D_set_debug_status = .true.
   end function

   logical function MT3D_processes_init (number_segments, number_substances, substances, &
                                         number_parameters, process_parameters, process_parameter_values, &
                                         number_processes, processes, deltt_in)
      !DEC$ ATTRIBUTES DLLEXPORT::MT3D_processes_init
      !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MT3D_PROCESSES_INIT' :: MT3D_processes_init
      
      use waq_omi_priv
      use waq_omi_interface
      use MT3D

      implicit none
      
      integer, intent(in)                                        :: number_segments
      integer, intent(in)                                        :: number_substances
      character(len=*), dimension(number_substances), intent(in) :: substances
      integer, intent(in)                                        :: number_parameters
      character(len=*), dimension(number_parameters), intent(in) :: process_parameters
      real, dimension(number_segments, number_parameters), intent(in) :: process_parameter_values
      integer, intent(in)                                        :: number_processes
      character(len=*), dimension(number_processes), intent(in)  :: processes
      real, intent(in)                                           :: deltt_in               ! In days, real
   
      
      integer, parameter      :: integration_method = 0
      
      integer, dimension(1,4) :: pointers = reshape((/ 0, 0, 0, 0  /), (/ 1,4 /))
      integer, dimension(4)   :: number_exchanges = (/ 0, 0, 0, 0 /)
      real, dimension(3)      :: dispersion = (/ 0.0, 0.0, 0.0 /)
      real, dimension(2)      :: dispersion_length = (/ 1.0, 1.0 /) 
      real, dimension(1)      :: flow = (/ 0.0 /)
      real, dimension(1)      :: area = (/ 1.0 /) 
   
      real, allocatable       :: substance_concentrations(:,:)
      real, allocatable       :: substance_boundary_concentrations(:)
      real, allocatable       :: volume(:)
      integer, allocatable    :: substance_mult(:)
      real                    :: dvol_dt
   
      integer                 :: startTime = 0     !< Start time in seconds since the reference date/time
      integer                 :: endTime = huge(0) !< Stop time in seconds since the reference date/time
      integer                 :: deltt             !< Time step in seconds
      
      character(len=8)        :: cseg 
      integer, dimension(number_segments) :: imon 
      character(len=8), dimension(number_segments) :: cmon 

      logical :: success, debug
      integer :: success_int, iseg, isub, ipar, i

      MT3D_processes_init = .false.

      deltt = int(deltt_in * 86400.0e0)     ! From days (real) to seconds integer

      allocate (substance_boundary_concentrations(number_substances))
      allocate (volume(number_segments))
      allocate (substance_mult(number_substances))
      substance_mult = 1
      
      success = SetReferenceDate( 2014, 1, 1, 0, 0, 0 )
      if(.not.success) return
      success = SetIntegrationOptions(integration_method, .true., .true., .true., .false., .false.)
      if(.not.success) return
      success = SetSimulationTimes( startTime, endTime, deltt )
      if(.not.success) return
      success = DefineWQSchematisation( number_segments, pointers, number_exchanges )
      if(.not.success) return

      if (mt3d_debug) then
         cseg = "s0000000"
         do iseg = 1, min(100,number_segments)
            imon(iseg) = iseg
            write(cseg(2:8), '(I7.7)') iseg
            cmon(iseg) = cseg
         end do
         success = DefineMonitoringLocations(imon, cmon, min(100,number_segments))
         if(.not.success) return
      else
         success = DefineMonitoringLocations( (/0/), (/ ' ' /), 0 )
         if(.not.success) return
      endif

      success = DefineWQDispersion(dispersion, dispersion_length ) ! dummy values
      if(.not.success) return
      
      success = DefineWQProcessesX( substances, substance_mult, number_substances, number_substances, &
                                    (/ 'DELT'/), 1 , &
                                   process_parameters, number_parameters,   &
                                   processes, number_processes)
      if(.not.success) return
      success = DefineWQExtraOutputParameters( (/ 'DELT' /), 1 )
      if(.not.success) return
      do isub = 1, number_substances
         success = SetCurrentValueScalarInit( substances(isub), 0.0 )
         if(.not.success) return
      end do
      do ipar = 1, number_parameters
         success = SetCurrentValueFieldInit( process_parameters (ipar), process_parameter_values(1:number_segments,ipar))
         if(.not.success) return
      end do

      if (mt3d_debug) then
         success = SetOutputTimers( 1, 0, endTime, deltt )
         if(.not.success) return
         success = SetOutputTimers( 2, 0, endTime, deltt )
         if(.not.success) return
         success = SetOutputTimers( 3, 0, endTime, deltt )
         if(.not.success) return
      else
         success = SetOutputTimers( 1, endTime, endTime, endTime )
         if(.not.success) return
         success = SetOutputTimers( 2, endTime, endTime, endTime )
         if(.not.success) return
         success = SetOutputTimers( 3, endTime, endTime, endTime )
         if(.not.success) return
      end if
  
      volume  = 1.0e0
      success = SetInitialVolume( volume )
      if(.not.success) return
      success_int = ModelInitialize()
      if(.not.success_int.eq.0) return
      success = SetFlowData( volume, area, flow )
      if(.not.success) return
      substance_boundary_concentrations = 0.0
      success = SetBoundaryConditions( 1, substance_boundary_concentrations )
      if(.not.success) return
   
      MT3D_processes_init = .true.
   end function MT3D_processes_init

   logical function MT3D_processes_setpar (number_parameters, number_segments, process_parameters, process_parameter_values)
      !DEC$ ATTRIBUTES DLLEXPORT::MT3D_processes_setpar
      !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MT3D_PROCESSES_SETPAR' :: MT3D_processes_setpar
      
!      use MT3D_PL
      use waq_omi_interface

      implicit none
      
      integer, intent(in)                                        :: number_parameters
      integer, intent(in)                                        :: number_segments
      character(len=*), dimension(number_parameters), intent(in) :: process_parameters
      real, dimension(number_segments, number_parameters), intent(in) :: process_parameter_values


      logical :: success
      integer :: ipar, i

      MT3D_processes_setpar = .false.

      do ipar = 1, number_parameters
         success = SetCurrentValueFieldInit( process_parameters (ipar), process_parameter_values(1:number_segments,ipar))
         if(.not.success) return
      end do

      MT3D_processes_setpar = .true.
   end function MT3D_processes_setpar
   
   logical function MT3D_processes_step (number_segments, number_substances, substance_concentrations, deltt_in )
      !DEC$ ATTRIBUTES DLLEXPORT::MT3D_processes_step
      !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MT3D_PROCESSES_STEP' :: MT3D_processes_step

!      use MT3D_PL
      use waq_omi_priv
      use waq_omi_interface

      implicit none

      integer, intent(in)                                                 :: number_segments
      integer, intent(in)                                                 :: number_substances
      real, dimension(number_segments, number_substances), intent (inout) :: substance_concentrations
      real, intent(in)                                                    :: deltt_in               ! In days

      integer                 :: startTime = 0   !< Start time in seconds since the reference date/time
      integer                 :: endTime         !< Stop time in seconds since the reference date/time
      integer                 :: deltt           !< Time step in seconds

!      double precision, dimension(number_segments, number_substances) :: substance_concentrations_dbl

      logical :: success
      integer :: isub, i, success_int

      deltt = int(deltt_in * 86400.0e0)     ! In seconds
      startTime = 0
      endTime = huge(0)                     ! no end time until finalization
      success = SetSimulationTimes( startTime, endTime, deltt )
      if(.not.success) return

      MT3D_processes_step = .false.

      do isub = 1, number_substances
         success = SetValuePriv( DLWQ_CONCENTRATION, isub, ODA_ALL_SEGMENTS, substance_concentrations(1:number_segments, isub), dlwq_set )
      end do
      if(.not.success) return
      success_int = ModelPerformTimeStep()
      if(.not.success_int.eq.0) return
      do isub = 1, number_substances
         success = GetValuePriv( DLWQ_CONCENTRATION, isub, ODA_ALL_SEGMENTS, substance_concentrations(1:number_segments, isub) )
         if(.not.success) return
      end do

      MT3D_processes_step = .true.
   end function MT3D_processes_step
   
   logical function MT3D_processes_fin ()
      !DEC$ ATTRIBUTES DLLEXPORT::MT3D_processes_fin
      !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MT3D_PROCESSES_FIN' :: MT3D_processes_fin

      use waq_omi_interface

      implicit none

      integer                 :: success_int

      success_int = ModelFinalize()

      MT3D_processes_fin = success_int.eq.0
   end function MT3D_processes_fin
   
