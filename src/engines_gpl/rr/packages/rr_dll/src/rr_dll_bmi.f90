!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

 module rr_dll_bmi

   use iso_c_binding
   use iso_c_utils
   use Network
   use Messages
   use string_module
   use RRmodule, only: RR_ITimestep
   use globals
   use MessageHandling
   use m_rrerror
   use rr_dll_version_module
   use timers

   implicit none
!
   public initialize
   public finalize
   public update

   integer, save :: timerInitialize = 0
   integer, save :: timerFinalize   = 0
   integer, save :: timerUpdate     = 0
   integer, save :: timergetvar     = 0
   integer, save :: timersetvar     = 0
   integer, save :: timerAll        = 0
   integer, save :: timerTimestep   = 0


!  ! This is assumed.....
   integer(c_int), BIND(C, name="MAXDIMS") :: MAXDIMS = 6
   !DEC$ ATTRIBUTES DLLEXPORT :: MAXDIMS
   integer(c_int), BIND(C, name="MAXSTRLEN") :: MAXSTRLEN = 1024
   !DEC$ ATTRIBUTES DLLEXPORT :: MAXSTRLEN

   type(C_PTR) :: x_temp

   real(c_double), target, allocatable, save :: RSLMAP6_bnd_dp(:,:,:)
   real(c_double), target, allocatable, save :: RSLMAP7_plv_dp(:,:,:)
   real(c_double), target, allocatable, save :: BndPar_dp(:)

   real(c_double), parameter :: noBndProvided = -9876.0D+0   ! missing value for incoming water levels
!
!  double precision, target :: t
!  double precision, target :: t_end
!  double precision, target :: t_start
!
!  double precision, target :: arr1(3)
!  integer, target :: arr2(2,3)
!  logical(c_bool), target :: arr3(2,2,3)
   private

   contains

   subroutine disable_exceptions() bind(C, name="disable_exceptions")
      !DEC$ ATTRIBUTES DLLEXPORT::disable_exceptions
      use globals, only: in_f90_runner
      in_f90_runner = .true.
   end subroutine disable_exceptions

   integer(c_int) function initialize(c_configfile) result(ierr) bind(C, name="initialize")
      !DEC$ ATTRIBUTES DLLEXPORT::initialize

      use conf_arr
      use Boundary

      implicit none

      ! Variables
      character(kind=c_char), intent(in) :: c_configfile(*)

      integer, external                   :: SE_Initialize
      character(len=strlen(c_configfile)) :: filename
      double precision                    :: start_time

      call timini()
      timon = .true.
      call timstrt('All', timerAll)
      call timstrt('Initialize', timerInitialize)

      dll_mode = .true.  ! Runs in DLL-Mode and uses BoundaryConditions.bc file
                         ! Will switch to false when old boundary file are used
      dimr_mode = .true.

      ! Initialize, but no logging yet
      call SetMessageHandling(write2screen = .false., lunMessages = 0, useLog = .false., &
                              callback = rrError, reset_counters = .true., thresholdLevel = LEVEL_ERROR)

      ! Convert c string to fortran string
      ! TODO: Read temporary commented out during development
      ! Must be activated when all data comes through files.
      filename = char_array_to_string(c_configfile)

      call create_OES()

      ierr = 0

      ierr = SE_Initialize('RR', filename)

      if (ierr .ne. 0) then
         if (.not. in_f90_runner) then
            call THROWEXCEPTION()
         else
            stop 'Error in SE_Initialize'
         endif
      endif

      RR_ITimestep = 0

      timeSettings%CurrentTimeStep = 0

      ! Force StartTime into globals to be sure
      call get_start_time(start_time)

      ! RR is single precision, create shadow arrays
      allocate(RSLMAP6_bnd_dp(size(RSLMAP6_bnd,1), size(RSLMAP6_bnd,2), size(RSLMAP6_bnd,3)))
      allocate(RSLMAP7_plv_dp(size(RSLMAP7_plv,1), size(RSLMAP7_plv,2), size(RSLMAP7_plv,3)))
      allocate (BndPar_dp(size(BndPar,1)))
      BndPar_dp = noBndProvided

      call timstop(timerInitialize)

   end function initialize

   integer(c_int) function finalize() result(ierr) bind(C, name="finalize")
      !DEC$ ATTRIBUTES DLLEXPORT::finalize

      use Boundary
      use dio_streams
      use globals

      implicit none

      integer                   :: iunit
      logical                   :: isOpened
      integer, external         :: SE_Finalize

      call timstrt('Finalize', timerFinalize)
      ierr = 0

      call closeBoundaryConditionFiles()

      ierr =  SE_Finalize('RR', 'Sobek_3b.fnm')

      call NewTablesDestroy()

      call LanguagesDestroy()

      call DioStreamCloseAllUnits()

      ! Close all left open units
      if (maxStreamUnit >= minStreamUnit) then

         do iunit = minFileUnitNumber, maxFileUnitNumber

            inquire(iunit, opened=isOpened)
            if (isOpened) then
               close(iunit)
            endif

         enddo

      endif

      if (allocated(RSLMAP6_bnd_dp)) deallocate(RSLMAP6_bnd_dp)
      if (allocated(RSLMAP7_plv_dp)) deallocate(RSLMAP7_plv_dp)
      if (allocated(BndPar_dp)) deallocate(BndPar_dp)

      call timstop(timerFinalize)
      call timstop(timerAll)
      call timdump('Timers.out')

   end function finalize

   !> Performs a single timestep with the current model.
   integer(c_int) function update(dt) result(ierr) bind(C,name="update")
      !DEC$ ATTRIBUTES DLLEXPORT::update

      use conf_arr
      use Boundary

      integer, external         :: SE_PerformTimeStep
      integer                   :: i, j, k, nstep
!      integer                   :: ncboun, ncpluv

      real(c_double), value, intent(in) :: dt

      call timstrt('Update', timerUpdate)
      ! copy incoming water levels from bmi arrays into bnd array
      do i = 1, size(BndPar_dp)
         if (BndPar_dp(i) /= noBndProvided) then
            BndPar(i,1) = sngl(BndPar_dp(i))
         endif
      end do
      ! at the first time step copy incoming water levels also into initial water level
      if (timeSettings%CurrentTimeStep .le. 1) then
         do i = 1, size(BndPar_dp)
            if (BndPar_dp(i) /= noBndProvided) then
               BndPar(i, 4) = BndPar(i, 1)
            endif
         end do
      endif

      if (dt <= 0.00) then
         nstep = 1
      else
         nstep = nint(dt/timeSettings%timestepSize)
      endif

      ierr = 0

      do i = 1, nstep
         timeSettings%CurrentTimeStep = timeSettings%CurrentTimeStep + 1
         call timstrt('Timestep', timerTimestep)
         ierr = SE_PerformTimeStep('RR', 'Sobek_3b.fnm')
         call timstop(timerTimestep)
         if (ierr .ne. 0) then
            return
         endif
      enddo

      ! copy results to bmi arrays:
!      ncboun = Network_get_nrboun()
      if (ncboun .gt. 0) then
         do i = 1, size(RSLMAP6_bnd_dp,1)
           do j = 1, size(RSLMAP6_bnd_dp,2)
             do k = 1, size(RSLMAP6_bnd_dp,3)
               RSLMAP6_bnd_dp(i,j,k) = RSLMAP6_bnd(i,j,k)
             end do
           end do
         end do
      endif

!      ncpluv = Network_get_nrboun()
      if (ncpluv .gt. 0) then
         do i = 1, size(RSLMAP7_plv_dp,1)
           do j = 1, size(RSLMAP7_plv_dp,2)
             do k = 1, size(RSLMAP7_plv_dp,3)
               RSLMAP7_plv_dp(i,j,k) = RSLMAP7_plv(i,j,k)
             end do
           end do
         end do
      endif

      ! convert from Q/timestep to Q/s:
!     Jan2022:output is changed to m3/s instead of m3/timestep, so not needed anymore!
!     RSLMAP7_plv_dp(1,:,:) = RSLMAP7_plv_dp(1,:,:)/timeSettings%timestepSize

      ! reset incoming water levels
      BndPar_dp = noBndProvided
      call timstop(timerUpdate)

   end function update

   !> Returns the rank of a variable
   subroutine get_var_rank(c_var_name, rank) bind(C, name="get_var_rank")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var_rank

      use iso_c_binding, only: c_int, c_char
      character(kind=c_char), intent(in) :: c_var_name(*)
      integer(c_int), intent(out) :: rank

      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name

      rank = 1
      return
   end subroutine get_var_rank

   !> Returns the shape of a variable
   subroutine get_var_shape(c_var_name, shape) bind(C, name="get_var_shape")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var_shape

      use iso_c_binding, only: c_int, c_char, c_loc

      character(kind=c_char), intent(in) :: c_var_name(*)
      integer(c_int), intent(inout) :: shape(MAXDIMS)

      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name

      shape = 0
      shape(1) = 1

   end subroutine get_var_shape

   !> Returns the type of a variable as a character string
   subroutine get_var_type(c_var_name, c_type)  bind(C, name="get_var_type")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var_type

      character(kind=c_char), intent(in) :: c_var_name(*)
      character(kind=c_char), intent(out) :: c_type(MAXSTRLEN)
      character(len=MAXSTRLEN) :: type_name, var_name

      ! Use one of the following types
      ! BMI datatype        C datatype        NumPy datatype
      ! BMI_STRING          char*             S<
      ! BMI_INT             int               int16
      ! BMI_LONG            long int          int32
      ! BMI_FLOAT           float             float32
      ! BMI_DOUBLE          double            float64

      type_name = 'double'

      c_type = string_to_char_array(trim(type_name))
   end subroutine get_var_type

   subroutine get_var(c_var_name, xptr) bind(C, name="get_var")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var

      use CONF_ARR

      implicit none

      ! arguments
      character(kind=c_char), intent(in)    :: c_var_name(*)   ! requested variable
      type(c_ptr)           , intent(inout) :: xptr            ! pointer to variable

      ! locals
      integer, parameter      :: path_len = 1024               ! of een andere (via use beschikbare) string size
      integer, parameter      :: id_len   = 128                ! of een andere (via use beschikbare) string size

      character(len=path_len)   :: var_name                   ! var. name, fortran
      double precision, pointer :: value_from_rr => null()    ! pointer to value from the RR kernel

      character(len=path_len) :: var_string                    ! var. name (copy, used to split)
      character(len=id_len)   :: locationType                  ! observations, outflows, etc.
      character(len=id_len)   :: locationID                    ! observation identifier, outflow identifier, etc.
      character(len=id_len)   :: quantityID                    ! requested quantity
      integer                 :: index                         ! index of the observation, outflow, etc.

      double precision, target, save :: localdummy            ! dummy variable to return not-null pointer for water_level

      call timstrt('Get_var', timergetvar)
      ! split the incoming 'locationType/locationId/quantityId'
      var_name = char_array_to_string(c_var_name)
      var_string = var_name
      locationType = splitstr(var_name, id_len, '/')
      locationID   = splitstr(var_name, id_len, '/')
      quantityID   = var_name

      select case(locationType)
      case('catchments')
         index = -1 ! TODO: find index for outflows
         select case(quantityID)
            case('water_discharge')

               call str_upper(locationID, id_len)
               call fndNd2(index, locationID) ! Find index for boundary
               if (index .le. 0) then
                  call ErrMsgStandard (994, 0, ' RR-Outflow '//trim(locationID)//' not found in ', 'get_var')
                  return
               endif

               ! NWRW Node
               if (id_nod2plvnam(index) > 0) then
                  value_from_rr => RSLMAP7_plv_dp(1, id_nod2plvnam(index), 1)
               elseif (id_nod2bndnam(index) > 0) then
                  value_from_rr => RSLMAP6_bnd_dp(1, id_nod2bndnam(index), 1)
               else
                   locationID = trim(locationID) // '_BOUNDARY'
                   index = -1
                   call fndNd2(index, locationID) ! Find index for boundary
                   if (index .le. 0) then
                      call ErrMsgStandard (994, 0, ' RR-Outflow boundary '//trim(locationID)//' not found in ', 'get_var')
                      return
                   endif

                   ! NWRW Node
                   if (id_nod2plvnam(index) > 0) then
                      value_from_rr => RSLMAP7_plv_dp(1, id_nod2plvnam(index), 1)
                   elseif (id_nod2bndnam(index) > 0) then
                      value_from_rr => RSLMAP6_bnd_dp(1, id_nod2bndnam(index), 1)
                   else
                      call ErrMsgStandard (994, 0, ' Node '//trim(locationID)//' is not an RR-Outflow ', 'get_var')
                  endif
               endif
            case('water_level')
               call str_upper(locationID, id_len)
               call fndNd2(index, locationID) ! Find index for boundary
               if (index .le. 0) then
                  call ErrMsgStandard (994, 0, ' RR-Outflow '//trim(locationID)//' not found in ', 'get_var')
                  return
               endif
               if (id_nod2bndnam(index) > 0) then
                  value_from_rr => BndPar_dp(id_nod2bndnam(index))
               else
                  call ErrMsgStandard (994, 0, ' Cannot get quantity '//trim(quantityId)//' on boundary '//trim(locationID)// ' in ', 'get_var')
                  value_from_rr => localdummy ! TODO: remove this when testdata has been updated in
                                              ! https://repos.deltares.nl/repos/DSCTestbench/trunk/cases/e107_dflow1d-drr/f01_general
               endif
            case default
               ! TODO error level
               call ErrMsgStandard (994, 0, trim(var_name) // ' not implemented in ', 'get_var')
         end select
      end select

      if (associated(value_from_rr)) then
        xptr = c_loc(value_from_rr)
      endif

      call timstop(timergetvar)
   end subroutine get_var


   subroutine set_var(c_var_name, xptr) bind(C, name="set_var")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_var
      use Boundary
      use RRmodule
      use MessageHandling

      implicit none

      ! arguments
      character(kind=c_char), intent(in)    :: c_var_name(*)  ! requested variable
      type(c_ptr), value, intent(in)        :: xptr           ! pointer to incoming value for variable

      ! locals
      integer, parameter             :: path_len = 1024       ! of een andere (via use beschikbare) string size
      integer, parameter             :: id_len   = 128        ! of een andere (via use beschikbare) string size

      character(len=path_len)        :: var_name              ! var. name, fortran
      real(c_double), pointer        :: x_1d_double_ptr(:)    ! pointer to incoming value for variable, Fortran
      double precision, dimension(1) :: value_to_rr           ! incoming value as Fortran scalar

      character(len=path_len)        :: var_string            ! var. name (copy, used to split)
      character(len=id_len)          :: locationType          ! observations, outflows, etc.
      character(len=id_len)          :: locationID            ! boundary identifier, etc.
      character(len=id_len)          :: quantityID            ! requested quantity
      integer                        :: index                 ! index of the boundary, etc.
      integer                        :: ibnd                  ! index of the boundary in BndPar

      call timstrt('Set_var', timersetvar)
      var_name = char_array_to_string(c_var_name)

      ! If Debug Level is set, rest can be skipped
      if (var_name == 'debugLevel') then
          call c_f_pointer(xptr, x_1d_double_ptr, (/ 1 /))
          call setMessageHandling(thresholdLevel = nint(x_1d_double_ptr(1)), prefix_logging = "drainfallrunoff")
          isMessLevelSet = .true.
          messLevelSet   = nint(x_1d_double_ptr(1))
          return
       endif

      ! split the incoming 'locationType/locationId/quantityId'
      var_string = var_name
      locationType = splitstr(var_string, id_len, '/')
      locationID   = splitstr(var_string, id_len, '/')
      quantityID   = var_string

      call c_f_pointer(xptr, x_1d_double_ptr, shape(value_to_rr))
      value_to_rr = x_1d_double_ptr(1)

      select case(locationType)
         case('catchments')
            call str_upper(locationID, id_len)
            call fndNd2(index, locationID) ! Find index for boundary
            if (index .le. 0) then
               call ErrMsgStandard (994, 0, ' Boundary '//trim(locationID)//' not found in ', 'set_var')
               return
            endif

            ! TODO: discuss this with GP, also, why 'c04_multiple_catchments' testcase does not have this conversion table filled...??
            ibnd = id_nod2bndnam(index)
            if (ibnd <= 0) then
               call ErrMsgStandard (994, 0, ' Cannot set quantity '//trim(quantityId)//' on boundary '//trim(locationID)// ' in ', 'set_var')
               return
            endif

            select case(quantityID)
               case('water_level')
                  BndPar(ibnd, 1) = sngl(value_to_rr(1))
                  if (timeSettings%CurrentTimeStep .le. 1) BndPar(ibnd, 4) = BndPar(ibnd, 1)
               case default
                  call ErrMsgStandard (994, 0, trim(var_name)//' not implemented in ', 'set_var')
            end select
         case default
            call ErrMsgStandard (994, 0, trim(locationType)//' not implemented in ', 'set_var')
      end select
      call timstop(timersetvar)

   end subroutine set_var

   subroutine get_current_time(current_time) bind(C, name="get_current_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time

      real(c_double)   :: current_time

      current_time = dble(timeSettings%CurrentTimeStep) * dble(timeSettings%timestepSize)

   end subroutine get_current_time

   subroutine get_current_julian_time(current_time)
      use ParallelData, only: JulianTimestep

      real(c_double)   :: current_time

      current_time = dble(timeSettings%CurrentTimeStep) * dble(timeSettings%timestepSize) * JulianTimestep

   end subroutine get_current_julian_time

   subroutine get_start_time(start_time) bind(C, name="get_start_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time

      use rrmodule, only: JulianStartDate

      real(c_double) :: start_time

      double Precision, external :: modified_julian_fromJulian

      start_time = 0.0d0

      julStart = Modified_Julian_fromJulian(JulianStartDate)

   end subroutine get_start_time

   subroutine get_end_time(end_time) bind(C, name="get_end_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time

      use rrmodule, only: Lasttm, Frsttm, JulianTimestep
      use time_module

      real(c_double) :: end_time

      end_time = dble ((Lasttm - Frsttm + 1) * JulianTimestep * 86400.D0)

   end subroutine get_end_time

   subroutine get_time_step(time_step) bind(C, name="get_time_step")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step

      real(c_double)   :: time_step

      time_step = dble(timeSettings%timestepSize)

   end subroutine get_time_step


   subroutine get_attribute(c_att_name, c_att_value) bind(C, name="get_attribute")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_attribute
      use messageHandling

      character(kind=c_char), intent(in)    :: c_att_name(MAXSTRINGLEN)  !< Attribute name as C-delimited character string.
      character(kind=c_char), intent(  out) :: c_att_value(MAXSTRINGLEN) !< Returned attribute value as C-delimited character string.

      character(len=strlen(c_att_name)) :: att_name
      character(len=idLen) :: att_value
      character(len=idLen) :: svn_value
      ! Store the name

      att_name = char_array_to_string(c_att_name)

      select case(trim(att_name))
      case ('version')
         call getversionnumberstring_rr_dll(att_value)
         call getsvnrevisionstring_rr_dll(svn_value)
         att_value = trim(att_value)//'.'//trim(svn_value)
       case default
         call SetMessage(LEVEL_ERROR, trim(att_name) // ' not yet implemented ')
         return
      end select
      c_att_value = string_to_char_array(trim(att_value))

   end subroutine get_attribute

end module rr_dll_bmi
