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

module dfm_error
   implicit none

   integer, parameter :: DFM_NOERR = 0 !< Success code.
   integer, parameter :: DFM_GENERICERROR = 1 !< Error without further details.
   integer, parameter :: DFM_EXIT = 10 !< Exit program without error.
   integer, parameter :: DFM_USERINTERRUPT = 11 !< No error, but execution stopped because of user interrupt.
   integer, parameter :: DFM_MISSINGARGS = 13 !< Missing input arguments
   integer, parameter :: DFM_SIGINT = 121 !< SIGINT signal was give from outside the program (Ctrl-C)
   integer, parameter :: DFM_EFILEFORMAT = -13 !< Error code: File has wrong format.
   integer, parameter :: DFM_WRONGINPUT = 14 !< Error code: Generic error about wrong data in input.
   integer, parameter :: DFM_EXTFORCERROR = 16 !< Error code: Related to external forcing (EC-module etc.)
   integer, parameter :: DFM_NOMODEL = 20 !< No model loaded.
   integer, parameter :: DFM_MODELNOTINITIALIZED = 21 !< Model was empty or not properly initialized.
   integer, parameter :: DFM_INVALIDSTATE = 22 !< Model state has become unphysical
   integer, parameter :: DFM_INVALIDTARGETTIME = 30 !< Invalid target time or timestep requested (via API).
   integer, parameter :: DFM_TIMESETBACK = 31 !< Warning only: time setbacks occurred in step_reduce.
   integer, parameter :: DFM_NOTIMPLEMENTED = 999 !< Generic error that functionality is intended but not yet implemented.

   logical :: do_check_bmi_timestep = .true. !< SH: temporary flag to be able to run both DFlowFM/Wave and 1d2d
   !<     to be removed when 1d2d time step handling is fixed
   !<     (see also routine deactivate_time_step_checking() in unstruc_bmi.f90)

contains
   !> Returns a human-readable error string for a given integer error code.
   subroutine dfm_strerror(errorstring, ierr)
      character(len=*), intent(out) :: errorstring !< The string variable in which the error text will be put.
      integer, intent(in) :: ierr !< The error code for which the error string should be returned.

      select case (ierr)
      case (DFM_NOERR)
         errorstring = 'No error'
      case (DFM_GENERICERROR)
         errorstring = 'Generic error'
      case (DFM_EXIT)
         errorstring = 'Program stopped'
      case (DFM_USERINTERRUPT)
         errorstring = 'User interrupted the model run.'
      case (DFM_MISSINGARGS)
         errorstring = 'Input arguments are missing.'
      case (DFM_SIGINT)
         errorstring = 'Interrupt signal was given from outside the program (Ctrl-C / SIGINT).'
      case (DFM_EFILEFORMAT)
         errorstring = 'Wrong file format.'
      case (DFM_WRONGINPUT)
         errorstring = 'Invalid data in input.'
      case (DFM_NOMODEL)
         errorstring = 'No model loaded.'
      case (DFM_MODELNOTINITIALIZED)
         errorstring = 'Model was not initialized or model is empty.'
      case (DFM_INVALIDSTATE)
         errorstring = 'Model has reached an invalid state.'
      case (DFM_INVALIDTARGETTIME)
         errorstring = 'Invalid target time or timestep requested.'
      case default
         write (errorstring, '(a,i0,a)') 'Unknown error (', ierr, ')'
      end select

   end subroutine dfm_strerror
end module dfm_error
