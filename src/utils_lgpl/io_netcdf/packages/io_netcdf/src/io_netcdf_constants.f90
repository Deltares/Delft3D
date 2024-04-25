!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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

! 
! 


!> The parameters and error codes for io_netcdf, I/O based on selected NetCDF conventions (UGRID, and more in the future).
!! @see io_ugrid
module ionc_constants
use netcdf, only: NF90_NOERR

implicit none

!
! Parameters
!
public :: IONC_CONV_NULL
public :: IONC_CONV_CF
public :: IONC_CONV_UGRID
public :: IONC_CONV_SGRID
public :: IONC_CONV_OTHER

!
! Error codes
!
public :: IONC_NOERR
public :: IONC_EBADID
public :: IONC_ENOPEN
public :: IONC_ENOMEM
public :: IONC_ENONCOMPLIANT
public :: IONC_ENOTAVAILABLE
public :: IONC_ENOTATT
public :: IONC_ENOTVAR
public :: IONC_ENOTDIM

private

!
! NetCDF conventions support. Based on the conventions used in the file,
! this library supports a bigger or smaller amount of intelligent inquiry functions.
! Note: a data set may adhere to multiple conventions at the same time.
! This is stored as an integer sum of the respective convention types. Therefore,
! all types below must be powers of two, to allow all combinations in one number.
!
integer, parameter :: IONC_CONV_NULL  = 0   !< Dataset conventions not yet detected
integer, parameter :: IONC_CONV_CF    = 1   !< Dataset adhering to Climate and Forecast (CF) conventions
integer, parameter :: IONC_CONV_UGRID = 2   !< Dataset adhering to UGRID-conventions
integer, parameter :: IONC_CONV_SGRID = 4   !< Dataset adhering to SGRID-conventions
integer, parameter :: IONC_CONV_OTHER = -99 !< Dataset adhering to unknown or unsupported conventions (user should fall back to NetCDF native API calls)

integer, public, parameter :: MAXSTRLEN = 255 !< Max string length (e.g. for inquiring attribute values.

!
! Error statuses
!
integer, parameter :: IONC_NOERR         = NF90_NOERR !< Successful: same error code as native NetCDF library.
integer, parameter :: IONC_EBADID        = -2001 !< Not a valid IONC dataset id.
integer, parameter :: IONC_ENOPEN        = -2002 !< File could not be opened.
integer, parameter :: IONC_ENOMEM        = -2003 !< Memory allocation error.
integer, parameter :: IONC_ENONCOMPLIANT = -2004 !< File is non-compliant with its specified conventions.
integer, parameter :: IONC_ENOTAVAILABLE = -2005 !< Requested function is not available, because the file has different conventions.
integer, parameter :: IONC_ENOTATT       = -1018 !< Some attribute was not found. Probably due to a native NF90_ENOTATT.
integer, parameter :: IONC_ENOTVAR       = -1015 !< Some variable was not found. Probably due to a native NF90_ENOTVAR.
integer, parameter :: IONC_ENOTDIM       = -1019 !< Some dimension was not found. Probably due to a native NF90_EBADDIM.

end module ionc_constants
