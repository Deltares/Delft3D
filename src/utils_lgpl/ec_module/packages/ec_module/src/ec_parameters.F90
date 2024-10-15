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
!

!> This module contains the Ec-module's enumerations and constants.
!! @author arjen.markus@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.spee@deltares.nl
module m_ec_parameters
   use precision

   implicit none

   integer, parameter :: maxNameLen = 256
   integer, parameter :: maxFileNameLen = 256
   integer, parameter :: maxFileReaderFiles = 3
#ifdef _WIN64
   integer :: maxFileUnits = 8000   !< maximum on Windows 10 (8000 < 8192)
#else
   integer :: maxFileUnits = 2000
#endif
   integer, parameter :: numberOfTargetItems = 4

   integer, parameter :: EC_MISSING_VALUE = -999
   integer, parameter :: ec_undef_int = -987
   real(fp), parameter :: ec_undef_fp = -9.87e+05_fp
   real(hp), parameter :: ec_undef_hp = -9.87e+10_hp
   real(fp), parameter :: ec_huge_fp = 3.402823466e+38_fp
   real(hp), parameter :: ec_huge_hp = 1.7976931348623158e+308_hp

   integer, parameter :: ec_second = 1
   integer, parameter :: ec_minute = 2
   integer, parameter :: ec_hour = 3
   integer, parameter :: ec_day = 4

   !> Enumeration for tEcElementSet role
   integer, parameter :: elmSetType_undefined = 0
   integer, parameter :: elmSetType_Cartesian = 1
   integer, parameter :: elmSetType_Cartesian_equidistant = 2
   integer, parameter :: elmSetType_spheric = 3
   integer, parameter :: elmSetType_spheric_equidistant = 4
   integer, parameter :: elmSetType_Ids = 5
   integer, parameter :: elmSetType_Grib = 6
   integer, parameter :: elmSetType_spw = 7
   integer, parameter :: elmSetType_cart_gk = 8
   integer, parameter :: elmSetType_scalar = 9 ! This is a single point, without a location.
   integer, parameter :: elmSetType_polytim = 10
   integer, parameter :: elmSetType_samples = 11
   integer, parameter :: elmSetType_spheric_ortho = 12! A spherical element set the lat, lon of which are one dimension (no cross-product array)
   integer, parameter :: elmSetType_Cartesian_ortho = 13! A cartesian element set the lat, lon of which are one dimension (no cross-product array)

   !> Enumeration for tEcItem role
   integer, parameter :: itemType_undefined = 0
   integer, parameter :: itemType_source = 1
   integer, parameter :: itemType_target = 2

   !> enumeration for tEcFileReader filetypes
   integer, parameter :: provFile_undefined = 0
   integer, parameter :: provFile_uniform = 1  !< kx values each timestep 1 dim arr       uni
   integer, parameter :: provFile_unimagdir = 2  !< 2 values each timestep; magnitude, direction
   integer, parameter :: provFile_svwp = 3  !< 3 fields each timestep 3 dim array      noint
   integer, parameter :: provFile_svwp_weight = 4  !< 3 fields each timestep 3 dim array      noint
   integer, parameter :: provFile_arcinfo = 5  !< 1 field each timestep 2 dim array        bilin/direct
   integer, parameter :: provFile_spiderweb = 6  !< 3 field each timestep 3 dim array        bilin/spw
   integer, parameter :: provFile_curvi = 7  !< 1 field each timestep 2 dim array        bilin/findnm
   integer, parameter :: provFile_curvi_weight = 8  !< 1 field each timestep 2 dim array        bilin/findnm
   integer, parameter :: provFile_samples = 9  !< 1 field each timestep, ascii file with x y z samples   method may be triangulation/averaging
   integer, parameter :: provFile_triangulationmagdir = 10  !< 2 fields u,v each tijdstap 3 dim array  triang, vectormax = 2
                                                            !! based on timeseries of wind at stations mag/dir
   integer, parameter :: provFile_poly_tim = 11  !< for line oriented bnd conditions, refs to uniform, fourier or harmonic
   integer, parameter :: provFile_fourier = 12  !< period(hrs), ampl(m), phas(deg)
   integer, parameter :: provFile_grib = 13  !< grib files from KNMI
   integer, parameter :: provFile_netcdf = 14  !< NetCDF files
   integer, parameter :: provFile_qhtable = 15  !<
   integer, parameter :: provFile_t3D = 16  !< temporary type for 3D salinity boundaries
   integer, parameter :: provFile_bc = 17  !< BC-format
   integer, parameter :: provFile_fourier_cor = 18  !< period(hrs), ampl(m), phas(deg) correction

   ! enumeration for access types
   integer, parameter :: accessType_undefined = 0
   integer, parameter :: accessType_fileReader = 1
   integer, parameter :: accessType_fileWriter = 2
   integer, parameter :: accessType_memory = 3
   integer, parameter :: accessType_evaluate = 4

   ! enumeration for interpolation types
   integer, parameter :: interpolate_unknown = 0 !< type is uninitialized
   integer, parameter :: interpolate_passthrough = 1 !< no interpolation is performed
   integer, parameter :: interpolate_timespace = 2 !< interpolate in time, then in space
   integer, parameter :: interpolate_spacetime = 3 !< interpolate in space, then in time
   integer, parameter :: interpolate_time = 4 !< interpolate in time
   integer, parameter :: interpolate_space = 5 !< interpolate in space
   integer, parameter :: interpolate_spacetimeSaveWeightFactors = 6 !< interpolate in space, save the space weight factors, then interpolate in time
   integer, parameter :: interpolate_time_extrapolation_ok = 7 !< interpolate in time
   integer, parameter :: interpolate_triangle = 8 !< interpolate in space by Delauney triangulation and linear interpolation.
   integer, parameter :: interpolate_averaging = 9  !< Not yet supported: only spatial, averaging
   integer, parameter :: interpolate_triangleindex = 10 !< Not yet supported: only spatial, index triangulation
   integer, parameter :: interpolate_smoothing = 11 !< Not yet supported: only spatial, smoothing
   integer, parameter :: interpolate_intdiffusion = 12 !< Not yet supported: only spatial, internal diffusion
   integer, parameter :: interpolate_vertprofile = 13 !< Not yet supported: only initial vertical profiles
   integer, parameter :: extrapolate_spacetimeSaveWeightFactors = 14 !< inter/extra-polate in space, save the space weight factors, then interpolate in time

   ! enumeration for time interpolation types
   integer, parameter :: timeint_lin = 1   !< linear
   integer, parameter :: timeint_bto = 2   !< block-to
   integer, parameter :: timeint_bfrom = 3   !< block-from
   integer, parameter :: timeint_lin_extrapol = 4   !< linear
   integer, parameter :: timeint_rainfall = 5   !< specific type of interpolation dividing the t1-value by t1-t0
   !< effectively from rainfall amount to intensity

   ! enumeration for vertical interpolation types
   integer, parameter :: zinterpolate_unknown = 0 !< type is uninitialized
   integer, parameter :: zinterpolate_linear = 1 !< linear interpolation
   integer, parameter :: zinterpolate_block = 2 !< piecewise constant interpolation (block-from)
   integer, parameter :: zinterpolate_log = 3 !< logarithmic
   integer, parameter :: zinterpolate_mean = 4 !< mean values, averaging

   ! enumeration for vertical coordinate types
   integer, parameter :: ztype_sigma = 0 !< sigma coordinates
   integer, parameter :: ztype_z = 1 !< z (absolute) coordinates
   !
   ! enumeration for operand types
   integer, parameter :: operand_undefined = 0
   integer, parameter :: operand_add = 1
   integer, parameter :: operand_replace = 2
   integer, parameter :: operand_replace_element = 3
   integer, parameter :: operand_add_element = 4
   integer, parameter :: operand_replace_if_value = 5
   !
   ! enumeration for tEcConverter types
   integer, parameter :: convType_undefined = 0
   integer, parameter :: convType_unimagdir = 2
   integer, parameter :: convType_uniform = 3 !< only time
   integer, parameter :: convType_fourier = 8
   integer, parameter :: convType_arcinfo = 9
   integer, parameter :: convType_curvi = 10
   integer, parameter :: convType_uniform_to_magnitude = 11 !< first time, then space
   integer, parameter :: convType_polytim = 12
   integer, parameter :: convType_spiderweb = 13
   integer, parameter :: convType_netcdf = 14
   integer, parameter :: convType_qhtable = 15
   integer, parameter :: convType_sigma = 16
   integer, parameter :: convType_samples = 17

   ! Error states, in addition to success=.true./.false. returns.
   integer, parameter :: EC_UNKNOWN_ERROR = -1 !< Unknown error.
   integer, parameter :: EC_NOERR = 0  !< Success
   integer, parameter :: EC_DATA_NOTFOUND = 11 !< Data not found in data source. May not be problematic if data is optional.
   integer, parameter :: EC_METADATA_INVALID = 12 !< Meta-data was mal-formatted or incomplete.
   integer, parameter :: EC_DATA_INVALID = 13 !< Data was mal-formatted or incomplete.
   integer, parameter :: EC_EOF = 14 !< EOF reached/file ended prematurely.
   integer, parameter :: EC_IO_ERROR = 15 !< Low-level I/O error from the multifile I/O routines.

   ! COORDINATE SYSTEM that applies to all elementsets in this instance
   integer, parameter :: EC_COORDS_CARTESIAN = 1  !< Cartesian coordinates (x,y)
   integer, parameter :: EC_COORDS_SFERIC = 2  !< Sferic coordinates (Lon,Lat) WGS84

   !------------------------ BC-header related parameters ----------------------------------
   ! Function types
   integer, parameter :: BC_FUNC_TSERIES = 1   !< timeseries
   integer, parameter :: BC_FUNC_HARMONIC = 2   !< harmonic
   integer, parameter :: BC_FUNC_ASTRO = 3   !< astronomic
   integer, parameter :: BC_FUNC_HARMOCORR = 4   !< harmonic correction
   integer, parameter :: BC_FUNC_ASTROCORR = 5   !< astronomic correction
   integer, parameter :: BC_FUNC_QHTABLE = 6   !< qh-relation
   integer, parameter :: BC_FUNC_TIM3D = 7   !< t3d  timeseries
   integer, parameter :: BC_FUNC_CMP3D = 8   !< t3d  components
   integer, parameter :: BC_FUNC_CONSTANT = 9   !< timeseries with one constant value

   ! Time interpolation type
   integer, parameter :: BC_TIMEINT_LIN = 1   !< linear
   integer, parameter :: BC_TIMEINT_BTO = 2   !< block-to
   integer, parameter :: BC_TIMEINT_BFROM = 3   !< block-from
   integer, parameter :: BC_TIMEINT_LIN_EXTRAPOL = 4   !< linear
   integer, parameter :: BC_TIMEINT_AMOUNT_TO_RATE = 5  !< specific type of interpolation dividing the t1-value by t1-t0
   !< for example to convert rainfall amount to rate

   ! Vertical position type
   integer, parameter :: BC_VPTYP_SINGLE = 0   !< depth averaged
   integer, parameter :: BC_VPTYP_PERCBED = 1   !< percentage from bed
   integer, parameter :: BC_VPTYP_ZDATUM = 2   !< z above datum
   integer, parameter :: BC_VPTYP_ZDATUM_DOWN = 3   !< z below datum
   integer, parameter :: BC_VPTYP_BEDSURF = 4   !< bedsurface
   integer, parameter :: BC_VPTYP_PERCSURF = 5   !< percentage from surface
   integer, parameter :: BC_VPTYP_ZBED = 6   !< z from bed
   integer, parameter :: BC_VPTYP_ZSURF = 7   !< z from surface

   ! Vertical interpolation type
   integer, parameter :: BC_VERTINT_LIN = 1   !< linear
   integer, parameter :: BC_VERTINT_LOG = 2   !< log
   integer, parameter :: BC_VERTINT_BLOCK = 3   !< block

   integer, parameter :: BC_FTYPE_ASCII = 1   !< ASCII BC file
   integer, parameter :: BC_FTYPE_NETCDF = 2   !< NETCDF BC file
   !------------------------ BC-header related parameters ----------------------------------

end module m_ec_parameters
