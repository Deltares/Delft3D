!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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
module wave_boundary_datastore
   ! The module stores essential information for wave boundary conditions in the following
   ! derived types:
   !  - waveBoundaryParameters
   !  - waveBoundaryAdministration
   !  - waveBoundaryTimeSeries
   !  - waveSpectrumAdministration
   !
   ! These derived types are accessed by wave_boundary_main, wave_boundary_init and
   ! wave_boundary_update
   use precision_basics, only: dp
   implicit none
   !
   !
   ! Define derived type to store wave parameter information
   type waveBoundaryParametersType
      character(1024) :: masterFileName
      integer :: np, ntheta
      real(dp) :: dtheta
      real(dp) :: x0, y0
      real(dp) :: hboundary
      logical :: nonhspectrum
      real(dp) :: sprdthr, trepfac
      integer :: Tm01switch
      real(dp), dimension(:), allocatable :: xb, yb, theta ! Note, can these be changed to pointers?
      real(dp), dimension(:), allocatable :: theta_s
      integer :: randomseed
      integer :: nspr
      real(dp) :: rho
      real(dp) :: nmax
      real(dp) :: fcutoff
      real(dp) :: swkhmin
      integer :: singledir
      integer :: ntheta_s
      integer :: wbcScaleEnergy
      integer :: wbcRemoveStokes
      real(dp) :: wbcEvarreduce
      real(dp) :: wbcQvarreduce
   end type waveBoundaryParametersType
   !
   !
   ! Define derived type to store information on boundary condition file (only required for
   ! spectral wave boundary conditions)
   type filenames
      character(1024) :: fname ! file name of boundary condition file
      integer :: listline ! read position in FILELIST files
      logical :: repeat = .false. ! indicate to repeat this file every rtbc cycle
   end type filenames
   !
   !
   ! Define derived type to store wave boundary administration information
   type waveBoundaryAdministrationType
      logical :: initialized = .false. ! Initialisation status
      real(dp) :: startComputeNewSeries ! Time at which to compute a boundary condition time series (s)
      real(dp) :: startCurrentSeries ! Time at which current boundary conditions started (s)
   end type waveBoundaryAdministrationType
   !
   !
   ! Define derived type to store spectral boundary administration information
   type waveSpectrumAdministrationType
      integer :: nspectra ! number of input spectra, set in init spectrum
      integer, dimension(:), allocatable :: ispectra
      type(filenames), dimension(:), allocatable :: bcfiles ! input wave spectrum files
      logical :: repeatwbc ! switch to repeat all of the wave boundary conditions
      integer :: bccount ! number of times boundary conditions have been generated, set in init spectrum
      real(dp) :: spectrumendtime ! end time of boundary condition written to administration file
      real(dp), dimension(:, :), allocatable :: lastwaveelevation ! wave height at the end of the last spectrum
      real(dp), dimension(:), allocatable :: xspec, yspec ! x,y coordinates of input wave spectra
      real(dp) :: Hbc, Tbc, Dbc ! computed representative wave height, period and wave direction
      real(dp), dimension(:, :), allocatable :: ee_s ! single_dir: stationary wave energy distribution along boundary (ntheta_s,npb)
      integer, dimension(:), allocatable :: n_index_loc ! y-index locations of all input spectra, set in init spectrum
      integer :: ind_end_taper ! index of where the taper function equals rtbc
      integer, dimension(:), allocatable :: kL, kR
      real(dp), dimension(:), allocatable :: wL, wR
   end type waveSpectrumAdministrationType
   !
   !
   ! Define derived type to store wave boundary time series information
   type waveBoundaryTimeSeriesType
      real(dp), dimension(:, :, :), allocatable :: eebct
      real(dp), dimension(:, :), allocatable :: qxbct, qybct
      real(dp), dimension(:, :), allocatable :: zsbct, ubct, vbct, wbct
      real(dp), dimension(:), allocatable :: tbc
   end type waveBoundaryTimeSeriesType
   !
   !
   ! Declare variables of type above
   type(waveBoundaryParametersType), allocatable, save :: waveBoundaryParameters(:)
   type(waveBoundaryAdministrationType), allocatable, save :: waveBoundaryAdministration(:)
   type(waveBoundaryTimeSeriesType), allocatable, save :: waveBoundaryTimeSeries(:)
   type(waveSpectrumAdministrationType), allocatable, save :: waveSpectrumAdministration(:)

end module wave_boundary_datastore
