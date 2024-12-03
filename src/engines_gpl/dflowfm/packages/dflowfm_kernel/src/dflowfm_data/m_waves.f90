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

module m_waves
   use precision, only: dp

   implicit none
   integer, parameter :: TPWAVDEFAULT = 0 !< Indicator for TP
   integer, parameter :: TPWAVSMOOTH = 1 !< Indicator for TPS
   integer, parameter :: TPWAVRELATIVE = 2 !< Indicator for RTP
   integer :: nwf !< nr of fetch wind dirs + 1
   real(kind=dp), allocatable :: fetch(:, :) !< wind dir dep. fetch lenght (m) of each cell, dimension 5,*, or 13, * nr of wind dirs + 1
   real(kind=dp), allocatable :: fetdp(:, :) !< wind dir dep. waterdepth (m)   of each cell, dimension 5,*, or 13, * nr of wind dirs + 1
   real(kind=dp), allocatable :: fett(:, :) !< reduce array, (2,ndx)

   real(kind=dp), allocatable, target :: hwav(:) !< [m] root mean square wave height (m) from external source, {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, target :: hwavcom(:) !< [m] root mean square wave height (m) from external source
   real(kind=dp), allocatable, target :: twav(:) !< [s] wave period {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, target :: twavcom(:) !< [s] wave period from external source {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, target :: phiwav(:) !< [degree] mean wave direction (degrees) from external source
   real(kind=dp), allocatable, target :: uorb(:) !< [m/s] orbital velocity {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, target :: ustokes(:) !< [m/s] wave induced velocity, link-based and link-oriented
   real(kind=dp), allocatable, target :: vstokes(:) !< [m/s] wave induced velocity, link-based and link-oriented
   real(kind=dp), allocatable :: rlabda(:) !< [m] wave length
   real(kind=dp), allocatable :: ustx_cc(:), usty_cc(:) !< [m/s] ustokes components cell centres

   real(kind=dp), allocatable, target :: dsurf(:) !< [w/m2] wave energy dissipation rate due to breaking at the free surface, "DISSURF" in WAVE
   real(kind=dp), allocatable, target :: dwcap(:) !< [w/m2] wave energy dissipation rate due to white capping
   real(kind=dp), allocatable, target :: distot(:) !< [w/m2] total wave energy dissipation rate, "DISTOT" in WAVE

   real(kind=dp) :: hwavuni = 0d0 !< uniform (*.mdu) value of ...
   real(kind=dp) :: twavuni = 0d0 !< uniform (*.mdu) value of ...
   real(kind=dp) :: phiwavuni = 0d0 !< uniform (*.mdu) value of ...

   real(kind=dp) :: ftauw !< Swartfactor, tune bed shear stress
   real(kind=dp) :: fwfac !< Soulsby factor, tune streaming
   real(kind=dp) :: fbreak !< tune breaking in tke model
   real(kind=dp) :: fwavpendep !< Layer thickness as proportion of Hrms over which wave breaking adds to TKE source. Default 0.5

   character(len=4) :: rouwav !< Friction model for wave induced shear stress

   real(kind=dp), allocatable, target :: sxwav(:) !< [N/m2] wave force in x (east)  direction on water surface (N/m2) from external source, "FX"   in WAVE
   real(kind=dp), allocatable, target :: sywav(:) !< [N/m2] wave force in y (north) direction on water surface (N/m2) from external source, "FY"   in WAVE
   real(kind=dp), allocatable, target :: sbxwav(:) !< [N/m2] wave force in x (east)  direction on water column  (N/m2) from external source, "WSBU" in WAVE
   real(kind=dp), allocatable, target :: sbywav(:) !< [N/m2] wave force in y (north) direction on water column  (N/m2) from external source, "WSBV" in WAVE
   real(kind=dp), allocatable, target :: uorbwav(:) !< [m/s] orbital velocity (m/s) from external source
   real(kind=dp), allocatable, target :: wlenwav(:) !< [m] wave length (m) from external source

   ! additional data for WAVE/SWAN-coupling
   real(kind=dp), allocatable, target :: mxwav(:) !< wave induced volume flux, in x-direction at flow-nodes
   real(kind=dp), allocatable, target :: mywav(:) !< wave induced volume flux, in y-direction at flow-nodes

   real(kind=dp), allocatable :: cfwavhi(:)
   real(kind=dp), allocatable :: cfhi_vanrijn(:)
   real(kind=dp), allocatable :: wblt(:)

   real(kind=dp) :: facmax !< maximum wave force
   real(kind=dp) :: JONSWAPgamma0 = 3.3 !< Peak enhancement factor JONSWAP

   ! for visualisation
   integer :: waveparopt
   integer :: numoptwav

   real(kind=dp), allocatable :: ust_mag(:)
   real(kind=dp), allocatable :: fwav_mag(:)

   ! parameters, may be overwritten by user in mdu-file
   real(kind=dp) :: gammax !< Maximum wave height/water depth ratio
   real(kind=dp) :: alfdeltau = 20d0 !< coeff for thickness of wave bed boundary layer
   real(kind=dp) :: hminlw !< [m] minimum depth for wave forcing in flow momentum equation RHS.
   integer :: jatpwav = TPWAVDEFAULT !< TPWAV, TPWAVSMOOTH, TPWAVRELATIVE
   integer :: jauorb !< multiply with factor sqrt(pi)/2 (=0), or not (=1). Default 0, delft3d style
   integer :: jahissigwav !< 1: sign wave height on his output; 0: hrms wave height on his output.
   integer :: jamapsigwav !< 1: sign wave height on map output; 0: hrms wave height on map output.
   integer :: jauorbfromswan !< 1: get uorb from SWAN, compare with Delft3D
   logical :: extfor_wave_initialized !< is set to .true. when the "external forcing"-part that must be initialized for WAVE during running (instead of during initialization) has actually been initialized

contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_waves() instead.
   subroutine default_waves()
      use m_physcoef

      rouwav = 'FR84'
      gammax = 1.0d0 !< Maximum wave height/water depth ratio
      hminlw = 0.2d0 !< [-] minimum depth for wave forcing in flow momentum equation RHS.
      jatpwav = TPWAVDEFAULT !< TPWAV, TPWAVSMOOTH, TPWAVRELATIVE
      jauorb = 0
      jahissigwav = 1
      jamapsigwav = 0 ! Present behaviour
      jauorbfromswan = 0
      ftauw = 1d0
      fwfac = 1d0
      fbreak = 1d0
      fwavpendep = 1.5d0 ! best setting based on sensitivity

      call reset_waves()
   end subroutine default_waves

!> Resets only waves variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, call default_waves() instead.
   subroutine reset_waves()
      extfor_wave_initialized = .false. !< is set to .true. when the "external forcing"-part that must be initialized for WAVE during running (instead of during initialization) has actually been initialized
   end subroutine reset_waves

end module m_waves
