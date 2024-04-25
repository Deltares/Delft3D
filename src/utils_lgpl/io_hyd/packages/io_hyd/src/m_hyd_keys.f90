!!  Copyright (C)  Stichting Deltares, 2021-2024.
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

  module m_hyd_keys

      ! module containing keywords in hyd file

      implicit none

      integer, parameter  :: nokey   = 84           ! number of keywords in hyd file

      character(len=40), parameter :: key(*) = [character(len=40) ::  &
        'task', &                                         ! 1
        'geometry', &                                     ! 2
        'horizontal-aggregation', &                       ! 3
        'minimum-vert-diffusion-used', &                  ! 4
        'vertical-diffusion', &                           ! 5
        'description', &                                  ! 6
        'end-description', &                              ! 7
        'reference-time', &                               ! 8
        'hydrodynamic-start-time', &                      ! 9
        'hydrodynamic-stop-time', &                       ! 10
        'hydrodynamic-timestep', &                        ! 11
        'conversion-ref-time', &                          ! 12
        'conversion-start-time', &                        ! 13
        'conversion-stop-time', &                         ! 14
        'conversion-timestep', &                          ! 15
        'grid-cells-first-direction', &                   ! 16
        'grid-cells-second-direction', &                  ! 17
        'number-hydrodynamic-layers', &                   ! 18
        'number-water-quality-layers', &                  ! 19
        'hydrodynamic-file', &                            ! 20
        'aggregation-file', &                             ! 21
        'grid-indices-file', &                            ! 22
        'grid-coordinates-file', &                        ! 23
        'volumes-file', &                                 ! 24
        'areas-file', &                                   ! 25
        'flows-file', &                                   ! 26
        'pointers-file', &                                ! 27
        'lengths-file', &                                 ! 28
        'salinity-file', &                                ! 29
        'temperature-file', &                             ! 30
        'vert-diffusion-file', &                          ! 31
        'surfaces-file', &                                ! 32
        'total-grid-file', &                              ! 33
        'discharges-file', &                              ! 34
        'chezy-coefficients-file', &                      ! 35
        'shear-stresses-file', &                          ! 36
        'walking-discharges-file', &                      ! 37
        'minimum-vert-diffusion', &                       ! 38
        'upper-layer', &                                  ! 39
        'lower-layer', &                                  ! 40
        'interface-depth', &                              ! 41
        'end-minimum-vert-diffusion', &                   ! 42
        'constant-dispersion', &                          ! 43
        'first-direction', &                              ! 44
        'second-direction', &                             ! 45
        'third-direction', &                              ! 46
        'end-constant-dispersion', &                      ! 47
        'hydrodynamic-layers', &                          ! 48
        'end-hydrodynamic-layers', &                      ! 49
        'water-quality-layers', &                         ! 50
        'end-water-quality-layers', &                     ! 51
        'discharges', &                                   ! 52
        'end-discharges', &                               ! 53
        'domains', &                                      ! 54
        'end-domains', &                                  ! 55
        'dd-boundaries', &                                ! 56
        'end-dd-boundaries', &                            ! 57
        'normal', &                                       ! 58
        'inlet', &                                        ! 59
        'outlet', &                                       ! 60
        'full-coupling', &                                ! 61
        'coupling-per-domain', &                          ! 62
        'attributes-file', &                              ! 63
        'depths-file', &                                  ! 64
        'curvilinear-grid', &                             ! 65
        'yes', &                                          ! 66
        'no', &                                           ! 67
        'calculated', &                                   ! 68
        'unstructured', &                                 ! 69
        'number-horizontal-exchanges', &                  ! 70
        'number-vertical-exchanges', &                    ! 71
        'number-water-quality-segments-per-layer', &      ! 72
        'horizontal-surfaces-file', &                     ! 73
        'boundaries-file', &                              ! 74
        'waqgeom-file', &                                 ! 75
        'automatic', &                                    ! 76
        'walking', &                                      ! 77
        'file-created-by', &                              ! 78
        'file-creation-date', &                           ! 79
        'sink-sources', &                                 ! 80
        'end-sink-sources', &                             ! 81
        'z-layers', &                                     ! 82
        'z-layers-ztop', &                                ! 83
        'z-layers-zbot']                                  ! 84


  end module m_hyd_keys
