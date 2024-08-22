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

module typos

    use m_waq_precision

    !     These types are used to support Domain decomposition
    type pnt                                     ! 2 directions because of inner corners to the same cell
        ! index runs in first 'n' direction (flow runs in second direction)
        integer         in1                       ! number of the receiver face (see below)
        integer         n1                        ! (n,m) active cell at receiver side
        integer         m1
        integer         f1                        ! refinement factor receiver or inverse factor for coarsening
        integer         i1                        ! index of finer cell in coarser
        ! index runs in second 'm' direction ( flows runs in first direction)
        integer         in2                       ! in2 = 0 => bound is at 0.0 in cell
        integer         n2                        !     = 1 => bound is at 1.0 in cell
        integer         m2
        integer         f2
        integer         i2                        ! zero means count your position in receiver
    end type pnt
    type domain                                  ! domain definition
        character(256)  name                      ! Part has an array of domains if DD applies
        integer         num_rows                      ! first, fast running index, size of the domain
        integer         num_columns                      ! second, slower running index, size of the domain
        integer         moff                      ! the m-offset in the larger total matrix of this domain
    end type domain
    type range                                   ! a range is a transect in a domain
        character(256)  name                      ! name of the domain
        integer         did                       ! domain ID (sequence number in array of domains
        integer         fn                        ! from-n   transect definition
        integer         fm                        ! from-m   one of the 2 indices, n or m is always
        integer         tn                        ! to-n        constant, a transect under an angle
        integer         tm                        ! to-m        in the grid is not allowed
    end type range
    type boundp                                  ! a domain boundary consists of 2 ranges,
        type (range)  r1                        ! one on each side of the boundary
        type (range)  r2
    end type boundp

    !     These types are used to support plot grids

    type PlotGrid
        integer(int_wp) :: ztype                      ! 0 = averaged (default and only option before 2011)
        ! 1 = absolute z-values in m from surface
        ! 2 = absolute z-values in m from bed
        ! 3 = relative z-values beteen 0.0 and 1.0 from surface
        integer(int_wp) :: mmap                       ! nr of x-cells in the grid
        integer(int_wp) :: nmap                       ! nr of y-cells in the grid
        real   (real_wp) :: xlow                       ! corner points of the plot grid window
        real   (real_wp) :: xhigh
        real   (real_wp) :: ylow
        real   (real_wp) :: yhigh
        real   (real_wp) :: zlow
        real   (real_wp) :: zhigh
        real   (real_wp) :: surf                       ! surface area of 1 plot grid cell
        integer(int_wp), pointer :: nmcell(:, :)   ! backpointer to hydrodynamic grid            (nmap,mmap)
        real   (real_wp), pointer :: amap  (:, :, :, :)   ! plot grid map             (substances,layers,nmap,mmap)
        real   (real_wp), pointer :: atrack(:, :, :)   ! plot grid map                        (layers,nmap,mmap)
        integer(int_wp), pointer :: nbin  (:, :, :)   !                                      (layers,nmap,mmap)
        integer(int_wp), pointer :: imask (:, :)   ! plotgrid cells that are inactive            (nmap,mmap)
    end type PlotGrid

end module typos
