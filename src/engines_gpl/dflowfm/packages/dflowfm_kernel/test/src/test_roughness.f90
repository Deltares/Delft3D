!!  Copyright (C)  Stichting Deltares, 2012-2026.
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

module test_roughness
   use assertions_gtest, only: f90_expect_relative_near
   use m_roughness

   use precision, only: dp
   implicit none(type, external)
   private

contains

   !$gtest
   subroutine test_roughness_branches() bind(C)
      use m_network
      use m_roughness
      use m_read_roughness
      use m_hash_search

      type(t_network) :: network
      type(t_CSType), pointer :: cross
      character(len=256) :: roughnessfiles
      integer :: ibranch
      integer :: section
      integer :: igrid
      real(kind=dp) :: h
      real(kind=dp) :: q
      real(kind=dp) :: u
      real(kind=dp) :: r
      real(kind=dp) :: d
      real(kind=dp) :: chainage
      real(kind=dp) :: chezy
      real(kind=dp) :: eps = 1.0e-4_dp

      call realloc(network%brs)
      network%brs%count = 7
      network%brs%branch(1)%id = 'Channel1'
      network%brs%branch(2)%id = 'Channel2'
      network%brs%branch(3)%id = 'Channel3'
      network%brs%branch(4)%id = 'Channel4'
      network%brs%branch(5)%id = 'Channel5'
      network%brs%branch(6)%id = 'Channel6'
      network%brs%branch(7)%id = 'Channel7'
      call fill_hashtable(network%brs)
      cross => null()

      roughnessfiles = 'roughness/roughness_main.ini;roughness/roughness-globals.ini'
      call roughness_reader(network, roughnessfiles)

      ibranch = 4
      section = hashsearch(network%rgs%hashlist, 'chezy45')
      igrid = 0
      h = 1.0e00_dp
      q = 0.0_dp
      u = 0.0_dp
      r = 2.0_dp
      d = 3.0_dp
      chainage = 0.0_dp

      ! check the friction values that are globally defined:
      chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
      call f90_expect_relative_near(chezy, 45.0_dp, eps, "chezy for chezy45 is nog correct")

      section = hashsearch(network%rgs%hashlist, 'Strickler')
      chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
      call f90_expect_relative_near(chezy, 44.89848_dp, eps, "chezy for Strickler is nog correct")

      section = hashsearch(network%rgs%hashlist, 'WhiteColebrookD3d')
      chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
      call f90_expect_relative_near(chezy, 53.52338_dp, eps, "chezy for WhiteColebrookD3d is nog correct")

      section = hashsearch(network%rgs%hashlist, 'WhiteColebrookWaqua')
      chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
      call f90_expect_relative_near(chezy, 55.42526_dp, eps, "chezy for WhiteColebrookWaqua is nog correct")

      section = hashsearch(network%rgs%hashlist, 'Manning')
      chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
      call f90_expect_relative_near(chezy, 11.22462_dp, eps, "chezy for Manning is nog correct")

      section = hashsearch(network%rgs%hashlist, 'BosBijkerk')
      chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
      call f90_expect_relative_near(chezy, 40.47176_dp, eps, "chezy for BosBijkerk is nog correct")

      ! Now check the interpolation
      section = hashsearch(network%rgs%hashlist, 'Main')
      ibranch = 4
      igrid = 0
      h = 1.0e00_dp
      q = 0.0_dp
      u = 0.0_dp
      r = 2.0_dp
      d = 3.0_dp
      chainage = 0.0_dp

      ! check the friction values that are globally defined:
      ! Channel 2 is not aviable, value shoud be 45 (global value)
      ibranch = 2
      chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
      call f90_expect_relative_near(chezy, 45.0_dp, eps, "chezy for Main channel2 is nog correct")

      ! Channel 1 expect Chezy value of 41
      ibranch = 1
      chainage = 10.0_dp
      chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
      call f90_expect_relative_near(chezy, 41.0_dp, eps, "chezy for Main channel2 is nog correct")

      ! Channel 4 expect Chezy value of 40
      ibranch = 4
      chainage = 10.0_dp
      chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
      call f90_expect_relative_near(chezy, 60.0_dp, eps, "chezy for Main channel2 is nog correct")

      ! Channel 7 Q smaller than limits, expect Chezy value of 47
      ibranch = 7
      chainage = 60.0_dp
      q = 50.0_dp
      chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
      call f90_expect_relative_near(chezy, 47.0_dp, eps, "chezy for Main channel2 is nog correct")

      ! Channel 7 Q inside the limits, expect Chezy value of 46.05
      ibranch = 7
      chainage = 60.0_dp
      q = 125.0_dp
      chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
      call f90_expect_relative_near(chezy, 46.05_dp, eps, "chezy for Main channel2 is nog correct")

      ! Channel 7 Q larger than limits, expect Chezy value of 42
      ibranch = 7
      chainage = 60.0_dp
      q = 400.0_dp
      chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
      call f90_expect_relative_near(chezy, 42.0_dp, eps, "chezy for Main channel2 is nog correct")
      continue

   end subroutine test_roughness_branches

end module test_roughness
