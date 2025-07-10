!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
    use ftnunit
    use m_roughness


    implicit none

contains

subroutine tests_roughness
    call test( test_roughness_branches,     'Tests roughness' )
end subroutine tests_roughness

subroutine test_roughness_branches
   use m_network
   use m_roughness
   use m_read_roughness
   use m_hash_search
   use ifport
   
   type(t_network)         :: network
   type(t_CSType), pointer :: cross
   character(len=256)      :: roughnessfiles
   integer                 :: ibranch
   integer                 :: section
   integer                 :: igrid
   logical                 :: success
   double precision        :: h
   double precision        :: q
   double precision        :: u
   double precision        :: r
   double precision        :: d
   double precision        :: chainage
   double precision        :: chezy
   double precision        :: eps = 1d-4
   
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

   success = CHANGEDIRQQ("roughness")

   roughnessfiles = 'roughness_main.ini;roughness-globals.ini'
   call roughness_reader(network, roughnessfiles)

   success = CHANGEDIRQQ("..")

   ibranch  = 4
   section  = hashsearch(network%rgs%hashlist, 'chezy45')
   igrid    = 0
   h        = 1d00
   q        = 0d0
   u        = 0d0
   r        = 2d0
   d        = 3d0
   chainage = 0d0
   
   ! check the friction values that are globally defined:
   chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
   call assert_comparable( chezy, 45d0, eps, "chezy for chezy45 is nog correct" )
   
   section  = hashsearch(network%rgs%hashlist, 'Strickler')
   chezy = getfrictionValue(network%rgs, network%spData, cross, ibranch, section, igrid, h, q, u, r, d, chainage)
   call assert_comparable( chezy, 44.89848d0, eps, "chezy for Strickler is nog correct" )

   section  = hashsearch(network%rgs%hashlist, 'WhiteColebrookD3d')
   chezy = getfrictionValue(network%rgs, network%spData, cross,  ibranch, section, igrid, h, q, u, r, d, chainage)
   call assert_comparable( chezy, 53.52338d0, eps, "chezy for WhiteColebrookD3d is nog correct" )

   section  = hashsearch(network%rgs%hashlist, 'WhiteColebrookWaqua')
   chezy = getfrictionValue(network%rgs, network%spData, cross,  ibranch, section, igrid, h, q, u, r, d, chainage)
   call assert_comparable( chezy, 55.42526d0, eps, "chezy for WhiteColebrookWaqua is nog correct" )

   section  = hashsearch(network%rgs%hashlist, 'Manning')
   chezy = getfrictionValue(network%rgs, network%spData, cross,  ibranch, section, igrid, h, q, u, r, d, chainage)
   call assert_comparable( chezy, 11.22462d0, eps, "chezy for Manning is nog correct" )

   section  = hashsearch(network%rgs%hashlist, 'BosBijkerk')
   chezy = getfrictionValue(network%rgs, network%spData, cross,  ibranch, section, igrid, h, q, u, r, d, chainage)
   call assert_comparable( chezy, 40.47176d0, eps, "chezy for BosBijkerk is nog correct" )

   ! Now check the interpolation
   section  = hashsearch(network%rgs%hashlist, 'Main')
   ibranch  = 4
   igrid    = 0
   h        = 1d00
   q        = 0d0
   u        = 0d0
   r        = 2d0
   d        = 3d0
   chainage = 0d0
   
   ! check the friction values that are globally defined:
   ! Channel 2 is not aviable, value shoud be 45 (global value)
   ibranch  = 2
   chezy = getfrictionValue(network%rgs, network%spData, cross,  ibranch, section, igrid, h, q, u, r, d, chainage)
   call assert_comparable( chezy, 45d0, eps, "chezy for Main channel2 is nog correct" )

   ! Channel 1 expect Chezy value of 41
   ibranch  = 1
   chainage = 10d0
   chezy = getfrictionValue(network%rgs, network%spData, cross,  ibranch, section, igrid, h, q, u, r, d, chainage)
   call assert_comparable( chezy, 41d0, eps, "chezy for Main channel2 is nog correct" )

   ! Channel 4 expect Chezy value of 40
   ibranch  = 4
   chainage = 10d0
   chezy = getfrictionValue(network%rgs, network%spData, cross,  ibranch, section, igrid, h, q, u, r, d, chainage)
   call assert_comparable( chezy, 60d0, eps, "chezy for Main channel2 is nog correct" )
   
   ! Channel 7 Q smaller than limits, expect Chezy value of 47
   ibranch  = 7
   chainage = 60d0
   q        = 50d0
   chezy = getfrictionValue(network%rgs, network%spData, cross,  ibranch, section, igrid, h, q, u, r, d, chainage)
   call assert_comparable( chezy, 47d0, eps, "chezy for Main channel2 is nog correct" )
   
   ! Channel 7 Q inside the limits, expect Chezy value of 46.05
   ibranch  = 7
   chainage = 60d0
   q        = 125d0
   chezy = getfrictionValue(network%rgs, network%spData, cross,  ibranch, section, igrid, h, q, u, r, d, chainage)
   call assert_comparable( chezy, 46.05d0, eps, "chezy for Main channel2 is nog correct" )
   
   ! Channel 7 Q larger than limits, expect Chezy value of 42
   ibranch  = 7
   chainage = 60d0
   q        = 400d0
   chezy = getfrictionValue(network%rgs, network%spData, cross,  ibranch, section, igrid, h, q, u, r, d, chainage)
   call assert_comparable( chezy, 42d0, eps, "chezy for Main channel2 is nog correct" )
continue

end subroutine test_roughness_branches
   
end module test_roughness
