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

module m_strucs
   use precision, only: dp
   use m_GlobalParameters
   integer :: nstru !< total nr of structures

   integer :: mxgeneral !< total nr of general structures
   integer :: mxuniversal !< total nr of unversal weirs    etc etc

   integer, allocatable :: Lstruc(:) !< Flow Linknumbers on which structures are defined
   integer, allocatable :: Itypstr(:) !< Type indication for each type
   integer, allocatable :: Ntypstr(:) !< So many-st nr of this type e.g. (1:mxgeneral)

   integer :: mxstrhis = 16 !< leading dimension of
   real(kind=dp), allocatable :: strhis(:, :) !< For all structures. when computing n+1, strhis has values of step n
   !< strhis( 1,:) : Gate Opening Height
   !< strhis( 2,:) : Crest level
   !< strhis( 3,:) : Crest width
   !< strhis( 4,:) : Discharge
   !< strhis( 5,:) : Velocity
   !< strhis( 6,:) :
   !< strhis( 7,:) :
   !< strhis( 8,:) :
   !< strhis( 9,:) : Water level left
   !< strhis(10,:) : Water level right
   !< strhis(11,:) :
   !< strhis(12,:) :
   !< strhis(13,:) : reduction factor (ST_RIVER_WEIR)
   !< strhis(14,:) : pressure difference
   !< strhis(15,:) : Waterlevel on crest (general structure)
   !< strhis(16,:) : Area

   real(kind=dp), allocatable :: strhis2(:, :) !< holds values of strhis of step n-1

   type tgeneralstruc
      real(kind=dp) :: widthleftW1 !< this and following: see Sobek manual
      real(kind=dp) :: levelleftZb1
      real(kind=dp) :: widthleftWsdl
      real(kind=dp) :: levelleftZbsl
      real(kind=dp) :: widthcenter
      real(kind=dp) :: levelcenter
      real(kind=dp) :: widthrightWsdr
      real(kind=dp) :: levelrightZbsr
      real(kind=dp) :: widthrightW2
      real(kind=dp) :: levelrightZb2
      real(kind=dp) :: gateheight
      real(kind=dp) :: gateheightintervalcntrl
      real(kind=dp) :: pos_freegateflowcoeff
      real(kind=dp) :: pos_drowngateflowcoeff
      real(kind=dp) :: pos_freeweirflowcoeff
      real(kind=dp) :: pos_drownweirflowcoeff
      real(kind=dp) :: pos_contrcoeffreegate
      real(kind=dp) :: neg_freegateflowcoeff
      real(kind=dp) :: neg_drowngateflowcoeff
      real(kind=dp) :: neg_freeweirflowcoeff
      real(kind=dp) :: neg_drownweirflowcoeff
      real(kind=dp) :: neg_contrcoeffreegate
      real(kind=dp) :: extraresistance
      real(kind=dp) :: dynstrucfact
      real(kind=dp) :: dynstructext
      real(kind=dp) :: gatedoorheight
      real(kind=dp) :: dooropeningwidth
      real(kind=dp) :: stabilitycounter
      real(kind=dp), allocatable :: widthcenteronlink(:) !< For each crossed flow link the the center width portion of this genstr. (sum(widthcenteronlink(1:numlink)) should equal widthcenter)
      real(kind=dp), allocatable :: gateheightonlink(:) !< For each crossed flow link the the gate height portion of this genstr. (will be set to dummy high value in open part of sideways closing gates.)
      real(kind=dp), allocatable :: gateclosedfractiononlink(:) !< part of the link width that is closed by the gate
      integer :: numlinks !< Nr of flow links that cross this generalstructure.

   end type tgeneralstruc

   integer, parameter :: idx_upstream1width = 1
   integer, parameter :: idx_upstream1level = 2
   integer, parameter :: idx_upstream2width = 3
   integer, parameter :: idx_upstream2level = 4
   integer, parameter :: idx_crestwidth = 5
   integer, parameter :: idx_crestlevel = 6
   integer, parameter :: idx_downstream1width = 7
   integer, parameter :: idx_dowsstream1level = 8
   integer, parameter :: idx_downstream2width = 9
   integer, parameter :: idx_downstream2level = 10
   integer, parameter :: idx_gateloweredgelevel = 11
   integer, parameter :: idx_gateheightintervalcntrl = 12
   integer, parameter :: idx_pos_freegateflowcoeff = 13
   integer, parameter :: idx_pos_drowngateflowcoeff = 14
   integer, parameter :: idx_pos_freeweirflowcoeff = 15
   integer, parameter :: idx_pos_drownweirflowcoeff = 16
   integer, parameter :: idx_pos_contrcoeffreegate = 17
   integer, parameter :: idx_neg_freegateflowcoeff = 18
   integer, parameter :: idx_neg_drowngateflowcoeff = 19
   integer, parameter :: idx_neg_freeweirflowcoeff = 20
   integer, parameter :: idx_neg_drownweirflowcoeff = 21
   integer, parameter :: idx_neg_contrcoeffreegate = 22
   integer, parameter :: idx_extraresistence = 23
   integer, parameter :: idx_dynstrucentent = 24
   integer, parameter :: idx_gateheight = 25
   integer, parameter :: idx_gateopeningwidth = 26

   integer, parameter :: NUMGENERALKEYWRD = 26
   character(len=256) :: generalkeywrd(NUMGENERALKEYWRD) = (/character(len=256) :: &
                                                             'Upstream1Width', & ! ( 1)
                                                             'Upstream1Level', & ! ( 2)
                                                             'Upstream2Width', & ! ( 3)
                                                             'Upstream2Level', & ! ( 4)
                                                             'CrestWidth', & ! ( 5)
                                                             'CrestLevel', & ! ( 6)
                                                             'Downstream1Width', & ! ( 7)
                                                             'Downstream1Level', & ! ( 8)
                                                             'Downstream2Width', & ! ( 9)
                                                             'Downstream2Level', & ! (10)
                                                             'GateLowerEdgeLevel', & ! (11)
                                                             'gateheightintervalcntrl', & ! (12)
                                                             'pos_freegateflowcoeff', & ! (13)
                                                             'pos_drowngateflowcoeff', & ! (14)
                                                             'pos_freeweirflowcoeff', & ! (15)
                                                             'pos_drownweirflowcoeff', & ! (16)
                                                             'pos_contrcoeffreegate', & ! (17)
                                                             'neg_freegateflowcoeff', & ! (18)
                                                             'neg_drowngateflowcoeff', & ! (19)
                                                             'neg_freeweirflowcoeff', & ! (20)
                                                             'neg_drownweirflowcoeff', & ! (21)
                                                             'neg_contrcoeffreegate', & ! (22)
                                                             'extraresistance', & ! (23)
                                                             'dynstructext', & ! (24)
                                                             'GateHeight', & ! (25)
                                                             'GateOpeningWidth' & ! (26)
                                                             /)
   type(tgeneralstruc), allocatable, target :: generalstruc(:)

   type tuniversalstruc

      integer :: idum
   end type tuniversalstruc
   type(tuniversalstruc), allocatable :: universalstruc(:)

end module m_strucs
