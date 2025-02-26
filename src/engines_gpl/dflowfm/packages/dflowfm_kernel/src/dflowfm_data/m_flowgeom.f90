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

!> in m_flowgeom: nd and ln apply to waterlevel nodes and links
 !! in m_netw    : nod and lin apply to 'grid' or 'net' nodes and links
module m_flowgeom
   use precision, only: dp

   use m_profiles
   use grid_dimens_module
   use m_flowparameters, only: jawave
   use m_cell_geometry

   implicit none

   ! node (s) related : dim=ndx
   type tnode !< node administration
      integer :: lnx !< max nr of links attached to this node
      integer, dimension(:), allocatable :: ln !< linknrs attached to this node, >0: to this flownode, <0: from this flownode

      integer, dimension(:), allocatable :: nod !< Mapping to net nodes
      real(kind=dp), dimension(:), allocatable :: x !< for now, this is only for quick/aligned plotting, the corners of a cell
      real(kind=dp), dimension(:), allocatable :: y !< for now, this is only for quick/aligned plotting, the corners of a cell
      integer :: nwx !< nr of walls attached
      integer, dimension(:), allocatable :: nw !< wallnrs attached to this node
   end type tnode

   real(kind=dp) :: bamin !< minimum 2D cell area
   real(kind=dp) :: bamin1D !< minimum cell area 1d nodes
   real(kind=dp) :: dxmin = 1d-3 !< minimum link length 1D (m)
   real(kind=dp) :: dxmin1D !< minimum link length 1D (m)
   real(kind=dp) :: dxmin2D !< minimum link length 2D (m)
   real(kind=dp) :: dxwuimin2D !< smallest fraction dx/wu , may increase dx if > 0

   real(kind=dp) :: wu1DUNI !< uniform 1D profile width
   real(kind=dp) :: hh1DUNI !< uniform 1D profile height

   real(kind=dp) :: wu1DUNI5 !< uniform 1D profile width in  streetinlet kn(3,L) = 5
   real(kind=dp) :: hh1DUNI5 !< uniform 1D profile height in streetinlet

   real(kind=dp) :: wu1DUNI7 !< uniform 1D profile width in  roofgutterpipe kn(3,L) = 7
   real(kind=dp) :: hh1DUNI7 !< uniform 1D profile height in roofgutterpipe

   integer :: ja1D2Dinternallinktype = 1

   type(griddimtype) :: griddim
   type(griddimtype) :: trachy_griddim

   ! Flow node numbering:
   ! 1:ndx2D, ndx2D+1:ndxi, ndxi+1:ndx1Db, ndx1Db+1:ndx
   ! ^ 2D int ^ 1D int      ^ 1D bnd       ^ 2D bnd ^ total
   ! the following variables have been moved in m_cell_geometry (module of gridgeom)
   ! integer, target                   :: ndx2d          !< [-] Number of 2D flow cells (= NUMP). {"rank": 0}
   ! integer, target                   :: ndx            !< [-] Number of flow nodes (internal + boundary). {"rank": 0}
   ! real(kind=dp), dimension(:), allocatable, target :: ba      !< [m2] bottom area, if < 0 use table in node type {"location": "face", "shape": ["ndx"]}
   ! real(kind=dp), dimension(:), allocatable         :: ba0     ! Backup of ba
   ! real(kind=dp), dimension(:), allocatable, target :: xz      !< [m/degrees_east] waterlevel point / cell centre, x-coordinate (m) {"location": "face", "shape": ["ndx"]}
   ! real(kind=dp), dimension(:), allocatable         :: xz0     !< backup of xz
   ! real(kind=dp), dimension(:), allocatable, target :: yz      !< [m/degrees_north] waterlevel point / cell centre, y-coordinate (m) {"location": "face", "shape": ["ndx"]}
   ! real(kind=dp), dimension(:), allocatable         :: yz0     !< backup of yz

   integer, target :: ndxi !< [-] Number of internal flowcells  (internal = 2D + 1D ). {"rank": 0}
   integer, target :: ndx1db !< [-] Number of flow nodes incl. 1D bnds (internal 2D+1D + 1D bnd). {"rank": 0}
   type(tnode), dimension(:), allocatable :: nd !< (ndx) flow node administration
   integer, dimension(:), allocatable, target :: kcs !< node code permanent
   integer, dimension(:), allocatable, target :: kfs !< [-] node code flooding {"shape": ["ndx"]}

   real(kind=dp), dimension(:), allocatable, target :: bare !< [m2] bottom area, for rain and evaporaton {"location": "face", "shape": ["ndx"]}
   real(kind=dp), dimension(:), allocatable :: bai !< inv bottom area (m2), if < 0 use table in node type
   real(kind=dp), dimension(:), allocatable, target :: ba_mor !< [m2] morphologically active bottom area, if < 0 use table in node type {"location": "face", "shape": ["ndx"]}
   real(kind=dp), dimension(:), allocatable, target :: bai_mor !< [m-2] inv morphologically active bottom area (m2)
   real(kind=dp), dimension(:), allocatable, target :: bl !< [m] bottom level (m) (positive upward) {"location": "face", "shape": ["ndx"]}
   real(kind=dp), dimension(:), allocatable, target :: bl_min !< [m] Minimal/deepest bottom level (m) (positive upward) {"location": "face", "shape": ["ndx"]}
   real(kind=dp), dimension(:), allocatable, target :: bl_ave !< [m] optional average bottom level in main channel required for dredging (m) (positive upward) (ndxi-ndx2d)
   real(kind=dp), dimension(:), allocatable, target :: bl_ave0 !< [m] optional average bottom level in main channel required for dredging (m) (positive upward) (ndxi-ndx2d)
   real(kind=dp), dimension(:), allocatable :: aif !< cell based skewness ai factor sqrt(1+(dz/dy)**2) = abed/asurface
   !< so that cfu=g(Au/conveyance)**2 = g*aif*(Au/convflat)**2
   !< convflat is flat-bottom conveyance
   real(kind=dp), dimension(:), allocatable :: aifu !< bed skewness at u point (Lnx)
   real(kind=dp), dimension(:), allocatable :: bz !< averaged bed level at cell center (Ndx)
   real(kind=dp), dimension(:), allocatable :: groundLevel !< For output purposes only: ground level of node (ndxi-ndx2d), only for 1D.
   integer, dimension(:), allocatable :: groundStorage !< For output purposes only: whether or not (1/0) storage on ground occurs (not for closed pipes) (ndxi-ndx2d), only for 1D.
   real(kind=dp), dimension(:), allocatable :: volMaxUnderground !< For output purposes only: maximal volume of node, under ground level (ndxi-ndx2d), only for 1D
   ! link (u) related : dim = lnx
   ! Flow link numbering:
   ! 1:lnx1d, lnx1d+1:lnxi, lnxi+1:lnx1Db, lnx1Db+1:lnx
   ! ^ 1D int ^ 2D int      ^ 1D bnd       ^ 2D bnd ^ total
   integer, target :: lnx1D !< [-] nr of 1D flow links (so first 1D, next 2D, next boundaries). {"rank": 0}
   integer, target :: lnxi !< [-] nr of flow links (internal, 1D+2D    ). {"rank": 0}
   integer, target :: lnx1Db !< [-] nr of flow links including 1D bnds (internal, 1D+2D, boundary: only 1D. 2D bnd behind it). {"rank": 0}
   integer, target :: lnx !< [-] nr of flow links (internal + boundary). First we have 1D links, next 2D links, next boundary links (first 1D, then 2D). {"rank": 0}
   integer, dimension(:, :), allocatable, target :: ln !< [-] 1D link (2,*) node   administration, 1=nd1,  2=nd2   linker en rechter celnr {"shape": [2, "lnkx"]}
   integer, dimension(:, :), allocatable, target :: LLkkk !< [-]    Link Link admin (5,*) , 1=lowL 2=hihL, 3=leftk, 4= midk, 5=rightk {"shape": [5, "lnx"]}
   integer, dimension(:, :), allocatable, target :: lncn !< [-] 2D link (2,*) corner administration, 1=nod1, 2=nod2  linker en rechter netnr {"shape": [2, "lnkx"]}
   integer, dimension(:), allocatable, target :: kcu !< [-] link code, 1=1D link, 2=2D link, -1= bc 1D, -2=bc 2D, 3=lateral_1d2d_link, 4=longitudinal_1d2d_link, 5=street_inlet_1d2d_link, 7=roof_gutter_1d2d_link {"shape": ["lnx"]}
   integer, dimension(:), allocatable :: Linkdried !< [-] latest dried links

   integer, dimension(:), allocatable, target :: iadv !< [-] type of advection for this link {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), dimension(:), allocatable :: teta !< link teta (m)
   integer, dimension(:, :), allocatable :: klnup !< link upwind cell pointer if q> 0 use (1:3,L), else (4:6,L)
   real(kind=dp), dimension(:), allocatable, target :: dx !< [m] link length (m) {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), dimension(:), allocatable :: dxi !< inverse dx
   real(kind=dp), dimension(:), allocatable, target :: wu !< [m] link initial width (m), if < 0 pointer to convtab {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), dimension(:), allocatable, target :: wu_mor !< [m] morphologically active width (m), if < 0 pointer to convtab {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), dimension(:), allocatable :: wui !< inverse link initial width (m), if < 0 pointer to convtab
   real(kind=dp), dimension(:), allocatable, target :: wu1D2D !< [m] Custom input for 1D2D link widths. {"location": "edge", "shape": ["lnx1D"]}
   real(kind=dp), dimension(:), allocatable, target :: hh1D2D !< [m] Custom input for 1D2D link height. {"location": "edge", "shape": ["lnx1D"]}
   real(kind=dp), dimension(:, :), allocatable :: prof1D !< dim = (3,lnx1D) 1= 1D prof width, 2=1D profile height, 3=proftyp, or: if 1,2< 0, pointers to prof 1,2, then 3=alfa1
   integer, dimension(:), allocatable :: jaduiktmp !< temparr
   real(kind=dp), dimension(:, :), allocatable, target :: bob !< [m] left and right inside lowerside tube (binnenkant onderkant buis) HEIGHT values (m) (positive upward), adjusted for structures {"location": "edge", "shape": [2, "lnx"]}
   real(kind=dp), dimension(:, :), allocatable, target :: bob0 !< [m] left and right inside lowerside tube (binnenkant onderkant buis) HEIGHT values (m) (positive upward), NOT adjusted for structures {"location": "edge", "shape": [2, "lnx"]}
   real(kind=dp), dimension(:), allocatable, target :: blup !< [m] "upwind" bed level at u point, as determined by sethu() {"location": "edge", "shape": ["lnx"]}
   integer, dimension(:), allocatable, target :: ibot !< local ibedlevtype for setting min or max network depths (temporary, result goes to bobs)

   real(kind=dp), dimension(:), allocatable :: acl !< left dx fraction, alfacl
   real(kind=dp), dimension(:, :), allocatable :: acn !< 2,L left and right wu fraction
   real(kind=dp), dimension(:), allocatable, target :: xu !< [m] velocity point x {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), dimension(:), allocatable, target :: yu !< [m] velocity point y {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), dimension(:), allocatable :: blu !< velocity point bottom level positive up (m)
   real(kind=dp), dimension(:), allocatable :: csu !< cosine comp of u0, u1
   real(kind=dp), dimension(:), allocatable :: snu !< sine   comp of u0, u1
   real(kind=dp), dimension(:, :), allocatable :: wcl !< link weights (2,lnx) for center scalar , 1,L for k1, 2,L for k2 Ln
   real(kind=dp), dimension(:, :), allocatable :: wcLn !< link weights (2,lnx) for corner scalar , 1,L for k3, 2,L for k4 Lncn
   real(kind=dp), dimension(:), allocatable :: wcx1 !< link weights (lnx) for cartesian comps center vectors k1
   real(kind=dp), dimension(:), allocatable :: wcy1 !< link weights (lnx) for cartesian comps center vectors k1
   real(kind=dp), dimension(:), allocatable :: wcx2 !< link weights (lnx) for cartesian comps center vectors k2
   real(kind=dp), dimension(:), allocatable :: wcy2 !< link weights (lnx) for cartesian comps center vectors k2
   real(kind=dp), dimension(:), allocatable :: wcnx3 !< link weights (lnx) for corner velocities k3
   real(kind=dp), dimension(:), allocatable :: wcny3 !< link weights (lnx) for corner velocities k3
   real(kind=dp), dimension(:), allocatable :: wcnx4 !< link weights (lnx) for corner velocities k4
   real(kind=dp), dimension(:), allocatable :: wcny4 !< link weights (lnx) for corner velocities k4

   real(kind=dp), dimension(:, :), allocatable :: csb !< cosine orientation from left/right neighboring flownode to flowlink, left/right as ln
   real(kind=dp), dimension(:, :), allocatable :: snb !< sine   orientation from left/right neighboring flownode to flowlink, left/right as ln
   integer, dimension(:), allocatable :: jacorner ! corner node (1) or not (0), dim(numk)

   real(kind=dp), dimension(:), allocatable :: wwL !< wall contribution to link weights wcl
   real(kind=dp), dimension(:), allocatable :: wc !< center weight factors (ndx)   , only for normalising

   real(kind=dp), dimension(:, :), allocatable :: csbn !< cosine orientation from left/right netnode to flowlink, left/right as lncn
   real(kind=dp), dimension(:, :), allocatable :: snbn !< sine   orientation from left/right netnode to flowlink, left/right as lncn

   real(kind=dp), dimension(:, :), allocatable :: slnup !< link upwind cell weight, if q> 0 use (1:3,L), else (4:6,L)
   real(kind=dp), dimension(:, :), allocatable :: csbup !< cosine orientation from upwind cell to flowlink
   real(kind=dp), dimension(:, :), allocatable :: snbup !< sine   orientation from upwind cell to flowlink

   real(kind=dp), dimension(:, :), allocatable :: csbw !< cosine orientation from left/right flowlink to wall (netlink), left/right as in walls(10,:) (left), walls (right)
   real(kind=dp), dimension(:, :), allocatable :: snbw !< sine   orientation from left/right flowlink to wall (netlink), left/right as in walls(10,:) (left), walls (right)

   real(kind=dp), dimension(:), allocatable :: csbwn !< cosine orientation from flownode to wall (netlink)
   real(kind=dp), dimension(:), allocatable :: snbwn !< sine   orientation from flownode to wall (netlink)

   integer, dimension(:), allocatable :: ln2lne !< flowlink to netlink nr dim = lnx
   integer, dimension(:), allocatable :: lne2ln !< netlink to flowlink nr dim = numL

   real(kind=dp), dimension(:), allocatable, target :: grounlay !< spatially varying ground layer thickness
   real(kind=dp), dimension(:), allocatable :: argr !< spatially varying ground layer area
   real(kind=dp), dimension(:), allocatable :: wigr !< spatially varying ground layer top width
   real(kind=dp), dimension(:), allocatable :: pergr !< spatially varying ground layer perimeter

   real(kind=dp) :: grounlayuni = -999d0 !< used if >= 0, default = dmiss
   integer :: jagrounlay = 0 !< use groundlayer 0/1
   integer, target :: wetLinkCount !< [-] nr of flow links that are wet
   integer, target :: wetLink2D !< Startposition of 2d links in onlywetLinks
   integer, target :: wetLinkBnd !< Startposition of boundary links in onlywetLinks
   integer, dimension(:), allocatable :: onlyWetLinks !< indices of flowlinks that are wet

   ! cell corner related, the links attached to a cell corner
   type tcorn !< corner administration
      integer :: lnx !< max nr of links attached to this corner
      integer, dimension(:), allocatable :: ln !< linknrs attached to this corner
      integer :: nwx !< nr of walls attached
      integer, dimension(:), allocatable :: nw !< wallnrs attached to this corner

   end type tcorn !< corner administration

   type(tcorn), dimension(:), allocatable :: cn !< cell cornerpoints, (in counting order of nod)
   real(kind=dp), dimension(:), allocatable :: ucnx !< cell corner velocity, global x-dir (m/s)
   real(kind=dp), dimension(:), allocatable :: ucny !< cell corner velocity, global y-dir (m/s) (in m_flowgeom...)
   real(kind=dp), dimension(:), allocatable, target :: vort !< [s-1] vorticity at netnodes {"shape": ["ndx"], "comment": "Currently not available, is nowhere allocated nor filled."}

   ! fixed wall related, may be expanded to closed internal walls later for now, dim=(7,*)
   integer :: mxwalls !< max nr of walls
   real(kind=dp), dimension(:, :), allocatable :: walls !< 1,* : inside waterlevel point (node)
                                                     !! 2,* : first  cornerpoint
                                                     !! 3,* : second cornerpoint
                                                     !! 4,* : flow link 1 attached to first  cornerpoint
                                                     !! 5,* : flow link 2 attached to second cornerpoint
                                                     !! 6,* : stress contribution to link 1
                                                     !! 7,* : stress contribution to link 1
   integer :: nwcnx !< max nr of cornerpoints to which walls are attached
   integer, dimension(:, :), allocatable :: nwalcnw !< pointer to those walls, 1 = left wall, 2 =right wall

! closed wall corner (netnode) related
   integer :: nrcnw !< nr of cn points attached to 2 closed walls
   integer, dimension(:), allocatable :: kcnw !< closed corner point nr k, reference to net nodes
   real, dimension(:), allocatable :: cscnw !< closed corner alignment cos (1:nrcnw)
   real, dimension(:), allocatable :: sncnw !< closed corner alignment sin (1:nrcnw)
   real, dimension(:), allocatable :: sfcnw !< closed corner partial slip sf = u*/u  (based on walls average)

! thin dam related
   integer :: nthd
   real(kind=dp), dimension(:, :), allocatable :: thindam

   ! branch related :
   type tbranch !< this is a branch type
      integer :: nx !< with nx links and nx + 1 nodes in it
      integer, dimension(:), allocatable :: ln !< successive flow linknrs
   end type tbranch

   integer :: mxflowbr !< max nr of flow branches
   type(tbranch), dimension(:), allocatable :: flowbr !< this is a list of flow branches

   integer, dimension(:), allocatable :: Lbnd1D !< for prof1D, boundary links refer to regular attached 1D links

   ! 1D endnode related
   integer :: mx1Dend !< nr of 1D endnodes
   integer, dimension(:), allocatable :: n1Dend !< node nrs of 1D endnodes

! netnode/flownode  related, dim = mxban
   real(kind=dp), dimension(:), allocatable :: banf !< horizontal netnode-flownode area (m2) (partial netnode area)
   real(kind=dp), dimension(:), allocatable :: ban !< horizontal netnode          area (m2) (complete netnode area)
   integer, dimension(:, :), allocatable :: nban !< base area pointers to banf, 1,* = netnode number, 2,* = flow node number, 3,* = link number, 4,* = 2nd link number
   integer :: mxban !< max dim of ban

   ! 1D2D link properties
   ! useful parameters :
   real(kind=dp) :: rrtol !< relative cellsize factor in search tolerance ()
   real(kind=dp), dimension(:, :), allocatable :: xyen !< temp boundary opposite point (end of EdgeNormal) (replaces ebtol tolerance)
   integer :: jarenumber !< renumberFlowNodes
   integer :: jaFlowNetChanged !< To enforce various net(link)-related init routines after renumbering
   integer :: jaAllowBndAtBifurcation !< allow 1d boundary at endnode when connecting branch leads to bifurcation

! JRE Stuff related to setting up wave directional grid
   integer :: ntheta !< Number of wave direction bins
   integer :: ntheta_s !< Number of wave direction bins, singledir
   real(kind=dp) :: thetamax !< upper limit wave directional sector
   real(kind=dp) :: thetamin !< lower limit wave directional sector
   integer :: thetanaut !< nautical convention or not
   real(kind=dp) :: dtheta !< directional resolution
   real(kind=dp) :: dtheta_s !< directional resolution single direction stationary part
   real(kind=dp) :: theta0 !< mean theta-grid direction
   real(kind=dp), dimension(:), allocatable :: thetabin !< bin-means of theta-grid
   real(kind=dp), dimension(:), allocatable :: thetabin_s !< bin-means of theta-grid singledir

   ! Villemonte calibration coefficients :
   real(kind=dp) :: VillemonteCD1 = 1.0d0 !< default for VillemonteCD1 = 1
   real(kind=dp) :: VillemonteCD2 = 10.0d0 !< default for VillemonteCD2 = 10

! Debug parameter
   integer :: cmd_icgsolver = 4 !< save commandline icgsolver

   integer, dimension(:), allocatable :: structuresAndWeirsList !< List containing flow links on which a structure or fixed weir is located.

contains
!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, call reset_flowgeom() instead.
   subroutine default_flowgeom()
      bamin = 1d-6 ! 1d0    ! minimum 2D cell area
      bamin1D = 0d-2 ! minimum cell area 1d nodes
      dxmin1D = 1d-3 ! minimum link length 1D (m)
      dxmin2D = 1d-3 ! minimum link length 2D (m)
      dxwuimin2D = 0.0d0 ! smallest fraction dx/wu , may increase dx if > 0

      wu1DUNI = 2d0 ! Uniform 1D profile width
      hh1DUNI = 3d3 ! Uniform 1D profile height

      wu1DUNI5 = 0.2d0 !< uniform 1D profile width in drain or street inlet
      hh1DUNI5 = 0.1d0 !< uniform 1D profile height in drain or street inlet

      wu1DUNI7 = 0.1d0 !< uniform 1D profile width roofgutterpipe
      hh1DUNI7 = 0.1d0 !< uniform 1D profile height roofgutterpipe

      ! useful parameters :
      rrtol = 3d0 ! relative cellsize factor in search tolerance ()
      jaAllowBndAtBifurcation = 0

      jarenumber = 1

      ! Remaining of variables is handled in reset_flowgeom()
      call reset_flowgeom()
   end subroutine default_flowgeom

!> Resets only flow geometry variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, use default_flowgeom() instead.
   subroutine reset_flowgeom()
! node (s) related : dim=ndx
      ndx2D = 0 ! nr of 2d FLOW CELLS = NUMP
      ndxi = 0 ! max nr of internal flowcells  (internal = 2D + 1D )
      ndx1Db = 0 ! nr of flow nodes incl. 1D bnds (internal 2D+1D + 1D bnd)
      ndx = 0 ! nr of flow nodes (internal + boundary)

! link (u) related : dim = lnx
      lnx1D = 0 ! nr of 1D flow links
      lnxi = 0 ! nr of flow links (internal           )
      lnx1Db = 0 ! nr of flow links incl. 1D bnds (internal 1D+2D + 1D bnd)
      lnx = 0 ! nr of flow links (internal + boundary)

! useful parameters :
      jaFlowNetChanged = 1 ! To enforce various net(link)-related init routines after renumbering

      if (jawave == 4) then ! reinitialize wave directional grid
         ntheta = 0
         ntheta_s = 0
      end if
   end subroutine reset_flowgeom
end module m_flowgeom
