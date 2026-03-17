module m_general_structure
!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
!-------------------------------------------------------------------------------

   use precision, only: dp
   use m_tables
   use m_struc_helper
   use m_GlobalParameters

   implicit none(type, external)

   private

   ! Publics
   public EXTRA_RESISTANCE_GENERAL_STRUCTURE
   public GATE_FRACTION_EPS

   public GEN_SYMMETRIC
   public GEN_FROMLEFT
   public GEN_FROMRIGHT

   public t_GeneralStructure

   public compute_general_structure
   public dealloc
   public update_widths

   ! Module parameters
   real(kind=dp) :: EXTRA_RESISTANCE_GENERAL_STRUCTURE = 0.0_dp
   real(kind=dp) :: GATE_FRACTION_EPS = 1.0e-5_dp

   integer, parameter :: GEN_SYMMETRIC = 1
   integer, parameter :: GEN_FROMLEFT = 2
   integer, parameter :: GEN_FROMRIGHT = 3

   type :: t_GeneralStructure ! see flgtar.f90
      real(kind=dp) :: wu1 !< w_u1
      real(kind=dp) :: zu1 !< z_u1
      real(kind=dp) :: wu2 !< w_u2
      real(kind=dp) :: zu2 !< z_u2
      real(kind=dp) :: ws !< crest width (as defined in input/RTC)
      real(kind=dp) :: ws_actual !< actual crest width (possibly limited by total width of flow links and must be > 0)
      real(kind=dp) :: zs !< crest level (as defined in input/RTC)
      real(kind=dp) :: zs_actual !< crest level (possibly adapted to BOB level, in case of multiple links the lowest point is taken).
      real(kind=dp) :: wd1 !< w_d1
      real(kind=dp) :: zd1 !< z_d1
      real(kind=dp) :: wd2 !< w_d2
      real(kind=dp) :: zd2 !< z_d2
      real(kind=dp) :: gateLowerEdgeLevel !< gate lower edge level (as defined in input/RTC)
      real(kind=dp) :: gateLowerEdgeLevel_actual !< gate lower edge level (possibly adapted to crest level, in case of multiple links the lowest point is taken)
      real(kind=dp) :: cgf_pos !< Positive free gate flow function
      real(kind=dp) :: cgd_pos !< Positive drowned gate flow function
      real(kind=dp) :: cwf_pos !< Positive free weir flow function
      real(kind=dp) :: cwd_pos !< Positive drowned weir flow function
      real(kind=dp) :: mugf_pos !< Positive flow contraction coefficient function
      real(kind=dp) :: cgf_neg !< Negative free gate flow function
      real(kind=dp) :: cgd_neg !< Negative drowned gate flow function
      real(kind=dp) :: cwf_neg !< Negative free weir flow function
      real(kind=dp) :: cwd_neg !< Negative drowned weir flow function
      real(kind=dp) :: mugf_neg !< Negative flow contraction coefficient function
      real(kind=dp) :: extraresistance !< Extra resistance
      real(kind=dp) :: gatedoorheight !< height of the doors
      real(kind=dp) :: gateopeningwidth !< width between the doors (as defined in input/RTC)
      real(kind=dp) :: gateopeningwidth_actual !< width between the doors (possibly adapted to crest width and always > 0)
      real(kind=dp) :: crestlength !< length of the crest for computing the extra resistance using bedfriction over the crest of the weir
      real(kind=dp), pointer, dimension(:) :: widthcenteronlink => null() !< For each crossed flow link the the center width portion of this genstr. (sum(widthcenteronlink(1:numlink)) should equal widthcenter)
      real(kind=dp), pointer, dimension(:) :: gateclosedfractiononlink => null() !< part of the link width that is closed by the gate
      real(kind=dp), pointer, dimension(:, :) :: fu => null() !< fu(1:3,L0) contains the partial computational value for fu (under/over/between gate, respectively)
      real(kind=dp), pointer, dimension(:, :) :: ru => null() !< ru(1:3,L0) contains the partial computational value for ru (under/over/between gate, respectively)
      real(kind=dp), pointer, dimension(:, :) :: au => null() !< au(1:3,L0) contains the partial computational value for au (under/over/between gate, respectively)
      integer :: numlinks !< Nr of flow links that cross this generalstructure.
      logical :: velheight !< Flag indicates the use of the velocity height or not
      integer :: openingDirection !< possible values GEN_SYMMETRIC, GEN_FROMLEFT, GEN_FROMRIGHT
      real(kind=dp), pointer, dimension(:) :: sOnCrest => null() !< water level on crest per link (length = numlinks)
      integer, pointer, dimension(:, :) :: state => null() !< state(1:3,L0) contains flow state on the L0th link of the structure for General Structure, Weir and Orifice
      !< 1: state of under gate flow, 2: state of over gate flow, 3: state of between gate flow
      !< 0 = No Flow
      !< 1 = Free Weir Flow
      !< 2 = Drowned Weir Flow
      !< 3 = Free Gate Flow
      !< 4 = Drowned Gate Flow
      integer :: allowedflowdir !< allowed flow direction
      !< 0 all directions
      !< 1 only positive flow
      !< 2 only negative flow
      !< 3 no flow allowed
      logical :: uselimitFlowNeg !< flag for limiting the maximum discharge through an orifice for negative flow
      logical :: uselimitFlowPos !< flag for limiting the maximum discharge through an orifice for positive flow
      real(kind=dp) :: limitFlowNeg !< maximal discharge in positive direction (in case useLimitFLowPos is true)
      real(kind=dp) :: limitFlowpos !< maximal discharge in negative direction (in case useLimitFLowNeg is true
   end type

   interface dealloc
      module procedure deallocGenstru
   end interface dealloc

contains

   !> compute FU, RU and AU for a single flow link in a general structure.
   subroutine compute_general_structure(genstr, direction, L0, maxWidth, bob0, fuL, ruL, auL, as1, as2, structwidth, s1m1, s1m2, &
                                        qtotal, Cz, dxL, dt, SkipDimensionChecks)
      ! modules

      ! Global variables
      type(t_GeneralStructure), pointer, intent(inout) :: genstr !< Derived type containing general structure information.
      integer, intent(in) :: direction !< Orientation of flow link w.r.t. the structure. (1 for same direction, -1 for reverse.)
      integer, intent(in) :: L0 !< Local link index.
      real(kind=dp), intent(inout) :: maxWidth !< Maximal width of the structure. Normally the the width of the flow link.
      real(kind=dp), dimension(2), intent(in) :: bob0 !< Bed level of channel upstream and downstream of the structure.
      real(kind=dp), intent(out) :: fuL !< fu component of momentum equation.
      real(kind=dp), intent(out) :: ruL !< Right hand side component of momentum equation.
      real(kind=dp), intent(inout) :: auL !< Flow area of structure opening.
      real(kind=dp), intent(in) :: as1 !< (Geometrical) upstream flow area.
      real(kind=dp), intent(in) :: as2 !< (Geometrical) downstream flow area.
      real(kind=dp), intent(out) :: structwidth !< Flow width of structure.
      real(kind=dp), intent(in) :: s1m1 !< (Geometrical) upstream water level.
      real(kind=dp), intent(in) :: s1m2 !< (Geometrical) downstream water level.
      real(kind=dp), intent(in) :: qtotal !< Total discharge (in case of a compound structure this is not equal to
      !< the discharge through the structure).
      real(kind=dp), intent(in) :: Cz !< Chezy value.
      real(kind=dp), intent(in) :: dxL !< Length of the flow link.
      real(kind=dp), intent(in) :: dt !< Time step (s).
      logical, intent(in) :: SkipDimensionChecks !< Flag indicating whether the dimensions of the structure is to be limited
      !< by the cross sectional dimensions of the channel and correct, or not.
      !
      !
      ! Local variables
      !
      real(kind=dp) :: alm
      real(kind=dp) :: arm
      real(kind=dp) :: s1ml
      real(kind=dp) :: s1mr
      real(kind=dp) :: qL
      real(kind=dp), dimension(2) :: bobstru !< same as BOB0, but with respect to the structure orientation

      logical :: velheight
      integer :: allowedflowdir
      real(kind=dp) :: cgd
      real(kind=dp) :: cgf
      real(kind=dp) :: crest
      real(kind=dp) :: cwd
      real(kind=dp) :: cwf
      real(kind=dp) :: dg
      real(kind=dp) :: ds
      real(kind=dp) :: ds1
      real(kind=dp) :: ds2
      real(kind=dp) :: hd
      real(kind=dp) :: hu
      real(kind=dp) :: lambda
      real(kind=dp) :: mugf
      real(kind=dp) :: rhoast
      real(kind=dp) :: rholeft
      real(kind=dp) :: rhoright
      real(kind=dp) :: flowDir
      real(kind=dp) :: ud
      real(kind=dp) :: uu
      real(kind=dp) :: w2
      real(kind=dp) :: wsd
      real(kind=dp) :: wstr
      real(kind=dp) :: zb2
      real(kind=dp) :: zs
      real(kind=dp) :: zgate
      real(kind=dp) :: gatefraction
      real(kind=dp) :: gle
      real(kind=dp) :: dx_struc
      real(kind=dp) :: dsL
      real(kind=dp) :: maxFlowL
      real(kind=dp) :: u1L
      real(kind=dp), dimension(3) :: fu
      real(kind=dp), dimension(3) :: ru
      real(kind=dp), dimension(3) :: au

      !
      !
      !! executable statements -------------------------------------------------------
      !

      genstr%zs_actual = genstr%zs
      genstr%gateLowerEdgeLevel_actual = genstr%gateLowerEdgeLevel

      if (.not. SkipDimensionChecks) then
         crest = max(bob0(1), bob0(2), genstr%zs)
      else
         crest = genstr%zs
         maxwidth = genstr%ws
      end if

      gle = max(crest, genstr%gateLowerEdgeLevel)
      genstr%gateLowerEdgeLevel_actual = gle
      ! upstream flow area should always be larger or equal to the flow area at the crest
      alm = max(as1, auL)
      arm = max(as2, auL)
      s1ml = s1m1
      s1mr = s1m2
      dsL = s1m2 - s1m1

      dx_struc = genstr%crestlength

      velheight = genstr%velheight
      !
      call UpAndDownstreamParameters(s1ml, s1mr, alm, arm, qtotal, velheight, &
                                     rholeft, rhoright, crest, hu, hd, uu, ud, flowDir)
      !
      ! apply orientation of the flow link to the direction dependend parameters

      if (SkipDimensionChecks) then
         bobstru = -1.0e5_dp
      else if (direction > 0) then
         bobstru(1) = bob0(1)
         bobstru(2) = bob0(2)
      else
         bobstru(1) = bob0(2)
         bobstru(2) = bob0(1)
      end if

      allowedflowdir = genstr%allowedflowdir
      if ((allowedflowdir == 3) .or. &
          (direction * flowDir == 1 .and. allowedflowDir == 2) .or. &
          (direction * flowDir == -1 .and. allowedflowDir == 1)) then
         fuL = 0.0_dp
         ruL = 0.0_dp
         auL = 0.0_dp
         genstr%fu(:, L0) = 0.0_dp
         genstr%ru(:, L0) = 0.0_dp
         genstr%au(:, L0) = 0.0_dp
         return
      end if

      call flgtar(genstr, L0, maxWidth, bobstru, direction * flowDir, zs, wstr, w2, wsd, zb2, ds1, ds2, cgf, cgd, cwf, cwd, mugf, lambda)
      !
      rhoast = rhoright / rholeft
      if (flowDir < 0.0_dp) rhoast = 1.0_dp / rhoast
      !

      gatefraction = genstr%gateclosedfractiononlink(L0)

      fu = genstr%fu(:, L0)
      ru = genstr%ru(:, L0)
      au = genstr%au(:, L0)
      if (gatefraction > GATE_FRACTION_EPS) then
         ! calculate flow under gate
         dg = gle - zs

         u1L = ru(1) - fu(1) * dsL
         qL = Au(1) * u1L

         call flqhgs(fu(1), ru(1), u1L, dxL, dt, structwidth, au(1), qL, flowDir, &
                     hu, hd, uu, zs, gatefraction * wstr, w2, wsd, zb2, ds1, ds2, dg, &
                     rhoast, cgf, cgd, cwf, cwd, mugf, lambda, Cz, dx_struc, ds, genstr%state(1, L0), velheight)
         genstr%sOnCrest(L0) = ds + crest ! waterlevel on crest

         ! Flow limiter is only available for an orifice type structure. In this case only flow under the door
         ! is possible. For General Structures (and Weirs) this limiter is not further implemented, only part "1"
         ! (out of 1:3) of the flow is limited.
         ! In 2D the maximum flow rate is divided over all individual links, where the limiter is applied for
         ! each flow link individually, weighted by flow link width.

         ! qL is in orientation of structure (as are the limitFlow values)
         qL = direction * au(1) * (ru(1) - fu(1) * dsL)
         if (qL > 0.0_dp .and. genstr%uselimitFlowPos) then
            maxFlowL = genstr%limitFlowPos * gatefraction * wstr / genstr%ws_actual
            if (qL > maxFlowL) then
               fu(1) = 0.0_dp
               ru(1) = direction * maxFlowL / max(1.0e-6_dp, au(1))
            end if
         else if (qL < 0.0_dp .and. genstr%uselimitFlowNeg) then
            maxFlowL = genstr%limitFlowNeg * gatefraction * wstr / genstr%ws_actual
            if (abs(qL) > maxFlowL) then
               fu(1) = 0.0_dp
               ru(1) = -direction * maxFlowL / max(1.0e-6_dp, au(1))
            end if

         end if

         !calculate flow over gate
         dg = huge(1.0_dp)
         zgate = gle + genstr%gatedoorheight
         u1L = ru(2) - fu(2) * dsL
         qL = Au(2) * u1L

         call flqhgs(fu(2), ru(2), u1L, dxL, dt, structwidth, au(2), qL, flowDir, &
                     hu, hd, uu, zgate, gatefraction * wstr, w2, wsd, zb2, ds1, ds2, dg, &
                     rhoast, cgf, cgd, cwf, cwd, mugf, 0.0_dp, 0.0_dp, dx_struc, ds, genstr%state(2, L0), velheight)
      else
         fu(1) = 0.0_dp
         ru(1) = 0.0_dp
         fu(2) = 0.0_dp
         ru(2) = 0.0_dp
      end if

      if (gatefraction < 1.0_dp - GATE_FRACTION_EPS) then
         ! calculate flow asif no door is present
         dg = huge(1.0_dp)
         u1L = ru(3) - fu(3) * dsL
         qL = Au(3) * u1L

         call flqhgs(fu(3), ru(3), u1L, dxL, dt, structwidth, au(3), qL, flowDir, &
                     hu, hd, uu, zs, (1.0_dp - gatefraction) * wstr, w2, wsd, zb2, ds1, ds2, dg, &
                     rhoast, cgf, cgd, cwf, cwd, mugf, lambda, Cz, dx_struc, ds, genstr%state(3, L0), velheight)
         genstr%sOnCrest(L0) = ds + crest ! waterlevel on crest

      else
         fu(3) = 0.0_dp
         ru(3) = 0.0_dp
      end if

      auL = (au(1) + au(2)) + au(3)
      if (auL > 0.0_dp) then
         fuL = (fu(1) * au(1) + fu(2) * au(2) + fu(3) * au(3)) / auL
         ruL = (ru(1) * au(1) + ru(2) * au(2) + ru(3) * au(3)) / auL
      else
         fuL = 0.0_dp
         ruL = 0.0_dp
      end if
      genstr%fu(:, L0) = fu
      genstr%ru(:, L0) = ru
      genstr%au(:, L0) = au
      !TEMP = laatste statement

   end subroutine compute_general_structure

   !> Compute coefficients for structure equation
   subroutine flgtar(genstr, L0, maxWidth, bobstru, flowDir, zs, wstr, w2, wsd, zb2, ds1, ds2, cgf, cgd, cwf, cwd, mugf, lambda)
      ! Parameters
      type(t_GeneralStructure), pointer, intent(in) :: genstr !< Derived type containing general structure information
      integer, intent(in) :: L0 !< Internal link number
      real(kind=dp), intent(in) :: maxWidth !<  Maximal width of the structure. Normally the the width of the flowlink
      real(kind=dp), intent(in), dimension(2) :: bobstru !< bed level of channel left and right of the structure (w.r.t. structure orientation)
      real(kind=dp), intent(out) :: cgd !< Contraction coefficient for drowned gate flow
      real(kind=dp), intent(out) :: cgf !< Contraction coefficient for gate flow
      real(kind=dp), intent(out) :: cwd !< Contraction coefficient for drowned weir flow.
      real(kind=dp), intent(out) :: cwf !< Contraction coefficient for free weir flow.
      real(kind=dp), intent(out) :: ds1 !< Delta s1 general structure.
      real(kind=dp), intent(out) :: ds2 !< Delta s2 general structure.
      real(kind=dp), intent(out) :: lambda !< Extra resistance
      real(kind=dp), intent(out) :: mugf !< Vertical contraction coefficient for free gate flow.
      real(kind=dp), intent(in) :: flowDir !< Flow direction (+1/-1).
      real(kind=dp), intent(out) :: w2 !< Width at right side of structure.
      real(kind=dp), intent(out) :: wsd !< Width structure right or left side.
      real(kind=dp), intent(out) :: wstr !< Width at centre of structure.
      real(kind=dp), intent(out) :: zb2 !< Bed level at right side of structure.
      real(kind=dp), intent(out) :: zs !< Bed level at centre of structure.

      ! Local variables
      real(kind=dp) :: help
      real(kind=dp) :: w1
      real(kind=dp) :: wsdl
      real(kind=dp) :: wsdr
      real(kind=dp) :: zb1
      real(kind=dp) :: zbsl
      real(kind=dp) :: zbsr

      wstr = min(maxWidth, genstr%widthcenteronlink(L0))

      if (genstr%numlinks == 1) then
         ! ws_actual is determined in update_widths restrictions 0 < ws < maxwidth)
         genstr%ws_actual = min(maxWidth, genstr%ws)
         wstr = genstr%ws_actual

         ! all other width parameters must always be <= maxWidth, but >= ws
         w1 = max(min(maxWidth, genstr%wu1), genstr%ws_actual)
         wsdl = max(min(maxWidth, genstr%wu2), genstr%ws_actual)
         wsdr = max(min(maxWidth, genstr%wd1), genstr%ws_actual)
         w2 = max(min(maxWidth, genstr%wd2), genstr%ws_actual)
      else ! Structure crosses more than one link: nonsensible to use single width left/right etc.
         ! same for all links. Use center linkwidth instead (i.e., typically wu(Lf))
         w1 = wstr
         wsdl = wstr
         wstr = wstr
         wsdr = wstr
         w2 = wstr
      end if

      ! zs always above bed level up and downstream (bob(:))
      zs = max(bobstru(1), bobstru(2), genstr%zs)
      if (zs > genstr%zs_actual) then
         ! Note: the adaptation of the crest level at the different links depend on the BOB of this link
         ! The highest value is taken as the actual crest level
         genstr%zs_actual = zs
      end if

      ! other levels above bed level up and downstream (bob(:)) but below the zs
      zb1 = min(max(bobstru(1), genstr%zu1), zs)
      zbsl = min(max(bobstru(1), genstr%zu2), zs)
      zbsr = min(max(bobstru(2), genstr%zd1), zs)
      zb2 = min(max(bobstru(2), genstr%zd2), zs)

      lambda = genstr%extraresistance
      !
      !     Determine cgf, cgd, cwf, cwd, mugf
      !     (flow direction dependent)
      !
      if (flowDir > 0.0_dp) then
         cgf = genstr%cgf_pos
         cgd = genstr%cgd_pos
         cwf = genstr%cwf_pos
         cwd = genstr%cwd_pos
         mugf = genstr%mugf_pos
      else
         cgf = genstr%cgf_neg
         cgd = genstr%cgd_neg
         cwf = genstr%cwf_neg
         cwd = genstr%cwd_neg
         mugf = genstr%mugf_neg
      end if

      ! Determine flow direction dependent parameters
      if (flowDir > 0.0_dp) then
         wsd = wsdr
         ds1 = zs - zbsr
         ds2 = zbsr - zb2
      else
         wsd = wsdl
         ds1 = zs - zbsl
         ds2 = zbsl - zb1
         help = w1
         w1 = w2
         w2 = help
         help = zb1
         zb1 = zb2
         zb2 = help
      end if
   end subroutine flgtar

   !> FLow QH relation for General Structure
   subroutine flqhgs(fuL, ruL, u1L, dxL, dt, structwidth, auL, qL, flowDir, &
                     hu, hd, uu, zs, wstr, w2, wsd, zb2, ds1, ds2, &
                     dg, rhoast, cgf, cgd, cwf, cwd, mugf, lambda, Cz, dx_struc, &
                     ds, state, velheight)
      ! Parameters
      real(kind=dp), intent(inout) :: auL !< flow area
      real(kind=dp), intent(inout) :: fuL !< fu component of momentum equation
      real(kind=dp), intent(inout) :: ruL !< Right hand side component of momentum equation
      real(kind=dp), intent(inout) :: u1L !< Flow velocity at current time step
      real(kind=dp), intent(inout) :: qL !< Discharge through structure
      real(kind=dp), intent(in) :: dxL !< Length of flow link
      real(kind=dp), intent(in) :: dt !< Time step
      real(kind=dp), intent(out) :: structwidth !< Flow width
      real(kind=dp), intent(in) :: cgd !< Contraction coefficient for drowned gate flow
      real(kind=dp), intent(in) :: cgf !< Contraction coefficient for gate flow
      real(kind=dp), intent(in) :: cwd !< Contraction coefficient for drowned weir flow.
      real(kind=dp), intent(in) :: cwf !< Contraction coefficient for free weir flow.
      real(kind=dp), intent(in) :: dg !< Gate opening height.
      real(kind=dp), intent(inout) :: ds !< Water level immediately downstream the gate.
      real(kind=dp), intent(in) :: ds1 !< Delta s1 general structure.
      real(kind=dp), intent(in) :: ds2 !< Delta s2 general structure.
      real(kind=dp), intent(in) :: hd !< Downstream water level.
      real(kind=dp), intent(in) :: hu !< Upstream water level.
      real(kind=dp), intent(in) :: lambda !< Extra resistance
      real(kind=dp), intent(in) :: Cz !< Chezy value
      real(kind=dp), intent(in) :: mugf !< Vertical contraction coefficient for free gate flow.
      real(kind=dp), intent(in) :: rhoast !< Ratio of density right and left of structure
      real(kind=dp), intent(in) :: flowDir !< Flow direction (+1/-1).
      real(kind=dp), intent(in) :: uu !< Upstream velocity (with velheight setting already accounted for).
      real(kind=dp), intent(in) :: w2 !< Width at right side of structure.
      real(kind=dp), intent(in) :: wsd !< Width structure right or left side.
      real(kind=dp), intent(in) :: wstr !< Width at centre of structure.
      real(kind=dp), intent(in) :: zb2 !< Bed level at right side of structure.
      real(kind=dp), intent(in) :: zs !< Bed level at centre of structure.
      integer, intent(out) :: state !< Flow state of the structure
      real(kind=dp), intent(in) :: dx_struc !< length of structure
      logical, intent(in) :: velheight !< logical indicates whether the momentum equation has to be taken into account

      ! Local variables
      logical :: imag
      real(kind=dp) :: cgd2
      real(kind=dp) :: cgda
      real(kind=dp) :: cgfa
      real(kind=dp) :: cwfa
      real(kind=dp) :: dc
      real(kind=dp) :: dlim
      real(kind=dp) :: elu
      real(kind=dp) :: hd1
      real(kind=dp) :: hs1
      real(kind=dp) :: mugfa
      real(kind=dp) :: velhght

      if (velheight) then
         velhght = uu * uu / (2.0_dp * gravity)
      else
         velhght = 0.0_dp
      end if

      elu = hu + velhght
      hs1 = elu - zs

      if (hs1 <= 0.0_dp .or. wstr <= 0.0_dp .or. min(cgf, cgd, cwf, cwd) <= 0.0_dp .or. dg < 1e-4_dp) then !hk: or gate closed
         state = 0
         ds = hs1
      else
         ! Compute critical water depth at the
         ! sill, dc and water depth at the sill,ds
         if (.not. velheight) then
            ds = hd - zs
         else
            dlim = hs1 * (wstr / w2 * 2.0_dp / 3.0_dp * sqrt(2.0_dp / 3.0_dp))**(2.0_dp / 3.0_dp)
            hd1 = max(hd, zb2 + dlim * 0.9_dp)

            ! Calculate ds by solving third order algebraic equation
            call flgsd3(wsd, wstr, zs, w2, zb2, ds1, ds2, elu, hd1, rhoast, cwd, ds, lambda)
         end if

         dc = 2.0_dp / 3.0_dp * hs1

         if (ds >= dc) then
            if (dg >= ds) then ! drowned weir
               state = 2
            else ! gate flow
               state = 3

               ! adapt coefficients on basis of Ds & Cwd
               call flccgs(dg, ds, cgd, cgf, cwd, mugf, cgda, cgfa, mugfa)
            end if
         else
            ! Adapt Cwf coefficient
            if (cwf < cwd) then
               if (GS_dpsequ(dc, 0.0_dp, 1.0e-02_dp)) then
                  cwfa = cwf
               else
                  cwfa = max(ds / dc * cwd, cwf)
               end if
            else if (ds > 0.0_dp) then
               cwfa = min(dc / ds * cwd, cwf)
            else
               cwfa = cwf
            end if

            if (dg >= dc) then ! free weir
               state = 1
               ds = dc
            else ! gate flow
               state = 3
               ! adapt coefficients on basis of Dc & Cwf
               call flccgs(dg, dc, cgd, cgf, cwfa, mugf, cgda, cgfa, mugfa)
            end if
         end if
         ! In case of gate flow determine type of gate flow
         ! (drowned or free)
         if (state == 3) then
            dc = mugfa * dg

            ! Cgd for second order equation = Cgd' * Mu'
            cgd2 = cgda * mugfa
            if (velheight) then
               call flgsd2(wsd, wstr, zs, w2, zb2, dg, ds1, ds2, elu, hd1, rhoast, cgd2, imag, ds, lambda)
            else
               ds = hd - zs
               imag = .false.
            end if

            if (imag) then ! free gate
               state = 3
               ds = dc
            else if (ds <= dc) then ! free gate
               state = 3

               ! Adapt coefficients
               if (cgda > cgfa) then
                  if (.not. GS_dpsequ(dc, 0.0_dp, 1.0e-20_dp)) then
                     cgfa = max(ds / dc * cgda, cgfa)
                  end if
               else if (ds > 0.0_dp) then
                  cgfa = min(dc / ds * cgda, cgfa)
               else
               end if
               ds = dc
            else ! drowned gate
               state = 4
            end if
         end if
      end if

      ! The flowe condition is known so calculate
      ! the linearization coefficients FU and RU
      call flgsfuru(fuL, ruL, u1L, auL, qL, dxL, dt, structwidth, state, &
                    flowDir, hu, hd, velhght, zs, ds, dg, dc, wstr, &
                    cwfa, cwd, mugfa, cgfa, cgda, dx_struc, lambda, Cz)
   end subroutine flqhgs

   !>  Compute water depth ds at the sill by solving a third order algebraic equation. \n
   !!  In case of drowned weir flow the water level atthe sill is required. The water
   !!  depth is calculated in this routine.
   subroutine flgsd3(wsd, wstr, zs, w2, zb2, ds1, ds2, elu, hd, rhoast, cwd, ds, lambda)
      ! Parameters
      real(kind=dp), intent(in) :: cwd !<
      real(kind=dp), intent(out) :: ds !<
      real(kind=dp), intent(in) :: ds1 !<
      real(kind=dp), intent(in) :: ds2 !<
      real(kind=dp), intent(in) :: elu !<
      real(kind=dp), intent(in) :: hd !<
      real(kind=dp), intent(in) :: lambda !<
      real(kind=dp), intent(in) :: rhoast !<
      real(kind=dp), intent(in) :: w2 !<
      real(kind=dp), intent(in) :: wsd !<
      real(kind=dp), intent(in) :: wstr !<
      real(kind=dp), intent(in) :: zb2 !<
      real(kind=dp), intent(in) :: zs !<

      ! Local variables
      real(kind=dp) :: aw
      real(kind=dp) :: bw
      real(kind=dp) :: cw
      real(kind=dp) :: d2
      real(kind=dp) :: fac
      real(kind=dp) :: h2a
      real(kind=dp) :: h2b
      real(kind=dp) :: h2c
      real(kind=dp) :: hsl
      real(kind=dp) :: hulp
      real(kind=dp) :: hulp1
      real(kind=dp) :: p
      real(kind=dp) :: phi
      real(kind=dp) :: q
      real(kind=dp) :: r60
      real(kind=dp) :: term
      real(kind=dp) :: u
      real(kind=dp) :: v

      real(kind=dp), parameter :: C23 = 2.0_dp / 3.0_dp
      real(kind=dp), parameter :: C13 = 1.0_dp / 3.0_dp

      d2 = hd - zb2
      hsl = elu - zs
      term = ((4.0_dp * cwd * cwd * rhoast * wstr * wstr) / (w2 * d2)) * (1.0_dp + lambda / d2)

      aw = (-term * hsl - 4.0_dp * cwd * wstr + (1.0_dp - rhoast) &
            * (w2 / 12.0_dp + wsd / 4.0_dp) + 0.5_dp * (rhoast + 1.0_dp) * (C13 * w2 + C23 * wsd)) / term

      bw = (4.0_dp * cwd * wstr * hsl + (1.0_dp - rhoast) &
            * ((d2 + ds1) * (w2 + wsd) / 6.0_dp + ds1 * wsd * C13) + 0.5_dp * (rhoast + 1.0_dp) &
            * ((ds1 + ds2 - d2) * (C13 * w2 + C23 * wsd) + (C23 * d2 + C13 * ds1) &
               * w2 + (C13 * d2 + C23 * ds1) * wsd)) / term

      cw = ((1.0_dp - rhoast) * ((d2 + ds1)**2 * (w2 + wsd) / 12.0_dp + ds1**2 * wsd / 6.0_dp) &
            + 0.5_dp * (rhoast + 1.0_dp) * (ds1 + ds2 - d2) &
            * ((C23 * d2 + C13 * ds1) * w2 + (C13 * d2 + C23 * ds1) * wsd)) / term

      ! Solve the equation ds**3 + aw*ds**2 + bw*ds +cw to get the water
      ! level at the sill
      p = bw / 3.0_dp - aw * aw / 9.0_dp
      q = aw * aw * aw / 27.0_dp - aw * bw / 6.0_dp + cw / 2.0_dp
      hulp = q * q + p * p * p

      if (hulp < 0.0_dp) then
         p = abs(p)
         phi = acos(abs(q) / p / sqrt(p)) / 3.0_dp
         r60 = acos(0.5_dp)
         fac = sign(2.0_dp, q) * sqrt(p)
         h2a = -fac * cos(phi)
         h2b = fac * cos(r60 - phi)
         h2c = fac * cos(r60 + phi)
         ds = max(h2a, h2b, h2c) - aw / 3.0_dp
      else
         hulp = sqrt(hulp)
         hulp1 = -q + hulp
         if (abs(hulp1) < 1.0e-6_dp) then
            u = 0; v = 0
         else ! hk: ook fix for Erwin, ARS 15132
            u = abs(hulp1)**C13 * sign(1.0_dp, hulp1)
            hulp1 = -q - hulp
            v = abs(hulp1)**C13 * sign(1.0_dp, hulp1)
         end if
         ds = u + v - aw / 3.0_dp
      end if
      ds = min(ds, elu - zs, hd - zs)
   end subroutine flgsd3

   !> FLow contraction coefficients for general structure.\n
   !! In the formulas for the gate and weir several coefficients are applied.
   !! To avoid discontinuities in the transition from weir to gate flow, the
   !!correction coefficient cgd should be corrected.
   subroutine flccgs(dg, dsc, cgd, cgf, cw, mugf, cgda, cgfa, mugfa)
      ! Parameters
      real(kind=dp), intent(in) :: cgd !< Correction coefficient for drowned gate flow.
      real(kind=dp), intent(out) :: cgda !< Adapted correction coefficient for drowned gate flow.
      real(kind=dp), intent(in) :: cgf !< Correction coefficient for free gate flow.
      real(kind=dp), intent(out) :: cgfa !< Adapted correction coefficient for free gate flow.
      real(kind=dp), intent(in) :: cw !< Correction coefficient for weir flow.
      real(kind=dp), intent(in) :: dg !< Gate opening height.
      real(kind=dp), intent(in) :: dsc !< Depth at sill or critical depth.
      real(kind=dp), intent(in) :: mugf !< Contraction coefficient for free gate flow.
      real(kind=dp), intent(out) :: mugfa !< Adapted contraction coefficient for free gate flow.

      if (.not. GS_dpsequ(dsc, 0.0_dp, 1.0e-20_dp)) then

         if (dg / dsc > mugf) then
            mugfa = dg / dsc
         else
            mugfa = mugf
         end if

         if (cgd > cw) then
            if (GS_dpsequ(dg, 0.0_dp, 1.0e-20_dp)) then
               cgda = cgd
            else
               cgda = min(dsc / dg * cw, cgd)
            end if
         else
            cgda = max(dg / dsc * cw, cgd)
         end if

         if (cgf > cw) then
            if (GS_dpsequ(dg, 0.0_dp, 1.0e-20_dp)) then
               cgfa = cgf
            else
               cgfa = min(dsc / dg * cw, cgf)
            end if
         else
            cgfa = max(dg / dsc * cw, cgf)
         end if

      else
         mugfa = mugf
         cgda = cgd
         cgfa = cgf
      end if
   end subroutine flccgs

   !> FLGSD2 (FLow Gen. Struct. Depth sill 2nd ord. eq.)\n
   !! Compute water depth ds at the sill by a second order algebraic equation.
   !! In case of drowned gate flow the water level at the sill is required.
   !! The water depth is calculated in this routine.
   subroutine flgsd2(wsd, wstr, zs, w2, zb2, dg, ds1, ds2, elu, hd, rhoast, cgd, imag, ds, lambda)
      ! Parameters
      logical, intent(out) :: imag !< Logical indicator, = TRUE when determinant of second order algebraic equation less than zero.
      real(kind=dp), intent(in) :: cgd !< Correction coefficient for drowned gate flow.
      real(kind=dp), intent(in) :: dg !< Gate opening height.
      real(kind=dp), intent(out) :: ds !< Water level immediately downstream the gate.
      real(kind=dp), intent(in) :: ds1 !< Delta s1 general structure.
      real(kind=dp), intent(in) :: ds2 !< Delta s2 general structure.
      real(kind=dp), intent(in) :: elu !< Upstream energy level.
      real(kind=dp), intent(in) :: hd !< Downstream water level.
      real(kind=dp), intent(in) :: lambda !< Extra resistance in general structure.
      real(kind=dp), intent(in) :: rhoast !< Downstream water density divided by upstream water density.
      real(kind=dp), intent(in) :: w2 !< Width at right side of structure.
      real(kind=dp), intent(in) :: wsd !< Width structure right or left side.
      real(kind=dp), intent(in) :: wstr !< Width at centre of structure.
      real(kind=dp), intent(in) :: zb2 !< Bed level at right side of structure.
      real(kind=dp), intent(in) :: zs !< Bed level at centre of structure.

      ! Local variables
      real(kind=dp) :: ag
      real(kind=dp) :: bg
      real(kind=dp) :: cg
      real(kind=dp) :: d2
      real(kind=dp) :: det
      real(kind=dp) :: hsl
      real(kind=dp) :: terma
      real(kind=dp) :: termb

      real(kind=dp), parameter :: C23 = 2.0_dp / 3.0_dp
      real(kind=dp), parameter :: C13 = 1.0_dp / 3.0_dp

      ag = (1.0_dp - rhoast) * (w2 / 12.0_dp + wsd / 4.0_dp) + 0.5_dp * (rhoast + 1.0_dp) &
           * (C13 * w2 + C23 * wsd)
      d2 = hd - zb2

      terma = (4.0_dp * rhoast * cgd * cgd * dg * dg * wstr * wstr) / (w2 * d2) * (1.0_dp + lambda / d2)
      termb = 4.0_dp * cgd * dg * wstr

      bg = (1.0_dp - rhoast) * ((d2 + ds1) * (w2 + wsd) / 6.0_dp + ds1 * wsd * C13) &
           + 0.5_dp * (rhoast + 1.0_dp) &
           * ((ds1 + ds2 - d2) * (C13 * w2 + C23 * wsd) + (C23 * d2 + C13 * ds1) &
              * w2 + (C13 * d2 + C23 * ds1) * wsd) + terma - termb

      hsl = elu - zs

      cg = (1.0_dp - rhoast) * ((d2 + ds1)**2 * (w2 + wsd) / 12.0_dp + ds1**2 * wsd / 6.0_dp) &
           + 0.5_dp * (rhoast + 1.0_dp) * (ds1 + ds2 - d2) &
           * ((C23 * d2 + C13 * ds1) * w2 + (C13 * d2 + C23 * ds1) * wsd) - terma * hsl + termb * hsl

      det = bg * bg - 4.0_dp * ag * cg
      if (det < 0.0_dp) then
         imag = .true.
      else
         imag = .false.
         ds = (-bg + sqrt(det)) / (2.0_dp * ag)
      end if
   end subroutine flgsd2

   !> FLow General Structure calculate FU and RU
   !! The linearization coefficients FU and RU are
   !! calculated for the general structure.
   !! The stage of the flow was already determined.
   subroutine flgsfuru(fuL, ruL, u1L, auL, qL, dxL, dt, structwidth, state, &
                       flowDir, hu, hd, velhght, zs, ds, dg, dc, wstr, &
                       cwfa, cwd, mugfa, cgfa, cgda, dx_struc, lambda, Cz)
      ! Parameters
      integer, intent(in) :: state !< Flow condition of general structure: \n
      !< 0 : closed or dry\n
      !< 1 : free weir flow\n
      !< 2 : drowned weir flow\n
      !< 3 : free gate flow\n
      !< 4 : drowned gate flow\n
      real(kind=dp), intent(out) :: fuL !< fu component of momentum equation
      real(kind=dp), intent(out) :: ruL !< Right hand side component of momentum equation
      real(kind=dp), intent(inout) :: u1L !< Flow velocity at current time step
      real(kind=dp), intent(inout) :: qL !< Discharge through structure
      real(kind=dp), intent(inout) :: auL !< flow area
      real(kind=dp), intent(out) :: structwidth !< Flow width
      real(kind=dp), intent(in) :: dxL !< Length of flow link
      real(kind=dp), intent(in) :: dt !< Time step
      real(kind=dp), intent(in) :: cgda !< Contraction coefficient for drowned gate flow (adapted)
      real(kind=dp), intent(in) :: cgfa !< Contraction coefficient for gate flow (adapted)
      real(kind=dp), intent(in) :: cwd !< Contraction coefficient for drowned weir flow.
      real(kind=dp), intent(in) :: cwfa !< Contraction coefficient for free weir flow. (adapted)
      real(kind=dp), intent(in) :: dc !< Critical water level (free gate flow)
      real(kind=dp), intent(in) :: dg !< Gate opening height.
      real(kind=dp), intent(in) :: ds !< Water level immediately downstream the gate.
      real(kind=dp), intent(in) :: hd !< Downstream water level.
      real(kind=dp), intent(in) :: hu !< Upstream water level.
      real(kind=dp), intent(in) :: mugfa !< Vertical contraction coefficient for free gate flow (adapted)
      real(kind=dp), intent(in) :: flowDir !< Flow direction (+1/-1).
      real(kind=dp), intent(in) :: velhght !< Velocity height
      real(kind=dp), intent(in) :: wstr !< Width at centre of structure.
      real(kind=dp), intent(in) :: zs !< Bed level at centre of structure.
      real(kind=dp), intent(in) :: lambda !< extra resistance
      real(kind=dp), intent(in) :: cz !< Chezy value
      real(kind=dp), intent(in) :: dx_struc !< length of structure

      ! Local variables
      real(kind=dp) :: cu
      real(kind=dp) :: dh
      real(kind=dp) :: dxdt
      real(kind=dp) :: hs1 ! Water depth based on upstream energy level
      real(kind=dp) :: hs1w ! Water depth based on upstream water level
      real(kind=dp) :: mu
      real(kind=dp) :: rhsc
      real(kind=dp) :: ustru
      real(kind=dp) :: su
      real(kind=dp) :: sd
      logical, external :: iterfuru

      if (state == 0) then ! closed or dry
         fuL = 0.0_dp
         ruL = 0.0_dp
         u1L = 0.0_dp
         qL = 0.0_dp
         auL = 0.0_dp
         return
      end if

      ! Calculate upstream energy level w.r.t sill
      hs1 = hu + velhght - zs
      hs1w = hu - zs

      dxdt = dxL / dt

      if (state == 1) then ! free weir flow
         cu = cwfa**2 * gravity / 1.5_dp
         auL = wstr * hs1 * 2.0_dp / 3.0_dp
         ustru = cwfa * sqrt(gravity * 2.0_dp / 3.0_dp * hs1)
         rhsc = cu * (hd + velhght - zs) * flowDir
      else if (state == 2) then ! drowned weir flow
         cu = cwd**2 * 2.0_dp * gravity
         auL = wstr * ds
         dh = max(hs1 - ds, 0.0_dp)
         ustru = cwd * sqrt(gravity * 2.0_dp * dh)
         rhsc = cu * (hd + velhght - (ds + zs)) * flowDir
      else if (state == 3) then ! free gate flow
         mu = mugfa * cgfa
         cu = mu**2 * 2.0_dp * gravity
         auL = wstr * dg
         dh = max(hs1 - dc, 0.0_dp)
         ustru = mu * sqrt(gravity * 2.0_dp * dh)
         rhsc = cu * (hd + velhght - (dc + zs)) * flowDir
      else if (state == 4) then ! drowned gate flow
         mu = mugfa * cgda
         cu = mu**2 * 2.0_dp * gravity
         auL = wstr * dg
         dh = max(hs1 - ds, 0.0_dp)
         ustru = mu * sqrt(gravity * 2.0_dp * dh)
         rhsc = cu * (hd + velhght - (ds + zs)) * flowDir
      end if

      structwidth = wstr

      if (flowDir > 0) then
         su = hu
         sd = hd
      else
         sd = hu
         su = hd
      end if

      call furu_iter(fuL, ruL, su, sd, u1L, qL, auL, ustru, cu, rhsc, dxdt, dx_struc, hs1w, lambda, Cz)

      qL = auL * u1L
   end subroutine flgsfuru

   !> DPSEQU (EQUal test with real(kind=dp) interval EPSilon)\n
   !! Logical function to check if the difference between two double
   !! precision values is lower than a defined interval epsilon.
   logical function GS_dpsequ(dvar1, dvar2, eps)
      ! Parameters
      real(kind=dp), intent(in) :: dvar1 !< real(kind=dp) variable.
      real(kind=dp), intent(in) :: dvar2 !< real(kind=dp) variable.
      real(kind=dp), intent(in) :: eps !< Interval epsilon.

      GS_dpsequ = abs(dvar1 - dvar2) < eps
   end function GS_dpsequ

   !> deallocate general structure pointer
   subroutine deallocGenstru(genstru)
      ! Parameters
      type(t_GeneralStructure), pointer, intent(inout) :: genstru !< pointer to general structure data type

      if (associated(genstru%widthcenteronlink)) deallocate (genstru%widthcenteronlink)
      if (associated(genstru%gateclosedfractiononlink)) deallocate (genstru%gateclosedfractiononlink)
      if (associated(genstru%fu)) deallocate (genstru%fu)
      if (associated(genstru%ru)) deallocate (genstru%ru)
      if (associated(genstru%au)) deallocate (genstru%au)
      if (associated(genstru%sOnCrest)) deallocate (genstru%sOnCrest)
      if (associated(genstru%state)) deallocate (genstru%state)
      deallocate (genstru)
   end subroutine deallocGenstru

   !> Computes and sets the widths and gate lower edge levels on each of the flow links
   !! crossed by a general structure (gate/weir/true genstru).
   !! This is now an extended version of SOBEK's setLineStructure, because it also enables
   !! a sideways closing gate with two doors from the left and right side, where the partially
   !! closed portions have gate flow, and the center open portion still only has normal weir
   !! flow across the sill. \n
   !! NOTE: The implementation for gates coming in from left or right is not corrrect.
   !! The total crest width becomes incorrect, when the gatedooropening is less than half the totalwidth.
   subroutine update_widths(genstru, numlinks, links, wu, SkipDimensionChecks)
      ! Parameters
      type(t_generalStructure), intent(inout) :: genstru !< general structure data
      integer, intent(in) :: numlinks !< number of links
      integer, dimension(:), intent(in) :: links !< array containing linknumbers
      real(kind=dp), dimension(:), intent(in) :: wu !< flow widths
      logical, intent(in) :: SkipDimensionChecks !< Flag indicating if the dimension checks have to be performed

      ! Local variables
      real(kind=dp) :: crestwidth, totalWidth, closedWidth, closedGateWidthL, closedGateWidthR, help
      integer :: L0, Lf

      ! 1: First determine total width of all genstru links (TODO: AvD: we should not recompute this every user time step)
      totalWidth = 0.0_dp
      if (numlinks == 0) then
         return ! Only upon invalid input (see warnings in log about missing structure params)
      end if

      do L0 = 1, numlinks
         Lf = abs(links(L0))
         genstru%widthcenteronlink(L0) = wu(Lf)
         totalWidth = totalWidth + wu(Lf)
      end do

      if (SkipDimensionChecks) then
         genstru%ws_actual = genstru%ws
      else
         genstru%ws_actual = max(0.0_dp, min(totalWidth, genstru%ws))
      end if

      genstru%gateopeningwidth_actual = max(0.0_dp, min(genstru%ws_actual, genstru%gateopeningwidth))

      genstru%numlinks = numlinks
      if (numlinks == 1) then
         genstru%widthcenteronlink(1) = genstru%ws_actual
         ! gateclosedfraction will always be between 0 (= fully opened) and 1 (= fully closed)
         if (genstru%ws_actual /= 0.0_dp) then
            genstru%gateclosedfractiononlink(1) = 1.0_dp - genstru%gateopeningwidth_actual / genstru%ws_actual
         else
            genstru%gateclosedfractiononlink(1) = 1.0_dp
         end if

      else

         ! 2a: the desired crest width for this overall structure (hereafter, the open links for this genstru should add up to this width)
         !     Also: only for gates, the desired door opening width for this overall structure
         !           (should be smaller than crestwidth, and for this portion the open gate door is emulated by dummy very high lower edge level)

         crestwidth = genstru%ws_actual
         closedWidth = max(0.0_dp, totalWidth - crestwidth) / 2.0_dp ! Intentionally symmetric: if crest/sill_width < totalwidth. Only gate door motion may have a direction, was already handled above.

         if (genstru%openingDirection == GEN_FROMLEFT) then
            closedGateWidthL = closedWidth + max(0.0_dp, crestwidth - genstru%gateopeningwidth)
            closedGateWidthR = closedWidth
         else if (genstru%openingDirection == GEN_FROMRIGHT) then
            closedGateWidthL = closedWidth
            closedGateWidthR = closedWidth + max(0.0_dp, crestwidth - genstru%gateopeningwidth)
         else ! GEN_SYMMETRIC
            closedGateWidthL = closedWidth + max(0.0_dp, 0.5_dp * (crestwidth - genstru%gateopeningwidth))
            closedGateWidthR = closedWidth + max(0.0_dp, 0.5_dp * (crestwidth - genstru%gateopeningwidth))
         end if

         ! 2b: Determine the width that needs to be fully closed on 'left' side
         ! close the line structure from the outside to the inside: first step increasing increments
         ! NOTE: closed means: fully closed because sill_width (crest_width) is smaller that totalwidth.
         !       NOT because of gate door closing: that is handled by closedGateWidthL/R and may still
         !       have flow underneath doors if they are up high enough.
         genstru%gateclosedfractiononlink = 0.0_dp

         do L0 = 1, numlinks

            Lf = abs(links(L0))

            if (closedWidth > 0.0_dp) then
               help = min(wu(Lf), closedWidth)
               genstru%widthcenteronlink(L0) = wu(Lf) - help ! 0.0_dp if closed
               closedWidth = closedWidth - help
            else
               genstru%widthcenteronlink(L0) = wu(Lf)
            end if

            if (closedGateWidthL > 0.0_dp) then
               help = min(wu(Lf), closedGateWidthL)
               closedGateWidthL = closedGateWidthL - help
               if (wu(Lf) > 0.0_dp) then
                  genstru%gateclosedfractiononlink(L0) = genstru%gateclosedfractiononlink(L0) + help / wu(Lf)
               end if
            else

            end if

            if (closedWidth <= 0.0_dp .and. closedGateWidthL <= 0.0_dp) then
               ! finished
               exit
            end if
         end do

         ! 2c: Determine the width that needs to be fully closed on 'right' side
         ! close the line structure from the outside to the inside: first step increasing increments
         ! NOTE: closed means: fully closed because sill_width (crest_width) is smaller that totalwidth.
         !       NOT because of gate door closing: that is handled by closedGateWidthL/R and may still
         !       have flow underneath doors if they are up high enough.
         closedWidth = max(0.0_dp, totalWidth - crestwidth) / 2.0_dp ! Intentionally symmetric: if crest/sill_width < totalwidth. Only gate door motion may have a direction, was already handled above.
         do L0 = numlinks, 1, -1
            Lf = abs(links(L0))

            if (closedWidth > 0.0_dp) then
               help = min(wu(Lf), closedWidth)
               genstru%widthcenteronlink(L0) = wu(Lf) - help ! 0.0_dp if closed
               closedWidth = closedWidth - help
            else
               genstru%widthcenteronlink(L0) = wu(Lf)
            end if

            if (closedGateWidthR > 0.0_dp) then
               help = min(wu(Lf), closedGateWidthR)
               closedGateWidthR = closedGateWidthR - help

               if (wu(Lf) > 0.0_dp) then
                  genstru%gateclosedfractiononlink(L0) = genstru%gateclosedfractiononlink(L0) + help / wu(Lf)
               end if
            end if

            if (closedWidth <= 0.0_dp .and. closedGateWidthR <= 0.0_dp) then
               ! finished
               exit
            end if
         end do
      end if

   end subroutine update_widths

end module m_general_structure
