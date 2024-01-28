module m_General_Structure
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
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
   
   use m_tables
   use m_struc_helper
   
   implicit none
   
   double precision, public    :: extra_resist_genstruc = 0d0
   double precision, public    :: gatefrac_eps = 1d-5

   public ComputeGeneralStructure
   public dealloc
   public update_widths
   
   integer, public, parameter :: GEN_SYMMETRIC = 1
   integer, public, parameter :: GEN_FROMLEFT  = 2
   integer, public, parameter :: GEN_FROMRIGHT = 3

   interface dealloc
      module procedure deallocGenstru
   end interface dealloc


   type, public :: t_GeneralStructure ! see flgtar.f90
      double precision                 :: wu1                           !< w_u1
      double precision                 :: zu1                           !< z_u1
      double precision                 :: wu2                           !< w_u2
      double precision                 :: zu2                           !< z_u2
      double precision                 :: ws                            !< crest width (as defined in input/RTC)
      double precision                 :: ws_actual                     !< actual crest width (possibly limited by total width of flow links and must be > 0)
      double precision                 :: zs                            !< crest level (as defined in input/RTC)
      double precision                 :: zs_actual                     !< crest level (possibly adapted to BOB level, in case of multiple links the lowest point is taken).
      double precision                 :: wd1                           !< w_d1
      double precision                 :: zd1                           !< z_d1
      double precision                 :: wd2                           !< w_d2
      double precision                 :: zd2                           !< z_d2
      double precision                 :: gateLowerEdgeLevel            !< gate lower edge level (as defined in input/RTC)
      double precision                 :: gateLowerEdgeLevel_actual     !< gate lower edge level (possibly adapted to crest level, in case of multiple links the lowest point is taken) 
      double precision                 :: cgf_pos                       !< Positive free gate flow function 
      double precision                 :: cgd_pos                       !< Positive drowned gate flow function 
      double precision                 :: cwf_pos                       !< Positive free weir flow function 
      double precision                 :: cwd_pos                       !< Positive drowned weir flow function 
      double precision                 :: mugf_pos                      !< Positive flow contraction coefficient function 
      double precision                 :: cgf_neg                       !< Negative free gate flow function 
      double precision                 :: cgd_neg                       !< Negative drowned gate flow function 
      double precision                 :: cwf_neg                       !< Negative free weir flow function 
      double precision                 :: cwd_neg                       !< Negative drowned weir flow function 
      double precision                 :: mugf_neg                      !< Negative flow contraction coefficient function 
      double precision                 :: extraresistance               !< Extra resistance
      double precision                 :: gatedoorheight                !< height of the doors
      double precision                 :: gateopeningwidth              !< width between the doors (as defined in input/RTC)
      double precision                 :: gateopeningwidth_actual       !< width between the doors (possibly adapted to crest width and always > 0) 
      double precision                 :: crestlength                   !< length of the crest for computing the extra resistance using bedfriction over the crest of the weir
      double precision, pointer        :: widthcenteronlink(:) => null()          !< For each crossed flow link the the center width portion of this genstr. (sum(widthcenteronlink(1:numlink)) should equal widthcenter)
      double precision, pointer        :: gateclosedfractiononlink(:) => null()   !< part of the link width that is closed by the gate
      double precision, pointer        :: fu(:,:) => null()                       !< fu(1:3,L0) contains the partial computational value for fu (under/over/between gate, respectively)
      double precision, pointer        :: ru(:,:) => null()                       !< ru(1:3,L0) contains the partial computational value for ru (under/over/between gate, respectively)
      double precision, pointer        :: au(:,:) => null()                       !< au(1:3,L0) contains the partial computational value for au (under/over/between gate, respectively)
      integer                          :: numlinks                      !< Nr of flow links that cross this generalstructure.
      logical                          :: velheight                     !< Flag indicates the use of the velocity height or not
      integer                          :: openingDirection              !< possible values GEN_SYMMETRIC, GEN_FROMLEFT, GEN_FROMRIGHT
      double precision, pointer        :: sOnCrest(:) => null()                   !< water level on crest per link (length = numlinks)
      integer,          pointer        :: state(:,:) => null()                    !< state(1:3,L0) contains flow state on the L0th link of the structure for General Structure, Weir and Orifice
                                                                        !< 1: state of under gate flow, 2: state of over gate flow, 3: state of between gate flow
                                                                        !< 0 = No Flow
                                                                        !< 1 = Free Weir Flow
                                                                        !< 2 = Drowned Weir Flow
                                                                        !< 3 = Free Gate Flow
                                                                        !< 4 = Drowned Gate Flow
      integer                          :: allowedflowdir                !< allowed flow direction
                                                                        !< 0 all directions
                                                                        !< 1 only positive flow
                                                                        !< 2 only negative flow
                                                                        !< 3 no flow allowed
      logical                          :: uselimitFlowNeg               !< flag for limiting the maximum discharge through an orifice for negative flow  
      logical                          :: uselimitFlowPos               !< flag for limiting the maximum discharge through an orifice for positive flow   
      double precision                 :: limitFlowNeg                  !< maximal discharge in positive direction (in case useLimitFLowPos is true)
      double precision                 :: limitFlowpos                  !< maximal discharge in negative direction (in case useLimitFLowNeg is true
   end type


   private

contains

   !> compute FU, RU and AU for a single flow link in a general structure.
   subroutine computeGeneralStructure(genstr, direction, L0, maxWidth, bob0, fuL, ruL, auL, as1, as2, structwidth, kfuL, s1m1, s1m2, &
                                      qtotal, Cz, dxL, dt, SkipDimensionChecks)
      ! modules

      ! Global variables
      type(t_GeneralStructure), pointer, intent(inout) :: genstr               !< Derived type containing general structure information.
      integer,                           intent(in   ) :: direction            !< Orientation of flow link w.r.t. the structure. (1d0 for same direction, -1d0 for reverse.)
      integer,                           intent(in   ) :: L0                   !< Local link index.
      double precision,                  intent(inout) :: maxWidth             !< Maximal width of the structure. Normally the the width of the flow link.
      double precision,                  intent(in   ) :: bob0(2)              !< Bed level of channel upstream and downstream of the structure.
      double precision,                  intent(  out) :: fuL                  !< fu component of momentum equation.
      double precision,                  intent(  out) :: ruL                  !< Right hand side component of momentum equation.
      double precision,                  intent(inout) :: auL                  !< Flow area of structure opening.
      double precision,                  intent(in   ) :: as1                  !< (Geometrical) upstream flow area.
      double precision,                  intent(in   ) :: as2                  !< (Geometrical) downstream flow area.
      double precision,                  intent(  out) :: structwidth          !< Flow width of structure.
      integer,                           intent(  out) :: kfuL                 !< Flag indicating whether the structure link is wet (=1) or not (=0).
      double precision,                  intent(in   ) :: s1m1                 !< (Geometrical) upstream water level.
      double precision,                  intent(in   ) :: s1m2                 !< (Geometrical) downstream water level.
      double precision,                  intent(in   ) :: qtotal               !< Total discharge (in case of a compound structure this is not equal to 
                                                                               !< the discharge through the structure).
      double precision,                  intent(in   ) :: Cz                   !< Chezy value.
      double precision,                  intent(in   ) :: dxL                  !< Length of the flow link.
      double precision,                  intent(in   ) :: dt                   !< Time step (s).
      logical,                           intent(in   ) :: SkipDimensionChecks  !< Flag indicating whether the dimensions of the structure is to be limited
                                                                               !< by the cross sectional dimensions of the channel and correct, or not.
      !
      !
      ! Local variables
      !
      double precision :: alm
      double precision :: arm
      double precision :: s1ml
      double precision :: s1mr
      double precision :: qL
      double precision :: bobstru(2)             !< same as BOB0, but with respect to the structure orientation

      logical                        :: velheight
      integer                        :: allowedflowdir
      double precision               :: cgd
      double precision               :: cgf
      double precision               :: crest
      double precision               :: cwd
      double precision               :: cwf
      double precision               :: dg
      double precision               :: ds
      double precision               :: ds1
      double precision               :: ds2
      double precision               :: hd
      double precision               :: hu
      double precision               :: lambda
      double precision               :: mugf
      double precision               :: rhoast
      double precision               :: rholeft
      double precision               :: rhoright
      double precision               :: flowDir
      double precision               :: ud
      double precision               :: uu
      double precision               :: w2
      double precision               :: wsd
      double precision               :: wstr
      double precision               :: zb2
      double precision               :: zs
      double precision               :: zgate
      double precision               :: gatefraction
      double precision               :: gle
      double precision               :: dx_struc  
      double precision               :: dsL
      double precision               :: maxFlowL
      double precision               :: u1L           
      double precision, dimension(3) :: fu
      double precision, dimension(3) :: ru
      double precision, dimension(3) :: au
      
      !
      !
      !! executable statements -------------------------------------------------------
      !

      genstr%zs_actual = genstr%zs
      genstr%gateLowerEdgeLevel_actual = genstr%gateLowerEdgeLevel
      
      if (.not. SkipDimensionChecks) then
         crest   = max(bob0(1), bob0(2), genstr%zs)
      else
         crest = genstr%zs
         maxwidth = genstr%ws
      endif
      
      gle = max(crest, genstr%gateLowerEdgeLevel)
      genstr%gateLowerEdgeLevel_actual = gle
      ! upstream flow area should always be larger or equal to the flow area at the crest
      alm  = max(as1, auL)
      arm  = max(as2, auL)
      s1ml = s1m1
      s1mr = s1m2
      dsL   = s1m2 - s1m1 

      dx_struc = genstr%crestlength
      
      velheight = genstr%velheight
      !
      call UpAndDownstreamParameters(s1ml, s1mr, alm, arm, qtotal, velheight, &
                                     rholeft, rhoright, crest, hu, hd,uu, ud, flowDir)
      !
      ! apply orientation of the flow link to the direction dependend parameters
      
      if (SkipDimensionChecks) then
         bobstru = -1d5
      else if (direction > 0) then
         bobstru(1) = bob0(1)
         bobstru(2) = bob0(2)
      else
         bobstru(1) = bob0(2)
         bobstru(2) = bob0(1)
      endif
      
      allowedflowdir = genstr%allowedflowdir
      if ((allowedflowdir == 3) .or. &
          (direction*flowDir == 1  .and. allowedflowDir == 2) .or. &
          (direction*flowDir == -1 .and. allowedflowDir == 1)) then
        fuL = 0.0d0
        ruL = 0.0d0
        auL = 0.0d0
        kfuL = 0
        genstr%fu(:,L0) = 0.0d0
        genstr%ru(:,L0) = 0.0d0
        genstr%au(:,L0) = 0.0d0     
        return
      endif

      call flgtar(genstr, L0, maxWidth, bobstru, direction*flowDir, zs, wstr, w2, wsd, zb2, ds1, ds2, cgf, cgd,   &
                  cwf, cwd, mugf, lambda)
      !
      rhoast = rhoright/rholeft
      if (flowDir < 0.0) rhoast = 1.0d0 / rhoast
      !
      
      gatefraction = genstr%gateclosedfractiononlink(L0)
      
      fu = genstr%fu(:,L0) 
      ru = genstr%ru(:,L0) 
      au = genstr%au(:,L0) 
      if (gatefraction > gatefrac_eps) then
         ! calculate flow under gate
         dg = gle - zs

         u1L = ru(1) - fu(1)*dsL 
         qL = Au(1)*u1L

         call flqhgs(fu(1), ru(1), u1L, dxL, dt, structwidth, kfuL, au(1), qL, flowDir, &
                     hu, hd, uu, zs, gatefraction*wstr, w2, wsd, zb2, ds1, ds2, dg,                &
                     rhoast, cgf, cgd, cwf, cwd, mugf, lambda, Cz, dx_struc, ds, genstr%state(1,L0), velheight)
         genstr%sOnCrest(L0) = ds + crest     ! waterlevel on crest
         
         ! Flow limiter is only available for an orifice type structure. In this case only flow under the door 
         ! is possible. For General Structures (and Weirs) this limiter is not further implemented, only part "1"
         ! (out of 1:3) of the flow is limited.
         ! In 2D the maximum flow rate is divided over all individual links, where the limiter is applied for 
         ! each flow link individually, weighted by flow link width.

         ! qL is in orientation of structure (as are the limitFlow values)
         qL = direction * au(1) * (ru(1) - fu(1)*dsL )
         if (qL > 0d0 .and. genstr%uselimitFlowPos) then
            maxFlowL = genstr%limitFlowPos * gatefraction*wstr / genstr%ws_actual
            if (qL > maxFlowL) then
               fu(1) = 0.0
               ru(1) = direction * maxFlowL / max(1d-6,au(1))
            endif
         else if (qL < 0d0 .and. genstr%uselimitFlowNeg) then
            maxFlowL = genstr%limitFlowNeg * gatefraction*wstr / genstr%ws_actual
            if (abs(qL) > maxFlowL) then
               fu(1) = 0.0
               ru(1) = -  direction * maxFlowL /  max(1d-6,au(1))
            endif
       
         endif

         !calculate flow over gate
         dg = huge(1d0)
         zgate = gle+genstr%gatedoorheight
         u1L = ru(2) - fu(2)*dsL 
         qL = Au(2)*u1L

         call flqhgs(fu(2), ru(2), u1L, dxL, dt, structwidth, kfuL, au(2), qL, flowDir, &
                     hu, hd, uu, zgate, gatefraction*wstr, w2, wsd, zb2, ds1, ds2, dg,                &
                     rhoast, cgf, cgd, cwf, cwd, mugf, 0d0, 0d0, dx_struc, ds, genstr%state(2,L0), velheight)
      else
         fu(1) = 0d0
         ru(1) = 0d0
         fu(2) = 0d0
         ru(2) = 0d0
      endif
      
      if (gatefraction< 1d0 - gatefrac_eps) then
         ! calculate flow asif no door is present
         dg = huge(1d0)
         u1L = ru(3) - fu(3)*dsL 
         qL = Au(3)*u1L
         
         call flqhgs(fu(3), ru(3), u1L, dxL, dt, structwidth, kfuL, au(3), qL, flowDir, &
                     hu, hd, uu, zs, (1d0-gatefraction)*wstr, w2, wsd, zb2, ds1, ds2, dg,                &
                     rhoast, cgf, cgd, cwf, cwd, mugf, lambda, Cz, dx_struc, ds, genstr%state(3,L0), velheight)
         genstr%sOnCrest(L0) = ds + crest     ! waterlevel on crest

      else
         fu(3) = 0d0
         ru(3) = 0d0
      endif
      
      auL =  (au(1) + au(2)) + au(3)
      if (auL > 0d0) then
         fuL = (fu(1)*au(1) + fu(2)*au(2) + fu(3)*au(3))/auL
         ruL = (ru(1)*au(1) + ru(2)*au(2) + ru(3)*au(3))/auL
      else
         fuL = 0d0
         ruL = 0d0
      endif
      genstr%fu(:,L0) = fu
      genstr%ru(:,L0) = ru
      genstr%au(:,L0) = au
      !TEMP = laatste statement
      
   end subroutine computeGeneralStructure

   !> Compute coefficients for structure equation                                   
   subroutine flgtar(genstr, L0, maxWidth, bobstru, flowDir, zs, wstr, w2, wsd, zb2, ds1, ds2, cgf,  &
                     cgd, cwf, cwd, mugf, lambda)
   !!--description-----------------------------------------------------------------
   ! NONE
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
       use m_GlobalParameters
       
       implicit none
      !
      ! Global variables
      !
      type(t_GeneralStructure), pointer, intent(in):: genstr    !< Derived type containing general structure information
      integer,          intent(in   )  :: L0                    !< Internal link number
      double precision, intent(in   )  :: maxWidth              !<  Maximal width of the structure. Normally the the width of the flowlink
      double precision, intent(in   )  :: bobstru(2)            !< bed level of channel left and right of the structure (w.r.t. structure orientation)
      double precision, intent(  out)  :: cgd                   !< Contraction coefficient for drowned gate flow
      double precision, intent(  out)  :: cgf                   !< Contraction coefficient for gate flow
      double precision, intent(  out)  :: cwd                   !< Contraction coefficient for drowned weir flow.
      double precision, intent(  out)  :: cwf                   !< Contraction coefficient for free weir flow.
      double precision, intent(  out)  :: ds1                   !< Delta s1 general structure.
      double precision, intent(  out)  :: ds2                   !< Delta s2 general structure.
      double precision, intent(  out)  :: lambda                !< Extra resistance
      double precision, intent(  out)  :: mugf                  !< Vertical contraction coefficient for free gate flow.
      double precision, intent(in   )  :: flowDir               !< Flow direction (+1/-1). 
      double precision, intent(  out)  :: w2                    !< Width at right side of structure.
      double precision, intent(  out)  :: wsd                   !< Width structure right or left side.
      double precision, intent(  out)  :: wstr                  !< Width at centre of structure.
      double precision, intent(  out)  :: zb2                   !< Bed level at right side of structure.
      double precision, intent(  out)  :: zs                    !< Bed level at centre of structure.
      !
      !
      ! Local variables
      !
      double precision               :: help
      double precision               :: w1
      double precision               :: wsdl
      double precision               :: wsdr
      double precision               :: zb1
      double precision               :: zbsl
      double precision               :: zbsr
      !
      !
      !! executable statements -------------------------------------------------------
      !
     
      wstr = min(maxWidth, genstr%widthcenteronlink(L0))
      
      if (genstr%numlinks == 1) then
         ! ws_actual is determined in update_widths restrictions 0 < ws < maxwidth)
         genstr%ws_actual =  min(maxWidth, genstr%ws)
         wstr =genstr%ws_actual

         ! all other width parameters must always be <= maxWidth, but >= ws
         w1   = max(min(maxWidth, genstr%wu1), genstr%ws_actual)
         wsdl = max(min(maxWidth, genstr%wu2), genstr%ws_actual)
         wsdr = max(min(maxWidth, genstr%wd1), genstr%ws_actual)
         w2   = max(min(maxWidth, genstr%wd2), genstr%ws_actual)
      else  ! Structure crosses more than one link: nonsensible to use single width left/right etc. 
            ! same for all links. Use center linkwidth instead (i.e., typically wu(Lf))
         w1   = wstr
         wsdl = wstr
         wstr = wstr
         wsdr = wstr
         w2   = wstr
      endif
      
      ! zs always above bed level up and downstream (bob(:))
      zs   = max(bobstru(1), bobstru(2), genstr%zs)
      if (zs > genstr%zs_actual) then
         ! Note: the adaptation of the crest level at the different links depend on the BOB of this link
         ! The highest value is taken as the actual crest level
         genstr%zs_actual = zs
      endif
      
      
      ! other levels above bed level up and downstream (bob(:)) but below the zs
      zb1  = min(max(bobstru(1), genstr%zu1), zs)
      zbsl = min(max(bobstru(1), genstr%zu2), zs)
      zbsr = min(max(bobstru(2), genstr%zd1), zs)
      zb2  = min(max(bobstru(2), genstr%zd2), zs)
      
      lambda = genstr%extraresistance
      !
      !     Determine cgf, cgd, cwf, cwd, mugf
      !     (flow direction dependent)
      !
      if (flowDir > 0.0D0) then
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
      endif
      !
      !     Determine flow direction dependent parameters
      !
      if (flowDir > 0.0D0) then
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
      endif
   end subroutine flgtar

   !> FLow QH relation for General Structure
   subroutine flqhgs(fuL, ruL, u1L, dxL, dt, structwidth, kfuL, auL, qL, flowDir, &
                  hu, hd, uu, zs, wstr, w2, wsd, zb2, ds1, ds2,   &
                  dg, rhoast, cgf, cgd, cwf, cwd, mugf, lambda, Cz, dx_struc,  &
                  ds, state, velheight)
       use m_GlobalParameters
       implicit none
      !
      ! Global variables
      !
      integer, intent(out)            :: kfuL      !< Flag indicating whether the structure link is wet (=1) or not (=0)
      double precision, intent(inout) :: auL       !< flow area
      double precision, intent(inout) :: fuL       !< fu component of momentum equation
      double precision, intent(inout) :: ruL       !< Right hand side component of momentum equation
      double precision, intent(inout) :: u1L       !< Flow velocity at current time step
      double precision, intent(inout) :: qL        !< Discharge through structure
      double precision, intent(in)    :: dxL       !< Length of flow link
      double precision, intent(in)    :: dt        !< Time step 
      double precision, intent(out)  :: structwidth      !< Flow width
      double precision, intent(in)   :: cgd        !< Contraction coefficient for drowned gate flow
      double precision, intent(in)   :: cgf        !< Contraction coefficient for gate flow
      double precision, intent(in)   :: cwd        !< Contraction coefficient for drowned weir flow.
      double precision, intent(in)   :: cwf        !< Contraction coefficient for free weir flow.
      double precision, intent(in)   :: dg         !< Gate opening height.
      double precision, intent(inout):: ds         !< Water level immediately downstream the gate.
      double precision, intent(in)   :: ds1        !< Delta s1 general structure.
      double precision, intent(in)   :: ds2        !< Delta s2 general structure.
      double precision, intent(in)   :: hd         !< Downstream water level.
      double precision, intent(in)   :: hu         !< Upstream water level.
      double precision, intent(in)   :: lambda     !< Extra resistance
      double precision, intent(in)   :: Cz         !< Chezy value
      double precision, intent(in)   :: mugf       !< Vertical contraction coefficient for free gate flow.
      double precision, intent(in)   :: rhoast     !< Ratio of density right and left of structure
      double precision, intent(in)   :: flowDir    !< Flow direction (+1/-1).
      double precision, intent(in)   :: uu         !< Upstream velocity (with velheight setting already accounted for).
      double precision, intent(in)   :: w2         !< Width at right side of structure.
      double precision, intent(in)   :: wsd        !< Width structure right or left side.
      double precision, intent(in)   :: wstr       !< Width at centre of structure.
      double precision, intent(in)   :: zb2        !< Bed level at right side of structure.
      double precision, intent(in)   :: zs         !< Bed level at centre of structure.
      integer, intent(out)           :: state      !< Flow state of the structure
      double precision, intent(in)   :: dx_struc   !< length of structure
      logical, intent(in)            :: velheight  !< logical indicates whether the momentum equation has to be taken into account
      
      !
      !
      ! Local variables
      !
      logical                        :: imag
      double precision               :: cgd2
      double precision               :: cgda
      double precision               :: cgfa
      double precision               :: cwfa
      double precision               :: dc
      double precision               :: dlim
      double precision               :: elu
      double precision               :: hd1
      double precision               :: hs1
      double precision               :: mugfa
      double precision               :: velhght
      !
      !
      !! executable statements -------------------------------------------------------
      !

      if (velheight) then
         velhght = uu*uu/(2.0D0*gravity)
      else
         velhght = 0d0
      end if

      elu = hu + velhght
      hs1 = elu - zs
      !
      if (hs1<=0.0D0 .or. wstr<=0.0D0 .or. min(cgf, cgd, cwf, cwd)<=0. .or.       &
        & dg<.0001) then          !hk: or gate closed
         state = 0
         ds = hs1
        else
         !
         !        Compute critical water depth at the
         !        sill, dc and water depth at the sill,ds
         !
           if (.not. velheight) then
              ds = hd-zs
           else
              
              dlim = hs1*(wstr/w2*2./3.*sqrt(2./3.))**(2.0/3.0)
              hd1 = max(hd, zb2 + dlim*0.9D0)
              !
              !
              !        Calculate ds by solving third order algebraic equation
              !
              call flgsd3(wsd, wstr, zs, w2, zb2, ds1, ds2, elu, hd1, rhoast, cwd, ds, &
                        & lambda)
           endif
              
           dc = 2.0D0/3.0D0*hs1
           
          !
           if (ds>=dc) then
             if (dg>=ds) then
                !
                !              - drowned weir -
                !
                state = 2
             else
                !
                !              - gate flow -
                !
                state = 3
                !
                !              adapt coefficients on basis of Ds & Cwd
                !
                call flccgs(dg, ds, cgd, cgf, cwd, mugf, cgda, cgfa, mugfa)
             endif
         else
            !
            !           Adapt Cwf coefficient
            !
            if (cwf<cwd) then
               if (GS_dpsequ(dc, 0.0D0, 1.0D-20)) then
                  cwfa = cwf
               else
                  cwfa = max(ds/dc*cwd, cwf)
               endif
            elseif (ds>0.0D0) then
               cwfa = min(dc/ds*cwd, cwf)
            else
               cwfa = cwf
            endif
            !
            if (dg>=dc) then
               !
               !              - free weir -
               !
               state = 1
               ds = dc
            else
               !
               !              - gate flow -
               !
               state = 3
               !
               !              adapt coefficients on basis of Dc & Cwf
               !
               call flccgs(dg, dc, cgd, cgf, cwfa, mugf, cgda, cgfa, mugfa)
            endif
         endif
         !
         !        In case of gate flow determine type of gate flow
         !        (drowned or free)
         !
         if (state==3) then
            dc = mugfa*dg
            !
            !           Cgd for second order equation = Cgd' * Mu'
            !
            cgd2 = cgda*mugfa
            !
            if (velheight) then
               call flgsd2(wsd, wstr, zs, w2, zb2, dg, ds1, ds2, elu, hd1, rhoast,   &
                         & cgd2, imag, ds, lambda)
            else 
               ds = hd-zs
               imag = .false.
            endif
            !
            if (imag) then
               !
               !              - free gate -
               !
               state = 3
               ds = dc
            elseif (ds<=dc) then
               !
               !              - free gate -
               !
               state = 3
               !
               !             Adapt coefficients
               !
               if (cgda>cgfa) then
                  if (.not.GS_dpsequ(dc, 0.0D0, 1.0D-20)) then
                     cgfa = max(ds/dc*cgda, cgfa)
                  endif
               elseif (ds>0.0D0) then
                  cgfa = min(dc/ds*cgda, cgfa)
               else
               endif
               ds = dc
            !TEM          WRITE (11,*) 'cgfa,mugfa',cgfa,mugfa
            else
               !
               !             - drowned gate -
               !
               state = 4
            endif
         endif
      !
      !
      endif
      !
      !TEM    WRITE (11,*) 'state,ds,dc,dg',state,ds,dc,dg
      !
      !       The flowe condition is known so calculate
      !       the linearization coefficients FU and RU
      !
      call flgsfuru(fuL, ruL, u1L, auL, qL, dxL, dt, structwidth, kfuL, state, &
                     flowDir, hu, hd, velhght, zs, ds, dg, dc, wstr,   &
                     cwfa, cwd, mugfa, cgfa, cgda, dx_struc, lambda, Cz)
   end subroutine flqhgs


   !>  Compute water depth ds at the sill by solving a third order algebraic equation. \n
   !!  In case of drowned weir flow the water level atthe sill is required. The water 
   !!  depth is calculated in this routine.                  
   subroutine flgsd3(wsd, wstr, zs, w2, zb2, ds1, ds2, elu, hd, rhoast, cwd,   &
                & ds, lambda)
      implicit none
      !
      ! Local parameters
      !
      double precision, parameter :: c23 = 2.0D0/3.0D0, c13 = 1.0D0/3.0D0
      !
      ! Global variables
      !
      double precision, intent(in)   :: cwd      !< 
      double precision, intent(out)  :: ds       !< 
      double precision, intent(in)   :: ds1      !< 
      double precision, intent(in)   :: ds2      !< 
      double precision, intent(in)   :: elu      !< 
      double precision, intent(in)   :: hd       !< 
      double precision, intent(in)   :: lambda   !< 
      double precision, intent(in)   :: rhoast   !< 
      double precision, intent(in)   :: w2       !< 
      double precision, intent(in)   :: wsd      !< 
      double precision, intent(in)   :: wstr     !< 
      double precision, intent(in)   :: zb2      !< 
      double precision, intent(in)   :: zs       !< 
      !
      !
      ! Local variables
      !
      double precision               :: aw
      double precision               :: bw
      double precision               :: cw
      double precision               :: d2
      double precision               :: fac
      double precision               :: h2a
      double precision               :: h2b
      double precision               :: h2c
      double precision               :: hsl
      double precision               :: hulp
      double precision               :: hulp1
      double precision               :: p
      double precision               :: phi
      double precision               :: q
      double precision               :: r60
      double precision               :: term
      double precision               :: u
      double precision               :: v
      !
      !
      !! executable statements -------------------------------------------------------
      !
      d2 = hd - zb2
      hsl = elu - zs
      !JK   WRITE (11,*)  'hsl',hsl
      term = ((4.0D0*cwd*cwd*rhoast*wstr*wstr)/(w2*d2))*(1.0D0 + lambda/d2)
      !
      aw = ( - term*hsl - 4.0D0*cwd*wstr + (1.0D0 - rhoast)                       &
         & *(w2/12.0D0 + wsd/4.0D0) + 0.5D0*(rhoast + 1.0D0)*(c13*w2 + c23*wsd))  &
         & /term
      !
      bw = (4.0D0*cwd*wstr*hsl + (1.0D0 - rhoast)                                 &
         & *((d2 + ds1)*(w2 + wsd)/6.D0 + ds1*wsd*c13) + 0.5D0*(rhoast + 1.0D0)   &
         & *((ds1 + ds2 - d2)*(c13*w2 + c23*wsd) + (c23*d2 + c13*ds1)             &
         & *w2 + (c13*d2 + c23*ds1)*wsd))/term
      !
      cw = ((1.0D0 - rhoast)*((d2 + ds1)**2*(w2 + wsd)/12.D0 + ds1**2*wsd/6.0D0)  &
         & + 0.5D0*(rhoast + 1.0D0)*(ds1 + ds2 - d2)                              &
         & *((c23*d2 + c13*ds1)*w2 + (c13*d2 + c23*ds1)*wsd))/term
      !
      !     Solve the equation ds**3 + aw*ds**2 + bw*ds +cw to get the water
      !     level at the sill
      !
      p = bw/3.0D0 - aw*aw/9.0D0
      q = aw*aw*aw/27.0D0 - aw*bw/6.0D0 + cw/2.0D0
      hulp = q*q + p*p*p
      !
      if (hulp<0.0D0) then
         p = abs(p)
         phi = acos(abs(q)/p/sqrt(p))/3.0D0
         r60 = acos(0.5D0)
         fac = sign(2.D0, q)*sqrt(p)
         h2a = -fac*cos(phi)
         h2b = fac*cos(r60 - phi)
         h2c = fac*cos(r60 + phi)
         ds = max(h2a, h2b, h2c) - aw/3.0D0
      else
         hulp = sqrt(hulp)
         hulp1 = -q + hulp
         if (abs(hulp1)<1E-6) then
            u = 0 ; v = 0
         else       ! hk: ook fix for Erwin, ARS 15132
            u = abs(hulp1)**c13*sign(1.0D0, hulp1)
            hulp1 = -q - hulp
            v = abs(hulp1)**c13*sign(1.0D0, hulp1)
         endif
         ds = u + v - aw/3.0D0
      endif
      ds = min(ds, elu-zs, hd-zs)
   end subroutine flgsd3
                
                
   !> FLow contraction coefficients for general structure.\n
   !! In the formulas for the gate and weir several coefficients are applied. 
   !! To avoid discontinuities in the transition from weir to gate flow, the
   !!correction coefficient cgd should be corrected.
   subroutine flccgs(dg, dsc, cgd, cgf, cw, mugf, cgda, cgfa, mugfa)
      implicit none
      !
      ! Global variables
      !
      double precision, intent(in)   :: cgd     !< Correction coefficient for drowned gate flow.
      double precision, intent(out)  :: cgda    !< Adapted correction coefficient for drowned gate flow.
      double precision, intent(in)   :: cgf     !< Correction coefficient for free gate flow.
      double precision, intent(out)  :: cgfa    !< Adapted correction coefficient for free gate flow.
      double precision, intent(in)   :: cw      !< Correction coefficient for weir flow.
      double precision, intent(in)   :: dg      !< Gate opening height.
      double precision, intent(in)   :: dsc     !< Depth at sill or critical depth.
      double precision, intent(in)   :: mugf    !< Contraction coefficient for free gate flow.
      double precision, intent(out)  :: mugfa   !< Adapted contraction coefficient for free gate flow.
      !
      !
      !! executable statements -------------------------------------------------------
      !
      if (.not.GS_dpsequ(dsc, 0.0D0, 1.D-20)) then
         !
         if (dg/dsc>mugf) then
            mugfa = dg/dsc
         else
            mugfa = mugf
         endif
         !
         if (cgd>cw) then
            if (GS_dpsequ(dg, 0.0D0, 1.0D-20)) then
               cgda = cgd
            else
               cgda = min(dsc/dg*cw, cgd)
            endif
         else
            cgda = max(dg/dsc*cw, cgd)
         endif
         !
         if (cgf>cw) then
            if (GS_dpsequ(dg, 0.0D0, 1.0D-20)) then
               cgfa = cgf
            else
               cgfa = min(dsc/dg*cw, cgf)
            endif
         else
            cgfa = max(dg/dsc*cw, cgf)
         endif
      !
      else
         mugfa = mugf
         cgda = cgd
         cgfa = cgf
      endif
   end subroutine flccgs


   !> FLGSD2 (FLow Gen. Struct. Depth sill 2nd ord. eq.)\n
   !! Compute water depth ds at the sill by a second order algebraic equation.
   !! In case of drowned gate flow the water level at the sill is required. 
   !! The water depth is calculated in this routine.
   subroutine flgsd2(wsd, wstr, zs, w2, zb2, dg, ds1, ds2, elu, hd, rhoast,    &
                   & cgd, imag, ds, lambda)
      implicit none
      !
      ! Local parameters
      !
      double precision, parameter :: c23 = 2.0D0/3.0D0, c13 = 1.0D0/3.0D0
      !
      ! Global variables
      !
      logical, intent(out)           :: imag      !< Logical indicator, = TRUE when determinant of second order algebraic equation less than zero.
      double precision, intent(in)   :: cgd       !< Correction coefficient for drowned gate flow.
      double precision, intent(in)   :: dg        !< Gate opening height.
      double precision, intent(out)  :: ds        !< Water level immediately downstream the gate.
      double precision, intent(in)   :: ds1       !< Delta s1 general structure.
      double precision, intent(in)   :: ds2       !< Delta s2 general structure.
      double precision, intent(in)   :: elu       !< Upstream energy level.
      double precision, intent(in)   :: hd        !< Downstream water level.
      double precision, intent(in)   :: lambda    !< Extra resistance in general structure.
      double precision, intent(in)   :: rhoast    !< Downstream water density divided by upstream water density.
      double precision, intent(in)   :: w2        !< Width at right side of structure.
      double precision, intent(in)   :: wsd       !< Width structure right or left side.
      double precision, intent(in)   :: wstr      !< Width at centre of structure.
      double precision, intent(in)   :: zb2       !< Bed level at right side of structure.
      double precision, intent(in)   :: zs        !< Bed level at centre of structure.
      !
      !
      ! Local variables
      !
      double precision               :: ag
      double precision               :: bg
      double precision               :: cg
      double precision               :: d2
      double precision               :: det
      double precision               :: hsl
      double precision               :: terma
      double precision               :: termb
      !
      !
      !! executable statements -------------------------------------------------------
      !
      !
      ag = (1.0D0 - rhoast)*(w2/12.0D0 + wsd/4.0D0) + 0.5D0*(rhoast + 1.0D0)      &
         & *(c13*w2 + c23*wsd)
      d2 = hd - zb2
      !
      terma = (4.0D0*rhoast*cgd*cgd*dg*dg*wstr*wstr)/(w2*d2)*(1.0D0 + lambda/d2)
      termb = 4.0D0*cgd*dg*wstr
      !
      bg = (1.0D0 - rhoast)*((d2 + ds1)*(w2 + wsd)/6.D0 + ds1*wsd*c13)            &
         & + 0.5D0*(rhoast + 1.0D0)                                               &
         & *((ds1 + ds2 - d2)*(c13*w2 + c23*wsd) + (c23*d2 + c13*ds1)             &
         & *w2 + (c13*d2 + c23*ds1)*wsd) + terma - termb
      !
      hsl = elu - zs
      !
      cg = (1.0D0 - rhoast)*((d2 + ds1)**2*(w2 + wsd)/12.D0 + ds1**2*wsd/6.0D0)   &
         & + 0.5D0*(rhoast + 1.0D0)*(ds1 + ds2 - d2)                              &
         & *((c23*d2 + c13*ds1)*w2 + (c13*d2 + c23*ds1)*wsd) - terma*hsl +        &
         & termb*hsl
      !
      det = bg*bg - 4.0D0*ag*cg
      if (det<0.0D0) then
         imag = .true.
      !JK      WRITE (11,*) 'Det=',det
      else
         imag = .false.
         ds = ( - bg + sqrt(det))/(2.0D0*ag)
      endif
   end subroutine flgsd2


   !> FLow General Structure calculate FU and RU \n
   !!\n
   !! The linearization coefficients FU and RU are
   !! calculated for the general structure.\n
   !! The stage of the flow was already determined.
   subroutine flgsfuru(fuL, ruL, u1L, auL, qL, dxL, dt, structwidth, kfuL, state, &
                       flowDir, hu, hd, velhght, zs, ds, dg, dc, wstr,&
                       cwfa, cwd, mugfa, cgfa, cgda, dx_struc, lambda, Cz)
      use m_GlobalParameters
      implicit none
      !
      ! Local parameters
      !
      double precision, parameter :: relax = 0.0D0, alfa = 0.9D0
      !
      ! Global variables
      !
      integer, intent(in)            :: state      !< Flow condition of general structure: \n
                                                   !< 0 : closed or dry\n
                                                   !< 1 : free weir flow\n
                                                   !< 2 : drowned weir flow\n
                                                   !< 3 : free gate flow\n
                                                   !< 4 : drowned gate flow\n
      integer, intent(out)           :: kfuL       !< Flag indicating whether the structure link is wet (=1) or not (=0)
      double precision, intent(out)  :: fuL        !< fu component of momentum equation
      double precision, intent(out)  :: ruL        !< Right hand side component of momentum equation
      double precision, intent(inout):: u1L        !< Flow velocity at current time step
      double precision, intent(inout):: qL         !< Discharge through structure
      double precision, intent(inout):: auL        !< flow area 
      double precision, intent(out)  :: structwidth      !< Flow width
      double precision, intent(in)   :: dxL        !< Length of flow link
      double precision, intent(in)   :: dt         !< Time step
      double precision, intent(in)   :: cgda       !< Contraction coefficient for drowned gate flow (adapted)
      double precision, intent(in)   :: cgfa       !< Contraction coefficient for gate flow (adapted)
      double precision, intent(in)   :: cwd        !< Contraction coefficient for drowned weir flow.
      double precision, intent(in)   :: cwfa       !< Contraction coefficient for free weir flow. (adapted)
      double precision, intent(in)   :: dc         !< Critical water level (free gate flow)
      double precision, intent(in)   :: dg         !< Gate opening height.
      double precision, intent(in)   :: ds         !< Water level immediately downstream the gate.
      double precision, intent(in)   :: hd         !< Downstream water level.
      double precision, intent(in)   :: hu         !< Upstream water level.
      double precision, intent(in)   :: mugfa      !< Vertical contraction coefficient for free gate flow (adapted)
      double precision, intent(in)   :: flowDir    !< Flow direction (+1/-1).
      double precision, intent(in)   :: velhght    !< Velocity height
      double precision, intent(in)   :: wstr       !< Width at centre of structure.
      double precision, intent(in)   :: zs         !< Bed level at centre of structure.
      double precision, intent(in)   :: lambda     !< extra resistance
      double precision, intent(in)   :: cz         !< Chezy value
      double precision, intent(in)   :: dx_struc   !< length of structure
      !
      !
      ! Local variables
      !
      double precision               :: cu
      double precision               :: dh
      double precision               :: dsqrt
      double precision               :: dxdt
      double precision               :: hs1  ! Water depth based on upstream energy level
      double precision               :: hs1w ! Water depth based on upstream water level
      double precision               :: mu
      double precision               :: rhsc
      double precision               :: ustru
      double precision               :: su
      double precision               :: sd

      logical, external              :: iterfuru
      !
      !! executable statements -------------------------------------------------------
      !
       !
      if (state==0) then
         !        closed or dry
         kfuL = 0
         fuL = 0.0
         ruL = 0.0
         u1L = 0.0
         qL = 0.0
         auL = 0.0
         return
      endif
      !
      !     Calculate upstream energy level w.r.t sill
      !
      hs1  = hu + velhght - zs
      hs1w = hu - zs
      !
      dxdt = dxL/dt

      if (state==1) then
         !           free weir flow
         cu = cwfa**2*gravity/1.5D0
         !TEM        WRITE (11,*) cu,cwfa
         auL = wstr*hs1*2.0D0/3.0D0
         ustru = cwfa*dsqrt(gravity*2.0D0/3.0D0*hs1)
         rhsc = cu*(hd + velhght - zs)*flowDir
      elseif (state==2) then
         !           drowned weir flow
         cu = cwd**2*2.0D0*gravity
         auL = wstr*ds
         dh = max(hs1 - ds, 0.D0)
         ustru = cwd*dsqrt(gravity*2.0D0*dh)
         rhsc = cu*(hd + velhght - (ds + zs))*flowDir
      elseif (state==3) then
         !           free gate flow
         mu = mugfa*cgfa
         cu = mu**2*2.0D0*gravity
         auL = wstr*dg
         dh = max(hs1 - dc, 0.D0)
         ustru = mu*dsqrt(gravity*2.0D0*dh)
         rhsc = cu*(hd + velhght - (dc + zs))*flowDir
      elseif (state==4) then
         !           drowned gate flow
         mu = mugfa*cgda
         cu = mu**2*2.0D0*gravity
         auL = wstr*dg
         dh = max(hs1 - ds, 0.D0)
         ustru = mu*dsqrt(gravity*2.0D0*dh)
         rhsc = cu*(hd + velhght - (ds + zs))*flowDir
      endif
      
      structwidth = wstr
      !
      if (flowDir>0) then
          su = hu
          sd = hd
      else
          sd = hu
          su = hd
      endif
      
      call furu_iter(fuL, ruL, su, sd, u1L, qL, auL, ustru, cu, rhsc, dxdt, dx_struc, hs1w, lambda, Cz)

      qL = auL*u1L
   end subroutine flgsfuru


   !> DPSEQU (EQUal test with Double precision interval EPSilon)\n
   !! Logical function to check if the difference between two double 
   !! precision values is lower than a defined interval epsilon.
   logical function GS_dpsequ(dvar1, dvar2, eps)

      implicit none
      !
      ! Global variables
      !
      double precision, intent(in)   :: dvar1   !< Double precision variable.
      double precision, intent(in)   :: dvar2   !< Double precision variable.
      double precision, intent(in)   :: eps     !< Interval epsilon.
      !
      !
      !! executable statements -------------------------------------------------------
      !
      GS_dpsequ = abs(dvar1 - dvar2)<eps
   end function GS_dpsequ
   
   !> deallocate general structure pointer
   subroutine deallocGenstru(genstru)
      implicit none
      
      type(t_GeneralStructure), pointer, intent(inout) :: genstru !< pointer to general structure data type
      
      if (associated(genstru%widthcenteronlink       )) deallocate(genstru%widthcenteronlink       )
      if (associated(genstru%gateclosedfractiononlink)) deallocate(genstru%gateclosedfractiononlink)
      if (associated(genstru%fu                      )) deallocate(genstru%fu                    )
      if (associated(genstru%ru                      )) deallocate(genstru%ru                    )
      if (associated(genstru%au                      )) deallocate(genstru%au                    )
      if (associated(genstru%sOnCrest                )) deallocate(genstru%sOnCrest              )
      if (associated(genstru%state                   )) deallocate(genstru%state              )
      deallocate(genstru)
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
      implicit none

      type(t_generalStructure), intent(inout)          :: genstru      !< general structure data
      integer,                  intent(in   )          :: numlinks     !< number of links
      integer, dimension(:),    intent(in   )          :: links        !< array containing linknumbers
      double precision, dimension(:),    intent(in   ) :: wu           !< flow widths
      logical,                  intent(in   )          :: SkipDimensionChecks     !< Flag indicating if the dimension checks have to be performed
     
      double precision :: crestwidth, totalWidth, closedWidth, closedGateWidthL, closedGateWidthR, help
      integer :: ng, L, L0, Lf

      ! 1: First determine total width of all genstru links (TODO: AvD: we should not recompute this every user time step)
      totalWidth = 0d0
      if (numlinks == 0) then
         return ! Only upon invalid input (see warnings in log about missing structure params)
      end if

      do L0=1,numlinks
         Lf = iabs(links(L0))
         genstru%widthcenteronlink(L0) = wu(Lf)
         totalWidth = totalWidth + wu(Lf)
      end do

      if (SkipDimensionChecks) then
         genstru%ws_actual = genstru%ws
      else
         genstru%ws_actual = max(0d0, min(totalWidth, genstru%ws))
      endif
      
      genstru%gateopeningwidth_actual = max(0d0, min(genstru%ws_actual, genstru%gateopeningwidth))

      genstru%numlinks= numlinks
      if (numlinks==1) then
         genstru%widthcenteronlink(1) = genstru%ws_actual
         ! gateclosedfraction will always be between 0 (= fully opened) and 1 (= fully closed)
         if (genstru%ws_actual /= 0d0) then
            genstru%gateclosedfractiononlink(1) = 1d0 - genstru%gateopeningwidth_actual/genstru%ws_actual
         else
            genstru%gateclosedfractiononlink(1) = 1d0
         endif
         
      else

         ! 2a: the desired crest width for this overall structure (hereafter, the open links for this genstru should add up to this width)
         !     Also: only for gates, the desired door opening width for this overall structure
         !           (should be smaller than crestwidth, and for this portion the open gate door is emulated by dummy very high lower edge level)
         
         crestwidth = genstru%ws_actual
         closedWidth = max(0d0, totalWidth - crestwidth)/2d0 ! Intentionally symmetric: if crest/sill_width < totalwidth. Only gate door motion may have a direction, was already handled above.
   
         if (genstru%openingDirection == GEN_FROMLEFT) then
            closedGateWidthL = closedWidth + max(0d0, crestwidth - genstru%gateopeningwidth)
            closedGateWidthR = closedwidth
         else if (genstru%openingDirection == GEN_FROMRIGHT) then
            closedGateWidthL = closedWidth
            closedGateWidthR = closedWidth + max(0d0, crestwidth - genstru%gateopeningwidth)
         else ! GEN_SYMMETRIC
            closedGateWidthL = closedWidth + max(0d0, .5d0*(crestwidth - genstru%gateopeningwidth))
            closedGateWidthR = closedWidth + max(0d0, .5d0*(crestwidth - genstru%gateopeningwidth))
         end if

         ! 2b: Determine the width that needs to be fully closed on 'left' side
         ! close the line structure from the outside to the inside: first step increasing increments
         ! NOTE: closed means: fully closed because sill_width (crest_width) is smaller that totalwidth.
         !       NOT because of gate door closing: that is handled by closedGateWidthL/R and may still
         !       have flow underneath doors if they are up high enough.
         genstru%gateclosedfractiononlink = 0d0
   
         do L0=1,numlinks
            
            Lf = iabs(links(L0))

            if (closedWidth > 0d0) then
               help = min (wu(Lf), closedWidth)
               genstru%widthcenteronlink(L0) = wu(Lf) - help ! 0d0 if closed
               closedWidth = closedWidth - help
            else
               genstru%widthcenteronlink(L0) = wu(Lf)
            end if

            if (closedGateWidthL > 0d0 ) then
               help = min (wu(Lf), closedGateWidthL)
               closedGateWidthL = closedGateWidthL - help
               if ( wu(Lf).gt.0d0 ) then
                  genstru%gateclosedfractiononlink(L0) = genstru%gateclosedfractiononlink(L0) + help/wu(Lf)
               end if
            else    
         
            end if

            if (closedWidth <= 0d0 .and. closedGateWidthL <= 0d0) then
               ! finished
               exit
            endif
         enddo

         ! 2c: Determine the width that needs to be fully closed on 'right' side
         ! close the line structure from the outside to the inside: first step increasing increments
         ! NOTE: closed means: fully closed because sill_width (crest_width) is smaller that totalwidth.
         !       NOT because of gate door closing: that is handled by closedGateWidthL/R and may still
         !       have flow underneath doors if they are up high enough.
         closedWidth = max(0d0, totalWidth - crestwidth)/2d0 ! Intentionally symmetric: if crest/sill_width < totalwidth. Only gate door motion may have a direction, was already handled above.
         do L0=numlinks,1,-1
            Lf = iabs(links(L0))

            if (closedWidth > 0d0) then
               help = min (wu(Lf), closedWidth)
               genstru%widthcenteronlink(L0) = wu(Lf) - help ! 0d0 if closed
               closedWidth = closedWidth - help
            else
               genstru%widthcenteronlink(L0) = wu(Lf)
            end if

            if (closedGateWidthR > 0d0) then
               help = min (wu(Lf), closedGateWidthR)
               closedGateWidthR = closedGateWidthR - help
         
               if ( wu(Lf).gt.0d0 ) then
                  genstru%gateclosedfractiononlink(L0) = genstru%gateclosedfractiononlink(L0) + help/wu(Lf)
               end if
            end if

             if (closedWidth <= 0d0 .and. closedGateWidthR <= 0d0) then
               ! finished
               exit
            endif
         enddo
   
      endif 

   end subroutine update_widths

end module m_General_Structure
