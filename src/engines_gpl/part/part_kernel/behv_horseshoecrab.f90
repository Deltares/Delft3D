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

module behv_horseshoecrab_mod
!
!  data definition module(s)
!
use m_stop_exit
use m_waq_precision          ! single/double precision
use timers
!
!  module procedure(s)
!
use orien_bathymetry_mod           ! explicit interface
use vert_swimm_tidal_mod           ! explicit interface
use vert_swimm_diurnal_mod         ! explicit interface
use intpltd_diurnal_mod            ! explicit interface
!
implicit none

contains
    subroutine behv_horseshoecrab ( btype    , hbtype  , v_swim , d_swim   , n        ,   &
                             m        , nmax    , mmax   , mnmaxk , lgrid       ,   &
                             lgrid2   , lgrid3  , nosegl , wpart  , ipart       ,   &
                             wsettl   , k       , kpart  , zpart  , xpart       ,   &
                             ypart    , nolay   , idelt  , day    , phase_diurn ,   &
                             ebb_flow , flow    , depth  , salin1 , temper1     ,   &
                             vol1     , vol2    , vel1   , vel2   , fstage      ,   &
                             ztop     , zlevel  , zdepth , zbot   , buoy        ,   &
                             vzact    , vz      , buoy1  , buoy2  , ztop1       ,   &
                             ztop2    , zbot1   , zbot2  , vzact1 , vzact2      ,   &
                             vswim1   , vswim2  , iseg   , lunrep , angle  )

        ! function  : Horse shoe crab (Carcinoscorpius rotundicauda) specific behaviour collected 
        !              
        

        ! arguments :

        integer(int_wp ), intent(in)     :: lunrep              ! report file
        integer(int_wp ), intent(in)     :: nosegl              ! number segments per layer
        integer(int_wp ), intent(in)     :: nolay               ! number of layers in calculation
        integer(int_wp ), intent(in)     :: nmax                ! first grid dimension
        integer(int_wp ), intent(in)     :: mmax                ! second grid dimension
        integer(int_wp ), intent(in)     :: mnmaxk              ! total number of active grid cells
        integer(int_wp ), pointer        :: lgrid ( : , : )     ! grid with active grid numbers, negatives for open boundaries
        integer(int_wp ), pointer        :: lgrid2( : , : )     ! total grid
        integer(int_wp ), pointer        :: lgrid3( : , : )     ! original grid (conc array)

        integer(int_wp ), pointer        :: kpart ( : )         ! third grid index of the particles
        real   (sp), pointer        :: xpart ( : )         ! x-value (0.0-1.0) first  direction within grid cell
        real   (sp), pointer        :: ypart ( : )         ! y-value (0.0-1.0) second direction within grid cell
        real   (sp), pointer        :: zpart ( : )         ! z-value (0.0-1.0) third  direction within grid cell
        real   (sp), pointer, intent(in)   :: wpart ( : , :)      ! weight factors of the subs per particle

        real   (sp), pointer        :: wsettl( : )         ! settling per particle

        real   (sp), pointer        :: angle ( : )         ! angle with horizontal
        real   (sp), pointer        :: depth( : )          ! total depth of the cells from top to bottom
        real   (sp), pointer        :: vol1  ( : )         ! volume begin hydr step
        real   (sp), pointer        :: vol2  ( : )         ! volume end hydr step
        real   (sp), pointer        :: flow  ( : )         ! all flows
        real   (sp), pointer        :: salin1( : )         ! salinity segment numbering
        real   (sp), pointer        :: temper1( : )        ! temperature segment numbering
        real   (sp), pointer        :: v_swim( : )         ! horizontal swimming velocity m/
        real   (sp), pointer        :: d_swim( : )         ! horizontal swimming direction (degree)
        real   (sp), pointer        :: vel1  ( : )         ! velocity begin hydr step
        real   (sp), pointer        :: vel2  ( : )         ! velocity end hydr step

        ! from input (const)

        integer                     :: iseg                ! iseg

        integer , pointer           :: btype(:)            ! behaviour type
        real(sp), pointer           :: buoy1(:)            ! buoyancy begin stage
        real(sp), pointer           :: buoy2(:)            ! buoyancy end stage
        real(sp), pointer           :: vzact1(:)           ! active vertical velocity begin stage
        real(sp), pointer           :: vzact2(:)           ! active vertical velocity end stage
        real(sp), pointer           :: ztop1(:)            ! depth top layer begin stage
        real(sp), pointer           :: ztop2(:)            ! depth top layer end stage
        real(sp), pointer           :: zbot1(:)            ! depth bot layer begin stage
        real(sp), pointer           :: zbot2(:)            ! depth bot layer end stage
        integer , pointer           :: hbtype(:)           ! horizontal behaviour type
        real(sp), pointer           :: vswim1(:)           ! swim velocity at begin of stage
        real(sp), pointer           :: vswim2(:)           ! swim velocity at end of stage

        ! local :

        real(sp), pointer           :: phase_diurn(:)      ! phase in diurnal behaviour
        integer(int_wp )                 :: ipart               ! particle index
        real   (sp)                 :: fstage              ! fraction of current stage
        integer(int_wp )                 :: istage              ! integer stage development
        integer(int_wp )                 :: idelt               ! timestep in seconds
        real   (sp)                 :: day                 ! time in days
        real   (sp)                 :: a                   ! a coefficient in development (-)
        real   (sp)                 :: b                   ! b coefficient in development (-)
        real   (sp)                 :: vz                  ! vz
        real   (sp)                 :: vzact               ! vzact
        real   (sp)                 :: buoy                ! buoy
        integer                     :: k                   ! k
        real   (sp)                 :: ztop                ! ztop
        real   (sp)                 :: zbot                ! zbot
        integer                     :: m                   ! m
        integer                     :: n                   ! n
        
        real   (sp)                 :: zdepth              ! z relative to water surface
        real   (sp)                 :: zlevel              ! z relative to bottom
        logical, pointer            :: ebb_flow( : )       ! true if flow is ebb
        logical                     :: daytime             ! true if it is daytime, false in night
        
        integer                     :: behaviour_type      ! actual behaviour type

        integer, parameter          :: behaviour_none     = 0 ! behaviour type none
        integer, parameter          :: behaviour_passive  = 1  ! behaviour type passive
        integer, parameter          :: behaviour_larval   = 2  ! behaviour type larval
        integer, parameter          :: behaviour_adult    = 3  ! behaviour type adult

        

        real                        :: vswim                  ! swimming velocity
        real                        :: local_angle            ! angle towards lowest salinity in grid
        
        real                        :: lb_sal                 ! lower boundary of salinity
        real                        :: ub_sal                 ! upper boundary of salinity

        real                        :: sal_n0
        real                        :: sal_n1
        real                        :: sal_n12
        real                        :: sal_n2
        real                        :: sal_n23
        real                        :: sal_n3
        real                        :: sal_n34
        real                        :: sal_n4
        real                        :: sal_n41

        real                        :: lb_temp                ! lower boundary of temperature
        real                        :: ub_temp                ! upper boundary of temperature       

        real                        :: temp_n0
        real                        :: temp_n1
        real                        :: temp_n12
        real                        :: temp_n2
        real                        :: temp_n23
        real                        :: temp_n3
        real                        :: temp_n34
        real                        :: temp_n4
        real                        :: temp_n41
        
        real                        :: lb_bath                ! lower boundary of bathymetry
        real                        :: ub_bath                ! upper boundary of bathymetry       

        real                        :: bath_n0
        real                        :: bath_n1
        real                        :: bath_n12
        real                        :: bath_n2
        real                        :: bath_n23
        real                        :: bath_n3
        real                        :: bath_n34
        real                        :: bath_n4
        real                        :: bath_n41
        
        logical                     :: stick_to_bottom        ! stick to bottom when reached     
        logical                     :: dive_at_night          ! dive during the night, if untrue dive during the day         
        
        ! HORSESHOE CRAB BEHAVIOUR TYPES
        
        istage  = wpart(2,ipart)                                                         ! Get current stage of particle
        
        !Set layer and/or settling velocity according to stage and type of vertical behaviour

        behaviour_type = btype(istage)                                                   ! Assign the vertical behaviour type by stage
        select case ( behaviour_type )                                                   ! Select behaviour by numbering

           case ( behaviour_none )                                                       !Behaviour 0 

              !The particle shows no active behaviour in the vertical and in the horizontal

              !Horizontal behaviour                                                        
              v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
              d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0

              !Vertical behaviour
              wsettl(ipart) = 0.0                                                       ! Setteling velocity is set to 0

            case ( behaviour_passive)                                                    !Behaviour 1

             !Horizontal behaviour                                                        
             v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
             d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0
             
             !The particle shows no active behaviour in the vertical and in the horizontal, 
             ! but is subject to bouyancy and a maximum diving depth.
            
             buoy = buoy1(istage) + fstage*(buoy2(istage)-buoy1(istage))               ! Set the bouyancy for the particle
             vz = buoy                                                                 ! Set the vertical direction for the particle
             
             wsettl(ipart) = vz

           case ( behaviour_larval)                                                     !Behaviour 1

             !Horizontal behaviour                                                        
             v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
             d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0
             
             stick_to_bottom = .true.                                                   ! Hardcoded stick to bottom
             lb_bath = 0.001                                                            ! Hardcoded lower boundary for bathymetry
             ub_bath = 500.000                                                          ! Hardcoded upper boundary for bathymetry

             buoy = buoy1(istage) + fstage*(buoy2(istage)-buoy1(istage))                ! Set the bouyancy for the particle
             vzact = vzact1(istage) + fstage*(vzact2(istage)-vzact1(istage))            ! Set the vertical swimming velocity for the particle
             vswim = vswim1(istage) + fstage*(vswim2(istage)-vswim1(istage))            ! Set the horizontal swimming velocity indicator
                          
             !Standard swimming velocity of particle
             v_swim(ipart) = vswim                                                      ! Set the horizontal swimming velocity to the indicator


             !Horizontal behaviour
             
             !The particle will dig into the sediment when ebb flow occurs, and emerse when flood flow occurs 
             ! when flood the particle will actively swimm towards the shallower coast
             ! wind will effect the particle when in the top layer
             
             ! check if it is daytime or night
             call intpltd_diurnal ( lunrep, day, daytime)
             
             ! Assemble bathymertry values of surrounding gridcells
             !  return: v_swim and d_swim
             call orien_bathymetry ( lunrep    , n           , m        , nmax        , mmax        ,    &
                                     mnmaxk    , lgrid       , lgrid2   , lgrid3      , depth       ,    &
                                     v_swim    , d_swim      , angle    , ipart       , xpart       ,    &
                                     ypart     , a           , b        , flow        , local_angle ,    &
                                     lb_bath   , ub_bath     , bath_n0  , bath_n1     , bath_n12    ,    &
                                     bath_n2   , bath_n23    , bath_n3  , bath_n34    , bath_n4     ,    &
                                     bath_n41 ) 
            
            ! No swimming during outgoing tide
             call vert_swimm_tidal (   lunrep,   ebb_flow  , iseg , k, nolay, &
                                       stick_to_bottom , ipart , wsettl , kpart , zpart , &
                                       buoy , vzact , v_swim , d_swim )
                         
            ! No swimming during the day
             call vert_swimm_diurnal (   lunrep        , daytime  , k       , nolay   , stick_to_bottom ,    &
                                         dive_at_night , ipart    , wsettl  , kpart   , zpart           ,    &
                                         buoy          , vzact    , v_swim  , d_swim  )
                
              
           case ( behaviour_adult )                                                       !Behaviour 2 

              ! the adult behaviour is added for the end of simulation
              !The particle shows no active behaviour in the vertical and in the horizontal

              !Horizontal behaviour                                                        
              v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
              d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0

              !Vertical behaviour
              wsettl(ipart) = 0.0                                                       ! Setteling velocity is set to 0
             
             
           case default                                                                 !Behaviour Default

              write(lunrep,*) ' error, larval behaviour type not defined'               ! Give an error notice that vertical behaviour is not defined
              call stop_exit(1)                                                         ! Stop the calculation
               
        end select

      return                                                                                                         !Return from the subroutine
      end subroutine
end module
