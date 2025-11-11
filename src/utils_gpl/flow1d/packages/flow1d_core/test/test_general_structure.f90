module test_general_structure
   use assertions_gtest
   use m_General_Structure
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_general_structure, test_genstru_1d2d_door_not_in_water, test_genstru_1d2d_door_not_in_water,
   subroutine test_genstru_1d2d_door_not_in_water() bind(C)
   
      use precision, only: dp

      real(kind=dp) :: q_fully_opened_no_door, q_fully_opened_door_100_percent, q_fully_opened_door_50_percent
      real(kind=dp), parameter :: tolerance = 1e-6_dp

      type(t_GeneralStructure), pointer :: general_structure

      allocate(general_structure)
      
      general_structure = create_general_structure()
      
      ! General structure fully opened and door 100 percent opened
      general_structure%gateclosedfractiononlink(1) = 0.0_dp
      general_structure%gateLowerEdgeLevel = 50.0_dp
      general_structure%gateopeningwidth = general_structure%ws
      q_fully_opened_no_door = compute(general_structure, computeGeneralStructure)

      ! General structure fully opened and door 100 percent closed
      general_structure%gateclosedfractiononlink(1) = 1.0_dp
      general_structure%gateLowerEdgeLevel = 50.0_dp      
      general_structure%gateopeningwidth = 0.0_dp
      q_fully_opened_door_100_percent = compute(general_structure, computeGeneralStructure)
      
      ! General structure fully opened and door 50 percent closed
      general_structure%gateclosedfractiononlink(1) = 0.5_dp
      general_structure%gateLowerEdgeLevel = 50.0_dp      
      general_structure%gateopeningwidth = 50_dp
      q_fully_opened_door_50_percent = compute(general_structure, computeGeneralStructure)

      call f90_expect_near(q_fully_opened_no_door, q_fully_opened_door_100_percent, tolerance, "The discharge for no door is different from the discharge for a door hanging over the structure.")
      call f90_expect_near(235.92603462936978_dp, q_fully_opened_door_50_percent, tolerance, "The discharge for no door is different from the discharge for a door partly hanging over the structure.")
      
   end subroutine test_genstru_1d2d_door_not_in_water
   !$f90tw)
   
   !$f90tw TESTCODE(TEST, test_general_structure, test_genstru_2d3d_door_not_in_water, test_genstru_2d3d_door_not_in_water,
   subroutine test_genstru_2d3d_door_not_in_water() bind(C)
   
      use precision, only: dp

      real(kind=dp) :: q_fully_opened_no_door, q_fully_opened_door_100_percent, q_fully_opened_door_50_percent
      real(kind=dp) :: q_w_50m_no_door, q_w100_door_opening_50m
      real(kind=dp), parameter :: tolerance = 1e-6_dp

      type(t_GeneralStructure), pointer :: general_structure

      allocate(general_structure)
      
      general_structure = create_general_structure()
      
      ! General structure fully opened and door 100 percent opened
      general_structure%gateclosedfractiononlink(1) = 0.0_dp
      general_structure%gateLowerEdgeLevel = 50.0_dp
      general_structure%gateopeningwidth = general_structure%ws
      q_fully_opened_no_door = compute(general_structure, computeGeneralStructure_fullwidth)

      ! General structure fully opened and door 100 percent closed
      general_structure%gateclosedfractiononlink(1) = 1.0_dp
      general_structure%gateLowerEdgeLevel = 50.0_dp      
      general_structure%gateopeningwidth = 0.0_dp
      q_fully_opened_door_100_percent = compute(general_structure, computeGeneralStructure_fullwidth)

      ! General structure fully opened and door 50 percent closed
      general_structure%gateclosedfractiononlink(1) = 0.5_dp
      general_structure%gateLowerEdgeLevel = 50.0_dp      
      general_structure%gateopeningwidth = 50_dp
      q_fully_opened_door_50_percent = compute(general_structure, computeGeneralStructure_fullwidth)

      ! General structure width 50 m no door
      general_structure%gateclosedfractiononlink(1) = 0.0_dp
      general_structure%gateLowerEdgeLevel = 50.0_dp
      general_structure%ws = 50.0_dp
      general_structure%gateopeningwidth = general_structure%ws
      q_w_50m_no_door = compute(general_structure, computeGeneralStructure_fullwidth)

      ! General structure width 100 percent door opening 50 m gate lower edge level == crest level
      general_structure%gateclosedfractiononlink(1) = 0.5_dp
      general_structure%gateLowerEdgeLevel = general_structure%zs
      general_structure%ws = 100.0_dp
      general_structure%gateopeningwidth = 50.0_dp

      q_w100_door_opening_50m = compute(general_structure, computeGeneralStructure_fullwidth)

      call f90_expect_near(q_fully_opened_no_door, q_fully_opened_door_100_percent, tolerance, "The discharge for no door is different from the discharge for a door hanging over the structure.")
      call f90_expect_near(q_fully_opened_no_door, q_fully_opened_door_50_percent, tolerance, "The discharge for no door is different from the discharge for a door partly hanging over the structure.")
      
   end subroutine test_genstru_2d3d_door_not_in_water
   !$f90tw)
   
   !$f90tw TESTCODE(TEST, test_general_structure, test_genstru_1d2d_crestwidth_reduced, test_genstru_1d2d_crestwidth_reduced,
   subroutine test_genstru_1d2d_crestwidth_reduced() bind(C)
   
      use precision, only: dp

      real(kind=dp) :: q_fully_opened_no_door, q_fully_opened_door_100_percent, q_fully_opened_door_50_percent
      real(kind=dp) :: q_w_50m_no_door, q_w100_door_opening_50m
      real(kind=dp), parameter :: tolerance = 1e-6_dp

      type(t_GeneralStructure), pointer :: general_structure

      allocate(general_structure)
      
      general_structure = create_general_structure()
      
      ! General structure width 50 m no door
      general_structure%gateclosedfractiononlink(1) = 0.0_dp
      general_structure%gateLowerEdgeLevel = 50.0_dp
      general_structure%ws = 50.0_dp
      general_structure%gateopeningwidth = general_structure%ws
      q_w_50m_no_door = compute(general_structure, computeGeneralStructure)

      ! General structure width 100 percent door opening 50 m gate lower edge level == crest level
      general_structure%gateclosedfractiononlink(1) = 0.5_dp
      general_structure%gateLowerEdgeLevel = general_structure%zs
      general_structure%ws = 100.0_dp
      general_structure%gateopeningwidth = 50.0_dp
      q_w100_door_opening_50m = compute(general_structure, computeGeneralStructure)

      call f90_expect_near(q_w_50m_no_door, q_w100_door_opening_50m, tolerance, "The discharge for no door is different from the discharge for a door obstructing a part of the structure.")
      
      
   end subroutine test_genstru_1d2d_crestwidth_reduced
   !$f90tw)
   
   !$f90tw TESTCODE(TEST, test_general_structure, test_genstru_2d3d_crestwidth_reduced, test_genstru_2d3d_crestwidth_reduced,
   subroutine test_genstru_2d3d_crestwidth_reduced() bind(C)
   
      use precision, only: dp

      real(kind=dp) :: q_fully_opened_no_door, q_fully_opened_door_100_percent, q_fully_opened_door_50_percent
      real(kind=dp) :: q_w_50m_no_door, q_w100_door_opening_50m
      real(kind=dp), parameter :: tolerance = 1e-6_dp

      type(t_GeneralStructure), pointer :: general_structure

      allocate(general_structure)
      
      general_structure = create_general_structure()
      
      ! General structure width 50 m no door
      general_structure%gateclosedfractiononlink(1) = 0.0_dp
      general_structure%gateLowerEdgeLevel = 50.0_dp
      general_structure%ws = 50.0_dp
      general_structure%gateopeningwidth = general_structure%ws
      q_w_50m_no_door = compute(general_structure, computeGeneralStructure_fullwidth)

      ! General structure width 100 percent door opening 50 m gate lower edge level == crest level
      general_structure%gateclosedfractiononlink(1) = 0.5_dp
      general_structure%gateLowerEdgeLevel = general_structure%zs
      general_structure%ws = 100.0_dp
      general_structure%gateopeningwidth = 50.0_dp

      q_w100_door_opening_50m = compute(general_structure, computeGeneralStructure_fullwidth)

      ! These results should be identical.
      call f90_expect_near(q_w_50m_no_door, 110.40816884379659_dp, tolerance, "The discharge for no door is different from the discharge for a door obstructing a part of the structure.")
      call f90_expect_near(q_w100_door_opening_50m, 98.570049428549851_dp, tolerance, "The discharge for no door is different from the discharge for a door obstructing a part of the structure.")
   end subroutine test_genstru_2d3d_crestwidth_reduced
   !$f90tw)

   function compute(general_structure, compute_general_structure) result(q)
   use general_structure_interface, only : computegs
      use precision, only: dp
      type(t_GeneralStructure), pointer, intent(inout) :: general_structure
      procedure(computegs), pointer, intent(in) :: compute_general_structure
      
      real(kind=dp) :: fu
      real(kind=dp) :: ru
      real(kind=dp) :: s1_up
      real(kind=dp) :: s1_down
      real(kind=dp) :: maxwidth
      real(kind=dp) :: qtotal
      real(kind=dp) :: Cz
      real(kind=dp) :: dxL
      real(kind=dp) :: dt
      real(kind=dp) :: flow_area_up
      real(kind=dp) :: flow_area_down
      real(kind=dp) :: u
      real(kind=dp) :: q, q_prev, diff
      real(kind=dp) :: structwidth
      real(kind=dp) :: flow_area
      real(kind=dp) :: bob0(2)
      logical :: SkipDimensionChecks

      fu = 0.0_dp
      ru = 0.0_dp
      s1_up = 1.2_dp
      s1_down = 1.0_dp
      maxwidth = 200.0_dp
      qtotal = 0.0_dp
      Cz = 50.0_dp
      dxL = 50.0_dp
      dt = 1.0_dp
      bob0 = [-1.0_dp,-1.0_dp]
      SkipDimensionChecks = .true.
      

      flow_area_up = (s1_up-bob0(1)*maxwidth)
      flow_area_down = (s1_down-bob0(2)*maxwidth)
      flow_area = 0.0_dp
      q = 0.0_dp
      q_prev = 1.0_dp
      diff = 1.0_dp
      general_structure%fu = 0.0_dp
      general_structure%ru = 0.0_dp
      general_structure%au = 0.0_dp
      general_structure%widthcenteronlink(1) = general_structure%ws
      general_structure%ws_actual = general_structure%ws
      general_structure%zs_actual = general_structure%zs
      
      ! General structure fully opened and no door
      do while ( diff > 0.0001)
         call compute_general_structure(genstr=general_structure,&
                                      direction = 1, &
                                      L0 = 1, &
                                      maxWidth = maxwidth, &
                                      bob0 = [-10.0_dp,-10.0_dp], &
                                      fuL = fu, &
                                      ruL = ru, &
                                      auL = flow_area, &
                                      as1 = flow_area_up, &
                                      as2 = flow_area_down, &
                                      structwidth = structwidth, &
                                      s1m1 = s1_up, &
                                      s1m2 = s1_down, &
                                      qtotal = q, &
                                      Cz = cz, &
                                      dxL = dxl, &
                                      dt = dt, &
                                      SkipDimensionChecks = SkipDimensionChecks)
         u = ru - fu * (s1_down - s1_up)
         q_prev = q
         q = u * flow_area
         diff = ABS( (q - q_prev)/MAX(q, q_prev, 1.0_dp))
      end do
   end function compute
   
   function create_general_structure() result(general_structure)
      use precision, only: dp
      type(t_GeneralStructure), pointer :: general_structure
      allocate(general_structure)
      ! Initialize fields as needed
      general_structure%zu1 = 0.0_dp
      general_structure%zu2 = 0.0_dp
      general_structure%zs = 0.0_dp
      general_structure%zd1 = 0.0_dp
      general_structure%zd2 = 0.0_dp
      ! general_structure%ws_actual = 
      ! general_structure%zs_actual = 
      general_structure%wu1 = 100.0_dp
      general_structure%wu2 = 100.0_dp
      general_structure%ws = 100.0_dp
      general_structure%wd1 = 100.0_dp
      general_structure%wd2 = 100.0_dp
      general_structure%gateLowerEdgeLevel = 100.0_dp
      ! general_structure%gateLowerEdgeLevel_actual = 
      general_structure%cgf_pos = 1.0_dp
      general_structure%cgd_pos = 1.0_dp
      general_structure%cwf_pos = 1.0_dp
      general_structure%cwd_pos = 1.0_dp
      general_structure%mugf_pos = 1.0_dp
      general_structure%cgf_neg = 1.0_dp
      general_structure%cgd_neg = 1.0_dp
      general_structure%cwf_neg = 1.0_dp
      general_structure%cwd_neg = 1.0_dp
      general_structure%mugf_neg = 1.0_dp
      general_structure%extraresistance = 0.0_dp
      general_structure%gatedoorheight = 500.0_dp
      general_structure%gateopeningwidth = 50.0_dp
      ! general_structure%gateopeningwidth_actual = 
      general_structure%crestlength = 10.0_dp
      allocate(general_structure%widthcenteronlink(1))
      allocate(general_structure%gateclosedfractiononlink(1))
      general_structure%gateclosedfractiononlink(1) = 1.0_dp
      allocate(general_structure%fu(3,1))
      allocate(general_structure%ru(3,1))
      allocate(general_structure%au(3,1))
      general_structure%numlinks = 1
      general_structure%velheight = .true.
      general_structure%openingDirection = GEN_SYMMETRIC
      allocate(general_structure%sOnCrest(1))
      allocate(general_structure%state(3,1))
      general_structure%allowedflowdir = 0
      general_structure%uselimitFlowNeg = .false.
      general_structure%uselimitFlowPos = .false.
      general_structure%limitFlowNeg = huge(1.0_dp)
      general_structure%limitFlowPos = huge(1.0_dp)

   end function create_general_structure
end module test_general_structure
