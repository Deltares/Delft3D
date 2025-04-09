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

module m_debgrz_input
    use m_waq_precision

    implicit none

    private
    public :: debgrz_input

    type :: debgrz_input
        integer(kind=int_wp)  :: use_with_protist   !< Switch whether to use DEB with Dynamo/Bloom (0)
                                                    !< or with Protist (1)                            [-]
        real(kind=real_wp) :: delt                  !< Timestep for processes                         [d]
        real(kind=real_wp) :: volume                !< Volume of computational cell                  [m3]
        real(kind=real_wp) :: temp                  !< Ambient water temperature                     [oC]
        real(kind=real_wp) :: depth                 !< Depth of segment                               [m]
        real(kind=real_wp) :: totaldepth            !< Total depth water column                       [m]
        real(kind=real_wp) :: tim                   !< Total inorganic matter                    [gDM/m3]
        real(kind=real_wp) :: gem                   !< Option for POX (value must not be changed)     [-]
        integer(kind=int_wp) :: switchv1            !< Use ISO-morphs (0) or V1-morphs (1)            [-]
        integer(kind=int_wp) :: benths              !< Switch to use benthic or pelagic for DEB species
                                                    !< 0 = do not use benthic = use pelagic;
                                                    !< 1 = use benthic                                [-]
        real(kind=real_wp) :: vtot                  !< Structural biomass grazer pop.    [gC/m3 or gC/m2]
        real(kind=real_wp) :: etot                  !< Energy storage grazer pop.        [gC/m3 or gC/m2]
        real(kind=real_wp) :: rtot                  !< Reproductional storage grazer pop.[gC/m3 or gC/m2]
        real(kind=real_wp) :: dens_ini              !< Initial number of grazer individuals           [-]
        real(kind=real_wp) :: length_ini            !< Initial Individual Length                 [gWW/m2]
        real(kind=real_wp) :: dummy                 !< Dummy variable                                 [-]
        real(kind=real_wp) :: vp                    !< Volume at start of reproductive stage        [cm3]
        real(kind=real_wp) :: shape                 !< Shape coefficient                              [-]
        real(kind=real_wp) :: em_l3                 !< Maximum storage density of DEB species     [J/cm3]
        real(kind=real_wp) :: eg_l3                 !< Volume-spec costs for growth of DEB species[J/cm3]
        real(kind=real_wp) :: pm_l3                 !< Respiration rate constant of DEB species     [J/d]
        real(kind=real_wp) :: jxm_l2                !< Max ingestion rate of DEB species        [J/cm2/d]
        real(kind=real_wp) :: kappai                !< Ingestion efficiency (pseudofaeces production) [-]
        real(kind=real_wp) :: kappaa                !< Assimilation efficiency                        [-]
        real(kind=real_wp) :: kappa                 !< Fraction of util.energy spent on maint&growth  [-]
        real(kind=real_wp) :: kappar                !< Fraction of repro.energy spent on              [-]
        real(kind=real_wp) :: ta                    !< Arrhenius temperature                          [K]
        real(kind=real_wp) :: tah                   !< Arr temp for rate of decrease at upper boundary[K]
        real(kind=real_wp) :: tal                   !< Arr temp for rate of decrease at lower boundary[K]
        real(kind=real_wp) :: th                    !< Upper boundary of tolerance range              [K]
        real(kind=real_wp) :: tl                    !< Lower boundary of tolerance range              [K]
        real(kind=real_wp) :: gsi_upper             !< Minimum GSI for spawning                       [-]
        real(kind=real_wp) :: gsi_lower             !< Minimum GSI while spawning                     [-]
        real(kind=real_wp) :: dospawn               !< Indication of spawning                         [-]
        real(kind=real_wp) :: rspawn                !< Spawning rate                                  [-]
        real(kind=real_wp) :: minsptemp             !< Minimum temperature for spawning              [oC]
        real(kind=real_wp) :: xk                    !< Halfrate const food uptake Sup fdr         [gC/m3]
        real(kind=real_wp) :: yk                    !< Halfrate const TIM                         [gC/m3]
        real(kind=real_wp) :: rmor_ref              !< Reference mortality rate grazers              [/d]
        real(kind=real_wp) :: cmor                  !< Length-dep coefficient mortality rate         [/d]
        real(kind=real_wp) :: ddmfk                 !< Halfrate concentration for density dependent 
                                                    !< mortality factor                  [gC/m3 or gC/m2]
        real(kind=real_wp) :: rhrv_ref              !< Reference  harvesting rate grazers            [/d]
        real(kind=real_wp) :: chrv                  !< Length-dep coefficient harvesting rate        [/d]
        real(kind=real_wp) :: conv_j_gc             !< Conversion factor from energy into mass     [gC/J]
        real(kind=real_wp) :: conv_cm3_gc           !< Conversion factor from cm3 into gC        [gC/cm3]
        real(kind=real_wp) :: conv_gafdw_gc         !< Conversion factor from gAFDW into gC    [gC/gAFDW]
        real(kind=real_wp) :: conv_gww_gc           !< Conversion factor from gWW into gC        [gC/gWW]
        real(kind=real_wp) :: tc                    !< C:C ratio grazers                          [gC/gC]
        real(kind=real_wp) :: tn                    !< N:C ratio grazers                          [gN/gC]
        real(kind=real_wp) :: tp                    !< P:C ratio grazers                          [gP/gC]
        real(kind=real_wp) :: tsi                   !< Si:C ratio grazers                        [gSi/gC]
        real(kind=real_wp) :: frdetbot              !< Fraction of detritus into sediment or water    [-]
        real(kind=real_wp) :: suspension            !< DEB species preference for suspension over
                                                    !< deposit feeding                                [-]
        real(kind=real_wp), allocatable :: pref(:)  !< DEB species preference for detritus            [-]
        real(kind=real_wp), allocatable :: fffood(:)!< Faecal fraction of detritus for DEB species    [-]
        real(kind=real_wp) :: minfood               !< Minimum amount of food for DEB species         [-]
        real(kind=real_wp) :: smotot                !< Shell matrix organic tissue       [gC/m3 or gC/m2]
        real(kind=real_wp) :: smitot                !< Shell matrix inorganic tissue    [gC/m3 or gDM/m2]
        real(kind=real_wp) :: egsmo                 !< Vol-spec growth costs for org shell matrix [J/cm3]
        real(kind=real_wp) :: egsmi                 !< Vol-spec growth costs for inorg shell matr [J/cm3]
        real(kind=real_wp) :: fpgrosmo              !< Growth-based contribution to shell matrix      [-]
        real(kind=real_wp) :: fpdissmo              !< Dissipation-based contribution to shell matrix [-]
        real(kind=real_wp) :: ycacosmo              !< Yield coefficient CaCO3 deposition on matrix   [-]
        real(kind=real_wp) :: cso_cm3_gc            !< Conversion factor org shell cm3 into gC   [gC/cm3]
        real(kind=real_wp) :: csi_cm3_gc            !< Conversion factor inorg shell cm3 into gC [gC/cm3]
        real(kind=real_wp), dimension(4) :: detrit  !< Detritus
        real(kind=real_wp), dimension(4) :: pom     !< Particulate organic matter
        real(kind=real_wp), dimension(4) :: dets1   !< Detritus in layer S1
        real(kind=real_wp), dimension(4) :: detbio  !< Pelagic detritus

        contains
            procedure :: initialize => initialize_debgrz_input
            procedure :: allocate_food_arrays
            procedure :: get_area

    end type debgrz_input

    contains

    !> Transfer values from generic array to process-specific input parameters.
    subroutine initialize_debgrz_input(this, iparray, process_space_real)
        class(debgrz_input) :: this !< The input_variables instance
        real(kind=real_wp), intent(in)     :: process_space_real(*)
        integer(kind=int_wp), intent(in)  :: iparray(*)

        integer(kind=int_wp) :: i
        integer(kind=int_wp), parameter ::ntotnut = 4   !< Number of nutrients: carbon, nitrogen, phosphorus and silica

        this%use_with_protist = process_space_real( iparray( 1))
        this%delt        =      process_space_real( iparray( 2))
        this%volume      =      process_space_real( iparray( 3))
        this%temp        =      process_space_real( iparray( 4))
        this%depth       =      process_space_real( iparray( 5))
        this%totaldepth  =      process_space_real( iparray( 6))
        this%tim         =      process_space_real( iparray( 7))
        this%gem         =      process_space_real( iparray( 8))
        this%switchv1    = nint(process_space_real( iparray( 9)))
        this%benths      = nint(process_space_real( iparray(10)))
        this%vtot        =      process_space_real( iparray(11))
        this%etot        =      process_space_real( iparray(12))
        this%rtot        =      process_space_real( iparray(13))
        this%dens_ini    =      process_space_real( iparray(14))
        this%length_ini  =      process_space_real( iparray(15))
        this%ddmfk       =      process_space_real( iparray(16))
        this%vp          =      process_space_real( iparray(17))
        this%shape       =      process_space_real( iparray(18))
        this%em_l3       =      process_space_real( iparray(19))
        this%eg_l3       =      process_space_real( iparray(20))
        this%pm_l3       =      process_space_real( iparray(21))
        this%jxm_l2      =      process_space_real( iparray(22))
        this%kappai      =      process_space_real( iparray(23))
        this%kappaa      =      process_space_real( iparray(24))
        this%kappa       =      process_space_real( iparray(25))
        this%kappar      =      process_space_real( iparray(26))
        this%ta          =      process_space_real( iparray(27))
        this%tah         =      process_space_real( iparray(28))
        this%tal         =      process_space_real( iparray(29))
        this%th          =      process_space_real( iparray(30))
        this%tl          =      process_space_real( iparray(31))
        this%gsi_upper   =      process_space_real( iparray(32))
        this%gsi_lower   =      process_space_real( iparray(33))
        this%dospawn     =      process_space_real( iparray(34))
        this%rspawn      =      process_space_real( iparray(35))
        this%minsptemp   =      process_space_real( iparray(36))
        this%xk          =      process_space_real( iparray(37))
        this%yk          =      process_space_real( iparray(38))
        this%rmor_ref    =      process_space_real( iparray(39))
        this%cmor        =      process_space_real( iparray(40))
        this%rhrv_ref    =      process_space_real( iparray(41))
        this%chrv        =      process_space_real( iparray(42))
        this%conv_j_gc   =      process_space_real( iparray(43))
        this%conv_cm3_gc =      process_space_real( iparray(44))
        this%conv_gafdw_gc=     process_space_real( iparray(45))
        this%conv_gww_gc =      process_space_real( iparray(46))
        this%tc          =      process_space_real( iparray(47))
        this%tn          =      process_space_real( iparray(48))
        this%tp          =      process_space_real( iparray(49))
        this%tsi         =      process_space_real( iparray(50))
        this%frdetbot    =      process_space_real( iparray(51))
        this%suspension  =      process_space_real( iparray(52))
        this%pref(1)     =      process_space_real( iparray(53))
        this%pref(2)     =      process_space_real( iparray(54))
        this%fffood(1)   =      process_space_real( iparray(55))
        this%fffood(2)   =      process_space_real( iparray(56))
        this%minfood     =      process_space_real( iparray(57))
        this%smotot      =      process_space_real( iparray(58))
        this%smitot      =      process_space_real( iparray(59))
        this%egsmo       =      process_space_real( iparray(60))
        this%egsmi       =      process_space_real( iparray(61))
        this%fpgrosmo    =      process_space_real( iparray(62))
        this%fpdissmo    =      process_space_real( iparray(63))
        this%ycacosmo    =      process_space_real( iparray(64))
        this%cso_cm3_gc  =      process_space_real( iparray(65))
        this%csi_cm3_gc  =      process_space_real( iparray(66))
        do i=1,ntotnut
            this%detrit(i)   = max(0.,process_space_real(iparray(66 + i              ))  )
            this%pom(i)      = max(0.,process_space_real(iparray(66 + i +     ntotnut))  )
            this%dets1(i)    = max(0.,process_space_real(iparray(66 + i + 2 * ntotnut))  )
            this%detbio(i)   = max(0.,this%detrit(i)*(1.0-this%gem) + this%pom(i)*this%gem)
        end do
    end subroutine initialize_debgrz_input

    !> Allocates the input food arrays
    subroutine allocate_food_arrays(this, food_count)

        class(debgrz_input) :: this !< The input_variables instance
        integer(kind=int_wp), intent(in)  :: food_count

        allocate(this%pref(food_count))
        allocate(this%fffood(food_count))
    end subroutine allocate_food_arrays

    !> Computes the area based on volume and depth
    real(kind=real_wp)function get_area(this) result(area)

        class(debgrz_input) :: this !< The input_variables instance

        if (this%depth == 0) then
            area = 0
        else
            area = this%volume / this%depth
        end if
    end function get_area

end module m_debgrz_input
