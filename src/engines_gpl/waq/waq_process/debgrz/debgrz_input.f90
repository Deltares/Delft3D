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
        integer(kind=int_wp)  :: use_with_protist   !< Switch whether to use DEB with Protist:
                                                    !< 0 => with Dynamo/Bloom, 1 => with Protist      [-]
        real(kind=real_wp) :: delt                  !< timestep for processes                         [d]
        real(kind=real_wp) :: volume                !< volume of computational cell                  [m3]
        real(kind=real_wp) :: temp                  !< ambient water temperature                     [oC]
        real(kind=real_wp) :: depth                 !< depth of segment                               [m]
        real(kind=real_wp) :: totaldepth            !< total depth water column                       [m]
        real(kind=real_wp) :: tim                   !< total inorganic matter                    [gDM/m3]
        real(kind=real_wp) :: gem                   !< option for POX (value must not be changed)     [-]
        integer(kind=int_wp) :: switchv1            !< use ISO-morphs (0) or V1-morphs (1)            [-]
        integer(kind=int_wp) :: benths              !< Switch to use benthic or pelagic for DEB species
                                                    !< 0 = do not use benthic = use pelagic;
                                                    !< 1 = use benthic                                [-]
        real(kind=real_wp) :: vtot                  !< structural biomass grazer pop.    [gC/m3 or gC/m2]
        real(kind=real_wp) :: etot                  !< energy storage grazer pop.        [gC/m3 or gC/m2]
        real(kind=real_wp) :: rtot                  !< reproductional storage grazer pop.[gC/m3 or gC/m2]
        real(kind=real_wp) :: dens_ini              !< initial number of grazer individuals ??
        real(kind=real_wp) :: length_ini            !< Initial Individual Length   ??            [gWW/m2]
        real(kind=real_wp) :: dummy                 !< dummy variable                                 [-]
        real(kind=real_wp) :: vp                    !< volume at start of reproductive stage        [cm3]
        real(kind=real_wp) :: shape                 !< shape coefficient                              [-]
        real(kind=real_wp) :: em_l3                 !< maximum storage density of DEB species     [J/cm3]
        real(kind=real_wp) :: eg_l3                 !< volume-spec costs for growth of DEB species[J/cm3]
        real(kind=real_wp) :: pm_l3                 !< respiration rate constant of DEB species     [J/d]
        real(kind=real_wp) :: jxm_l2                !< max ingestion rate of DEB species        [J/cm2/d]
        real(kind=real_wp) :: kappai                !< Ingestion efficiency (pseudofaeces production) [-]
        real(kind=real_wp) :: kappaa                !< Assimilation efficiency                        [-]
        real(kind=real_wp) :: kappa                 !< fraction of util.energy spent on maint&growth  [-]
        real(kind=real_wp) :: kappar                !< fraction of repro.energy spent on              [-]
        real(kind=real_wp) :: ta                    !< Arrhenius temperature                          [K]
        real(kind=real_wp) :: tah                   !< Arr temp for rate of decrease at upper boundary[K]
        real(kind=real_wp) :: tal                   !< Arr temp for rate of decrease at lower boundary[K]
        real(kind=real_wp) :: th                    !< Upper boundary of tolerance range              [K]
        real(kind=real_wp) :: tl                    !< Lower boundary of tolerance range              [K]
        real(kind=real_wp) :: gsi_upper             !< Minimum GSI for spawning                       [-]
        real(kind=real_wp) :: gsi_lower             !< minimum GSI while spawning                     [-]
        real(kind=real_wp) :: dospawn               !< indication of spawning                         [-]
        real(kind=real_wp) :: rspawn                !< Spawning rate                                  [-]
        real(kind=real_wp) :: minsptemp             !< Minimum temperature for spawning              [oC]
        real(kind=real_wp) :: xk                    !< Halfrate const food uptake Sup fdr         [gC/m3]
        real(kind=real_wp) :: yk                    !< Halfrate const TIM                         [gC/m3]
        real(kind=real_wp) :: rmor_ref              !< reference mortality rate grazers              [/d]
        real(kind=real_wp) :: cmor                  !< length-dep coefficient mortality rate         [/d]
        real(kind=real_wp) :: rhrv_ref              !< reference  harvesting rate grazers            [/d]
        real(kind=real_wp) :: chrv                  !< length-dep coefficient harvesting rate        [/d]
        real(kind=real_wp) :: conv_j_gc             !< conversion factor from energy into mass     [gC/J]
        real(kind=real_wp) :: conv_cm3_gc           !< conversion factor from cm3 into gC        [gC/cm3]
        real(kind=real_wp) :: conv_gafdw_gc         !< conversion factor from gAFDW into gC    [gC/gAFDW]
        real(kind=real_wp) :: conv_gww_gc           !< conversion factor from gWW into gC        [gC/gWW]
        real(kind=real_wp) :: tc                    !< C:C ratio grazers                          [gC/gC]
        real(kind=real_wp) :: tn                    !< N:C ratio grazers                          [gN/gC]
        real(kind=real_wp) :: tp                    !< P:C ratio grazers                          [gP/gC]
        real(kind=real_wp) :: tsi                   !< Si:C ratio grazers                        [gSi/gC]
        real(kind=real_wp) :: frdetbot              !< fraction of detritus into sediment or water    [-]
        real(kind=real_wp) :: suspension            !< DEB species preference for  suspension over
                                                    !< deposit feeding                                [-]
        real(kind=real_wp), allocatable :: pref(:)  !< DEB species preference for detritus            [-]
        real(kind=real_wp), allocatable :: fffood(:)!< Faecal fraction of detritus for DEB species    [-]
        real(kind=real_wp) :: minfood               !< Minimum amount of food for DEB species         [-]
        real(kind=real_wp) :: smotot                !< Shell matrix organic tissue       [gC/m3 or gC/m2]
        real(kind=real_wp) :: smitot                !< Shell matrix inorganic tissue    [gC/m3 or gDM/m2]
        real(kind=real_wp) :: egsmo                 !< Vol-spec growth costs for org shell matrix [J/cm3]
        real(kind=real_wp) :: egsmi                 !< Vol-spec growth costs for inorg shell matr [J/cm3]
        real(kind=real_wp) :: frgsmo                !< fraction of growth flux to shell matrix        [-]
        real(kind=real_wp) :: frrespsmo             !< fraction of resp flux to shell matrix          [-]
        real(kind=real_wp) :: frsmosmi              !< fraction of shell matrix flux to calcification [-]
        real(kind=real_wp) :: cso_cm3_gc            !< conversion factor org shell cm3 into gC   [gC/cm3]
        real(kind=real_wp) :: csi_cm3_gc            !< conversion factor inorg shell cm3 into gC [gC/cm3]
        real(kind=real_wp), dimension(4) :: detrit  !< Detritus
        real(kind=real_wp), dimension(4) :: pom     !< particulate organic matter
        real(kind=real_wp), dimension(4) :: dets1   !< Detritus in layer S1
        real(kind=real_wp), dimension(4) :: detbio  !< Pelagic detritus

        contains
            procedure :: initialize => initialize_debgrz_input
            procedure :: allocate_food_arrays
            procedure :: get_area

    end type debgrz_input

    contains

    subroutine initialize_debgrz_input(this, iparray, pmsa)
        !< Transfer values from generic array to process-specific input parameters.
        class(debgrz_input) :: this !< the input_variables instance
        real(kind=real_wp), intent(in)     :: pmsa(*)
        integer(kind=int_wp), intent(in)  :: iparray(*)

        integer(kind=int_wp) :: i
        integer(kind=int_wp), parameter ::ntotnut = 4   !< number of nutrients: carbon, nitrogen, phosphorus and silica

        this%use_with_protist = pmsa( iparray( 1))
        this%delt        =      pmsa( iparray( 2))
        this%volume      =      pmsa( iparray( 3))
        this%temp        =      pmsa( iparray( 4))
        this%depth       =      pmsa( iparray( 5))
        this%totaldepth  =      pmsa( iparray( 6))
        this%tim         =      pmsa( iparray( 7))
        this%gem         =      pmsa( iparray( 8))
        this%switchv1    = nint(pmsa( iparray( 9)))
        this%benths      = nint(pmsa( iparray(10)))
        this%vtot        =      pmsa( iparray(11))
        this%etot        =      pmsa( iparray(12))
        this%rtot        =      pmsa( iparray(13))
        this%dens_ini    =      pmsa( iparray(14))
        this%length_ini  =      pmsa( iparray(15))
        this%dummy       =      pmsa( iparray(16))
        this%vp          =      pmsa( iparray(17))
        this%shape       =      pmsa( iparray(18))
        this%em_l3       =      pmsa( iparray(19))
        this%eg_l3       =      pmsa( iparray(20))
        this%pm_l3       =      pmsa( iparray(21))
        this%jxm_l2      =      pmsa( iparray(22))
        this%kappai      =      pmsa( iparray(23))
        this%kappaa      =      pmsa( iparray(24))
        this%kappa       =      pmsa( iparray(25))
        this%kappar      =      pmsa( iparray(26))
        this%ta          =      pmsa( iparray(27))
        this%tah         =      pmsa( iparray(28))
        this%tal         =      pmsa( iparray(29))
        this%th          =      pmsa( iparray(30))
        this%tl          =      pmsa( iparray(31))
        this%gsi_upper   =      pmsa( iparray(32))
        this%gsi_lower   =      pmsa( iparray(33))
        this%dospawn     =      pmsa( iparray(34))
        this%rspawn      =      pmsa( iparray(35))
        this%minsptemp   =      pmsa( iparray(36))
        this%xk          =      pmsa( iparray(37))
        this%yk          =      pmsa( iparray(38))
        this%rmor_ref    =      pmsa( iparray(39))
        this%cmor        =      pmsa( iparray(40))
        this%rhrv_ref    =      pmsa( iparray(41))
        this%chrv        =      pmsa( iparray(42))
        this%conv_j_gc   =      pmsa( iparray(43))
        this%conv_cm3_gc =      pmsa( iparray(44))
        this%conv_gafdw_gc=     pmsa( iparray(45))
        this%conv_gww_gc =      pmsa( iparray(46))
        this%tc          =      pmsa( iparray(47))
        this%tn          =      pmsa( iparray(48))
        this%tp          =      pmsa( iparray(49))
        this%tsi         =      pmsa( iparray(50))
        this%frdetbot    =      pmsa( iparray(51))
        this%suspension  =      pmsa( iparray(52))
        this%pref(1)     =      pmsa( iparray(53))
        this%pref(2)     =      pmsa( iparray(54))
        this%fffood(1)   =      pmsa( iparray(55))
        this%fffood(2)   =      pmsa( iparray(56))
        this%minfood     =      pmsa( iparray(57))
        this%smotot      =      pmsa( iparray(58))
        this%smitot      =      pmsa( iparray(59))
        this%egsmo       =      pmsa( iparray(60))
        this%egsmi       =      pmsa( iparray(61))
        this%frgsmo      =      pmsa( iparray(62))
        this%frrespsmo   =      pmsa( iparray(63))
        this%frsmosmi    =      pmsa( iparray(64))
        this%cso_cm3_gc  =      pmsa( iparray(65))
        this%csi_cm3_gc  =      pmsa( iparray(66))
        do i=1,ntotnut
            this%detrit(i)   = max(0.,pmsa(iparray(66 + i              ))  )
            this%pom(i)      = max(0.,pmsa(iparray(66 + i +     ntotnut))  )
            this%dets1(i)    = max(0.,pmsa(iparray(66 + i + 2 * ntotnut))  )
            this%detbio(i)   = max(0.,this%detrit(i)*(1.0-this%gem) + this%pom(i)*this%gem)
        end do

    end subroutine initialize_debgrz_input

    subroutine allocate_food_arrays(this, food_count)
        !< Allocates the input food arrays

        class(debgrz_input) :: this !< the input_variables instance
        integer(kind=int_wp), intent(in)  :: food_count

        allocate(this%pref(food_count))
        allocate(this%fffood(food_count))
    end subroutine allocate_food_arrays

    real(kind=real_wp)function get_area(this) result(area)
        !< computes the area based on volume and depth

        class(debgrz_input) :: this !< the input_variables instance

        if (this%depth == 0) then
            area = 0
        else
            area = this%volume / this%depth
        end if

    end function get_area

end module m_debgrz_input
