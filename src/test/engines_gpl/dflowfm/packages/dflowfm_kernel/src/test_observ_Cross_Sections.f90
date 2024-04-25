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
module test_obserCrossSections
    use ftnunit
    use precision

    implicit none
    real(fp), parameter :: eps = 1.0e-6_fp

contains
!
!
!==============================================================================
subroutine tests_observCrossSections
    !call test( test_read_snapped_observ_crs, 'Tests the reading and snapping of observation cross sections' )
end subroutine tests_observCrossSections
!
!
!==============================================================================
subroutine test_read_snapped_observ_crs
    use gridoperations
    use m_flowgeom, only: xu, yu
    use m_netw
    use m_monitoring_crosssections
    use unstruc_model
    use m_partitioninfo, only: jampi
    use network_data, only: numk
    use ifport
    !
    ! Externals
    integer, external :: flow_modelinit
    !
    ! Locals
    integer, parameter                           :: N_Observ_Crs = 3
    integer                                      :: i
    integer                                      :: istat
    double precision                             :: refcoord1(2)
    double precision                             :: refcoord2(2,3)
    double precision                             :: refcoord3(2)
    integer                                      :: ref_lnx(N_Observ_Crs)
    integer                                      :: ref_L1, ref_L3
    integer                                      :: ref_L2(2)
    double precision                             :: refdata1(2)
    double precision                             :: refdata2(2,2)
    double precision                             :: refdata3(2)
    character(len=40), dimension(N_Observ_Crs)   :: refnames
    character(len=40)                            :: mdufile
    
   
    ! reference: coordinates of the original location of the three observation cross sections
    data refcoord1 /1.458360454017425d5, 4.271440320425944d5/
    data refcoord2 /145883.000000, 427375.000000, &
                   145897.000000, 427379.000000, &
                   145939.000000, 427382.000000  /
    data refcoord3 /1.458105097560976d5, 4.271732395492436d5/
    ! reference: number of snapped flowlinks for each observation cross section
    data ref_lnx / 1, 2, 1/
    ! reference: Index of snapped flowlinks for each observation cross section
    data ref_L1 /3/
    data ref_L2 / 29, 38/
    data ref_L3 /11/
    ! reference: coordinates of the velocity points of the snapped flowlinks for each observation cross section
    data refdata1 /145839.15000, 427141.25000/
    data refdata2 /145888.15000, 427376.85000, &
                   145928.70000, 427385.80000   /
    data refdata3 /145802.80000, 427187.00000/
    
    refnames(1) = 'ObservCross1'
    refnames(2) = 'ObservCross_xy2'
    refnames(3) = 'ObservCross2'
    !
    ! Body
    jampi = 0
    kmax  = 2
    lmax  = 2
    numk  = 0
    !call inidat()
    call increaseNetw(kmax, lmax)

    call resetFullFlowModel()
    
    
    !
    istat = CHANGEDIRQQ("observCrossSections_snapped")
    mdufile = 'FlowFM.mdu'
    call loadModel(mdufile)
    istat = flow_modelinit()
    istat = CHANGEDIRQQ("..")
    !
    ! first observCrs
    call assert_equal     (crs(1)%name, refnames(1), 'Name of the first observation cross section incorrect' )
    call assert_comparable(crs(1)%path%xp(1), refcoord1(1), eps, 'x-coordinate of the first observation cross section incorrect' )
    call assert_comparable(crs(1)%path%yp(1), refcoord1(2), eps, 'y-coordinate of the first observation cross section incorrect' )
    call assert_equal     (crs(1)%path%lnx, ref_lnx(1), 'Number of snapped flowlinks of the first observation cross section incorrect' )
    call assert_equal     (crs(1)%path%ln(1),  ref_L1,  'Index of snapped flowlink of the first observation cross section incorrect' )
    call assert_comparable(xu(crs(1)%path%ln(1)), refdata1(1), eps, 'x-coordinate of snapped flowlink of the first observation cross section incorrect' )
    call assert_comparable(yu(crs(1)%path%ln(1)), refdata1(2), eps, 'y-coordinate of snapped flowlink of the first observation cross section incorrect' )
    
    ! second observCrs
    call assert_equal(crs(2)%name, refnames(2) ,   'Name of the second observation cross section incorrect' )
    do i=1,3
        call assert_comparable(crs(2)%path%xp(i), refcoord2(1,i), eps, 'x-coordinate of the second observation cross section incorrect' )
        call assert_comparable(crs(2)%path%yp(i), refcoord2(2,i), eps, 'y-coordinate of the second observation cross section incorrect' )
    enddo
    call assert_equal(crs(2)%path%lnx, ref_lnx(2), 'Number of snapped flowlinks of the second observation cross section incorrect' )
    do i=1, 2
       call assert_equal     (crs(2)%path%ln(i), ref_L2(i), 'Index of snapped flowlink of the second observation cross section incorrect' )
       call assert_comparable(xu(crs(2)%path%ln(i)), refdata2(1,i), eps, 'x-coordinate of snapped flowlink of the second observation cross section incorrect' )
       call assert_comparable(yu(crs(2)%path%ln(i)), refdata2(2,i), eps, 'y-coordinate of snapped flowlink of the second observation cross section incorrect' )
    end do
    ! third observCrs
    call assert_equal     (crs(3)%name, refnames(3) , 'Name of the third observation cross section incorrect' )
    call assert_comparable(crs(3)%path%xp(1), refcoord3(1), eps, 'x-coordinate of the third observation cross section incorrect' )
    call assert_comparable(crs(3)%path%yp(1), refcoord3(2), eps, 'y-coordinate of the third observation cross section incorrect' )
    call assert_equal     (crs(3)%path%lnx, ref_lnx(3), 'Number of snapped flowlinks of the third observation cross section incorrect' )
    call assert_equal     (crs(3)%path%ln(1),  ref_L3,  'Index of snapped flowlink of the third observation cross section incorrect' )
    call assert_comparable(xu(crs(3)%path%ln(1)), refdata3(1), eps, 'x-coordinate of snapped flowlink of the third observation cross section incorrect' )
    call assert_comparable(yu(crs(3)%path%ln(1)), refdata3(2), eps, 'y-coordinate of snapped flowlink of the third observation cross section incorrect' )
    
end subroutine test_read_snapped_observ_crs

end module test_obserCrossSections
