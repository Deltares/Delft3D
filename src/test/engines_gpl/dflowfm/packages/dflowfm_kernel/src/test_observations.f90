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
module chdir_mod

  implicit none

  interface
    integer function c_chdir(path) bind(C,name="chdir")
      use iso_c_binding
      character(kind=c_char) :: path(*)
    end function
  end interface

contains

  subroutine chdir(path, err)
    use iso_c_binding
    character(*) :: path
    integer, optional, intent(out) :: err
    integer :: loc_err

    loc_err =  c_chdir(path//c_null_char)

    if (present(err)) err = loc_err
  end subroutine
end module chdir_mod
    
module test_observations
    use ftnunit
    use precision

    implicit none
    real(fp), parameter :: eps = 1.0e-6_fp

contains
!
!
!==============================================================================
subroutine tests_observations
    call test( test_read_obs_points, 'Tests the reading of observation points' )
    !call test( test_read_snapped_obs_points, 'Tests the reading of snapped observation points' )
end subroutine tests_observations
!
!
!==============================================================================
subroutine test_read_obs_points
    use m_observations
    use m_partitioninfo, only: jampi
    !
    ! Locals
    integer, parameter                         :: N_OBS_POINTS = 4
    integer                                    :: i
    double precision                           :: refdata(2,N_OBS_POINTS)
    character(len=40), dimension(N_OBS_POINTS) :: refnames
    !
    data refdata /0.000000000000000D+000, 0.000000000000000D+000, &
                  145964.000000         , 427274.000000         , &
                  0.000000000000000D+000, 0.000000000000000D+000, &
                  145852.00000          , 427142.000000           /
    refnames(1) = 'TestLocation1'
    refnames(2) = 'TestLocation_xy1'
    refnames(3) = 'TestLocation2'
    refnames(4) = 'TestLocation_xy2'
    !
    ! Body
    jampi = 0
    call loadObservations("observations/ObservationPoints_2.ini", 0)
    call assert_true(allocated(xobs), 'xobs is allocated')
    if (allocated(xobs)) then
       do i=1,N_OBS_POINTS
           call assert_comparable(xobs(i)  , refdata(1,i), eps, 'x-coordinate of observation points incorrect' )
           call assert_comparable(yobs(i)  , refdata(2,i), eps, 'y-coordinate of observation points incorrect' )
           call assert_equal     (namobs(i), refnames(i) , "Observation point name incorrect" )
       enddo
    end if
end subroutine test_read_obs_points
!
!
!==============================================================================
subroutine test_read_snapped_obs_points
    use gridoperations
    use m_cell_geometry, only: xz, yz
    use m_netw
    use m_observations
    use unstruc_model
    use m_partitioninfo, only: jampi
    use ifport
    use m_flow_modelinit, only: flow_modelinit
    use m_resetfullflowmodel, only: resetfullflowmodel
    !
    ! Locals
    integer, parameter                           :: N_OBS_POINTS = 4
    integer                                      :: i
    integer                                      :: istat
    logical                                      :: success_
    integer          , dimension(N_OBS_POINTS)   :: ref_k
    double precision , dimension(2,N_OBS_POINTS) :: refdata
    character(len=40), dimension(N_OBS_POINTS)   :: refnames
    character(len=40)                            :: mdufile
    !
    data ref_k / 522, 1043, 1565, 1304 /
    data refdata /100000.000000         , 25000.0000000         , &
                  0.000000000000000D+000, 50000.0000000         , &
                  0.000000000000000D+000, 100000.000000         , &
                  100000.000000         , 75000.0000000           /
    refnames(1) = 'TestLocation1'
    refnames(2) = 'TestLocation_xy1'
    refnames(3) = 'TestLocation2'
    refnames(4) = 'TestLocation_xy2'
    !
    ! Body
    jampi = 0
    kmax  = 2
    lmax  = 2
    call resetFullFlowModel()
    call increaseNetw(kmax, lmax)
    !
    success_ = CHANGEDIRQQ("observations_snapped")
    !istat = SYSTEM("echo %CD%")
    
    mdufile = 'Flow1d.mdu'
    call loadModel(mdufile)
    istat = flow_modelinit()
    
    success_ = CHANGEDIRQQ("..")
    !
    do i=1,N_OBS_POINTS
        call assert_equal     (kobs(i)    , ref_k    (i), 'index of snapped observation points incorrect' )
        call assert_comparable(xz(kobs(i)), refdata(1,i), eps, 'x-coordinate of snapped observation points incorrect' )
        call assert_comparable(yz(kobs(i)), refdata(2,i), eps, 'y-coordinate of snapped observation points incorrect' )
        call assert_equal     (namobs(i)  , refnames (i), "Snapped observation point name incorrect" )
    enddo
end subroutine test_read_snapped_obs_points

end module test_observations
