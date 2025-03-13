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

program test_dflowfm_kernel
    use ftnunit
    use test_observations
    use test_cross_sections
    use test_roughness
    use test_obserCrossSections
    use test_ini_Field_1dField
    use test_MDU_File_Version
    use test_storage_nodes
    use test_1d_grid
    use test_read_property
    use test_airdensity
    use test_lateral
    use test_temporal_statistics
    use test_read_statistical_output
    use test_statistical_output
    
    implicit none
    
    call prepareTests()
    call runtests_init()

    call tests_lateral()
    call tests_roughness()
    call tests_cross_sections()
    call tests_observations()
    call tests_observCrossSections()
    call tests_MDU_fileversion()
    call tests_1d_grid()
    call tests_storageNodes()
    call tests_iniField_1dField()
    call tests_read_property()
    call tests_compute_airdensity()
    call tests_temporal_statistics()
    call tests_read_statistical_output()
    call tests_statistical_output()
    !
    ! Done - properly finalize
    !
    call runtests_final()
    call showResult()

contains

!> Routine to start the testing
!! Note: This routine merely takes care that the unit tests are indeed run
subroutine prepareTests()

    integer  :: lun   !< LU-number

    open( newunit=lun, file = 'ftnunit.run' )
    write( lun, '(a)' ) 'ALL'
    close( lun )

end subroutine prepareTests

!> Start the browser to show the result
subroutine showResult()
    !character(len=1) :: answer
    !
    !write(*,*)     'Press ENTER ...'
    !read(*,'(a)' ) answer

    call system( 'ftnunit.html' )

end subroutine showResult


end program test_dflowfm_kernel
