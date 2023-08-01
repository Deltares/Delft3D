!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
!                                                                               
!-------------------------------------------------------------------------------
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------
!
module m_waq_bmi_data
    use iso_c_binding
    use precision
    use properties
    use MessageHandling
    !
    implicit none
    !
    ! parameters
    !
    integer, parameter :: MAXDIMS              = 6             !< Maximum number of dims as used in BMI communication
    integer, parameter :: MAXSTRLEN            = 1024          !< Maximum string length
    integer, parameter :: UNIQUE_ID_LEN        = 6             !< Length of UniqueID
    
    character(20), parameter :: KERNELNAME     = "WAQ_BMI"    !< Name of this kernel
    !
    ! integers
    !
    integer                                        :: fm_ndx             !< From D-Flow FM: nr of flow nodes (internal + boundary)
    integer                                        :: fm_ndkx            !< From D-Flow FM: dim of 3d flow nodes (internal + boundary)
    integer                                        :: fm_numconst        !< From D-Flow FM: Total number of constituents
    integer                                        :: fm_namlen          !< From D-Flow FM: Max length of (character type) parameter names
    integer                             , pointer  :: fm_isalt           !< From D-Flow FM: index of salt        in constituents array
    integer                             , pointer  :: fm_itemp           !< From D-Flow FM: index of temperature in constituents array
    !
    ! integer arrays
    !
    !
    ! Real arrays
    !
    real(hp), dimension(:)              , pointer  :: fm_xzw             !< X coordinates                            of waterlevel points as obtained from D-Flow FM
    real(hp), dimension(:)              , pointer  :: fm_yzw             !< Y coordinates                            of waterlevel points as obtained from D-Flow FM
    real(hp), dimension(:)              , pointer  :: fm_water_depth     !< Water depth                              at waterlevel points as obtained from D-Flow FM
    integer , dimension(:)              , pointer  :: fm_kbot            !< bottom layer cell number                                      as obtained from D-Flow FM
    integer , dimension(:)              , pointer  :: fm_ktop            !< top    layer cell number                                      as obtained from D-Flow FM
    real(hp), dimension(:)              , pointer  :: fm_velocity_x      !< X component of velocity                  at waterlevel points as obtained from D-Flow FM
    real(hp), dimension(:)              , pointer  :: fm_velocity_y      !< Y component of velocity                  at waterlevel points as obtained from D-Flow FM
    real(hp), dimension(:)              , pointer  :: fm_rho             !< Density                                  at waterlevel points as obtained from D-Flow FM
    real(hp), dimension(:)              , pointer  :: fm_z_level         !< z levels  (m) of interfaces (w-points)   at waterlevel points as obtained from D-Flow FM
    real(hp), dimension(:,:)            , pointer  :: fm_constituents    !< Constituents (salt, temp, sed, tracer)   at waterlevel points as obtained from D-Flow FM
                                                                         !< DIM 1: constituent id
                                                                         !< DIM 2: FM flow node id
    !
    ! character arrays
    !
    character(len=:)    , dimension(:)  , pointer  :: fm_namcon          !< Names of constituents as obtained from D-Flow FM
    character(MAXSTRLEN), dimension(:)  , pointer  :: basecase           !< Name identifying the FM model
    !
    ! logical arrays
    !
    !
    ! reals
    !
    real(fp)                                       :: current_time       !< COSUMO_BMI starts at t=0.0 and is updated via BMI calls
    !
    ! logicals
    !
    logical                                        :: skipuniqueid       !< true: add 6 random characters to name of communication files to ensure uniqueness
    !
    ! characters
    !
    character                                      :: slash              !< Directory separator, platform dependent
    character(6)                                   :: uniqueid           !< 6 capitals, randomly set in initialization phase
    character(MAXSTRLEN), target                   :: runid              !< As obtained from D-Flow FM
    character(MAXSTRLEN)                           :: infile             !< name of (XML) input/config file
    character(MAXSTRLEN), dimension(3)             :: filename           !< 1: FF2NF file name
                                                                         !< 2: NF2FF file name

contains
!
!
!==============================================================================
subroutine waq_bmi_data_init()
    use string_module, only: get_dirsep
    !
    ! Locals
    integer        :: i
    real(fp)       :: dummy
    character(300) :: cdummy
    !
    ! Body
    fm_ndx                  = 0
    fm_ndkx                 = 0
    fm_numconst             = 0
    fm_namlen               = 0
    fm_isalt                => null ()
    fm_itemp                => null ()
    !
    ! integer arrays
    !
    !
    ! Real arrays
    !
    fm_xzw            => null ()
    fm_yzw            => null ()
    fm_water_depth    => null ()
    fm_kbot           => null ()
    fm_ktop           => null ()
    fm_velocity_x     => null ()
    fm_velocity_y     => null ()
    fm_rho            => null ()
    fm_z_level        => null ()
    fm_constituents   => null ()
    !
    ! character arrays
    !
    fm_namcon         => null ()
    basecase          => null ()
    !
    ! logical arrays
    !
    !
    ! reals
    !
    current_time = 0.0_fp
    !
    ! logicals
    !
    skipuniqueid = .false.
    !
    ! characters
    !
    slash        = get_dirsep()
    call getcwd(cdummy)
    call getUniqueId(uniqueid,cdummy)
    !
    runid        = ' '
    infile       = ' '
    filename     = ' '
    !
    ! other
    !
end subroutine waq_bmi_data_init
!
!
!==============================================================================
subroutine realloc_waq_bmi_data()
    use m_alloc
    !
    call mess(LEVEL_INFO, "Reallocating waq arrays")
    !call reallocP(n_diff           , nf_num_dif                    , keepExisting=.false., fill = 0)
end subroutine realloc_waq_bmi_data
!
!
!==============================================================================
!> Clean up Nearfield data
subroutine waq_bmi_data_finalize()
    !
    ! integer arrays
    !
    !
    ! Real arrays
    !
    !
    ! character arrays
    !
    if (associated(fm_namcon)        ) deallocate(fm_namcon)
    if (associated(basecase)         ) deallocate(basecase)
    !
    ! logical arrays
    !
    !
    ! reals
    !
    current_time = 0.0_fp
    !
    ! logicals
    !
    !
    ! characters
    !
    infile    = ' '
    uniqueid  = ' '
    runid     = ' '
    filename  = ' '
    !
    ! other
    !
end subroutine waq_bmi_data_finalize
!
!
!==============================================================================
!> Creates a unique id of 6 characters, all capital alphabet elements.
!> Without seedString:
!>     RANDOM_SEED uses the current time.
!>     Id is not so unique: when starting this executable multiple times at the same time,
!>     there is a reasonable chance that the Id's are the same.
!>     A way to solve this is using Fortran2018: call RANDOM_INITILIZE(REPEATABLE=.false., IMAGE_DISTINCT=.true.)
!> With seedString:
!>     Id is the same when seedString is the same.
!>     Use this when a string can be used to create distinct Id's.
!>     Here, the CurrentWorkingDirectory is used.
!>     The ichar of each character in seedString are summed to obtain a "unique" integer, feeded to RANDOM_SEED
subroutine getUniqueId(id, seedString)
    character(UNIQUE_ID_LEN), intent(out) :: id          !< UniqueId
    character(*), optional  , intent(in ) :: seedString  !< String to distinct Id's
                                                         !< If seedString is the same then id is the same
    !
    ! Locals
    integer                            :: i         !< Loop parameter
    integer                            :: istat     !< (de-)allocate status
    integer                            :: charsum   !< Sum of ichar of each character in seedString
    integer                            :: seeddim   !< Dimension of the seed parameter
    integer, dimension(:), allocatable :: seedput   !< Seed argument with dimension seeddim, filled with charsum
    real(fp)                           :: rrandom   !< Output of random_number, in [0.0,1.0]
    !
    ! Body
    charsum = 0
    if (present(seedString)) then
        do i=1,len(seedString)
            charsum=charsum+ichar(seedString(i:i))
        enddo
        call RANDOM_SEED(size=seeddim)
        allocate(seedput(seeddim), stat=istat)
        seedput = charsum
        call RANDOM_SEED(put=seedput)
        deallocate(seedput, stat=istat)
    else
        call RANDOM_SEED()
    endif
    do i=1,UNIQUE_ID_LEN
        call random_number(rrandom)
        ! A = char(65)
        ! rrandom is in [0.0,1.0]
        id(i:i) = char(floor(65.0+rrandom*26.0))
    enddo
end subroutine getUniqueId


end module m_waq_bmi_data

