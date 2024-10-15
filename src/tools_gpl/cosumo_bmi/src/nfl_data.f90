!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------
!
module m_nfl_data
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
    integer, parameter :: READ_DIMENSIONS_ONLY = 1             !< The COSUMO configuration file is read in two steps:
                                                               !< Step 1: READ_DIMENSIONS_ONLY
    integer, parameter :: READ_FULL_CONTENTS   = 2             !< The COSUMO configuration file is read in two steps:
                                                               !< Step 2: READ_FULL_CONTENTS
    character(20), parameter :: KERNELNAME     = "COSUMO_BMI"  !< Name of this kernel
    integer, parameter :: UNIQUE_ID_LEN        = 6             !< Length of UniqueID
    !
    ! integers
    !
    integer                                        :: diafile            !< lunid of diagnosis file
    integer                                        :: nf_num_dif         !< number of diffusers/discharges
    integer                                        :: no_amb_max         !< Max number of ambient points over all diffusers
    integer                                        :: fm_ndx             !< From D-Flow FM: nr of flow nodes (internal + boundary)
    integer                                        :: fm_ndkx            !< From D-Flow FM: dim of 3d flow nodes (internal + boundary)
    integer                                        :: fm_numconst        !< From D-Flow FM: Total number of constituents
    integer                                        :: fm_namlen          !< From D-Flow FM: Max length of (character type) parameter names
    integer                             , pointer  :: fm_isalt           !< From D-Flow FM: index of salt        in constituents array
    integer                             , pointer  :: fm_itemp           !< From D-Flow FM: index of temperature in constituents array
    integer                                        :: nf_sour_idimMAX    !< Max number of source points over all diffusers
    integer                                        :: nf_sink_idimMAX    !< Max number of sink   points over all diffusers
    integer                                        :: nf_intake_idimMAX  !< Max number of intake points over all diffusers
    !
    ! integer arrays
    !
    integer , dimension(:)              , pointer  :: n_diff             !< Index to FM flow node of each diffuser location
    integer , dimension(:)              , pointer  :: no_amb             !< Number of ambient points for each diffuser
    integer , dimension(:,:)            , pointer  :: n_amb              !< Index to FM flow node of each ambient location
    integer , dimension(:)              , pointer  :: n_intake           !< Index to FM flow node of each intake  location as read from the cosumo_config.xml file
    integer , dimension(:)              , pointer  :: k_intake           !< Currently always 0: Index to FM 3D flow node of each intake location; is not specified in cosumo_config.xml file
    !
    ! Real arrays
    !
    real(fp), dimension(:)              , pointer  :: x_diff             !< X coordinate of each diffuser      as read from the cosumo_config.xml file
    real(fp), dimension(:)              , pointer  :: y_diff             !< Y coordinate of each diffuser      as read from the cosumo_config.xml file
    real(fp), dimension(:,:)            , pointer  :: x_amb              !< X coordinate of each ambient point as read from the cosumo_config.xml file
    real(fp), dimension(:,:)            , pointer  :: y_amb              !< Y coordinate of each ambient point as read from the cosumo_config.xml file
    real(fp), dimension(:)              , pointer  :: x_intake           !< X coordinate of each intake  point as read from the cosumo_config.xml file
    real(fp), dimension(:)              , pointer  :: y_intake           !< Y coordinate of each intake  point as read from the cosumo_config.xml file
    real(fp), dimension(:)              , pointer  :: q_diff             !< Discharge    of each diffuser      as read from the cosumo_config.xml file
    real(fp), dimension(:,:)            , pointer  :: const_diff         !< Additon (absolute of excess) to each constituent of each diffuser as read from the cosumo_config.xml file
    real(fp), dimension(:)              , pointer  :: d0                 !< Diameter of diffuser               as read from the cosumo_config.xml file
    real(fp), dimension(:)              , pointer  :: h0                 !< Height above bed of diffuser       as read from the cosumo_config.xml file
    real(fp), dimension(:)              , pointer  :: sigma0             !< Horizontal angle of diffuser       as read from the cosumo_config.xml file
                                                                         !< 0 pointing to east, 90 pointing to north
    real(fp), dimension(:)              , pointer  :: theta0             !< Not used Corjet parameter          as read from the cosumo_config.xml file
    real(fp), dimension(:,:)            , pointer  :: nf_const           !< Constituent values               as read from NF2FF file
                                                                         !< DIM 1: diffuser
                                                                         !< DIM 2: constituent
    real(fp), dimension(:,:,:)          , pointer  :: nf_intake          !< Intake distribution points       as read from NF2FF file
                                                                         !< DIM 1: diffuser
                                                                         !< DIM 2: intake point id
                                                                         !< DIM 3: X, Y, Z
    real(fp), dimension(:,:,:)          , pointer  :: nf_sink            !< Sinks distribution points        as read from NF2FF file
                                                                         !< DIM 1: diffuser
                                                                         !< DIM 2: sink point id
                                                                         !< DIM 3: X, Y, Z, S, H, B
    real(fp), dimension(:,:,:)          , pointer  :: nf_sour            !< Sources distribution points      as read from NF2FF file
                                                                         !< DIM 1: diffuser
                                                                         !< DIM 2: source point id
                                                                         !< DIM 3: X, Y, Z, S, H, B, Umag, Udir
    real(fp), dimension(:)              , pointer  :: nf_q_source        !< Qsource per diffuser             as read from NF2FF file
    real(fp), dimension(:)              , pointer  :: nf_q_intake        !< Qintake per diffuser             as read from NF2FF file
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
    character(MAXSTRLEN), dimension(:)  , pointer  :: basecase           !< Name identifying the FM model, may differ for each diffuser
    character(MAXSTRLEN), dimension(:)  , pointer  :: base_path          !< Path to the location to place the FF2NF file
    character(MAXSTRLEN), dimension(:)  , pointer  :: waitfiles          !< names of near field files to appear, created at an older timestep (itnflri > 0)
    character(10)       , dimension(:)  , pointer  :: nf_const_operator  !< Constituent operator as read from NF2FF file
    !
    ! logical arrays
    !
    logical, dimension(:)               , pointer  :: nf_src_mom         !< true: Umag and Udir in nf_sour are filled, as read from NF2FF file
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
                                                                         !< 3: Base case
    !
    ! other
    !
    type(tree_data), pointer                       :: cosumofile_ptr     !< Pointer to datastructure containing the contents of the cosumo_config.xml file

contains
!
!
!==============================================================================
subroutine nfl_data_init()
    use system_utils, only: FILESEP
    !
    ! Locals
    character(300) :: cdummy
    !
    ! Body
    nf_num_dif              = 0
    no_amb_max              = 0
    fm_ndx                  = 0
    fm_ndkx                 = 0
    fm_numconst             = 0
    fm_namlen               = 0
    fm_isalt                => null ()
    fm_itemp                => null ()
    nf_sour_idimMAX         = 0
    nf_sink_idimMAX         = 0
    nf_intake_idimMAX       = 0
    !
    ! integer arrays
    !
    n_diff            => null ()
    no_amb            => null ()
    n_amb             => null ()
    n_intake          => null ()
    k_intake          => null ()
    !
    ! Real arrays
    !
    x_diff            => null ()
    y_diff            => null ()
    x_amb             => null ()
    y_amb             => null ()
    x_intake          => null ()
    y_intake          => null ()
    q_diff            => null ()
    const_diff        => null ()
    d0                => null ()
    h0                => null ()
    sigma0            => null ()
    theta0            => null ()
    nf_const          => null ()
    nf_intake         => null ()
    nf_sink           => null ()
    nf_sour           => null ()
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
    nf_q_source       => null ()
    nf_q_intake       => null ()
    !
    ! character arrays
    !
    fm_namcon         => null ()
    basecase          => null ()
    base_path         => null ()
    waitfiles         => null ()
    nf_const_operator => null ()
    !
    ! logical arrays
    !
    nf_src_mom   => null ()
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
    slash        = FILESEP
    call getcwd(cdummy)
    call getUniqueId(uniqueid,cdummy)
    !
    runid        = ' '
    infile       = ' '
    filename     = ' '
    !
    ! other
    !
    cosumofile_ptr => null ()
end subroutine nfl_data_init
!
!
!==============================================================================
subroutine realloc_nfl_data()
    use m_alloc
    !
    call mess(LEVEL_INFO, "Reallocating nfl arrays")
    call reallocP(n_diff           , nf_num_dif                    , keepExisting=.false., fill = 0)
    call reallocP(no_amb           , nf_num_dif                    , keepExisting=.false., fill = 0)
    call reallocP(n_amb            , (/ nf_num_dif, no_amb_max /)  , keepExisting=.false., fill = 0)
    call reallocP(n_intake         , nf_num_dif                    , keepExisting=.false., fill = 0)
    call reallocP(k_intake         , nf_num_dif                    , keepExisting=.false., fill = 0)
    call reallocP(x_diff           , nf_num_dif                    , keepExisting=.false., fill = 0.0_fp)
    call reallocP(y_diff           , nf_num_dif                    , keepExisting=.false., fill = 0.0_fp)
    call reallocP(x_amb            , (/ nf_num_dif, no_amb_max /)  , keepExisting=.false., fill = 0.0_fp)
    call reallocP(y_amb            , (/ nf_num_dif, no_amb_max /)  , keepExisting=.false., fill = 0.0_fp)
    call reallocP(x_intake         , nf_num_dif                    , keepExisting=.false., fill = 0.0_fp)
    call reallocP(y_intake         , nf_num_dif                    , keepExisting=.false., fill = 0.0_fp)
    call reallocP(q_diff           , nf_num_dif                    , keepExisting=.false., fill = 0.0_fp)
    call reallocP(const_diff       , (/ nf_num_dif, fm_numconst /) , keepExisting=.false., fill = 0.0_fp)
    call reallocP(d0               , nf_num_dif                    , keepExisting=.false., fill = 0.0_fp)
    call reallocP(h0               , nf_num_dif                    , keepExisting=.false., fill = 0.0_fp)
    call reallocP(sigma0           , nf_num_dif                    , keepExisting=.false., fill = 0.0_fp)
    call reallocP(theta0           , nf_num_dif                    , keepExisting=.false., fill = 0.0_fp)
    call reallocP(nf_const         , (/ nf_num_dif, fm_numconst /) , keepExisting=.false., fill = 0.0_fp)
    call reallocP(basecase         , nf_num_dif                    , keepExisting=.false., fill = ' ')
    call reallocP(base_path        , nf_num_dif                    , keepExisting=.false., fill = ' ')
    call reallocP(waitfiles        , nf_num_dif                    , keepExisting=.false., fill = ' ')
    call reallocP(nf_const_operator, nf_num_dif                    , keepExisting=.false., fill = ' ')
    call reallocP(nf_q_intake      , nf_num_dif                    , keepExisting=.false., fill = 0.0_fp)
    call reallocP(nf_q_source      , nf_num_dif                    , keepExisting=.false., fill = 0.0_fp)
    call reallocP(nf_src_mom       , nf_num_dif                    , keepExisting=.false., fill = .false.)
end subroutine realloc_nfl_data
!
!
!==============================================================================
!> Clean up Nearfield data
subroutine nfl_data_finalize()
    nf_num_dif = 0
    no_amb_max = 0
    !
    ! integer arrays
    !
    if (associated(n_diff)  ) deallocate(n_diff)
    if (associated(no_amb)  ) deallocate(no_amb)
    if (associated(n_amb)   ) deallocate(n_amb)
    if (associated(n_intake)) deallocate(n_intake)
    if (associated(k_intake)) deallocate(k_intake)
    !
    ! Real arrays
    !
    if (associated(x_diff)     ) deallocate(x_diff)
    if (associated(y_diff)     ) deallocate(y_diff)
    if (associated(x_amb)      ) deallocate(x_amb)
    if (associated(y_amb)      ) deallocate(y_amb)
    if (associated(x_intake)   ) deallocate(x_intake)
    if (associated(y_intake)   ) deallocate(y_intake)
    if (associated(q_diff)     ) deallocate(q_diff)
    if (associated(const_diff) ) deallocate(const_diff)
    if (associated(d0)         ) deallocate(d0)
    if (associated(h0)         ) deallocate(h0)
    if (associated(sigma0)     ) deallocate(sigma0)
    if (associated(theta0)     ) deallocate(theta0)
    if (associated(nf_const)   ) deallocate(nf_const)
    if (associated(nf_intake)  ) deallocate(nf_intake)
    if (associated(nf_sink)    ) deallocate(nf_sink)
    if (associated(nf_sour)    ) deallocate(nf_sour)
    if (associated(nf_q_source)) deallocate(nf_q_source)
    if (associated(nf_q_intake)) deallocate(nf_q_intake)
    !
    ! character arrays
    !
    if (associated(fm_namcon)        ) deallocate(fm_namcon)
    if (associated(basecase)         ) deallocate(basecase)
    if (associated(base_path)        ) deallocate(base_path)
    if (associated(waitfiles)        ) deallocate(waitfiles)
    if (associated(nf_const_operator)) deallocate(nf_const_operator)
    !
    ! logical arrays
    !
    if (associated(nf_src_mom)) deallocate(nf_src_mom)
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
    if (associated(cosumofile_ptr)) deallocate(cosumofile_ptr)
end subroutine nfl_data_finalize
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


end module m_nfl_data

