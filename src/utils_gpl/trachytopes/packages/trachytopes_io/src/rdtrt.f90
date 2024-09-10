module m_rdtrt

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
!  
!  
!!--description-----------------------------------------------------------------

use precision

!
! functions and subroutines
!

public rdtrt
public rdttar
public expblk
public dimtrt
public dittar

contains

subroutine rdtrt(lundia    ,error     ,lftrto    ,dt        , &
               & kmax      ,itimtt    ,gdtrachy  , &
               & griddim   ,dryflc    ,mdfile_ptr,waqol     , &
               & ddbval    ,d3d_tunit)

!
! Reads the dimensions ntrt, nttaru, nttarv
! Initializes nroupa
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use properties
    use trachytopes_data_module
    use grid_dimens_module, only: griddimtype
    use dfparall
    use system_utils, only: exifil
    use MessageHandling
    !
    implicit none
    !
!    type(trachy_type),target :: gdtrachy
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                   , pointer :: alf_area_ser
    real(fp)                   , pointer :: trtminh
    real(fp)                   , pointer :: dttrt
    integer                    , pointer :: iarea_avg
    integer                    , pointer :: nodir
    integer                    , pointer :: nroupa
    integer                    , pointer :: nttaru
    integer                    , pointer :: ntrt
    integer                    , pointer :: max_cl
    integer                    , pointer :: ntrtnrm
    integer                    , pointer :: ntrtcrs
    integer                    , pointer :: ntrtobs
    integer                    , pointer :: n_q
    integer                    , pointer :: n_zs
    integer                    , pointer :: mfg
    integer                    , pointer :: nfg
    integer                    , pointer :: nlb
    integer                    , pointer :: nub
    integer                    , pointer :: nmlb
    integer                    , pointer :: nmub
    integer                    , pointer :: mlb
    integer                    , pointer :: mub
    integer                    , pointer :: mmax
    integer                    , pointer :: nmax

    integer , dimension(:,:)   , pointer :: ittaru
    integer , dimension(:)     , pointer :: itrt_list
    integer , dimension(:,:)   , pointer :: ittdef
    integer , dimension(:,:)   , pointer :: ittlin
    real(fp), dimension(:)     , pointer :: fraccu_list
    real(fp), dimension(:)     , pointer :: rgcalu
    real(fp), dimension(:)     , pointer :: rttaru
    real(fp), dimension(:,:)   , pointer :: rttxyz
!    real(fp), dimension(:)     , pointer :: rttarv
    real(fp), dimension(:,:)   , pointer :: rttdef
    real(fp), dimension(:,:)   , pointer :: rttfu
!    real(fp), dimension(:,:)   , pointer :: rttfv
    real(fp), dimension(:)     , pointer :: rttacLin
    real(fp), dimension(:)     , pointer :: vegh2d
    real(fp), dimension(:)     , pointer :: vden2d
    character(256)             , pointer :: md_ttdfile ! Name of trachytopes definition file.
    character(256)             , pointer :: md_arlfile ! Name of trachytopes area definition file

    type(trachy_type) , target               :: gdtrachy
    type (griddimtype), target , intent(in)  :: griddim
    real(fp)                   , intent(in)  :: dryflc
    type (tree_data)  , pointer, intent(in)  :: mdfile_ptr
    logical                    , intent(in)  :: waqol
    real(fp)                   , intent(in)  :: d3d_tunit ! d3d_tunit available for compatitibility with Delft3D

!
! Local parameters
!
    integer, parameter :: IROUGH = 300
    integer, parameter :: MAXFLD = 12
!
! Global variables
!
    integer  :: itimtt
    integer  :: itrtcrs
    integer  :: itrtobs
    integer  :: lundia
    integer  :: kmax
    integer  :: nmaxus
    integer  :: nmmax
    integer  :: ddbval
    integer  :: prev_trt_no    ! previous trt_no for discharge/waterlevel dependent trachytopes
    integer  :: prev_trt_type  ! previous type of discharge/waterlevel dependent trachytopes
    logical  :: error
    logical  :: lftrto
    real(fp) :: dt
!
! Local variables
!
    integer                          :: i
    integer                          :: ibeg
    integer                          :: icx
    integer                          :: iend
    integer                          :: iocond
    integer                          :: istat
    integer                          :: istr
    integer                          :: it
    integer                          :: itttmp
    integer                          :: j
    integer                          :: jdir
    integer                          :: lfile
    integer                          :: luntmp
    integer                          :: m
    integer                          :: m1
    integer                          :: mll
    integer                          :: mcurec
    integer                          :: mtrt
    integer                          :: mtrtnrm
    integer                          :: mtrtcrs
    integer                          :: mtrtobs
    integer                          :: m_q
    integer                          :: m_zs
    integer                          :: n
    integer                          :: n1
    integer                          :: nll
    integer                          :: nm
    integer                          :: nrflds
    integer, dimension(IROUGH)       :: nropar
    integer, dimension(MAXFLD)       :: ifield
    integer, dimension(MAXFLD)       :: itype
    integer, dimension(MAXFLD)       :: lenchr
    integer                          :: nmaxddb
    logical                          :: lokay
    logical                          :: lrcode
    real(fp)                         :: rtimtt
    real(fp), dimension(MAXFLD)      :: rfield
    character(30), dimension(MAXFLD) :: cfield
    character(11)                    :: fmttmp
    character(132)                   :: rec132
    character(20)                    :: chulp
    character(256)                   :: filtmp
    character(256)                   :: msgtmp
    character(6)                     :: keyw
    character(20)                    :: txtput1
!
!! executable statements -------------------------------------------------------
!
    alf_area_ser   => gdtrachy%gen%alf_area_ser
    trtminh        => gdtrachy%gen%trtminh
    iarea_avg      => gdtrachy%gen%iarea_avg
    nroupa         => gdtrachy%gen%nroupa
    ntrt           => gdtrachy%gen%ntrt
    ntrtnrm        => gdtrachy%gen%ntrtnrm
    ntrtcrs        => gdtrachy%gen%ntrtcrs
    ntrtobs        => gdtrachy%gen%ntrtobs
    max_cl         => gdtrachy%gen%max_cl
    n_q            => gdtrachy%gen%n_q
    n_zs           => gdtrachy%gen%n_zs
    nodir          => gdtrachy%gen%nodir
    md_ttdfile     => gdtrachy%gen%md_ttdfile
    dttrt          => gdtrachy%gen%dttrt
    itrt_list      => gdtrachy%gen%itrt_list
    fraccu_list    => gdtrachy%gen%fraccu_list
    !
    nlb            => griddim%nlb    !WO - was gdp%d%nlb  previously
    nub            => griddim%nub    !
    mlb            => griddim%mlb    !
    mub            => griddim%mub    !
    nmlb           => griddim%nmlb   !
    nmub           => griddim%nmub   !
    mfg            => griddim%mfg    !
    nfg            => griddim%nfg    !
    nmax           => griddim%nmax
    mmax           => griddim%mmax

    if (mod(nmax,2) == 1) then
        nmaxus = nmax - 1
    else
        nmaxus = nmax
    end if

    nmaxddb = nmax + 2*ddbval

    nmmax = nmaxddb*(mmax + 2*ddbval)
    !nm      = n + nmaxddb * (m - 1 + ddbval ) + ddbval
    !(see call n_and_m_to_nm.f90 WO)
    !
    ! n_m_to_nm      => griddim%n_m_to_nm
    !
    !
    ! Allocate trachytope arrays that are used in main routines
    !
    if (.not. associated(gdtrachy%dir(1)%rttfu)) then
       !
       istat = 0
       if (waqol) then
          if (istat==0) allocate(gdtrachy%gen%vegh2d(nmlb:nmub), stat = istat)
          if (istat==0) allocate(gdtrachy%gen%vden2d(nmlb:nmub), stat = istat)
          vegh2d      => gdtrachy%gen%vegh2d
          vden2d      => gdtrachy%gen%vden2d
          vegh2d = 0.0_fp
          vden2d = 0.0_fp
       endif
       !
       do jdir = 1,nodir
          if (istat==0) allocate(gdtrachy%dir(jdir)%rttfu(nmlb:nmub,kmax), stat = istat)
          !
          if (istat/=0) then
             call SetMessage(LEVEL_ERROR, 'RDTRT: memory alloc error')
             error = .true.
             return
          endif
          !
          ! update local pointers
          !
          rttfu          => gdtrachy%dir(jdir)%rttfu
          md_arlfile     => gdtrachy%dir(jdir)%md_arlfile
          !
          ! initialize arrays
          !
          rttfu(nmlb:nmub,1:kmax) = 0.0_fp
          !
       end do
    endif
    !
    ! Initialize scalars
    !
    lftrto       = .false.
    itimtt       = 1
    iarea_avg    = 1
    alf_area_ser = 0.6_fp
    !
    ! Read value of Trtrou, default NO
    !
    chulp = 'N'
    call prop_get(mdfile_ptr, '*','Trtrou',chulp)
    !
    ! set LFTRTO to TRUE if CHULP = Y/y
    !
    call small(chulp ,1 )
    if (chulp=='y') lftrto = .true.
    !
    ! if Trtrou turned out to be NO, don't look any further.
    !
    if (.not.lftrto) goto 9999
    !
    ! Allocate trachytope arrays that are only used locally
    !
    istat = 0
    if (.not. associated(gdtrachy%dir(1)%ittaru)) then
       do jdir = 1,nodir
          nttaru => gdtrachy%dir(jdir)%nttaru
                        allocate(gdtrachy%dir(jdir)%ittaru(nttaru,4)                      , stat = istat)
          if (istat==0) allocate(gdtrachy%dir(jdir)%rgcalu(nmlb:nmub)                     , stat = istat)
          if (istat==0) allocate(gdtrachy%dir(jdir)%rttaru(nttaru)                        , stat = istat)
          if (istat==0) allocate(gdtrachy%dir(jdir)%rttxyz(nttaru,3)                      , stat = istat)
          if (istat==0) allocate(gdtrachy%dir(jdir)%lin(2,nmlb:nmub)                      , stat = istat)
          if (istat==0) allocate(gdtrachy%dir(jdir)%acLin(nmlb:nmub)                      , stat = istat)
          if (istat==0) allocate(gdtrachy%dir(jdir)%blu_trt(nmlb:nmub)                    , stat = istat)
          if (istat==0) allocate(gdtrachy%dir(jdir)%kcu_trt(nmlb:nmub)                    , stat = istat)
          if (istat==0) allocate(gdtrachy%dir(jdir)%zsu_prev(nmlb:nmub)                   , stat = istat)
       end do
       ! general
       if (istat==0) allocate(gdtrachy%gen%ittdef(ntrt,2)                                 , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%rttdef(ntrt,nroupa)                            , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%crs(ntrtcrs)                                   , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%obs(ntrtobs)                                   , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%table_zs(n_zs)                                 , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%table_q(n_q)                                   , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%rttdef_zs(n_zs,nroupa)                         , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%rttdef_zs_slope(n_zs,nroupa)                   , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%rttdef_zs_cross(n_zs,nroupa)                   , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%ittdef_zs(n_zs)                                , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%rttdef_q(n_q,nroupa)                           , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%rttdef_q_slope(n_q,nroupa)                     , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%rttdef_q_cross(n_q,nroupa)                     , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%ittdef_q(n_q)                                  , stat = istat)
       !
       if (istat/=0) then
          call SetMessage(LEVEL_ERROR, 'RDTRT: memory alloc error')
          error = .true.
          return
       endif
       !
       ! update local pointers
       !
       ! general
       md_ttdfile     => gdtrachy%gen%md_ttdfile
       ittdef         => gdtrachy%gen%ittdef
       rttdef         => gdtrachy%gen%rttdef
       !
       do jdir = 1,nodir
          !
          ittlin          => gdtrachy%dir(jdir)%lin
          rttacLin        => gdtrachy%dir(jdir)%acLin
!          md_arlfile      => gdtrachy%dir(jdir)%md_arlfile
          if (jdir == 1) then
             icx = nmaxddb   ! nmax
          else
             icx = 1
          end if
          do m = 1,mmax
             do n = 1,nmax
                nm          = n + nmaxddb * (m - 1 + ddbval ) + ddbval
                ittlin(1,nm) = nm
                ittlin(2,nm) = nm+icx
                rttacLin(nm)  = 0.5_fp
             enddo
          enddo
       enddo
    endif
    !
    call SetMessage(LEVEL_INFO, 'Start  of trachytopes input')
    !
    ! locate 'DtTrt' record for update time step
    !
    rtimtt = dt
    call prop_get(mdfile_ptr, '*', 'DtTrt', rtimtt)
    dttrt = rtimtt
    !
    ! Check on multiple
    itimtt = nint(rtimtt/dt)
    if (abs(real(itimtt,fp)*dt-rtimtt) > (0.1_fp*dt)) then       ! previously : if (dtn(itimtt, rtimtt, dt)) which is approximate - updates are preformed within 0.1 of dtmax, cf. Delft3D, WO)
       call SetMessage(LEVEL_ERROR, 'RDTRT: Trachytope update time is not a multiple of the user time step' )
       error = .true.
       goto 9999
    endif
    if (abs(real(itimtt,fp)*dt) < (0.1_fp*dt)) then
       call SetMessage(LEVEL_ERROR, 'RDTRT: Trachytope update time is smaller than the user time step' )
       error = .true.
       goto 9999
    endif
    !
    txtput1 = 'DtTrt'
    write (msgtmp, '(a,a,f7.3,a)') txtput1,': ',rtimtt*d3d_tunit,' seconds'
    call SetMessage(LEVEL_INFO, msgtmp)
    write (msgtmp, '(a,a,i5,a)') txtput1,': every ',itimtt,' timesteps'
    call SetMessage(LEVEL_INFO, msgtmp)
    !
    ! Trtdef: trachytope definition file (must exist, no default)
    !
    filtmp = ' '
    keyw   = 'Trtdef'
    call prop_get(mdfile_ptr, '*',keyw,filtmp)
    md_ttdfile = filtmp
    !
    txtput1 = keyw
    write (msgtmp, '(a,a,a)') txtput1,': ',trim(filtmp)
    call SetMessage(LEVEL_INFO, msgtmp)
    !
    ! keyword not found ?
    !
    if (filtmp == ' ') then
       call SetMessage(LEVEL_ERROR, 'Missing value (or the keyword): '// keyw //' in file')
       error = .true.
       goto 9999
    endif
    !
    ! test file existence
    !
    lfile = index(filtmp, ' ')
    if (lfile==0) lfile = 13
    lfile = lfile - 1
    if (.not.exifil(trim(filtmp), lundia)) then
       !
       ! file does not exist !!
       !
       call SetMessage(LEVEL_ERROR, 'The specified file '// trim(filtmp) //' does not exist')
       error = .true.
       goto 9999
    endif
    !
    ! open trachytope definition file
    !
    open (newunit=luntmp, file = trim(filtmp), form = 'formatted', iostat = iocond,  &
        & status = 'old')
    if (iocond/=0) then
       call SetMessage(LEVEL_ERROR, 'Error while opening file '// trim(filtmp))
       error = .true.
       goto 9999
    endif
    !
    ! freeformatted file
    ! read records till end of file
    !
    mtrt    = 0
    mcurec  = 0
    mtrtnrm = 0
    mtrtcrs = 0
    mtrtobs = 0
    m_q     = 0
    m_zs    = 0
    prev_trt_no   = TRACHY_UNDEFINED
    prev_trt_type = TRACHY_UNDEFINED
    !
    ! Create list with possible roughnes descriptions.
    !     NROPAR the number of parameters for the codes
    !
    nropar = -1
    !
    !    1-49: Special treatment
    !
    !     Water free terrain
    nropar(1) = 0
    !
    !     Combinations of area roughnesses
    nropar(2) = 4
    !
    !   51-99: Standard roughness coefficients
    !
    !     Constant White-Colebrook / Nikuradse value
    nropar(51) = 1
    !
    !     Constant Chezy value
    nropar(52) = 1
    !
    !     Constant Manning value
    nropar(53) = 1
    !
    !     Constant z0 value
    nropar(54) = 1
    !
    !     Ebb-flood constant White-Colebrook / Nikuradse value
    nropar(61) = 2
    !
    !     Ebb-flood constant Chezy value
    nropar(62) = 2
    !
    !     Ebb-flood constant Manning value
    nropar(63) = 2
    !
    !     Ebb-flood constant z0 value
    nropar(64) = 2
    !
    !    ! 101-149: Alluvial roughness predictors
    !
    !     Waqua description (Simple v. Rijn)
    nropar(101) = 2
    !
    !     power law
    nropar(102) = 2
    !
    !     Van Rijn
    nropar(103) = 0
    !
    !     Struiksma
    nropar(104) = 5
    !
    !     Quadratic combination of bedform roughness heights
    ! used to be 7, use keywords: BdfRpC, BdfRpR, BdfMrC, BdfMrR, BdfDnC, BdfDnR, BdfD50
    nropar(105) = 0
    !
    !     Linear superposition of bedform roughness heights
    nropar(106) = 0
    !
    ! 151-199: Vegetation roughness predictors (areas)
    !
    !     Waqua vegetation formulation 1
    nropar(151) = 2
    !
    !     Waqua vegetation formulation 2
    nropar(152) = 4
    !
    !     Baptist vegetation formulation
    nropar(153) = 4
    nropar(154) = 4
    !     Vaestilae & Jaervelae (2017) and Jarvela (2004) vegetation formulations
    nropar(155) = 10
    nropar(156) = 6
    !
    ! 201-249: Vegetation roughness predictors (linear)
    !
    !     Waqua lineair elements formulation 1
    nropar(201) = 2
    !
    !     Waqua lineair elements formulation 2
    nropar(202) = 4
    !
    ! 251-299: Vegetation roughness predictors (point)
    !
    !     Waqua tree elements formulation 1
    nropar(251) = 2
    !
    ! -->
    !
    ! read line
    !
  110 continue
    read (luntmp, '(a)', iostat = iocond) rec132
    if (iocond/=0) then
       !
       ! End-of-file ?
       !
       if (iocond<0) goto 199
       !
       ! Reading error
       !
       call SetMessage(LEVEL_ERROR, 'Read error from file: '// trim(filtmp))
       error = .true.
       goto 199
    else
       mcurec = mcurec + 1
    endif
    !
    ! Interpret line ...
    !
    !
    ! Comment line
    !
    if ((rec132(1:1)=='*') .or. (rec132(1:1)=='#')) goto 110
    !
    ! Scan the record.
    !
    ibeg = 1
    iend = 132
    !
    ! Comment at end of line
    !
    ! loop over rec132 and find #
    !
    comment_at_end_of_line_loop: &
        do istr = ibeg, iend
            if (rec132(istr:istr) == '#') then
                iend = istr
                exit comment_at_end_of_line_loop
            end if
        end do &
    comment_at_end_of_line_loop
    !
    call scannr(rec132    ,ibeg      ,iend      ,nrflds    ,itype     , &
              & ifield    ,rfield    ,cfield    ,lenchr    ,MAXFLD    , &
              & .true.    ,.true.    ,.true.    )
    !
    ! When no sub-fields are found, record appears to be empty
    !
    if (nrflds==0) goto 110
    !
    if (nrflds<0) then
       !
       ! Cannot interpret line
       !
       rec132 = ' '
       write (rec132, '(i12)') mcurec
       call SetMessage(LEVEL_ERROR, 'Read error from file: '// &
           & trim(filtmp) // ', Record: ' // trim(rec132))
       error = .true.
       goto 199
    endif
    !
    ! Check the contents
    !
    if (itype(1)==1 .and. itype(2)==1 .and. ifield(1) /= prev_trt_no) then
       !
       ! Determine roughness description (i)
       !
       lrcode = .false.
       if (ifield(2)<=IROUGH) then
          i = ifield(2)
          if (nropar(i)>=0) then
             lrcode = .true.
          endif
       endif
       if (lrcode) then
          !
          ! Check required parameters
          !
          if (nrflds/=nropar(i) + 2) then
             rec132 = ' '
             write (rec132, '(i12)') mcurec
             call SetMessage(LEVEL_ERROR, 'Trachytopes: Wrong number of parameters defined, File: ' &
                 & // trim(filtmp) // ', Record: ' // trim(rec132))
             error  = .true.
             goto 199
          endif
          !
          ! Check the parameters
          !
          lokay = .true.
          do j = 1, nropar(i)
             if (itype(j + 2)/=2 .and. itype(j + 2)/=1) then
                lokay = .false.
             endif
          enddo
          !
          ! Cannot interpret line
          !
          if (.not.lokay) then
             rec132 = ' '
             write (rec132, '(i12)') mcurec
             call SetMessage(LEVEL_FATAL, 'Read error from file: '// &
                 & trim(filtmp) // ', Record: ' // trim(rec132))
             error = .true.
             goto 199
          endif
          !
          ! Everything seems to be OK.
          !
          ! Increment MTRT for checking array size.
          !
          mtrt = mtrt + 1
          mtrtnrm = mtrtnrm + 1
          prev_trt_no   = TRACHY_UNDEFINED
          prev_trt_type = TRACHY_UNDEFINED
          !
          if (mtrt>ntrt) then
             call SetMessage(LEVEL_ERROR, 'Trachytopes: Number of trachytope definitions in file > array size.')
             error = .true.
             goto 199
          endif
          if (mtrtnrm>ntrtnrm) then
             call SetMessage(LEVEL_ERROR, 'Trachytopes: Number of standard trachytope definitions in file > array size.')
             error = .true.
             goto 199
          endif
          !
          ! Store the trachytope number and roughness description in arrays
          !
          ittdef(mtrt, 1) = ifield(1)
          ittdef(mtrt, 2) = ifield(2)
          !
          ! Store parameters data in array
          !
          do j = 1, nropar(i)
             if (itype(j + 2)==2) then
                rttdef(mtrt, j) = rfield(j + 2)
             else
                rttdef(mtrt, j) = real(ifield(j + 2),fp)
             endif
          enddo
       else
          !
          ! Unknown rougness code
          !
          rec132 = ' '
          write (rec132, '(i12)') mcurec
          call SetMessage(LEVEL_ERROR, 'Trachytopes: Unknown roughness code in  file ' &
              & // trim(filtmp) // ', Record: ' // trim(rec132))
          error  = .true.
          goto 199
       endif
    elseif (itype(1)==1 .and. itype(2)==3) then
        ! Reading is done in trachytopes module
        ! Checking of valid cross-section/observation-station name is done later in FM or Delft3D
        !
        if (cfield(2)(1:9) == 'DISCHARGE') then        ! TO DO: use strcmpi?
            ! DISCHARGE    -> cross-section name
            mtrtcrs = mtrtcrs + 1
            mtrt    = mtrt + 1
            prev_trt_no   = ifield(1)
            prev_trt_type = TRACHY_DISCHARGE_TYPE
            !
            gdtrachy%gen%crs(mtrtcrs)%name   = cfield(3)
            gdtrachy%gen%crs(mtrtcrs)%id     = TRACHY_UNDEFINED
            gdtrachy%gen%crs(mtrtcrs)%rec132 = rec132
            gdtrachy%gen%crs(mtrtcrs)%itrt   = mtrt
            gdtrachy%gen%crs(mtrtcrs)%idx_start = m_q + 1
            gdtrachy%gen%crs(mtrtcrs)%idx_end   = m_q         ! gets updated when first dependent row is read
            !
            if (mtrtcrs>ntrtcrs) then
                call SetMessage(LEVEL_ERROR, &
                    & 'Trachytopes: Number of discharge-dependent trachytope definitions in file > array size.')
                error = .true.
                goto 199
            endif
            if (mtrt>ntrt) then
                call SetMessage(LEVEL_ERROR, &
                    & 'Trachytopes: Number of trachytope definitions in file > array size (check in discharge).')
                error = .true.
                goto 199
            endif
            !
            goto 110
        elseif (cfield(2)(1:10) == 'WATERLEVEL') then  ! TO DO: use strcmpi?
            ! WATERLEVEL   -> observation-station name
            mtrtobs = mtrtobs + 1
            mtrt    = mtrt + 1
            prev_trt_no   = ifield(1)
            prev_trt_type = TRACHY_WATERLEVEL_TYPE
            !
            gdtrachy%gen%obs(mtrtobs)%name   = cfield(3)
            gdtrachy%gen%obs(mtrtobs)%id     = TRACHY_UNDEFINED
            gdtrachy%gen%obs(mtrtobs)%rec132 = rec132
            gdtrachy%gen%obs(mtrtobs)%itrt   = mtrt
            gdtrachy%gen%obs(mtrtobs)%idx_start = m_zs + 1
            gdtrachy%gen%obs(mtrtobs)%idx_end   = m_zs       ! gets updated when first dependent row is read
            !
            if (mtrtobs>ntrtobs) then
                call SetMessage(LEVEL_ERROR, 'Trachytopes: Number of waterlevel-dependent trachytope definitions in file > array size.')
                error = .true.
                goto 199
            endif
            if (mtrt>ntrt) then
                call SetMessage(LEVEL_ERROR, 'Trachytopes: Number of trachytope definitions in file > array size (check in waterlevel).')
                error = .true.
                goto 199
            endif
            !
            goto 110
        else
            call SetMessage(LEVEL_ERROR, 'Trachytopes: Unknown roughness code in  file ' // trim(filtmp) // ', Record: ' // trim(rec132))
            error  = .true.
            goto 199
        end if
    elseif (itype(1)==1 .and. itype(2)/=3 .and. itype(3)==1) then
        if (ifield(1) == prev_trt_no) then
            ! add to table for discharge/water level depdendent data
            if (prev_trt_type == TRACHY_WATERLEVEL_TYPE) then
                ! add values to table for waterlevel dependent trachytopes
                m_zs = m_zs + 1
                ittdef(mtrt, 1) = ifield(1)
                ittdef(mtrt, 2) = ifield(3)
                gdtrachy%gen%obs(mtrtobs)%nropars   = nropar(ifield(3))
                gdtrachy%gen%ittdef_zs(m_zs) = ifield(3)
                !
                ! Store parameters data in array temporarily (will be overwritten later)
                !
                lrcode = .false.
                if (ifield(3)<=IROUGH) then
                    i = ifield(3)
                    if (nropar(i)>=0) then
                        lrcode = .true.
                    end if
                end if
                if (lrcode) then
                    gdtrachy%gen%table_zs(m_zs)         = rfield(2)
                    gdtrachy%gen%obs(mtrtobs)%idx_end   = m_zs       ! check later: idx_end >= idx_start + 1
                    do j = 1, nropar(i)
                        rttdef(mtrt, j)    = rfield(j + 3)
                        gdtrachy%gen%rttdef_zs(m_zs, j)          = rfield(j + 3)
                    end do
                else
                    call SetMessage(LEVEL_ERROR, 'Trachytopes: Unknown roughness code for waterlevel dependence in file ' &
                        & // trim(filtmp) // ', Record: ' // trim(rec132))
                    error  = .true.
                    goto 199
                end if
                !
            elseif (prev_trt_type == TRACHY_DISCHARGE_TYPE) then
                ! add values to table for discharge dependent trachytopes
                m_q = m_q + 1
                ittdef(mtrt, 1) = ifield(1)
                ittdef(mtrt, 2) = ifield(3)
                gdtrachy%gen%crs(mtrtcrs)%nropars   = nropar(ifield(3))
                gdtrachy%gen%ittdef_q(m_q) = ifield(3)
                !
                ! Store parameters data in array temporarily (will be overwritten later)
                !
                lrcode = .false.
                if (ifield(3)<=IROUGH) then
                    i = ifield(3)
                    if (nropar(i)>=0) then
                        lrcode = .true.
                    end if
                end if
                if (lrcode) then
                    gdtrachy%gen%table_q(m_q)           = rfield(2)
                    gdtrachy%gen%crs(mtrtcrs)%idx_end   = m_q        ! check later: idx_end >= idx_start + 1
                    do j = 1, nropar(i)
                        rttdef(mtrt, j)    = rfield(j + 3)
                        gdtrachy%gen%rttdef_q(m_q, j)   = rfield(j + 3)
                    end do
                else
                    call SetMessage(LEVEL_ERROR, 'Trachytopes: Unknown roughness code for discharge dependence in file ' &
                        & // trim(filtmp) // ', Record: ' // trim(rec132))
                    error  = .true.
                    goto 199
                end if
            else
                call SetMessage(LEVEL_ERROR, 'Unknown read error #3 in rdtrt.f90')
                error = .true.
            end if
        else
            call SetMessage(LEVEL_ERROR, 'Unknown read error #4 in rdtrt.f90')
            error = .true.
        end if
    else
       !
       ! Cannot interpret line
       !
       rec132 = ' '
       write (rec132, '(i12)') mcurec
       call SetMessage(LEVEL_ERROR, 'Read error from file: '// trim(filtmp) // &
           & ', Record: ' // trim(rec132))
       error = .true.
       goto 199
    endif
    !
    ! Go to the next record.
    !
    goto 110
    ! <--
    !
    ! close file
    !
  199 continue
    close (luntmp)
    !
    ! Check that tables have entries [ idx_start > idx_end]
    !
    do itrtcrs = 1, ntrtcrs
        if (gdtrachy%gen%crs(itrtcrs)%idx_start + 1 > gdtrachy%gen%crs(itrtcrs)%idx_end) then
            call SetMessage(LEVEL_ERROR, 'Error reading trachytopes: Expected 2 '// &
                & 'or more TRTDEF DISCHARGE TRT_EQ PARAMS after ->  "'//trim(gdtrachy%gen%md_ttdfile)// &
                & '": '//trim(gdtrachy%gen%crs(itrtcrs)%rec132))
            error = .true.
        end if
        itttmp = gdtrachy%gen%ittdef_q(gdtrachy%gen%crs(itrtcrs)%idx_start)
        do m_q = gdtrachy%gen%crs(itrtcrs)%idx_start, gdtrachy%gen%crs(itrtcrs)%idx_end
            if (gdtrachy%gen%ittdef_q(m_q) /= itttmp) then
               call SetMessage(LEVEL_ERROR, 'Error reading trachytopes: Trt function '//&
                   & 'type should be the same for all discharges in  "'//&
                   & trim(gdtrachy%gen%md_ttdfile)//'": '//trim(gdtrachy%gen%crs(itrtcrs)%rec132))
               error = .true.
            end if
        end do
        do m_q = gdtrachy%gen%crs(itrtcrs)%idx_start+1, gdtrachy%gen%crs(itrtcrs)%idx_end
            ! check if discharges are monotonically increasing
            if (gdtrachy%gen%table_q(m_q)>gdtrachy%gen%table_q(m_q-1)) then
                do j = 1,gdtrachy%gen%crs(itrtcrs)%nropars
                    ! compute slope
                    gdtrachy%gen%rttdef_q_slope(m_q-1,j) = (gdtrachy%gen%rttdef_q(m_q, j) - &
                        & gdtrachy%gen%rttdef_q(m_q-1, j))/(gdtrachy%gen%table_q(m_q)-gdtrachy%gen%table_q(m_q-1))
                    ! compute crossing
                    gdtrachy%gen%rttdef_q_cross(m_q-1,j) = gdtrachy%gen%rttdef_q(m_q-1, j) - &
                        & gdtrachy%gen%rttdef_q_slope(m_q-1,j) * gdtrachy%gen%table_q(m_q-1)
                end do
            else
                call SetMessage(LEVEL_ERROR, 'Trachytopes: Discharges should be monotonically' &
                    & //'increasing for discharge dependence in file ' // trim(gdtrachy%gen%md_ttdfile) &
                    & // ', Record: ' // trim(gdtrachy%gen%crs(itrtcrs)%rec132))
                error  = .true.
            end if
        end do
    end do
    !
    do itrtobs = 1, ntrtobs
        if (gdtrachy%gen%obs(itrtobs)%idx_start + 1 > gdtrachy%gen%obs(itrtobs)%idx_end) then
            call SetMessage(LEVEL_ERROR, 'Error reading trachytopes: Expected 2 '&
                & //'or more TRTDEF WATERLEVEL TRT_EQ PARAMS after -> "'//trim(gdtrachy%gen%md_ttdfile) &
                & //'": '//trim(gdtrachy%gen%obs(itrtobs)%rec132))
            error = .true.
        end if
        itttmp = gdtrachy%gen%ittdef_zs(gdtrachy%gen%obs(itrtobs)%idx_start)
        do m_zs = gdtrachy%gen%obs(itrtobs)%idx_start, gdtrachy%gen%obs(itrtobs)%idx_end
            if (gdtrachy%gen%ittdef_zs(m_zs) /= itttmp) then
               call SetMessage(LEVEL_ERROR, 'Error reading trachytopes: Trt '&
                   &//'function type should be the same for all waterlevels in  "'&
                   &//trim(gdtrachy%gen%md_ttdfile)//'": '//trim(gdtrachy%gen%obs(itrtobs)%rec132))
               error = .true.
            end if
        end do
        do m_zs = gdtrachy%gen%obs(itrtobs)%idx_start+1, gdtrachy%gen%obs(itrtobs)%idx_end
            ! check if waterlevels are monotonically increasing
            if (gdtrachy%gen%table_zs(m_zs)>gdtrachy%gen%table_zs(m_zs-1)) then
                do j = 1,gdtrachy%gen%obs(itrtobs)%nropars
                     ! compute slope
                     gdtrachy%gen%rttdef_zs_slope(m_zs-1,j) = (gdtrachy%gen%rttdef_zs(m_zs, j)-gdtrachy%gen%rttdef_zs(m_zs-1, j))/(gdtrachy%gen%table_zs(m_zs)-gdtrachy%gen%table_zs(m_zs-1))
                     ! compute crossing
                     gdtrachy%gen%rttdef_zs_cross(m_zs-1,j) = gdtrachy%gen%rttdef_zs(m_zs-1, j) - gdtrachy%gen%rttdef_zs_slope(m_zs-1,j) * gdtrachy%gen%table_zs(m_zs-1)
                end do
            else
                call SetMessage(LEVEL_ERROR, 'Trachytopes: Water levels should '&
                    &//'be monotonically increasing for waterlevel dependence in file '&
                    &// trim(gdtrachy%gen%md_ttdfile) // ', Record: ' // trim(gdtrachy%gen%obs(itrtobs)%rec132))
                error  = .true.
            end if
        end do
    end do
    !
    if (error) goto 9999
    do jdir = 1, nodir
        !
        ! Trtu/Trtv  : trachytope area file for U/V-direction
        !                            (must exist, no default)
        !
        nttaru => gdtrachy%dir(jdir)%nttaru
        ittaru => gdtrachy%dir(jdir)%ittaru
        rttaru => gdtrachy%dir(jdir)%rttaru
        rttxyz => gdtrachy%dir(jdir)%rttxyz
        md_arlfile => gdtrachy%dir(jdir)%md_arlfile
        !
        filtmp = ' '
        if (nodir .eq. 1) then
            keyw   = 'Trtl'
        else
            keyw   = 'Trt'//char(116+jdir)    ! 'Trtu' or 'Trtv'
        end if
        call prop_get(mdfile_ptr, '*',keyw,filtmp)
        !
        txtput1 = keyw
        write (msgtmp, '(a,a,a)') txtput1,': ',trim(filtmp)
        call SetMessage(LEVEL_INFO, msgtmp)

        md_arlfile = trim(filtmp)
        !
        ! keyword not found ?
        !
        if (filtmp == ' ' .and. ntrt /= 1) then
           call SetMessage(LEVEL_ERROR, 'Missing value (or the keyword): '// keyw // ' in file')
           error = .true.
           goto 9999
        endif
        !
        ! read file
        !
        if (filtmp == ' ') then
           i = 0
           do m = 1,mmax
              do n = 1,nmax
                 i = i+1
                 ittaru(i,1) = n
                 ittaru(i,2) = m
                 ittaru(i,3) = ittdef(1,1)
                 ittaru(i,4) = ittaru(i,1)+nmaxddb*(ittaru(i,2)-1 + ddbval) + ddbval
                 rttaru(i) = 1.0_fp
              enddo
           enddo
        else
           call rdttar(filtmp    ,lundia    ,error     ,nttaru    ,ittaru    , &
                     & rttaru    ,rttxyz    ,nmax      ,ddbval)
           if (error) goto 9999
        endif
    end do
    !
    do jdir = 1, nodir
        !
        ! TrtClu/TrTClv : Calibration trachytopes in U-direction
        ! If not found fill array with 1.0
        !
        rgcalu => gdtrachy%dir(jdir)%rgcalu
        !
        filtmp = ' '
        if (nodir .eq. 1) then
            keyw   = 'TrtCll'
        else
            keyw   = 'TrtCl'//char(116+jdir)    ! 'TrtClu' or 'TrtClv'
        end if
        call prop_get(mdfile_ptr, '*',keyw,filtmp)
        !
        txtput1 = keyw
        write (msgtmp, '(a,a,a)') txtput1,': ',trim(filtmp)
        call SetMessage(LEVEL_INFO, msgtmp)
        !
        ! keyword not found ?
        !
        if (filtmp /= ' ') then
           !
           ! test file existence
           !
           lfile = index(filtmp, ' ')
           if (lfile==0) lfile = 13
           lfile = lfile - 1
           if (.not.exifil(filtmp, lundia)) then
              !
              ! file does not exist !!
              !
              call SetMessage(LEVEL_ERROR, 'The specified file ' // trim(filtmp) // ' does not exist ')
              error = .true.
              goto 9999
           endif
           !
           ! Calibration data has been specified
           ! Use routine that also read the depth file to read the data
           !
           fmttmp = 'FORMATTED'
           call depfil(lundia    ,error     ,filtmp    ,fmttmp    , &
                     & rgcalu    ,1         ,1         ,griddim)
           if (error) goto 9999
        else
           !
           ! Fill with 1.0
           !
           do nm = 1,nmmax
               rgcalu(nm) = 1.0_fp
           enddo
        endif
    end do
    !
    !  Minimum water depth in roughness computations
    !
    trtminh = dryflc
    keyw    = 'TrtMnH'
    call prop_get(mdfile_ptr, '*', keyw, trtminh)
    !
    txtput1 = keyw
    write (msgtmp, '(a,a,f7.3,a)') txtput1,': ',trtminh,' m'
    call SetMessage(LEVEL_INFO, msgtmp)
    !
    !  Area averaging method:
    !  1: Nikuradse k based
    !  2: Chezy C based (parallel and serial)
    !
    keyw = 'TrtMth'
    call prop_get(mdfile_ptr, '*', keyw, iarea_avg)
    !
    txtput1 = keyw
    if (iarea_avg<1 .or. iarea_avg>2) then
       call SetMessage(LEVEL_ERROR, 'Trachytopes: ' // 'TrtMth should be 1 or 2')
       error = .true.
       goto 9999
    endif
    write (msgtmp, '(a,a,i3)') txtput1,': ',iarea_avg
    !
    if (iarea_avg == 2) then
       !
       !  Alfa factor for serial and parallel averaging of area roughnesses
       !
       keyw = 'TrtAsr'
       call prop_get(mdfile_ptr, '*', keyw, alf_area_ser)
       !
       txtput1 = keyw
       write (msgtmp, '(a,a,f7.3)') txtput1,': ',alf_area_ser
       call SetMessage(LEVEL_INFO, msgtmp)
       !
    endif
    !
    max_cl = 8
    keyw    = 'TrtMxR'
    call prop_get(mdfile_ptr, '*', keyw, max_cl)
    if (istat==0) then
       if (istat==0) allocate(gdtrachy%gen%fraccu_list(max_cl)  , stat = istat)
       if (istat==0) allocate(gdtrachy%gen%itrt_list(max_cl)    , stat = istat)
       itrt_list       => gdtrachy%gen%itrt_list
       fraccu_list     => gdtrachy%gen%fraccu_list
       !
       if (istat/=0) then
          call SetMessage(LEVEL_ERROR, 'Trachytopes: ' // 'RDTRT: memory alloc error (step 2)')
          error = .true.
          goto 9999
       endif
    endif

    !
    call SetMessage(LEVEL_INFO, 'End    of trachytopes input')
    !
    ! transfer to local subdomain
    !
    mll = mmax
    nll = nmaxus
    do jdir = 1, nodir
        nttaru => gdtrachy%dir(jdir)%nttaru
        ittaru => gdtrachy%dir(jdir)%ittaru
        rttaru => gdtrachy%dir(jdir)%rttaru
        if (parll .and. nttaru > 0) then
           do it = 1,nttaru
              n1 = ittaru(it,1) - nfg + 1
              m1 = ittaru(it,2) - mfg + 1
              ! check if observation point is inside or outside subdomain
              if ( m1>=1 .and. n1>=1 .and. m1<=mll .and. n1<=nll ) then    ! to do: check if this works for link input (WO)
                 !
                 ! point is inside subdomain
                 !
                 ittaru(it,1) = n1
                 ittaru(it,2) = m1
                 ittaru(it,4) = ittaru(it,1)+nmaxddb*(ittaru(it,2)-1 + ddbval) + ddbval
              else
                 ittaru(it,1) = -1
                 ittaru(it,2) = -1
                 ittaru(it,3) = -1
                 rttaru(it) = -1.0
                 ittaru(it, 4) = TRACHY_NOT_IN_SUBDOMAIN
              endif
           enddo
        endif
    end do
    !
 9999 continue
end subroutine rdtrt


subroutine rdttar(filnam    ,lundia    ,error     ,nttaru    ,ittaru    , &
                & rttaru    ,rttxyz    ,nmax      ,ddbval)
!!--description-----------------------------------------------------------------
! Reads trachytope data from the
! trachytopes area file in U-direction.
! Routine is called for U/V-direction
! respectively.
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use trachytopes_data_module
    use system_utils, only: exifil
    use MessageHandling
    !
    implicit none
    !
!
! Local parameters
!
    integer, parameter :: MAXFLD = 12
!
! Global variables
!
    integer                              :: lundia
    integer                              :: nttaru
    integer       , dimension(nttaru, 4) :: ittaru
    integer                              :: ddbval
    logical                              :: error
    real(fp)      , dimension(nttaru)    :: rttaru
    real(fp)      , dimension(nttaru, 3) :: rttxyz
    character(256)                       :: filnam

!
! Local variables
!
    integer                                     :: i
    integer                                     :: dimctr
    integer                                     :: iblbeg
    integer                                     :: iocond
    integer                                     :: mcurec
    integer                                     :: mttaru
    integer                                     :: nblcks
    integer                                     :: nmax     !not wanted ?
    integer                                     :: nmaxddb
    integer                                     :: nt, mt, nmt
    integer                                     :: nrflds
    integer       , dimension(4)                :: nmpblk
    integer       , dimension(MAXFLD)           :: ifield
    integer       , dimension(MAXFLD)           :: itype
    integer       , dimension(MAXFLD)           :: lenchr
    integer                                     :: luntmp2
    logical                                     :: leql
    logical                                     :: lfirst
    logical                                     :: lokay
    logical                                     :: lcomment
    logical                                     :: lprblk
    real(fp)      , dimension(MAXFLD)           :: rfield
    character(30) , dimension(MAXFLD)           :: cfield
    character(132)                              :: rec132
!
!! executable statements -------------------------------------------------------
    !
    ! initialize
    !
    lfirst = .true.
    do i = 1, 4
       nmpblk(i) = 0
    enddo
    !
    ! update nmaxddb
    !
    nmaxddb = nmax + 2*ddbval
    !
    ! test file existence
    !
    if (.not.exifil(filnam, lundia)) then
       !
       ! file does not exist !!
       !
       call SetMessage(LEVEL_ERROR, 'The specified file ' // trim(filnam) // ' does not exist ')
       error = .true.
       goto 9999
    endif
    !
    ! open file
    !
    open (newunit=luntmp2, file = trim(filnam), form = 'formatted', iostat = iocond,  &
        & status = 'old')
    if (iocond/=0) then
       call SetMessage(LEVEL_ERROR, 'Error while opening file ' // trim(filnam))
       error = .true.
       goto 9999
    endif
    !
    ! freeformatted file
    !       read record and count number of useful areas
    !       till end of file
    !
    mttaru = 0
    lprblk = .false.
    mcurec = 0
    iblbeg = -1
    nblcks = 0
    ! -->
    !
    ! read line
    !
  210 continue
    read (luntmp2, '(a)', iostat = iocond) rec132
    if (iocond==0) then
       mcurec = mcurec + 1
    else
       !
       ! End-of-file ?
       !
       if (iocond<0) then
          !
          ! Check on pending blocks
          !
          if (iblbeg>=0) then
             call expblk(lundia    ,ittaru    ,rttaru    ,nttaru    ,iblbeg    , &
                       & nblcks    ,nmpblk    ,error     ,nmax      ,ddbval)
          endif
          ! <--
          !
          ! Exit
          !
          !
          ! close file
          !
          close (luntmp2)
          goto 9999
       endif
       !
       ! Reading error
       !
       rec132 = ' '
       write (rec132, '(i12)') mcurec + 1
       call SetMessage(LEVEL_ERROR, 'Read error from file: ' // trim(filnam) // ', Record: ' // trim(rec132))
       error = .true.
       close (luntmp2)
       goto 9999
    endif
    !
    ! Interpret line ...
    !
    call interpret_record( rec132, nrflds, itype, ifield, rfield, cfield, lenchr, lcomment, lokay )

    if ( lcomment ) then
        goto 210
    endif

    !
    if (.not.lokay) then
       !
       ! Cannot interpret line
       !
       rec132 = ' '
       write (rec132, '(i12)') mcurec
       call SetMessage(LEVEL_ERROR, 'Read error from file: ' // trim(filnam) // ', Record: ' // trim(rec132))
       error = .true.
       close (luntmp2)
       goto 9999
    endif
    !
    ! Assign arl, aru/arv input to data structures
    !
    if (nrflds==3) then
       ! Unstructured input based on net link number
       !
       ! Handle pending block if applicable
       !
       if (lprblk) then
          !
          ! Expand block data
          !
          call expblk(lundia    ,ittaru    ,rttaru    ,nttaru    ,iblbeg    , &
                    & nblcks    ,nmpblk    ,error     ,nmax      ,ddbval)
          if (error) then
             close (luntmp2)
             goto 9999
          endif
          !
          ! Reset block administration
          !
          iblbeg = -1
          nblcks = 0
          !
          ! Block separator
          !
          mttaru = mttaru + 1
          if (mttaru>nttaru) then
             call SetMessage(LEVEL_ERROR, 'Trachytopes: Number of area entries ' //                &
                  & ' in file ' // trim(filnam) // ' > array size.')
             error = .true.
             close (luntmp2)
             goto 9999
          endif
          !
          ! Store block separator values
          !
          ittaru(mttaru, 1) = -1
          ittaru(mttaru, 2) = -1
          ittaru(mttaru, 3) = -1
          rttaru(mttaru) = -1.0
          ittaru(mttaru, 4) = TRACHY_NOT_IN_SUBDOMAIN
          !
          lprblk = .false.
       endif
       !
       ! Increment MTTARU for one point
       !
       mttaru = mttaru + 1
       if (mttaru>nttaru) then
          call SetMessage(LEVEL_ERROR, 'Trachytopes: Number of area entries ' //                &
           & ' in file ' // trim(filnam) // ' > array size.')
          error = .true.
          close (luntmp2)
          goto 9999
       endif
       !
       ! Store data read into arrays
       !
       ittaru(mttaru, 1) = -22222 !ifield(2) !ifield(1)
       ittaru(mttaru, 2) = -22222 !1         !ifield(2)
       ittaru(mttaru, 3) = ifield(2)
       !
       ! (ifield(1)is nm input for single domain simulation)
       ! must be transformed to nm in dd sense.
       !
       nmt = ifield(1)                                                   ! local nm coordinate not considering dd.
       mt = floor(real(nmt-1)/real(nmax)) + 1                            ! local m coordinate
       nt = nmt - nmax * (mt - 1 )                                       ! local n coordinate
       ittaru(mttaru, 4) = nt + nmaxddb * (mt - 1 + ddbval ) + ddbval    ! local nm coordinate considering dd.
       if (itype(3)==2) then
          rttaru(mttaru) = rfield(3)
       else
          rttaru(mttaru) = real(ifield(3),fp)
       endif
    elseif (nrflds==4) then
       !
       ! Handle pending block if applicable
       !
       if (lprblk) then
          !
          ! Expand block data
          !
          call expblk(lundia    ,ittaru    ,rttaru    ,nttaru    ,iblbeg    , &
                    & nblcks    ,nmpblk    ,error     ,nmax      ,ddbval)
          if (error) then
             close (luntmp2)
             goto 9999
          endif
          !
          ! Reset block administration
          !
          iblbeg = -1
          nblcks = 0
          !
          ! Block separator
          !
          mttaru = mttaru + 1
          if (mttaru>nttaru) then
             call SetMessage(LEVEL_ERROR, 'Trachytopes: Number of area entries ' //                &
             & ' in file ' // trim(filnam) // ' > array size.')
             error = .true.
             close (luntmp2)
             goto 9999
          endif
          !
          ! Store block separator values
          !
          ittaru(mttaru, 1) = -1
          ittaru(mttaru, 2) = -1
          ittaru(mttaru, 3) = -1
          rttaru(mttaru) = -1.0
          ittaru(mttaru, 4) = TRACHY_NOT_IN_SUBDOMAIN
          !
          lprblk = .false.
       endif
       !
       ! Increment MTTARU for one point
       !
       mttaru = mttaru + 1
       if (mttaru>nttaru) then
          call SetMessage(LEVEL_ERROR, 'Trachytopes: Number of area entries ' //                &
           & ' in file ' // trim(filnam) // ' > array size.')
          error = .true.
          close (luntmp2)
          goto 9999
       endif
       !
       ! Store data read into arrays
       !
       ittaru(mttaru, 1) = ifield(1)
       ittaru(mttaru, 2) = ifield(2)
       ittaru(mttaru, 3) = ifield(3)
       ittaru(mttaru,4) = ittaru(mttaru,1)+nmaxddb*(ittaru(mttaru,2)-1 + ddbval) + ddbval
       if (itype(4)==2) then
          rttaru(mttaru) = rfield(4)
       else
          rttaru(mttaru) = real(ifield(4),fp)
       endif
    elseif (nrflds==5) then
       ! Unstructured input based on x,y,z, of velocity point of net link
       !
       ! Handle pending block if applicable
       !
       if (lprblk) then
          !
          ! Expand block data
          !
          call expblk(lundia    ,ittaru    ,rttaru    ,nttaru    ,iblbeg    , &
                    & nblcks    ,nmpblk    ,error     ,nmax      ,ddbval)
          if (error) then
             close (luntmp2)
             goto 9999
          endif
          !
          ! Reset block administration
          !
          iblbeg = -1
          nblcks = 0
          !
          ! Block separator
          !
          mttaru = mttaru + 1
          if (mttaru>nttaru) then
             call SetMessage(LEVEL_ERROR, 'Trachytopes: Number of area entries ' //                &
               & ' in file ' // trim(filnam) // ' > array size.')
             error = .true.
             close (luntmp2)
             goto 9999
          endif
          !
          ! Store block separator values
          !
          ittaru(mttaru, 1) = -1
          ittaru(mttaru, 2) = -1
          ittaru(mttaru, 3) = -1
          rttaru(mttaru) = -1.0
          ittaru(mttaru, 4) = TRACHY_NOT_IN_SUBDOMAIN
          !
          lprblk = .false.
       endif
       !
       ! Increment MTTARU for one point
       !
       mttaru = mttaru + 1
       if (mttaru>nttaru) then
          call SetMessage(LEVEL_ERROR, 'Trachytopes: Number of area entries ' //                &
              & ' in file ' // trim(filnam) // ' > array size.')
          error = .true.
          close (luntmp2)
          goto 9999
       endif
       !
       ! Store data read into arrays
       !
       ittaru(mttaru, 1) = -22222 !ifield(2) !ifield(1)
       ittaru(mttaru, 2) = -22222 !1         !ifield(2)
       ittaru(mttaru, 3) = ifield(4)
       !
       ! (ifield(1)is nm input for single domain simulation)
       ! must be transformed to nm in dd sense.
       !
       ! Store x,y,z locations in rttxyz
       do dimctr = 1,3
          if (itype(dimctr)==2) then
             rttxyz(mttaru,dimctr) = rfield(dimctr)
          else
             rttxyz(mttaru,dimctr) = real(ifield(dimctr),fp)
          endif
       enddo
       !
       ittaru(mttaru, 4) = TRACHY_MISSING_VALUE                          ! set to missing value, and replace from unstruc for now.
                                                                         ! In future: connect to kd-tree from within trachytopes module?
       if (itype(5)==2) then
          rttaru(mttaru) = rfield(5)
       else
          rttaru(mttaru) = real(ifield(5),fp)
       endif
    else
       !
       ! Reserve space for block separators
       ! Not for first record!!
       !
       if (.not.lfirst) then
          if (lprblk) then
             leql = .true.
             do i = 1, 4
                leql = leql .and. (ifield(i)==nmpblk(i))
             enddo
             if (.not.leql) then
                !
                ! New block, expand pending one first
                !
                call expblk(lundia    ,ittaru    ,rttaru    ,nttaru    ,iblbeg    , &
                          & nblcks    ,nmpblk    ,error     ,nmax      ,ddbval)
                if (error) then
                   close (luntmp2)
                   goto 9999
                endif
                !
                ! Reset block administration
                !
                iblbeg = -1
                nblcks = 0
                !
                ! Block separator
                !
                mttaru = mttaru + 1
                if (mttaru>nttaru) then
                   call SetMessage(LEVEL_ERROR, 'Trachytopes: Number of area entries ' //                &
                    & ' in file ' // trim(filnam) // ' > array size.')
                   error = .true.
                   close (luntmp2)
                   goto 9999
                endif
                !
                ! Store block separator values
                !
                ittaru(mttaru, 1) = -1
                ittaru(mttaru, 2) = -1
                ittaru(mttaru, 3) = -1
                rttaru(mttaru) = -1.0
                ittaru(mttaru,4)  = TRACHY_NOT_IN_SUBDOMAIN
             endif
          else
             !
             ! Block separator
             !
             mttaru = mttaru + 1
             if (mttaru>nttaru) then
                call SetMessage(LEVEL_ERROR, 'Trachytopes: Number of area entries ' //                &
                     & ' in file ' // trim(filnam) // ' > array size.')
                error = .true.
                close (luntmp2)
                goto 9999
             endif
             !
             ! Store block separator values
             !
             ittaru(mttaru, 1) = -1
             ittaru(mttaru, 2) = -1
             ittaru(mttaru, 3) = -1
             rttaru(mttaru)    = -1.0
             ittaru(mttaru, 4) = TRACHY_NOT_IN_SUBDOMAIN
          endif
       endif
       !
       ! Block administration
       !
       if (iblbeg<0) then
          iblbeg = mttaru
       endif
       nblcks = nblcks + 1
       !
       ! Increment MTTARU for block
       !
       mttaru = mttaru + (abs(ifield(1) - ifield(3)) + 1)                       &
              & *(abs(ifield(2) - ifield(4)) + 1)
       if (mttaru>nttaru) then
          call SetMessage(LEVEL_ERROR, 'rachytopes: Number of area entries ' //                &
               & ' in file ' // trim(filnam) // ' > array size.')
          error = .true.
          close (luntmp2)
          goto 9999
       endif
       !
       ! Store N1, M1 block data in arrays
       !
       ittaru(iblbeg + nblcks, 1) = min(ifield(1), ifield(3))
       ittaru(iblbeg + nblcks, 2) = min(ifield(2), ifield(4))
       ittaru(iblbeg + nblcks, 3) = ifield(5)
       ittaru(iblbeg + nblcks, 4) = ittaru(iblbeg + nblcks,1)+nmaxddb*(ittaru(iblbeg + nblcks,2)-1 + ddbval) + ddbval
       if (itype(6)==2) then
          rttaru(iblbeg + nblcks) = rfield(6)
       else
          rttaru(iblbeg + nblcks) = real(ifield(6),fp)
       endif
       !
       ! Set previous block on
       !
       lprblk = .true.
       !
       ! Save current values
       !
       do i = 1, 4
          nmpblk(i) = ifield(i)
       enddo
    endif
    !
    ! One record read
    !
    lfirst = .false.
    goto 210
 9999 continue
end subroutine rdttar


subroutine expblk(lundia    ,ittaru    ,rttaru    ,nttaru    ,iblbeg    , &
                & nblcks    ,nmpblk    ,error     ,nmax      ,ddbval)
!!--description-----------------------------------------------------------------
! Expands block trachytope data to
! N, M data
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use MessageHandling
    !use globaldata
    !
    implicit none
    !
    ! type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                       , intent(in)  :: iblbeg
    integer                                     :: lundia
    integer                                     :: ddbval
    integer                       , intent(in)  :: nblcks
    integer                       , intent(in)  :: nttaru
    integer                       , intent(in)  :: nmax
    integer , dimension(4)        , intent(in)  :: nmpblk
    integer , dimension(nttaru, 4)              :: ittaru
    logical                       , intent(out) :: error
    real(fp), dimension(nttaru)                 :: rttaru

!
! Local variables
!
    integer :: ib
    integer :: icur
    integer :: im
    integer :: in
    integer :: m1
    integer :: m2
    integer :: n1
    integer :: n2
    integer :: nmaxddb
!
!! executable statements -------------------------------------------------------
!
    ! Determine N.M data
    !
    n1 = min(nmpblk(1), nmpblk(3))
    m1 = min(nmpblk(2), nmpblk(4))
    n2 = max(nmpblk(1), nmpblk(3))
    m2 = max(nmpblk(2), nmpblk(4))
    !
    ! Set nmaxdbb (account for spoke cells)
    !
    nmaxddb = nmax + 2*ddbval
    !
    ! Set the current index
    !
    icur = iblbeg + nblcks
    !
    ! The expansion
    !
    ! First finalize M1
    !
    do in = n1 + 1, n2
       do ib = 1, nblcks
          !
          ! Increase index and check it.
          !
          icur = icur + 1
          if (icur>nttaru) then
             call SetMessage(LEVEL_ERROR, 'Trachytopes: Out of array size while ' //               &
                & 'expanding trachytope data')
             error = .true.
             goto 9999
          endif
          !
          ! Store the data
          !
          ittaru(icur, 1) = in
          ittaru(icur, 2) = m1
          ittaru(icur, 3) = ittaru(iblbeg + ib, 3)
          rttaru(icur)    = rttaru(iblbeg + ib)
          ittaru(icur,4) = ittaru(icur,1) + nmaxddb*(ittaru(icur,2)-1 + ddbval) + ddbval
       enddo
    enddo
    !
    ! The rest from M1+1 to M2
    !
    do im = m1 + 1, m2
       do in = n1, n2
          do ib = 1, nblcks
             !
             ! Increase index and check it.
             !
             icur = icur + 1
             if (icur>nttaru) then
                call SetMessage(LEVEL_ERROR, 'Trachytopes: Out of array size while ' //               &
                   & 'expanding trachytope data')
                error = .true.
                goto 9999
             endif
             !
             ! Store the data
             !
             ittaru(icur, 1) = in
             ittaru(icur, 2) = im
             ittaru(icur, 3) = ittaru(iblbeg + ib, 3)
             rttaru(icur)    = rttaru(iblbeg + ib)
             ittaru(icur, 4) = ittaru(icur,1) + nmaxddb * (ittaru(icur,2)-1 + ddbval) + ddbval
          enddo
       enddo
    enddo
    !
 9999 continue
end subroutine expblk

subroutine dimtrt(lundia    ,error     ,gdtrachy   ,mdfile_ptr , &
                & griddim   )
!!--description-----------------------------------------------------------------
! Reads the dimensions ntrt, nttaru, nttarv
! Initializes nroupa, nodir
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use trachytopes_data_module
    use grid_dimens_module, only: griddimtype
    use dfparall
    use system_utils, only: exifil
    use properties   ! includes tree_structures
    use tree_structures
    use MessageHandling

    implicit none
    !
!    type(globdat)    , target               :: gdp
    type(trachy_type), target                :: gdtrachy
    type(tree_data)  , pointer , intent(in)  :: mdfile_ptr
    type(griddimtype), target  , intent(in)  :: griddim
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer  , pointer :: nroupa
    integer  , pointer :: nodir
    integer  , pointer :: nttaru
    integer  , pointer :: ntrt
    integer  , pointer :: ntrtnrm
    integer  , pointer :: ntrtcrs
    integer  , pointer :: ntrtobs
    integer  , pointer :: n_q
    integer  , pointer :: n_zs
    integer  , pointer :: nmmax
!
! Local parameters
!
    integer, parameter :: MAXFLD = 12
!
! Global variables
!
    integer             :: lundia
    logical             :: error
!
! Local variables
!
    integer                                       :: ibeg
    integer                                       :: iend
    integer                                       :: iocond
    integer                                       :: istr
    integer                                       :: jdir
    integer                                       :: luntmp
    integer                                       :: nrflds
    integer        , dimension(MAXFLD)            :: ifield
    integer        , dimension(MAXFLD)            :: itype
    integer        , dimension(MAXFLD)            :: lenchr
    integer                                       :: prev_trt_no    ! previous trt_no for discharge/waterlevel dependent trachytopes
    integer                                       :: prev_trt_type  ! previous type of discharge/waterlevel dependent trachytopes
    logical                                       :: lftrto
    real(fp)       , dimension(MAXFLD)            :: rfield
    character(30)  , dimension(MAXFLD)            :: cfield
    character(132)                                :: rec132
    character(20)                                 :: chulp
    character(256)                                :: filtmp
    character(6)                                  :: keyw

!
!! executable statements -------------------------------------------------------
!
    ntrt          => gdtrachy%gen%ntrt
    ntrtnrm       => gdtrachy%gen%ntrtnrm
    ntrtcrs       => gdtrachy%gen%ntrtcrs
    ntrtobs       => gdtrachy%gen%ntrtobs
    nroupa        => gdtrachy%gen%nroupa
    nodir         => gdtrachy%gen%nodir
    nmmax         => griddim%nmmax
    n_q           => gdtrachy%gen%n_q
    n_zs          => gdtrachy%gen%n_zs

    !
    ! Initialize array dimensions for default empty settings
    !
    lftrto = .false.
    ntrt    = 0
    ntrtnrm = 0
    ntrtcrs = 0
    ntrtobs = 0
    n_q     = 0
    n_zs    = 0
    nroupa = 12
    do jdir = 1,nodir
       nttaru        => gdtrachy%dir(jdir)%nttaru
       nttaru = 0
    end do
    prev_trt_no   = TRACHY_UNDEFINED
    prev_trt_type = TRACHY_UNDEFINED
    !
    ! Read value of Trtrou, default NO
    !
    chulp = 'N'
    call prop_get(mdfile_ptr, '*','Trtrou',chulp)
    !
    ! set LFTRTO to TRUE if CHULP = Y/y
    !
    call small(chulp     ,1         )
    if (chulp=='y') lftrto = .true.
    !
    ! if Trtrou turned out to be NO, don't look any further.
    !
    if (.not.lftrto) goto 9999
    !
    !
    ! Trtdef: trachytope definition file (must exist, no default)
    !
    filtmp = ' '
    keyw   = 'Trtdef'
    call prop_get(mdfile_ptr, '*',keyw,filtmp)
    !
    ! keyword not found ?
    !
    if (filtmp == ' ') then
       call SetMessage(LEVEL_ERROR, 'Missing value (or the keyword): '// keyw //' in file')
       error = .true.
       goto 9999
    endif
    !
    ! test file existence
    !
    if (.not.exifil(filtmp, lundia)) then
       !
       ! file does not exist !!
       !
       call SetMessage(LEVEL_ERROR, 'The specified file ' // trim(filtmp) // ' does not exist ')
       error = .true.
       goto 9999
    endif
    !
    ! open trachytope definition file
    !
    open (newunit=luntmp, file = trim(filtmp), form = 'formatted', iostat = iocond,  &
        & status = 'old')
    if (iocond/=0) then
       call SetMessage(LEVEL_ERROR, 'Error while opening file '// trim(filtmp))
       error = .true.
       goto 9999
    endif
    !
    !
    ! freeformatted file
    !           read record and add 1 to NTRT till end of file
    !
    ! -->
    !
    ! read line
    !
  110 continue
    read (luntmp, '(a)', iostat = iocond) rec132
    if (iocond/=0) then
       !
       ! End-of-file ?
       !
       if (iocond<0) goto 199
       !
       ! Reading error
       !
       call SetMessage(LEVEL_ERROR, 'Read error from file: '// trim(filtmp))
       error = .true.
       ! <--
       !
       ! close file
       !
  199  continue
       close (luntmp)
       if (error) goto 9999
       do jdir = 1,nodir
           nttaru        => gdtrachy%dir(jdir)%nttaru
           !
           !
           ! Trtu/Trtv  : trachytope area file for U/V-direction
           !              (must exist, no default)
           !
           filtmp = ' '
           if (nodir .eq. 1) then
               keyw   = 'Trtl'
           else
               keyw = 'Trt'//char(116+jdir)    ! 'Trtu' or 'Trtv'
           end if
           call prop_get(mdfile_ptr, '*',keyw,filtmp)
           !
           ! keyword not found ?
           !
           if (filtmp == ' ' .and. ntrt /= 1) then
              call SetMessage(LEVEL_ERROR, 'Missing value (or the keyword): '// keyw //' in file')
              error = .true.
              goto 9999
           endif
           !
           ! read file and determine value of NTTARU
           !
           if (filtmp == ' ') then
              ! uniform
              nttaru = nmmax
           else
              call dittar(filtmp    ,lundia    ,error     ,nttaru    )
              if (error) goto 9999
           endif
       end do
       goto 9999
    endif
    !
    ! Interpret line ...
    !
    !
    ! Comment line
    !
    if ((rec132(1:1)=='*') .or. (rec132(1:1)=='#')) goto 110
    ibeg = 1
    iend = 132
    ! Comment at end of line
    !
    ! loop over rec132 and find #
    !
    comment_at_end_of_line_loop: &
        do istr = ibeg, iend
            if (rec132(istr:istr) == '#') then
                iend = istr
                exit comment_at_end_of_line_loop
            end if
        end do &
    comment_at_end_of_line_loop
        call scannr(rec132    ,ibeg      ,iend      ,nrflds    ,itype     , &
              & ifield    ,rfield    ,cfield    ,lenchr    ,MAXFLD    , &
              & .true.    ,.true.    ,.true.    )
    !
    ! When sub-fields are found, reserve space
    !
    if (nrflds>0) then
        if (itype(1)==1 .and. itype(2)==1 .and. ifield(1) /= prev_trt_no) then
            ntrt    = ntrt + 1
            ntrtnrm = ntrtnrm + 1
            prev_trt_no   = TRACHY_UNDEFINED
            prev_trt_type = TRACHY_UNDEFINED
        elseif (itype(1)==1 .and. itype(2)==3) then
            if (cfield(2)(1:9) == 'DISCHARGE') then        ! TO DO: use strcmpi?
                ntrtcrs = ntrtcrs + 1
                ntrt    = ntrt + 1
                prev_trt_no   = ifield(1)
                prev_trt_type = TRACHY_DISCHARGE_TYPE
            elseif (cfield(2)(1:10) == 'WATERLEVEL') then  ! TO DO: use strcmpi?
                ntrtobs = ntrtobs + 1
                ntrt    = ntrt + 1
                prev_trt_no = ifield(1)
                prev_trt_type = TRACHY_WATERLEVEL_TYPE
            else
                ! do nothing
            endif
        elseif (itype(1)==1 .and. itype(2)/=3 .and. itype(3)==1) then
            if (ifield(1) == prev_trt_no) then
                ! add to table for discharge/water level depdendent data
                if (prev_trt_type == TRACHY_WATERLEVEL_TYPE) then
                    ! count values to table for waterlevel dependent trachytopes
                    n_zs = n_zs + 1
                elseif (prev_trt_type == TRACHY_DISCHARGE_TYPE) then
                    ! count values to table for discharge dependent trachytopes
                    n_q = n_q + 1
                else
                   call SetMessage(LEVEL_ERROR, 'Unknown read error #1 in rdtrt.f90')
                   error = .true.
                end if
            else
                call SetMessage(LEVEL_ERROR, 'Unknown read error #2 in rdtrt.f90')
                error = .true.
            end if
        endif
    endif
    goto 110
 9999 continue
end subroutine dimtrt

subroutine dittar(filnam    ,lundia    ,error     ,nttaru    )
!!--description-----------------------------------------------------------------
!
! Determines the dimension nttaru from the
! trachytopes area file in U-direction.
! Routine is called for U/V-direction
! respectively.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!    use globaldata
    use system_utils, only: exifil
    use MessageHandling
    !
    implicit none
    !
!
! Local parameters
!
    integer, parameter :: MAXFLD = 12
!
! Global variables
!
    integer                     :: lundia
    integer                     :: nttaru
    logical        , intent(out):: error
    character(256)              :: filnam
!
! Local variables
!
    integer                          :: i
    integer                          :: iocond
    integer                          :: luntmp
    integer                          :: mcurec
    integer                          :: nrflds
    integer, dimension(4)            :: nmpblk
    integer, dimension(MAXFLD)       :: ifield
    integer, dimension(MAXFLD)       :: itype
    integer, dimension(MAXFLD)       :: lenchr
    logical                          :: leql
    logical                          :: lfirst
    logical                          :: lokay
    logical                          :: lcomment
    logical                          :: lprblk
    real(fp), dimension(MAXFLD)      :: rfield
    character(30), dimension(MAXFLD) :: cfield
    character(132)                   :: rec132
!
!! executable statements -------------------------------------------------------
!
    !
    ! initialize ...
    !
    lfirst = .true.
    do i = 1, 4
       nmpblk(i) = 0
    enddo
    !
    ! test file existence
    !
    if (.not.exifil(filnam, lundia)) then
       !
       ! file does not exist !!
       !
       call SetMessage(LEVEL_ERROR, 'The specified file ' // trim(filnam) // ' does not exist ')
       error = .true.
       goto 9999
    endif
    !
    ! open file
    !
    open (newunit=luntmp, file = trim(filnam), form = 'formatted', iostat = iocond,  &
        & status = 'old')
    if (iocond/=0) then
       call SetMessage(LEVEL_ERROR, 'Error while opening file '// trim(filnam))
       error = .true.
       goto 9999
    endif
    !
    ! freeformatted file
    !       read record and count number of useful areas
    !       till end of file
    !
    nttaru = 0
    lprblk = .false.
    mcurec = 0
    ! -->
    !
    ! read line
    !
  210 continue
    read (luntmp, '(a)', iostat = iocond) rec132
    if (iocond==0) then
       mcurec = mcurec + 1
    else
       !
       ! End-of-file ?
       !
       if (iocond<0) then
          ! <--
          !
          ! close file
          !
          close (luntmp)
          goto 9999
       endif
       !
       ! Reading error
       !
       error = .true.
       rec132 = ' '
       write (rec132, '(i12)') mcurec + 1
       call SetMessage(LEVEL_ERROR, 'Read error from file: '// trim(filnam) // ', Record: ' // trim(rec132))
       close (luntmp)
       goto 9999
    endif
    !
    ! Interpret line ...
    !
    call interpret_record( rec132, nrflds, itype, ifield, rfield, cfield, lenchr, lcomment, lokay )

    if ( lcomment ) then
        goto 210
    endif

    if (.not.lokay) then
       !
       ! Cannot interpret line
       !
       error = .true.
       rec132 = ' '
       write (rec132, '(i12)') mcurec
       call SetMessage(LEVEL_ERROR, 'Read error from file: '// trim(filnam) // ', Record: ' // trim(rec132))
       close (luntmp)
       goto 9999
    endif
    if (nrflds==3) then
       !
       ! Reserve space for block separators
       !
       if (lprblk) then
          nttaru = nttaru + 1
          lprblk = .false.
       endif
       !
       ! Increment NTTARU for one point
       !
       nttaru = nttaru + 1
    elseif (nrflds==4) then
       !
       ! Reserve space for block separators
       !
       if (lprblk) then
          nttaru = nttaru + 1
          lprblk = .false.
       endif
       !
       ! Increment NTTARU for one point
       !
       nttaru = nttaru + 1
    elseif (nrflds==5) then
       !
       ! Reserve space for block separators
       !
       if (lprblk) then
          nttaru = nttaru + 1
          lprblk = .false.
       endif
       !
       ! Increment NTTARU for one point
       !
       nttaru = nttaru + 1
    else ! (e.g. nrflds==6)
       !
       ! Reserve space for block separators
       !           Not for first record!!
       !
       if (.not.lfirst) then
          if (lprblk) then
             leql = .true.
             do i = 1, 4
                leql = leql .and. (ifield(i)==nmpblk(i))
             enddo
             if (.not.leql) nttaru = nttaru + 1
          else
             nttaru = nttaru + 1
          endif
       endif
       !
       !
       ! Increment NTTARU for block
       !
       nttaru = nttaru + (abs(ifield(1) - ifield(3)) + 1)                       &
              & *(abs(ifield(2) - ifield(4)) + 1)
       !
       ! Set previous block on
       !
       lprblk = .true.
       !
       ! Save current values
       !
       do i = 1, 4
          nmpblk(i) = ifield(i)
       enddo
    endif
    !
    ! One record read
    !
    lfirst = .false.
    goto 210
 9999 continue
end subroutine dittar

subroutine interpret_record( rec132, nrflds, itype, ifield, rfield, cfield, lenchr, lcomment, lokay )
    character(len=*), intent(in)    :: rec132
    integer, intent(out)            :: nrflds
    integer, intent(out)            :: itype(:)
    logical, intent(out)            :: lcomment
    logical, intent(out)            :: lokay

    ! Note: inout, then we do not have to bother about setting them
    integer, intent(inout)          :: ifield(:)
    real(fp), intent(inout)         :: rfield(:)
    character(len=*), intent(inout) :: cfield(:)
    integer, intent(inout)          :: lenchr(:)

    integer                      :: ibeg, iend

    nrflds   = 0
    itype    = -999
    lokay    = .true.
    lcomment = .false.

    !
    ! Comment line
    !
    if ((rec132(1:1)=='*') .or. (rec132(1:1)=='#')) then
        lcomment = .true.
        return
    endif

    !
    ! Check for five integers: a block
    !
    ibeg = 1
    iend = 132
    call scannr(rec132    ,ibeg      ,iend      ,nrflds    ,itype       , &
              & ifield    ,rfield    ,cfield    ,lenchr    ,size(ifield), &
              & .true.    ,.true.    ,.true.    )
    !
    ! When no sub-fields are found, record appears to be empty
    !
    if (nrflds==0) then
        lcomment = .true.
        return
    endif

    !
    ! Check the contents
    !
    lokay = .false.
    !
    ! Check if it is a valid link record
    ! e.g.:  L   trtdefno   trtperc
    !
    if (nrflds==3 .and. itype(1)==1 .and. itype(2)==1 .and.   &
      & (itype(3)==2 .or. itype(3)==1)) then
       lokay = .true.
    endif
    !
    ! Check if it is a valid single point record
    ! e.g.:  n    m    trtdefno   trtperc
    !
    if (nrflds==4 .and. itype(1)==1 .and. itype(2)==1 .and. itype(3)==1 .and.   &
      & (itype(4)==2 .or. itype(4)==1)) then
       lokay = .true.
    endif
    !
    ! Check if it is a valid net link record for unstructured input
    ! e.g.:  x_u    y_u    z_u    trtdefno   trtperc
    ! (At present z is not-used, but 5 input parameters have been chosen to avoid confusion with the other options [nrflds=3,4 or 6])
    !
    if (nrflds==5 .and. &
      & (itype(1)==2 .or. itype(1)==1) .and. &
      & (itype(2)==2 .or. itype(2)==1) .and. &
      & (itype(3)==2 .or. itype(3)==1) .and. &
      & itype(4)==1 .and. (itype(5)==2 .or. itype(5)==1)) then
       lokay = .true.
    endif
    !
    ! Check if it is a valid block record
    ! e.g.:  n1    m1    n2    m2    trtdefno   trtperc
    !
    if (nrflds==6 .and. itype(1)==1 .and. itype(2)==1 .and. itype(3)==1 .and.   &
      & itype(4)==1 .and. itype(5)==1 .and. (itype(6)==2 .or. itype(6)==1)) then
       lokay = .true.
    endif
end subroutine interpret_record

end module m_rdtrt


