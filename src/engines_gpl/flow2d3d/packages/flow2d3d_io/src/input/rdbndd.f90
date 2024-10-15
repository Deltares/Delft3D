subroutine rdbndd(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                & nto       ,ntof      ,ntoq      ,mmax      ,nmaxus    , &
                & kmax      ,mxdnto    ,mxnto     ,filbnd    ,fmtbnd    , &
                & ascon     ,nambnd    ,typbnd    ,datbnd    ,mnbnd     , &
                & alpha     ,tprofu    ,statns    ,nhsub     ,yestdd    , &
                & gdp       )
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
!
!    Function: - Reads the boundary definition records from the
!                MD-file: FILBND,FMTBND,TYPBND,DATBND,MNBND,
!                ALPHA, TPROFU and STATNS
!              - The order of reading is sequential for each
!                opening.
!              - Determines NTOT, NTOF, and NTOQ. Required order
!                of opening types: H/A type (for NTOF)
!                                  Q type   (for NTOQ)
!                                  T type   (for NTOT)
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use dfparall
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer              , pointer :: itis
    integer              , pointer :: mfg
    integer              , pointer :: mlg
    integer              , pointer :: nfg
    integer              , pointer :: nlg
    integer              , pointer :: mmaxgl
    integer              , pointer :: nmaxgl
    integer, dimension(:), pointer :: bct_order
    integer              , pointer :: global_num_qh_bnd
    integer              , pointer :: global_num_qtot_bnd
    integer              , pointer :: gntoftoq
!
! Global variables
!
    integer                                           :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                           :: lundia !  Description and declaration in inout.igs
    integer                                           :: lunmd  !  Description and declaration in inout.igs
    integer                           , intent(in)    :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                           :: mxdnto !!  Array dimension for opening boundary definition arrays, which are dynamic declared (NTO in call from READM and MXNTO in call from TDATOM)
    integer                                           :: mxnto  !!  Max. number of open boundaries
    integer                           , intent(in)    :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                           :: nrrec  !!  Pointer to the record number in the MD-file
    integer                           , intent(inout) :: nto    !  Description and declaration in esm_alloc_int.f90
    integer                           , intent(inout) :: ntof   !  Description and declaration in dimens.igs
    integer                           , intent(inout) :: ntoq   !  Description and declaration in dimens.igs
    integer     , dimension(7, mxdnto)                :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(mxnto)                    :: nhsub  !  integer array to store sequence numbers of harmonic boundary condition in own subdomain
    logical                                           :: error  !!  Flag=TRUE if an error is encountered
    real(fp)    , dimension(mxdnto)                   :: alpha  !  Description and declaration in esm_alloc_real.f90
    character(*)                                      :: filbnd !!  File name for the boundary definition file
    character(*)                                      :: mdfrec !!  Standard rec. length in MD-file (300)
    character(1)                                      :: ascon  !!  'Y' if open bnd. contains type 'A'
    character(1), dimension(mxdnto)                   :: typbnd !  Description and declaration in esm_alloc_char.f90
    character(1), dimension(mxnto)                    :: datbnd !!  Type of open boundary:
                                                                !!     -'H'(armonic/Tide)
                                                                !!     -'A'(stronomic/Tide)
                                                                !!     -'Q'(QH-relation)
                                                                !!     -'T'(ime series/time dependent)
    character(12), dimension(mxnto, 2)                :: statns !!  References to tidal stations at boundary support points
    character(2)                      , intent(out)   :: fmtbnd !!  File format for the boundary definition file
    character(20), dimension(mxdnto)                  :: nambnd !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(mxdnto)                  :: tprofu !  Description and declaration in esm_alloc_char.f90
    logical, intent(in)                               :: yestdd ! Flag for call from .true. : tdatom .false.: readmd
!
! Local variables
!
    integer                                     :: i
    integer                                     :: idef        ! Help var. containing default va- lue(s) for integer variable 
    integer                                     :: inprof      ! Index number of first character in PROFIL string of TPROFC definition
    integer                                     :: istat       
    integer                                     :: j	       
    integer                                     :: lenc        ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                                     :: lkw	       
    integer                                     :: lnto        ! Local number of boundary openings read in attribute file, test with input NTO 
    integer                                     :: lntof       ! Local number of harmonic boundary openings read in attribute file, test with input NTO 
    integer                                     :: lntoq       ! Local number of QH openings read in attribute file, test with input NTO 
    integer                                     :: n           ! Help var. 
    integer                                     :: nlook       ! Help var.: nr. of data to look for in the MD-file 
    integer                                     :: nn	       
    integer                                     :: ntest       ! Help var. 
    integer                                     :: ntrec       ! Help. var to keep track of NRREC 
    integer       , dimension(4)                :: ival        ! Help array (int.) where the data, recently read from the MD-file, are stored temporarily 
    integer       , dimension(mxdnto)           :: nsd         ! integer array to store sequence numbers of arrays mnbnd, alpha, typbnd, datbnd, statns, nambnd and tprofu in own subdomain
    integer       , dimension(:,:), allocatable :: itemp       ! work array to store mnbnd temporarily
    logical                                     :: defaul      ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                                     :: found
    logical                                     :: lerror      ! Flag=TRUE if a local error is encountered 
    logical                                     :: ltest       ! Help var. for complex if test 
    logical                                     :: newkw       ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                                     :: nodef       ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                                     :: outsd       ! indicating whether boundary opening is outside subdomain (.TRUE.) or not (.FALSE.)
    logical                                     :: onParbndIsInside
    logical                                     :: isSameOrien ! indicating whether the partition boundary orientation (idir) is the same as the open boundary orientation (.TRUE.) or not (.FALSE.). The default is true.
    real(fp)                                    :: rdef        ! Help var. containing default va- lue(s) for real variable 
    real(fp)      , dimension(4)                :: rval        ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    real(fp)      , dimension(:), allocatable   :: rtemp       ! work array to store alpha temporarily
    character(1)                                :: cdefd       ! Default value when DATBND not found 
    character(1)                                :: cdeft       ! Default value when TYPBND not found 
    character(11)                               :: fmtdef      ! Default file format (usually=FR) 
    character(11)                               :: fmttmp      ! Help variable for file format 
    character(12)                               :: cdefl       ! Default value when STATNS not found 
    character(12)                               :: fildef      ! Default file name (usually = blank) 
    character(20)                               :: cdefn       ! Default value when NAMBND not found 
    character(20)                               :: cdefp       ! Default value when PROFU not found 
    character(20) , dimension(2)                :: chulp       ! Help var. 
    character(3)                                :: errmsg      ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(6)                                :: keyw        ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(60)                               :: profil      ! Total string of possible profiles
    character(100)                              :: message     
    character(20) , dimension(:,:), allocatable :: ctmp1       ! work array to store nambnd/tprofu temporarily
    character( 1) , dimension(:,:), allocatable :: ctmp2       ! work array to store typbnd/datbnd temporarily
    character(12) , dimension(:,:), allocatable :: ctmp3       ! work array to store statns temporarily
    !
    data profil/'uniform             logarithmic         3d-profile          '/
!
!! executable statements -------------------------------------------------------
!
    itis      => gdp%gdrdpara%itis
    mfg       => gdp%gdparall%mfg
    mlg       => gdp%gdparall%mlg
    nfg       => gdp%gdparall%nfg
    nlg       => gdp%gdparall%nlg
    nmaxgl    => gdp%gdparall%nmaxgl
    mmaxgl    => gdp%gdparall%mmaxgl
    bct_order => gdp%gdbcdat%bct_order
    global_num_qh_bnd => gdp%gdbcdat%global_num_qh_bnd
    global_num_qtot_bnd => gdp%gdbcdat%global_num_qtot_bnd
    gntoftoq  => gdp%gdbcdat%gntoftoq
    !
    ! initialize local parameters
    !
    idef   = 1
    rdef   = 0.0
    nlook  = 1
    lerror = .false.
    found  = .false.
    ltest  = .false.
    newkw  = .true.
    defaul = .true.
    isSameOrien = .true.
    nodef  = .not.defaul
    fildef = ' '
    fmtdef = 'FRformatted'
    fmttmp = ' '
    cdefn  = 'Boundary Opening    '
    cdefd  = 'H'
    cdeft  = 'Z'
    cdefp  = 'uniform             '
    cdefl  = '            '
    !
    ! initialize parameters that are to be read
    !
    alpha  = 0.0
    datbnd = 'H'
    filbnd = ' '
    fmtbnd = 'FR'
    mnbnd  = 0
    nambnd = ' '
    statns = ' '
    tprofu = 'uniform'
    typbnd = 'Z'
    !
    ! locate 'Filbnd' record for boundary definition in extra input file
    !
    filbnd = fildef
    call prop_get(gdp%mdfile_ptr,'*','Filbnd',filbnd)
    !
    ! open boundary definition in file? <YES>
    !
    if (filbnd /= fildef) then
       !
       ! locate 'Fmtbnd' record for format definition of input file
       !
       keyw  = 'Fmtbnd'
       ntrec = nrrec
       lenc  = 2
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,fmttmp    ,fmtdef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          fmttmp = fmtdef(3:)
       else
          !
          ! get format (fmttmp = formatted or unformatted)
          !
          call filfmt(lundia    ,keyw      ,fmttmp    ,lerror    ,gdp       )
          if (lerror) then
             lerror = .false.
             fmttmp = fmtdef(3:)
          endif
       endif
       fmtbnd = 'FR'
       if (fmttmp(:2) == 'un') then
          fmtbnd = 'UN'
       endif
       !
       ! read data from file
       ! goto end of subroutine if reading error occurred or file did
       ! not exist (error  = .true.)
       ! NOTE: test for lnto and lntof may be deleted (per definition
       ! not possible, see also rddim or dimrd)
       !
       call bndfil(lundia    ,error     ,kmax      ,lnto      ,lntof     , &
                 & lntoq     ,mxdnto    ,mxnto     ,filbnd    ,fmttmp    , &
                 & profil    ,nambnd    ,typbnd    ,datbnd    ,mnbnd     , &
                 & alpha     ,tprofu    ,statns    ,gdp       )
       if (error) goto 9999
       if (lnto/=nto) then
          call prterr(lundia    ,'U145'    ,' '       )
          !
          error = .true.
          goto 9999
       endif
       if (lntof/=ntof) then
          call prterr(lundia    ,'U145'    ,' '       )
          !
          error = .true.
          goto 9999
       endif
       if (lntoq/=ntoq) then
          call prterr(lundia    ,'U145'    ,' '       )
          error = .true.
          goto 9999
       endif
    !
    ! open boundary definition in file? <NO>
    !
    else
       !
       ! open boundary definition contains a group of records
       ! all records part of the group are supposed to lie between two
       ! records with keyword 'Nambnd'
       ! first search for keyword Profu to set parameter "found"
       ! then set records = first opening => start on top of file
       !
       newkw = .true.
       ntrec = nrrec
       keyw  = 'ProfU'
       lkw   = 5
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       lerror = .false.
       rewind (lunmd)
       read (lunmd, '(a300)') mdfrec
       nrrec = 1
       ntrec = nrrec
       do n = 1, nto
          !
          ! locate and read 'Nambnd' record, default value not allowed
          !
          keyw = 'Nambnd'
          write (cdefn(18:20), '(i3)') n
          chulp(1) = cdefn
          lenc     = 20
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,chulp(1)  ,cdefn     ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             error = .true.
             lerror = .false.
             nambnd(n) = cdefn
             call prterr(lundia    ,'U150'    ,' '       )
          else
             nambnd(n) = chulp(1)
          endif
          !
          ! locate and read 'Typbnd' record, default value not allowed
          !
          typbnd(n)    = cdeft
          keyw         = 'Typbnd'
          chulp(1)(:1) = cdeft
          lenc         = 1
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,chulp(1)  ,cdeft     ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             error = .true.
             lerror = .false.
          else
             typbnd(n) = chulp(1)(:1)
          endif
          !
          ! locate and read 'Datbnd' record, default value allowed
          !
          datbnd(n)    = cdefd
          keyw         = 'Datbnd'
          chulp(1)(:1) = cdefd
          lenc         = 1
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,chulp(1)  ,cdefd     ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          !
          ! reading error?
          !
          if (lerror) then
             lerror = .false.
             error = .true.
          else
             datbnd(n) = chulp(1)(:1)
          endif
          !
          ! locate 'MNbnd ' record
          ! read mnbnd (4) from record,
          ! default value not allowed => nodef
          ! WARNING: mnbnd(5,n) and mnbnd(6,n) are NOT read for MNbnd records in mdf-file
          !
          keyw = 'MNbnd '
          nlook = 4
          call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,ival      ,idef      ,nodef     ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             error = .true.
             lerror = .false.
          else
             mnbnd(1, n) = ival(1)
             mnbnd(2, n) = ival(2)
             mnbnd(3, n) = ival(3)
             mnbnd(4, n) = ival(4)
          endif
          !
          ! locate 'Alpha ' record
          ! read alpha from record,
          ! default value allowed => defaul
          !
          alpha(n) = rdef
          keyw     = 'Alpha '
          nlook    = 1
          call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          !
          ! reading error?
          !
          if (lerror) then
             lerror = .false.
          else
             alpha(n) = rval(1)
          endif
          !
          ! locate 'ProfU ' record
          !
          tprofu(n) = cdefp
          ltest     = (  found .and. kmax>1                                            &
                       & .and.(typbnd(n)=='C' .or. typbnd(n)=='Q' .or. typbnd(n)=='R'  &
                       &       .or. typbnd(n)=='T'))
          if (ltest) then
             !
             ! read profile type from record, only if TYPBND = C/Q/R/T
             ! and kmax > 1
             ! default value allowed => default
             !
             keyw = 'ProfU '
             lenc = 20
             chulp(1) = cdefp
             call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,chulp(1)  ,cdefp     ,lenc      ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             !
             !
             ! reading error?
             !
             if (lerror) then
                tprofu(n) = cdefp
                lerror = .false.
                error = .true.
             else
                tprofu(n) = chulp(1)
             endif
          endif
          call small(tprofu(n) ,lenc      )
          !
          ! Check for undefined profile definition
          !
          inprof = index(profil, tprofu(n))
          if (inprof == 0) then
             call prterr(lundia    ,'U066'    ,chulp(1)  )
             tprofu(n) = cdefp
             error = .true.
          endif
          !
          ! TPROFU may be "3d-profile" only for Time series
          ! (DATBND(N) = 'T')
          !
          if (datbnd(n)/='T' .and. tprofu(n)(:10)=='3d-profile') then
             call prterr(lundia ,'U021' ,'<3D-profile> not allowed for H/A/Q open boundary definitions' )
             tprofu(n) = cdefp
             error = .true.
          endif
          !
          ! locate 'Label ' record
          ! read station names only if datbnd = 'A'
          !
          statns(n, 1) = cdefl
          statns(n, 2) = cdefl
          if (datbnd(n) == 'A') then
             keyw     = 'Label '
             chulp(1) = cdefl
             chulp(2) = cdefl
             nlook    = 2
             lenc     = 12
             call readnc(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,chulp     ,cdefl     ,lenc      ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             !
             !
             ! reading error?
             ! NLOOK can have a different value if not all elements are
             ! read; is not correct
             !
             if (nlook /= 2) then
                call prterr(lundia    ,'U036'    ,keyw      )
                lerror = .false.
             elseif (lerror) then
                lerror = .false.
             else
                statns(n, 1) = chulp(1)(:12)
                statns(n, 2) = chulp(2)(:12)
             endif
          endif
       enddo
    endif
    !
    ! test all read values comparing with definitions and domain
    !
    ntest = 0
    !
    ! If Datbnd (1) = 'A'then ascon = 'Y'
    !
    ascon = 'N'
    if (datbnd(1) == 'A') then
       ascon = 'Y'
    endif
    !
    ! not twice the same name
    !
    do n = 1, nto
       do nn = 1, n - 1
          if (nambnd(nn)==nambnd(n)) then
             error = .true.
             call prterr(lundia    ,'U174'    ,nambnd(n) )
          endif
       enddo
    enddo
    !
    ! for parallel runs, determine if boundary opening is inside subdomain and store it
    !
    do n = 1, mxnto
       nhsub(n) = n
    enddo
    if (.not.parll .and. .not.yestdd) then
       allocate (gdp%gdbcdat%bct_order(nto), stat=istat)
       gdp%gdbcdat%bct_order = 0
       if (istat /= 0) then
          call prterr(lundia, 'P004', 'memory alloc error in rdbndd')
          call d3stop(1, gdp)
       endif
       bct_order => gdp%gdbcdat%bct_order
       do n = 1, nto
          bct_order(n) = n
       enddo
    endif
    !
    ! See big comment block below
    !
    gntoftoq = ntof + ntoq
    global_num_qh_bnd = ntoq
    global_num_qtot_bnd = 0
    do n = 1, nto
       if (typbnd(n) == 'T') then
          global_num_qtot_bnd = global_num_qtot_bnd + 1
       endif
    enddo
    if (parll .and. .not.yestdd) then
       !      
       ! bct_order must have the dimension of the global number of boundary conditions (mxdnto).
       ! nto is the number of open boundaries in this specific partition
       !
       allocate(gdp%gdbcdat%bct_order(mxdnto), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'P004', 'memory alloc error in rdbndd(bct_order)')
          call d3stop(1, gdp)
       endif    
       bct_order     => gdp%gdbcdat%bct_order
       bct_order     = 0
       !
       ! The (global) boundaries are ordered:
       ! - first the astronomic/harmonic boundaries (ntof pieces)
       ! - then the Q boundaries                    (ntoq pieces)
       ! - finally the Time series boundaries       (nto-ntof-ntoq pieces)
       ! When running parallel, all boundaries that are completely outside this partition are removed in this subroutine
       ! nto, ntof, ntoq will be adapted for this partition
       ! Each boundary (partly) inside this partition has a reference to the original global boundary index:
       !     bct_order(i) =  order of boundary i in original global boundaries list
       ! For Time series boundaries:
       !     In the original global boundary list, index-ntof-ntoq refers to the related table in the bct file:
       !         boundary "ntof+ntoq+i" refers to table i in the bct file
       !     When running parallel and boundaries outside this partition are removed, 
       !     the correct table in the bct file is found by using bct_order and gntoftoq:
       !         "bct_order(i) - gntoftoq" refers to the correct table in the bct file. See updbct.f90
       !     This also holds for non-parallel calculations
       !
       !
       ! Store the original global mnbnd
       ! Only mnbnd_global(1:4,:) is set yet, but that's all that's needed
       !
       allocate (gdp%gdbcdat%mnbnd_global(7, mxdnto), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'P004', 'memory alloc error in rdbndd(mnbnd_global)')
          call d3stop(1, gdp)
       endif
       gdp%gdbcdat%mnbnd_global = mnbnd
       nn    = 0
       nsd   = 0
       do n = 1, nto
          !
          ! first, check whether mnbnd values are inside entire domain
          ! if not, default is used
          !
          if ( mnbnd(1,n)<1 .or. mnbnd(2,n)<1 .or. mnbnd(3,n)<1 .or. mnbnd(4,n)<1 ) then
             error = .true.
             call prterr( lundia, 'U151', ' ')
             mnbnd(1, n) = max(mnbnd(1, n), 1)
             mnbnd(2, n) = max(mnbnd(2, n), 1)
             mnbnd(3, n) = max(mnbnd(3, n), 1)
             mnbnd(4, n) = max(mnbnd(4, n), 1)
          endif
          if ( mnbnd(1,n)>mmaxgl .or. mnbnd(2,n)>nmaxgl .or. mnbnd(3,n)>mmaxgl .or. mnbnd(4,n)>nmaxgl ) then
             error = .true.
             call prterr( lundia, 'U151', ' ')
             mnbnd(1, n) = min(mnbnd(1, n), mmaxgl)
             mnbnd(2, n) = min(mnbnd(2, n), nmaxgl)
             mnbnd(3, n) = min(mnbnd(3, n), mmaxgl)
             mnbnd(4, n) = min(mnbnd(4, n), nmaxgl)
          endif
          !
          ival(1) = mnbnd(1, n) - mfg + 1
          ival(2) = mnbnd(2, n) - nfg + 1
          ival(3) = mnbnd(3, n) - mfg + 1
          ival(4) = mnbnd(4, n) - nfg + 1
          !
          ! check if boundary opening is fully (.TRUE.) or partly (.FALSE.) outside subdomain
          !
          !
          ! idir: the partition boundary orientation: 1(row n) or 2(column m)
          ! mnbnd: open boundary orientation.
          ! if they are different, the onParbndIsInside should be true.
          !
          if (mnbnd(1, n) == mnbnd(3, n)) then
             if (idir == 1) then
                isSameOrien = .false.
             else
                isSameOrien = .true. 
             endif 
          elseif (mnbnd(2, n) == mnbnd(4, n)) then
             if (idir == 1) then
                isSameOrien = .true.
             else
                isSameOrien = .false. 
             endif 
          endif
          if (isSameOrien) then 
             onParbndIsInside = .false.
          else
             onParbndIsInside = .true.
          endif
          call adjlin (ival, outsd, mmax, nmaxus, onParbndIsInside)
          !
          if (.not.outsd) then
             !
             ! (part of) boundary opening is inside subdomain, store sequence number
             !
             mnbnd(1, n) = ival(1)
             mnbnd(2, n) = ival(2)
             mnbnd(3, n) = ival(3)
             mnbnd(4, n) = ival(4)
             nn          = nn + 1
             nsd(nn)     = n
          endif
       enddo
       !
       ! restore mnbnd, alpha, typbnd, datbnd, statns, nambnd and tprofu of own subdomain
       ! re-count nto, ntof and ntoq in own subdomain
       !

                       allocate(ctmp1(2,nn), stat=istat)
       if (istat == 0) allocate(ctmp2(2,nn), stat=istat)
       if (istat == 0) allocate(ctmp3(2,nn), stat=istat)
       if (istat == 0) allocate(itemp(7,nn), stat=istat)
       if (istat == 0) allocate(rtemp(  nn), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'P004', 'memory alloc error in rdbndd')
          call d3stop(1, gdp)
       endif
       do n = 1, nn
          ctmp1(1,n)   = nambnd(nsd(n))
          ctmp1(2,n)   = tprofu(nsd(n))
          ctmp2(1,n)   = typbnd(nsd(n))
          ctmp2(2,n)   = datbnd(nsd(n))
          ctmp3(1,n)   = statns(nsd(n),1)
          ctmp3(2,n)   = statns(nsd(n),2)
          do j = 1, 7
             itemp(j,n) = mnbnd(j,nsd(n))
          enddo
          rtemp(n) = alpha(nsd(n))
       enddo
       nambnd = ' '
       tprofu = ' '
       typbnd = ' '
       datbnd = ' '
       statns = ' '
       mnbnd  = 0
       alpha  = 0.
       nhsub  = 0
       nto    = nn
       ntof   = 0
       ntoq   = 0
       do n = 1, nto
          nambnd(n)   = ctmp1(1,n)
          tprofu(n)   = ctmp1(2,n)
          typbnd(n)   = ctmp2(1,n)
          datbnd(n)   = ctmp2(2,n)
          statns(n,1) = ctmp3(1,n)
          statns(n,2) = ctmp3(2,n)
          do j = 1, 7
             mnbnd(j,n) = itemp(j,n)
          enddo
          alpha(n) = rtemp(n)
          if (datbnd(n)=='Q') then
             ntoq = ntoq + 1
          elseif (datbnd(n)/='T') then
             ntof = ntof + 1
             ! determine which harmonic boundary sections belong to which subdomains
             nhsub(ntof) = nsd(n)
          endif
          bct_order(n) = nsd(n)         
       enddo
       deallocate(ctmp1,ctmp2,ctmp3,itemp,rtemp, stat=istat)
    endif ! parll and not.yestdd
    do n = 1, nto
       !
       ! NAMBND may not be blank
       !
       if (nambnd(n)==' ') then
          write (cdefn(18:20), '(i3)') n
          nambnd(n) = cdefn
          call prterr(lundia    ,'U150'    ,' '       )
       endif
       !
       ! typbnd should be one of the pre-defined types
       !
       if (      typbnd(n)/='Z' .and. typbnd(n)/='C' .and. typbnd(n)/='Q' &
         & .and. typbnd(n)/='R' .and. typbnd(n)/='T' .and. typbnd(n)/='N'  ) then
          error = .true.
          call prterr(lundia    ,'U047'    ,' '       )
          typbnd(n) = cdeft
       endif
       !
       ! Datbnd should be one of the pre-defined values
       !
       if (datbnd(n)/='T' .and. datbnd(n)/='H' .and. datbnd(n)/='A' .and.       &
         & datbnd(n)/='Q') then
          error = .true.
          write (errmsg, '(i3)') n
          call prterr(lundia    ,'V004'    ,errmsg    )
          datbnd(n) = cdefd
       endif
       !
       ! test following order of Datbnd for consistancy,
       ! as long as ntest = 0 datbnd = only 'H' or only 'A' is permitted
       !
       if ((datbnd(n)=='H' .or. datbnd(n)=='A') .and. ntest/=0) then
          error = .true.
          call prterr(lundia    ,'U039'    ,' '       )
       elseif (datbnd(n)=='Q') then
          if (ntest>1) then
             error = .true.
             call prterr(lundia    ,'U078'    ,' '       )
          else
             ntest = 1
          endif
       elseif (datbnd(n)=='T') then
          ntest = 2
       else
       endif
       !
       ! test consistency for astronomic or harmonic boundary values
       !
       if (ascon=='Y' .and. datbnd(n)=='H' .or. ascon=='N' .and. datbnd(n)=='A')&
         & then
          error = .true.
          call prterr(lundia    ,'U065'    ,' '       )
       endif
       !
       ! test consistency for QH relations
       !
       if (datbnd(n)=='Q' .and. typbnd(n)/='Z') then
          error = .true.
          call prterr(lundia    ,'U077'    ,' '       )
       endif
       if (mnbnd(5,n) /= 0) then
          if (tprofu(n) /= 'uniform') then
             write(message,'(3a)') 'Restricted vertical opening on a non-uniform open boundary (', &
                               & trim(nambnd(n)), ') is not supported.'
             call prterr(lundia, 'P004', trim(message))
             error = .true.
          endif
          if (     mnbnd(5,n) < 1         &
            & .or. mnbnd(6,n) > kmax      &
            & .or. mnbnd(5,n) > mnbnd(6,n) ) then
             write(message,'(3a)') 'Improper vertical specification of open boundary ', &
                               & trim(nambnd(n)), '. Default is used.'
             call prterr(lundia, 'P004', trim(message))
             mnbnd(5,n) = 0
             error = .true.
          else
             write(message,'(3a,i0,a,i0)') 'Open boundary ', trim(nambnd(n)), &
                 & ' is restricted to the layers with number ', mnbnd(5,n), ' to ', mnbnd(6,n)
             call prterr(lundia, 'G051', trim(message))
          endif
       endif
       !
       ! test mnbnd values inside domain
       !
       if ( parll .and. .not.yestdd ) cycle
       if (mnbnd(1, n)<1 .or. mnbnd(2, n)<1 .or. mnbnd(3, n)<1 .or. mnbnd(4, n)<1) then
          error = .true.
          call prterr(lundia    ,'U151'    ,' '       )
          !
          mnbnd(1, n) = max(mnbnd(1, n), 1)
          mnbnd(2, n) = max(mnbnd(2, n), 1)
          mnbnd(3, n) = max(mnbnd(3, n), 1)
          mnbnd(4, n) = max(mnbnd(4, n), 1)
       endif
       if (mnbnd(1, n)>mmax .or. mnbnd(2, n)>nmaxus .or. mnbnd(3, n)>mmax .or.  &
         & mnbnd(4, n)>nmaxus) then
          error = .true.
          call prterr(lundia    ,'U151'    ,' '       )
          mnbnd(1, n) = min(mnbnd(1, n), mmax)
          mnbnd(2, n) = min(mnbnd(2, n), nmaxus)
          mnbnd(3, n) = min(mnbnd(3, n), mmax)
          mnbnd(4, n) = min(mnbnd(4, n), nmaxus)
       endif
    enddo
    !
    ! Allocate and initialize the mask array for boundaries coupled to SobekSIM
    ! ext_bnd may be already allocated when rdbndd was called by TDATOM
    !
    if (associated(gdp%gdbcdat%ext_bnd)) deallocate(gdp%gdbcdat%ext_bnd, stat=istat)
    allocate (gdp%gdbcdat%ext_bnd(nto), stat=istat)
    if (istat /= 0) then
       call prterr(lundia, 'P004', 'memory alloc error in rdbndd(ext_bnd)')
       call d3stop(1, gdp)
    endif
    gdp%gdbcdat%ext_bnd = 0
 9999 continue
end subroutine rdbndd
