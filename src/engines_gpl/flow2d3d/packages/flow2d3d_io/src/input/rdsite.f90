subroutine rdsite(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                & dt        ,filsta    ,fmtsta    ,nostat    ,filtra    , &
                & fmttra    ,ntruv     ,fildro    ,fmtdro    ,ndro      , &
                & drogue    ,namdro    ,mndro     ,itdro     ,dxydro    , &
                & drodep    ,gdp       )
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
!    Function: - Reads the positions of the monitoring stations
!                (if NOSTAT > 0) and cross sections (if NTRUV >0)
!                from the MD-file
!              - Consistence test on NOSTAT/NTRUV read in routine
!                DECARR with nr. of stations/cross sections read
!                in this routine
!              - Read release and stop times and release places
!                of drogues if drogue = .true.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use flow_tables
    use split_stations, only: split_sta_parll, get_sta_node
    !
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                          , pointer :: julday
    integer                          , pointer :: itis
    integer                          , pointer :: stacnt
    integer                          , pointer :: rtcmod
    integer                          , pointer :: rtcact
    integer        , dimension(:,:)  , pointer :: mnrtcsta
    integer        , dimension(:,:)  , pointer :: mnrtcsta_gl
    character(20)  , dimension(:)    , pointer :: namrtcsta
    character(256)                   , pointer :: filrtc
    type (handletype)                , pointer :: moving_stat_file
    integer        , dimension(:)    , pointer :: sta_orgline    
    integer        , dimension(:)    , pointer :: tra_orgline    
    integer        , dimension(:)    , pointer :: stat_type
    integer        , dimension(:)    , pointer :: stat_drogue
    integer        , dimension(:)    , pointer :: stat_table
    integer        , dimension(:)    , pointer :: stat_par
    integer        , dimension(:)    , pointer :: stat_tabidx
    integer        , dimension(:, :) , pointer :: mnit
    integer        , dimension(:, :) , pointer :: mnstat
    character(20)  , dimension(:)    , pointer :: namst
    character(20)  , dimension(:)    , pointer :: namtra
    real(fp)       , dimension(:, :) , pointer :: xystat
    character(256)                   , pointer :: filmst
    integer        , dimension(:)    , pointer :: order_sta
    integer        , dimension(:)    , pointer :: order_tra
    integer, pointer                           :: mfg
    integer, pointer                           :: nfg
    integer, pointer                           :: mlg
    integer, pointer                           :: nlg
!
! Global variables
!
    integer                                          :: lundia !  Description and declaration in inout.igs
    integer                                          :: lunmd  !  Description and declaration in inout.igs
    integer                                          :: ndro   !  Description and declaration in dimens.igs
    integer                                          :: nostat !  Description and declaration in dimens.igs
    integer                                          :: nrrec  !!  Pointer to the record number in the MD-file
    integer                                          :: ntruv  !  Description and declaration in dimens.igs
    integer       , dimension(2, ndro)               :: itdro  !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(2, ndro)               :: mndro  !  Description and declaration in esm_alloc_int.f90
    logical                                          :: drogue !  Description and declaration in procs.igs
    logical                                          :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                         :: dt     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(2, ndro)               :: dxydro !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(ndro)                  :: drodep !  Description and declaration in esm_alloc_real.f90
    character(*)                                     :: fildro !!  File name for the drogue file
    character(*)                                     :: filsta !!  File name for the monitoring stations file
    character(*)                                     :: filtra !!  File name for the cross sections file
    character(*)                                     :: mdfrec !!  Standard rec. length in MD-file (300)
    character(2)                       , intent(out) :: fmtdro !!  File format for the drogue file
    character(2)                       , intent(out) :: fmtsta !!  File format for the monitoring stations file
    character(2)                       , intent(out) :: fmttra !!  File format for the cross sections file
    character(20) , dimension(ndro)                  :: namdro !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                    :: idef         ! Help var. containing default va- lue(s) for integer variable
    integer                                    :: istat
    integer                                    :: it
    integer                                    :: lenc         ! Length of character var.
    integer                                    :: lkw
    integer                                    :: luntri       ! Unit number for trigger file for TRISIM for running programs simultaniously all time dependent data if requested 
    integer                                    :: m1           ! m-index of observation/drogue point or begin point of cross-section
    integer                                    :: m2           ! m-index of end point of cross-section
    integer                                    :: n1           ! n-index of observation/drogue point or begin point of cross-section
    integer                                    :: n2           ! n-index of end point of cross-section
    integer                                    :: n            ! Help var.
    integer                                    :: i            ! Help var.
    integer                                    :: mfl          ! first m-index of this local partition, excluding the halo
    integer                                    :: mll          ! last  m-index of this local partition, excluding the halo
    integer, external                          :: newlun
    integer                                    :: nfl          ! first n-index of this local partition, excluding the halo
    integer                                    :: nll          ! last  n-index of this local partition, excluding the halo
    integer                                    :: nlook        ! Help var.: nr. of data to look for in the MD-file
    integer                                    :: nn           ! Help var.
    integer                                    :: ntrec        ! Help var. to keep track of NRREC 
    integer                                    :: nval         ! Help var. to determine number of table entries
    integer                                    :: pary         ! Help var. to determine parameter number for y-coordinate.
    integer                                    :: taby         ! Help var. to determine table number for y-coordinate.
    integer      , dimension(4)                :: ival         ! Help array (int.) where the data, recently read from the MD-file, are stored temporarily
    integer      , dimension(:,:), allocatable :: itmp1        ! work array to store mnstat/mnit/mndro temporarily
    integer      , dimension(:,:), allocatable :: itmp2        ! work array to store itdro temporarily
    integer      , dimension(:)  , allocatable :: itmp3        ! work array
    integer      , dimension(:)  , allocatable :: nsd          ! integer array to store sequence number of arrays for observation, cross-section and drogue points in own subdomain
    logical                                    :: defaul       ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook)
    logical                                    :: dtn
    logical                                    :: found        ! FOUND=TRUE if KEYW in the MD-file was found
    logical                                    :: lerror       ! Flag=TRUE if an error is encountered
    logical                                    :: lexist       ! Logical to determine file existence 
    logical                                    :: newkw        ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line
    logical                                    :: nodef        ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook)
    real(fp)                                   :: rdef         ! Help var. containing default va- lue(s) for real variable
    real(fp)                                   :: t
    real(fp)     , dimension(2)                :: rval         ! Help array (real) where the data, recently read from the MD-file, are stored temporarily
    real(fp)     , dimension(:,:), allocatable :: rtemp        ! work array to store dxydro temporarily
    character(120)                             :: errmsg       ! Help string for errormessage 
    character(11)                              :: fmtdef       ! Default file format (usually=blank)
    character(11)                              :: fmttmp       ! Help variable for file format
    character(12)                              :: fildef       ! Default file name (usually = blank)
    character(12)                              :: filsim       ! Name for trigger file for TRISIM for running programs simultaniously 
    character(20)                              :: cdef         ! Default value when CVAR not found
    character(20)                              :: chulp        ! Help var.
    character(20), dimension(:) , allocatable  :: ctemp        ! work array to store namst/namtra/namdro temporarily
    character(1024)                            :: errormessage ! Help string for errormessage 
    character(6)                               :: keyw         ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM)
!
!! executable statements -------------------------------------------------------
!
    julday     => gdp%gdinttim%julday
    itis       => gdp%gdrdpara%itis
    stacnt     => gdp%gdrtc%stacnt
    rtcmod     => gdp%gdrtc%rtcmod
    rtcact     => gdp%gdrtc%rtcact
    filrtc     => gdp%gdrtc%filrtc
    !
    !
    ! Allocate arrays for stations and cross sections
    !
    istat = 0
    if (istat == 0) allocate( gdp%gdstations%sta_orgline(   nostat), stat=istat)
    if (istat == 0) allocate( gdp%gdstations%tra_orgline(   ntruv ), stat=istat)
    if (istat == 0) allocate( gdp%gdstations%stat_type  (   nostat), stat=istat)
    if (istat == 0) allocate( gdp%gdstations%stat_drogue(   nostat), stat=istat)
    if (istat == 0) allocate( gdp%gdstations%stat_table (   nostat), stat=istat)
    if (istat == 0) allocate( gdp%gdstations%stat_par   (   nostat), stat=istat)
    if (istat == 0) allocate( gdp%gdstations%stat_tabidx(   nostat), stat=istat)
    if (istat == 0) allocate( gdp%gdstations%mnit       (4, ntruv ), stat=istat)
    if (istat == 0) allocate( gdp%gdstations%mnstat     (2, nostat), stat=istat)
    if (istat == 0) allocate( gdp%gdstations%xystat     (2, nostat), stat=istat)
    if (istat == 0) allocate( gdp%gdstations%namst      (   nostat), stat=istat)
    if (istat == 0) allocate( gdp%gdstations%namtra     (   ntruv ), stat=istat)
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
       call d3stop(1, gdp)
    endif
    !    
    moving_stat_file => gdp%gdstations%moving_stat_file
    sta_orgline      => gdp%gdstations%sta_orgline
    tra_orgline      => gdp%gdstations%tra_orgline
    stat_type        => gdp%gdstations%stat_type 
    stat_drogue      => gdp%gdstations%stat_drogue
    stat_table       => gdp%gdstations%stat_table
    stat_par         => gdp%gdstations%stat_par
    stat_tabidx      => gdp%gdstations%stat_tabidx
    mnit             => gdp%gdstations%mnit
    mnstat           => gdp%gdstations%mnstat
    namst            => gdp%gdstations%namst
    namtra           => gdp%gdstations%namtra
    xystat           => gdp%gdstations%xystat
    filmst           => gdp%gdstations%filmst
    !
    do n = 1, nostat
       sta_orgline(n) = n
    enddo
    !
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    !nmaxgl => gdp%gdparall%nmaxgl
    !mmaxgl => gdp%gdparall%mmaxgl
    !
    ! By default all stations have a fixed N,M location
    ! By default all X,Y locations are undefined
    !
    stat_type   = 0
    stat_drogue = -1
    stat_table  = -1
    stat_par    = -1
    stat_tabidx = 1
    xystat      = -999.0_fp
    !
    lerror = .false.
    newkw  = .true.
    defaul = .true.
    nodef  = .not.defaul
    fildef = ' '
    fmtdef = 'FRformatted'
    fmttmp = ' '
    idef   = 1
    rdef   = 0.0_fp
    nlook  = 0
    lenc   = 20
    namst  = ' '
    mnstat = 0
    cdef   = ' '
    chulp  = ' '
    !
    ! Read info of monitoring station from attribute file or md-file
    ! Initialize file name
    !
    filsta = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filsta', filsta)
    !
    ! monitoring station definitions in file? <YES>
    !
    if (filsta/=fildef) then
       !
       ! locate 'Fmtsta' record for format definition of input file
       !
       call prop_get_string(gdp%mdfile_ptr, '*', 'Fmtsta', fmttmp)
       !
       if (fmttmp(:2) == 'un') then
          fmttmp = 'unformatted'
          fmtsta = 'UN'
       else
          fmttmp = 'formatted'
          fmtsta = 'FR'
       endif
       !
       ! read monitoring station definitions from file.
       ! Stop if reading error occurred or file did not
       ! exist (error  = .true.)
       !
       call stafil(lundia    ,filsta    ,fmttmp    ,error     ,nostat    , &
                 & namst     ,mnstat    ,gdp       )
       if (error) goto 9999
    !
    ! monitoring station definitions in file? <NO> and NOSTAT <> 0
    ! start from top and read a record first, because newkw = .true.
    !
    elseif (nostat>0) then
       rewind (lunmd)
       read (lunmd, '(a300)') mdfrec
       nrrec = 1
       ntrec = nrrec
       do n = 1, nostat
          !
          ! locate 'Namst' record for name of monitoring station
          ! read namst from record
          !
          keyw = 'Namst '
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          !
          ! reading error?
          !
          if (lerror) then
             lerror = .false.
             namst(n) = cdef
          else
             namst(n) = chulp
          endif
          !
          ! there must be a name defined !!
          !
          if (namst(n)==cdef) then
             error = .true.
             call prterr(lundia    ,'V013'    ,' '       )
             exit
          endif
          !
          ! locate 'MNst' record for coordinates of monitoring
          ! station read mstat and nstat from record default value
          ! not allowed => nodef
          !
          keyw = 'MNst  '
          nlook = 2
          call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,ival      ,idef      ,nodef     ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             error = .true.
             lerror = .false.
             exit
          else
             mnstat(1, n) = ival(1)
             mnstat(2, n) = ival(2)
          endif
       enddo
    !
    ! stop reading file
    !
    else
    endif
    !
    ! not twice the same name
    !
    do n = 1, nostat
       do nn = 1, n - 1
          if (namst(nn)==namst(n)) then
             error = .true.
             call prterr(lundia    ,'U170'    ,namst(n)  )
          endif
       enddo
    enddo
    !
    ! for parallel runs, determine which observation points are inside subdomain (excluding the halo) and store them
    !
    if (parll) then
        call split_sta_parll(nostat, mnstat, namst, gdp%gdparall%order_sta, lundia, gdp)
    endif
    !
    if (nostat > 0) then
       filmst = ' '
       call prop_get_string(gdp%mdfile_ptr, '*', 'Filmst', filmst)
       if (filmst /= ' ') then
          write(lundia,'(A,A,A)') 'Reference to moving observation point file ''',trim(filmst),'''.'
          call flw_readtable(moving_stat_file, filmst, julday, gdp)
       endif
    endif
    !
    ! read info of monitoring cross-section, from attribute file or
    ! from md-file
    ! Initialize file name
    !
    filtra = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filcrs', filtra)
    !
    ! monitoring cross-section definitions in file? <YES>
    !
    if (filtra/=fildef) then
       !
       ! locate 'Fmtcrs' record for format definition of input file
       !
       call prop_get_string(gdp%mdfile_ptr, '*', 'Fmtcrs', fmttmp)
       !
       if (fmttmp(:2) == 'un') then
          fmttmp = 'unformatted'
          fmttra = 'UN'
       else
          fmttmp = 'formatted'
          fmttra = 'FR'
       endif
       !
       ! read monitoring cross-section definitions from file.
       ! Stop if reading error occurred or file did not
       ! exist (error  = .true.)
       !
       call trafil(lundia    ,filtra    ,fmttmp    ,error     ,ntruv     , &
                 & namtra    ,mnit      ,gdp       )
       if (error) goto 9999
    !
    ! monitoring cross-section definitions in file? <NO> and NTRUV <> 0
    ! start from top and read a record first, because newkw = .true.
    !
    elseif (ntruv > 0) then
       rewind (lunmd)
       read (lunmd, '(a300)') mdfrec
       nrrec = 1
       ntrec = nrrec
       do n = 1, ntruv
          !
          ! locate 'Namcrs' record for name of monitoring cross-
          ! section read namtra from record
          !
          keyw = 'Namcrs'
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             lerror = .false.
             namtra(n) = cdef
          else
             namtra(n) = chulp
          endif
          !
          ! there must be a name defined !!
          !
          if (namtra(n)==cdef) then
             error = .true.
             call prterr(lundia    ,'V014'    ,' '       )
             exit
          endif
          !
          ! locate 'MNcrs' record for coordinates of monitoring
          ! cross-sections
          ! read m1crs, n1crs, m2crs and n2crs from record
          ! defaul value not allowed => nodef
          !
          keyw = 'MNcrs '
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
             exit
          else
             mnit(1, n) = ival(1)
             mnit(2, n) = ival(2)
             mnit(3, n) = ival(3)
             mnit(4, n) = ival(4)
          endif
       enddo
    !
    ! stop reading file
    !
    else
    endif
    !
    ! not twice the same name
    !
    do n = 1, ntruv
       do nn = 1, n - 1
          if (namtra(nn)==namtra(n)) then
             error = .true.
             call prterr(lundia    ,'U171'    ,namtra(n) )
          endif
       enddo
    enddo
    if (parll .and. inode==master) then
       !
       ! Store mnit in mnit_global before any re-ordering/partition related removements
       ! mnit will be adapted for each partition
       !
       allocate(gdp%gdparall%mnit_global(4,ntruv), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
          call d3stop(1, gdp)
       endif
       do n = 1, ntruv
          gdp%gdparall%mnit_global(:,n) = mnit(:,n)
       enddo
    endif
    !
    ! tra_orgline is initialized here instead of chksit
    ! This makes the remapping in each partition easier
    !
    do n = 1, ntruv
       tra_orgline(n) = n       
    enddo
    !
    ! Reorder sections in U-V directions
    ! All U-oriented sections are placed in front of the V-oriented sections
    ! tra_orgline is used to store the original order
    ! Output is written in the original order
    !
    nn = 1
    i  = nn
    do while (i <= ntruv)
       !
       ! test first for U points (m1=m2)
       !
       if (mnit(1,i) == mnit(3,i)) then
          m1              = mnit(1, nn)
          n1              = mnit(2, nn)
          m2              = mnit(3, nn)
          n2              = mnit(4, nn)
          chulp           = namtra(nn)
          n               = tra_orgline(nn)
          !
          mnit(1:4, nn)   = mnit(1:4, i)
          namtra(nn)      = namtra(i)
          tra_orgline(nn) = tra_orgline(i)
          !
          mnit(1:4, i)    = (/m1, n1, m2, n2/)
          namtra(i)       = chulp
          tra_orgline(i)  = n
          !
          i               = nn
          nn              = nn + 1
       endif
       i = i + 1
    enddo
    !
    ! for parallel runs, determine if cross-section is inside subdomain and store it
    !
    if (parll) then
       if (ntruv == 0) then
          !
          ! No cross-sections in the complete model:
          ! order_tra must be allocated with length 1 and value 0
          !
          allocate(gdp%gdparall%order_tra(1), stat=istat)
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
             call d3stop(1, gdp)
          endif
          order_tra => gdp%gdparall%order_tra
          order_tra(1) = 0
       else
          !
          ! ntruv > 0
          !
          allocate(nsd(ntruv), stat=istat)
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
             call d3stop(1, gdp)
          endif
          nn  = 0
          nsd = 0
          do n = 1, ntruv
             m1 = mnit(1,n) - mfg + 1
             n1 = mnit(2,n) - nfg + 1
             m2 = mnit(3,n) - mfg + 1
             n2 = mnit(4,n) - nfg + 1
             !
             ! check if cross-section points (begin, end) are fully inside or (partly) outside subdomain
             !
             if ( min(m1,m2) < 1 .or. min(n1,n2) < 1 .or. max(m1,m2) > gdp%d%mmax .or. max(n1,n2) > gdp%d%nmaxus ) then
                !
                ! check if one of the two points are outside subdomain
                !
                if ( min(m1,m2) <= gdp%d%mmax .and. min(n1,n2) <= gdp%d%nmaxus .and. max(m1,m2) >= 1 .and. max(n1,n2) >= 1 ) then
                   !
                   ! solution: clip the begin- and end points in this subdomain. 
                   ! 
                   mnit(1,n) = min(max(m1,1),gdp%d%mmax )
                   mnit(2,n) = min(max(n1,1),gdp%d%nmaxus)
                   mnit(3,n) = min(max(m2,1),gdp%d%mmax )
                   mnit(4,n) = min(max(n2,1),gdp%d%nmaxus)
                   write (lundia, '(10x,''remap n='',i4,'' mnit1 = '',2i4,'' mint2 = '', 2i4,'' - node number '',i3.3)') n, mnit(1, n), mnit(2, n), mnit(3, n), mnit(4, n), inode
                   nn      = nn + 1
                   nsd(nn) = n
                endif
             else
                !
                ! cross-section is inside subdomain, store sequence number
                !
                mnit(1,n) = m1
                mnit(2,n) = n1
                mnit(3,n) = m2
                mnit(4,n) = n2
                nn        = nn + 1
                nsd(nn)   = n
             endif
          enddo
          !
          ! restore mnit and namtra of own subdomain
          !
          !
          ! in the parallel case, the original ordering of the
          ! cross sections is kept (for comparisons purpose) in order_tra
          ! order_tra is set to 0 when the partition does not contain
          ! any cross section
          !
          if (nn == 0) then
             allocate(gdp%gdparall%order_tra(1), stat=istat)
             if (istat /= 0) then
                call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
                call d3stop(1, gdp)
             endif
             order_tra => gdp%gdparall%order_tra
             order_tra(1) = 0
          else
             istat = 0
             if (istat == 0) allocate(ctemp(nn)                 , stat=istat)
             if (istat == 0) allocate(itmp1(4,nn)               , stat=istat)
             if (istat == 0) allocate(itmp3(nn)                 , stat=istat)
             if (istat == 0) allocate(gdp%gdparall%order_tra(nn), stat=istat)
             if (istat /= 0) then
                call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
                call d3stop(1, gdp)
             endif
             order_tra => gdp%gdparall%order_tra
          endif
          do n = 1, nn
             order_tra(n) = nsd(n)
             ctemp(n) = namtra(nsd(n))
             do i = 1, 4
                itmp1(i,n) = mnit(i,nsd(n))
             enddo
             itmp3(n) = tra_orgline(nsd(n))
          enddo
          !
          ! Resets effective nb of cross sections on each
          ! partition and reallocates arrays accordingly
          ! with dummy values if final nb found ntruv
          ! is 0 (to avoid using null ptr in subsequent
          ! routine calls)
          !
          namtra = ' '
          nullify(tra_orgline)
          nullify(mnit)
          nullify(namtra)
          deallocate(gdp%gdstations%tra_orgline, stat=istat)
          deallocate(gdp%gdstations%mnit       , stat=istat)
          deallocate(gdp%gdstations%namtra     , stat=istat)
          ntruv = max(1, nn)
          istat = 0
          if (istat == 0) allocate(gdp%gdstations%tra_orgline(ntruv), stat=istat)
          if (istat == 0) allocate(gdp%gdstations%mnit(4,ntruv)     , stat=istat)
          if (istat == 0) allocate(gdp%gdstations%namtra(ntruv)     , stat=istat)
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
             call d3stop(1, gdp)
          endif
          tra_orgline => gdp%gdstations%tra_orgline
          mnit        => gdp%gdstations%mnit
          namtra      => gdp%gdstations%namtra
          if (ntruv == 1 .and. order_tra(1) == 0) then
             mnit(1:4,1)    = (/1,1,1,1/)
             namtra(1)      = ''
             tra_orgline(1) = 0
          else
             do n = 1, ntruv
                namtra(n) = ctemp(n)
                do i = 1, 4
                   mnit(i,n) = itmp1(i,n)
                enddo
                tra_orgline(n) = itmp3(n)
             enddo
          endif
          if (nn /= 0) deallocate(ctemp,itmp1,itmp3, stat=istat)
          deallocate(nsd, stat=istat)
       endif
    endif
    !
    ! read info of drogues, from attribute file or from md-file
    ! Initialize file name
    !
    fildro = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Fildro', fildro)
    if (fildro == ' ') then
       !
       ! FLOW-IP does not yet support 'Fildro', but 'Filpar'
       !
       call prop_get_string(gdp%mdfile_ptr, '*', 'Filpar', fildro)
    endif
    !
    ! drogue file specified? <YES>
    !
    if (fildro/=fildef) then
       call prop_get_string(gdp%mdfile_ptr, '*', 'Fmtdro', fmttmp)
       !
       if (fmttmp(:2) == 'un') then
          fmttmp = 'unformatted'
          fmtdro = 'UN'
       else
          fmttmp = 'formatted'
          fmtdro = 'FR'
       endif
       !
       ! read drogue data from file only.
       ! goto end of subroutine if reading error occurred or file
       ! did not exist (error  = .true.)
       !
       call drofil(lundia    ,fildro    ,fmttmp    ,error     ,ndro      , &
                 & dt        ,namdro    ,mndro     ,itdro     ,dxydro    , &
                 & drodep    ,gdp       )
       if (error) goto 9999
       !
       ! redefine DROGUE, for drogues read from file
       !
       drogue = .true.
    !
    ! drogues in file? <NO>
    !
    elseif (drogue) then
       rewind (lunmd)
       read (lunmd, '(a300)') mdfrec
       nrrec = 1
       ntrec = nrrec
       nlook = 2
       do n = 1, ndro
          !
          ! locate 'Nampar' record for drogue definition
          ! read namdro from record
          ! Nampar record in md-file else ndro = 0 (DIMSIT)
          !
          keyw = 'Nampar'
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             lerror = .false.
             namdro(n) = cdef
          else
             namdro(n) = chulp
          endif
          !
          ! there must be a name defined !!
          !
          if (namdro(n)==cdef) then
             error = .true.
             call prterr(lundia    ,'V034'    ,' '       )
             exit
          endif
          !
          ! locate 'Tpar' record for calculate drogues
          ! read tparf and tparl from record
          !
          keyw = 'Tpar  '
          call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             error = .true.
             rval(1) = rdef
             rval(2) = rdef
          endif
          !
          ! test times as multiple of DT
          !
          itdro(1, n) = nint(rval(1)/dt)
          itdro(2, n) = nint(rval(2)/dt)
          if (dtn(itdro(1, n), rval(1), dt)) then
             error = .true.
             errmsg = 'start time'
             call prterr(lundia    ,'U044'    ,errmsg    )
             write (lundia, '(a,a)') ' for drogue: ', namdro(n)
          endif
          if (dtn(itdro(2, n), rval(2), dt)) then
             error = .true.
             errmsg = 'stop  time'
             call prterr(lundia    ,'U044'    ,errmsg    )
             write (lundia, '(a,a)') ' for drogue: ', namdro(n)
          endif
          if (error) then
             exit
          endif
          !
          ! locate 'XYpar' record for calculate drogues
          ! read xypar from record
          ! xypar is defined as real M,N coordinates
          ! hence MNDRO = int ("xypar") and DXYDRO = "xypar" - MNDRO
          !
          keyw = 'XYpar '
          call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             error = .true.
             exit
          endif
          !
          ! define MNDRO and DXYDRO (RVAL - MNDRO)
          !
          mndro (1, n) = int(rval(1))
          dxydro(1, n) = rval(1) - mndro(1, n)
          mndro (2, n) = int(rval(2))
          dxydro(2, n) = rval(2) - mndro(2, n)
       enddo
    !
    ! stop reading file
    !
    else
    endif
    !
    ! not twice the same name
    !
    do n = 1, ndro
       do nn = 1, n - 1
          if (namdro(nn)==namdro(n)) then
             error = .true.
             call prterr(lundia    ,'U172'    ,namdro(n) )
          endif
       enddo
    enddo
    !
    ! for parallel runs, determine which drogue points are inside subdomain and store them
    !
    if ( parll ) then
       allocate(nsd(ndro), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
          call d3stop(1, gdp)
       endif
       nn  = 0
       nsd = 0
       do n = 1, ndro
          m1 = mndro(1, n) -mfg +1
          n1 = mndro(2, n) -nfg +1
          !
          ! check if drogue point is inside or outside subdomain
          !
          if ( m1 >= 1 .and. n1 >= 1 .and. m1 <= gdp%d%mmax .and. n1 <= gdp%d%nmaxus ) then
             !
             ! observation point is inside subdomain, store sequence number
             !
             mndro(1, n) = m1
             mndro(2, n) = n1
             nn          = nn +1
             nsd(nn)     = n
          endif
       enddo
       !
       ! restore itdro, mndro, dxydro and namdro of own subdomain
       !
       istat = 0
       if (istat == 0) allocate(ctemp(nn)  , stat=istat)
       if (istat == 0) allocate(itmp1(2,nn), stat=istat)
       if (istat == 0) allocate(itmp2(2,nn), stat=istat)
       if (istat == 0) allocate(rtemp(2,nn), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
          call d3stop(1, gdp)
       endif
       do n = 1, nn
          ctemp(n)   = namdro(nsd(n))
          itmp1(1,n) = mndro(1,nsd(n))
          itmp1(2,n) = mndro(2,nsd(n))
          itmp2(1,n) = itdro(1,nsd(n))
          itmp2(2,n) = itdro(2,nsd(n))
          rtemp(1,n) = dxydro(1,nsd(n))
          rtemp(2,n) = dxydro(2,nsd(n))
       enddo
       namdro = ' '
       mndro  = 0
       itdro  = 0
       dxydro = 0.
       ndro   = nn
       do n = 1, ndro
          namdro(n)   = ctemp(n)
          mndro(1,n)  = itmp1(1,n)
          mndro(2,n)  = itmp1(2,n)
          itdro(1,n)  = itmp2(1,n)
          itdro(2,n)  = itmp2(2,n)
          dxydro(1,n) = rtemp(1,n)
          dxydro(2,n) = rtemp(2,n)
       enddo
       deallocate(nsd,ctemp,itmp1,itmp2,rtemp, stat=istat)
    endif
    !
    rtcact = noRTC
    call prop_get_string(gdp%mdfile_ptr, '*', 'RTCact', chulp)
    call small(chulp, 999)
    if (chulp == "bmi") then
       rtcact = RTCviaBMI
       write(lundia,'(a)') '*** MESSAGE Real Time Control via BMI interface'
       ! Array cbuvrt must be set to -1 to flag that valid values have not yet been set via BMI
    elseif (chulp == "rtcmodule") then
       rtcact = RTCmodule
       write(lundia,'(a)') '*** MESSAGE Real Time Control via online RTC module'
    endif
    !
    !
    ! Read info of RTC input station from attribute file or md-file
    ! Initialize file name
    !
    filrtc = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filrtc', filrtc)
    !
    ! RTC input station definitions in file? <YES>
    !
    if (filrtc /= fildef) then
       rtcmod = dataFromFLOWToRTC
       fmttmp = 'formatted'
       !
       !  Read number of locations to be communicated with RTC
       !
       call dimstr(lunmd, filrtc, lundia, error, nrrec, stacnt, gdp)
    else
       stacnt = 0
    endif
    !
    istat = 0
    if (istat == 0) allocate(gdp%gdrtc%mnrtcsta(2,stacnt)                      , stat = istat)
    if (istat == 0) allocate(gdp%gdrtc%mnrtcsta_gl(2,stacnt)                   , stat = istat)
    if (istat == 0) allocate(gdp%gdrtc%inodertcsta(stacnt)                     , stat = istat)
    if (istat == 0) allocate(gdp%gdrtc%namrtcsta(stacnt)                       , stat = istat)
    if (istat == 0) allocate(gdp%gdrtc%r0rtcsta(gdp%d%lstsci,gdp%d%kmax,stacnt), stat = istat)
    if (istat == 0) allocate(gdp%gdrtc%s1rtcsta(stacnt)                        , stat = istat)
    if (istat == 0) allocate(gdp%gdrtc%zrtcsta(gdp%d%kmax,stacnt)              , stat = istat)
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    ! Set local pointers
    !
    mnrtcsta    => gdp%gdrtc%mnrtcsta
    mnrtcsta_gl => gdp%gdrtc%mnrtcsta_gl
    namrtcsta   => gdp%gdrtc%namrtcsta
    !
    if (stacnt>0) then
       !
       ! Read RTC input station definitions from file
       ! Stop if reading error occurred or file did not exist (error = .true.)
       !
       call stafil(lundia   , filrtc  , fmttmp, error, stacnt, &
                 & namrtcsta, mnrtcsta, gdp   )
       if (error) goto 9999
    endif
    !
    ! not twice the same name
    !
    do n = 1, stacnt
       do nn = 1, n - 1
          if (namrtcsta(nn)==namrtcsta(n)) then
             error = .true.
             call prterr(lundia, 'U170', namrtcsta(n))
          endif
       enddo
    enddo
    !
    ! for parallel runs, determine which observation points are inside subdomain (excluding the halo) and store them
    ! first backup global m,n coordinates
    !
    mnrtcsta_gl = mnrtcsta
    if (parll) then
        call get_sta_node(stacnt, mnrtcsta, gdp%gdrtc%inodertcsta, gdp)
    endif
    !
    ! Check whether the name of any station matches the name of any drogue
    !
    do n = 1, nostat
       do nn = 1, ndro
          if (namst(n) == namdro(nn)) then
             stat_type(n) = 2
             stat_drogue(n) = nn
             write(lundia,'(A,A,A)') 'Connecting observation point ''',trim(namst(n)),''' to drogue.'
             exit
          endif
       enddo
       if (stat_type(n) == 0 .and. filmst /= ' ') then
          call flw_gettable(moving_stat_file, namst(n) , 'x-coordinate'  , &
                          & stat_table(n), stat_par(n) , nval, 0, gdp       )
          if (nval == 1) then
             call flw_gettable(moving_stat_file, namst(n) , 'y-coordinate'  , &
                             & taby, pary , nval, 1, gdp       )
             if (taby /= stat_table(n) .or. pary /= stat_par(n)+1) then
                write(errormessage,'(A,A,A)') 'Y-coordinate for station ''',trim(namst(n)),''' should follow x-coordinate.'
                call prterr(lundia, 'U021', trim(errormessage))
                call d3stop(1, gdp)
             elseif (nval == 1) then
                stat_type(n) = 1
                write(lundia,'(A,A,A)') 'XY-coordinates found for observation point ''',trim(namst(n)),'''.'
             elseif (nval > 1) then
                write(errormessage,'(A,A,A)') 'Multiple y-coordinates found for observation point ''',trim(namst(n)),'''.'
                call prterr(lundia, 'U021', trim(errormessage))
                call d3stop(1, gdp)
             else
                write(errormessage,'(A,A,A)') 'No y-coordinate found for observation point ''',trim(namst(n)),'''.'
                call prterr(lundia, 'U021', trim(errormessage))
                call d3stop(1, gdp)
             endif
          elseif (nval > 1) then
             write(errormessage,'(A,A,A)') 'Multiple x-coordinates found for observation point ''',trim(namst(n)),'''.'
             call prterr(lundia, 'U021', trim(errormessage))
             call d3stop(1, gdp)
          endif
       endif
    enddo
    !
    ! Create trigger file for TRISIM to indicate RTC running
    !
    if (rtcmod/=noRTC .and. rtcact/=RTCviaBMI) then
       filsim = 'TMP_SYNC.RUN'
       inquire (file = filsim, exist = lexist)
       if (lexist) then
          open (newunit=luntri, file = filsim)
          close (luntri, status = 'delete')
       endif
       open (newunit=luntri, file = filsim, form = 'unformatted', status = 'unknown')
       !
       ! Write 'RUNRTC' by telephone
       !
       write (luntri) 786782
       close (luntri)
    endif
 9999 continue
end subroutine rdsite
