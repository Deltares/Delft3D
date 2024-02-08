subroutine inidis(lundia    ,error     ,runid     ,cyclic    ,timnow    , &
                & itdis     ,itstrt    ,itfinish  ,sferic    ,grdang    , &
                & nsrc      ,nsrcd     ,lstsc     ,j         ,nmmaxj    , &
                & icx       ,icy       ,namsrc    ,disint    ,dismmt    , &
                & namcon    ,mnksrc    ,alfas     ,disch     , &
                & disch0    ,disch1    ,rint      ,rint0     ,rint1     , &
                & umdis     ,umdis0    ,umdis1    ,vmdis     ,vmdis0    , &
                & vmdis1    ,bubble    ,kmax      ,kspu      ,kspv      , &
                & upwsrc    ,gdp       )
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
!    Function: Reads the time dependent data from file for the first time
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer  , pointer :: nxbub
    integer  , pointer :: itdate
    integer  , pointer :: lundis
    real(fp) , pointer :: tstop
    real(fp) , pointer :: dt
    real(fp) , pointer :: scalef
    real(fp) , pointer :: timscl
!
! Global variables
!
    integer                                                                     , intent(in)  :: icx      !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                                     , intent(in)  :: icy      !  Increment in the Y-dir. (see ICX)
    integer                                                                     , intent(in)  :: itfinish !  Description and declaration in inttim.igs
    integer                                                                     , intent(in)  :: itstrt   !  Description and declaration in inttim.igs
    integer                                                                                   :: j        !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                                     , intent(in)  :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                                                                     , intent(in)  :: lstsc    !  Description and declaration in dimens.igs
    integer                                                                     , intent(in)  :: lundia   !  Description and declaration in inout.igs
    integer                                                                     , intent(in)  :: nmmaxj   !  Description and declaration in dimens.igs
    integer                                                                     , intent(in)  :: nsrc     !  Description and declaration in esm_alloc_int.f90
    integer                                                                     , intent(in)  :: nsrcd    !  Description and declaration in dimens.igs
    integer                                                                     , intent(in)  :: upwsrc   !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(5, nsrcd)                                          , intent(out) :: itdis    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(7, nsrc)                                           , intent(in)  :: mnksrc   !  Description and declaration in esm_alloc_int.f90
    logical                                                                     , intent(in)  :: bubble   !  Description and declaration in procs.igs        
    logical                                                                     , intent(in)  :: cyclic   !!  Flag = TRUE if cyclic system assumed
    logical                                                                     , intent(out) :: error    !!  Flag=TRUE if an erroris encountered
    logical                                                                     , intent(in)  :: sferic   !  Description and declaration in tricom.igs
    real(fp)                                                                                  :: grdang   !  Description and declaration in tricom.igs
    real(fp)                                                                    , intent(in)  :: timnow
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             , intent(in)  :: alfas    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(lstsc, nsrcd)                                                    :: rint     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(lstsc, nsrcd)                                                    :: rint0    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(lstsc, nsrcd)                                                    :: rint1    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrcd)                                                           :: disch    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrcd)                                                           :: disch0   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrcd)                                                           :: disch1   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrcd)                                                           :: umdis    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrcd)                                                           :: umdis0   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrcd)                                                           :: umdis1   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrcd)                                                           :: vmdis    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrcd)                                                           :: vmdis0   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrcd)                                                           :: vmdis1   !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                , intent(in)  :: runid
    character(1) , dimension(nsrcd)                                                           :: disint   !  Description and declaration in esm_alloc_char.f90
    character(1) , dimension(nsrcd)                                                           :: dismmt   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(lstsc)                                             , intent(in)  :: namcon   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nsrc)                                                            :: namsrc   !  Description and declaration in esm_alloc_char.f90
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)                :: kspu     !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)                :: kspv     !  Description and declaration in esm_alloc_int.f90
!
!> Local variables
!
    integer                                    :: ddb
    integer                                    :: icxy        ! MAX value of ICX and ICY 
    integer                                    :: iend
    integer                                    :: ifound
    integer                                    :: iocond
    integer                                    :: irecrd      ! Counter of records if input file is a direct access file 
    integer, dimension(:), allocatable         :: irecs
    integer                                    :: isrc        ! Index number of discharge location
    integer                                    :: isrc_nodup  ! isrc, skipping duplicate discharges, introduced by bubble screens
    integer                                    :: istart
    integer                                    :: ja_upw      ! Default only around momentum discharge locations upwind is expected = 1 For no upwind JA_UPW = -1
    integer                                    :: l           ! Loop counter over LSTSC 
    integer                                    :: lrec        ! Record length of direct access file 
    integer                                    :: lrid        ! Length of character string runid 
    integer                                    :: m
    integer                                    :: md
    integer                                    :: n
    integer                                    :: nd
    integer                                    :: newlun
    integer                                    :: nm          ! N,M index for discharge location 
    integer                                    :: npara       ! Number of parameter records in time dependent direct access file 
    integer                                    :: nparrd      ! NR. of parameter records actual read
    integer                                    :: nst_nobub   !< Number of discharges excluding bubble points
    integer                                    :: ntimrd
    real(fp)                                   :: alpha       !< linear interpolation factor
    real(fp)                                   :: rtdis0      !< Previous  read time for discharge time-series 
    real(fp)                                   :: rtdis1      !< Following read time for discharge time-series
    logical                                    :: access      ! Flag to read file as direct access or sequential 
    logical                                    :: opend       ! Help flag = TRUE when file is still open (Delft3D) and 
    character(1)                               :: dumchr      ! Dummy character (#) in first record of direct access file 
    character(10)                              :: cntent      ! String with <contents> input 
    character(max(300,(4+lstsc)*16))           :: record      ! Record for DIS file (125 or 153) 
    character(20)                              :: chlp20
    character(20)                              :: namhlp      ! Name of NAMCON(L) in small characters 
    character(20)                              :: namhlp_old  ! Previous value of namhlp
    character(256)                             :: filnam      ! Help var. for file name 
    character(36), dimension(:), allocatable   :: parnam      ! Number of parameter records in time dependent direct access files for DIS 
    character(36), dimension(3)                :: defpar      ! Default parameters 
!
!! executable statements -------------------------------------------------------
!
    nxbub   => gdp%d%nxbub
    timscl  => gdp%gdinidis%timscl
    lundis  => gdp%gdluntmp%lundis
    itdate  => gdp%gdexttim%itdate
    tstop   => gdp%gdexttim%tstop
    dt      => gdp%gdexttim%dt
    scalef  => gdp%gdupddis%scalef
    !
    allocate(parnam(lstsc + 4))
    if (parll) then
       allocate(irecs(nsrcd))
       irecs(:) = -1
    endif
    !
    ddb       = gdp%d%ddbound
    icxy      = max(icx, icy)
    nst_nobub = nsrcd - nxbub
    defpar(1) = 'flux/discharge rate '
    defpar(2) = 'flow magnitude      '
    defpar(3) = 'flow direction      '
    !
    ! define length of RUNID
    !
    call remove_leading_spaces(runid     ,lrid      )
    !
    ! Time dependent discharges
    !
    filnam = 'TMP_' // runid(:lrid) // '.dis'
    !
    ! Test if file is already opened (multi entry Delft3D)
    !
    inquire (file = filnam(:8 + lrid), opened = opend)
    if (.not.opend) then
       open (newunit=lundis, file = filnam(:8 + lrid), form = 'formatted',              &
            & status = 'old')
       read (lundis, '(a1,i5)', iostat = iocond) dumchr, lrec
       !
       ! erroror EOF (IOCOND <> 0), not allowed
       !
       if (iocond/=0) then
          call prterr(lundia    ,'G007'    ,filnam(:8 + lrid)    )
          error= .true.
          goto 9999
       endif
       close (lundis)
       !
       ! for parallel runs, for discharges in this partion: find locations of discharge in input file
       !
       if (parll) then
          irecrd = 0
          !
          ! file not open as direct access!
          !
          open (newunit=lundis, file = filnam(:8 + lrid), form = 'formatted')
   10     continue
          irecrd = irecrd + 1
          read (lundis, '(a)', end=20) record(:lrec - 1)
          iend   = len(record)
          istart = 1
          call srckey(record    ,istart    ,iend      ,ifound    ,gdp       )
          if (ifound == 3) then
             !
             ! location keyword found
             !
             call keyinp(record(istart:iend)  ,chlp20   )
             call small(chlp20    ,len(chlp20)          )
             isrc_nodup = 0
             namhlp_old = ' '
             do isrc = 1, nsrc
                namhlp = namsrc(isrc)
                call small(namhlp, len(namhlp))
                if (namhlp /= namhlp_old) then
                   isrc_nodup = isrc_nodup + 1
                endif
                namhlp_old = namhlp
                if (chlp20 == namhlp) then
                   !
                   ! Discharge record found in the input file
                   ! Store the location of this record table-name in irecs
                   !
                   irecs(isrc_nodup) = irecrd - 2
                   exit
                endif
             enddo
          endif
          goto 10
   20     close (lundis)
       endif
       !
       ! Open file as direct access
       !
       open (newunit=lundis, file = filnam(:8 + lrid), form = 'formatted',              &
            & access = 'direct', recl = lrec)
       !
       ! Initialize ITDIS array
       !
       irecrd = 2
       access = .true.
       !
       do isrc = 1, nsrcd
          itdis(1, isrc) = -1
          itdis(2, isrc) = -1
          itdis(3, isrc) = -1
          itdis(4, isrc) = -1
          itdis(5, isrc) = -1
       enddo
    endif
    !
    ! Loop over nsrcd discharge points
    !
    do isrc = 1, nsrcd
       !
       ! No bubble point: mnksrc(6,i) can be used to check whether this point is inside this partition
       ! Bubble point   : mnksrc mismatches with isrc. This is repaired in cnvbub
       !
       if (isrc<=nst_nobub .and. mnksrc(6,isrc)==-1) cycle
       if (parll) then
          irecrd = irecs(isrc)
       endif
       !
       ! Always read umdis/vmdis values from file
       !
       itdis(1, isrc) = -1
       itdis(2, isrc) = -1
       if (itdis(3, isrc)== -1) then
          !
          ! Read initial values from file
          ! IRECRD = 2 as input and start of time varying data as
          ! output
          !
          read (lundis, '(a)', rec = irecrd) record(:lrec - 1)
          npara = 2 + lstsc + 2
          call flhnew(lundis    ,lundia    ,error     ,record(:lrec - 1)    ,access    , &
                    & irecrd    ,namsrc(isrc)         ,cntent    ,disint(isrc)         ,itdate    , &
                    & timscl    ,ntimrd    ,parnam    ,npara     ,nparrd    , &
                    & bubble    ,gdp       )
          if (error) then
             exit
          endif
          !
          ! Test string CNTENT and define momentum
          !
          if (cntent(:10)=='regular   ' .or. cntent(:10)=='walking   ' .or.     &
            & cntent(:10)=='inoutlet  ' .or. cntent(:10)=='power     ' .or.     &
            & cntent(:10)=='culvert   ') then
             if (nparrd/=2 + lstsc) then
                call prterr(lundia    ,'V097'    ,' '       )
                error= .true.
                exit
             endif
             dismmt(isrc) = 'N'
          elseif (cntent(:8)=='momentum') then
             if (nparrd/=4 + lstsc) then
                call prterr(lundia    ,'V097'    ,' '       )
                error= .true.
                exit
             endif
             dismmt(isrc) = 'Y'
          else
             call prterr(lundia    ,'V096'    ,cntent    )
             error= .true.
             exit
          endif
          !
          ! Test name of constituent conform read parameter name
          ! Only first 20 characters are of significance
          !
          if (parnam(2)(:20)/=defpar(1)(:20)) then
             call prterr(lundia    ,'V096'    ,parnam(2)(:20)       )
             error= .true.
             exit
          endif
          !
          do l = 1, lstsc
             namhlp = namcon(l)
             call small(namhlp    ,20        )
             if (parnam(2 + l)(:20)/=namhlp) then
                call prterr(lundia    ,'V096'    ,parnam(2 + l)(:20)   )
                error= .true.
                goto 9999
             endif
          enddo
          !
          if (dismmt(isrc)=='Y') then
             do l = 1, 2
                if (parnam(2 + lstsc + l)(:20)/=defpar(1 + l)(:20)) then
                   call prterr(lundia    ,'V096'    ,parnam(2 + lstsc + l)(:20)      )
                   error= .true.
                   goto 9999
                endif
             enddo
          endif
          !
          ! Define ITDIS (3>5,ISRC) record values
          !
          itdis(3, isrc) = irecrd
          itdis(4, isrc) = irecrd + ntimrd - 1
          itdis(5, isrc) = itdis(3, isrc) - 1
          !
          ! Define start record for next information records
          !
          irecrd = irecrd + ntimrd
       endif
       itdis(5, isrc) = itdis(3, isrc) - 1
       !
       select case (mnksrc(7,isrc))
       case (3,4,5,7)
          !
          ! culvert: initialize parameters on zero and cycle
          !
          disch(isrc) = 0.0_fp
          umdis(isrc) = 0.0_fp
          vmdis(isrc) = 0.0_fp
          do l = 1, lstsc
              rint(l, isrc) = 0.0_fp
          enddo
          cycle
       case default
          !
          ! nothing
          !
       end select
       !
       ! Define discharge location
       ! Test over KCS is not necessary. Discharge location always
       ! in point with KCS = 1 (see CHKDIS)
       !
       ! WARNING: nm is wrong for bubble points (due to extending mnksrc)
       ! This does not harm because momentum is not used for bubble points
       !
       nm = (mnksrc(5, isrc) + ddb) + ((mnksrc(4, isrc) - 1) + ddb)*icxy
       !
       ! Read first time step(s) and discharge data
       !
       call upddis(lundis    ,lundia    ,sferic    ,itdis     , &
                 & isrc      ,nm        ,grdang    ,timnow    ,dt        , &
                 & itfinish  ,timscl    ,nsrcd     ,lstsc     ,j         , &
                 & nmmaxj    ,dismmt    ,alfas     , &
                 & disch0    ,disch1    ,rint0     ,rint1     , &
                 & umdis0    ,umdis1    ,vmdis0    ,vmdis1    ,gdp       )
       if (disint(isrc)=='Y') then
            if (itdis(1,isrc) == itdis(2,isrc)) then
               alpha = 0.0_fp
            else
               rtdis0 = real(itdis(1,isrc),fp)
               rtdis1 = real(itdis(2,isrc),fp)
               alpha  = (timnow-rtdis0) / (rtdis1-rtdis0)
            endif
            disch(isrc) = (1.0_fp-alpha)*disch0(isrc) + alpha*disch1(isrc)
            umdis(isrc) = (1.0_fp-alpha)*umdis0(isrc) + alpha*umdis1(isrc)
            vmdis(isrc) = (1.0_fp-alpha)*vmdis0(isrc) + alpha*vmdis1(isrc)
            do l = 1, lstsc
                rint(l, isrc) = (1.0_fp-alpha)*rint0(l, isrc) + alpha*rint1(l, isrc)
            enddo
       else
            disch(isrc) = disch0(isrc)
            umdis(isrc) = umdis0(isrc)
            vmdis(isrc) = vmdis0(isrc)
            do l = 1, lstsc
                rint(l, isrc) = rint0(l, isrc)
            enddo
       endif
       !
       ! Test and fill KSPU/V(nm,0) array
       ! This does not work for bubble points, because n and m are wrong
       ! (due to extending mnksrc)
       ! This does not harm because a structure is not allowed at bubble points
       !
       if (isrc <= nst_nobub) then
          if (upwsrc == 0) then
               if (dismmt(isrc) == 'Y') then
                   ja_upw = 1
               else
                   ja_upw = -1
               endif
          else
               ja_upw = upwsrc
          endif
          !
          ! Check whether a structure is positioned at intake/outlet
          ! Floating structure is allowed
          !
          ! Intake
          !
          m  = mnksrc(1, isrc)
          n  = mnksrc(2, isrc)
          md = max(1, m - 1)
          nd = max(1, n - 1)
          if ( abs(kspu(n, m, 0))>2 .or. abs(kspu(n , md, 0))>2 .or.  &
               abs(kspv(n, m, 0))>2 .or. abs(kspv(nd, m , 0))>2 ) then
             call prterr(lundia, 'V057', namsrc(isrc))
          endif
          kspu(n , m , 0) = ja_upw
          kspu(n , md, 0) = ja_upw
          kspv(n , m , 0) = ja_upw
          kspv(nd, m , 0) = ja_upw
          !
          ! Outfall
          !
          m  = mnksrc(4, isrc)
          n  = mnksrc(5, isrc)
          md = max(1, m - 1)
          nd = max(1, n - 1)
          if ( abs(kspu(n, m, 0))>2 .or. abs(kspu(n , md, 0))>2 .or.  &
               abs(kspv(n, m, 0))>2 .or. abs(kspv(nd, m , 0))>2 ) then
             call prterr(lundia, 'V057', namsrc(isrc))
          endif
          kspu(n , m , 0) = ja_upw
          kspu(n , md, 0) = ja_upw
          kspv(n , m , 0) = ja_upw
          kspv(nd, m , 0) = ja_upw
       endif
    enddo
    !
 9999 continue
    deallocate(parnam)
    if (parll) deallocate(irecs)
end subroutine inidis
