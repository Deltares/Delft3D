subroutine strfil(lundia    ,error     ,filstr    ,mmax      , &
                & nmaxus    ,kmax      ,istruc    ,kspu      ,kspv      , &
                & ubrlsu    ,ubrlsv    ,uwtypu    ,uwtypv    ,dphkru    , &
                & dphkrv    ,gdp       )
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
!    Function: Reads rigid sheet/weir data from attribute file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use dfparall
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Local parameters
!
    integer, parameter :: mxstr = 11
!
! Global variables
!
    integer                                                             , intent(in)  :: istruc !!  =+/-3: local weir
                                                                                                !!  =+/-4: gate
                                                                                                !!  =+/-5: rigid sheets
                                                                                                !!  =+/-6: porous plate
                                                                                                !!  =+/-7: bridge
                                                                                                !!  =+/-9: 2D Weir
    integer                                                             , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: lundia !  Description and declaration in inout.igs
    integer                                                             , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)              :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)              :: kspv   !  Description and declaration in esm_alloc_int.f90
    logical                                                                           :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(out) :: dphkru !!  Depth in u points incl. crest or
                                                                                                !!  Crown height at u points
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(out) :: dphkrv !!  Depth in v points incl. crest or
                                                                                                !!  Crown height at v points
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(out) :: uwtypu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(out) :: uwtypv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax) , intent(out) :: ubrlsu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax) , intent(out) :: ubrlsv !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                      :: filstr !!  Name of the relevant file
!
! Local variables
!
    integer                         :: iabsst ! ABS (ISTRUC) 
    integer                         :: ibeg   ! Begin position in the RECORD from where the search for data/record is started 
    integer                         :: idef   ! Help var. containing default va- lue(s) for integer variable 
    integer                         :: iend   ! Last position in the RECORD when the searched data/record is finished 
    integer                         :: ier    ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                         :: inc
    integer                         :: incx   ! Increment between M1,M2 
    integer                         :: incy   ! Increment between N1,N2 
    integer                         :: iocond ! IO status output: > 0 error< 0 end-of-file = 0 ok 
    integer                         :: k      ! Loop variable 
    integer                         :: k1
    integer                         :: k2
    integer                         :: kabssp ! ABS (KSPU/V(NM,0)) 
    integer                         :: kkmax
    integer                         :: kkmin
    integer                         :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                         :: lfile  ! Length of file name 
    integer                         :: luntmp ! Unit number for attribute file 
    integer                         :: m
    integer                         :: m1     ! First m-index for rigid sheets 
    integer                         :: m2     ! Last m-index for rigid sheets 
    integer                         :: maxinc ! Maximum of (increment between M1,M2 & increment between N1,N2) 
    integer                         :: n      ! Loop variable 
    integer                         :: n1     ! First n-index for rigid sheets 
    integer                         :: n2     ! Last n-index for rigid sheets 
    integer                         :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                         :: nrigid ! Help var. (counter) 
    integer, dimension(6)           :: ival   ! Help array (int.) where the data, recently read from file, are stored temporarily 
    integer, external               :: newlun
    logical                         :: skip   ! Skip flag for structures outside active domain
    real(fp)                        :: brlosc ! Constant value for wall roughness 
    real(fp)                        :: rdef   ! Help var. containing default va- lue(s) for real variable 
    real(fp)                        :: value1 ! = Crest level (weir) or bottom coord. of the 3d gate (fixed)
    real(fp)                        :: value2 ! = UWT(ype)    (weir) ot top coord. of the 3d gate (fixed)
    character(1)                    :: direct ! Direction (U/V) of rigid sheet/weir 
    character(132)                  :: rec132 ! Standard rec. length in an attribute file (132) 
    character(18), dimension(mxstr) :: struct ! Array containing names of defined structure 
    character(50)                   :: errmsg ! Text string error messages 
    character(100)                  :: errtxt
    integer                         :: mfl    ! first m-index of this local partition, including the halo
    integer                         :: mll    ! last  m-index of this local partition, including the halo
    integer                         :: nfl    ! first n-index of this local partition, including the halo
    integer                         :: nll    ! last  n-index of this local partition, including the halo
    integer, pointer                :: mfg
    integer, pointer                :: nfg
    integer, pointer                :: mlg
    integer, pointer                :: nlg


!
!
!
    data (struct(m), m = 1, mxstr)/'discharge location', 'floating structure',  &
         & 'local weir        ', 'gate              ', 'rigid sheet       ',     &
         & 'porous plate      ', 'bridge            ', 'barrier           ',     &
         & '2d weir           ', 'fixed 3d gate     ', 'UNKNOWN STRUCTURE '/
!
!! executable statements -------------------------------------------------------
!
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg

    ! Initialize local parameters
    !
    lenc   = 1
    idef   = 0
    rdef   = 0.0
    iabsst = abs(istruc)
    errmsg = struct(min(iabsst, mxstr))

    !
    ! test file existence, if so read
    !
    call remove_leading_spaces(filstr    ,lfile     )
    if (exifil(filstr, lundia)) then
       !
       ! file = exist
       !
       nrigid = 0
       open (newunit=luntmp, file = filstr(1:lfile), form = 'formatted', status = 'old')
       !
       ! freeformatted file, skip lines starting with a '*'
       !
       call skipstarlines(luntmp    )
       !
       ! Freeformatted file
       !
       !-->     read input till end-of-file
       !
 1000  continue
       read (luntmp, '(a)', iostat = iocond) rec132
       if (iocond/=0) then
          !
          ! Reading error?
          !
          if (iocond>0) then
             call prterr(lundia    ,'G007'    ,filstr(1:lfile)      )
             !
             error = .true.
          endif
          goto 1100
       endif
       !
       ! Empty record? Continue on following line
       !
       if (rec132==' ') then
          call prterr(lundia    ,'G033'    ,filstr(1:lfile)      )
          !
          goto 1000
       endif
       !
       ! Read direct from record
       ! string to long and no value not allowed => IER > 0
       !
       ibeg = 1
       iend = 132
       call read1c(rec132    ,132       ,ibeg      ,iend      ,direct    , &
                 & lenc      ,ier       )
       if (ier<=0) then
          error = .true.
          call prterr(lundia    ,'G007'    ,filstr(1:lfile)      )
          !
          goto 1100
       endif
       !
       ! Read IVAL  and define local variables
       ! number of values to read (NLOOK) to many and
       ! no value not allowed => IER > 0
       ! For local weir all layers are taken in account
       !
       ibeg = iend + 1
       nlook = 6
       if ((iabsst==3) .or. (iabsst==7) .or. (iabsst==9) .or. (iabsst==10)) then
          nlook = 4
       endif
       call readni(rec132    ,132       ,ibeg      ,iend      ,nlook     , &
                 & ival      ,idef      ,ier       )
       if (ier <= 0) then
          error = .true.
          call prterr(lundia    ,'G007'    ,filstr(1:lfile)      )
          !
          goto 1100
       endif
       m1 = ival(1)
       n1 = ival(2)
       m2 = ival(3)
       n2 = ival(4)
       !
       if ((iabsst==3) .or. (iabsst==7) .or. (iabsst==9) .or. (iabsst==10)) then
          k1 = 1
          k2 = kmax
       else
          k1 = ival(5)
          k2 = ival(6)
       endif
       !
       ! read BRLOSC from record, default value not allowed
       ! for local weir or rigid sheet
       !
       brlosc = 0.0
       if (iabsst /= 4) then
          ibeg = iend + 1
          call read1r(rec132    ,132       ,ibeg      ,iend      ,brlosc    , &
                    & rdef      ,ier       )
          if (ier<=0) then
             call prterr(lundia    ,'G007'    ,filstr(1:lfile)      )
             !
             error = .true.
             goto 1100
          endif
       endif
       !
       ! if local weir or fixed positioned 3d gate read value1 from record;
       ! [value1 = crest level for local weir and 
       !         = bottom (z-)coordinate of the gate]
       ! default not allowed
       !
       if ((iabsst==3) .or. (iabsst==9) .or. (iabsst==10)) then
          ibeg = iend + 1
          call read1r(rec132    ,132       ,ibeg      ,iend      ,value1   , &
                    & rdef      ,ier       )
          if (ier <= 0) then
             call prterr(lundia    ,'G007'    ,filstr(1:lfile)      )
             !
             error = .true.
             goto 1100
          endif
          !
          ! if local weir or fixed positioned 3d gate read value2 from record;
          ! [value2 = UWT(ype) for local weir and 
          !         = top (z-)coordinate of the gate]
          ! default not allowed
          !
          ibeg = iend + 1
          call read1r(rec132    ,132       ,ibeg      ,iend      ,value2   , &
                    & rdef      ,ier       )
          if (ier <= 0) then
             call prterr(lundia    ,'G007'    ,filstr(1:lfile)      )
             !
             error = .true.
             goto 1100
          endif
       endif
       !
       ! Define local weir/ gate / rigid sheet / porous plate or
       ! bridge / fixed positioned 3d gate sequence number
       ! All tests are executed and when an error occurred read the
       ! next structure definition
       !
       nrigid = nrigid + 1
       if (nrigid <= 99999) then
          write (errmsg(20:24), '(i5)') nrigid
       else
          write (errmsg(20:24), '(a5)') '*****'
       endif

       !
       ! Test contents of read coordinate pairs
       !
       call increm(m1        ,n1        ,m2        ,n2        ,incx      , &
                 & incy      ,maxinc    ,error     )
       !
       ! If error then no multiple of 45 degrees
       !
       if (error) then
          call prterr(lundia    ,'V231'    ,errmsg(:24)          )
          !
          goto 1100
       endif


       skip = .false.

       if (parll ) then

          if (abs(incx).gt.0 .and. abs(incy).gt.0) then
             errtxt = 'skewed structures in parallel are not allowed'
             call prterr(lundia ,'U021' ,errtxt )
             error = .true.
             goto 1100
          endif

          m1 = m1 - mfg + 1
          n1 = n1 - nfg + 1
          m2 = m2 - mfg + 1
          n2 = n2 - nfg + 1

          if (idir == 1) then
             !
             ! n direction is split
             !
             mfl = 1
             mll = gdp%d%mmax
             nfl = 1
             nll = gdp%d%nmaxus
          elseif (idir == 2) then
             !
             ! m direction is split
             !
             nfl = 1
             nll = gdp%d%nmaxus
             mfl = 1
             mll = gdp%d%mmax
          endif
       
          ! check if point is inside or outside subdomain, including the halo
          ! a kind of clipping algorithm has to be used for non-pointsize structures
          !  Note that for skewed wiers, this clipping is much more difficult.
          ! we have skipped that possibility above; so we only have 1d-structures here.

          if ((max(m1,m2) .lt. mfl ) .or. (max(n1,n2).lt. nfl) .or.    &
              (min(m1,m2) .gt. mll ) .or. (min(n1,n2) .gt. nll)) then
             skip = .true.
          else   
             !
             ! 1D-clipping
             !
             m1 = min(mll,max(m1,mfl))
             m2 = min(mll,max(m2,mfl))
             n1 = min(nll,max(n1,nfl))
             n2 = min(nll,max(n2,nfl))
          endif

       endif

       !
       ! Test contents of read coordinate pairs
       !
       call increm(m1        ,n1        ,m2        ,n2        ,incx      , &
                 & incy      ,maxinc    ,error      )
       !
       ! If error then no multiple of 45 degrees
       !
       if (error) then
          call prterr(lundia    ,'V231'    ,errmsg(:24)          )
          !
          goto 1100
       endif


       if ((m1>mmax) .or. (m1<1) .or. (m2>mmax) .or. (m2<1)) then
          call prterr(lundia    ,'V249'    ,errmsg(:24)          )
          !
          ! Don't stop, just give warning; skip entry
          !
          skip = .true.
       endif
       !
       if ((n1>nmaxus) .or. (n1<1) .or. (n2>nmaxus) .or. (n2<1)) then
          call prterr(lundia    ,'V249'    ,errmsg(:24)          )
          !
          ! Don't stop, just give warning; skip entry
          !
          skip = .true.
       endif
       !
       kkmin = min(k1, k2)
       kkmax = max(k1, k2)
       if ((kkmin<1) .or. (kkmax>kmax)) then
          call prterr(lundia    ,'V232'    ,errmsg(:16)          )
          !
          error = .true.
          goto 1100
       endif
       !
       ! Value for wall roughness permitted ?
       ! for local weir / rigid sheet
       !
       if (iabsst /= 4) then
          if (brlosc <= 0.0) then
             call prterr(lundia    ,'V234'    ,errmsg(:24)          )
             !
             error = .true.
          endif
       endif
       !
       ! For gates with fixed position: Value2 > Value1
       !
       if (iabsst==10 .and. value2 <= value1) then
          call prterr(lundia    ,'V222'    ,' '                    )
          !
          error = .true.
          goto 1100
       endif
       !
       ! Write input BRLOSC to array for <M1,N1,K1> to <M2,N2,K2>
       ! belonging to the given direction in DIRECT
       !
       call small(direct    ,1         )
       m = m1 - incx
       n = n1 - incy
       !
       ! First U-direction
       !
       if (skip) then
          !
          ! Skip this structure
          !
       elseif (direct == 'u') then
          do inc = 1, maxinc + 1
             m = m + incx
             n = n + incy
             !
             ! Check for different structures defined
             !
             kabssp = abs(kspu(n, m, 0))
             if (kabssp>2) then
                !
                ! Allowed exception:
                ! several gates (struct(4)) at the same m,n
                ! coordinate, as long as the k coordinates do not
                ! interfere
                !
                if ((kabssp==4) .and. (iabsst==4)) then
                   do k = kkmin, kkmax
                      if (kspu(n, m, k) == 1) then
                         errmsg(19:) = ' on ' // struct(kabssp)
                         call prterr(lundia    ,'V235'    ,errmsg(:40)          )
                         write(lundia,*) '          at m=',m,',n= ',n,',k=',k,' (U point)'
                         !
                         error = .true.
                         goto 1100
                      endif
                   enddo
                else
                   errmsg(19:) = ' on ' // struct(kabssp)
                   call prterr(lundia    ,'V235'    ,errmsg(:40)          )
                   write(lundia,*) '          at m=',m,',n= ',n,' (U point)'
                   !
                   error = .true.
                   goto 1100
                endif
             endif
             !
             ! Check for discharge location or floating structure
             ! defined (allowed)
             !
             if (kabssp > 0) then
                errmsg(19:) = ' near ' // struct(kabssp)
                call prterr(lundia    ,'V236'    ,errmsg    )
             !
             endif
             !
             ! Define Weir values or coordinates of the 3d-gate (see RDSTRU)
             ! DPHKRU = Crest level (IABSST = 3 / 9) or CDWZBU (IABSST = 10)
             ! UWTYPU = UWType      (IABSST = 3 / 9) or CDWZTU (IABSST = 10)
             !
             if ((iabsst==3) .or. (iabsst==9) .or. (iabsst==10)) then
                dphkru(n, m) = value1
                uwtypu(n, m) = value2
             endif
             !
             ! Define structure point masks and coefficient
             !
             kspu(n, m, 0) = istruc
             if (iabsst /= 10) then
                do k = kkmin, kkmax
                   ubrlsu(n, m, k) = brlosc
                   kspu(n, m, k) = 1
                enddo
             else
                !
                ! For CDW (KSPU(nm,0) == 10; UBRLSU == CDWLSU which is a 2 dimensional array
                !
                ubrlsu(n, m, 1) = brlosc
             endif
          enddo
       elseif (direct == 'v') then
          do inc = 1, maxinc + 1
             m = m + incx
             n = n + incy
             !
             ! Check for different structures defined (not allowed)
             !
             kabssp = abs(kspv(n, m, 0))
             if (kabssp > 2) then
                !
                ! Allowed exception:
                ! several gates (struct(4)) at the same m,n
                ! coordinate, as long as the k coordinates do not
                ! interfere
                !
                if ((kabssp==4) .and. (iabsst==4)) then
                   do k = kkmin, kkmax
                      if (kspv(n, m, k) == 1) then
                         errmsg(19:) = ' on ' // struct(kabssp)
                         call prterr(lundia    ,'V235'    ,errmsg(:40)          )
                         write(lundia,*) '          at m=',m,',n= ',n,',k=',k,' (V point)'
                         !
                         error = .true.
                         goto 1100
                      endif
                   enddo
                else
                   errmsg(19:) = ' on ' // struct(kabssp)
                   call prterr(lundia    ,'V235'    ,errmsg(:40)          )
                   write(lundia,*) '          at m=',m,',n= ',n,' (V point)'
                   !
                   error = .true.
                   goto 1100
                endif
             endif
             !
             ! Check for discharge location or floating structure
             ! defined (allowed)
             !
             if (kabssp > 0) then
                errmsg(19:) = ' near ' // struct(kabssp)
                call prterr(lundia    ,'V236'    ,errmsg    )
             !
             endif

             ! Define Weir values or 3d fixed gate coordinates (see RDSTRU)
             ! DPHKRV = Crest level or cdwZBV
             ! UWTYPV = UWType      or cdwZTV 
             !
             if ((iabsst==3) .or. (iabsst==9) .or. (iabsst==10)) then
                dphkrv(n, m) = value1
                uwtypv(n, m) = value2
             endif
             !
             ! Define structure point masks and coefficient
             !
             kspv(n, m, 0) = istruc

             if (iabsst /= 10) then
                do k = kkmin, kkmax
                   ubrlsv(n, m, k) = brlosc
                   kspv(n, m, k) = 1
                enddo
              else
                !
                ! For CDW (KSPU(nm,0) == 10; UBRLSU == CDWLSU which is a 2 dimensional array
                !
                ubrlsv(n, m, 1) = brlosc
            endif
          enddo
       else
          errmsg(25:28) = ' (' // direct // ')'
          call prterr(lundia    ,'V233'    ,errmsg(:28)          )
          !
          error = .true.
       endif
       !
       ! <--
       !
       goto 1000
       !
       ! <--    next boundary number
       !
       !
       ! close file
       !
 1100  continue
       close (luntmp)
    !
    ! test file existence <NO>
    !
    else
       error = .true.
    endif
end subroutine strfil
