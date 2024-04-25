subroutine chkkc(lundia    ,error     ,runid     ,fldry     ,fltd      , &
               & noroco    ,norow     ,mmax      ,nmax      ,irocol    , &
               & kcu       ,kcv       ,kcs       ,gdp       )
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
!    Function: Sets the mask arrays to indicate active computa-
!              tional velocity points
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
    integer, pointer :: nmaxus
!
! Global variables
!
    integer                                                                   :: lundia !  Description and declaration in inout.igs
    integer                                                      , intent(in) :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in) :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in) :: noroco !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in) :: norow  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, noroco)                                , intent(in) :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: kcv    !  Description and declaration in esm_alloc_int.f90
    logical                                                     , intent(in)  :: fldry  !  Description and declaration in tmpfil.igs
    logical                                                     , intent(in)  :: fltd   !  Description and declaration in tmpfil.igs
    logical                                                     , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)                                                              :: runid  !!  Run identification code for the current simulation
!
! Local variables
!
    integer             :: i      ! Help var. 
    integer             :: idx1   ! Help var. 
    integer             :: idx2   ! Help var. 
    integer             :: idy1   ! Help var. 
    integer             :: idy2   ! Help var. 
    integer             :: incx   ! Increment step calculated from two x-coordinates (= 0, -1 or 1) 
    integer             :: incy   ! Increment step calculated from two Y-coordinates (= 0, -1 or 1) 
    integer             :: linod  ! Help variable to extend length file name with node number
    integer             :: lrid   ! Length of character string runid 
    integer             :: lundry ! Unit number of the unformatted file for the dry points used in the TRI- SULA simulation 
    integer             :: luntd  ! Unit number of the unformatted file for the dam points used in the TRI- SULA simulation 
    integer             :: m      ! Current M-index of the active point in the current computational ROW 
    integer             :: mendu  ! Last M-index of U-points
    integer             :: mends  ! Last M-index of WL-points
    integer             :: mstau  ! First M-index of U-points
    integer             :: mstas  ! First M-index of WL-points
    integer             :: mhalob ! First (begin) M-index of WL-points in halo area
    integer             :: mhaloe ! Last (end) M-index of WL-points in halo area
    integer             :: nhalob ! First (begin) N-index of WL-points in halo area
    integer             :: nhaloe ! Last (end) N-index of WL-points in halo area
    integer             :: m1     ! Help var. 
    integer             :: m2     ! Help var. 
    integer             :: maxinc ! Maximum of (INCXA,INCYA) 
    integer             :: mf     ! First M-index of the active point in the current computational ROW 
    integer             :: ml     ! Last M-index of the active point in the current computational ROW 
    integer             :: mld    ! One to last M-index of the active point in the current computational ROW 
    integer             :: mlu    ! One plus last M-index of the active point in the current computational ROW
    integer             :: n      ! Current N-index of the active point in the current computational COLUMN 
    integer             :: nendv  ! Last N-index of V-points
    integer             :: nstav  ! First N-index of V-points
    integer             :: n1     ! Help var. 
    integer             :: n2     ! Help var. 
    integer             :: newlun
    integer             :: nf     ! First N-index of the active point in the current computational COLUMN 
    integer             :: nl     ! Last N-index of the active point in the current computational COLUMN 
    integer             :: nld    ! One to last N-index of the active point in the current computational COLUMN 
    integer             :: nlu    ! One plus last N-index of the active point in the current computational COLUMN
    logical             :: lerror ! Local flag=TRUE if an error is encountered, but you want to follow through 
    character(1)        :: dirtd  ! Velocity point upon which the thin dam is specified (U or V) 
    character(11)       :: errmsg ! Character var. containing the errormessage to be written to file. The message depend on the error. 
    character(256)      :: filnam ! Help var. 
!
!! executable statements -------------------------------------------------------
!
    !
    nmaxus     => gdp%d%nmaxus
    !
    !
    ! initialize local parameters
    ! initialisation kcu, kcv and kcs not required (done in esm_alloc_int)
    !
    dirtd = ' '
    lerror = .false.
    !
    ! define length of runid
    !
    call remove_leading_spaces(runid     ,lrid      )
    !
    ! fill array for closed boundaries (rows)
    !
    do i = 1, norow
       n = irocol(1, i)
       mf = irocol(2, i) - 1
       ml = irocol(3, i)
       mld = ml - 1
       mlu = ml + 1
       if (irocol(4, i)==10) then      ! coupling boundary at start of row
          mhalob = min(mf+1,ihalom)
          mhaloe = min(ml,ihalom)
          mstau  = mhaloe+1
          mstas  = mstau
          do m=mhalob,mhaloe
             kcs(n,m) = -1
             kcu(n,m) = -1
          enddo
       elseif (irocol(4, i)>1) then    ! open boundary at start of row
          mstau      = mf
          mstas      = mf + 1
          kcs(n, mf) = 2
       else
          mstau = mf + 1
          mstas = mf + 1
       endif
       if (irocol(5, i)==10) then      ! coupling boundary at end of row
          mhalob = max(mf+1,mmax-ihalom+1)
          mhaloe = min(ml,mmax)
          mendu  = mhalob-1
          mends  = mendu
          do m=mhalob,mhaloe
             kcs(n,m) = -1
             kcu(n,m) = -1
          enddo
       elseif (irocol(5, i)>1) then    ! open boundary at end of row
          mendu = ml
          mends = ml
          kcs(n, mlu) = 2
       else
          mendu = mld
          mends = ml
       endif
       do m = mstau, mendu
          kcu(n, m) = 1
       enddo
       do m = mstas, mends
          kcs(n, m) = 1
       enddo
    enddo
    !
    ! fill arrays for closed boundaries (columns)
    ! Note: for parallel runs, the mask arrays in halo area are set to -1
    !
    do i = norow + 1, noroco
       m = irocol(1, i)
       nf = irocol(2, i) - 1
       nl = irocol(3, i)
       nld = nl - 1
       nlu = nl + 1
       if (irocol(4, i)==10) then      ! coupling boundary at start of column
          nhalob = min(nf+1,ihalon)
          nhaloe = min(nl,ihalon)
          nstav  = nhaloe+1
          do n=nhalob,nhaloe
             kcs(n,m) = -1
             kcv(n,m) = -1
          enddo
       elseif (irocol(4, i)>1) then    ! open boundary at start of column
          nstav = nf
          kcs(nf, m) = 2
       else
          nstav = nf + 1
       endif
       if (irocol(5, i)==10) then      ! coupling boundary at end of column
          if (mod(nmaxus,2).eq.0) then ! check if number of points is even 
             nlu=nlu-1
          endif
          nhalob = max(nf+1,nmaxus-ihalon+1)
          nhaloe = min(nl,nmaxus)
          nendv  = nhalob-1
          do n=nhalob,nhaloe
             kcs(n,m) = -1
             kcv(n,m) = -1
          enddo
       elseif (irocol(5, i)>1) then    ! open boundary at end of column
          nendv = nl
          kcs(nlu, m) = 2
       else
          nendv = nld
       endif
       do n = nstav, nendv
          kcv(n, m) = 1
       enddo
    enddo
    !
    ! set counterpart of mask arrays kcu and kcv to -1 in halo area
    !
    do i = 1, norow
       n   = irocol(1, i)
       mf  = irocol(2, i) - 1
       ml  = irocol(3, i)
       mld = ml - 1
       mlu = ml + 1
       do m = mf, mlu
          if ( (kcs(n,m)==-1) ) then
             kcv(n,m) = -1
          endif
       enddo
    enddo
    !
    do i = norow + 1, noroco
       m   = irocol(1, i)
       nf  = irocol(2, i) - 1
       nl  = irocol(3, i)
       nld = nl - 1
       nlu = nl + 1
       do n = nf, nlu
          if ( (kcs(n,m) == -1) ) then
             kcu(n,m) = -1
          endif
       enddo
    enddo
    !
    ! check if open boundary points lie within halo areas
    !
    do i = 1, norow
       n   = irocol(1, i)
       mf  = irocol(2, i) - 1
       mlu = irocol(3, i) + 1
       if ( (kcs(n,mf+1)==-1) .and. (kcs(n,mf)==2) ) then
          kcs(n,mf ) = -1
          kcu(n,mf ) = -1
       endif
       if ( (kcs(n,mlu-1)==-1) .and. (kcs(n,mlu)==2) .and. ((mlu-1)/=mmax) ) then
          kcs(n,mlu) = -1
          kcu(n,mlu) = -1
       endif
    enddo
    do i = norow + 1, noroco
       m   = irocol(1, i)
       nf  = irocol(2, i) - 1
       nlu = irocol(3, i) + 1
       if ( (kcs(nf+1,m) == -1) .and. (kcs(nf,m)==2) ) then
          kcs(nf ,m) = -1
          kcv(nf ,m) = -1
       endif
       if ( (kcs(nlu-1,m)==-1) .and. (kcs(nlu,m)==2) .and. ((nlu-1)/=nmax)) then
          kcs(nlu,m) = -1
          kcv(nlu,m) = -1
       endif
    enddo
    !
    ! initialise KCU and KCV for permanent drypoints (if fldry = .true.)
    !     open and read file till EOF
    !
    if (fldry) then
       filnam = 'TMP_' // runid(:lrid) // '.dry'
       !
       ! append node number to file name in case of parallel computing within single-domain case
       !
       linod = 0
       if ( parll ) then
          linod = 4
          write(filnam(8+lrid+1:8+lrid+linod),'(a,i3.3)') '-', inode
       endif
       !
       open (newunit=lundry, file = filnam(:8 + lrid+linod), form = 'unformatted',      &
            & status = 'old')
            ! -->
  310  continue
       read (lundry, end = 320, err = 9999) idx1, idy1, idx2, idy2
       !
      ! calculate increment and fill arrays
      ! dry point may not lie on an inactive point or boundary point
       !           errorif the line does not form multiple of 45 degrees
       !
      ! for the parallel code the effects of the enclosure is not calculated 
      ! correctly. For the moment leave the effects out (kcs, kcv, kcu sets)
      ! by commenting the lines below.
      ! Note this is an obsolete feature and seldom used.
       call increm(idx1      ,idy1      ,idx2      ,idy2      ,incx      , &
                 & incy      ,maxinc    ,lerror    )
       if (lerror) then
          call prterr(lundia    ,'V035'    ,' '       )
          !
          error  = .true.
          lerror = .false.
          goto 310
       endif
       !
       idx1 = idx1 - incx
       idy1 = idy1 - incy
       !
       do i = 1, maxinc + 1
          idx1 = idx1 + incx
          idy1 = idy1 + incy
          write (errmsg, '(''('',i4,'','',i4,'')'')') idx1, idy1
          if (kcs(idy1, idx1)==0) then
             call prterr(lundia    ,'V036'    ,errmsg    )
          !
          elseif (kcs(idy1, idx1)==2) then
             call prterr(lundia    ,'V037'    ,errmsg    )
             !
             error = .true.
          else
             kcs(idy1, idx1) = 0
             kcv(idy1, idx1) = 0
             kcv(idy1 - 1, idx1) = 0
             kcu(idy1, idx1) = 0
             kcu(idy1, idx1 - 1) = 0
          endif
       enddo
       goto 310
       ! <--
  320  continue
       close (lundry, status = 'delete')
    endif
    !
    ! initialise KCU and KCV for thin dams (if fltd  = .true.)
    !     open and read file till EOF
    !
    if (fltd) then
       filnam = 'TMP_' // runid(:lrid) // '.td '
       !
       ! append node number to file name in case of parallel computing within single-domain case
       !
       linod = 0
       if ( parll ) then
          linod = 4
          write(filnam(7+lrid+1:7+lrid+linod),'(a,i3.3)') '-', inode
       endif
       !
       open (newunit=luntd, file = filnam(:7 + lrid+linod), form = 'unformatted',       &
            & status = 'old')
            ! -->
  410  continue
       read (luntd, end = 420, err = 7777) idx1, idy1, idx2, idy2, dirtd
       !
       ! calculate increment and fill arrays
       !           errorif the line does not form multiple of 45 degrees
       !
       call increm(idx1      ,idy1      ,idx2      ,idy2      ,incx      , &
                 & incy      ,maxinc    ,lerror    )
       if (lerror) then
          call prterr(lundia    ,'V038'    ,' '       )
          !
          error  = .true.
          lerror = .false.
          goto 410
       endif
       !
       idx1 = idx1 - incx
       idy1 = idy1 - incy
       !
       ! thin dam may not lie outside model; it's direction must be
       !           either 'U' or 'V' and it should not lie on an inactive point
       !
       do i = 1, maxinc + 1
          idx1 = idx1 + incx
          idy1 = idy1 + incy
          if (idx1>mmax .or. idy1>nmax .or. idx1<1 .or. idy1<1) then
             call prterr(lundia    ,'V040'    ,errmsg    )
             !
             error = .true.
             goto 9999
          endif
          if (dirtd/='U' .and. dirtd/='V') then
             call prterr(lundia    ,'U010'    ,' '       )
             !
             error = .true.
          elseif (dirtd=='U' .and. kcu(idy1, idx1)/=0) then
             kcu(idy1, idx1) = 0
          elseif (dirtd=='V' .and. kcv(idy1, idx1)/=0) then
             kcv(idy1, idx1) = 0
          else
             write (errmsg, '(''('',i4,'','',i4,'')'')') idx1, idy1
             call prterr(lundia    ,'V039'    ,errmsg    )
          endif
       enddo
       goto 410
       ! <--
  420  continue
       close (luntd, status = 'delete')
    endif
    goto 9999
 7777 continue
    call prterr(lundia    ,'G007'    ,filnam    )
 9999 continue
end subroutine chkkc
