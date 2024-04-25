subroutine inicut(lundia    ,error     ,runid     ,nmax      ,mmax      , &
                & nmaxus    ,kmax      ,flcut     ,fl45      ,kcu       , &
                & kcv       ,kcs       ,kfsmin    ,kfsmax    ,kcu45     , &
                & kcv45     ,kcscut    ,xcor      ,ycor      ,gud       , &
                & guu       ,guv       ,guz       ,gvd       ,gvu       , &
                & gvv       ,gvz       ,gsqs      ,gsqiu     ,gsqiv     , &
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
! Reads the file that defines stair case (closed) boundary
! Two separate cases are recognised:
! a. 45 degrees staircase closed boundary;
!    the boundary points are specified
!    at input like permanent dry points
! b. 1:n cut cells boundary (non-orthogonal boundary cells).
!    The boundary segments are specified like thin dams.
!    (direction of 'dams' to be included')
! If 45 deg. staircase boundary is specified:
!    fill KCU/V45 accordingly;
!    KCU45 and KCV45 are both defined at zeta points
!    KCU45 is used for Call in X-direction and KCV45 for Call in Y-direction.
!    Reduce the horizontal area with a factor 2.
! If 1:n cut cells boundary is specified:
!    fill KCSCUT with 1 (default values: 0 (regular active point)
!                                       -1 (inactive and open boundary points)
!    Recompute GSQS, GUV, GVU, GSQIU, GSQIV, GUD, GVD, GUZ and GVZ for Cut Cells.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                                           , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                         :: lundia !  Description and declaration in inout.igs
    integer                                                           , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                           , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                           , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                    :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                    :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)              :: kcscut !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(out) :: kcu45  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(out) :: kcv45  !  Description and declaration in esm_alloc_int.f90
    logical, intent(in)                                                             :: fl45
    logical, intent(in)                                                             :: flcut
    logical                                                                         :: error
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                   :: gsqiu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                   :: gsqiv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                   :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                   :: gud    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(out) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                   :: guz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                   :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(out) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                   :: gvz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(in)  :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(in)  :: ycor   !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                    :: runid
!
! Local variables
!
    integer                        :: incx          ! Increment step calculated from two x-coordinates (= 0, -1 or 1)
    integer                        :: incy          ! Increment step calculated from two Y-coordinates (= 0, -1 or 1)
    integer                        :: ix
    integer                        :: linod         ! Help variable to extend length file name with node number
    integer                        :: k
    integer                        :: kcstot
    integer                        :: kenm
    integer                        :: lrid          ! Length of character string runid
    integer                        :: luntmp
    integer                        :: m             ! Current M-index of the active point in the current computational ROW
    integer                        :: m1grd
    integer                        :: m2grd
    integer                        :: maxinc        ! Maximum of (INCXA,INCYA)
    integer                        :: md
    integer                        :: mu
    integer                        :: n             ! Current N-index of the active point in the current computational COLUMN
    integer                        :: n1grd
    integer                        :: n2grd
    integer                        :: nd            ! Current N-index minus 1 (see N)
    integer                        :: nrp           ! Counter for the number of points in the current polygon
    integer                        :: nu            ! Current N-index plus  1 (see N)
    integer, external              :: newlun
    real(fp)                       :: area
    real(fp)                       :: check_dir
    real(fp)                       :: check_kcstot1
    real(fp)                       :: check_kcstot2
    real(fp)                       :: gu
    real(fp)                       :: gv
    real(fp), dimension(4)         :: xx
    real(fp), dimension(4)         :: yy
    character(1)                   :: dir
    character(20)                  :: errmsg        ! Character var. containing the errormessage to be written to file. The message depend on the error.
    character(256)                 :: fixid         ! fixed size version of runid, needed for character concatenation
    character(256)                 :: filnam        ! Help var.
    character(4)                   :: errornr        ! Number of the errormessage which will be printed in case of error
!
!! executable statements -------------------------------------------------------
!
    m1grd = 0
    n1grd = 0
    m2grd = 0
    n2grd = 0
    nrp = 0
    !
    errornr = 'G006'
    errmsg = '1-NMAX and 1-MMAX'
    !
    ! Initialisation; set all points as normal points by default
    !
    do n = 1, nmax
       do m = 1, mmax
          do k = 1, kmax
             kcu45(n, m, k) = 0
             kcv45(n, m, k) = 0
             if (kcs(n, m)/=1 .and. kcs(n, m)/=-1) then 
                kcscut(n, m, k) = -1
             else
                kcscut(n, m, k) = 0
             endif
          enddo
       enddo
    enddo
    !
    ! define length of runid and put in fixed size array
    ! size is tested in iniid
    !
    call remove_leading_spaces(runid     ,lrid      )
    fixid(1:lrid) = runid(1:lrid)
    !
    if (.not.fl45) goto 34
    !
    ! open unformatted scratch file for 45 degrees staircase boundary
    ! (see rdgrid)
    !
    filnam = 'TMP_' // fixid(1:lrid) // '.45'
    !
    ! append node number to file name in case of parallel computing within single-domain case
    !
    linod = 0
    if ( parll ) then
       linod = 4
       write(filnam(7+lrid+1:7+lrid+linod),666) inode
    endif
    !
    open (newunit=luntmp, file = filnam(:7 + lrid+linod), form = 'unformatted',     &
        & status = 'old')
        !
        ! --> read all points
        !
   10 continue
       read (luntmp, err = 7777, end = 34) m1grd, n1grd, m2grd, n2grd
       !
       ! check mgrd and ngrd against 1,1 and mmax,nmaxus
       !
       if (m1grd>mmax .or. n1grd>nmaxus .or. m1grd<1 .or. n1grd<1 .or.             &
         & m2grd>mmax .or. n2grd>nmaxus .or. m2grd<1 .or. n2grd<1) then
          errornr = 'U004'
          goto 8888
       endif
       !
       nrp = nrp + 1
       !
       ! Determine increment and maximum increment
       !
       call increm(m1grd     ,n1grd     ,m2grd     ,n2grd     ,incx      , &
                 & incy      ,maxinc    ,error     )
       write (errmsg, '(4i5)') m1grd, n1grd, m2grd, n2grd
       !
       ! if erroror lines are not 45 degrees -> error
       !
       if (error.or. abs(incx)/=abs(incy)) then
          errornr = 'V082'
          goto 8888
       endif
       !
       ! fill KCU/V45 for all the cells along the specified line segment
       ! KCU45 and KCV45 are determined by an arithmetic expression from
       ! KCU and KCV (value between brackets is the multiplication factor):
       !
       !        ! (2)                ! (1)
       !
       ! (4) -  +  - (1)      (2) -  +  - (8)
       !
       !        ! (8)                ! (4)
       !
       ! The values are used to determine the direction of the 45 degrees
       ! boundary (left upper corner, left bottom corner etc. etc.)
       !
       ! Adjust (reduce) GSQS area for this cell with a factor of 2
       !
       m = m1grd - incx
       n = n1grd - incy
       !
       mn_loop_kc45:do ix = 1, maxinc + 1
          m = m + incx
          n = n + incy
          !
          ! if kcs = 0 or kcs = 2 error
          ! and kcu/v45 must be either 3, 6, 9 or 12 (this point
          ! must lie adjacent to two dry points)
          ! Rectify
          !
          if (kcs(n, m)==0 .or. kcs(n, m)==2) then
             errornr = 'V083'
             write (errmsg, '(2i10)') m, n
             goto 8888
          endif
          nd = max(1, n - 1)
          md = max(1, m - 1)
          nu = min(nmaxus, n + 1)
          mu = min(mmax, m + 1)
          kcstot = kcs(n, mu) + kcs(nd, m) + kcs(nu, m) + kcs(n, md)
          if (kcstot/=2) then
             errornr = 'V084'
             write (errmsg, '(2i8)') m, n
             goto 8888
          endif
          !
          gsqs(n, m) = 0.5*gsqs(n, m)
          !
          set_kc45:do k = 1, kmax
             kcu45(n, m, k) = kcu(n, m) + 2*kcv(n, m) + 4*kcu(n, md) + 8*kcv(nd, m)
             kcv45(n, m, k) = kcv(n, m) + 2*kcu(n, m) + 4*kcv(nd, m) + 8*kcu(n, md)
          enddo set_kc45
       enddo mn_loop_kc45
    goto 10
    !
    ! No errorwhile reading from 45 deg stair case file
    !
   34 continue
    if (nrp>0) then
       call prterr(lundia    ,'G051'    ,'45 deg. Staircase Boundary read'          )
       nrp = 0
       close (luntmp, status = 'delete')
    endif
    !
    if (.not.flcut) goto 134
    !
    ! open unformatted scratch file for 1:n staircase boundary
    ! (see rdgrid)
    !
    filnam = 'TMP_' // fixid(1:lrid) // '.cut'
    !
    ! append node number to file name in case of parallel computing within single-domain case
    !
    linod = 0
    if ( parll ) then
       linod = 4
       write(filnam(8+lrid+1:8+lrid+linod),666) inode
    endif
    !
    open (newunit=luntmp, file = filnam(:8 + lrid+linod),                     &
        & form = 'unformatted', status = 'old')
        !
        ! --> read all points
        !
  110 continue
       read (luntmp, err = 7777, end = 134) m1grd, n1grd, m2grd, n2grd, dir
       !
       ! check mgrd and ngrd against 1,1 and mmax,nmaxus
       !
       if (m1grd>mmax .or. n1grd>nmaxus .or. m1grd<1 .or. n1grd<1 .or.             &
         & m2grd>mmax .or. n2grd>nmaxus .or. m2grd<1 .or. n2grd<1) then
          errornr = 'U004'
          goto 8888
       endif
       !
       nrp = nrp + 1
       !
       ! Determine increment and maximum increment
       !
       call increm(m1grd     ,n1grd     ,m2grd     ,n2grd     ,incx      , &
                 & incy      ,maxinc    ,error     )
       write (errmsg, '(4i5)') m1grd, n1grd, m2grd, n2grd
       !
       ! if error-> error
       !
       if (error) then
          errornr = 'V085'
          goto 8888
       endif
       !
       m = m1grd - incx
       n = n1grd - incy
       !
       ! fill KCSCUT for all the points along the specified line segment.
       ! Adjust the value of various grid parameters afterwards
       ! Distances are reflected from neigbouring normal cell.
       !
       mn_loop_kcscut:do ix = 1, maxinc + 1
          m = m + incx
          n = n + incy
          mu = min(m + 1, mmax)
          nu = min(n + 1, nmaxus)
          nd = max(1, n - 1)
          md = max(1, m - 1)
          write (errmsg, '(2i10)') m, n
          error= .false.
          !
          ! if kcs = 2 (boundary point) error
          !
          if (kcs(n, m)==2) then
             errornr = 'V086'
             error= .true.
             goto 8888
          endif
          !
          ! check direction of cut cells and alignment with the thin dams
          !
          checkdir:select case (dir)
          case ('U')
             if (kcu(n, m) > 0) then
                error= .true.
             else
                kcstot = kcs(n, m) + kcs(n, mu)
                checkkcstot1:select case (kcstot)
                case default
                   error= .true.
                case (1:)
                   if (kcscut(n, mu, 1)/= - 1) kcscut(n, mu, 1) = 1
                   if (kcscut(n, m, 1)/= - 1) kcscut(n, m, 1) = 1
                end select checkkcstot1
             endif
          case ('V')
             if (kcv(n, m) > 0) then
                error= .true.
             else
                kcstot = kcs(n, m) + kcs(nu, m)
                checkkcstot2:select case (kcstot)
                case default
                   error= .true.
                case (1:)
                   if (kcscut(nu, m, 1)/= - 1) kcscut(nu, m, 1) = 1
                   if (kcscut(n, m, 1)/= - 1) kcscut(n, m, 1) = 1
                end select checkkcstot2
             endif
          case default
             call prterr(lundia    ,'U010'    ,' '       )
             error= .true.
          end select checkdir
          !
          if (error) then
             errornr = 'V087'
             goto 8888
          endif
          !
          set_kcscut:do k = 2, kmax
             kcscut(n, m, k) = kcscut(n, m, 1)
          enddo set_kcscut
       enddo mn_loop_kcscut
    goto 110
    !
    ! End of file or no data read
    !
  134 continue
    if (nrp>0) then
       call prterr(lundia    ,'G051'    ,'Cut-cell Boundary read'        )
       close (luntmp, status = 'delete')
    endif
    !
    ! No errorwhile reading from cut-cell file
    ! Check Whether KCSCUT fulfills the following convention
    ! Not allowed 3 cut cells forming L-shape or square,
    ! i.e. KCSCUT having values:       1 1    1 1     1      1      1 1
    !                                    1    1       1 1    1 1    1 1
    !
    check_kcscut:do m = 2, mmax - 1
       do n = 2, nmaxus - 1
          kcstot = max(0, kcs(n, m)*kcscut(n, m, 1))                            &
                 & + max(0, kcs(n - 1, m)*kcscut(n - 1, m, 1))                  &
                 & + max(0, kcs(n - 1, m + 1)*kcscut(n - 1, m + 1, 1))          &
                 & + max(0, kcs(n, m + 1)*kcscut(n, m + 1, 1))
          if (kcstot>=3) then
             write (errmsg(1:10), '(2i5)') m, n
             call prterr(lundia    ,'v088'    ,errmsg(1:10)         )
             error= .true.
          endif
       enddo
    enddo check_kcscut
    if (error) goto 8888
    !
    ! Reset Computation of GUV only if one of the GUU values <> 0.
    ! (KCS's <> 0) (GUV is GUU at V-velocity point)
    !
    reset_guv:do m = 1, mmax
       md = max(1, m - 1)
       mu = min(m + 1, mmax)
       do n = 1, nmaxus
          nu = min(n + 1, nmaxus)
          if (kcscut(n, m, 1)==1 .and. kcscut(nu, m, 1)==0) then
             guv(n, m) = guz(nu, m)
          elseif (kcscut(n, m, 1)==0 .and. kcscut(nu, m, 1)==1) then
             guv(n, m) = guz(n, m)
          elseif (kcscut(n, m, 1)==1 .and. kcscut(nu, m, 1)==1) then
             if (kcscut(n, md, 1)==0 .and. kcscut(nu, md, 1)==0) then
                guv(n, m) = gud(n, md)
             elseif (kcscut(n, mu, 1)==0 .and. kcscut(nu, mu, 1)==0) then
                guv(n, m) = gud(n, m)
             else
                write (errmsg(1:10), '(2i5)') m, n
                call prterr(lundia    ,'v075'    ,errmsg(1:10)         )
             endif
          else
          endif
       enddo
    enddo reset_guv
    !
    ! Reset Computation of GVU only if one of the GVV values <> 0.
    ! (GVU is GVV at U-velocity point)
    !
    reset_gvu:do m = 1, mmax
       mu = min(m + 1, mmax)
       do n = 1, nmaxus
          nd = max(n - 1, 1)
          nu = min(n + 1, nmaxus)
          if (kcscut(n, m, 1)==1 .and. kcscut(n, mu, 1)==0) then
             gvu(n, m) = gvz(n, mu)
          elseif (kcscut(n, m, 1)==0 .and. kcscut(n, mu, 1)==1) then
             gvu(n, m) = gvz(n, m)
          elseif (kcscut(n, m, 1)==1 .and. kcscut(n, mu, 1)==1) then
             if (kcscut(nd, m, 1)==0 .and. kcscut(nd, mu, 1)==0) then
                gvu(n, m) = gvd(nd, m)
             elseif (kcscut(nu, m, 1)==0 .and. kcscut(nu, mu, 1)==0) then
                gvu(n, m) = gvd(n, m)
             else
                write (errmsg(1:10), '(2i5)') m, n
                call prterr(lundia    ,'v076'    ,errmsg(1:10)         )
             endif
          else
          endif
       enddo
    enddo reset_gvu
    !
    ! Recompute: GSQS for CUT cells
    ! general formula for quadrangle
    !
    comp_gsqi:do m = mmax, 1, -1
       md = max(m - 1, 1)
       mu = min(m + 1, mmax)
       do n = 1, nmaxus
          nd = max(1, n - 1)
          nu = min(n + 1, nmaxus)
          if (kcscut(n, m, 1)==1) then
             xx(1) = xcor(n, m)
             yy(1) = ycor(n, m)
             xx(2) = xcor(n, md)
             yy(2) = ycor(n, md)
             xx(3) = xcor(nd, md)
             yy(3) = ycor(nd, md)
             xx(4) = xcor(nd, m)
             yy(4) = ycor(nd, m)
             call area4(xx        ,yy        ,area      )
             if (area<=0) then
                gsqs(n, m) = 1.0
                write (errmsg(1:10), '(2i5)') m, n
                call prterr(lundia    ,'v089'    ,errmsg(1:10)         )
             else
                gsqs(n, m) = area
             endif
          endif
       enddo
    enddo comp_gsqi
    !
    ! Recomputation of GUD, GUZ, GVD, GVZ, GSQSIU and GSQSIV
    ! for CUT-cells
    ! Information from cells in interior area is used (normal cells)
    ! For corner point, information from direction normal cell
    ! is used. Here we apply the assumption that L-shape cut cells
    ! are not allowed (see CHECK_KCSCUT above)
    !
    do m = 1, mmax
       md = max(1, m - 1)
       mu = min(m + 1, mmax)
       do n = 1, nmaxus
          nu = min(n + 1, nmaxus)
          nd = max(1, n - 1)
          if (kcscut(n, m, 1)==1) then
             if (kcv(n, m)==0 .and. kcscut(nd, m, 1)==0) then
                !
                ! closed boundary at top of cell
                !
                gvd(n, m) = gvd(nd, m)
                gvd(n, md) = gvd(nd, md)
                gud(nd, m) = guu(nd, m)
                gud(nd, md) = guu(nd, md)
                gud(n, m) = gud(nd, m)
                gud(n, md) = gud(nd, md)
                guz(n, m) = guz(nd, m)
                gvz(n, m) = gvv(nd, m)
             elseif (kcv(nd, m)==0 .and. kcscut(nu, m, 1)==0) then
                !
                ! closed boundary at bottom of cell
                !
                gvd(nd, m) = gvd(n, m)
                gvd(nd, md) = gvd(n, md)
                gud(n, m) = guu(nu, m)
                gud(n, md) = guu(nu, md)
                gud(nd, m) = gud(n, m)
                gud(nd, md) = gud(n, md)
                guz(n, m) = guz(nu, m)
                gvz(n, m) = gvv(n, m)
             else
             endif
             if (kcu(n, m)==0 .and. kcscut(n, md, 1)==0) then
                !
                ! closed boundary at right side of cell
                !
                gvd(n, md) = gvv(n, md)
                gvd(nd, md) = gvv(nd, md)
                gvd(n, m) = gvd(n, md)
                gvd(nd, m) = gvd(nd, md)
                gud(n, m) = gud(n, md)
                gud(nd, m) = gud(nd, md)
                guz(n, m) = guu(n, md)
                gvz(n, m) = gvz(n, md)
             elseif (kcu(n, md)==0 .and. kcscut(n, mu, 1)==0) then
                !
                ! closed boundary at left side of cell
                !
                gvd(n, m) = gvv(n, mu)
                gvd(nd, m) = gvv(nd, mu)
                gvd(n, md) = gvd(n, m)
                gvd(nd, md) = gvd(nd, m)
                gud(n, md) = gud(n, m)
                gud(nd, md) = gud(nd, m)
                guz(n, m) = guu(n, m)
                gvz(n, m) = gvz(n, mu)
             else
             endif
          endif
       enddo
    enddo
    goto 9999
    !
 7777 continue
    errornr = 'G007'
 8888 continue
    error= .true.
 9999 continue
    if (error) call prterr(lundia, errornr, errmsg)
    !
 666  format('-',i3.3)
end subroutine inicut
