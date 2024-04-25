subroutine inigrd(lundia    ,error     ,runid     ,nmax      ,mmax      , &
                & nmaxus    ,icom      ,ipx       ,ipy       ,gdp       )
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
!    Function: - Reads the computational grid enclosure file
!              - Lines formed by the vertices of the polygons are
!                checked against the condition of 45 degrees
!              - Fills ICOM with the appropriate values to iden-
!                tify the active/inactive points
! Method used:
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
    integer, pointer     :: nmaxgl
    integer, pointer     :: mmaxgl
!
! Global variables
!
    integer                                                            :: lundia !  Description and declaration in inout.igs
    integer      , intent(in)                                          :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer      , intent(in)                                          :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer      , intent(in)                                          :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(*)                                        :: ipx    !!  X-coordinate of the comp. grid enclosure (temporary/work array)
    integer      , dimension(*)                                        :: ipy    !!  Y-coordinate of the comp. grid enclosure (temporary/work array)
    integer      , dimension(gdp%d%mlb:gdp%d%mub, gdp%d%nlb:gdp%d%nub) :: icom   !!  Temporary work array containing:0 = if a point is not active,1 = if a point is active
    logical                                                            :: error  !!  Flag=TRUE if an error is encountered
    character(*)                                                       :: runid  !!  Run identification code for the current simulation
!
! Local variables
!
    integer                        :: icode   ! Help var. 
    integer                        :: incx    ! Increment step calculated from two x-coordinates (= 0, -1 or 1) 
    integer                        :: incy    ! Increment step calculated from two Y-coordinates (= 0, -1 or 1) 
    integer                        :: incy0   ! Help var. 
    integer                        :: incy1   ! Help var. 
    integer                        :: ipoly   ! Help var. - counter for the current polygon number 
    integer                        :: ival    ! Help var. 
    integer                        :: ix      ! Help var. 
    integer                        :: iy      ! Help var. 
    integer                        :: linod   ! Help variable to extend length file name with node number
    integer                        :: lrid    ! Length of character string runid 
    integer                        :: lungrd  ! Unit number of the unformatted file containing the computational grid enclosure 
    integer                        :: m       ! Current M-index of the active point in the current computational ROW 
    integer                        :: maxinc  ! Maximum of (INCXA,INCYA) 
    integer                        :: maxnrp  ! Local dim. (max. of NRP allowed) 
    integer                        :: mgrd    ! Help var. 
    integer                        :: n       ! Current N-index of the active point in the current computational COLUMN 
    integer                        :: nd      ! Current N-index minus 1 (see N) 
    integer                        :: ndd     ! ND-1 
    integer                        :: ndum    ! Help var. 
    integer                        :: newlun
    integer                        :: ngrd    ! Help var. 
    integer                        :: nrp     ! Counter for the number of points in the current polygon 
    integer                        :: nu      ! Current N-index plus  1 (see N) 
    integer                        :: nuu     ! NU+1 
    character(16)                  :: errmsg  ! Character var. containing the error message to be written to file. The message depend on the error. 
    character(256)                 :: fixid   ! fixed size version of runid, needed for character concatenation 
    character(256)                 :: filnam  ! Help var.
    character(4)                   :: errornr ! Number of the error message which will be printed in case of error
!
!! executable statements -------------------------------------------------------
!
    mmaxgl => gdp%gdparall%mmaxgl
    nmaxgl => gdp%gdparall%nmaxgl
    !
    !
    ! initialisation local parameters
    ! maxnrp is check for array dimensions
    !
    do n = 1, nmax
       do m = 1, mmax
          icom(m, n) = 0
       enddo
    enddo
    !
    ipoly = 0
    ndum = 0
    mgrd = 0
    ngrd = 0
    !
    errornr = 'G006'
    errmsg = ' '
    !
    maxnrp = nmax*(mmax + 4)
    !
    ! define length of runid and put in fixed size array
    ! size is tested in iniid
    !
    call remove_leading_spaces(runid     ,lrid      )
    fixid(1:lrid) = runid(1:lrid)
    !
    ! open unformatted scratch file for grid (see rdgrid)
    !
    filnam = 'TMP_' // fixid(1:lrid) // '.grd'
    !
    ! append node number to file name in case of parallel computing within single-domain case
    !
    linod = 0
    if ( mmaxgl /= mmax .or. nmaxgl /= nmaxus ) then
       linod = 4
       write(filnam(8+lrid+1:8+lrid+linod),'(a,i3.3)') '-', inode
    endif
    !
    open (newunit=lungrd, file = filnam(:8 + lrid+linod),                      &
        & form = 'unformatted', status = 'old')
    !
    ! in case of parallel Delft3D-FLOW array ICOM is already stored in scratch file
    ! (see routine strgrd)
    !
    if ( mmaxgl /= mmax .or. nmaxgl /= nmaxus ) then
       read (lungrd, end = 8888, err = 7777) ((icom(m,n), m=1, mmax), n=1, nmax)
       goto 9999
    endif
    !
    ! read first coordinate pair for first polygon
    !
    read (lungrd, end = 8888, err = 7777) mgrd, ngrd
    ipx(1) = mgrd
    ipy(1) = ngrd
    !
    ! --> read for all polygons
    !
  110 continue
    nrp = 1
    ipoly = ipoly + 1
    !
    ! check mgrd and ngrd aginst 1,1 and mmax,nmaxus
    !
    if (mgrd>mmax .or. ngrd>nmaxus .or. mgrd<1 .or. ngrd<1) then
       errornr = 'U004'
       write (errmsg, '(2i8)') mgrd, ngrd
       goto 8888
    endif
    !
    ! --->   read all points of one polygon
    !
  120 continue
    read (lungrd, err = 1000, end = 1001) mgrd, ngrd
    ipx(nrp + 1) = mgrd
    ipy(nrp + 1) = ngrd
    !
    ! check mgrd and ngrd against 1,1 and mmax,nmaxus
    !
    if (mgrd>mmax .or. ngrd>nmaxus .or. mgrd<1 .or. ngrd<1) then
       errornr = 'U004'
       write (errmsg, '(2i8)') mgrd, ngrd
       goto 8888
    endif
    !
    if (ipx(nrp + 1)/=ipx(1) .or. ipy(nrp + 1)/=ipy(1)) then
       nrp = nrp + 1
       if (nrp>maxnrp) then
          errornr = 'V020'
          goto 8888
       endif
       ! <---
       goto 120
    endif
    !
    ! fill non-horizontal "enclosure points"
    ! all line segment checked (loop 1-nrp and segment (n,n+1))
    ! Passing on parameter ndum twice works fine
    !
    call increm(ndum      ,ipy(nrp)  ,ndum      ,ipy(1)    ,incx      , &
              & incy      ,maxinc    ,error     )
    do n = 1, nrp
       incy0 = incy
       call increm(ipx(n)    ,ipy(n)    ,ipx(n + 1),ipy(n + 1),incx      , &
                 & incy      ,maxinc    ,error     )
       write (errmsg, '(4i4)') ipx(n), ipy(n), ipx(n + 1), ipy(n + 1)
       !
       ! if error then vertice not multiple of 45 degrees
       !
       if (error) then
          errornr = 'V021'
          goto 8888
       endif
       !
       ! fill linesegment with a code = 2, for vertical enclosure
       ! polygon
       !
       if (incy/=0) then
          ix = ipx(n)
          iy = ipy(n)
          ival = 2
          do m = 1, maxinc - 1
             ix = ix + incx
             iy = iy + incy
             !
             ! check if the lines intersects
             ! it is assumed that corner points are not coinciding
             !
             icom(ix, iy) = ival
          enddo
          !
          ! fill vertices
          !
          icom(ipx(n), ipy(n)) = 2
          if (incy0*incy<0) then
             icom(ipx(n), ipy(n)) = 3
          endif
          if (incy0==0) then
             nd = max(1, n - 1)
             ndd = max(1, nd - 1)
             incy1 = ipy(nd) - ipy(ndd)
             if (incy1*incy<0) then
                icom(ipx(n), ipy(n)) = 3
             endif
          endif
       endif
    enddo
    !
    ! fill horizontal enclosure points
    ! Passing on parameter ndum twice works fine
    !
    call increm(ndum      ,ipy(nrp)  ,ndum      ,ipy(1)    ,incx      , &
              & incy      ,maxinc    ,error     )
    do n = 1, nrp
       incy0 = incy
       call increm(ipx(n)    ,ipy(n)    ,ipx(n + 1),ipy(n + 1),incx      , &
                 & incy      ,maxinc    ,error     )
       write (errmsg, '(4i4)') ipx(n), ipy(n), ipx(n + 1), ipy(n + 1)
       !
       if (incy==0) then
          ix = ipx(n)
          iy = ipy(n)
          ival = -abs(ipx(n + 1) - ipx(n))
          do m = 1, maxinc
             ix = ix + incx
             iy = iy + incy
             !
             ! check if the lines intersects
             ! it is assumed that corner points are not coinciding
             !
             icom(ix, iy) = ival
          enddo
          !
          ! fill vertices
          !
          icom(ipx(n), ipy(n)) = 2
          nu = n + 1
          if (nu>nrp) nu = 1
          nuu = nu + 1
          if (nuu>nrp) nuu = 1
          incy1 = ipy(nuu) - ipy(nu)
          if (incy1*incy0<0) then
             icom(ipx(n), ipy(n)) = 3
          endif
       endif
    enddo
    !
    ! read new polygon, if any
    !
    read (lungrd, err = 1000, end = 1001) mgrd, ngrd
    ipx(1) = mgrd
    ipy(1) = ngrd
    goto 110
    !
    ! <--
 1000 continue
    error = .true.
    !
    ! Begin and end points of polygon not identical -> error;
    ! Fill icom if no error is encountered
    !
 1001 continue
    if (error .or. (ipx(nrp + 1)/=ipx(1) .or. ipy(nrp + 1)/=ipy(1))) then
       errornr = 'V022'
       write (errmsg, '(i16)') ipoly
       goto 8888
    else
       do n = 1, nmax
          icode = 0
          m = 1
          ! -->
          !
          ! build matrix with computational points
          !
 1120     continue
          if (icom(m, n)<0) then
             m = m - icom(m, n)
          elseif (icom(m, n)==2) then
             icode = 1 - icode
             m = m + 1
          elseif (icom(m, n)==3) then
             m = m + 1
          else
             icom(m, n) = icode
             m = m + 1
          endif
          if (m<mmax) goto 1120
       ! <--
       enddo
    endif
    goto 9999
    !
 7777 continue
    errornr = 'G007'
 8888 continue
    error = .true.
 9999 continue
    if (error) call prterr(lundia, errornr, errmsg)
    !
    !
    ! close and delete unformatted scratch file for grid (see rdgrid)
    !
    close (lungrd)
end subroutine inigrd
