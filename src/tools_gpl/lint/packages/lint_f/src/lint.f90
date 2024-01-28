subroutine main_lint ()
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_lint
    use lint_version_module
    implicit none
!
! Local parameters
!
    integer, parameter :: mnmax = 100000, npmax = 10000
    ! COMMON variables
    !
    integer :: ncol
    common /coln  / ncol
!
! Local variables
!
    integer                           :: i
    integer                           :: ic
    integer                           :: icu
    integer                           :: icv
    integer                           :: icx
    integer                           :: icy
    integer                           :: ip
    integer                           :: ipoly
    integer                           :: ipp
    integer                           :: iprint
    integer                           :: ipt
    integer                           :: itel
    integer                           :: k
    integer                           :: lunlog
    integer                           :: m
    integer                           :: mn
    integer                           :: n
    integer                           :: np
    integer                           :: npoly
    integer                           :: npt
    integer                           :: nsteps
    integer                           :: j
    integer, dimension(:,:), pointer  :: iref
    integer, dimension(:)  , pointer  :: code
    integer, dimension(:)  , pointer  :: iflag
    integer, dimension(:)  , pointer  :: np0
    integer, dimension(:)  , pointer  :: nrin
    integer, dimension(:)  , pointer  :: nrx
    integer, dimension(:)  , pointer  :: nry
    logical                           :: ex
    logical                           :: intact
    real(hp)                          :: ang
    real(hp)                          :: crostra
    real(hp)                          :: crosvec
    real(hp)                          :: cumdist
    real(hp)                          :: cumtra
    real(hp)                          :: cumtraneg
    real(hp)                          :: cumtrapos
    real(hp)                          :: dist
    real(hp)                          :: dx
    real(hp)                          :: dy
    real(hp)                          :: um
    real(hp)                          :: vm
    real(hp)                          :: xm
    real(hp)                          :: ym
    real(hp), dimension(15)           :: val
    real(hp), dimension(:,:), pointer :: w
    real(hp), dimension(:)  , pointer :: u
    real(hp), dimension(:)  , pointer :: v
    real(hp), dimension(:)  , pointer :: x
    real(hp), dimension(:)  , pointer :: y
    real(hp), dimension(:)  , pointer :: xpoly
    real(hp), dimension(:,:), pointer :: xpoly0
    real(hp), dimension(:)  , pointer :: xs
    real(hp), dimension(:)  , pointer :: ypoly
    real(hp), dimension(:,:), pointer :: ypoly0
    real(hp), dimension(:)  , pointer :: ys
    real(hp), dimension(:)  , pointer :: up
    real(hp), dimension(:)  , pointer :: vp
    real(hp), dimension(:)  , pointer :: xp
    real(hp), dimension(:)  , pointer :: yp
    real(hp)                          :: xymiss
    character(120)                    :: cident
    character(256)                    :: error1
    character(256)                    :: error2
    character(256)                    :: fpol
    character(256)                    :: fvect
    character(256)                    :: inpnam
    character(4)                      :: blname
!
!
!! executable statements -------------------------------------------------------
!
    ! Allocate
    !
    allocate(iref(4, npmax))
    allocate(code(mnmax))
    allocate(iflag(npmax))
    allocate(np0(1000))
    allocate(nrin(npmax))
    allocate(nrx(npmax))
    allocate(nry(npmax))
    allocate(w(4, npmax))
    allocate(u(mnmax))
    allocate(v(mnmax))
    allocate(x(mnmax))
    allocate(y(mnmax))
    allocate(xpoly(npmax))
    allocate(xpoly0(10000,1000))
    allocate(xs(npmax))
    allocate(ypoly(npmax))
    allocate(ypoly0(10000,1000))
    allocate(ys(npmax))
    allocate(up(npmax))
    allocate(vp(npmax))
    allocate(xp(npmax))
    allocate(yp(npmax))
    !
    !
    ! LINT computes the line integral of a 2D vector quantity over
    !      specified polylines
    !
    cident = ' '
    call getfullversionstring_LINT(cident)
    !
    call pldep
    !
    k = len_trim(cident)
    write(*, '(A)')
    write(*, '(A)') cident(5:k)
    write(*, '(A)')
    write(*, '(A,$)') 'Input filename: '
    read (*, '(A)') inpnam
    !
    intact = .false.
    if (inpnam==' ') then
       intact = .true.
    else
       inquire (file = trim(inpnam), exist = ex)
       if (.not.ex) intact = .true.
    endif
    !
    write(*, '(A)')
    write(*, '(a)') 'Diagnostics will be written to file lint.log'
    !
    lunlog = 10
    open (10, file = 'lint.log')
    k = len_trim(cident)
    write(10, '(A)')
    write(10, '(A)') cident(5:k)
    write(10, '(A)')
    !
    !     Read user input interactive
    !
    if (intact) then
       open (9, file = 'lint.inp')
       write( *, '(a)') 'Your manually created input will be written to the file lint.inp'
       !
       write (*, '(A)') ' '
       write (*, '(A)') 'Interactive input'
       write (*, '(A)') ' '
       write (9,*)
       !
       write( *, '(A,$)') 'Filename TEKAL datafile is                   : '
       read ( *, '(A)') fvect
       write( 9, '(A)') trim(fvect)
       write(10, '(A,a)')        'Filename TEKAL datafile is      : ', trim(fvect)

       write( *, '(A,$)') 'Column nrs. x, y, u, v are                   : '
       read ( *, *) icx, icy, icu, icv
       write( 9, *) icx, icy, icu, icv
       write(10, '(A,4(1x,i8))') 'Column nrs. x, y, u, v are      : ', icx, icy, icu, icv

       write( *, '(A,$)') 'Filename detailed output is                  : '
       read ( *, '(A)') error1
       write( 9, '(A)') trim(error1)
       write(10, '(A,a)')        'Filename detailed output is     : ', trim(error1)

       write( *, '(A,$)') 'Filename integrated output is                : '
       read ( *, '(A)') error2
       write( 9, '(A)') trim(error2)
       write(10, '(A,a)')        'Filename integrated output is   : ', trim(error2)

       write( *, '(A,$)') 'Number of subdivisions per polygon element is: '
       read ( *, *) nsteps
       write( 9, *) nsteps
       write(10, '(A,1x, i8)')   'Subdivisions per polygon element: ', nsteps

       write( *, '(A,$)') 'Detailed screen output (0/1) is              : '
       read ( *, *) iprint
       write( 9, *) iprint
       write(10, '(A,1x,i8)')    'Detailed screen output (0/1)    : ', iprint

       write( *, '(A,$)') 'Filename polygon file is                     : '
       read ( *, '(A)') fpol
       write( 9, '(A)') trim(fpol)
       write(10, '(A,a)')        'Filename polygon file is        : ', trim(fpol)
    !
    !     Read user input from file
    !
    else
       inquire (file = trim(inpnam), exist = ex)
       if (.not.ex) then
          write( *, '(a,a,a)') 'ERROR: File ', trim(inpnam), ' not found'
          write(10, '(a,a,a)') 'ERROR: File ', trim(inpnam), ' not found'
          goto 9999
       endif
       open (9, file = trim(inpnam))
       !
       ! Reading input file
       !
       read ( 9, *) ! skip record with input filename
       !
       read ( 9, '(a)') fvect
       write(10, '(a,a)')        'Filename TEKAL datafile is      : ', trim(fvect)
       !
       read ( 9, *) icx, icy, icu, icv
       write(10, '(a,4(1x,i8))') 'Column nrs. x, y, u, v are      : ', icx, icy, icu, icv
       !
       read ( 9, '(a)') error1
       write(10, '(a,a)')        'Filename detailed output is     : ', trim(error1)
       !
       read ( 9, '(a)') error2
       write(10, '(a,a)')        'Filename integrated output is   : ', trim(error2)
       !
       read ( 9, *) nsteps
       write(10, '(a,1x, i8)')   'Subdivisions per polygon element: ', nsteps
       !
       read ( 9, *) iprint
       write(10, '(a,1x,i8)')    'Detailed screen output (0/1)    : ', iprint
       !
       read ( 9, '(a)') fpol
       write(10, '(a,a)')        'Filename polygon file is        : ', trim(fpol)
    endif
    !
    open (12, file = trim(error1))
    k = len_trim(cident)
    !
    write (12, '(a)') '*'
    write (12, '(a,a )') '* ', cident(5:k)
    !
    open (13, file = trim(error2))
    !
    !     Read   vector file
    inquire (file = trim(fvect), exist = ex)
    if (ex) then
       open (11, file = trim(fvect))
    else
       write ( *, '(a)') 'ERROR: File ', trim(fvect), ' does not exist'
       write (10, '(a)') 'ERROR: File ', trim(fvect), ' does not exist'
       goto 9999
    endif
    !
    ! -->
   30 continue
    read (11, '(A4)') blname
    if (blname(1:1)=='*') goto 30
    ! <--
    read (11, *) mn, ncol, m
    if (m>0) then
       n = mn/m
    else
       write (10, '(a)') ' ERROR: m=0'
       write ( *, '(a)') ' ERROR: m=0'
       goto 9998
    endif
    if (mn > mnmax) then
      write ( *, '(a, a)') 'ERROR: Too many lines in block: ', blname
      write ( *, '(a,i5)') 'Decrease number of lines to   : ', mnmax
      write ( *, '(a, a)') 'ERROR: Too many lines in block: ', blname
      write ( *, '(a,i5)') 'Decrease number of lines to   : ', mnmax
      goto 9999
    endif
    !
    itel = 0
    xymiss = 999.999_hp
    do i = 1, mn
       itel = itel + 1
       read (11, *) (val(ic), ic = 1, icv)
       x(itel) = val(icx)
       y(itel) = val(icy)
       u(itel) = val(icu)
       v(itel) = val(icv)
       if (abs(x(itel) - xymiss)<1.D-6 .and. abs(y(itel) - xymiss)<1.D-6) then
          code(itel) = -1
       else
          code(itel) = 1
       endif
    enddo
    !
    ! Read polygon file
    !
    inquire (file = trim(fpol), exist = ex)
    if (ex) then
       call readldb(xpoly0,ypoly0,npoly,np0,fpol)
    else
       write ( *, '(a)') 'ERROR: File ', trim(fpol), ' does not exist'
       write (10, '(a)') 'ERROR: File ', trim(fpol), ' does not exist'
       goto 9999
    endif
    !
    write(13,'(a)') '*'
    write(13,'(a,a )') '* ', cident(5:k)
    write(13,'(a)') '*'
    write(13,'(a)')'* column 1 : transect number'
    write(13,'(a)')'* column 2 : integrated net transport'
    write(13,'(a)')'* column 3 : integrated positive transport'
    write(13,'(a)')'* column 4 : integrated negative transport'
    write(13,'(a)')'*'
    write(13, '(a)')'BL01'
    write(13, '(2(1x,I5))') npoly, 4
    !
    ! Loop over polygons
    do ipoly = 1, npoly
       np = np0(ipoly)
       do ip = 1, np
          xpoly(ip) = xpoly0(ip,ipoly)
          ypoly(ip) = ypoly0(ip,ipoly)
       enddo
       !
       ! Subdivide polygon
       xp(1) = xpoly(1)
       yp(1) = ypoly(1)
       ipt = 1
       do ip = 1, np - 1
          do ipp = 1, nsteps
             ipt = ipt + 1
             xp(ipt) = xpoly(ip) + real(ipp,hp)/real(nsteps,hp) &
                     & *(xpoly(ip + 1) - xpoly(ip))
             yp(ipt) = ypoly(ip) + real(ipp,hp)/real(nsteps,hp) &
                     & *(ypoly(ip + 1) - ypoly(ip))
          enddo
       enddo
       npt = ipt
       if (npt > npmax) then
          write ( *, '(a,i5)') 'ERROR: Too many parts in polyline  : ', ipoly
          write ( *, '(a,i5)') 'Decrease subdivision per element to: ', npmax/np
          write (10, '(a,i5)') 'ERROR: Too many parts in polyline  : ', ipoly
          write (10, '(a,i5)') 'Decrease subdivision per element to: ', npmax/np
          goto 9999
       endif
       !        Interpolate vector values to expanded polygon points
       call mkmap(code      ,x         ,y         ,m         ,n         , &
                & xp        ,yp        ,npt       ,xs        ,ys        , &
                & nrx       ,nry       ,iflag     ,nrin      ,w         , &
                & iref      ,iprint    ,lunlog    )
       call grmap(u         ,mn        ,up        ,npt       ,iref      , &
                & w         ,4         ,iprint    )
       call grmap(v         ,mn        ,vp        ,npt       ,iref      , &
                & w         ,4         ,iprint    )
       !
       !        Compute transport across each section
       !
       cumdist = 0.0
       cumtra = 0.0
       cumtrapos = 0.0
       cumtraneg = 0.0
       !
       dx = xp(2) - xp(1)
       dy = yp(2) - yp(1)
       ang  = atan2(dy, dx)
       crosvec = -up(1)*sin(ang) + vp(1)*cos(ang)
       !
       write(12,'(a)')'*'
       write(12,'(a)')'* column 1 : distance'
       write(12,'(a)')'* column 2 : x'
       write(12,'(a)')'* column 3 : y'
       write(12,'(a)')'* column 4 : vector'
       write(12,'(a)')'* column 5 : cumulative transport'
       write(12,'(a)')'*'
       write (12, '(''B'',i3.3)') ipoly
       write (12, '(2(1x,I5))') npt, 5
       write (12, '(6(1pe15.7))') 0.0 ,xp(1) ,yp(1), crosvec ,0.0
       !
       do ip = 2, npt
          dx = xp(ip) - xp(ip - 1)
          dy = yp(ip) - yp(ip - 1)
          xm = 0.5*(xp(ip) + xp(ip - 1))
          ym = 0.5*(yp(ip) + yp(ip - 1))
          um = 0.5*(up(ip) + up(ip - 1))
          vm = 0.5*(vp(ip) + vp(ip - 1))
          !
          dist = sqrt(dx*dx + dy*dy)
          ang  = atan2(dy, dx)
          crosvec = -um*sin(ang) + vm*cos(ang)
          crostra = crosvec*dist
          cumdist = cumdist + dist
          cumtra  = cumtra + crostra
          if (crostra>0.0) then
             cumtrapos = cumtrapos + crostra
          else
             cumtraneg = cumtraneg + crostra
          endif
          !
          crosvec = -up(ip)*sin(ang) + vp(ip)*cos(ang)
          !
          write (12, '(6(1pe15.7))') cumdist, xp(ip), yp(ip), crosvec, cumtra
          !
       enddo
       write (13,'(i5,3(1pe15.7))') ipoly,cumtra,cumtrapos,cumtraneg
    enddo
 9998 continue
 9999 continue
    close (9)
    close (10)
    close (11)
    close (12)
    close (13)
    !
    ! Deallocate
    !
    deallocate(iref)
    deallocate(code)
    deallocate(iflag)
    deallocate(np0)
    deallocate(nrin)
    deallocate(nrx)
    deallocate(nry)
    deallocate(w)
    deallocate(u)
    deallocate(v)
    deallocate(x)
    deallocate(y)
    deallocate(xpoly)
    deallocate(xpoly0)
    deallocate(xs)
    deallocate(ypoly)
    deallocate(ypoly0)
    deallocate(ys)
    deallocate(up)
    deallocate(vp)
    deallocate(xp)
    deallocate(yp)
end subroutine main_lint
