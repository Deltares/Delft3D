subroutine main_kubint ()
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
    use precision_kubint
    use kubint_version_module
    implicit none
!
! Local parameters
!
    integer, parameter :: mnmax = 400000, npmax = 1000, nsmax = 250000,         &
                        & npolmx = 500
    ! COMMON variables
    !
    integer :: ncol
    common /coln  / ncol
!
! Local variables
!
    integer                           :: i
    integer                           :: ic
    integer                           :: icx
    integer                           :: icy
    integer                           :: icz
    integer                           :: inout
    integer                           :: ip
    integer                           :: ipoly
    integer                           :: iprint
    integer                           :: it
    integer                           :: itm
    integer                           :: itel
    integer                           :: ix
    integer                           :: iy
    integer                           :: j
    integer                           :: k
    integer                           :: lunlog
    integer                           :: m
    integer                           :: mn
    integer                           :: n
    integer                           :: nfine
    integer                           :: npolm
    integer                           :: npoly
    integer                           :: npt
    integer, dimension(:,:), pointer  :: iref
    integer, dimension(:)  , pointer  :: code
    integer, dimension(:)  , pointer  :: np
    integer, dimension(:)  , pointer  :: iflag
    integer, dimension(:)  , pointer  :: nrin
    integer, dimension(:)  , pointer  :: nrx
    integer, dimension(:)  , pointer  :: nry
    logical                           :: ex
    logical                           :: intact
    real(hp)                           :: avgp
    real(hp)                          :: dx
    real(hp)                          :: dy
    real(hp)                          :: rnr
    real(hp)                          :: sump
    real(hp)                          :: sumz
    real(hp)                          :: xmax
    real(hp)                          :: xmin
    real(hp)                          :: ymax
    real(hp)                          :: ymin
    real(hp)                          :: xymiss
    real(hp)                          :: amiss
    real(hp), dimension(15)           :: val
    real(hp), dimension(:,:), pointer :: w
    real(hp), dimension(:)  , pointer :: x
    real(hp), dimension(:)  , pointer :: y
    real(hp), dimension(:)  , pointer :: z
    real(hp), dimension(:,:), pointer :: xpoly
    real(hp), dimension(:,:), pointer :: ypoly
    real(hp), dimension(:)  , pointer :: area
    real(hp), dimension(:,:), pointer :: avgz
    real(hp), dimension(:,:), pointer :: volp
    real(hp), dimension(:,:), pointer :: volume
    real(hp), dimension(:)  , pointer :: xp
    real(hp), dimension(:)  , pointer :: xs
    real(hp), dimension(:)  , pointer :: yp
    real(hp), dimension(:)  , pointer :: ys
    real(hp), dimension(:)  , pointer :: zp
    character(120)                    :: cident
    character(256)                    :: error1
    character(256)                    :: fpol
    character(256)                    :: fvect
    character(256)                    :: inpnam
    character(4)                      :: blname
!
!
!! executable statements -------------------------------------------------------
!
    ! Allocation
    !
    allocate(iref(4,nsmax))
    allocate(code(mnmax))
    allocate(np(npolmx))
    allocate(iflag(nsmax))
    allocate(nrin(nsmax))
    allocate(nrx(nsmax))
    allocate(nry(nsmax))
    allocate(w(4,nsmax))
    allocate(x(mnmax))
    allocate(y(mnmax))
    allocate(z(mnmax))
    allocate(xpoly(npmax,npolmx))
    allocate(ypoly(npmax,npolmx))
    allocate(area(npolmx))
    allocate(xp(nsmax))
    allocate(xs(nsmax))
    allocate(yp(nsmax))
    allocate(ys(nsmax))
    allocate(zp(nsmax))
    !
    itm   = 100  ! maximum number of read blocks
    npolm = 500  ! equal to npolmx
    allocate (avgz  (itm,npolm))
    allocate (volp  (itm,npolm))
    allocate (volume(itm,npolm))
    !
    if (fp == sp) then
       stop 'Error: Need to be run/compile in double precision_kubint mode'
    endif
    !
    ! KUBINT computes the integral of a 2D function over areas
    !        enclosed by specified polygons
    !
    !
    call pldep
    !
    !     Identification string
    !
    cident = ' '
    call getfullversionstring_KUBINT(cident)
    !
    !-output to kubint.log
    lunlog = 10
    open (10, file = 'kubint.log')
    k = len_trim(cident)
    write(10, *)
    write(10, '(a)') cident(5:k)
    write(10, *)
    write( *, *)
    write( *, '(a)') cident(5:k)
    write( *, *)
    write( *, '(A,$)') 'Input filename: '
    read ( *, '(A)') inpnam
    intact = .false.
    if (inpnam==' ') then
       intact = .true.
    else
       inquire (file = trim(inpnam), exist = ex)
       if (.not.ex) intact = .true.
    endif
    write( *, *)
    write( *, '(a)') 'Diagnostics will be written to file kubint.log'
    !-----interactive
    if (intact) then
       open (9, file = 'kubint.inp')
       write( *, '(a)') 'Your manually created input will be written to the file kubint.inp'

       write (*, '(a)') ' '
       write (*, '(a)') 'Interactive input'
       write (*, '(a)') ' '
       write (9,'(a)') ' ' ! record for input filename, if interactive then blank record

       write( *, '(a,$)') 'Filename TEKAL datafile     : '
       read ( *, '(a)') fvect
       write( 9, '(a)') trim(fvect)
       write(10, '(a,a)') 'Filename TEKAL datafile       : ', trim(fvect)

       write( *, '(a,$)') 'Columns nr x, y, z are      : '
       read ( *, *) icx, icy, icz
       write( 9, '(1x,i5)') icx
       write( 9, '(1x,i5)') icy
       write( 9, '(1x,i5)') icz
       write(10, '(a,3(1x,i5))') 'Columns nr x, y, z            : ', icx, icy, icz

       write( *, '(a,$)') 'Filename output             : '
       read ( *, '(a)') error1
       write( 9, '(a)') trim(error1)
       write(10, '(a,a)') 'Filename output               : ', trim(error1)

       write( *, '(a)')   'Resolution (no. pixels in x-'
       write( *, '(a,$)') ' and y-direction)           : '
       read ( *, *) nfine
       write( 9, '(1x,i5)') nfine
       write(10, '(a)') 'Resolution (no. pixels in x-'
       write(10, '(a,i5)') 'and y-direction)              : ', nfine

       write( *, '(a,$)') 'Detailed screen output (0/1): '
       read ( *, *) iprint
       write( 9, '(1x,i5)') iprint
       write(10, '(a,i5)') 'Detailed screen output (0/1)  : ', iprint

       write( *, '(a,$)') 'Filename polygons           : '
       read ( *, '(a)') fpol
       write( 9, '(a)') trim(fpol)
       write(10, '(a,a)')  'Filename polygons             : ', trim(fpol)

       close(9)
    else
       !-------from file
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
       read ( 9, *) ! skip record with input file name
       !
       write(10, '(a,a)') 'Input file name               : ', trim(inpnam)
       write(10, *)
       !
       read ( 9, '(A)') fvect
       write(10, '(a,a)') 'Filename TEKAL datafile       : ', trim(fvect)
       !
       read ( 9, *) icx, icy, icz
       write(10, '(a,3(1x,i5))') 'Columns nr x, y, z            : ', icx, icy, icz
       !
       read ( 9, '(A)') error1
       write(10, '(a,a)') 'Filename output               : ', trim(error1)
       !
       read ( 9, *) nfine
       write(10, '(a)') 'Resolution (no. pixels in x-'
       write(10, '(a,1x,i5)') 'and y-direction)              : ', nfine
       !
       read ( 9, *) iprint
       write(10, '(a,1x,i5)') 'Detailed screen output (0/1)  : ', iprint
       !
       read ( 9, '(a)') fpol
       write(10, '(a,a)')  'Filename polygons             : ', trim(fpol)
       !
       close (9)
    endif
    !
    call readldb(xpoly,ypoly,npoly,np,fpol)
    !
    write( *, *)
    write(10, *)
    if (fvect==' ') then
       write( *, '(a)') 'ERROR: No file name for TEKAL data file'
       write(10, '(a)') 'ERROR: No file name for TEKAL data file'
       goto 9999
    endif
    !
    inquire (file = trim(fvect), exist = ex)
    if (.not.ex) then
       write( *, '(a,a,a)') 'ERROR: File ', trim(fvect), ' not found'
       write(10, '(a,a,a)') 'ERROR: File ', trim(fvect), ' not found'
       goto 9999
    endif
    open (11, file = trim(fvect))
    !
    open (12, file = trim(error1))
    k = len_trim(cident)
    write(12, '(a)') '*'
    write(12, '(a,a)') '* ',cident(5:k)
    it = 1
    !
    ! ==>>
    !
 2000 continue
    if (it > itm) then
       write( *,'(a)') ' ERROR: Number of blocks to read maximized to 100'
       write( *,'(a)') '        Just first 100 blocks processed'
       write(10,'(a)') ' ERROR: Number of blocks to read maximized to 100'
       write(10,'(a)') '        Just first 100 blocks processed'
       goto 2001
    endif
    write( *, '(a)') 'Reading data file.......................'
    !     Read   data file
    ! -->
   30 continue
    read (11, '(a4)', end = 2001, err = 2001) blname
    if (blname(1:1)=='*') goto 30
    ! <--
    read (11, *) mn, ncol, m
    if (m>0) then
       n = mn/m
    else
       write( *, '(a,a)') 'ERROR: m=0 in blok ', trim(blname)
       write(10, '(a,a)') 'ERROR: m=0 in blok ', trim(blname)
       goto 9999
    endif
    itel = 0
    xymiss = 0.0_hp
    amiss  = 999.999_hp
    do i = 1, mn
       itel = itel + 1
       read (11, *) (val(ic), ic = 1, icz)
       x(itel) = val(icx)
       y(itel) = val(icy)
       z(itel) = val(icz)
       if ( ( x(itel) == xymiss .and. y(itel) == xymiss       ) .or.  &
            z(itel) == amiss ) then
          code(itel) = -1
       else
          code(itel) = 1
       endif
    enddo
    !     Loop over polygons
    do ipoly = 1, npoly
       !        Generate fine grid around polygon
       write( *, '(a)') 'Generating grid.........................'
       call minmax(xpoly(1, ipoly)      ,np(ipoly) ,xmin      ,xmax      )
       call minmax(ypoly(1, ipoly)      ,np(ipoly) ,ymin      ,ymax      )
       dx   = (xmax - xmin)/dble(nfine)
       dy   = (ymax - ymin)/dble(nfine)
       xmin = xmin + .5*dx
       ymin = ymin + .5*dy
       do iy = 1, nfine
          do ix = 1, nfine
             xp(ix + (iy - 1)*nfine) = xmin + dble(ix - 1)*dx
             yp(ix + (iy - 1)*nfine) = ymin + dble(iy - 1)*dy
             zp(ix + (iy - 1)*nfine) = 0.
          enddo
       enddo
       npt = nfine*nfine
       !        Interpolate function values to fine grid
       write( *, '(a)') 'Making grid-map.........................'
       call mkmap(code      ,x         ,y         ,m         ,n         , &
                & xp        ,yp        ,npt       ,xs        ,ys        , &
                & nrx       ,nry       ,iflag     ,nrin      ,w         , &
                & iref      ,iprint    ,lunlog)
       write( *, '(a)') 'Interpolating...........................'
       call grmap(z         ,mn        ,zp        ,npt       ,iref      , &
                & w         ,4         ,iprint    )
       if (iprint==1) then
          write( *, '(3(1pe15.7))') (xp(i), yp(i), zp(i), i = 1, npt)
       endif
       write( *, '(a)') 'Checking grid cells and adding values...'
       sumz = 0.0
       sump = 0.0
       rnr  = 0.0
       do i = 1, npt
          call ipon(xpoly(1, ipoly)   ,ypoly(1, ipoly)   ,np(ipoly) , &
             &      xp(i)             ,yp(i)  ,  inout   , lunlog)
          if (iprint==1) write( *, '(F6.1,I5,F10.3)') rnr, inout, zp(i)
          if (inout==1 .and. iref(1,i)>0) then
             if (zp(i)>0.0) then
                sump = sump + zp(i)
             endif
             sumz = sumz + zp(i)
             rnr  = rnr + 1.0
          elseif (inout==0 .and. iref(1,i)>0) then
             if (zp(i)>0.0) then
                sump = sump + 0.5*zp(i)
             endif
             sumz = sumz + 0.5*zp(i)
             rnr  = rnr  + 0.5
          else
          endif
       enddo
       avgz  (it,ipoly) = sumz/rnr
       avgp             = sump/rnr
       area  (ipoly) = rnr*dx*dy
       volume(it,ipoly) = avgz(it,ipoly)*area(ipoly)
       volp  (it,ipoly) = avgp*area(ipoly)
    enddo
    it = it + 1
    goto 2000
    ! <<==
2001 continue
    it = it - 1

    write(12, '(a)') '*'
    write(12, '(a)') '* column 1 : number of polygon'
    write(12, '(a)') '* column 2 : area'
    write(12, '(a)') '* column 3 : volume'
    write(12, '(a)') '* column 4 : average'
    write(12, '(a)') '* column 5 : positive volume'
    write(12, '(a)') '* column 6 : negative volume'
    write(12, '(a)') '*'
    do i = 1, it
       write(12, '(a,i0.4)') 'BL',i
       write(12, '(2(1x,i5))') it*npoly, 6
       do ipoly = 1, npoly
          write(12,'(i5,5(1pe15.7))') ipoly,area(ipoly),volume(i,ipoly),avgz(i,ipoly),volp(i,ipoly), (volume(i,ipoly) - volp(i,ipoly))
       enddo
    enddo

 9999 continue
    close (10)
    close (11)
    close (12)

    if (associated(avgz  )) deallocate (avgz  )
    if (associated(volp  )) deallocate (volp  )
    if (associated(volume)) deallocate (volume)

    ! Deallocation
    !
    deallocate(iref)
    deallocate(code)
    deallocate(np)
    deallocate(iflag)
    deallocate(nrin)
    deallocate(nrx)
    deallocate(nry)
    deallocate(w)
    deallocate(x)
    deallocate(y)
    deallocate(z)
    deallocate(xpoly)
    deallocate(ypoly)
    deallocate(area)
    deallocate(xp)
    deallocate(xs)
    deallocate(yp)
    deallocate(ys)
    deallocate(zp)
end subroutine main_kubint
