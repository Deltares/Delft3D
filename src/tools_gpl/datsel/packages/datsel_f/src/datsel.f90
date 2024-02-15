subroutine datsel ()
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
    use precision_datsel
    use deltares_common_version_module
    use datsel_version_module
    implicit none
! Local parameters
!
    ! COMMON variables
    !
    real(hp)                           :: facrad
    common /fr    / facrad
!
! Local variables
!
    integer                                  :: crenef
    integer                                  :: clsnef
    integer                                  :: ierror
    integer                                  :: first
    integer                                  :: grpndm
    integer                                  :: i
    integer                                  :: icel
    integer                                  :: iconst       ! Modification lmax
    integer                                  :: idim
    integer                                  :: iel
    integer                                  :: ief
    integer                                  :: ilayer
    integer                                  :: incr
    integer                                  :: iopt
    integer                                  :: k
    integer                                  :: k0
    integer                                  :: k1
    integer                                  :: kmax
    integer                                  :: last
    integer                                  :: lmax         ! Modification lmax
    integer                                  :: n1
    integer                                  :: n2
    integer                                  :: nbytsg
    integer                                  :: ncel
    integer                                  :: ncelmx
    integer                                  :: ncol
    integer                                  :: nfiles
    integer                                  :: npnt
    integer                                  :: deffds
    integer       , dimension(5)             :: elmdms
    integer       , dimension(3,5)           :: uindex
    integer       , dimension(5)             :: usrord
    integer       , dimension(5)             :: grpdms
    integer       , dimension(5)             :: grpord
    integer       , dimension(:)  , pointer  :: nelms
    integer       , dimension(:)  , pointer  :: kcs
    integer       , dimension(:)  , pointer  :: celidt
    integer                       , external :: clsdat
    integer                       , external :: clsdef
    integer                       , external :: getelt
    integer                       , external :: inqelm
    integer                       , external :: inqgrp
    integer                       , external :: inqmxi
    integer                       , external :: opndat
    integer                       , external :: opndef
    logical                                  :: ex
    logical                                  :: lerror
    logical                                  :: intact
    real(hp)                                 :: arg
    real(hp)      , dimension(:)  , pointer  :: alfas
    real(hp)      , dimension(:)  , pointer  :: u
    real(hp)      , dimension(:)  , pointer  :: um
    real(hp)      , dimension(:)  , pointer  :: umean
    real(hp)      , dimension(:)  , pointer  :: uvar
    real(hp)      , dimension(:)  , pointer  :: uvm
    real(hp)      , dimension(:)  , pointer  :: v
    real(hp)      , dimension(:)  , pointer  :: vmean
    real(hp)      , dimension(:)  , pointer  :: vvar
    real(hp)      , dimension(:)  , pointer  :: xcor
    real(hp)      , dimension(:)  , pointer  :: ycor
    real          , dimension(:)  , pointer  :: wrk_2d
    real          , dimension(:)  , pointer  :: wrk_3d
    real          , dimension(:)  , pointer  :: wrk_4d
    character(16) , dimension(:)  , pointer  :: alfnam
    character(200)                           :: cas
    character(16)                            :: celnam
    character(256)                           :: cident
    character(64)                            :: elmdes
    character(16)                            :: elmqty
    character(8)                             :: elmtyp
    character(16)                            :: elmunt
    character(256)                           :: fildat
    character(256)                           :: fildef
    character(256)                           :: filnam
    character(30) , dimension(:,:), pointer  :: funnam
    character(16) , dimension(:)  , pointer  :: grdnam
    character(256)                           :: inpnam
    character(16) , dimension(:,:), pointer  :: kgrp
    character(16) , dimension(:,:), pointer  :: knam
    character(40)                            :: label
    character(256)                           :: outnam
    character(256)                           :: padnam
    character(256)                           :: pathd
    character(10) , dimension(:)  , pointer  :: prefix
    character(1)  , dimension(:,:), pointer  :: typ
    character(16) , dimension(:,:), pointer  :: ugrp
    character(16) , dimension(:,:), pointer  :: unam
    character(16) , dimension(:,:), pointer  :: vnam
    character(16) , dimension(:)  , pointer  :: xnam
    character(16) , dimension(:)  , pointer  :: ynam
!
!! executable statements -------------------------------------------------------
!
    call pldep
    !
    !     Identification string
    !
    cident = ' '
    call getfullversionstring_deltares_common(cident)
    call getfullversionstring_DATSEL(cident)

    k = len_trim(cident)
    write( *, *)
    write( *, '(a)') cident(5:k)
    write( *, *)
    !
    !     Read definition file FILES.DEF
    !     ------------------------------
    facrad = atan(1.)/45.
    lerror = .false.
    call getmp(lerror, pathd)
    if (lerror) then
       write (*, '(a)') "ERROR: Directory ""default"" not found. Needed for <files.def>."
       call ntstop
    endif
    !
    ! Read <files.def> file
    !
    filnam = trim(pathd) // 'files.def'
    inquire (file = trim(filnam), exist = ex)
    if (.not.ex) then
       write(*, '(A)') ' ERROR: Definition file <files.def> not found', trim(filnam)
       call ntstop
    endif
    !
    open (11, file = trim(filnam))
    read (11, *) nfiles
    !
    allocate(prefix(nfiles))
    allocate(xnam  (nfiles))
    allocate(ynam  (nfiles))
    allocate(alfnam(nfiles))
    allocate(grdnam(nfiles))
    allocate(nelms (nfiles))
    !
    allocate(funnam(20, nfiles))
    allocate(unam  (20, nfiles))
    allocate(vnam  (20, nfiles))
    allocate(ugrp  (20, nfiles))
    allocate(knam  (20, nfiles))
    allocate(kgrp  (20, nfiles))
    allocate(typ   (20, nfiles))

    funnam = ' '
    unam   = ' '
    vnam   = ' '
    ugrp   = ' '
    knam   = ' '
    kgrp   = ' '
    typ    = ' '
    !
    do ief = 1, nfiles
       read (11, *) prefix(ief)
       read (11, *) xnam(ief), ynam(ief), alfnam(ief), grdnam(ief)
       read (11, *) nelms(ief)

       if (nelms(ief) > 20) then
          write(*,*) 'Contact Deltares'
       endif
       do iel = 1, nelms(ief)
          read (11, *) funnam(iel, ief), unam(iel, ief), vnam(iel, ief),           &
                     & ugrp(iel, ief), knam(iel, ief), kgrp(iel, ief),             &
                     & typ (iel, ief)
       enddo
    enddo
    close (11)
    !
    ! Ask input filename
    ! ------------------
    filnam = ' '
    inpnam = ' '
    write (*, '(A,$)') 'Input filename: '
    read  (*, '(A)') inpnam
    intact = .false.
    if (inpnam==' ') then
       intact = .true.
    else
       inquire (file = trim(inpnam), exist = ex)
       if (.not.ex) intact = .true.
    endif

    write( *, *)
    write( *, '(a)') 'Diagnostics will be written to file datsel.log'
    if (intact) then
       open  (10, file = 'datsel.inp')
       write ( *, '(a)') 'Your manually created input will be written to the file datsel.inp'
       !
       write (*, '(A)') ' '
       write (*, '(A)') 'Interactive input'
       write (*, '(A)') ' '
       write (10,'(a)') ' ' ! record for input filename, if interactive then blank record
       !
       write (*, '(A)') 'Available file types:'
       do ief = 1, nfiles
          write (*, '(A,I2,A,A)') ' ', ief, ' ', prefix(ief)
       enddo
  125  continue
       write (*, '(A,I2,A,$)') 'File type number (1 -', nfiles, ') = '
       read  (*, *) ief
       write(10,'(1x,i5)') ief
       if (ief<1 .or. ief>7) goto 125
       !
       write (*, '(A)') ' '
       write (*, '(A,A)') 'Available Functions for filetype ', prefix(ief)
       do iel = 1, nelms(ief)
          write (*, '(A,I2,A,A)') ' ', iel, ' ', funnam(iel, ief)
       enddo
  135  continue
       write (*, '(A,I3,A,$)') 'Function name number (1 -', nelms(ief), ') = '
       read  (*, *) iel
       write(10,'(1x,i5)') iel
       !
       write (*, '(A)') ' '
       if (iel<1 .or. iel>nelms(ief)) goto 135
       write (*, '(A,/,A,/,A)')   'If number of time steps is 0 (zero), ', &
                       &          'then the input will be continued with asking for', &
                       &          'first, increment and last time step'
       write (*, '(A,$)') 'Number of time steps      = '
       read  (*, *) ncel
       write(10,'(1x,i5)') ncel
       !
       if (ncel>0) then
          allocate(celidt(ncel))
          write (*, '(A)') 'Time step numbers are     = '
          do icel = 1, ncel
             read  (*, *) celidt(icel)
             write(10,'(7x,i5)') celidt(icel)
          enddo
       else
          write (*, '(A,A,$)') 'First time step, increment,', ' last time step: '
          read  (*, *) first, incr, last
          write(10,'(1x,i5)') first
          write(10,'(1x,i5)') incr
          write(10,'(1x,i5)') last
          !
          if (incr==0) then
            ncel = 1
          else
            ncel = (last - first)/incr + 1
          endif
          allocate(celidt(ncel))
          celidt(1) = first
          do icel = 2, ncel
            celidt(icel) = celidt(icel - 1) + incr
          enddo
       endif
       !
       write (*, '(A,/,A,$)') 'Time-varying output (type 1) or',                &
                            & 'Time-average output (type 2)     = '
       read  (*, *) iopt
       write (10,'(1x,i5)') iopt
       !
       write (*, '(A,$)') 'Datafile path (eg d:\test\)      = '
       read  (*, '(A)') padnam
       write (10,'(a)') trim(padnam)
       !
       write (*, '(A,$)') 'Datafile case                    = '
       read  (*, '(A)') cas
       write (10,'(a)') trim(cas)
       !
       write (*, '(A,$)') 'Datafile label                   = '
       read  (*, '(A)') label
       write (10,'(a)') trim(label)
       !
       write (*, '(A,$)') 'Output filename (incl. path)     = '
       read  (*, '(A)') outnam
       write (10,'(a)') trim(outnam)
       !
       write (10,'(a)')      ! To give the <enter> in routine ntstop
    else
       open (11, file = trim(inpnam))
       read (11, *) ief
       read (11, *) iel
       read (11, *) ncel
       if (ncel>0) then
          allocate(celidt(ncel))
          do icel = 1, ncel
             read  (11, *) celidt(icel)
          enddo
       else
          read  (11, *) first, incr, last
          !
          if (incr==0) then
            ncel = 1
          else
            ncel = (last - first)/incr + 1
          endif
          allocate(celidt(ncel))
          celidt(1) = first
          do icel = 2, ncel
             celidt(icel) = celidt(icel - 1) + incr
          enddo
       endif
       read (11, *) iopt
       read (11, '(A)') padnam
       read (11, '(A)') cas
       read (11, '(A)') label
       read (11, '(A)') outnam
    endif
    !
    !     Construct filename
    !     ------------------
    filnam = trim(padnam) // trim(prefix(ief)) // trim(cas) // trim(label)
    !
    !     Write datsel.log
    !     ----------------
    open (12, file = 'datsel.log')
    k = len_trim(cident)
    write(12, '(A)')
    write(12, '(1X,A)') cident(5:k)
    write(12, '(A)')
    write(12, *) ' File type           : ', prefix(ief)
    write(12, *) ' Function            : ', funnam(iel, ief)
    write(12, *) ' Number of time steps: ', ncel
    write(12, *) ' Time step numbers   : '
    do icel = 1, ncel
       write(12, *) celidt(icel)
    enddo
    if (iopt==1) then
       write(12, *) ' Time-varying output is generated'
    elseif (iopt==2) then
       write(12, *) ' Time-average output is generated'
    else
       write(*, '(A)') ' Illegal time-varying output option'
       close(10)
       close(11)
       close(12)
       call ntstop
    endif
    write(12, *) ' Datafile            : ', trim(filnam)
    write(12, *) ' Output filename     : ', trim(outnam)
    !
    open (13, file = trim(outnam))
    k = len_trim(cident)
    write(13, '(''*'')')
    write(13, '(''* '',A)') cident(5:k)
    write(13, '(''*'')')
    !
    !     Open definition- and datafile
    !     -----------------------------
    fildef = trim(filnam) // '.def'
    fildat = trim(filnam) // '.dat'
    ierror = crenef(deffds, fildat, fildef, ' ', 'r')
    call nefout(ierror     ,' ')
    !
    !     Find out grid dimensions
    !     ------------------------
    idim   = 5
    elmdms = 0
    ierror = inqelm(deffds, unam(iel, ief), elmtyp, nbytsg, elmqty, elmunt, &
          &        elmdes, idim, elmdms)
    call nefout(ierror     ,'inqelm'  )
    n1    = elmdms(1)
    n2    = elmdms(2)
    kmax  = max(elmdms(3), 1)
    lmax  = max(elmdms(4), 1)                                   ! Modification lmax
    !
    if (n1*n2 == 0) then
       write(12,*)
       write(12,'(a,a,a,a,a)') 'Chosen field ''', trim(funnam(iel, ief)),''' contains no data on output file ''', trim(fildat),''''
       write(*,*)
       write(*,'(a,a,a,a,a)') 'Chosen field ''', trim(funnam(iel, ief)),''' contains no data on output file ''', trim(fildat),''''
       call ntstop
    endif
    !
    allocate(kcs  (n1*n2))
    allocate(xcor (n1*n2))
    allocate(ycor (n1*n2))
    allocate(alfas(n1*n2))
    allocate(u     (n1*n2*kmax*lmax))
    allocate(um   (n1*n2*kmax))
    allocate(umean(n1*n2))
    allocate(uvar (n1*n2))
    allocate(v    (n1*n2*kmax))
    allocate(uvm  (n1*n2*kmax))
    allocate(vmean(n1*n2))
    allocate(vvar (n1*n2))
    allocate(wrk_2d(n1*n2))
    allocate(wrk_3d(n1*n2*kmax))
    allocate(wrk_4d(n1*n2*kmax*lmax))
    !
    if (kmax>1) then
       if (intact) then
          write(*, '(A,$)') 'Layer number = '
          read (*, *)  ilayer
          write(10,'(1x,i5)') ilayer
       else
          read (11, *) ilayer
       endif
       write(12, '(A,I3)') 'Layer number      : ' , ilayer
    else
       ilayer = 1
    endif
    if (lmax>1) then                                           ! Modification lmax
       if (intact) then                                        ! Modification lmax
          write(*, '(A,$)') 'Constituent number = '            ! Modification lmax
          read (*, *)  iconst                                  ! Modification lmax
          write(10,'(1x,i5)') iconst
       else                                                    ! Modification lmax
          read (11, *) iconst                                  ! Modification lmax
       endif                                                   ! Modification lmax
       write(12, '(A,I3)') 'Constituent number: ', iconst      ! Modification lmax
    else                                                       ! Modification lmax
       iconst = 1                                              ! Modification lmax
    endif                                                      ! Modification lmax
    npnt = n1*n2
    !
    k1 = (iconst-1)*kmax*npnt+(ilayer - 1)*npnt + 1            ! Modification lmax
    k0 = k1 - 1
    !
    !     Find out group dimensions
    !     -------------------------
    grpndm = 5
    ierror  = inqgrp(deffds, ugrp(iel, ief), celnam, grpndm, grpdms, grpord)
    call nefout(ierror     ,'inqgrp'  )
    ncelmx = grpdms(1)
    if (ncelmx==0) then
       ierror = inqmxi(deffds, ugrp(iel, ief), ncelmx)
       call nefout(ierror     ,'inqmxi'  )
    endif
    !
    !     Stop if last celidt > ncelmx
    !     ----------------------------
145 continue
    if (celidt(ncel)>ncelmx) then
       write(*, *) ' Warning, last time step number ( ',celidt(ncel),' ) greater than', ncelmx
       ncel = ncel -1
       write(*,*) ' Number of time steps ',ncel
       if (ncel>1) then
          goto 145
       else
            close(10)
            close(11)
            close(12)
            close(13)
            call ntstop
       endif
    endif
    !
    !     Read grid
    !     ---------
    uindex(1,1) = 1
    uindex(2,1) = 1
    uindex(3,1) = 1
    usrord(1)   = 1

    ierror = getelt(deffds, grdnam(ief), xnam(ief), uindex, usrord, n1*n2*4, wrk_2d)
    call nefout(ierror     ,xnam(ief))
    xcor = dble(wrk_2d)

    ierror = getelt(deffds, grdnam(ief), ynam(ief), uindex, usrord, n1*n2*4, wrk_2d)
    call nefout(ierror     ,ynam(ief))
    ycor = dble(wrk_2d)

    if (alfnam(ief)/='NONE') then
       ierror = getelt(deffds, grdnam(ief), alfnam(ief), uindex, usrord, n1*n2*4,  &
             & wrk_2d)
       call nefout(ierror     ,alfnam(ief))
       alfas = dble(wrk_2d)
    else
       do i = 1, npnt
          alfas(i) = 0.
       enddo
    endif
    if (knam(iel, ief)/='NONE') then
       ierror = getelt(deffds, kgrp(iel, ief), knam(iel, ief), uindex, usrord,  &
             & n1*n2*4, kcs)
       call nefout(ierror     ,knam(iel, ief)        )
    else
       do i = 1, npnt
          kcs(i) = 1
       enddo
    endif
    !
    !     Translate coordinates to centre points if neccessary
    !     ----------------------------------------------------
    if (typ(iel, ief)/='d' .and. typ(iel, ief)/='v') then
       call co2cen(xcor      ,ycor      ,kcs       ,n1        ,n2        )
    endif
    write(13, '(A1,1X,A8,1X,A30,1X,A4,1X,A)') &
        & '*', 'Function', funnam(iel, ief), 'from', trim(filnam)
    write(13,'(a)') '*'
    !
    !     Initialise time-averaged values
    !     -------------------------------
    do i = 1, npnt
       umean(i) = 0.
       vmean(i) = 0.
       uvar(i)  = 0.
       vvar(i)  = 0.
    enddo
    !
    !     Loop over time steps
    !     --------------------
    do icel = 1, ncel
       uindex(1,1) = celidt(icel)
       uindex(2,1) = celidt(icel)
       uindex(3,1) = 1
       !
       !        Scalar or vector
       !        ----------------
       if (typ(iel, ief)=='d' .or. typ(iel, ief)=='c' .or. typ(iel, ief)=='s')  &
         & then
          !
          !           Read scalar field
          !           -----------------
          ierror = getelt(deffds, ugrp(iel, ief), unam(iel, ief), uindex,  &
                & usrord, n1*n2*kmax*lmax*4, wrk_4d)
          call nefout(ierror     ,unam(iel, ief)        )
          u = dble(wrk_4d)

          if (typ(iel, ief)=='s') then
             call co1cen(u         ,kcs       ,n1        ,n2        )
          endif
          !
          !           Write TEKAL file
          !           ----------------
          ncol = 3
          if (iopt==1) then
             write(13, '(A1,I3.3)') 'M', icel
             write(13, *) npnt, ncol, n1, n2
             do i = 1, npnt
                if (kcs(i)<=0) then
                      write(13, '( a)') '  999.999  999.999  999.999'
                   else
                      write(13, '(3(1PE15.7))') xcor(i), ycor(i), u(i + k0)
                endif
             enddo

          else
             do i = 1, npnt
                umean(i) = umean(i) + u(i + k0)
                uvar(i)  = uvar(i) + u(i + k0)*u(i + k0)
             enddo
          endif
       else
          !
          !           Read vector field
          !           -----------------
          ierror = getelt(deffds, ugrp(iel, ief), unam(iel, ief), uindex,  &
                & usrord, n1*n2*kmax*4, wrk_3d)
          call nefout(ierror     ,unam(iel, ief)        )
          u = dble(wrk_3d)

          ierror = getelt(deffds, ugrp(iel, ief), vnam(iel, ief), uindex,  &
                & usrord, n1*n2*kmax*4, wrk_3d)
          call nefout(ierror     ,vnam(iel, ief)        )
          v = dble(wrk_3d)
          !
          !           Interpolate to centre points if necessary
          !           -----------------------------------------
          if (typ(iel, ief)=='u') then
             call uv2cen(u(k1)     ,v(k1)     ,kcs       ,n1        ,n2        )
          endif
          !
          !           Transform from magn.,ang. to vector
          !           -----------------------------------
          if (typ(iel, ief)=='v' .or. typ(iel, ief)=='w') then
             call dir2uv(u(k1)     ,v(k1)     ,kcs       ,n1        ,n2        )
          endif
          !
          !           Transform from curvilinear to cartesian
          !           ---------------------------------------
          if (typ(iel, ief)=='t' .or. typ(iel, ief)=='u' .or. typ(iel, ief)     &
            & =='w') then
             call cur2ca(u(k1)     ,v(k1)     ,alfas     ,kcs       ,n1        , &
                       & n2        )
          endif
          ncol = 5
          if (iopt==1) then
             !
             !              Compute vector magnitude
             !              ------------------------
             call vecmag(u(k1)     ,v(k1)     ,um        ,kcs       ,npnt      )
             !
             !              Write vector field
             !              ------------------
             write(13, '(A1,I3.3)') 'M', icel
             write(13, *) npnt, ncol, n1, n2
             do i = 1, npnt
                if (kcs(i)<=0) then
                   write(13, '(a)') '  999.999  999.999  999.999  999.999  999.999'
                else
                   write(13, '(5(1pe15.7))') xcor(i), ycor(i), u(i + k0), v(i + k0), um(i)
                endif
             enddo
          else
             do i = 1, npnt
                umean(i) = umean(i) + u(i + k0)
                vmean(i) = vmean(i) + v(i + k0)
                uvar(i)  = uvar(i)  + u(i + k0)*u(i + k0)
                vvar(i)  = vvar(i)  + v(i + k0)*v(i + k0)
             enddo
          endif
       endif
    !
    !        End of loop over time steps
    !        ---------------------------
    enddo
    !
    !     Write time-averaged field
    !     -------------------------
    if (iopt==2) then
       if (ncol==3) then
          write(13, '(A4)') 'MA01'
          write(13, *) npnt, ncol + 1, n1, n2
          do i = 1, npnt
             umean(i) = umean(i)/dble(ncel)
             arg = uvar(i)/dble(ncel) - umean(i)*umean(i)
             if (arg>0.) then
                uvar(i) = sqrt(arg)
             else
                uvar(i) = 0.
             endif
             if (kcs(i)<=0) then
                write(13, '(a)') '  999.999  999.999  999.999  999.999'
             else
                write(13, '(4(1PE15.7))') xcor(i), ycor(i), umean(i), uvar(i)
             endif
          enddo
       else
          do i = 1, npnt
             umean(i) = umean(i)/dble(ncel)
             arg      = uvar(i)/dble(ncel) - umean(i)*umean(i)
             if (arg>0.) then
                uvar(i) = sqrt(arg)
             else
                uvar(i) = 0.
             endif
             vmean(i) = vmean(i)/dble(ncel)
             arg      = vvar(i)/dble(ncel) - vmean(i)*vmean(i)
             if (arg>0.) then
                vvar(i) = sqrt(arg)
             else
                vvar(i) = 0.
             endif
          enddo
          call vecmag(umean     ,vmean     ,um        ,kcs       ,npnt      )
          call vecmag(uvar      ,vvar      ,uvm       ,kcs       ,npnt      )
          write(13, '(A4)') 'MA01'
          write(13, *) npnt, ncol + 1, n1, n2
          do i = 1, npnt
             if (kcs(i)<=0) then
                write(13, '(a)') '  999.999  999.999  999.999  999.999  999.999  999.999'
            else
                write(13, '(6(1PE15.7))') xcor(i), ycor(i), umean(i), vmean(i), um(i), uvm(i)
             endif
          enddo
       endif
    endif
    !
    deallocate(prefix)
    deallocate(funnam)
    deallocate(xnam  )
    deallocate(ynam  )
    deallocate(alfnam)
    deallocate(grdnam)
    deallocate(nelms )

    deallocate(celidt)

    deallocate(xcor )
    deallocate(ycor )
    deallocate(alfas)
    deallocate(u    )
    deallocate(um   )
    deallocate(umean)
    deallocate(uvar )
    deallocate(v    )
    deallocate(uvm  )
    deallocate(vmean)
    deallocate(vvar )
    !
    deallocate(kcs  )
    !
    deallocate(wrk_2d)
    deallocate(wrk_3d)
    deallocate(wrk_4d)
    !
    close(10)
    close(11)
    close(12)
    close(13)
    ierror = clsnef(deffds)
    if (ierror/=0) then
       call nefout(ierror     ,' ')
    endif
end subroutine
