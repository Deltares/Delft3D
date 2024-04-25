subroutine rdq2eb(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                & filq2e    ,nto       ,mmax      ,nmax      ,nmaxus    , &
                & mnbnd     ,rtubnd    ,gdp       )
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
!    Function: - Reads boundary condition for 2D turbulence model
!                from an attribute file
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: itis
!
! Global variables
!
    integer                                                                       :: lundia  !  Description and declaration in inout.igs
    integer                                                                       :: lunmd   !  Description and declaration in inout.igs
    integer                                                         , intent(in)  :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                         , intent(in)  :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: nmaxus  !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: nrrec   !!  Pointer to the record number in the MD-file
    integer                                                         , intent(in)  :: nto     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(7, nto)                                     , intent(in)  :: mnbnd   !  Description and declaration in esm_alloc_int.f90
    logical                                                                       :: error   !!  Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 2), intent(out) :: rtubnd  !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                  :: filq2e  !!  File name containing boundary condition
                                                                                             !!  for 2D turbulence parameter (RTU2D0)
    character(300)                                                                :: mdfrec  !!  Standard rec. length in MD-file (300)
                                                                                             !!  300 = 256 + a bit (field, =, etc.)
!
! Local variables
!
    integer                :: incx
    integer                :: incy
    integer                :: iocond  ! IO status output: > 0 error< 0 end-of-file = 0 ok 
    integer                :: ito
    integer                :: ix
    integer                :: iy
    integer                :: lenc    ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                :: lfile   ! Length of file name 
    integer                :: lkw
    integer                :: luntmp  ! Unit number for attribute file 
    integer                :: m1
    integer                :: m2
    integer                :: maxinc
    integer                :: mb
    integer                :: mxy
    integer                :: n1
    integer                :: n2
    integer                :: nb
    integer                :: nlook   ! Help var.: nr. of data to look for in the MD-file 
    integer                :: ntrec   ! Help. var to keep track of NRREC 
    integer, external      :: newlun
    logical                :: ex      ! Logical flag for file existence 
    logical                :: found   ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                :: lerror  ! Flag=TRUE if an error is encountered 
    logical                :: newkw   ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                :: randpt
    real(fp)               :: rdef    ! Default value for boundary condition input for turbulent quantities should be < 0 !! 
    real(fp), dimension(2) :: rval    ! Help array to read boundary condi- tion input for turbulent quantities 
    character(1)           :: comma
    character(11)          :: errmsg
    character(12)          :: fildef  ! Default file name (usually = blank) 
    character(6)           :: keyw    ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!! executable statements -------------------------------------------------------
!
    itis  => gdp%gdrdpara%itis
    !
    lerror = .false.
    newkw  = .true.
    found  = .true.
    ntrec  = nrrec
    nlook  = 1
    rdef   = 0.0
    comma  = char(44)
    fildef = ' '
    errmsg = '(         )'
    !
    ! initialize arrays
    !
    do nb = 1, nmax
       do mb = 1, mmax
          rtubnd(nb, mb, 1) = rdef
          rtubnd(nb, mb, 2) = rdef
       enddo
    enddo
    !
    ! locate 'Filq2e' record for Boundary condition file containing BC
    ! for 2D turbulence parameters
    !
    keyw  = 'Filq2e'
    ntrec = nrrec
    lkw   = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    !
    ! found ? then read NPRCUS
    !
    if (found) then
       keyw = 'Filq2e'
       ntrec = nrrec
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filq2e    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          filq2e = fildef
          goto 9999
       endif
    endif
    !
    ! test file existence, if so read
    !
    call remove_leading_spaces(filq2e    ,lfile     )
    inquire (file = filq2e(1:lfile), exist = ex)
    if (ex) then
       !
       ! file = exist
       !
       open (newunit=luntmp, file = filq2e(1:lfile), form = 'formatted', status = 'old')
       !
       !-->     read boundary condition (at each boundary point, optional)
       !
 1000  continue
       read (luntmp, *, iostat = iocond) mb, nb, rval(1), rval(2)
       if (iocond /= 0) then
          !
          ! reading error?
          !
          if (iocond > 0) then
             call prterr(lundia    ,'G007'    ,filq2e(1:lfile)      )
             error = .true.
          endif
          goto 1100
       endif
       !
       ! Define boundary conditions
       ! Loop over boundary for rows
       ! The definition of the open boundary has already been checked
       ! previously therefore the outcome of error in INCREM will
       ! never be .true.
       !
       randpt = .false.
       do ito = 1, nto
          m1 = mnbnd(1, ito)
          n1 = mnbnd(2, ito)
          m2 = mnbnd(3, ito)
          n2 = mnbnd(4, ito)
          call increm(m1        ,n1        ,m2        ,n2        ,incx      , &
                    & incy      ,maxinc    ,error     )
          ix = m1 - incx
          iy = n1 - incy
          do mxy = 1, maxinc + 1
             ix = ix + incx
             iy = iy + incy
             if (ix==mb .and. iy==nb) then
                randpt = .true.
                rtubnd(nb, mb, 1) = rval(1)
                rtubnd(nb, mb, 2) = rval(2)
             endif
          enddo
       enddo
       if (randpt) goto 1000
       write (errmsg(2:10), '(i4,a1,i4)') mb, comma, nb
       call prterr(lundia    ,'V248'    ,errmsg    )
       !
       !
       ! <--    next boundary point
       !
       !
       ! close file
       !
 1100  continue
       close (luntmp)
    else
       !
       ! file = not exist
       !
       call prterr(lundia    ,'G004'    ,filq2e(1:lfile)      )
       !
       ! no open boundary conditions specified!!!
       ! error  = .true.
       !
    endif
 9999 continue
end subroutine rdq2eb
