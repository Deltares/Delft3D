subroutine dimtrt(lunmd     ,lundia    ,error     ,nrrec     ,gdp       )
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
! Reads the dimensions ntrt, nttaru, nttarv
! Initializes nroupa
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use globaldata
    use string_module
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer  , pointer :: nroupa
    integer  , pointer :: nttaru
    integer  , pointer :: nttarv
    integer  , pointer :: ntrt
!
! Local parameters
!
    integer, parameter :: maxfld = 10
!
! Global variables
!
    integer             :: lundia
    integer             :: lunmd
    integer             :: nrrec
    logical             :: error
!
! Local variables
!
    integer                                       :: ibeg
    integer                                       :: iend
    integer                                       :: iocond
    integer                                       :: lfile
    integer                                       :: luntmp
    integer                                       :: nlook
    integer                                       :: nrflds
    integer                                       :: ntrec
    integer        , dimension(maxfld)            :: ifield
    integer        , dimension(maxfld)            :: itype
    integer        , dimension(maxfld)            :: lenchr
    integer                            , external :: newlun
    logical                                       :: lftrto
    logical                                       :: newkw
    real(fp)       , dimension(maxfld)            :: rfield
    character(10)  , dimension(maxfld)            :: cfield
    character(12)                                 :: fildef
    character(132)                                :: rec132
    character(20)                                 :: cdef
    character(20)                                 :: chulp
    character(256)                                :: filtmp
    character(6)                                  :: keyw
!
!! executable statements -------------------------------------------------------
!
    nroupa        => gdp%gdtrachy%nroupa
    nttaru        => gdp%gdtrachy%nttaru
    nttarv        => gdp%gdtrachy%nttarv
    ntrt          => gdp%gdtrachy%ntrt
    !
    ! Initialize array dimensions for default empty settings
    !
    lftrto = .false.
    ntrt   = 0
    nroupa = 7
    nttaru = 0
    nttarv = 0
    !
    ! Read value of Trtrou, default NO
    !
    chulp = 'N'
    call prop_get_string(gdp%mdfile_ptr,'*','Trtrou',chulp)
    !
    ! set LFTRTO to TRUE if CHULP = Y/y
    !
    call small(chulp     ,1         )
    if (chulp=='y') lftrto = .true.
    !
    ! if Trtrou turned out to be NO, don't look any further.
    !
    if (.not.lftrto) goto 9999
    !
    !
    ! Trtdef: trachytope definition file (must exist, no default)
    !
    filtmp = ' '
    keyw   = 'Trtdef'
    call prop_get_string(gdp%mdfile_ptr,'*',keyw,filtmp)
    !
    ! keyword not found ?
    !
    if (filtmp == ' ') then
       call prterr(lundia    ,'V091'    ,keyw      )
       error = .true.
       goto 9999
    endif
    !
    ! test file existence
    !
    call remove_leading_spaces(filtmp    ,lfile     )
    if (.not.exifil(filtmp, lundia)) then
       !
       ! file does not exist !!
       !
       call prterr(lundia    ,'P101'    ,filtmp(1:lfile)      )
       error = .true.
       goto 9999
    endif
    !
    ! open trachytope definition file
    !
    open (newunit=luntmp, file = filtmp(1:lfile), form = 'formatted', iostat = iocond,  &
        & status = 'old')
    if (iocond/=0) then
       call prterr(lundia    ,'U015'    ,filtmp(1:lfile)      )
       error = .true.
       goto 9999
    endif
    !
    !
    ! freeformatted file
    !           read record and add 1 to NTRT till end of file
    !
    ntrt = 0
    ! -->
    !
    ! read line
    !
  110 continue
    read (luntmp, '(a)', iostat = iocond) rec132
    if (iocond/=0) then
       !
       ! End-of-file ?
       !
       if (iocond<0) goto 199
       !
       ! Reading error
       !
       call prterr(lundia    ,'G007'    ,filtmp(1:lfile)      )
       error = .true.
       ! <--
       !
       ! close file
       !
  199  continue
       close (luntmp)
       if (error) goto 9999
       !
       !
       ! Trtu  : trachytope area file for U-direction
       !         (must exist, no default)
       !
       filtmp = ' '
       keyw = 'Trtu'
       call prop_get_string(gdp%mdfile_ptr,'*',keyw,filtmp)
       !
       ! keyword not found ?
       !
       if (filtmp == ' ' .and. ntrt /= 1) then
          call prterr(lundia    ,'V091'    ,keyw      )
          error = .true.
          goto 9999
       endif
       !
       ! read file and determine value of NTTARU
       !
       if (filtmp == ' ') then
          ! uniform
          nttaru = gdp%d%nmax * gdp%d%mmax
       else
          call dittar(filtmp    ,lundia    ,error     ,nttaru    ,gdp)
          if (error) goto 9999
       endif
       !
       !
       ! Trtv  : trachytope area file for V-direction
       !         (must exist, no default)
       !
       filtmp = ' '
       keyw = 'Trtv'
       call prop_get_string(gdp%mdfile_ptr,'*',keyw,filtmp)
       !
       ! keyword not found ?
       !
       if (filtmp == ' ' .and. ntrt /= 1) then
          call prterr(lundia    ,'V091'    ,keyw      )
          error = .true.
          goto 9999
       endif
       !
       ! read file and determine value of NTTARV
       !
       if (filtmp == ' ') then
          ! uniform
          nttarv = gdp%d%nmax * gdp%d%mmax
       else
          call dittar(filtmp    ,lundia    ,error     ,nttarv    ,gdp)
       endif
       goto 9999
    endif
    !
    ! Interpret line ...
    !
    !
    ! Comment line
    !
    if ((rec132(1:1)=='*') .or. (rec132(1:1)=='#')) goto 110
    ibeg = 1
    iend = 132
    call scannr(rec132    ,ibeg      ,iend      ,nrflds    ,itype     , &
              & ifield    ,rfield    ,cfield    ,lenchr    ,maxfld    , &
              & .true.    ,.true.    ,.true.    )
    !
    ! When sub-fields are found, reserve space
    !
    if (nrflds>0) then
       ntrt = ntrt + 1
    endif
    goto 110
 9999 continue
end subroutine dimtrt
