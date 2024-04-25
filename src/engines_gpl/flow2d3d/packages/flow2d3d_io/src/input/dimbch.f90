subroutine dimbch(lunmd     ,lundia    ,error     ,nrrec     ,kc        , &
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
!    Function: Reads the dimension for harmonic components from
!              the MD-file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use globaldata
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'pardef.igd'
    integer , dimension(:), pointer :: pindex
!
! Global variables
!
    integer                :: kc     !  Description and declaration in dimens.igs
    integer                :: lundia !  Description and declaration in inout.igs
    integer                :: lunmd  !  Description and declaration in inout.igs
    integer                :: nrrec  !!  Record counter keeping the track of the last record read
    logical , intent(out)  :: error  !!  Flag=TRUE if an error is encountered
!
!
! Local variables
!
    integer                          :: ibeg    ! Start column index in a record to locate parameter value 
    integer                          :: iend    ! End column index in a record to locate parameter value 
    integer                          :: ier     ! =  0 -> end of record encountered =  1 -> value found = -1 -> empty value ([]) found 
    integer                          :: iocond  ! Reading condition, should be 0 
    integer                          :: j
    integer                          :: jbeg
    integer                          :: jend
    integer                          :: k
    integer                          :: lenc    ! Number of char. to read in string 
    integer                          :: lfile   ! Number of non blank characters of file name 
    integer                          :: lr132   ! Length of REC132 -> 132 characters 
    integer                          :: lstat
    integer                          :: luntmp  ! Unit number of FILTMP 
    integer                          :: mcmp    ! Logical unit number 
    integer, external                :: newlun
    integer                          :: nlook   ! Nr. of values to look for in a record 
    integer                          :: ntrec   ! Current record counter. It's value is changed to detect if all records in the MD-file have been read 
    logical                          :: defaul  ! Flag to detrmine if a default value is allowed when no value is read 
    logical                          :: lexist  ! Flag to check whether TRIANA file exists
    logical                          :: lerror  ! Flag=TRUE if an local error is encountered
    logical                          :: newkw   ! Flag to specify if the keyword to look for is a new keyword 
    integer                          :: nrcmp   ! Index of component 
    real(fp)                         :: rdef    ! Default value for real parameters 
    real(fp)                         :: rvar    ! Help array for real parameters 
    real(fp)       , dimension(mxkc) :: omega   ! Array to store Frequencies of the open boundary condition of the type DATBND=H 
    character(1)                     :: blank
    character(1)                     :: tab
    character(11)                    :: fmtdef  ! Default format of an attribute file = blank 
    character(11)                    :: fmttmp  ! Format of FILTMP (UN/FRee formatted)
    character(256)                   :: filana  ! File name of the TRIANA-file
    character(12)                    :: fildef  ! Default file name = blank 
    character(132)                   :: rec132
    character(8)                     :: name    ! Name of tidal component 
    character(12)                    :: string
    character(256)                   :: filtmp  ! Attribute file name 
    character(300)                   :: mdfrec  ! Record read from the MD-file 300 = 256 + a bit (field, =, ##, etc.) 
    character(6)                     :: keyw    ! Keyword to look for in the MD-file 
!
! Function called
!
    integer, external                :: numcmp
!
!
!! executable statements -------------------------------------------------------
!
    !
    ! initialize local parameters
    !
    mdfrec = ' '
    rec132 = ' '
    fmttmp = ' '
    fildef = ' '
    name   = ' '
    string = ' '
    filtmp = fildef
    fmtdef = 'FRformatted'
    lfile  = 12
    lstat  = 20
    lr132  = 132
    nlook  = 1
    lerror = .false.
    newkw  = .true.
    defaul = .true.
    rdef   = 0.0_fp
    !
    blank  = char(32)
    tab    = char(09)
    !
    do j = 1, mxkc
       omega(j) = 0.0_fp
    enddo
    !
    filana = ''
    filtmp = ''
    call prop_get(gdp%mdfile_ptr, '*', 'Filana', filana)
    call prop_get(gdp%mdfile_ptr, '*', 'FilbcH', filtmp)
    !
    if (filana/=fildef) then
       !
       ! allocate and initialize pindex
       !
       allocate (gdp%gdbcdat%pindex(0:mxkc), stat=iocond)
       pindex => gdp%gdbcdat%pindex
       pindex = -1
       !
       ! check that the given TRIANA file exists
       !
       inquire (file = filana, exist = lexist)
       if (.not. lexist) then            
          write (*,*) 'TRIANA file ', trim(filana), ' not found.'
          error = .true.
          goto 9999
       endif
       !
       ! determine kc by reading the TRIANA file, and look for components used
       ! first, set pindex(component number)
       !
       open (newunit=mcmp, file = filana)
       do
          read (mcmp, '(a)', iostat = iocond) rec132
          if (iocond < 0) exit
          !
          ! Skip lines start with '*' or '+'
          !
          if (rec132(1:1)=='*' .or. rec132(1:1)=='+') then
             cycle
          endif
          ibeg = 1
          call read1c(rec132    ,lr132     ,ibeg      ,iend      ,string    , &
                    & lstat     ,ier       )
          !
          ! empty line, so continue reading
          !
          if (ier==0) cycle
          !
          ! check whether string is a component name
          !
          name  = string(1:8)
          nrcmp = numcmp(name)
          if (nrcmp < 0) cycle
          pindex(nrcmp) = 1
       enddo
       close (mcmp)
       !
       ! second, count all components found
       ! and build pindex for use in triasc and reacmp
       !
       pindex(0) = 1 ! A0
       kc = 1
       do j = 1, mxkc
          if (pindex(j) == 1) then
             kc = kc + 1
             pindex(j) = kc
          endif
       enddo
    else if (filtmp/=fildef) then
       !
       ! Harmonic boundary conditions in attribute file? <YES>
       ! locate 'FmtbcH' record for format definition of input file
       ! and then look for file format (unformatted / freeformatted)
       !
       fmttmp = ' '
       keyw = 'FmtbcH'
       ntrec = nrrec
       lenc = 11
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,fmttmp    ,fmtdef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          fmttmp = 'FRformatted'
       endif
       call filfmt(lundia    ,keyw      ,fmttmp    ,lerror    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          fmttmp = fmtdef(3:)
       endif
       !
       ! test file existence
       !
       lfile = len(filtmp)
       !
       if (exifil(filtmp, lundia)) then
          !
          ! open input file
          !
          open (newunit=luntmp, file = filtmp(1:lfile), form = fmttmp, status = 'old')
          !
          ! unformatted file, start with KC = MXKC, and substract till actual value for KC is found
          ! NOTE: this does not work on SUN (rvar array and test on 0.)
          !
          if (fmttmp(1:2)=='un') then
             kc = mxkc + 1
             ! -->
  420        continue
             rewind (luntmp)
             kc = kc - 1
             if (kc>0) then
                read (luntmp, iostat = iocond) (rvar, k = 1, kc)
                !
                ! error while reading IOCOND>0, End-Of-File IOCOND<0
                !
                if (iocond/=0) then
                   if (iocond<0) goto 600
                   if (iocond>0) goto 420
                ! <--
                endif
             endif
             close (luntmp)
          !
          else
          !
          ! freeformatted file, read record,
          ! skip lines starting with a '*' and start searching values in record
          !
  425        continue
             read (luntmp, '(a)', iostat = iocond) rec132
             !
             ! error while reading IOCOND>0, End-Of-File IOCOND<0
             !
             if (iocond/=0) then
                if (iocond<0) goto 600
                call prterr(lundia    ,'G007'    ,filtmp(:lfile)       )
                !
                error = .true.
                goto 9999
             endif
             if (rec132(1:1)=='*') goto 425
             ! ==>>
  430        continue
             iend = 0
             ! -->
             !
             ! read value from record and add 1 to kc (ier > 0)
             !
  440        continue
             ibeg = iend + 1
             call read1r(rec132    ,lr132     ,ibeg      ,iend      ,rvar      , &
                       & rdef      ,ier       )
             if (ier>0) then
                kc = kc + 1
                goto 440
             endif
             ! <--
             !
             ! end of record (ier = 0), read new record
             ! if record = blank more omega records or seperation blank record
             !
             if (ier==0) then
                read (luntmp, '(a)', iostat = iocond) rec132
                !
                ! error while reading IOCOND>0, End-Of-File IOCOND<0
                !
                if (iocond/=0) then
                   if (iocond<0) goto 600
                   call prterr(lundia    ,'G007'    ,filtmp(:lfile)       )
                   !
                   error = .true.
                   goto 9999
                endif
                if (rec132/=' ') goto 430
             ! <<==
             endif
          endif
       else
          !
          ! file does not exist !!
          !
          error = .true.
          goto 9999
       endif
       !
       ! End of reading file, close file and reset KC if KC=0
       !
  600  continue
       if (kc==0) kc = 1
       close (luntmp)
    !
    ! harmonic boundary conditions written in an attribute file? <NO>
    !
    else
       !
       ! locate 'Omega' record for boundary definition
       ! all omega values has to be written in successive records
       ! default value allowed => defaul is .true.
       !
       keyw = 'Omega '
       newkw = .true.
       ntrec = nrrec
       nlook = 0
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,omega     ,rdef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          goto 9999
       endif
       kc = mxkc
       ! -->
  510  continue
       if (omega(kc)<=0.0) then
          kc = kc - 1
          if (kc>0) goto 510
       ! <--
       endif
       if (kc==0) kc = 1
    endif
    !
 9999 continue
end subroutine dimbch
