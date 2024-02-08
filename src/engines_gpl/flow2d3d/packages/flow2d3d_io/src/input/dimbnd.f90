subroutine dimbnd(lunmd     ,lundia    ,error     ,nrrec     ,filtmp    , &
                & nto       ,ntof      ,ntoq      ,ntot      ,ascon     , &
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
!    Function: Reads the dimensions for the open boundary def-
!              initions from md-file or attribute file
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
    integer , pointer :: nlcest
!
! Global variables
!
    integer                    :: lundia !  Description and declaration in inout.igs
    integer                    :: lunmd  !  Description and declaration in inout.igs
    integer                    :: nrrec  !!  Record counter keeping the track of the last record read
    integer                    :: nto    !  Description and declaration in esm_alloc_int.f90
    integer                    :: ntof   !  Description and declaration in dimens.igs
    integer                    :: ntoq   !  Description and declaration in dimens.igs
    integer      , intent(out) :: ntot   !  Description and declaration in dimens.igs
    logical      , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)               :: filtmp !!  File name for open bounday conditions
    character(1) , intent(out) :: ascon  !!  Flag (Y/N) if minimal first boundary data type is ASCON
!
!
! Local variables
!
    integer                        :: ibeg    ! Start column index in a record to lo- cate parameter value 
    integer                        :: iend    ! End column index in a record to loca- te parameter value 
    integer                        :: ier     ! =  0 -> end of record encountered =  1 -> value found = -1 -> empty value ([]) found 
    integer                        :: iocond  ! Reading condition, should be 0 
    integer                        :: ldtbnd  ! Length of character value for DATBND 
    integer                        :: lenc    ! Help variable 
    integer                        :: lfile   ! Help variable 
    integer                        :: lquote
    integer                        :: lr132   ! Length of REC132 -> 132 characters 
    integer                        :: luntmp  ! Unit number of FILTMP 
    integer                        :: nlook   ! Nr. of values to look for in a record 
    integer                        :: nrrec0  ! Record counter keeping the track of the last record read 
    integer                        :: ntrec   ! Current record counter. It's value is changed to detect if all records in the MD-file have been read 
    integer         , external     :: newlun
    logical                        :: lerror  ! Flag=TRUE if an local error is encountered
    logical                        :: newkw   ! Flag to specify if the keyword to look for is a new keyword 
    character(1)                   :: cdum    ! Dummy character variable 
    character(1)                   :: datbnd  ! Boundary data type read (Timedepen- dent or Harmonics) 
    character(11)                  :: fmtdef  ! Default format of an attribute file = blank 
    character(11)                  :: fmttmp  ! Format of FILTMP (UN/FRee formatted) 
    character(12)                  :: fildef  ! Default file name = blank 
    character(132)                 :: rec132  ! Record read from an attribute file 
    character(20)                  :: cdef    ! Default value for chulp 
    character(20)                  :: chulp   ! Help variable to read character from MD-file 
    character(300)                 :: mdfrec  ! Record read from the MD-file 300 = 256 + a bit (field, =, ##, etc.) 
    character(6)                   :: keyw    ! Keyword to look for in the MD-file 
!
!
!! executable statements -------------------------------------------------------
!
    nlcest => gdp%d%nlcest
    !
    ! initialize local parameters
    !
    mdfrec = ' '
    rec132 = ' '
    fmttmp = ' '
    fildef = ' '
    fmtdef = 'FRformatted'
    lfile  = 12
    lr132  = 132
    nlook  = 1
    ldtbnd = 1
    cdef   = ' '
    chulp  = cdef
    lerror = .false.
    newkw  = .true.
    !
    ! locate 'Filbnd' record in case boundary definition is written in a extra input file
    !
    filtmp = fildef
    call prop_get_string(gdp%mdfile_ptr,'*','Filbnd',filtmp)
    if (filtmp/=fildef) then
       !
       ! open boundary definition in file? <YES>
       ! locate 'Fmtbnd' record for format definition of input file
       ! and then look for file format (unformatted / freeformatted)
       !
       keyw = 'Fmtbnd'
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
          if (fmttmp(1:2)=='un') then
          !
          ! unformatted file
          ! read record and add 1 to NTO till end of file
          ! when error occures during reading then no boundaries
          !
          ! -->
  110        continue
             read (luntmp, iostat = iocond) chulp, chulp(:1), datbnd
             !
             ! error while reading IOCOND>0 (not OK),
             ! End-Of-File IOCOND<0 (OK)
             !
             if (iocond/=0) then
                if (iocond>0) then
                   call prterr(lundia    ,'G007'    ,filtmp(:lfile)       )
                   !
                   error = .true.
                   goto 9999
                endif
                !
                ! close file
                !
                close (luntmp)
                goto 710
             endif
             !
             call small(datbnd    ,ldtbnd    )
             if (datbnd=='q') then
                ntoq = ntoq + 1
             elseif (datbnd/='t') then
                ntof = ntof + 1
             else
             endif
             if (datbnd=='a') ascon = 'Y'
             nto = nto + 1
             goto 110
          ! <--
          else
             !
             ! freeformatted file
             ! read record and add 1 to NTO till end of file
             !
             call skipstarlines(luntmp    )
             !
             lquote = index(rec132, '''')
             if (lquote==0) then
             !
             ! semi freeformatted file
             ! read record and add 1 to NTO till end of file
             ! when error occures during reading then no boundaries
             !
             ! -->
  210           continue
                read (luntmp, '(a)', iostat = iocond) rec132
                !
                ! error while reading IOCOND>0 (not OK),
                ! End-Of-File IOCOND<0 (OK)
                !
                if (iocond/=0) then
                   if (iocond>0) then
                      call prterr(lundia    ,'G007'    ,filtmp(:lfile)       )
                      !
                      error = .true.
                      goto 9999
                   endif
                   close (luntmp)
                   goto 710
                endif
                !
                ! read TYPBND from record, empty or long string not allowed
                !
                ibeg = 21
                call read1c(rec132    ,lr132     ,ibeg      ,iend      ,cdum      , &
                          & ldtbnd    ,ier       )
                if (ier<=0) then
                   call prterr(lundia    ,'G007'    ,filtmp(:lfile)       )
                   !
                   error = .true.
                   goto 9999
                endif
                !
                ! read DATBND from record, empty or long
                ! string not allowed
                !
                ibeg = iend + 1
                call read1c(rec132    ,lr132     ,ibeg      ,iend      ,datbnd    , &
                          & ldtbnd    ,ier       )
                if (ier<=0) then
                   call prterr(lundia    ,'G007'    ,filtmp(:lfile)       )
                   !
                   error = .true.
                   goto 9999
                endif
                !
                call small(datbnd    ,ldtbnd    )
                if (datbnd=='q') then
                   ntoq = ntoq + 1
                elseif (datbnd/='t') then
                   ntof = ntof + 1
                else
                endif
                if (datbnd=='a') ascon = 'Y'
                nto = nto + 1
                goto 210
             ! <--
             else
             !
             ! freeformatted file
             ! read record and add 1 to NTO till end of file
             ! when error occures during reading then no boundaries
             !
             ! -->
  310           continue
                read (luntmp, *, iostat = iocond) chulp, chulp(:1), datbnd
                !
                ! error while reading IOCOND>0, End-Of-File IOCOND<0
                !
                if (iocond/=0) then
                   if (iocond>0) then
                      call prterr(lundia    ,'G007'    ,filtmp(:lfile)       )
                      !
                      error = .true.
                      goto 9999
                   endif
                   close (luntmp)
                   goto 710
                endif
                !
                call small(datbnd    ,ldtbnd    )
                if (datbnd=='q') then
                   ntoq = ntoq + 1
                elseif (datbnd/='t') then
                   ntof = ntof + 1
                else
                endif
                if (datbnd=='a') ascon = 'Y'
                nto = nto + 1
                goto 310
             ! <--
             endif
          endif
       !
       ! file does not exist !!
       !
       else
          error = .true.
          goto 9999
       endif
    else
       !
       ! boundary definition is written in a extra input file? <NO>
       !
       ! locate 'Nambnd' record for boundary definition
       !
       ! -->
       nrrec0 = nrrec
  610  continue
       keyw = 'Nambnd'
       ntrec = nrrec0
       nlook = 0
       lenc = 20
       chulp = cdef
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       if (lerror .or. nlook<0) then
          error = .true.
          goto 9999
       endif
       if (nlook==999) goto 710
       !
       ! boundary defintion found (non-blanks in record part)
       !
       if (chulp/=cdef) then
          keyw = 'Datbnd'
          ntrec = nrrec
          nlook = 1
          cdef = 'H'
          datbnd = cdef(:1)
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,datbnd    ,cdef      ,ldtbnd    ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             call prterr(lundia    ,'U101'    ,'Nambnd & ' // keyw  )
             !
             error = .true.
             goto 9999
          endif
          !
          nto = nto + 1
          call small(datbnd    ,ldtbnd    )
          if (datbnd=='q') then
             ntoq = ntoq + 1
          elseif (datbnd/='t') then
             ntof = ntof + 1
          else
          endif
          if (datbnd=='a') ascon = 'Y'
          !
          ! locate next 'Nambnd' record for boundary definition
          !
          newkw = .false.
          goto 610
       endif
    ! <--
    endif
    !
  710 continue
    ntot = nto - ntof - ntoq
    !
9999 continue
    !
    ! nlcest is estimated internally (7*(nmax+mmax)) but might be too small
    ! The user can specify this value himself
    ! nlcest=0: not specified by the user
    !
    nlcest = 0
    call prop_get(gdp%mdfile_ptr,'*','nlcest',nlcest)
end subroutine dimbnd
