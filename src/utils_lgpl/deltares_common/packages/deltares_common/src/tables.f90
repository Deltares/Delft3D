module tables
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
! Tables module
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use string_module
    use time_module, only: ymd2jul
    implicit none
    !
    public tablefiletype
    !
    public org_readtable
    public org_cleartable
    public org_getntables
    public org_gettable
    public org_checktable
    public org_checktableparnames
    public org_gettablelocation
    public org_gettablentimes
    public org_gettabletimes
    public org_gettabledata
    public org_getfilename
    !
    integer, parameter, public :: MAXTABLECLENGTH = 47

    integer, parameter, public :: CHKTAB_PARNAME  = 1
    integer, parameter, public :: CHKTAB_POSITIVE = 2
    integer, parameter, public :: CHKTAB_BLOCK    = 3
    integer, parameter, public :: CHKTAB_LOGICAL  = 4

    interface org_gettable
       module procedure org_gettable_vector, org_gettable_scalar
    end interface

    interface org_gettabledata
       module procedure org_gettabledata_vector, org_gettabledata_scalar
    end interface

    type tableparametertype
        character(MAXTABLECLENGTH) :: name
        character(MAXTABLECLENGTH) :: unit
        character(MAXTABLECLENGTH) :: interpolation
    end type tableparametertype

    type tabletype
        real(hp)                                          :: timestep
        real(hp)                                          :: timeunit
        real(fp)                ,dimension(3)             :: geocoords
        real(fp)                ,dimension(3)             :: metriccoords
        real(fp)                                          :: reftime
        integer                                           :: refdate
        integer                                           :: layer
        integer                                           :: nrecords
        integer                                           :: nparameters
        type(tableparametertype), dimension(:)  , pointer :: parameters
        real(hp)                , dimension(:)  , pointer :: times
        real(fp)                , dimension(:,:), pointer :: values
        character(MAXTABLECLENGTH)                        :: name
        character(MAXTABLECLENGTH)                        :: contents
        character(MAXTABLECLENGTH)                        :: location
        character(MAXTABLECLENGTH)                        :: interpolation
        character(MAXTABLECLENGTH)                        :: extrapolation
        character(MAXTABLECLENGTH)                        :: timeunitstr
        character(MAXTABLECLENGTH)                        :: timefunction
    end type tabletype

    type tablefiletype
        character(256)                         :: filename = 'UNKNOWN'
        type(tabletype), dimension(:), pointer :: tables => NULL()
    end type tablefiletype
!
!
!
contains
!
!
!===============================================================================
subroutine org_readtable(this, filnam, refjulday, errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Read table files (i.e. BCT/BCC/BCM)
!
!!------------------------------------------------------------------------------
!
! Local parameters
!
    integer, parameter :: MAXFLD    = 400
    integer, parameter :: MAXERRSTR = 256
    integer, parameter :: MAXLINE   = 4096
    integer, parameter :: INT_READ  = 1  ! used by itype
    integer, parameter :: REAL_READ = 2  ! used by itype
    integer, parameter :: CHAR_READ = 3  ! used by itype
!
! Global variables
!
    integer             , intent(in)  :: refjulday
    character(*)        , intent(in)  :: filnam
    character(MAXERRSTR), intent(out) :: errorstring
    type(tablefiletype)               :: this
!
! Local variables
!
    integer                                               :: i
    integer                                               :: idd
    integer                                               :: ierr
    integer                                               :: ihh
    integer                                               :: ijuldate
    integer                                               :: imm
    integer                                               :: iss
    integer                                               :: iline
    integer                                               :: ipar
    integer                                               :: ipar0
    integer                                               :: iread_phase
    integer                                               :: irec
    integer                                               :: istat
    integer                                               :: itable
    integer                                               :: itimestep
    integer                                               :: iyyyymmdd
    integer                                               :: lstri1
    integer                                               :: lstri2
    integer                                               :: lstri3
    integer                                               :: lunbcm
    integer                                               :: nrecords
    integer                                               :: ntables
    integer                                               :: ntoken
    integer                                               :: nvalues
    integer                   , dimension(:), allocatable :: itype
    integer                   , dimension(:), allocatable :: ifield
    integer                   , dimension(:), allocatable :: lenchr
    !
    logical                                               :: feof
    logical                                               :: error
    logical                                               :: isdata
    !
    real(fp)                  , dimension(:), allocatable :: rfield
    !
    character(MAXTABLECLENGTH), dimension(:), allocatable :: cfield
    character(10)                                         :: stri1
    character(10)                                         :: stri2
    character(MAXLINE)                                    :: line
    !
    type(tabletype)           , dimension(:), pointer     :: tables => NULL()
    type(tabletype)                         , pointer     :: table
!
!! executable statements -------------------------------------------------------
!
    istat       = 0
    error       = .false.
    errorstring = 'org_readtable: memory alloc error'
    !
    open(newunit=lunbcm, file = filnam, form = 'formatted', &
        & status = 'old', iostat = istat)
    if (istat /= 0) then
       errorstring = '*** ERROR Error while opening file '//trim(filnam)
       goto 210
    endif
    this%filename = filnam
    !
    ! Allocate scannr arrays
    !
                    allocate(itype(MAXFLD) , stat = istat)
    if (istat == 0) allocate(ifield(MAXFLD), stat = istat)
    if (istat == 0) allocate(rfield(MAXFLD), stat = istat)
    if (istat == 0) allocate(cfield(MAXFLD), stat = istat)
    if (istat == 0) allocate(lenchr(MAXFLD), stat = istat)
    if (istat /= 0) goto 210
    itype  = 0
    ifield = -999
    rfield = -999.0
    cfield = 'undefined'
    !
    ! Count number of tables in file
    !
    ntables = 0
    do iread_phase = 1,3
       !
       ! Begin of iread_phase loop
       !          1: determine number of tables
       !          2: determine number of parameters and records per table
       !          3: read parameters and data values
       !
       itable = 0
       iline  = 0
       feof   = .false.
       do while (.not.feof)
          !
          ! table loop
          !
          itable = itable + 1
          ipar   = 0
          !
          if (iread_phase > 1) then
             table => tables(itable)
          endif
          !
          if (iread_phase == 3) then
             !
             ! time column should be treated in a different manner
             !
             if (trim(table%timefunction) == 'non-equidistant') then
                ipar = -1
             endif
          endif
          !
          isdata = .false.
          !
          nvalues = 0
          !
          do while (.not.isdata)
             !
             ! keyword loop for current table
             !
             ! => process keyword
             !
             if (iline == 0) then
                !
                ! No line read yet
                !
             elseif (itype(1) == CHAR_READ) then
                !
                ! character parameter read
                !
                call org_readtable_keyword()
             endif
             if (error) goto 200
             !
             ! Read new line
             !
             iline = iline+1
             read (lunbcm, '(A)', iostat = istat) line
             lstri1 = len_trim(line)
             if (lstri1 > MAXLINE - 10) then
                 write (*,*) 'warning: line number ', iline, ' of file ', filnam, 'possibly too long.'
             endif
             if (istat < 0) feof = .true.
             !
             ntoken = 0
             call scannr(   line,       1,        len(line),  ntoken,   itype, &
                       &  ifield,  rfield,  cfield,  lenchr,  MAXFLD,  .true., &
                       & .false., .false.)
             !
             if (ntoken<0) goto 200
             !
             !----------------------------------------------
             !  debug:
             !  do i = 1, ntoken
             !     if (itype(i) == INT_READ) then
             !        write(*,*) '>',ifield(i),'<'
             !     elseif (itype(i) == REAL_READ) then
             !        write(*,*) '>',rfield(i),'<'
             !     elseif (itype(i) == CHAR_READ) then
             !        write(*,*) '>',cfield(i),'<'
             !     endif
             !  enddo
             !----------------------------------------------
             !
             if (itype(1) /= CHAR_READ) then
                if (ipar == 0) then
                   errorstring = 'Data encountered before parameters definition'
                   goto 200
                else
                   !
                   ! Goto data loop
                   !
                   isdata = .true.
                endif
             endif
             !
             ! End of keyword loop for current table
             !
          enddo
          !
          if (iread_phase == 2) then
             if (table%timeunitstr == 'ddhhmmss' .and. &
               & table%timefunction == 'equidistant') then
                if (table%timestep < 0.0_hp) then
                   errorstring = 'Missing time-step in equidistant table'
                   goto 210
                else
                   itimestep = int(table%timestep)
                   iss = mod(itimestep,100)
                   itimestep = (itimestep - iss)/100
                   imm = mod(itimestep,100)
                   itimestep = (itimestep - imm)/100
                   ihh = mod(itimestep,100)
                   idd = (itimestep - ihh)/100
                   !
                   table%timestep = real(idd,hp) + &
                                  & real(ihh,hp) / 60.0_hp + &
                                  & real(imm,hp) / 1440.0_hp + &
                                  & real(iss,hp) / 86400.0_hp
                   table%timeunitstr = 'days'
                endif
             endif
          endif
          !
          irec = 1
          ipar = 1
          !
          if (iread_phase == 3) then
             !
             ! time column should be treated in a different manner
             !
             if (trim(table%timefunction) == 'non-equidistant') then
                ipar = 0
             endif
          endif
          !
          ! store initial ipar value for use on each record line
          !
          ipar0 = ipar
          !
          call org_readtable_data()
       enddo
       !
       select case(iread_phase)
       case(1)
          !
          ! Allocate space for the tables
          !
          ntables = itable
          !
          ! write(*,*) 'NTables = ',ntables
          !
          allocate (this%tables(ntables), stat = istat)
          if (istat /= 0) goto 210
          tables => this%tables
          !
          do itable = 1, ntables
             table => tables(itable)
             !
             table%timestep      = -999.0_hp
             table%geocoords     = -999.0_fp
             table%metriccoords  = -999.0_fp
             table%layer         = -999
             table%refdate       = -999
             table%reftime       = -999.0_fp
             table%nrecords      = -999
             table%nparameters   = 0
             write(table%name,'(A,I4,A)') 'Table',itable,' (nameless)'
             table%contents      = ''
             table%location      = ''
             table%interpolation = 'linear'
             table%extrapolation = 'none'
             table%timeunitstr   = 'minutes'
             table%timeunit      = 1.0_hp / 1440.0_hp
             table%timefunction  = 'non-equidistant'
          enddo
          !
          rewind(lunbcm)
       case(2)
          !
          ! Allocate space for the parameters and data
          !
          do itable = 1, ntables
             table => tables(itable)
             !
             if (table%refdate == -999 .and. table%timeunitstr /= 'date') then
                errorstring = 'Missing reference-date record in table ''' // &
                            & trim(table%name) // ''''
                goto 210
             endif
             !
             if (trim(table%timefunction) == 'non-equidistant') then
                !
                ! First column (times) will be stored in separate array
                !
                table%nparameters = table%nparameters - 1
             endif
             !
                             allocate(table%parameters(table%nparameters), stat = istat)
             if (istat == 0) allocate(table%times(table%nrecords), stat = istat)
             if (istat == 0) allocate(table%values(table%nrecords,table%nparameters), stat = istat)
             if (istat /= 0) goto 210
          enddo
          !
          rewind(lunbcm)
       case(3)
          !
          ! File read successfully
          !
          close (lunbcm)
       endselect
       !
       ! End of iread_phase loop
       !
    enddo
    !
    ! Deallocate scannr arrays
    !
    deallocate(itype , stat=ierr)
    deallocate(ifield, stat=ierr)
    deallocate(rfield, stat=ierr)
    deallocate(cfield, stat=ierr)
    deallocate(lenchr, stat=ierr)
    !
    ! File read successfully; don't report an error now ...
    !
    errorstring = ' '
    return
    !
200 continue
    if (ntoken < 0) then
       if (ntoken == -2) then
          errorstring = 'Line parse error: too many sub-fields'
       elseif (ntoken == -3) then
          errorstring = 'Line parse error: sub-field string too long'
       elseif (ntoken == -4) then
          errorstring = 'Line parse error: unmatched quotes'
       else
          errorstring = 'Line parse error'
       endif
    endif
    lstri1 = len_trim(filnam)
    lstri2 = len_trim(line)
    lstri3 = len_trim(errorstring)
    !
    ! 21 is length of 'Reading XLine I004: X'
    !
    if (lstri1 + lstri3 < MAXERRSTR - 21 - 1) then
       if (lstri1 + lstri2 + lstri3 >= MAXERRSTR - 21) then
          lstri2 = MAXERRSTR - 21 - 1 - lstri1 - lstri3
       endif
       write(errorstring,'(4A,I4,4A)') &
          & 'Reading ',trim(filnam),char(10), &
          & 'Line ',iline,': ',line(1:lstri2),char(10), &
          & trim(errorstring)
    endif
210 continue
!
!
!
contains
!
!
!===============================================================================
subroutine org_readtable_keyword()
    call str_lower(cfield(1),len(cfield(1)))
    if (org_readtable_isComment(cfield(1))) then
       !
       ! Skip comments and record length
       !
    elseif (trim(cfield(1)) == 'table-name') then
       if (iread_phase == 2) then
          if (ntoken < 2) then
             errorstring = 'Too few arguments on line'
             error = .true.
             goto 100
          elseif (ntoken > 2 .and. .not.org_readtable_isComment(cfield(3))) then
             errorstring = 'Too many arguments on line'
             error = .true.
             goto 100
          elseif (itype(2) /= CHAR_READ) then
             errorstring = 'Table name must be a string'
             error = .true.
             goto 100
          else
             table%name = cfield(2)
          endif
       endif
    elseif (trim(cfield(1)) == 'contents') then
       if (iread_phase == 2) then
          if (ntoken < 2) then
             errorstring = 'Too few arguments on line'
             error = .true.
             goto 100
          elseif (ntoken > 2 .and. .not.org_readtable_isComment(cfield(3))) then
             errorstring = 'Too many arguments on line'
             error = .true.
             goto 100
          elseif (itype(2) /= CHAR_READ) then
             errorstring = 'Contents must be a string'
             error = .true.
             goto 100
          else
             call str_lower(cfield(2),len(cfield(2)))
             table%contents = cfield(2)
          endif
       endif
    elseif (trim(cfield(1))=='geo-coordinates') then
       !
       ! geographical co-ordinates
       !
       if (iread_phase == 2) then
          if (ntoken<3) then
             errorstring = 'Missing coordinate data'
             error = .true.
             goto 100
          else
             !
             ! geo-coordinates <long> <lat> [depth]
             ! geo-coordinates <long> <lat> layer <layer number>
             !
             if (itype(2)==INT_READ .or. itype(2)==REAL_READ) then
                table%geocoords(1) = rfield(2)
             else
                errorstring = 'Longitude must be a numeric value'
                error = .true.
                goto 100
             endif
             if (itype(3)==INT_READ .or. itype(3)==REAL_READ) then
                table%geocoords(2) = rfield(3)
             else
                errorstring = 'Latitude must be a numeric value'
                error = .true.
                goto 100
             endif
             if (ntoken==3) then
                !
                ! no optional elevation data
                !
             elseif (ntoken==4) then
                !
                ! geo-coordinates <long> <lat> [depth]
                !
                if (itype(4)==INT_READ .or. itype(4)==REAL_READ) then
                   table%geocoords(3) = rfield(4)
                else
                   errorstring = 'Depth must be a numeric value'
                   error = .true.
                   goto 100
                endif
             elseif (ntoken==5) then
                !
                ! geo-coordinates <long> <lat> layer <layer number>
                !
                call str_lower(cfield(4),len(cfield(4)))
                if (cfield(4) /= 'layer') then
                   errorstring = 'Expected keyword ''layer'''
                   error = .true.
                   goto 100
                elseif (itype(5) /= INT_READ .or. ifield(5) < 0) then
                   errorstring = 'Layer must be a non-negative integer'
                   error = .true.
                   goto 100
                else
                   table%layer = ifield(5)
                endif
             else ! if (ntoken > 5) then
                errorstring = 'Too many arguments on line'
                error = .true.
                goto 100
             endif
          endif
       endif
    elseif (trim(cfield(1)) == 'location') then
       if (iread_phase == 2) then
          if (ntoken < 2) then
             errorstring = 'Too few arguments on line'
             error = .true.
             goto 100
          elseif (ntoken > 2 .and. .not.org_readtable_isComment(cfield(3))) then
             errorstring = 'Too many arguments on line'
             error = .true.
             goto 100
          elseif (itype(2) /= CHAR_READ) then
             errorstring = 'Location must be a string'
             error = .true.
             goto 100
          else
             table%location = cfield(2)
          endif
       endif
    elseif (trim(cfield(1)) == 'interpolation') then
       if (ipar <= 0) then
          if (iread_phase == 2) then
             if (ntoken < 2) then
                errorstring = 'Too few arguments on line'
                error = .true.
                goto 100
             elseif (ntoken > 2 .and. .not.org_readtable_isComment(cfield(3))) then
                errorstring = 'Too many arguments on line'
                error = .true.
                goto 100
             elseif (itype(2) /= CHAR_READ) then
                errorstring = 'Interpolation type must be a string'
                error = .true.
                goto 100
             else
                call str_lower(cfield(2),len(cfield(2)))
                table%interpolation = cfield(2)
                if (cfield(2) /= 'linear' .and. &
                  & cfield(2) /= 'block') then
                   errorstring = 'Interpolation type must be ''linear'' or ''block'''
                   error = .true.
                   goto 100
                endif
             endif
          endif
       else
          if (iread_phase == 3) then
             if (ntoken < 2) then
                errorstring = 'Too few arguments on line'
                error = .true.
                goto 100
             elseif (ntoken > 2 .and. .not.org_readtable_isComment(cfield(3))) then
                errorstring = 'Too many arguments on line'
                error = .true.
                goto 100
             elseif (itype(2) /= CHAR_READ) then
                errorstring = 'Interpolation type must be a string'
                error = .true.
                goto 100
             else
                call str_lower(cfield(2),len(cfield(2)))
                table%parameters(ipar)%interpolation = cfield(2)
                if (cfield(2) /= 'linear' .and. &
                  & cfield(2) /= 'block') then
                   errorstring = 'Interpolation type must be ''linear'' or ''block'''
                   error = .true.
                   goto 100
                endif
             endif
          endif
       endif
    elseif (trim(cfield(1)) == 'extrapolation') then
       if (iread_phase == 2) then
          if (ntoken < 2) then
             errorstring = 'Too few arguments on line'
             error = .true.
             goto 100
          elseif (ntoken > 2 .and. .not.org_readtable_isComment(cfield(3))) then
             errorstring = 'Too many arguments on line'
             error = .true.
             goto 100
          elseif (itype(2) /= CHAR_READ) then
             errorstring = 'Extrapolation type must be a string'
             error = .true.
             goto 100
          else
             call str_lower(cfield(2),len(cfield(2)))
             table%extrapolation = cfield(2)
             if (cfield(2) /= 'periodic' .and. &
               & cfield(2) /= 'constant' .and. &
               & cfield(2) /= 'none') then
                errorstring = 'Extrapolation type must be ''periodic'', ''constant'' or ''none'''
                error = .true.
                goto 100
             endif
          endif
       endif
    elseif (trim(cfield(1)) == 'records-in-table') then
       if (iread_phase == 2) then
          if (ntoken < 2) then
             errorstring = 'Too few arguments on line'
             error = .true.
             goto 100
          elseif (ntoken > 2 .and. .not.org_readtable_isComment(cfield(3))) then
             errorstring = 'Too many arguments on line'
             error = .true.
             goto 100
          elseif (itype(2) /= INT_READ) then
             errorstring = 'Number of records in table must be positive integer'
             error = .true.
             goto 100
          else
             table%nrecords = ifield(2)
             if (ifield(2) <= 0) then
                errorstring = 'Number of records in table must be positive integer'
                error = .true.
                goto 100
             endif
          endif
       endif
    elseif (trim(cfield(1)) == 'metric') then
       !
       ! metric co-ordinates
       !
       if (iread_phase == 2) then
          call str_lower(cfield(2),len(cfield(2)))
          if (itype(2) /= CHAR_READ) then
             errorstring = 'Unknown keyword: '//trim(cfield(1))
             error = .true.
             goto 100
          elseif (trim(cfield(2)) /= 'coordinates') then
             errorstring = 'Unknown keyword: '//trim(cfield(1))//' '//trim(cfield(2))
             error = .true.
             goto 100
          elseif (ntoken<4) then
             errorstring = 'Missing coordinate data'
             error = .true.
             goto 100
          else
             !
             ! metric coordinates <x> <y> [depth]
             ! metric coordinates <x> <y> layer <layer number>
             !
             if (itype(3) /= CHAR_READ) then
                table%metriccoords(1) = rfield(3)
             else
                errorstring = 'X co-ordinate must be a numeric value'
                error = .true.
                goto 100
             endif
             if (itype(4) /= CHAR_READ) then
                table%metriccoords(2) = rfield(4)
             else
                errorstring = 'Y co-ordinate must be a numeric value'
                error = .true.
                goto 100
             endif
             if (ntoken == 4) then
                !
                ! no optional elevation data
                !
             elseif (ntoken == 5) then
                !
                ! metric coordinates <x> <y> [depth]
                !
                if (itype(5) /= CHAR_READ) then
                   table%metriccoords(3) = rfield(5)
                else
                   errorstring = 'Depth must be a numeric value'
                   error = .true.
                   goto 100
                endif
             elseif (ntoken == 6) then
                !
                ! metric coordinates <x> <y> layer <layer number>
                !
                call str_lower(cfield(5),len(cfield(5)))
                if (cfield(5) /= 'layer') then
                   errorstring = 'Expected keyword ''layer'''
                   error = .true.
                   goto 100
                elseif (itype(6) /= INT_READ .or. ifield(6) < 0) then
                   errorstring = 'Layer must be a non-negative integer'
                   error = .true.
                   goto 100
                else
                   table%layer = ifield(6)
                endif
             else ! if (ntoken > 6) then
                errorstring = 'Too many arguments on line'
                error = .true.
                goto 100
             endif
          endif
       endif
    elseif (trim(cfield(1)) == 'layer') then
       !
       ! layer <layer number>
       !
       if (iread_phase == 2) then
          if (ntoken < 2) then
             errorstring = 'Too few arguments on line'
             error = .true.
             goto 100
          elseif (ntoken > 2 .and. .not.org_readtable_isComment(cfield(3))) then
             errorstring = 'Too many arguments on line'
             error = .true.
             goto 100
          elseif (itype(2) /= INT_READ .or. ifield(2) < 0) then
             errorstring = 'Layer must be a non-negative integer'
             error = .true.
             goto 100
          else
             table%layer = ifield(2)
          endif
       endif
    elseif (trim(cfield(1)) == 'time-unit') then
       if (iread_phase == 2) then
          if (ntoken < 2) then
             errorstring = 'Too few arguments on line'
             error = .true.
             goto 100
          elseif (ntoken > 2 .and. .not.org_readtable_isComment(cfield(3))) then
             errorstring = 'Too many arguments on line'
             error = .true.
             goto 100
          elseif (itype(2) /= CHAR_READ) then
             errorstring = 'Time unit must be a string'
             error = .true.
             goto 100
          else
             call str_lower(cfield(2),len(cfield(2)))
             table%timeunitstr = cfield(2)
             if (trim(cfield(2)) == 'date' .or. trim(cfield(2)) == 'absolute') then
                 table%timeunit = -1.0_hp
                 table%timeunitstr = 'date'
             elseif (trim(cfield(2)) == 'years') then
                 table%timeunit = 365.0_hp
             elseif (trim(cfield(2)) == 'decades') then
                 table%timeunit = 3650.0_hp
             elseif (trim(cfield(2)) == 'days') then
                 table%timeunit = 1.0_hp
             elseif (trim(cfield(2)) == 'hours') then
                 table%timeunit = 1.0_hp / 24.0_hp
             elseif (trim(cfield(2)) == 'minutes') then
                 table%timeunit = 1.0_hp / 1440.0_hp
             elseif (trim(cfield(2)) == 'seconds') then
                 table%timeunit = 1.0_hp / 86400.0_hp
             elseif (trim(cfield(2)) == 'ddhhmmss') then
                 table%timeunit = 1.0_hp
             else
                errorstring = 'Time unit must be ''date'', ''years'', ''decades'', ''days'', ''hours'', &
                 ''minutes'', ''seconds'', ''ddhhmmss'''
                error = .true.
                goto 100
             endif
          endif
       endif
    elseif (trim(cfield(1)) == 'time-step') then
       if (iread_phase == 2) then
          if (ntoken < 2) then
             errorstring = 'Too few arguments on line'
             error = .true.
             goto 100
          elseif (ntoken > 2 .and. .not.org_readtable_isComment(cfield(3))) then
             errorstring = 'Too many arguments on line'
             error = .true.
             goto 100
          elseif (itype(2) /= CHAR_READ) then
             table%timestep = rfield(2)
          else
             errorstring = 'Time step must be a numeric value'
          endif
       endif
    elseif (trim(cfield(1)) == 'reference-time') then
       if (iread_phase == 2) then
          if (ntoken < 2) then
             errorstring = 'Too few arguments on line'
             error = .true.
             goto 100
          elseif (ntoken > 3 .and. .not.org_readtable_isComment(cfield(4))) then
             errorstring = 'Too many arguments on line'
             error = .true.
             goto 100
          elseif (itype(2) == INT_READ) then
             if (ntoken == 2) then
               !
               ! <yyyymmdd>
               !
               table%refdate = ymd2jul(ifield(2))
               table%reftime = 0.0_fp
             elseif (ntoken == 3 .and. itype(3) == INT_READ) then
               !
               ! <yyyymmdd> <hhmmss>
               !
               table%refdate = ymd2jul(ifield(2))
               ihh = ifield(3) / 10000
               ifield(3) = ifield(3) - ihh * 10000
               imm = ifield(3) / 100
               iss = ifield(3) - imm * 100
               table%reftime = real(  real(ihh,hp) / 60.0_hp   + &
                                    & real(imm,hp) / 1440.0_hp + &
                                    & real(iss,hp) / 86400.0_hp   ,fp)
             else
                errorstring = 'Invalid reference time specification'
                error = .true.
                goto 100
             endif
          elseif (itype(2) == REAL_READ) then
             errorstring = 'Invalid reference time specification'
             error = .true.
             goto 100
          else ! if (itype(2) == CHAR_READ) then
             call str_lower(cfield(2),len(cfield(2)))
             table%refdate = refjulday
             table%reftime = 0.0_fp
             if (trim(cfield(2)) /= 'from model') then
                errorstring = 'Reference time must be explicitly specified or it should be ''from model'''
                error = .true.
                goto 100
             endif
          endif
       endif
    elseif (trim(cfield(1)) == 'constant') then
       if (iread_phase == 2) then
          if (ntoken < 1) then
             errorstring = 'Too few arguments on line'
             error = .true.
             goto 100
          elseif (ntoken > 1 .and. .not.org_readtable_isComment(cfield(2))) then
             errorstring = 'Too many arguments on line'
             error = .true.
             goto 100
          else
             table%timefunction = 'constant'
             table%nrecords = 1
          endif
       endif
    elseif (trim(cfield(1)) == 'time-function') then
       if (iread_phase == 2) then
          if (ntoken < 2) then
             errorstring = 'Too few arguments on line'
             error = .true.
             goto 100
          elseif (ntoken > 2 .and. .not.org_readtable_isComment(cfield(3))) then
             errorstring = 'Too many arguments on line'
             error = .true.
             goto 100
          elseif (itype(2) /= CHAR_READ) then
             errorstring = 'Time function must be a string'
             error = .true.
             goto 100
          else
             call str_lower(cfield(2),len(cfield(2)))
             table%timefunction = cfield(2)
             if (trim(cfield(2)) /= 'astronomic' .and. &
               & trim(cfield(2)) /= 'harmonic' .and. &
               & trim(cfield(2)) /= 'equidistant' .and. &
               & trim(cfield(2)) /= 'non-equidistant') then
                errorstring = 'Time function must be ''astronomic'', ''harmonic'', ''equidistant'' or ''non-equidistant'''
                error = .true.
                goto 100
             endif
          endif
       endif
    elseif (trim(cfield(1))=='parameter') then
       ipar = ipar + 1
       if (iread_phase == 2) then
          table%nparameters = ipar
       elseif (iread_phase == 3) then
          if (ntoken < 2) then
             errorstring = 'Too few arguments on line'
             error = .true.
             goto 100
          elseif (ntoken > 4 .and. .not.org_readtable_isComment(cfield(5))) then
             errorstring = 'Too many arguments on line'
             error = .true.
             goto 100
          elseif (itype(2) /= CHAR_READ) then
             errorstring = 'Parameter name must be a string'
             error = .true.
             goto 100
          elseif (ntoken < 4) then
             errorstring = 'Missing unit string'
             error = .true.
             goto 100
          elseif (itype(4) /= CHAR_READ) then
             errorstring = 'Unit must be a string'
             error = .true.
             goto 100
          endif
          if (ipar == 0) then
             !
             ! Time column should be skipped; check whether
             ! the parameter is indeed called 'time'.
             !
             call str_lower(cfield(2),MAXTABLECLENGTH)
             ! allow 'time' and 'time starting at ...' (in case of reuse TMP files)
             if (trim(cfield(2)(1:5)) /= 'time ') then
                errorstring = 'Parameter in first column should be ''time'''
                error = .true.
                goto 100
             endif
          else
             !
             ! Normal data column
             !
             table%parameters(ipar)%name = cfield(2)
             table%parameters(ipar)%unit = cfield(4)
             table%parameters(ipar)%interpolation = table%interpolation
          endif
       endif
    else
       if (.not.org_readtable_isComment(cfield(1))) then
          errorstring = 'Unknown keyword: '//cfield(1)
          error = .true.
          goto 100
       endif
    endif
    !
100 continue
end subroutine org_readtable_keyword
!
!
!===============================================================================
subroutine org_readtable_data()
    do while (isdata .and. (.not.feof))
       !
       ! data loop for current table
       !
       if (iread_phase >= 2) then
label_token: do i = 1, ntoken
             !
             if (itype(i) == CHAR_READ) then
                !
                ! a string in the table is not always an error since
                ! in case of time-units 'date' the yyyymmddhhmmss
                ! value cannot be converted to a single precision
                ! real or long integer
                !
                if (org_readtable_isComment(cfield(i))) then
                   !
                   ! skip comments also in table. The whole remainder of
                   ! the line should be interpreted as a comment, so jump
                   ! out of the token loop
                   !
                   exit label_token
                elseif (ipar == 0 .and. trim(table%timeunitstr) == 'date') then
                   !
                   ! okay continue
                   !
                else
                   write(errorstring,*) 'Unexpected string encountered while reading data'
                   error = .true.
                   goto 150
                endif
             endif
             !
             if (iread_phase == 2) then
                !
                ! count values only, don't put them in the array since that
                ! has not yet been allocated
                !
                nvalues = nvalues + 1
                !
             elseif (ipar == 0) then
                !
                ! time column
                ! table%timefunction = 'non-equidistant'
                !
                ! copy contents of time column to times array instead of
                ! copying it to the values array.
                !
                select case(trim(table%timeunitstr))
                case ('date')
                   !
                   ! yyyymmddhhmmss
                   !
                   ! this value may be indicated as string, integer or
                   ! floating point value depending on the kind settings
                   ! always use the string value cfield(i)
                   !
                   read(cfield(i),'(I8,I2,I2,I2)') iyyyymmdd,ihh,imm,iss
                   ijuldate = ymd2jul(iyyyymmdd)
                   table%times(irec) = real(ijuldate,hp) + &
                                     & real(ihh,hp) / 60.0_hp + &
                                     & real(imm,hp) / 1440.0_hp + &
                                     & real(iss,hp) / 86400.0_hp
                case default
                   !
                   ! Insert time into table; itype cannot be CHAR_READ since
                   ! that exception has already been caught above
                   !
                   table%times(irec) = real(rfield(i),hp) * table%timeunit
                end select
                !
             else
                !
                ! data column
                !
                if (ipar == 1) then
                   !
                   ! generate time column when not available from file
                   !
                   if (trim(table%timefunction) == 'constant') then
                      table%times(irec) = 0.0_hp
                   elseif (trim(table%timefunction) == 'equidistant') then
                      table%times(irec) = real(irec-1,hp) * table%timestep &
                                                        & * table%timeunit
                   endif
                endif
                !
                ! Insert value into table; itype cannot be CHAR_READ since
                ! that exception has already been caught above
                !
                table%values(irec,ipar) = rfield(i)
                !
             endif
             !
             ipar = ipar + 1
             !
             ! wrap to next row as needed
             !
             if (ipar > table%nparameters) then
                ipar = ipar0
                irec = irec + 1
             endif
          enddo &
& label_token
       endif
       !
       ! read new line
       !
       iline = iline+1
       read (lunbcm, '(A)', iostat = istat) line
       if (istat<0) feof = .true.
       !
       ntoken = 0
       call scannr(   line,       1,        len(line),  ntoken,   itype, &
                 &  ifield,  rfield,  cfield,  lenchr,  MAXFLD,  .true., &
                 & .false., .false.)
       if (ntoken<0) then
          error = .true.
          goto 150
       endif
       !
       !----------------------------------------------
       !  do i = 1, ntoken
       !     if (itype(i) == INT_READ) then
       !        write(*,*) '>',ifield(i),'<'
       !     elseif (itype(i) == REAL_READ) then
       !        write(*,*) '>',rfield(i),'<'
       !     elseif (itype(i) == CHAR_READ) then
       !        write(*,*) '>',cfield(i),'<'
       !     endif
       !  enddo
       !----------------------------------------------
       !
       if (itype(1)==CHAR_READ) then
          isdata = .false.
       endif
    enddo
    !
    ! Finish reading table (also if we have reached the end of the file !!)
    !
    if (iread_phase == 2) then
       !
       ! Check for consistency of number of values in table versus
       ! specified number of parameters and records
       !
       ! We don't have to check whether table%nparameters == 0
       ! because this check is effectively carried out at the end of the
       ! keyword loop resulting in the error message "Data encountered
       ! before parameters definition"
       !
       nrecords = nvalues/table%nparameters
       if (mod(nvalues,table%nparameters) /= 0) then
          write(stri1,'(I7)') nvalues
          write(stri2,'(I7)') table%nparameters
          errorstring = 'Number of values (' // trim(adjustl(stri1)) // &
              & ') in table is not multiple of number of parameters (' // &
              & trim(adjustl(stri2)) // ')'
          error = .true.
          goto 150
       elseif (table%nrecords == -999) then
          table%nrecords = nrecords
       elseif (nrecords /= table%nrecords) then
          write(stri1,'(I7)') nrecords
          write(stri2,'(I7)') table%nrecords
          errorstring = 'Actual number of records (' // trim(adjustl(stri1)) // &
              & ') in table does not match specified number (' // &
              & trim(adjustl(stri2)) // ')'
          error = .true.
          goto 150
       endif
    endif
    !
150 continue
end subroutine org_readtable_data
!
!
!===============================================================================
function org_readtable_isComment(aString) result(isComment)
   !
   ! result
   logical :: isComment
   !
   ! parameters
   character(*) :: aString
   !
   ! body
   if (       aString(1:1) == '*' &
       & .or. aString(1:1) == '#' &
       & .or. aString(1:1) == ';') then
      isComment = .true.
   else
      isComment = .false.
   endif
end function org_readtable_isComment
!
!
!
end subroutine org_readtable
!
!
!===============================================================================
subroutine org_cleartable(this)
!!--description-----------------------------------------------------------------
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(tablefiletype)                    :: this
!
! Local variables
!
    integer                                :: i
    integer                                :: istat
    type(tabletype), dimension(:), pointer :: tables
!
!! executable statements -------------------------------------------------------
!
    if (associated(this%tables)) then
       !
       tables => this%tables
       !
       do i = 1, size(tables)
          if (associated(tables(i)%parameters))  deallocate(tables(i)%parameters,  STAT = istat)
          if (associated(tables(i)%values))      deallocate(tables(i)%values,      STAT = istat)
       enddo
       deallocate(this%tables,  STAT = istat)
    endif
end subroutine org_cleartable
!
!
!===============================================================================
subroutine org_gettabletimes(this       ,itable     ,times      ,refjulday  , &
                       & errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get all times from the specified table
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer                ,intent(in)  :: itable
    integer                ,intent(in)  :: refjulday
    real(fp), dimension(*) ,intent(out) :: times
    type(tablefiletype)    ,intent(in)  :: this
    character(256)         ,intent(out) :: errorstring
!
! Local variables
!
    real(hp)                 :: datediff
    !
    integer                  :: nrec
    type(tabletype), pointer :: table
!
!! executable statements -------------------------------------------------------
!
    table => this%tables(itable)
    errorstring = ' '
    !
    select case(trim(table%timefunction))
    case ('non-equidistant','equidistant','constant')
       !
       ! note: equidistant and constant tables have been
       ! extended with additional time column in org_readtable
       !
       ! times are stored in days and returned in hours
       !
       datediff = real(refjulday-table%refdate,hp)
       do nrec = 1,table%nrecords
          times(nrec) = real(table%times(nrec)-datediff,fp) * 24.0_fp
       enddo
    case default
       errorstring = 'Time-function '''// &
          & trim(table%timefunction)//''' not yet implemented,'// &
          & ' please contact code supplier'
       return
    endselect
    !
end subroutine org_gettabletimes
!
!
!===============================================================================
subroutine org_gettabledata_vector(this       ,ivec       ,values     , &
                             & timhr      ,refjulday  ,errorstring,extrapol_in)
!!--description-----------------------------------------------------------------
!
!    Function: Get data from table for specified time
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer, dimension(4)               :: ivec
    integer                ,intent(in)  :: refjulday
    real(fp), optional     ,intent(in)  :: extrapol_in
    real(fp)               ,intent(in)  :: timhr
    real(fp), dimension(:) ,intent(out) :: values
    type(tablefiletype)    ,intent(in)  :: this
    character(256)         ,intent(out) :: errorstring
!
! Local variables
!
    real(fp)                  :: extrapol
!
!! executable statements -------------------------------------------------------
!
    if (present(extrapol_in)) then
       extrapol = extrapol_in
    else
       extrapol = 0.0_fp
    endif
    call org_gettabledata_scalar(this       ,ivec(1)    ,ivec(2)    , &
               & ivec(3)    ,ivec(4)    ,values     ,timhr      , &
               & refjulday  ,errorstring,extrapol)
end subroutine org_gettabledata_vector
!
!
!===============================================================================
subroutine org_gettabledata_scalar(this       ,itable     ,ipar       , &
                 & npar       ,irec       ,values     ,timhr      , &
                 & refjulday  ,errorstring,extrapol_in)
!!--description-----------------------------------------------------------------
!
!    Function: Get data from table for specified time
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer                ,intent(in)  :: itable
    integer                ,intent(in)  :: ipar
    integer                             :: irec
    integer                ,intent(in)  :: npar
    integer                ,intent(in)  :: refjulday
    real(fp), optional     ,intent(in)  :: extrapol_in
    real(fp)               ,intent(in)  :: timhr
    real(fp), dimension(:) ,intent(out) :: values
    type(tablefiletype)    ,intent(in)  :: this
    character(256)         ,intent(out) :: errorstring
!
! Local variables
!
    real(hp)                 :: timreq
    !
    real(fp)                 :: alpha
    real(fp)                 :: extrapol
    !
    integer                  :: i
    integer                  :: j
    integer                  :: datediff
    !
    logical                  :: blockfunction
    logical                  :: inrange
    !
    integer, pointer         :: nrec
    type(tabletype), pointer :: table
!
!! executable statements -------------------------------------------------------
!
    table => this%tables(itable)
    errorstring = ' '
    !
    select case(trim(table%timefunction))
    case ('non-equidistant','equidistant','constant')
       !
       ! Search for times in table (note equidistant and
       ! constant tables have been extended with additional
       ! time column in org_readtable)
       !
       timreq = real(timhr,hp) / 24.0_fp
       if (table%timeunitstr == 'date') then
          timreq = timreq + real(refjulday,hp)
       else
          datediff = refjulday - table%refdate
          timreq   = timreq + real(datediff,hp) - table%reftime
       endif
       nrec => table%nrecords
       !
       inrange = .true.
       !
       ! Check whether the data is in the appropriate range
       ! Use comparereal to make sure that in single precision, small differences are allowed
       !
       if (comparereal(real(timreq,fp), real(table%times(1),fp)) == -1) then
          !
          ! requested time before the first time in the table
          !
          select case(trim(table%extrapolation))
          case ('periodic')
             errorstring = 'Periodic boundary conditions not '// &
                & 'yet implemented, please contact code supplier'
             return
          case ('constant')
             irec  = 1
             do i = 1, npar
                j = ipar + i - 1
                values(i) = table%values(irec,j)
             enddo
             inrange = .false.
          case default
             errorstring = 'Requested time lies before available '// &
                & 'times for '//trim(table%parameters(ipar)%name(1:20))// &
                & ' at '//trim(table%location)
             return
          endselect
       elseif (comparereal(real(timreq,fp), real(table%times(nrec),fp)) == 1) then
          !
          ! requested time beyond the last time in the table
          !
          blockfunction = .true.
          do i = 1, npar
             j = ipar + i - 1
             if (table%parameters(j)%interpolation /= 'block') then
                blockfunction = .false.
                exit
             endif
          enddo
          !
          if (blockfunction) then
             irec  = nrec
             do i = 1, npar
                j = ipar + i - 1
                values(i) = table%values(irec,j)
             enddo
             inrange = .false.
             !
          else
             select case(trim(table%extrapolation) )
             case ('periodic')
                errorstring = 'Periodic boundary conditions not '// &
                   & 'yet implemented, please contact code supplier'
                return
             case ('constant')
                irec  = nrec
                do i = 1, npar
                   j = ipar + i - 1
                   values(i) = table%values(irec,j)
                enddo
                inrange = .false.
             case default
                if (present(extrapol_in)) then
                   !
                   ! extrapol_in: interval behind table%times(nrec) where extrapolation is used [hr]
                   !
                   extrapol = extrapol_in / 24.0_fp
                else
                   extrapol = 0.0_fp
                endif
                if (comparereal(real(timreq,fp), real(table%times(nrec),fp)+extrapol) /= 1) then
                   irec  = nrec
                   do i = 1, npar
                      j = ipar + i - 1
                      values(i) = table%values(irec,j)
                   enddo
                   inrange = .false.
                else
                   errorstring = 'Requested time lies beyond available '// &
                      & 'times for '//trim(table%parameters(ipar)%name(1:20))// &
                      & ' at '//trim(table%location)
                   return
                endif
             endselect
             !
          endif
       endif
       !
       if (inrange) then
          !
          ! Reset search index
          ! Use comparereal to make sure that in single precision, small differences are allowed
          !
          if (comparereal(real(timreq,fp), real(table%times(irec),fp)) == -1) irec = 1
          !
          ! search time-interval ...
          !
          do while (irec < nrec)
             !
             ! Use comparereal to make sure that in single precision, small differences are allowed
             !
             if (comparereal(real(timreq,fp), real(table%times(irec+1),fp)) == 0) then
                 alpha = 0.0_fp
                 exit
             elseif (comparereal(real(timreq,fp), real(table%times(irec+1),fp)) == -1) then
                 if (table%times(irec+1) > table%times(irec)) then
                    alpha = ( table%times(irec+1) - timreq ) / ( table%times(irec+1)-table%times(irec) )
                    exit
                 else
                    errorstring = 'Times are not increasing at '//trim(table%location)
                    return
                 endif
             endif
             irec = irec + 1
          enddo
          !
          ! time found
          !
          do i = 1, npar
             j = ipar + i - 1
             if (trim(table%parameters(j)%interpolation) == 'block') then
                !
                ! block interpolation
                !
                values(i) = table%values(irec,j)
             else
                !
                ! linear interpolation
                !
                values(i) = table%values(irec,j) * alpha + &
                          & table%values(irec+1,j) * (1.0_fp - alpha)
             endif
          enddo
       endif
    case default
       errorstring = 'Time-function '''// &
          & trim(table%timefunction)//''' not yet implemented,'// &
          & ' please contact code supplier'
       return
    endselect
    !
end subroutine org_gettabledata_scalar
!
!
!===============================================================================
subroutine org_gettable_vector(this      ,location  ,parname   ,ivec      , &
                         & nparmin   ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Find table for specified location and quantity
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer, dimension(4)  ,intent(out) :: ivec
    integer                ,intent(in)  :: nparmin
    character(*)           ,intent(in)  :: location
    character(*)           ,intent(in)  :: parname
    type(tablefiletype)    ,intent(in)  :: this
    character(256)         ,intent(out) :: errorstring
!
!! executable statements -------------------------------------------------------
!
    call org_gettable(this      ,location  ,parname   ,ivec(1)   , &
                & ivec(2)   ,ivec(3)   ,nparmin   ,errorstring)
    ivec(4) = 1
end subroutine org_gettable_vector
!
!
!===============================================================================
subroutine org_gettable_scalar(this      ,location  ,parname   ,itable    , &
                         & ipar      ,npar      ,nparmin   ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Find table for specified location and quantity
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer                ,intent(out) :: itable
    integer                ,intent(out) :: ipar
    integer                ,intent(out) :: npar
    integer                ,intent(in)  :: nparmin
    character(*)           ,intent(in)  :: location
    character(*)           ,intent(in)  :: parname
    type(tablefiletype)    ,intent(in)  :: this
    character(256)         ,intent(out) :: errorstring
!
! Local variables
!
    integer                                :: i
    integer                                :: j
    integer                                :: lpn
    !
    type(tabletype), dimension(:), pointer :: tables
!
!! executable statements -------------------------------------------------------
!
    tables => this%tables
    errorstring = ' '
    itable = -999
    !
    lpn = min(20,len(parname))
    !
loop_tables: do i = 1, size(tables)
       if (trim(tables(i)%location) == location) then
          !
          do j = 1, tables(i)%nparameters
             if (tables(i)%parameters(j)%name(1:lpn) == parname(1:lpn)) then
                itable = i
                ipar = j
                exit loop_tables
             endif
          enddo
       endif
    enddo &
& loop_tables
    !
    if (itable < 0) then
       npar = 0
       if (npar < nparmin) then
          errorstring = 'Missing ''' // trim(parname) // &
             & ''' data for location ''' // trim(location) // &
             & ''' in ' // trim(this%filename)
          return
       else
          return
       endif
    endif
    !
    j = ipar
    do while (trim(tables(itable)%parameters(j)%name(1:lpn)) == parname(1:lpn))
       j = j + 1
       if (j > tables(itable)%nparameters) exit
    enddo
    npar = j - ipar
    !
end subroutine org_gettable_scalar
!
!
!===============================================================================
subroutine org_checktable(this      ,itable    ,ipar      , &
                    & npar      ,chktyp    ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Check whether all values in table are
!              * positive              if CHKTAB_POSITIVE
!              * logical               if CHKTAB_LOGICAL
!              * block-wise specified  if CHKTAB_BLOCK
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer                      ,intent(in)  :: itable
    integer                      ,intent(in)  :: ipar
    integer                      ,intent(in)  :: npar
    integer                      ,intent(in)  :: chktyp
    type(tablefiletype)          ,intent(in)  :: this
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
    integer                                :: i
    integer                                :: j
    !
    type(tabletype), dimension(:), pointer :: tables
!
!! executable statements -------------------------------------------------------
!
    tables => this%tables
    errorstring = ' '
    !
    do j = ipar, ipar + npar - 1
       if ((iand(chktyp,CHKTAB_LOGICAL)==1 .or. &
         &  iand(chktyp,CHKTAB_BLOCK)==1).and. &
         & trim(tables(itable)%parameters(j)%interpolation) /= 'block') then
          errorstring = 'Interpolation method should be "block" for ''' // &
             & trim(tables(itable)%parameters(j)%name) // ''' at location ''' // &
             & trim(tables(itable)%location) // ''' in ' // &
             & trim(this%filename)
          return
       endif
       do i = 1, tables(itable)%nrecords
          if (iand(chktyp,CHKTAB_POSITIVE)==1 .and. &
             & tables(itable)%values(i,j) < 0.0) then
             errorstring = 'Negative value encountered for ''' // &
                & trim(tables(itable)%parameters(j)%name) // ''' at location ''' // &
                & trim(tables(itable)%location) // ''' in ' // &
                & trim(this%filename)
             return
          endif
          if (iand(chktyp,CHKTAB_LOGICAL)==1 .and. &
            & comparereal(tables(itable)%values(i,j),0.0_fp) /= 0 .and. &
            & comparereal(tables(itable)%values(i,j),1.0_fp) /= 0) then
             errorstring = 'Not all values are 0 or 1 for ''' // &
                & trim(tables(itable)%parameters(j)%name) // ''' at location ''' // &
                & trim(tables(itable)%location) // ''' in ' // &
                & trim(this%filename)
             return
          endif
       enddo
    enddo
    !
end subroutine org_checktable
!
!
!===============================================================================
subroutine org_checktableparnames(this      ,parnames  ,itable    , &
                            & ipar      ,npar      ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Check parameter names in table
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer                      ,intent(in)  :: itable
    integer                      ,intent(in)  :: ipar
    integer                      ,intent(in)  :: npar
    character(*), dimension(npar),intent(in)  :: parnames
    type(tablefiletype)          ,intent(in)  :: this
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
    integer                                :: i
    integer                                :: j
    !
    type(tabletype), dimension(:), pointer :: tables
!
!! executable statements -------------------------------------------------------
!
    tables => this%tables
    errorstring = ' '
    !
    i = 0
    do j = ipar, ipar + npar - 1
       i = i + 1
       if (trim(tables(itable)%parameters(j)%name) /= parnames(i)) then
          errorstring = 'Expected ''' // trim(parnames(i)) // &
             & ''' but found ''' // trim(tables(itable)%parameters(j)%name) // &
             & ''' for location ''' // trim(tables(itable)%location) // &
             & ''' in ' // trim(this%filename)
          return
       endif
    enddo
    !
end subroutine org_checktableparnames
!
!
!===============================================================================
character(256) function org_getfilename(this    )
!!--description-----------------------------------------------------------------
!
!    Function: Get the name of the file
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(tablefiletype)          ,intent(in)  :: this
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    org_getfilename = this%filename
end function org_getfilename
!
!
!===============================================================================
integer function org_getntables(this    ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get the number of tables
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(tablefiletype)          ,intent(in)  :: this
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    if (associated(this%tables)) then
       errorstring = ' '
       org_getntables = size(this%tables)
    else
       errorstring = 'Tables not yet initialised'
       org_getntables = 0
    endif
end function org_getntables
!
!
!===============================================================================
character(MAXTABLECLENGTH) function org_gettablelocation(this    ,itable     ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get location name of table
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(tablefiletype)          ,intent(in)  :: this
    integer                      ,intent(in)  :: itable
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    if (associated(this%tables)) then
       if (itable<=size(this%tables)) then
          errorstring = ' '
          org_gettablelocation = this%tables(itable)%location
       else
          errorstring = 'Table index is too large'
          org_gettablelocation = ' '
       endif
    else
       errorstring = 'Tables not yet initialised'
       org_gettablelocation = ' '
    endif
end function org_gettablelocation
!
!
!===============================================================================
integer function org_gettablentimes(this    ,itable     ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get the number of times in table
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(tablefiletype)          ,intent(in)  :: this
    integer                      ,intent(in)  :: itable
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    if (associated(this%tables)) then
       if (itable<=size(this%tables)) then
          errorstring = ' '
          org_gettablentimes = this%tables(itable)%nrecords
       else
          errorstring = 'Table index is too large'
          org_gettablentimes = 0
       endif
    else
       errorstring = 'Tables not yet initialised'
       org_gettablentimes = 0
    endif
end function org_gettablentimes

end module tables
