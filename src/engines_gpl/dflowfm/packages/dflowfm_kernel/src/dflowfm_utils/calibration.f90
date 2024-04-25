!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> This module reads and handles the computation of calibration factors.
!!
!! It consists of 
!!    - a data structure 
!!    - a reading routine 
!!    - a time-dependent updating routine 

module m_calibration 
implicit none

public cldtype
public clddata
public read_cldfile
public read_cllfile
public clr_clddata
public update_clddata
public calibration_backup_frcu

! parameters
integer, parameter, public :: CL_UNDEFINED        = -99999     
integer, parameter, public :: CL_NOT_IN_SUBDOMAIN = -77777
integer, parameter, public :: CL_WATERLEVEL_TYPE  = 1
integer, parameter, public :: CL_DISCHARGE_TYPE   = 2
integer, parameter, public :: CLD_MAXFLD = 4
integer, parameter, public :: CLD_MAXCHR = 80
integer, parameter, public :: CLL_MAXFLD = 5
integer, parameter, public :: CLD_MAXDEF = 10000

integer, parameter, public :: GET_DIMENSIONS      = 101
integer, parameter, public :: FILL_DATA           = 102

! dimensions 
integer :: ncld          !> Total number of calibration definitions
integer :: ncldnrm       !> Total number of calibration definitions non-h, non-q
integer :: ncldcrs       !> Total number cross sections referred to in discharge dependent calibration definitions
integer :: ncldobs       !> Total number observation stations referred to in discharge dependent calibration definitions
integer :: n_q           !> Total number of rows in discharge tables
integer :: n_zs          !> Total number of rows in waterlevel tables
integer :: nclpa = 1     !> Total number of calibration parameters 
integer :: ncll          !> Total number of calibration area definitions

type cldtype              
   ! definitions 
   double precision, dimension(:), allocatable       :: cldtable_q    !> Calibration defintion q values 
   double precision, dimension(:), allocatable       :: cldtable_zs   !> Calibration defintion zs values 
   double precision, dimension(:), allocatable       :: rttdef        !> Calibration defintion real values (non-h, non-q, and non-interpolated h and q) 
   integer         , dimension(:), allocatable       :: ittdef        !> Calibration defintion integer values (i.e. calibration definition codes) 
   integer         , dimension(:), allocatable       :: def2icld      !> Calibration defintion number to index in calibration defintion table 

   integer         , dimension(:), allocatable       :: ittdef_q      !> Calibration defintion integer values associated with q values (i.e. calibration definition codes) 
   integer         , dimension(:), allocatable       :: start_q       !> Start row indices for q table 
   integer         , dimension(:), allocatable       :: end_q         !> End row indices for q table 
   double precision, dimension(:), allocatable       :: rttdef_q      !> Calibration defintion real values associated with q values 
   double precision, dimension(:), allocatable       :: slope_q       !> Slope for table associated with q values 
   double precision, dimension(:), allocatable       :: cross_q       !> Slope for table associated with q values 
   integer         , dimension(:), allocatable       :: icld_q        !> Get icld index for general definition table

   integer         , dimension(:), allocatable       :: ittdef_zs     !> Calibration defintion integer values associated with zs values (i.e. calibration definition codes) 
   integer         , dimension(:), allocatable       :: start_zs      !> Start row indices for zs table 
   integer         , dimension(:), allocatable       :: end_zs        !> End row indices for zs table 
   double precision, dimension(:), allocatable       :: rttdef_zs     !> Calibration defintion real values associated with zs values 
   double precision, dimension(:), allocatable       :: slope_zs      !> Slope for table associated with zs values 
   double precision, dimension(:), allocatable       :: cross_zs      !> Slope for table associated with zs values 
   integer         , dimension(:), allocatable       :: icld_zs       !> Get icld index for general definition table
   
   integer         , dimension(:), allocatable       :: crs           !> Calibration defintion related cross-section id
   integer         , dimension(:), allocatable       :: obs           !> Calibration defintion related observation station id

   ! areas
   integer         , dimension(:,:), allocatable     :: ittar         !> Calibration area integer values (i.e. link, calibration definition codes) 
   double precision, dimension(:), allocatable       :: rttar         !> Calibration area real values (i.e. area percentage)

   ! error catching 
   double precision, dimension(:), allocatable       :: sumar         !> Sum of calibration areas (should be <= 1)  TO DO: check if we want this way, or more like trachytope approach with blocks?.
   integer         , dimension(:), allocatable       :: linar         !> Line number in file for error message  

   
end type cldtype

type(cldtype)  :: clddata   !> Structure holding calibration definition 
    
contains 

subroutine read_cldfile(md_cldfile, clddata, phase)  
 use unstruc_messages
 use m_missing,       only : dmiss, intmiss
 use unstruc_files,   only : mdia
 use system_utils,    only : exifil
 use m_monitoring_crosssections, only: crs, ncrs
 use m_observations,  only: namobs, numobs
 
    character(len=*),         intent(in)    :: md_cldfile
    integer,                  intent(in)    :: phase
    type(cldtype),            intent(inout) :: clddata
    
!
! Local variables
!
    integer                          :: icld    = 0
    integer                          :: icldnrm = 0
    integer                          :: icldcrs = 0
    integer                          :: icldobs = 0
    integer                          :: i_q     = 0
    integer                          :: i_zs    = 0    
    integer                          :: ibeg
    integer                          :: icrs
    integer                          :: iend
    integer, dimension(CLD_MAXFLD)    :: ifield
    integer, dimension(CLD_MAXFLD)    :: itype
    integer, dimension(CLD_MAXFLD)    :: lenchr
    integer                          :: iobs
    integer                          :: iocond
    integer                          :: istat = 0
    integer                          :: istr
    integer                          :: lundia 
    integer                          :: luntmp
    integer                          :: nrflds
    integer                          :: prev_cld_no    = CL_UNDEFINED
    integer                          :: prev_cld_type  = CL_UNDEFINED
    character(CLD_MAXCHR), dimension(CLD_MAXFLD) :: cfield
    character(len=255)               :: filtmp
    character(len=132)               :: rec132
    logical                          :: error 
!    logical                          :: cld_in_arl = .true.
    double precision, dimension(CLD_MAXFLD)      :: rfield

!
!! executable statements -------------------------------------------------------
!
    error = .false.
!   
    if (phase == GET_DIMENSIONS) then 
        call mess(LEVEL_INFO,' ') 
        call mess(LEVEL_INFO,'*** Start of calibration definition input: ' // trim(md_cldfile))
    endif 
    filtmp = md_cldfile
    lundia = mdia 
    icld    = 0
    icldnrm = 0
    icldcrs = 0
    icldobs = 0
    i_q     = 0
    i_zs    = 0
    if (phase == FILL_DATA) then 
        if (istat==0) allocate(clddata%ittdef(ncld)                , stat = istat)
        if (istat==0) allocate(clddata%rttdef(ncld)                , stat = istat)
        if (istat==0) allocate(clddata%def2icld(CLD_MAXDEF)        , stat = istat)
        if (istat==0) allocate(clddata%crs(ncldcrs)                , stat = istat)
        if (istat==0) allocate(clddata%obs(ncldobs)                , stat = istat)
        if (istat==0) allocate(clddata%cldtable_q(n_q)             , stat = istat)
        if (istat==0) allocate(clddata%cldtable_zs(n_zs)           , stat = istat)
        if (istat==0) allocate(clddata%rttdef_zs(n_zs)             , stat = istat)
        if (istat==0) allocate(clddata%start_zs(ncldobs)           , stat = istat)
        if (istat==0) allocate(clddata%end_zs(ncldobs)             , stat = istat)
        if (istat==0) allocate(clddata%slope_zs(n_zs)              , stat = istat)
        if (istat==0) allocate(clddata%cross_zs(n_zs)              , stat = istat)
        if (istat==0) allocate(clddata%ittdef_zs(n_zs)             , stat = istat)
        if (istat==0) allocate(clddata%icld_zs(ncldobs)            , stat = istat)
        if (istat==0) allocate(clddata%rttdef_q(n_q)               , stat = istat)
        if (istat==0) allocate(clddata%slope_q(n_q)                , stat = istat)
        if (istat==0) allocate(clddata%cross_q(n_q)                , stat = istat)
        if (istat==0) allocate(clddata%start_q(ncldcrs)            , stat = istat)
        if (istat==0) allocate(clddata%end_q(ncldcrs)              , stat = istat)
        if (istat==0) allocate(clddata%ittdef_q(n_q)               , stat = istat)
        if (istat==0) allocate(clddata%icld_q(ncldcrs)             , stat = istat)
        if (istat/=0) then
           errmsg = 'read_cldfile: memory alloc error'
           call mess(LEVEL_ERROR, errmsg)
           error = .true.
           goto 9999      
        endif
        clddata%def2icld = intmiss
        !
    elseif (phase == GET_DIMENSIONS) then 
        ncld    = 0
        ncldnrm = 0
        ncldcrs = 0
        ncldobs = 0
        n_q     = 0
        n_zs    = 0
    endif
    !
    ! Initialize array dimensions for default empty settings
    !
    !
    ! keyword not found ?
    !
    if (filtmp == ' ') then
       errmsg = 'Missing value (or the keyword):  [calibration]DefinitionFile in .mdu file'
       call mess(LEVEL_ERROR, errmsg)
       error = .true.
       goto 9999
    endif
    !
    ! test file existence
    !
    if (.not.exifil(filtmp, lundia)) then
       !
       ! file does not exist !!
       !
       errmsg = 'The specified file ' // trim(filtmp) // ' does not exist '
       call mess(LEVEL_ERROR, errmsg)
       error = .true.
       goto 9999
    endif
    !
    ! open trachytope definition file
    !
    open (newunit=luntmp, file = trim(filtmp), form = 'formatted', iostat = iocond,  &
        & status = 'old')
    if (iocond/=0) then
       errmsg = 'Error while opening file '// trim(filtmp)
       call mess(LEVEL_ERROR, errmsg)
       error = .true.
       goto 9999
    endif
    !
    !
    ! freeformatted file
    !           read record and add 1 to NCLD till end of file
    !
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
       errmsg = 'Read error from file: '// trim(filtmp)
       call mess(LEVEL_ERROR, errmsg)
       error = .true.
       ! <--
       !
       ! close file
       !
  199 continue
       
       if (phase == FILL_DATA) then 
           call mess(LEVEL_INFO,'    Number of waterlevel dependent definitions = ', ncldobs )
           call mess(LEVEL_INFO,'    Number of discharge dependent definitions = ', ncldcrs )
           call mess(LEVEL_INFO,'    Number of other definitions               = ', ncldnrm )
           call mess(LEVEL_INFO,'    ____________________________________________________________')
           call mess(LEVEL_INFO,'    Total number of definitions               = ', ncld )
           call mess(LEVEL_INFO,'*** Succesfully read calibration definition input ')
       endif 
    
       close (luntmp)
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
    ! Comment at end of line 
    !
    ! loop over rec132 and find #
    !
    comment_at_end_of_line_loop: &
        do istr = ibeg, iend
            if (rec132(istr:istr) == '#') then 
                iend = istr
                exit comment_at_end_of_line_loop
            end if 
        end do &
    comment_at_end_of_line_loop        
        call scannr(rec132    ,ibeg      ,iend      ,nrflds    ,itype     , &
              & ifield    ,rfield    ,cfield    ,lenchr    ,CLD_MAXFLD    , &
              & .true.    ,.true.    ,.true.    )
    !
    ! When sub-fields are found, reserve space
    !
    if (nrflds>0) then
        if (itype(1)==1 .and. itype(2)/=3 .and. ifield(1) /= prev_cld_no) then
            icld    = icld + 1
            icldnrm = icldnrm + 1
            prev_cld_no   = CL_UNDEFINED
            prev_cld_type = CL_UNDEFINED
            if (phase == FILL_DATA) then 
                clddata%ittdef(icld) = ifield(1)
                clddata%def2icld(ifield(1)) = icld
                clddata%rttdef(icld) = rfield(2)
            endif
        elseif (itype(1)==1 .and. itype(2)==3) then   
            if (cfield(2)(1:9) == 'DISCHARGE') then       
                icldcrs = icldcrs + 1
                icld    = icld + 1
                prev_cld_no   = ifield(1)
                prev_cld_type = CL_DISCHARGE_TYPE
                if (phase == FILL_DATA) then 
                    ! fill data 
                    clddata%ittdef(icld) = ifield(1)
                    clddata%def2icld(ifield(1)) = icld
                    clddata%rttdef(icld) = dmiss   ! can only get a value after update step 
                    ! link to cross-section 
                    clddata%crs(icldcrs) = intmiss
                    do icrs = 1,ncrs
                        if (trim(cfield(3)) == trim(crs(icrs)%name)) then
                           clddata%crs(icldcrs) = icrs
                        end if
                    end do
                    if (clddata%crs(icldcrs) == intmiss) then 
                        write(errmsg,'(a, i0, a, a, a)') 'Error reading calibration definition ', clddata%ittdef(icld), ': Cross-section "',trim(cfield(3)),'" not found in model.'
                        call mess(LEVEL_ERROR, errmsg)
                    end if
                    clddata%start_q(icldcrs) = i_q + 1
                    clddata%icld_q(icldcrs) = icld

                endif
            elseif (cfield(2)(1:10) == 'WATERLEVEL') then 
                icldobs = icldobs + 1
                icld    = icld + 1
                prev_cld_no = ifield(1)
                prev_cld_type = CL_WATERLEVEL_TYPE
                if (phase == FILL_DATA) then 
                    clddata%ittdef(icld) = ifield(1)
                    clddata%def2icld(ifield(1)) = icld
                    clddata%rttdef(icld) = dmiss ! can only get a value after update step 

                    ! link to observation station  
                    clddata%obs(icldobs) = intmiss
                    do iobs = 1,numobs
                        if (trim(cfield(3)) == trim(namobs(iobs))) then
                            clddata%obs(icldobs) = iobs
                        end if
                    end do
                    if (clddata%obs(icldobs) == intmiss) then 
                        write(errmsg,'(a, i0, a, a, a)') 'Error reading calibration definition ', clddata%ittdef(icld), ': Observation station "',trim(cfield(3)),'" not found in model.'
                        call mess(LEVEL_ERROR, errmsg)
                    end if
                    clddata%start_zs(icldobs) = i_zs + 1
                    clddata%icld_zs(icldobs) = icld
                endif
            else
                ! do nothing 
            endif
        elseif (itype(1)==1 .and. itype(2)/=3) then
            if (ifield(1) == prev_cld_no) then 
                ! add to table for discharge/water level depdendent data
                if (prev_cld_type == CL_WATERLEVEL_TYPE) then
                    ! count values to table for waterlevel dependent trachytopes
                    i_zs = i_zs + 1
                    if (phase == FILL_DATA) then 
                        clddata%ittdef_zs(i_zs)   = ifield(1)
                        clddata%cldtable_zs(i_zs) = rfield(2)
                        clddata%rttdef_zs(i_zs)   = rfield(3)
                        clddata%end_zs(icldobs)   = i_zs
                        if (i_zs > clddata%start_zs(icldobs)) then
                            if ( .not. (clddata%cldtable_zs(i_zs) > clddata%cldtable_zs(i_zs - 1)) ) then 
                                errmsg = 'Water level dependent calibration definition not monotonically increasing on line: '// rec132
                                call mess(LEVEL_ERROR, errmsg)
                            endif     
                            ! compute slope
                            clddata%slope_zs(i_zs-1) = (clddata%rttdef_zs(i_zs)-clddata%rttdef_zs(i_zs-1)) / (clddata%cldtable_zs(i_zs) - clddata%cldtable_zs(i_zs - 1))
                            ! compute crossing 
                            clddata%cross_zs(i_zs-1) = clddata%rttdef_zs(i_zs-1) - clddata%slope_zs(i_zs-1) * clddata%cldtable_zs(i_zs - 1)
                        endif 
                    endif
                elseif (prev_cld_type == CL_DISCHARGE_TYPE) then
                    ! count values to table for discharge dependent trachytopes
                    i_q = i_q + 1
                    if (phase == FILL_DATA) then 
                        clddata%ittdef_q(i_q)   = ifield(1)
                        clddata%cldtable_q(i_q) = rfield(2)
                        clddata%rttdef_q(i_q)   = rfield(3)
                        clddata%end_q(icldcrs)  = i_q
                        if (i_q > clddata%start_q(icldcrs)) then 
                            if ( .not. (clddata%cldtable_q(i_q) > clddata%cldtable_q(i_q - 1)) ) then 
                                 errmsg = 'Discharge dependent calibration definition not monotonically increasing on line: '// rec132
                                 call mess(LEVEL_ERROR, errmsg)
                            endif      
                            ! compute slope
                            clddata%slope_q(i_q-1) = (clddata%rttdef_q(i_q)-clddata%rttdef_q(i_q-1)) / (clddata%cldtable_q(i_q) - clddata%cldtable_q(i_q - 1))
                            ! compute crossing 
                            clddata%cross_q(i_q-1) = clddata%rttdef_q(i_q-1) - clddata%slope_q(i_q-1) * clddata%cldtable_q(i_q - 1)
                        endif 
                    endif
                else 
                   errmsg = 'Unknown read error #1 in rd_cldfile() on line: '// rec132
                   call mess(LEVEL_ERROR, errmsg)
                   error = .true.
                   goto 9999
                endif
            else
                errmsg = 'Unknown read error #2 in rd_cldfile() on line: '// rec132
                call mess(LEVEL_ERROR, errmsg)
                error = .true.   
                goto 9999
            end if                
        endif
    elseif (nrflds .eq. -1) then 
        errmsg = 'One ore more parameters wrong in rd_cldfile() on line: '// rec132
        call mess(LEVEL_ERROR, errmsg)
        error = .true.
        goto 9999
    elseif (nrflds .eq. -2) then 
        write (errmsg, '(A,i4,A,A)'), 'More than ', CLD_MAXFLD, ' fields in rd_cldfile() on line: ', rec132
        call mess(LEVEL_ERROR, errmsg)
        error = .true.
        goto 9999
    elseif (nrflds .eq. -3) then 
        write (errmsg, '(A,i4,A,A)'), 'Character string longer than ', CLD_MAXCHR, ' characters in rd_cldfile() on line: ', rec132
        call mess(LEVEL_ERROR, errmsg)
        error = .true.
        goto 9999
    elseif (nrflds .eq. -4) then 
        errmsg = 'Unmatching quotes in rd_cldfile() on line: '// rec132
        call mess(LEVEL_ERROR, errmsg)
        error = .true.
        goto 9999
    endif
    goto 110

    
9999 continue

    if (phase == GET_DIMENSIONS) then 
        ncld    = icld
        ncldnrm = icldnrm
        ncldcrs = icldcrs
        ncldobs = icldobs
        n_q     = i_q
        n_zs    = i_zs        
    endif
    
end subroutine read_cldfile


subroutine read_cllfile(md_cllfile, clddata, phase)  
 use unstruc_messages
 use m_missing,       only : dmiss, intmiss
 use unstruc_files,   only : mdia
 use system_utils,    only : exifil
 use m_monitoring_crosssections, only : crs, ncrs
 use m_observations,  only : namobs, numobs
 use network_data,    only : lnn, numl 
 use kdtree2Factory
 use m_sferic,        only: jsferic
 use geometry_module, only: dbdistance

   character(len=*),         intent(in)    :: md_cllfile
   integer,                  intent(in)    :: phase
   type(cldtype),            intent(inout) :: clddata    
   
   integer                          :: lundia 
   logical                          :: error 
   character(len=256)               :: filnam
!
! Local variables
!
!    integer                          :: i
    integer                          :: ibeg
    integer                          :: icld
    integer                          :: icll
    integer                          :: icll_found
!    integer                          :: icurec
    integer                          :: iend
    integer                          :: iocond
    integer                          :: istat
    integer                          :: jcll
    integer                          :: L
!    integer                          :: lcurec
!    integer                          :: lfile
    integer                          :: luntmp
    integer                          :: mcurec
    integer                          :: nrflds
!    integer, dimension(4)            :: nmpblk
    integer, dimension(CLL_MAXFLD)   :: ifield
    integer, dimension(CLL_MAXFLD)   :: itype
    integer, dimension(CLL_MAXFLD)   :: lenchr
!    logical                          :: leql
!    logical                          :: lfirst
    logical                          :: lokay
!    logical                          :: lprblk
    double precision, dimension(CLL_MAXFLD)      :: rfield
    character(30), dimension(CLL_MAXFLD) :: cfield
    character(132)                   :: rec132
    character(10)                    :: ltmp 
    
    double precision                 :: dtol_cl = 1d-4
    double precision                 :: x
    double precision                 :: y
    double precision                 :: dist

   istat = 0 
   lundia = mdia 
   filnam = md_cllfile
   error = .false. 
   
   if (phase == GET_DIMENSIONS) then 
       call mess(LEVEL_INFO,' ') 
       call mess(LEVEL_INFO,'*** Start of calibration area definition input: ' // trim(md_cllfile))
   endif    
   if (phase == FILL_DATA) then 
        if (istat==0) allocate(clddata%ittar(ncll, 2)             , stat = istat)    ! net link number, definition number 
        if (istat==0) allocate(clddata%rttar(ncll)                , stat = istat)    ! area fraction  
        if (istat==0) allocate(clddata%sumar(numl)                , stat = istat)
        if (istat==0) allocate(clddata%linar(ncll)                , stat = istat)    ! line numbers in file 
        clddata%sumar = 0.d0
        clddata%linar = intmiss
   endif
!
!! executable statements -------------------------------------------------------
!
    !
    ! test file existence
    !
    if (.not.exifil(filnam, lundia)) then
       !
       ! file does not exist !!
       !
       errmsg = 'The specified file ' // trim(filnam) // ' does not exist '
       call mess(LEVEL_ERROR, errmsg)
       error = .true.
       goto 9999
    endif
    !
    ! open file
    !
    open (newunit=luntmp, file = trim(filnam), form = 'formatted', iostat = iocond,  &
        & status = 'old')
    if (iocond/=0) then
       errmsg = 'Error while opening file '// trim(filnam)
       call mess(LEVEL_ERROR, errmsg)
       error = .true.
       goto 9999
    endif
    !
    ! freeformatted file
    !       read record and count number of useful areas
    !       till end of file
    !
    icll = 0
    icll_found = 0
    !lprblk = .false.
    mcurec = 0
    ! -->
    !
    ! read line
    !
  210 continue
    read (luntmp, '(a)', iostat = iocond) rec132
    if (iocond==0) then
       mcurec = mcurec + 1
    else
       !
       ! End-of-file ?
       !
       if (iocond<0) then
          ! <--
          !
          ! close file
          !
          close (luntmp)
          
          if (phase == FILL_DATA) then 
              call mess(LEVEL_INFO,'    Number of area definitions = ', icll )
              if (icll > icll_found) then 
                  call mess(LEVEL_INFO,'    of which not connected     = ', icll - icll_found )
              endif    
              call mess(LEVEL_INFO,'*** Succesfully read calibration area definition input')
          endif              
          
          goto 9999
       endif
       !
       ! Reading error
       !
       error = .true.
       rec132 = ' '
       write (rec132, '(i12)') mcurec + 1
       errmsg = 'Read error from file: '// trim(filnam) // ', Record: ' // trim(rec132)
       call mess(LEVEL_ERROR, errmsg)
       close (luntmp)
       goto 9999
    endif
    !
    ! Interpret line ...
    !
    !
    ! Comment line
    !
    if ((rec132(1:1)=='*') .or. (rec132(1:1)=='#')) goto 210
    !
    ! Scan the record
    !
    ibeg = 1
    iend = 132
    call scannr(rec132    ,ibeg      ,iend      ,nrflds    ,itype     , &
              & ifield    ,rfield    ,cfield    ,lenchr    ,CLL_MAXFLD    , &
              & .true.    ,.true.    ,.true.    )
    !
    ! When no sub-fields are found, record appears to be empty
    !
    if (nrflds==0) goto 210
    !
    ! Check the contents
    !
    lokay = .false.
    !
    ! Check if it is a valid net link record for unstructured input
    !
    if (nrflds==5 .and. & 
      & (itype(1)==2 .or. itype(1)==1) .and. & 
      & (itype(2)==2 .or. itype(2)==1) .and. &         
      & (itype(3)==2 .or. itype(3)==1) .and. &         
      & itype(4)==1 .and. (itype(5)==2 .or. itype(5)==1)) then
       lokay = .true.
       icll = icll + 1
    endif
    !
    !! TO DO: Update to allow observation stations for parts outside domain which are inactive
    !!itrt = trachy_fl%gen%ittdef(trachy_fl%gen%crs(itrtcrs)%itrt,1)
    !
    !!cld_in_arl =  .false.
    !!itt = 0
    !!do while ((.not. trt_in_arl) .and. (itt < trachy_fl%dir(1)%nttaru))
    !!   itt = itt + 1
    !!   if (trachy_fl%dir(1)%ittaru(itt,3) == itrt) then    ! if trachytope is included in .arl file
    !!      trt_in_arl = .true.
    !!   end if
    !!enddo
    !
    !! To do move to after reading cll ?
    !if ((clddata%obs(icldobs) == intmiss) .and. cld_in_arl) then
    !    call mess(LEVEL_ERROR, 'Error reading calibration definition file: Observation station does not exist in "'//trim(md_cldfile)//'": '//rec132)
    !    error = .true.
    !    goto 9999
    !end if               
    !
    if (phase == FILL_DATA) then 
        clddata%ittar(icll, 1) = intmiss

        x=rfield(1)
        y=rfield(2)
        ! z=rfield(3) (not used).

        ! fill query vector
        call make_queryvector_kdtree(treeglob,x,y, jsferic)

        ! find nearest link
        call kdtree2_n_nearest(treeglob%tree,treeglob%qv,1,treeglob%results)

        ! get link number
        L = treeglob%results(1)%idx

        ! check distance
        dist = treeglob%results(1)%dis
        ! dist = dbdistance(xuL(L),yuL(L),x,y)  (alternatively)

        if ( dist.lt.dtol_cl .and. lnn(L) > 0) then
            clddata%ittar(icll,1) = L  !(net link number)
            icll_found = icll_found + 1
            icld = clddata%def2icld(ifield(4))
            if (icld == intmiss) then 
                errmsg = 'Calibration definition number not defined in : '// trim(filnam) 
                write(ltmp, '(i0)') mcurec
                errmsg = trim(errmsg) // '. See line number: ' // trim(ltmp)
                errmsg = trim(errmsg) // ' Last record: ' // trim(rec132)  
                call mess(LEVEL_ERROR, errmsg)
            endif
            clddata%ittar(icll, 2) = icld
            clddata%rttar(icll) = rfield(5)
            clddata%linar(icll) = mcurec
            clddata%sumar(L) = clddata%sumar(L) + clddata%rttar(icll)
            if (clddata%sumar(L) > 1d0) then 
                ! check that sum of areas per link <= 1 
                errmsg = 'Areal sum larger than 1 in file: '// trim(filnam) 
                errmsg = trim(errmsg) // '. See line numbers: '
                do jcll = 1, icll 
                    if (clddata%ittar(jcll,1) == L) then
                        write(ltmp, '(i0)') clddata%linar(jcll)
                        errmsg = trim(errmsg) // ' ' // trim(ltmp) // ','
                    endif    
                enddo    
                errmsg = trim(errmsg) // ' Last record: ' // trim(rec132)  
                call mess(LEVEL_ERROR, errmsg)
                close (luntmp)
                goto 9999
            endif 
        else
            clddata%ittar(icll,1) = CL_NOT_IN_SUBDOMAIN
            clddata%linar(icll) = CL_NOT_IN_SUBDOMAIN
        end if
    
    endif
    !        
    if (.not.lokay) then
       !
       ! Cannot interpret line
       !
       error = .true.
       rec132 = ' '
       write (rec132, '(i12)') mcurec
       errmsg = 'Read error from file: '// trim(filnam) // ', Record: ' // trim(rec132)
       call mess(LEVEL_ERROR, errmsg)
       close (luntmp)
       goto 9999
    endif

    goto 210
9999 continue
    if (phase == GET_DIMENSIONS) then 
        ncll = icll
    endif
    
end subroutine read_cllfile

subroutine update_clddata() 
    use unstruc_messages
    use m_monitoring_crosssections
    use m_observations, only: numobs, valobs, IPNT_S1
    
    !integer, intent(in) :: update_mode
    double precision :: zs 
    double precision :: q 
    double precision :: f
    
    integer :: icldobs 
    integer :: icldcrs 
    
    ! Retrieve discharge and water levels  
    
    ! Set water level dependent calibration value
    do icldobs = 1,ncldobs
        zs = valobs( clddata%obs(icldobs),IPNT_S1) 
        call f_from_table_of_x( clddata%cldtable_zs, clddata%rttdef_zs, clddata%start_zs(icldobs), clddata%end_zs(icldobs), clddata%slope_zs, clddata%cross_zs, zs, f)
        ! store to definitions
        clddata%rttdef(clddata%icld_zs(icldobs)) = f 
        !
    enddo
    !
    ! Set discharge dependent calibration value
    do icldcrs = 1,ncldcrs
        q = crs(clddata%crs(icldcrs))%sumvalcur(IPNT_Q1C)
        call f_from_table_of_x( clddata%cldtable_q, clddata%rttdef_q, clddata%start_q(icldcrs), clddata%end_q(icldcrs), clddata%slope_q, clddata%cross_q, q, f)
        ! store to definitions
        clddata%rttdef(clddata%icld_q(icldcrs)) = f 
        !
    enddo    
    
end subroutine update_clddata    

subroutine update_clldata() 
use network_data,    only : numl 
use m_flow,          only : cfclval

! Updates and averages all calibration factors on net links
    integer :: icll
    integer :: L 
    integer :: icld
    
    cfclval = 0.d0
    do icll = 1, ncll 
       L = clddata%ittar(icll,1)
       if (L .ne. CL_NOT_IN_SUBDOMAIN) then
          icld = clddata%ittar(icll,2)
          cfclval(L) = cfclval(L) + clddata%rttar(icll)*clddata%rttdef(icld)
       endif
    enddo 
    
    ! add background calibration factor == 1 for parts missing areal definition 
    do L = 1, numl
        cfclval(L) = cfclval(L) + (1.d0 - clddata%sumar(L))!*1.d0 
    enddo

end subroutine update_clldata


subroutine f_from_table_of_x( xvals, fvals, idx_start, idx_end, fslope, fcross, x, f)
! Calibration helper function -- Determines f value from a f table which is a function of x based on a query value x 
! TO DO: general function available? combine with trachy?

    implicit none 

    double precision, dimension(:), intent(in) :: xvals 
    double precision, dimension(:), intent(in) :: fvals 
    integer, intent(in) :: idx_start
    integer, intent(in) :: idx_end
    double precision, dimension(:), intent(in) :: fcross
    double precision, dimension(:), intent(in) :: fslope
    double precision, intent(in) :: x 

    double precision, intent(out) :: f

    integer :: idx 
    
    if (x < xvals(idx_start)) then 
        ! value is smaller than first value in table, 
        ! so take first value from the table 
        f = fvals(idx_start)
    elseif (x .ge. xvals(idx_end)) then 
        ! value is larger than or equal to the last value in table, 
        ! so take last value from the table 
        f = fvals(idx_end)
    else
        ! x value lies in the range of values in the f table, 
        ! so find interval
        find_index_in_table_loop: &
        do idx = idx_start, idx_end-1
            if (x < xvals(idx+1)) then 
                ! idx is at the right position now
                exit find_index_in_table_loop
            end if  
        end do & 
        find_index_in_table_loop
        f = fcross(idx) + x*fslope(idx)
    end if 
end subroutine f_from_table_of_x

! Initialise calibration factors
subroutine calibration_backup_frcu()
 use m_flow,        only: frcu, frcu_bkp 

 implicit none
 
 frcu_bkp = frcu

end subroutine calibration_backup_frcu    
    
subroutine clr_clddata(clddata)
use unstruc_messages

type(cldtype),  intent(inout) :: clddata
integer                       :: istat

    if (allocated(clddata%cldtable_q))     deallocate (clddata%cldtable_q  , STAT = istat)
    if (allocated(clddata%cldtable_zs))    deallocate (clddata%cldtable_zs , STAT = istat)
    ! TO DO: add others -> and where to connect deallocate in FM ? 
    
    if (istat/=0) then
       errmsg = 'clr_clddata: memory deallocation error'
       call mess(LEVEL_ERROR, errmsg)
    endif

end subroutine clr_clddata

    
end module
