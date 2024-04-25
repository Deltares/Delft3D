subroutine rdusrp(lunmd     ,lundia    ,error     ,mdfrec    ,nrrec     , &
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
!    Function: - Reads the user defined processes from the
!                MD-file: NPRCUS and nrpcus times PRCUSR & NPRINP
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                        , pointer :: itis
    integer                        , pointer :: nprocs
    integer      , dimension(:, :) , pointer :: nprdim
    integer      , dimension(:)    , pointer :: nread
    integer                        , pointer :: nprcus
    integer      , dimension(:, :) , pointer :: nprinp
    integer      , dimension(:)    , pointer :: icousr
    real(fp)     , dimension(:)    , pointer :: rcousr
    character*256, dimension(:)    , pointer :: filusr
    character*20 , dimension(:)    , pointer :: procs
    character*20 , dimension(:)    , pointer :: prcusr
    character*20 , dimension(:)    , pointer :: ccousr
!
! Global variables
!
    integer                    :: lundia !  Description and declaration in inout.igs
    integer                    :: lunmd  !  Description and declaration in inout.igs
    integer                    :: nrrec  !!  Pointer to the record number in the MD-file
    logical      , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)               :: mdfrec !!  Standard rec. length in MD-file (300)
!
!
! Local variables
!
    integer                     :: idef   ! Default value for integer 
    integer                     :: k
    integer                     :: lenc   ! Help var. (length of var. chulp to be looked for in the MD-file) 
    integer                     :: lkw
    integer                     :: n
    integer                     :: nk
    integer                     :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                     :: ntrec  ! Help. var to keep track of NRREC 
    integer      , dimension(4) :: isom
    integer      , dimension(4) :: ival   ! Help array for reading 
    logical                     :: defaul ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                     :: found
    logical                     :: lerror ! Flag=TRUE if a local error is encountered 
    logical                     :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                     :: nodef  ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    character(20)               :: chdef  ! Default file name (blank) 
    character(20)               :: chhlp  ! Help variable for reading 
    character(3)                :: errtxt ! errormessage text 
    character(6)                :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    nprocs  => gdp%gdusrpar%nprocs
    nprdim  => gdp%gdusrpar%nprdim
    nread   => gdp%gdusrpar%nread
    nprcus  => gdp%gdusrpar%nprcus
    nprinp  => gdp%gdusrpar%nprinp
    icousr  => gdp%gdusrpar%icousr
    rcousr  => gdp%gdusrpar%rcousr
    filusr  => gdp%gdusrpar%filusr
    procs   => gdp%gdusrpar%procs
    prcusr  => gdp%gdusrpar%prcusr
    ccousr  => gdp%gdusrpar%ccousr
    itis    => gdp%gdrdpara%itis
    !
    do n = 1, nprocs
       nread(n) = 0
    enddo
    !
    do n = 1, mxusrp
       prcusr(n) = ' '
       filusr(n) = ' '
       rcousr(n) = 0.0
       icousr(n) = 0
       ccousr(n) = ' '
       do k = 1, 4
          nprinp(k, n) = 0
       enddo
    enddo
    !
    nprcus = 0
    !
    !-----initialize local parameters
    !
    lerror = .false.
    newkw = .true.
    defaul = .true.
    nodef = .not.defaul
    found = .true.
    idef = 0
    chdef = ' '
    !
    !-----Test value of nprocs
    !
    if (nprocs==0) goto 9999
    !
    !-----locate 'Nprcus' record for number of user defined input processes
    !
    keyw = 'Nprcus'
    ntrec = nrrec
    lkw = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    !
    !-----found ? then read NPRCUS
    !
    if (found) then
       nlook = 1
       call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,ival      ,idef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       !
       !--------reading error?
       !
       if (lerror) then
          lerror = .false.
          ival(1) = idef
       endif
       nprcus = ival(1)
       !
       !--------test NPRCUS inside dimension MXUSRP
       !
       if (nprcus>nprocs) then
          write (errtxt(:2), '(i2)') nprocs
          call prterr(lundia    ,'V211'    ,errtxt(:2))
          !
          error = .true.
          lerror = .true.
          goto 9999
       endif
       !
       !--------locate user defined processes 'Prcusr' record
       !        for name of user defined process
       !        Note: be aware that the name 'Prcusr' is case-sensitive !!
       !
       do k = 1, 4
          isom(k) = 0
       enddo
       !
       ntrec = nrrec
       do n = 1, nprcus
          keyw = 'Prcusr'
          lenc = 20
          nlook = 1
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,chhlp     ,chdef     ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          !
          !-----------reading error?
          !
          if (lerror) then
             lerror = .false.
             chhlp = chdef
          endif
          prcusr(n) = chhlp
          !
          !-----------test contents of PRCUSR(N) Should not be blank !!
          !
          if (prcusr(n)==chdef) then
             call prterr(lundia    ,'V212'    ,' '       )
             !
             error = .true.
             lerror = .true.
             exit
          endif
          !
          !-----------test contents of PRCUSR(N) should be one of the pre-
          !           defined processes
          !
          nk = 0
          do k = 1, nprocs
             if (prcusr(n)==procs(k)) then
                if (nread(k)/=0) then
                   call prterr(lundia    ,'V213'    ,prcusr(n) )
                   !
                   error = .true.
                   lerror = .true.
                   goto 9999
                endif
                nk = k
                exit
             endif
          enddo
          !
          if (nk==0) then
             call prterr(lundia    ,'V202'    ,prcusr(n) )
             !
             error = .true.
             lerror = .true.
             exit
          endif
          nread(nk) = n
          !
          !-----------locate user defined processes 'Prcusr' record
          !           for name of user defined process
          !
          keyw = 'Nprinp'
          nlook = 4
          call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,ival      ,idef      ,nodef     ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          !
          !-----------reading error?
          !
          if (lerror) then
             lerror = .false.
             error = .true.
             do k = 1, 4
                ival(k) = idef
             enddo
          endif
          !
          !-----------Define NPRINP and compare contents NPRINP with NPRDIM
          !           test inside dimension MXUSRP
          !
          do k = 1, 4
             nprinp(k, n) = ival(k)
             !
             if (nprinp(k, n)/=nprdim(k, nk)) then
                call prterr(lundia    ,'V214'    ,prcusr(n) )
                !
                error = .true.
                lerror = .true.
                goto 9999
             endif
             !
             isom(k) = isom(k) + ival(k)
             if (isom(k)>mxusrp) then
                write (errtxt(:2), '(i2)') mxusrp
                call prterr(lundia    ,'V215'    ,errtxt(:2))
                !
                error = .true.
                lerror = .true.
                goto 9999
             endif
          !
          enddo
       !
       enddo
    endif
    !
    !
    !-----Reset read input to avoid inconstant reading later on
    !
 9999 continue
    if (lerror) then
       call prterr(lundia    ,'V201'    ,' '       )
       !
       nprcus = 0
       do n = 1, mxusrp
          prcusr(n) = chdef
          do k = 1, 4
             nprinp(k, n) = idef
          enddo
       enddo
    endif
end subroutine rdusrp
