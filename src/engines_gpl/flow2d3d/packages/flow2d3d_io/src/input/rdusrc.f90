subroutine rdusrc(lunmd     ,lundia    ,error     ,mdfrec    ,nrrec     , &
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
!    Function: - Reads the user defined constants from the
!                MD-file: RCOUSR, ICOUSR & CCOUSR
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
    integer                        , pointer :: nprcus
    integer      , dimension(:, :) , pointer :: nprinp
    integer      , dimension(:)    , pointer :: icousr
    real(fp)     , dimension(:)    , pointer :: rcousr
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
    integer            :: idef   ! Default value for integer 
    integer            :: lenc   ! Help var. (length of var. chulp to be looked for in the MD-file) 
    integer            :: n
    integer            :: ncharc
    integer            :: nintc
    integer            :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer            :: nrealc
    integer            :: ntrec  ! Help. var to keep track of NRREC 
    logical            :: defaul ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical            :: lerror ! Flag=TRUE if a local error is encountered 
    logical            :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical            :: nodef  ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    real(fp)           :: rdef
    character(20)      :: chdef
    character(6)       :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    nprcus  => gdp%gdusrpar%nprcus
    nprinp  => gdp%gdusrpar%nprinp
    icousr  => gdp%gdusrpar%icousr
    rcousr  => gdp%gdusrpar%rcousr
    ccousr  => gdp%gdusrpar%ccousr
    !
    if (nprcus==0) goto 9999
    !
    !-----initialize local parameters
    !
    lerror = .false.
    newkw = .true.
    defaul = .true.
    nodef = .not.defaul
    rdef = 0.0
    idef = 0
    chdef = ' '
    !
    !-----initialize number of real constants to be read
    !      (only if NREALC > 0)
    !
    nrealc = 0
    do n = 1, nprcus
       nrealc = nrealc + nprinp(2, n)
    enddo
    if (nrealc>0) then
       !
       !--------locate 'Rcousr' record for user defined real constants
       !
       keyw = 'Rcousr'
       ntrec = nrrec
       nlook = nrealc
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rcousr    ,rdef      ,nodef     ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       !
       !--------reading error?
       !
       if (lerror) then
          lerror = .false.
          call prterr(lundia    ,'V221'    ,'real'    )
          !
          error = .true.
          goto 9999
       endif
    endif
    !
    !-----initialize number of integer constants to be read
    !      (only if NINTC > 0)
    !
    nintc = 0
    do n = 1, nprcus
       nintc = nintc + nprinp(3, n)
    enddo
    if (nintc>0) then
       !
       !--------locate 'Icousr' record for user defined integer constants
       !
       keyw = 'Icousr'
       ntrec = nrrec
       nlook = nintc
       call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,icousr    ,idef      ,nodef     ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       !
       !--------reading error?
       !
       if (lerror) then
          lerror = .false.
          call prterr(lundia    ,'V221'    ,'integer' )
          !
          error = .true.
          goto 9999
       endif
    endif
    !
    !-----initialize number of character constants to be read
    !      (only if NCHARC > 0)
    !
    ncharc = 0
    do n = 1, nprcus
       ncharc = ncharc + nprinp(4, n)
    enddo
    if (ncharc>0) then
       !
       !--------locate 'Ccousr' record for user defined character constants
       !        first entry read no new record
       !
       do n = 1, ncharc
          keyw = 'Ccousr'
          ntrec = nrrec
          lenc = 20
          nlook = 1
          call readnc(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,ccousr(n) ,chdef     ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          !
          !-----------reading error?
          !
          if (lerror) then
             lerror = .false.
             call prterr(lundia    ,'V221'    ,'character'          )
             !
             error = .true.
             exit
          endif
          !
          !-----------next entries read new record
          !
          newkw = .false.
       enddo
    endif
    !
 9999 continue
end subroutine rdusrc
