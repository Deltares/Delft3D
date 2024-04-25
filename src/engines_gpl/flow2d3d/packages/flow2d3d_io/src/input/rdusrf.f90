subroutine rdusrf(lunmd     ,lundia    ,error     ,mdfrec    ,nrrec     , &
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
!    Function: - Reads the user defined files from the MD-file:
!                FILUSR
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
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
    character*256, dimension(:)    , pointer :: filusr
!
! Global variables
!
    integer                   :: lundia !  Description and declaration in inout.igs
    integer                   :: lunmd  !  Description and declaration in inout.igs
    integer                   :: nrrec  !!  Pointer to the record number in the
                                        !!  MD-file
    logical     , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)              :: mdfrec !!  Standard rec. length in MD-file (300)
!
! Local variables
!
    integer        :: lenc   ! Help var. (length of var. chulp to be looked for in the MD-file) 
    integer        :: n
    integer        :: nfil   ! Number of files to be read 
    integer        :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer        :: ntrec  ! Help. var to keep track of NRREC 
    logical        :: lerror ! Flag=TRUE if a local error is encountered 
    logical        :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    character(256) :: fildef ! Default file name (usually = blank) 
    character(6)   :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!! executable statements -------------------------------------------------------
!
    nprcus  => gdp%gdusrpar%nprcus
    nprinp  => gdp%gdusrpar%nprinp
    filusr  => gdp%gdusrpar%filusr
    !
    if (nprcus==0) goto 9999
    !
    !-----initialize local parameters
    !
    lerror = .false.
    newkw = .true.
    fildef = ' '
    nlook = 1
    !
    !-----initialize number of files to be read (only if NFIl > 0)
    !
    nfil = 0
    do n = 1, nprcus
       nfil = nfil + nprinp(1, n)
    enddo
    if (nfil>0) then
       !
       !--------locate 'Filusr' record for user defined file names
       !        File name can be blank. total number of files >= MXUSRP
       !        see also routine RDUSRP
       !        first entry read no new record
       !
       do n = 1, nfil
          keyw = 'Filusr'
          ntrec = nrrec
          lenc = 256
          call readnc(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,filusr(n) ,fildef    ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          !
          !-----------reading error?
          !
          if (lerror) then
             lerror = .false.
             filusr(n) = fildef
          endif
          !
          !-----------next entries read new record
          !
          newkw = .false.
          !
          !-----------test FILUSR(NFIL) Should not be blank !!
          !
          if (filusr(n)==fildef) then
             call prterr(lundia    ,'V220'    ,' '       )
             !
             error = .true.
             exit
          endif
       enddo
    endif
    !
 9999 continue
end subroutine rdusrf
