subroutine rdrund(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                & runtxt    ,gdp       )
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
!    Function: Reads run description defined by user
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
    integer , pointer :: itis
!
! Global variables
!
    integer                                  :: lundia !  Description and declaration in inout.igs
    integer                                  :: lunmd  !  Description and declaration in inout.igs
    integer                                  :: nrrec  !!  Pointer to the record number in the MD-file
    logical                                  :: error  !!  Flag=TRUE if an error is encountered
    character(*)                             :: mdfrec !!  Standard rec. length in MD-file (300)
    character(30), dimension(10)             :: runtxt !!  Textual description of model input
!
!
! Local variables
!
    integer                        :: i      ! Loop counter for RUNTXT array 
    integer                        :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                        :: lkw    ! Actual length of KEYW 
    integer                        :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                        :: ntrec  ! Help. var to keep track of NRREC 
    integer                        :: nwrite ! Number of lines to write to diagnostic file 
    logical                        :: found  ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                        :: lerror ! Flag=TRUE if an error is encountered 
    logical                        :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    character(30)                  :: tdef   ! Help variable containing blanks 
    character(30), dimension(10)   :: thulp  ! Help character variable 
    character(6)                   :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    itis  => gdp%gdrdpara%itis
    !
    lerror = .false.
    newkw = .true.
    tdef = ' '
    !
    !     initialise text strings
    !
    do i = 1, 10
       runtxt(i) = tdef
       thulp(i) = tdef
    enddo
    !
    nwrite = 0
    !
    !-----model description
    !     default value allowed => defaul
    !
    keyw = 'Runtxt'
    ntrec = nrrec
    lkw = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    !-----not found ?
    !
    if (found) then
       lenc = 30
       nlook = 10
       call readnc(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,thulp     ,tdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       !
       !--------reading error?
       !        NLOOK can have a different value if not all elements are
       !        read; is ok
       !
       if (lerror) then
          lerror = .false.
          do i = 1, 10
             runtxt(i) = tdef
          enddo
       else
          do i = 1, 10
             runtxt(i) = thulp(i)
             if (runtxt(i)/=tdef) nwrite = i
          enddo
       endif
    endif
    !
    !-----write output to file
    !
    if (nwrite/=0) then
       write (lundia, '(a)') '*** Start of User Defined Model description '
       write (lundia, '(a)') ' -'
       do i = 1, nwrite
          write (lundia, '(a,a)') ' -  ', runtxt(i)
       enddo
       write (lundia, '(a)') ' -'
       write (lundia, '(a)') '*** End   of User Defined Model description '
       write (lundia, '(a)')
    endif
end subroutine rdrund
