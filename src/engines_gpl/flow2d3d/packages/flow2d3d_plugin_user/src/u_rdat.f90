subroutine u_rdat(lundia    ,error     ,gdp       )
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
! - Reads the user defined data from arbitrary input
! file and stores all data through the common
! block defined in 'datusr.inc'
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
    character*256 , pointer :: filus1
    character*256 , pointer :: filus2
    character*256 , pointer :: filus3
    logical       , pointer :: first
!
! Global variables
!
    integer                        :: lundia !  Description and declaration in inout.igs
    logical, intent(out)           :: error
!
! Local variables
!
    integer                        :: ios
    integer                        :: luninp
    integer, external              :: newlun
    logical                        :: ex
    character(12)                  :: inpfil
    character(256)                 :: errtx1
    character(256)                 :: errtx2
!
!! executable statements -------------------------------------------------------
!
    first   => gdp%gdu_ppr%first
    filus1  => gdp%gddatusr%filus1
    filus2  => gdp%gddatusr%filus2
    filus3  => gdp%gddatusr%filus3
    !
    ! you may initialize local parameters here (EXAMPLE)
    !
    inpfil = 'filuser.inp'
    errtx1 = '**error premature EOF incomplete file:' // inpfil
    errtx2 = '**error file:' // inpfil // ' not found in working directory'
    filus1 = ' '
    filus2 = ' '
    filus3 = ' '
    !
    ! Open user file (EXAMPLE)
    ! test file existence using FLOW library utility first
    ! LEAVE ROUTINE WHEN FILE DOES NOT EXIST
    !
    ex = .false.
    inquire (file = inpfil(:11), exist = ex)
    if (.not.ex) goto 9999
    !
    call prterr(lundia    ,'G051'    ,'User defined output specified' )
    open (newunit=luninp, file = inpfil, form = 'formatted', err = 7777)
    !
    ! Read 3 file names from user file (EXAMPLE)
    ! One file for each of the following data: Map, History and Drogues
    !
    read (luninp, '(a)', iostat = ios) filus1
    if (ios<0) goto 8888
    read (luninp, '(a)', iostat = ios) filus2
    if (ios<0) goto 8888
    read (luninp, '(a)', iostat = ios) filus3
    if (ios<0) goto 8888
    close (luninp)
    goto 9999
    !
    ! Write error text to DIAGNOSTIC file (EXAMPLE)
    ! In this example, we use the standard PRTERR routine from FLOW
    ! general  library using the freely available error number U021
    ! to illustrate the use of PRTERR routine.
    ! OF COURSE you may also use the standard FORTRAN write statement
    ! as follows: write (lundia,'(a)') errtx1
    !
 7777 continue
    call prterr(lundia    ,'U021'    ,errtx2    )
    error = .true.
    goto 9999
 8888 continue
    call prterr(lundia    ,'U021'    ,errtx1    )
    error = .true.
    !
 9999 continue
end subroutine u_rdat
