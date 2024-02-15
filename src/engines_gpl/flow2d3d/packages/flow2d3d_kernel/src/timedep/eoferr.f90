subroutine eoferr(srttdd    ,lundia    ,iocond    ,tlread    ,gdp       )
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
!    Function: Checks IO condition from reading time varying
!              data read in ROUTNM and exit program
!              NOTE: in future exit from program using logical
!                    ERROR and no exit when reaching EOF but
!                    re-defining data read to last read data
!                    and last time read to ITFINISH
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
!
! Global variables
!
    integer, intent(in)            :: iocond
                                   !!  Flag for reading errors
                                   !!  = 0 No error
                                   !!  < 0 End-Of-File reached
                                   !!  > 0 Reading error
    integer, intent(in)            :: lundia !  Description and declaration in inout.igs
    real(fp), intent(in)               :: tlread
                                   !!  Last time read from file (in minutes)
    character(3), intent(in)       :: srttdd
                                   !!  Character string containing file
                                   !!  definition for Time dependent Data
!
!
! Local variables
!
    integer                        :: iexit                ! Exit value <> 0 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----Write error message to LUNDIA depending on value of IOCOND
    !
    if (iocond<0) then
       write (lundia, '(2a)')                                                   &
                          & '*** ERROR End-Of-file reached for file TMP_<runid>.'&
                         & , srttdd
    else
       write (lundia, '(2a)')                                                   &
                      & '*** ERROR Error occured while reading file TMP_<runid>.'&
                     & , srttdd
    endif
    !
    !-----Write last time read to LUNDIA
    !
    write (lundia, '(a,g13.6)') '  Last time read: ', tlread
    !
    !-----Exit program
    !
    iexit = 1
    call d3stop(iexit     ,gdp       )
end subroutine eoferr
