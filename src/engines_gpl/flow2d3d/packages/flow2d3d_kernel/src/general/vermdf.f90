subroutine vermdf(mdfrec    ,nrver     ,lundia    ,error     ,gdp       )
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
!    Function: Reads version nr. from the 1st record of MD-file
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
    integer , pointer :: ifis
    integer , pointer :: itis
!
! Global variables
!
    integer                   :: lundia !  Description and declaration in inout.igs
    integer                   :: nrver  !!  Integer representative of versio
    logical     , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*), intent(in)  :: mdfrec !!  Standard rec. length in MD-file
!
!
! Local variables
!
    integer            :: ihek
    integer            :: ikey
    integer            :: iread
    character(4)       :: versio ! Version nr. of the current package 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    ifis  => gdp%gdrdpara%ifis
    itis  => gdp%gdrdpara%itis
    !
    ikey = index(mdfrec(:itis), 'Ident')
    if (ikey==0) then
       nrver = 9999
    else
       !
       !--------Locate # in string
       !        if not found version will be set >>>
       !
       ihek = index(mdfrec(ifis:), '#')
       if (ihek==0) then
          nrver = 9999
       else
          !
          !-----------Read version number which is defined as xx.yy or xxdyy
          !           So we need to skip the middle character
          !
          iread = ifis - 1 + ihek + 16
          versio(1:2) = mdfrec(iread:iread + 1)
          versio(3:4) = mdfrec(iread + 3:iread + 4)
          read (versio, '(i4)') nrver
       endif
    endif
    !
    !-----Test version number of MD-file
    !
    if (nrver<240) then
       call prterr(lundia    ,'G106'    ,' '       )
       !
       error = .true.
    endif
    !
    if (nrver==9999) then
       call prterr(lundia    ,'G107'    ,' '       )
       !
       nrver = 300
    endif
end subroutine vermdf
