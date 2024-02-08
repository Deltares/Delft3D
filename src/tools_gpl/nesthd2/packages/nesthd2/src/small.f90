subroutine small(string    ,lenstr    )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer, intent(in)            :: lenstr
    character(*)    :: string
!
!
! Local variables
!
    integer                        :: i
    integer                        :: j
    integer                        :: newlen
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    ! Externe programmanaam  : SMALL.F
    ! Programmeur            : Cor van der Schelde
    ! Funktie omschrijving   : Omzetten van hoofdletters (in een string)
    !                          naar kleine letters (ivm UNIX)
    ! Aangeroepen door       : Various routines
    !                    Date: 01-08-2002
    !              CVS header
    !                 $Author: Mourits $
    !                   $Date: 18-04-03 16:49 $
    !                 $Source: /u/trisula/cvsroot/trisula/alg/small.f,v $
    !               $Revision: 1 $
    !
    ! verklaring externe variabelen/parameters
    ! ----------------------------------------
    ! naam    type      lengte   omschrijving
    ! ------  --------  ------   ------------
    ! lenstr  i*4       1        lengte van de string
    ! string  ch*(*)    1        string
    !
    !
    ! verklaring lokale variabelen
    ! ----------------------------
    ! naam    type      lengte   omschrijving
    ! ------  --------  ------   ------------
    ! i       i*4       1        loop variabele
    ! j       i*4       1        hulp variabele
    ! newlen  i*4       1        echte lengte string
    !
    !
    !
    ! common blocks          : geen
    !
    ! subroutines            : ichar  (intrinsic function)
    !                          char   (intrinsic function)
    !
    ! lun                    : geen
    !
    !
    !  declarations
    !
    !
    newlen = min(lenstr, len(string))
    do i = 1, newlen
       j = ichar(string(i:i))
       if ((j>64) .and. (j<91)) then
          j = j + 32
          string(i:i) = char(j)
       endif
    enddo
end subroutine small
