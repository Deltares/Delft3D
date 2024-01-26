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

  SUBROUTINE DISPNODEVALS(KP)
  use m_netw
  USE M_DEVICES
  implicit none
  integer :: KP

  double precision :: fff
  double precision :: fxx
  double precision :: fyy
  double precision :: fzz
  integer :: l
  integer :: n
  CHARACTER TEX*23
  IF (KP .EQ. 0) RETURN
  CALL DRCIRC(XK(KP),YK(KP),ZK(KP))

  TEX = 'NODE NR    :           '
  WRITE(TEX (14:),'(I10)') KP
  CALL KTEXT(TEX,IWS-22,4,15)

  TEX = 'X COORD    :           '
  WRITE(TEX (14:),'(E10.3)') XK(KP)
  CALL KTEXT(TEX,IWS-22,13,15)

  TEX = 'Y COORD    :           '
  WRITE(TEX (14:),'(E10.3)') YK(KP)
  CALL KTEXT(TEX,IWS-22,14,15)

  TEX = 'Z COORD    :           '
  WRITE(TEX (14:),'(E10.3)') ZK(KP)
  CALL KTEXT(TEX,IWS-22,15,15)

  TEX = 'ELEM       :           '
  DO N = 1,NMK(KP)
     L = NOD(KP)%LIN(N)
     WRITE(TEX ( 6:11),'(I6 )') N
     WRITE(TEX (14:23),'(I10)') L
     CALL KTEXT(TEX,IWS-22,15+N,15)
  ENDDO

  if (netflow .eq. 2) return


  TEX = 'NR OF ELEMS:           '
  WRITE(TEX (14:),'(I10)') NMK(KP)
  CALL KTEXT(TEX,IWS-22,6,15)


  RETURN
  END SUBROUTINE DISPNODEVALS
