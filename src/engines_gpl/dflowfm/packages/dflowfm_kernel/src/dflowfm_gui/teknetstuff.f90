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

   SUBROUTINE TEKNETSTUFF(key)
   use unstruc_colors
   use unstruc_display, only: jaHighlight
   use m_netw
   implicit none
   integer :: ndraw
   double precision :: XP, YP

   integer :: key, K1, K2

   COMMON /DRAWTHIS/ ndraw(50)

   IF (NDRAW(7) .GE. 2) CALL TEKLINKVALS(NDRAW(11))

   IF (NDRAW(8) .GE. 2) CALL TEKNODEVALS(NDRAW(19))

   CALL TEKNET(NCOLDN,key)

   CALL TEKPREVIOUSNET(NCOLRN)



   IF (NDRAW(7) .GE. 2) CALL TEKLINKNUMS(NDRAW(11),NCOLLN)

   IF (NDRAW(8) .GE. 2) CALL TEKNODENUMS(NDRAW(19),NCOLDN)


   CALL TEKNETCELLS(NDRAW(33),0,1)

   ! CALL TEKBOTTOM(NDRAW(27)) old net stuff

   if (jaHighlight == 1) then
      if (nOdmax .ne. 0) then
         call gtext( 'NETNODMax', xK(nOdmax), yK(nOdmax), 31  )
      endif
      if (nOdmin .ne. 0) then
         call gtext( 'NETNODMin', xK(nOdmin), yK(nOdmin), 221 )
      endif
      if (LINmax .ne. 0) then
         K1 = KN(1,LINMAX)
         K2 = KN(2,LINMAX)
         IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
            XP = 0.5D0*(XK(K1) + XK(K2) )
            YP = 0.5D0*(YK(K1) + YK(K2) )
         ENDIF
         call gtext( 'NETLINMax', XP, YP, 31  )
      endif
      if (LINmin .ne. 0) then
         K1 = KN(1,LINMIN)
         K2 = KN(2,LINMIN)
         IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
            XP = 0.5D0*(XK(K1) + XK(K2) )
            YP = 0.5D0*(YK(K1) + YK(K2) )
            call gtext( 'NETLINMin', XP, YP, 221 )
         ENDIF
      endif
      if (netcelmax .ne. 0) then
         call gtext( 'NETcelmax', xzw(netcelmax), yzw(netcelmax), 31  )
      endif
      if (netcelmin .ne. 0) then
         call gtext( 'NETcelmin', xzw(netcelmin), yzw(netcelmin), 221 )
      endif
   end if

   RETURN
   END SUBROUTINE TEKNETSTUFF
