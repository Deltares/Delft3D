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

   SUBROUTINE DRAWNU(KEY)
   use m_netw
   USE M_SAMPLES
   use m_arcinfo
   use unstruc_display
   use unstruc_opengl
   implicit none

   double precision :: epsgs
   integer :: itgs
   integer :: maxitgs
   integer :: metdraw
   integer :: ndraw

   integer :: KEY, ja, nsiz

   COMMON /DRAWTHIS/  ndraw(50)

   COMMON /SOLVER/    EPSGS, MAXITGS, ITGS

!
   IF (KEY .NE. 3) RETURN

   METDRAW = NDRAW(9)

   CALL IMouseCursorHIDE()
   CALL PLOT(NDRAW(10))
   IF (NDRAW(10) .EQ. -1) THEN
      RETURN
   ENDIF

   if (jaOpengl == 0) then
      IF (METDRAW .EQ. 1)   CALL FULLSCREEN()
      IF (NDRAW(1) .EQ. 1 .and. jaOpenGL.eq.0 )  CALL CLS1()
      IF (NDRAW(26) .EQ. 1) CALL SHOWBITMAP(0)
      IF (METDRAW .EQ. 1)   CALL SMALLSCREEN()
   else
      CALL BEGINRENDER()
   endif


   METDRAW = NDRAW(9)
 ! ndraw(28)= show what on nodes   ndraw(19)=how to show on nodes , NDRAW(8) = SHOW WHAT ON NETNODES
 ! ndraw(29)= show what on links   ndraw(11)=how to show on links , NDRAW(7) = SHOW WHAT ON NETLINKS

   if (ndraw(3) > 4) CALL TEKLAN(NCOLLN)

   IF (NDRAW(7) .GE. 2) THEN
       CALL NETLINKVALS(NDRAW(7),NCOLLN)
       CALL MINMXNETLINS()
   ENDIF

   IF (NDRAW(8) .GE. 2) THEN
       CALL NETNODEVALS(NDRAW(8))
       CALL MINMXNETNODS()
   ENDIF

   IF (METDRAW .EQ. 1) THEN

      CALL TEKNETSTUFF(key)

      CALL TEKFLOWSTUFF(key)

      call highlight_nodesnlinks()

      if (ndrawpol == 3) then
         call tekpolygon()
      endif

      call TEKgrid(key)

      if (mca*nca > maxsamarc) then 
         call TEKarc(ndraw(32))
      else if (ns > 0) then 
         call teksam(ndraw(32))
      endif
    
      if (ndraw(2) == 6) then
         CALL TEKNET(NCOLDN,key) ! network on top
      end if

      if (ndraw(3) <= 4)  CALL TEKLAN(NCOLLN)

      call plotObservations()

      call teksorsin()

      call plotSplines()

     ! obs plotting used to be here [AvD]
      if (NDRAW(18) > 1) then
         nsiz = ndraw(18)-1
         call tekrai(nsiz,ja)
      endif

      call tekprofs()         ! and initialise some turb parstm.amp

      call plotCrossSections()

      call plotThinDams()
      call plotFixedWeirs()

      call tekwindvector()

      if (ndrawpol > 1 .and. ndrawpol .ne. 3) then
         call tekpolygon()
      endif

      call plotdots()

      call plotStructures()

   ELSE IF (METDRAW .EQ. 2) THEN

      ! CALL PERSPC()

   ENDIF

   ! WARNING: Anything drawn up to this point with something other than OpenGL, is overwritten!
   ! So make sure you use OpenGL for any rendering up to this point, move EndRender up, or place
   ! that graphics code after EndRender.

   CALL ENDRENDER()

   IF (METDRAW .EQ. 1) CALL FULLSCREEN()
   CALL ISOSCALE()
   CALL ISOSCALE2()
   CALL TXTLINES()

   IF (METDRAW .EQ. 1) CALL SMALLSCREEN()
   IF (METDRAW .EQ. 1) CALL AXES()
   CALL ANCHORCLS()
   CALL DISPOS()

   CALL TEXTFLOW()
   if (idisLink /= 0) then ! Display info. screen for a 1D flowlink if it has been clicked
      call disln(idisLink)
      call dis_info_1d_link(idisLink)
   end if

   CALL IMouseCursorShow()

   IF (NDRAW(10) .EQ. 2) THEN
      CALL PLOT(NDRAW(10))
   ENDIF

   RETURN
   END SUBROUTINE DRAWNU
