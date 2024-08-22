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

   subroutine DRAWNU(KEY)
      use m_netw
      use M_SAMPLES
      use m_arcinfo
      use unstruc_display
      use unstruc_opengl
      implicit none

      double precision :: epsgs
      integer :: itgs
      integer :: maxitgs
      integer :: metdraw
      integer :: ndraw

      integer :: KEY, nsiz

      common / DRAWTHIS / ndraw(50)

      common / SOLVER / EPSGS, MAXITGS, ITGS

!
      if (KEY /= 3) return

      METDRAW = NDRAW(9)

      call IMouseCursorHIDE()
      call PLOT(NDRAW(10))
      if (NDRAW(10) == -1) then
         return
      end if

      if (jaOpengl == 0) then
         if (METDRAW == 1) call FULLSCREEN()
         if (NDRAW(1) == 1 .and. jaOpenGL == 0) call CLS1()
         if (NDRAW(26) == 1) call SHOWBITMAP(0)
         if (METDRAW == 1) call SMALLSCREEN()
      else
         call BEGINRENDER()
      end if

      METDRAW = NDRAW(9)
      ! ndraw(28)= show what on nodes   ndraw(19)=how to show on nodes , NDRAW(8) = SHOW WHAT ON NETNODES
      ! ndraw(29)= show what on links   ndraw(11)=how to show on links , NDRAW(7) = SHOW WHAT ON NETLINKS

      if (ndraw(3) > 4) call TEKLAN(NCOLLN)

      if (NDRAW(7) >= 2) then
         call NETLINKVALS(NDRAW(7))
         call MINMXNETLINS()
      end if

      if (NDRAW(8) >= 2) then
         call NETNODEVALS(NDRAW(8))
         call MINMXNETNODS()
      end if

      if (METDRAW == 1) then

         call TEKNETSTUFF(key)

         call TEKFLOWSTUFF(key)

         call highlight_nodesnlinks()

         if (ndrawpol == 3) then
            call tekpolygon()
         end if

         call TEKgrid(key)

         if (mca * nca > maxsamarc) then
            call TEKarc(ndraw(32))
         else if (ns > 0) then
            call teksam(ndraw(32))
         end if

         if (ndraw(2) == 6) then
            call TEKNET(key) ! network on top
         end if

         if (ndraw(3) <= 4) call TEKLAN(NCOLLN)

         call plotObservations()

         call teksorsin()

         call plotSplines()

         ! obs plotting used to be here [AvD]
         if (NDRAW(18) > 1) then
            nsiz = ndraw(18) - 1
            call tekrai(nsiz)
         end if

         call tekprofs() ! and initialise some turb parstm.amp

         call plotCrossSections()

         call plotThinDams()
         call plotFixedWeirs()

         call tekwindvector()

         if (ndrawpol > 1 .and. ndrawpol /= 3) then
            call tekpolygon()
         end if

         call plotdots()

         call plotStructures()

      else if (METDRAW == 2) then

         ! CALL PERSPC()

      end if

      ! WARNING: Anything drawn up to this point with something other than OpenGL, is overwritten!
      ! So make sure you use OpenGL for any rendering up to this point, move EndRender up, or place
      ! that graphics code after EndRender.

      call ENDRENDER()

      if (METDRAW == 1) call FULLSCREEN()
      call ISOSCALE()
      call ISOSCALE2()
      call TXTLINES()

      if (METDRAW == 1) call SMALLSCREEN()
      if (METDRAW == 1) call AXES()
      call ANCHORCLS()
      call DISPOS()

      call TEXTFLOW()
      if (idisLink /= 0) then ! Display info. screen for a 1D flowlink if it has been clicked
         call disln(idisLink)
         call dis_info_1d_link(idisLink)
      end if

      call IMouseCursorShow()

      if (NDRAW(10) == 2) then
         call PLOT(NDRAW(10))
      end if

      return
   end subroutine DRAWNU
