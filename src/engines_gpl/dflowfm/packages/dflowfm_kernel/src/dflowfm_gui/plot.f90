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

      !> Plot for hardcopy needs to be called twice: one to open hardcopy
      !! driver (file), then perform actual plotting, and second call to
      !! plot() closes the driver/file again. Steered by nopen argument.
      !!     Normal snapshot sequence: nchdev .le. 12: 1 open , 2 close,    0 neutral
      !!     Interactive screendump  : nchdev .ge. 13: 1 dump ,-1 nothing , 0 neutral
      subroutine PLOT(NOPEN)
         use string_module
         use unstruc_colors
         use unstruc_display
         use unstruc_messages
         use unstruc_model, only: md_ident, md_snapshotdir, md_snapshot_seqnr
         use unstruc_opengl, only: jaopengl
         implicit none
         integer :: i
         integer :: ihcopts
         integer :: l
         integer :: nhcdev
         integer :: nopen, mout
         integer :: numhcopts
         character PLOTJE * 255, EXT * 4
         common / HARDCOPY / NHCDEV, NUMHCOPTS, IHCOPTS(2, 20)
         common / PLOTFIL / PLOTJE

         if (Jaopengl == 1) then
            nhcdev = 14
         end if

!     file vullen: nhcdev .le. 12: 1 open , 2 dicht, 0 neutraal
!     screendump : nhcdev .ge. 13: 1 dump ,-1 niks , 0 neutraal
         if (NOPEN == 1) then

            open (newunit=mout, file=trim(md_ident)//'.x1y1x2')
            write (mout, *) x1, y1, x2
            close (mout)

            if (NHCDEV == 1) then
               EXT = '.hgl'
            else if (NHCDEV == 2) then
               EXT = '.ps '
               do I = 1, NUMHCOPTs
                  if (IHCOPTS(1, I) == 22) then
                     if (IHCOPTS(2, I) == 1) EXT = '.eps'
                  end if
               end do

            else if (NHCDEV == 3) then
               EXT = '.acd'
            else if (NHCDEV == 4) then
               EXT = '.rgh'
            else if (NHCDEV == 5) then
               EXT = '.tkx'
            else if (NHCDEV == 6) then
               EXT = '.bmp'
            else if (NHCDEV == 7) then
               EXT = '.pcx'
            else if (NHCDEV == 8) then
               EXT = '.dxf'
            else if (NHCDEV == 9) then
               EXT = '.cgm'
            else if (NHCDEV == 10) then
               EXT = '.wpm'
            else if (NHCDEV == 11) then
               EXT = '.wmf'
            else if (NHCDEV == 12) then
               EXT = '.gl2'
            else if (NHCDEV == 13) then
               EXT = '.bmp'
            else if (NHCDEV == 14) then
               EXT = '.pcx'
            end if
            L = len_trim(PLOTJE)
            if (L == 0) then
               md_snapshot_seqnr = md_snapshot_seqnr + 1
               L = len_trim(md_snapshotdir)
               if (L > 0) then
                  PLOTJE = md_snapshotdir
                  L = L + 1
                  plotje(L:L) = '/'
               end if
               write (PLOTJE(L + 1:), '(I6.6,A4)') md_snapshot_seqnr, EXT
            else
               ! Not in use now, but it's possible through common /plotfil/ to specify file name.
               ! md_snapshotdir is not used then...
               write (PLOTJE(L + 1:), '(A4)') EXT
            end if

!        SET OPTIONS
            if (NHCDEV <= 12) then
               NOPEN = 2
               call IGRPALETTERGB(0, NREDP, NGREENP, NBLUEP)

               call IGrHardCopySelect(1, NHCDEV)
               if (NHCDEV == 7) call IGrHardCopySelect(1, 6)
               do I = 1, NUMHCOPTS
                  call IGrHardCopyOptions(IHCOPTS(1, I), IHCOPTS(2, I))
               end do
               if (NHCDEV == 7) call IGrHardCopyOptions(26, 0)
               call IGrHardCopy(trim(PLOTJE))
               !WRITE(msgbuf,'(2A)') 'You created plotfile ', trim(PLOTJE) ; call msg_flush()
               call IWINOPEN(1, 1, 20, 1)
               call IWINOUTCENTRE(1, 'creating '//trim(PLOTJE))
            else
               NOPEN = 2

               !  CALL ISCREENSAVEIMAGE(trim(PLOTJE))
               !  CALL IGRSAVEIMAGE(trim(PLOTJE))
               !  PLOTJE = ' '
            end if
         else if (NOPEN == 2) then
            if (NHCDEV <= 12) then
               call IWINCLOSE(1)
               call IGrHardCopy('S')
               call IGRPALETTERGB(0, NREDS, NGREENS, NBLUES)
               NOPEN = 0
            else
               call ISCREENSAVEIMAGE(trim(PLOTJE))
               write (msgbuf, '(2A)') 'You created SCREENDUMP ', trim(PLOTJE)
               call msg_flush()
               NOPEN = 0
            end if
            PLOTJE = ' '
         else if (NOPEN == -1) then
            NOPEN = 0
            PLOTJE = ' '
         end if
         return
      end
