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
      SUBROUTINE PLOT(NOPEN)
      use string_module
      use unstruc_colors
      use unstruc_display
      use unstruc_messages
      use unstruc_model,  only: md_ident, md_snapshotdir, md_snapshot_seqnr
      use unstruc_opengl, only: jaopengl
      implicit none
      integer :: i
      integer :: ihcopts
      integer :: l
      integer :: nhcdev
      integer :: nopen, mout
      integer :: numhcopts
      CHARACTER PLOTJE*255,EXT*4
      COMMON /HARDCOPY/  NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
      COMMON /PLOTFIL/   PLOTJE

      if (Jaopengl == 1)  then
         nhcdev = 14
      endif

!     file vullen: nhcdev .le. 12: 1 open , 2 dicht, 0 neutraal
!     screendump : nhcdev .ge. 13: 1 dump ,-1 niks , 0 neutraal
      IF (NOPEN .EQ. 1) THEN

         open (newunit=mout, file = trim(md_ident)//'.x1y1x2')
         write(mout,*) x1,y1, x2
         close(mout)


         IF (NHCDEV .EQ. 1) THEN
            EXT = '.hgl'
         ELSE IF (NHCDEV .EQ. 2) THEN
            EXT = '.ps '
            do I = 1,NUMHCOPTs
               IF (IHCOPTS(1,I) .EQ. 22) THEN
                  IF (IHCOPTS(2,I) .EQ. 1) EXT = '.eps'
               ENDIF
            end do

         ELSE IF (NHCDEV .EQ. 3) THEN
            EXT = '.acd'
         ELSE IF (NHCDEV .EQ. 4) THEN
            EXT = '.rgh'
         ELSE IF (NHCDEV .EQ. 5) THEN
            EXT = '.tkx'
         ELSE IF (NHCDEV .EQ. 6) THEN
            EXT = '.bmp'
         ELSE IF (NHCDEV .EQ. 7) THEN
            EXT = '.pcx'
         ELSE IF (NHCDEV .EQ. 8) THEN
            EXT = '.dxf'
         ELSE IF (NHCDEV .EQ. 9) THEN
            EXT = '.cgm'
         ELSE IF (NHCDEV .EQ. 10) THEN
            EXT = '.wpm'
         ELSE IF (NHCDEV .EQ. 11) THEN
            EXT = '.wmf'
         ELSE IF (NHCDEV .EQ. 12) THEN
            EXT = '.gl2'
         ELSE IF (NHCDEV .EQ. 13) THEN
            EXT = '.bmp'
         ELSE IF (NHCDEV .EQ. 14) THEN
            EXT = '.pcx'
         ENDIF
         L  = len_trim( PLOTJE )
         IF (L .EQ. 0) THEN
            md_snapshot_seqnr = md_snapshot_seqnr + 1
            L = len_trim(md_snapshotdir)
            if (L > 0) then
                PLOTJE = md_snapshotdir
                L = L+1
                plotje(L:L) = '/'
            end if
            WRITE (PLOTJE(L+1:),'(I6.6,A4)') md_snapshot_seqnr,EXT
         ELSE
            ! Not in use now, but it's possible through common /plotfil/ to specify file name.
            ! md_snapshotdir is not used then...
            WRITE (PLOTJE(L+1:),'(A4)') EXT
         ENDIF

!        SET OPTIONS
         IF (NHCDEV .LE. 12) THEN
            NOPEN = 2
            CALL IGRPALETTERGB(  0,NREDP,NGREENP,NBLUEP)

            CALL IGrHardCopySelect(1,NHCDEV)
            IF (NHCDEV .EQ. 7) CALL IGrHardCopySelect(1,6)
            do I = 1,NUMHCOPTS
               CALL IGrHardCopyOptions( IHCOPTS(1,I), IHCOPTS(2,I) )
            end do
            IF (NHCDEV .EQ. 7) CALL IGrHardCopyOptions(26,0)
            CALL IGrHardCopy(trim(PLOTJE))
            !WRITE(msgbuf,'(2A)') 'You created plotfile ', trim(PLOTJE) ; call msg_flush()
            CALL IWINOPEN(1,1,20,1)
            CALL IWINOUTCENTRE(1,'creating '//trim(PLOTJE))
         ELSE
            NOPEN = 2

           !  CALL ISCREENSAVEIMAGE(trim(PLOTJE))
           !  CALL IGRSAVEIMAGE(trim(PLOTJE))
           !  PLOTJE = ' '
         ENDIF
      ELSE IF (NOPEN .EQ. 2) THEN
         IF (NHCDEV .LE. 12) THEN
            CALL IWINCLOSE(1)
            CALL IGrHardCopy('S')
            CALL IGRPALETTERGB( 0, NREDS, NGREENS, NBLUES)
            NOPEN = 0
         ELSE
            CALL ISCREENSAVEIMAGE(trim(PLOTJE))
            WRITE(msgbuf,'(2A)') 'You created SCREENDUMP ', trim(PLOTJE)
            call msg_flush()
            NOPEN = 0
         ENDIF
         PLOTJE = ' '
      ELSE IF (NOPEN .EQ. -1) THEN
         NOPEN = 0
         PLOTJE = ' '
      ENDIF
      RETURN
      END
