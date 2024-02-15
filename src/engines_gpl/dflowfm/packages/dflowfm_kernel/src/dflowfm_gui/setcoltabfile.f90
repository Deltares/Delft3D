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

SUBROUTINE SETCOLTABFILE(FILNAM,JASECOND)
    use unstruc_colors
    implicit none
    double precision :: dv, dv2
    integer :: i
    integer :: ierror
    integer :: iblue
    integer :: igreen
    integer :: ihue
    integer :: ired
    integer :: isat
    integer :: jaauto, jaauto2
    integer :: jahls
    integer :: jasecond
    integer :: k
    integer :: light
    integer :: minp
    integer :: ncols, ncols2
    integer :: nie, nie2
    integer :: nis, nis2
    integer :: nisn
    integer :: nv, nv2
    double precision :: val, val2
    double precision :: vmax, vmax2
    double precision :: vmin, vmin2
    integer, parameter  :: mxq = 1, mxclass = 1
    COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
    COMMON /DEPMAX2/ VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2
    CHARACTER FILNAM*(*), FOLNAM*86
    FOLNAM = FILNAM
    IF (FILNAM(1:5) .EQ. '*.hls') THEN
       MINP   = 0
       CALL FILEMENU(MINP,FOLNAM,ierror)
    ELSE
       k = len_trim(filnam)
       folnam(1:k) = filnam(1:k)
       CALL SYSORLOCALFIL(MINP,FOLNAM,0)
    ENDIF
    IF (MINP /= 0) THEN
       IF (INDEX(FOLNAM,'HLS') .GE. 1 .OR. INDEX(FOLNAM,'hls') .GE. 1 ) THEN
           JAHLS = 1
       ELSE IF (INDEX(FOLNAM,'RGB') .GE. 1 .OR. INDEX(FOLNAM,'rgb') .GE. 1 ) THEN
           JAHLS = 2
       ELSE
           CALL QNMESSAGE('CHOOSE *.hls OR *.rgb FILE')
           RETURN
       ENDIF
       IF (JASECOND .EQ. 0) THEN
          coltabfile  = folnam
       else
          coltabfile2 = folnam
       endif

       K = 1
       READ (MINP,*,END = 999,ERR=888)
 20    CONTINUE
       IF (JAHLS .EQ. 1) THEN
          READ (MINP,*,END = 999,ERR=888) IHUE,LIGHT,ISAT
          IHUE  = MAX(0,MIN(IHUE ,360))
          LIGHT = MAX(0,MIN(LIGHT,100))
          ISAT  = MAX(0,MIN(ISAT ,100))
          IF (JASECOND .EQ. 0) THEN
             CALL IGRPALETTEHLS(NCOLS(K),IHUE,LIGHT,ISAT)
          ELSE
             CALL IGRPALETTEHLS(NCOLS2(K),IHUE,LIGHT,ISAT)
          ENDIF
       ELSE IF (JAHLS .EQ. 2) THEN
          READ (MINP,*,END = 999,ERR=888) IRED,IGREEN,IBLUE
          IRED  = MAX(0,MIN(IRED   ,255))
          IGREEN= MAX(0,MIN(IGREEN ,255))
          IBLUE = MAX(0,MIN(IBLUE  ,255))
          IF (JASECOND .EQ. 0) THEN
             CALL IGRPALETTERGB(NCOLS(K),IRED,IGREEN,IBLUE)
          ELSE
             CALL IGRPALETTERGB(NCOLS2(K),IRED,IGREEN,IBLUE)
          ENDIF
       ENDIF
       K = K + 1
       GOTO 20
999    CONTINUE
       call doclose (MINP)
       IF (JASECOND .EQ. 0) THEN
          NV  = MAX(2,K-2)
          NIE = NIS + NV + 1
       else
          NV2  = MAX(2,K-2)
          NIE2 = NIS2 + NV2 + 1
       ENDIF
       RETURN
888    CONTINUE ! Read error in coltabfile, back to defaults.
       call doclose (MINP)
    ENDIF
    RETURN
    END
