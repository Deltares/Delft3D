!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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

 ! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 15-08-97 11:32a  $
!
! current revision: $Revision:: 3               $


      SUBROUTINE RDLOCA

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0.                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Bepalen Mappix knoop id (inlezen of standaard?
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! *********************************************************************
! *** 9 maps:
! ***   1 verhard gebied
! ***   2 onverhard gebied
! ***   3 kasgebied
! ***   4 open water
! ***   5 kunstwerken
! ***   6 randknopen
! ***   7 Pluvius inloopknopen
! ***   8 balansen (alle knopen)
! ***   9 zoutconcentraties (alle knopen)
! ***   10 RWZI
! ***   11 Industry
! *********************************************************************

     use Network
     USE CONF_FIL
     USE CONF_ARR
     use Link
     use Structures
     use Openwater
     use Greenhouse
     use Unpaved
     use Paved
     use NWRW
     use Boundary

!
!
      Integer iMap, iNode, i, iDebug, ikind, IExtnr, IIntVolgnr, ILink

      iDebug = ConfFil_get_iDebug()
      IF (iDebug /= 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDLOCA')

      DO IMAP = 1,NKAART
         select case (imap)
           case (1)
             NLCMAP (IMAP) = NCVHG
           case (2)
             NLCMAP (IMAP) = NCOVHG
           case (3)
             NLCMAP (IMAP) = NCKAS
           case (4)
             NLCMAP (IMAP) = NCOW
           case (5)
             NLCMAP (IMAP) = NCSTRU
           case (6)
             NLCMAP (IMAP) = NCBOUN
           case (7)
             NLCMAP (IMAP) = NCPLUV
           case (8)
             NLCMAP (IMAP) = NCNODE
           case (9)
             NLCMAP (IMAP) = NCNODE
           case (10)
             NLCMAP (IMAP) = NCRWZI
           case (11)
             NLCMAP (IMAP) = NCINDUS
           case (12)
             NLCMAP (IMAP) = NCSACR
           case (13)
             NLCMAP (IMAP) = NCLINK
           case (14)
             NLCMAP (IMAP) = NCCell
           case (15)
             NLCMAP (IMAP) = NCRRRunoff
           case default
             NLCMAP (IMAP) = 0
         end select
      ENDDO


      DO INODE=1,NCNODE
         ikind  = EINODE (INODE,3)
         IExtnr = EINODE (INODE,1)
         IIntVolgnr = EINODE (INODE,2)
         DO IMAP = 1,NKAART
            ! 13=link flows map, treated below
            IF (imap .ne. 13) then
              IXLMAP (IMAP,INODE) = 0
! paved, tm NWRW
              IF (ikind  .EQ. IMAP .and. imap .le. 7) IXLMAP(IMAP,IIntVolgnr) = Iextnr
! balans, salt: all nodes
              IF (IMAP .EQ. 8 .OR. IMAP .EQ. 9)       IXLMAP(IMAP,INODE) = IExtnr
! RWZI
              IF (IMAP .EQ. 10 .AND. ikind .EQ. 14)   IXLMAP(IMAP,IIntVolgnr) = IExtnr
! Industry
              IF (IMAP .EQ. 11 .AND. ikind .EQ. 15)   IXLMAP(IMAP,IIntVolgnr) = IExtnr
! Sacramento
              IF (IMAP .EQ. 12 .AND. ikind .EQ. 16)   IXLMAP(IMAP,IIntVolgnr) = IExtnr
! Cel
              IF (IMAP .EQ. 14 .AND. ikind .EQ. 17)   IXLMAP(IMAP,IIntVolgnr) = IExtnr
! RR Runoff
              IF (IMAP .EQ. 15 .AND. ikind .GE. 18 .and. ikind .le. 20)   IXLMAP(IMAP,IIntVolgnr) = IExtnr  ! EXTR, SCS, HBV
              IF (IMAP .EQ. 15 .AND. ikind .eq. 22)   IXLMAP(IMAP,IIntVolgnr) = IExtnr        ! LGSI
              IF (IMAP .EQ. 15 .AND. ikind .eq. 23)   IXLMAP(IMAP,IIntVolgnr) = IExtnr        ! WagMod
              IF (IMAP .EQ. 15 .AND. ikind .eq. 31)   IXLMAP(IMAP,IIntVolgnr) = IExtnr        ! NAM
            Endif
         ENDDO
      ENDDO

! Link flows map
      Imap = 13
      DO Ilink=1,NClink
         IExtnr = EILINK (Ilink)
         IXLMAP (IMAP,Ilink) = IExtNr
      ENDDO
!
      IF (iDebug /= 0) THEN
        DO IMAP=1,NKAART
          WRITE(IDEBUG,*) ' IMAP', IMAP,' NLCMAP',NLCMAP(IMAP)
          DO I=1,NLCMAP(IMAP)
             WRITE(IDEBUG,*) ' IXLMAP(I)',I,IXLMAP(IMAP,I)
          ENDDO
        ENDDO
      ENDIF
!
!
      RETURN
      END
