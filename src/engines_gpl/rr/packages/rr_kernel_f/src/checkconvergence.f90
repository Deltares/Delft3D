!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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

  Subroutine CheckConvergence  (Epscri, Epscr2, Iter, Converged)

!*********************************************************************
!***                D E L F T         H Y D R A U L I C S
!***
!***              WATER RESOURCES AND ENVIRONMENT DIVISION
!*********************************************************************
!*** Program :  DELFT-3B version 2.00                Date: Nov   1996
!*** Module  :
!*********************************************************************
!*** Created    : March  1995                     By : Geert Prinsen
!*********************************************************************
!*** Last update: 19 February 1997                 By: Peter Schrier
!*********************************************************************
!*** Brief description:
!*** ------------------
!***  This subroutine checks convergence of NODELP computations, stores data
!*********************************************************************
!*** Input/output parameters:     none.
!*** ------------------------
!*********************************************************************
!*** ADRESS     : Deltares,
!***              P.O.BOX 177,
!***              2600 MH DELFT, THE NETHERLANDS
!**********************************************************************

  use Boundary
  USE CONF_FIL
  USE CONF_ARR
  use Crop
  use Structures
  use Greenhouse
  use Messages
  use Network
  use NWRW
  use Openwater
! use Output
  use Paved
  use Unpaved
  use RWZI
  use Industry
  use Balance
  use ParallelData
  use Output

!*** OTHER DATA

  IMPLICIT NONE

  REAL          EPSCRI, EPS, epsCr2

  Integer       teller1, ikind
! Idebug
  Integer       iter, iOW, iNode, iStr, nodeUp, noDown, iOwD
  LOGICAL       Converged

!*********************************************************************
!***     Loop over all configuration nodes
!***       met convergentiecriterium op outflow open water
!***                         huidige iteratie QOUTOW en vorige iteratie QOUT0
!***           ook check op QIN0 (.,2) van onverhard gebied
!***           ook check op QIN0 (.,4) van structures
!***       iteratief!
!*********************************************************************
! ESPCRI gebruiken voor relative check, EPSCR2 voor absolute check,


        Idebug = ConfFil_get_iDebug()

!  EpsCr2 is al op hoger nivo gezet
!       EpsCr2 = 0.000001
!  dus hier niet meer

        if (idebug .ne. 0) Write(Idebug,*) ' CheckConvergence  Converged=',Converged
        DO INode =1,NCnode
           IKind = EiNode(inode,3)
           If (ikind .eq. 4) then
             Iow = EiNode(inode,2)
             EPS = ABS ( QOUT0(IOW) - QOUTOW(IOW) )
             if (idebug .ne. 0) Write(Idebug,*) ' Epscri1 etc', EpsCri, Epscr2, Eps, Qoutow(iow)
             IF (EPS .GT. EPSCRI*ABS(QOUT0(IOW)) .AND. ABS(QOUTOW(IOW)) .GT. EPSCR2) then
                Converge (inode) = Converge(inode) + 1
                Converged = .false.
             endif
             if (idebug .ne. 0) Write(Idebug,*) ' CheckConvergence  Converged IOW=',IOW, Converged
! check unpaved
             EPS = ABS ( QIN0(IOW,2) - QINOW(IOW,2) )
             if (idebug .ne. 0) Write(Idebug,*) ' Epscri2 etc', EpsCri, Epscr2, Eps, QINOW(iow,2)
             IF (EPS .GT. EPSCRI*ABS(QIN0(IOW,2)) .AND. ABS(QINOW(IOW,2)) .GT. EPSCR2) then
               Converged = .false.
               Converge (inode) = Converge(inode) + 1
             endif
             if (idebug .ne. 0) Write(Idebug,*) ' CheckConvergence  Converged IOW=',IOW, Converged
             EPS = ABS ( QIN0(IOW,4) - QINOW(IOW,4) )
! check structures
             if (idebug .ne. 0) Write(Idebug,*) ' Epscri3 etc', EpsCri, Epscr2, Eps, QINOW(iow,4)
             IF (EPS .GT. EPSCRI*ABS(QIN0(IOW,4)) .AND. ABS(QINOW(IOW,4)) .GT. EPSCR2) then
                Converged = .false.
                Converge (inode) = Converge(inode) + 1
             endif
             if (idebug .ne. 0) Write(Idebug,*) ' CheckConvergence  Converged IOW=',IOW, Converged
! check industry
             EPS = ABS ( QIN0(IOW,6) - QINOW(IOW,6) )
             if (idebug .ne. 0) Write(Idebug,*) ' Epscri4 etc', EpsCri, Epscr2, Eps, QINOW(iow,4)
             IF (EPS .GT. EPSCRI*ABS(QIN0(IOW,6)) .AND. ABS(QINOW(IOW,6)) .GT. EPSCR2) then
                Converged = .false.
                Converge (inode) = Converge(inode) + 1
             endif
             if (idebug .ne. 0) Write(Idebug,*) ' CheckConvergence  Converged IOW=',IOW, Converged
           Endif
        Enddo

        If (ITER .LT. MAXITR .and. .not. Converged) THEN
!***      Berekeningen nog niet geconvergeerd;
!***      Store flows to/from open water, structures last iteration
! Jan 96 structures
          DO INODE=1,NCNODE
            IF (EiNode(INODE,3) .EQ. 5) THEN
              ISTR = EiNode(INODE,2)
              NODEUP = UPNODE(INODE)
              QSTRU01(ISTR) = QSTRU1(ISTR)
              QSTRU02(ISTR) = QSTRU2(ISTR)
              IF (EiNode(NODEUP,3) .EQ. 4) THEN
                 IOW = EiNode(NODEUP,2)
                 QOWUPR(ISTR) = QOUTOW(IOW) - QSTRU(ISTR)
              ELSE
                 QOWUPR(ISTR) = 0.0
              ENDIF
              NODOWN = DONODE(INODE)
              IF (EiNode(NODOWN,3) .EQ. 4)  THEN
                 IOWD = EiNode(NODOWN,2)
                 QOWDWR(ISTR) = QINOW(IOWD,4) - QSTRU(ISTR)
              ELSE
                 QOWDWR(ISTR) = 0.0
              ENDIF
            ENDIF
          ENDDO
!open water
          DO IOW =1,NCOW
            QIN0 (IOW,2) = QINOW(IOW,2)
            QIN0 (IOW,4) = QINOW(IOW,4)
            QIN0 (IOW,5) = QINOW(IOW,5)
            QIN0 (IOW,6) = QINOW(IOW,6)
            QINOW(IOW,2) = 0.0
            QINOW(IOW,4) = 0.0
            QINOW(IOW,5) = 0.0
            QINOW(IOW,6) = 0.0        ! Ook Industry in de iteratieloop meenemen
          ENDDO
!         Vector/Array manipulations
          QOUT0  = QOUTOW
          QOUTOW = 0.0
!Nov 2001 Industry
          QIndAll0 = QIndAll
          QIndReturnFlow0 = QIndReturnFlow

!*** Reset boundary flows via structures
!***        flows via structure worden opnieuw via iteratie bepaald)
          DO INODE=1,NCNODE
             IF (EiNode(INODE,3) .EQ. 5) THEN
                NODOWN = DONODE(INODE)
                NODEUP = UPNODE(INODE)
                IF (EiNode(NODOWN,3) .EQ. 6) THEN
                   ISTR = EiNode(INODE,2)
                   teller1 = EiNode(NODOWN,2)
                   QBND (teller1) = QBND(teller1) - QSTRU(ISTR)
                ELSEIF (EiNode(NODEUP,3) .EQ. 6) THEN
                   ISTR = EiNode(INODE,2)
                   teller1 = EiNode(NODEUP,2)
                   QBND (teller1) = QBND(teller1) + QSTRU(ISTR)
                ENDIF
              ENDIF
          ENDDO  ! of boundary via structures

!!! Reset flows to boundaries which will be updated in iteration loop:
!!! flows to/from unpaved area;
!!! flows to/from industrial area (ivm possible differences in demand and allocation)
!!! flows to/from structures (partly done above already).
!         Vector/Array manipulations
          QBND = QBND - QINBND%totalUnpaved - QINBND%totalIndustry -QINBND%totalRWZI
          QINBND%totalIndustry  = 0.0
          QINBND%totalRWZI      = 0.0
          QINBND%totalUnpaved   = 0.0
          QINBND%totalStructure = 0.0

! ARS 12563
          QInPluv = QinPluv - QPluv%totalUnpaved  - QPluv%totalIndustry -QPluv%totalRWZI
          QPluv%totalUnpaved   = 0.0
          QPluv%totalIndustry  = 0.0
          QPluv%totalRWZI      = 0.0
          QPluv%totalStructure = 0.0

        EndIf  ! of maxiter check


    Return
    END Subroutine CheckConvergence

