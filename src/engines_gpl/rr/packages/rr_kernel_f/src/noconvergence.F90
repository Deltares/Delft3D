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

  Subroutine NoConvergence (Iout1, Itmstp, Warn4, NrTimestepsNoConvergence, Iter, EPSCRI)

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
!***  This subroutine gives a message in case of no convergence of computations
!*********************************************************************
!*** Input/output parameters:     none.
!*** ------------------------
!*********************************************************************
!*** ADRESS     : Deltares,
!***              P.O.BOX 177,
!***              2600 MH DELFT, THE NETHERLANDS
!**********************************************************************

  USE CONF_FIL
  USE CONF_ARR
  use Network
! use Openwater
  use Balance
  use ParallelData

!*** OTHER DATA

  IMPLICIT NONE !!!!!!!!!

  REAL          EPSCRI, EPS, eps2, epslocal
  Integer       Iout1, Itmstp, Iter
  LOGICAL       WARN4
  Integer       NrTimestepsNoConvergence

! local variable
  Integer       Iow
! This subroutine gives a message in case of no convergence of computations
! Updates the number of timesteps with no convergence

      WARN4 = .FALSE.
      EPSlocal = EPSCRI * 100.0
!     if (CheckBalance) write(iobal,*) '  balansfout openwater outflows  inflow unpaved  inflow struct  total'
      DO IOW =1,NCOW
        EPS = ABS ( QOUT0(IOW) - QOUTOW(IOW) )
        EPS2= MAX ( ABS(QOUT0(IOW)), ABS(QOUTOW(IOW)) )
        IF (EPS .GT. EPSLocal * EPS2) WARN4=.TRUE.
        EPS = ABS ( QIN0(IOW,2) - QINOW(IOW,2) )
        EPS2= MAX ( ABS(QIN0(IOW,2)), ABS(QINOW(IOW,2)) )
        IF (EPS .GT. EPSLocal * EPS2) WARN4=.TRUE.
        EPS = ABS ( QIN0(IOW,4) - QINOW(IOW,4) )
        EPS2= MAX ( ABS(QIN0(IOW,4)), ABS(QINOW(IOW,4)) )
        IF (EPS .GT. EPSLocal * EPS2) WARN4=.TRUE.
        EPS = ABS ( QIN0(IOW,5) - QINOW(IOW,5) )
        EPS2= MAX ( ABS(QIN0(IOW,5)), ABS(QINOW(IOW,5)) )
        IF (EPS .GT. EPSLocal * EPS2) WARN4=.TRUE.
        EPS = ABS ( QIN0(IOW,6) - QINOW(IOW,6) )
        EPS2= MAX ( ABS(QIN0(IOW,6)), ABS(QINOW(IOW,6)) )
        IF (EPS .GT. EPSLocal * EPS2) WARN4=.TRUE.
        Eps = Qout0(iow)-QoutOw(iow) + QIN0(IOW,2)-QINOW(IOW,2) + QIN0(IOW,4)-QINOW(IOW,4) + &
                                           QIN0(IOW,5)-QINOW(IOW,5) + QIN0(IOW,6)-QINOW(IOW,6)
!        if (CheckBalance .and. abs(eps) .gt. 1E-5) &
!            write(iobal,'(A,I5,6F10.6)') '  iow=',iow, Qout0(iow)-QoutOw(iow), &
!                             QIN0(IOW,2)-QINOW(IOW,2), QIN0(IOW,4)-QINOW(IOW,4), &
!                             QIN0(IOW,5)-QINOW(IOW,5), QIN0(IOW,6)-QINOW(IOW,6), Eps
      ENDDO
      IF (WARN4 .AND. ITER .GE. MAXITR) THEN
         NrTimestepsNoConvergence = NrTimestepsNoConvergence + 1

      ENDIF



  Return
  END Subroutine NoConvergence

