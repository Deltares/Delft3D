function yesmap (start ,stop ,incm ,istep)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Fileio module (interface to nefis)
!
! Programmer:         J.Kuipers
!
! Module:             YESMAP (YES MAP results are written)
!
! Module description: Determine if map results of a module must be writ-
!                     ten to the NEFIS file.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 incm              I  Time step increment for writing maps.
!  4 istep             I  Current time step number (t(n+1)).
!  1 start             I  Starting time step number for writing of maps.
!  2 stop              I  Last time step number for writing of maps.
!  0 yesmap            O  True when map results are written.
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: yesmap.pf,v $
! Revision 1.2  1995/05/30  06:57:19  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:19  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:44  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer  start  ,stop ,incm ,istep
   logical  yesmap
!
   if (incm .ne. 0) then
      yesmap = istep.ge.start .and. istep.le.stop .and.&
      &mod(istep-start,abs(incm)).eq.0
   else
      yesmap=.false.
   endif
!
!     if (yesmap .and. incm .lt. 0) incm = incm*2
!
end
