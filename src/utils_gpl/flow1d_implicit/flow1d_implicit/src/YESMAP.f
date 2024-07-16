      function yesmap (start ,stop ,incm ,istep)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Fileio module (interface to nefis)
c
c Programmer:         J.Kuipers
c
c Module:             YESMAP (YES MAP results are written)
c
c Module description: Determine if map results of a module must be writ-
c                     ten to the NEFIS file.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 incm              I  Time step increment for writing maps.
c  4 istep             I  Current time step number (t(n+1)).
c  1 start             I  Starting time step number for writing of maps.
c  2 stop              I  Last time step number for writing of maps.
c  0 yesmap            O  True when map results are written.
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: yesmap.pf,v $
c Revision 1.2  1995/05/30  06:57:19  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:19  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:44  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer  start  ,stop ,incm ,istep
      logical  yesmap
c
      if (incm .ne. 0) then
            yesmap = istep.ge.start .and. istep.le.stop .and.
     &         mod(istep-start,abs(incm)).eq.0
	else
	      yesmap=.false.
      endif
c
c     if (yesmap .and. incm .lt. 0) incm = incm*2
c
      end
