      function EPSEQU (rvar1, rvar2, eps)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             EPSEQU (EQUal test with interval EPSilon)
c
c Module description: Logical function to check if the difference be-
c                     tween two real values is lower than a defined
c                     interval epsilon.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 eps               I  Interval epsilon.
c  0 epsequ            O  Function value, TRUE/FALSE.
c  1 rvar1             I  Real variable.
c  2 rvar2             I  Real variable.
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
c $Log: epsequ.pf,v $
c Revision 1.2  1995/05/30  07:02:22  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:25  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:59  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of function:
c
      logical EPSEQU
c
c     Declaration of parameters:
c
      real    rvar1, rvar2, eps
c
      EPSEQU = abs(rvar1 - rvar2) .lt. eps
      end
