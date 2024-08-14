      function EQUAL (rvar1, rvar2)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             EQUAL (EQUAL test of two real variables)
c
c Module description: Logical function to check if two real values are
c                     identical.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  0 equal             O  Function value, TRUE/FALSE.
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
c $Log: equal.pf,v $
c Revision 1.2  1995/05/30  07:02:22  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:26  hoeks_a
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
      logical EQUAL
c
c     Declaration of parameters:
c
      real    rvar1, rvar2
c
      EQUAL = rvar1 .eq. rvar2
      end
