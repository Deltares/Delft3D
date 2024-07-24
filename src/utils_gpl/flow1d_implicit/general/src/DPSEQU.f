      function DPSEQU (dvar1, dvar2, eps)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             DPSEQU (EQUal test with Double precision interval EPSilon)
c
c Module description: Logical function to check if the difference be-
c                     tween two double precision values is lower than a
c                     defined interval epsilon.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  0 dpsequ            O  -
c  1 dvar1             I  Double precision variable.
c  2 dvar2             I  Double precision variable.
c  3 eps               I  Interval epsilon.
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
c $Log: dpsequ.pf,v $
c Revision 1.1  1995/09/22  10:02:56  kuipe_j
c variable dimensions, new headers
c
c
c***********************************************************************
c
c     Declaration of function:
c
      logical DPSEQU
c
c     Declaration of parameters:
c
      double precision  dvar1, dvar2, eps
c
      DPSEQU = abs(dvar1 - dvar2) .lt. eps
      end
