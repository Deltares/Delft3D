      function safrez (nqfloc ,ngrid ,qfloc ,qq )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SAFREZ (SAlt FREsh water discharge Zwendl)
c
c Module description: Summation of fresh water discharges at all locati-
c                     ons that belong to a mouth (ZWENDL dispersion
c                     formulation).
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 ngrid             I  Number of grid points in network.
c  1 nqfloc            I  Number of Q fresh locations (empirical formu-
c                         lation of dispersion).
c  3 qfloc(2,nqfloc)   I  Locations and signs of fresh water discharges
c                         (Empirical formulation of dispersion):
c                         (1,i) = Fresh water discharge location i (grid
c                                 point).
c                         (2,i) = Sign of fresh water discharge location
c                                 i (-1 or 1).
c  4 qq(ngrid)         I  Discharge in every grid point.
c  0 safrez            O  Fresh water discharge at mouth.
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
c $Log: safrez.pf,v $
c Revision 1.2  1995/05/30  07:06:04  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:46  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:13  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer  nqfloc    ,ngrid
      real     safrez
      real     qfloc(2,*)
      double precision qq(ngrid) 
c
c     Declaration of local parameters
c
      integer  ifl   ,igr
      real     q
c
      q = 0.
      do 10 ifl = 1,nqfloc
         igr = int(qfloc(1,ifl))
         q   = q + qq(igr) * qfloc(2,ifl)
   10 continue
      safrez = q
c
      end
