      subroutine wqgpfl ( ngrid ,qaggr ,igp   ,isecwq  ,qcalc )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQGPFL (Water Quality GridPoint FLow)
c
c Module description: This routine calculates the flow in a particular
c                     gridpoint/section.
c
c                     The total flow for the requested gridpoint/section
c                     is extracted. When the whole channel is indicated,
c                     including parallel sections, all flows are summed
c                     to one flow.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 igp               I  Gridpoint.
c  4 isecwq            I  Section:
c                         0 = Channel including parallel sections
c                         1 = Main channel
c                         2 = Sub section 1
c                         3 = Sub section 2
c  1 ngrid             I  Number of grid points in network.
c  2 qaggr(ngrid,3)    I  Aggregated flow through main and subsections 1
c                         and 2, using time step of the water quality
c                         process run.
c  5 qcalc             O  Calculated flow value.
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
c $Log: wqgpfl.pf,v $
c Revision 1.3  1999/03/15  15:54:01  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:08:36  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:57  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:28  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c       Parameters
c
      integer   igp,
     +          isecwq,
     +          ngrid

      real      qcalc

      real      qaggr (ngrid,3)

c
c     Determine flow through section
c
      if      (isecwq .eq. 0) then
         qcalc = qaggr (igp,1)
      else if (isecwq .eq. 1) then
         qcalc = qaggr (igp,2)
      else if (isecwq .eq. 2) then
         qcalc = qaggr (igp,3)
      else
         qcalc = qaggr (igp,1) - qaggr (igp,2) - qaggr (igp,3)
      endif

      return
      end
