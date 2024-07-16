      subroutine wqfbgp ( ngrid  ,qaggr  ,igpfrm ,igpto  ,
     +                    isecwq ,intpol ,qex    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQFBGP (Water Quality Flow Between GridPoints)
c
c Module description: This routine calculates the exchange flow between
c                     two gridpoints.
c
c                     First the left and right flows are calculated and
c                     an interpolation is done to calculate the exchange
c                     flow between the gridpoints. The sign of the cal-
c                     culated exchange flow will be changed if the
c                     pointer direction is opposite to the branch direc-
c                     tion.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 igpfrm            I  Gridpoint from.
c  4 igpto             I  Gridpoint to.
c  6 intpol            I  Interpolation factor.
c  5 isecwq            P  -
c  1 ngrid             I  Number of grid points in network.
c  2 qaggr             P  -
c  7 qex               IO Calculated exchange flow.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqgpfl  Water Quality GridPoint FLow
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
c $Log: wqfbgp.pf,v $
c Revision 1.3  1999/03/15  15:53:52  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:08:26  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:46  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:32  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:27  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c       Parameters
c
      integer  ngrid,
     +         igpfrm,
     +         igpto,
     +         isecwq

      real     intpol, qex

      real     qaggr (ngrid,3)

c
c     Variables
c
      real     q1, q2
c
c     Calculate flow on left side
c
      call wqgpfl ( ngrid  ,qaggr  ,igpfrm ,
     +              isecwq ,q1     )
c
c     Calculate flow on right side
c
      call wqgpfl ( ngrid  ,qaggr  ,igpto ,
     +              isecwq ,q2     )
c
c     Calculate exchange flow by interpolation
c
      qex = ( (1.-intpol) * q1 ) + ( intpol * q2 )
c
c     Change sign if from -> to is negative
c
      if (igpto .lt. igpfrm) then
         qex = -qex
      endif
c
      return
      end
