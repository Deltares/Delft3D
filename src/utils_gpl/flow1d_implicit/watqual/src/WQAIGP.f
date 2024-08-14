      subroutine wqaigp ( ngrid  ,af     ,afs    ,igp    ,
     +                    isecfr ,isecto ,exarea )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQAIGP (Water Quality Area In GridPoint)
c
c Module description: This routine calculates the exchange area in a
c                     particular gridpoint/section.
c
c                     The routine calculates two areas (A1 and A2) and
c                     determines the minimum of these values. This mini-
c                     mum is the exchange area in the gridpoint/section.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 af                P  -
c  3 afs               P  -
c  7 exarea            O  Calculated exchange area.
c  4 igp               P  -
c  5 isecfr            P  -
c  6 isecto            P  -
c  1 ngrid             I  Number of grid points in network.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqgpar  Water Quality GridPoint ARea
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
c $Log: wqaigp.pf,v $
c Revision 1.3  1999/03/15  15:53:45  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:08:19  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:40  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:20  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:26  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer  igp    ,isecfr ,isecto ,ngrid
      real     exarea
      real     af  (ngrid),
     +         afs (ngrid,2)

c
c     Variables
c
      real     area1 ,area2

c
c     Calculate "from" area
c
      call wqgpar ( ngrid  ,af     ,afs    ,igp    ,
     +              isecfr ,area1  )
c
c     Calculate "to" area
c
      call wqgpar ( ngrid  ,af     ,afs    ,igp    ,
     +              isecto ,area2  )

c
c     Exchange area is minimum surface of "from" and "to" area
c
      exarea = min ( area1, area2 )
        
      return
      end
