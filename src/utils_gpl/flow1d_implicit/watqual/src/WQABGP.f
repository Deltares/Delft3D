      subroutine wqabgp ( ngrid  ,af     ,afs    ,igpfrm ,
     +                    igpto  ,isecwq ,intpol ,exarea
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQABGP (Water Quality Area Between GridPoints)
c
c Module description: This routine calculates the exchange area between
c                     two gridpoints.
c
c                     First the left and right area are calculated and
c                     an interpolation is done to calculate the exchange
c                     area between the gridpoints.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 af                P  -
c  3 afs               P  -
c  8 exarea            O  Calculated exchange area.
c  4 igpfrm            P  -
c  5 igpto             P  -
c  7 intpol            I  Interpolation factor.
c  6 isecwq            P  -
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
c $Log: wqabgp.pf,v $
c Revision 1.2  1995/05/30  07:08:17  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:39  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:26  kuipe_j
c Initial version
c
c
c***********************************************************************
c
C       Parameters
C
      integer  igpfrm ,igpto ,isecwq ,ngrid

      real     intpol ,exarea

      real     af (ngrid),
     +         afs(ngrid,2)

c
c       Variables
c
      real     area1  ,area2

c
c     Calculate "from" area
c
      call wqgpar ( ngrid  ,af     ,afs    ,igpfrm ,
     +              isecwq ,area1  )
c
c     Calculate "to" area
c
      call wqgpar ( ngrid  ,af     ,afs    ,igpto  ,
     +              isecwq ,area2  )

c
c     Exchange area by interpolation between "from" and "to" area
c
      exarea = ( (1.-intpol) * area1 ) + ( intpol * area2 )

      return
      end
