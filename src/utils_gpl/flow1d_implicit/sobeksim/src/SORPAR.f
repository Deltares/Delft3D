      function SORPAR ( par, ix )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SORPAR (SObek Real PARameter)
c
c Module description: Extract real parameter from run array variable
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 ix                I  Index in array ...run or ...par
c  1 par               I  Parameter array (...par)
c  0 sorpar            O  Function value.
c=======================================================================
c

c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sorpar.pf,v $
c Revision 1.4  1995/09/22  10:04:30  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:57:04  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:58  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:24  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:39:45  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Parameters
c
      real    par(*)
      integer ix
      real    sorpar
c
c     Extract
c
      sorpar =  par(ix)

      return
      end
