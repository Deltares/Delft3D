      function FLLWST(ncsrel ,cnstrl ,contnr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLLWST (FLow LoWest Structure)
c
c Module description: Determine lowest related structure for a given
c                     controller.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 cnstrl(2,ncsrel)  I  table for controller structure relations.
c                         (1,i) = Controller number of controller
c                                 related to structure at (2,i)
c                         (2,i) = Structure number of structure related
c                                 to controller at (1,i)
c  3 contnr            I  Controller number.
c  0 fllwst            O  Function value of function FLLWST (=Lowest
c                         related structure number for a given
c                         controller).
c  1 ncsrel            I  number of controller structure relations.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: fllwst.pf,v $
c Revision 1.3  1996/05/30  09:56:38  kuipe_j
c general structure dlim, controllers
c
c Revision 1.2  1995/09/22  10:01:53  kuipe_j
c variable dimensions, new headers
c
c
c
c***********************************************************************
c
c     Function declaration:
c
      integer FLLWST
c
c     Declaration of parameters:
c
      integer ncsrel, contnr
      integer cnstrl(2,*)
c
c     Declaration of local variables:
c
      integer irel, strunr
c
c     Determine lowest related structure
c
      strunr = 1000000
      do 10 irel = 1, ncsrel
         if ( cnstrl(1,irel) .eq. contnr ) then
            strunr = min(cnstrl(2,irel), strunr)
         endif
   10 continue
c
      FLLWST = strunr
      end
