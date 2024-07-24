      function FLIHIS(ind)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLIHIS (FLow Index renumbering structure HIStory)
c
c Module description: Index renumbering of controll parameters from
c                     array contrl to array strhis.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  0 flihis            O  Function value of function FLIHIS (=index for
c                         array strhis)
c  1 ind               I  index in array contrl.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flihis.pf,v $
c Revision 1.2  1995/09/22  10:01:43  kuipe_j
c variable dimensions, new headers
c
c
c
c***********************************************************************
c
c     Function declaration:
c
      integer FLIHIS
c
c     Declaration of parameter
c
      integer ind
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
      if      ( ind .eq. ccpcrh ) then
         FLIHIS = 2
      else if ( ind .eq. ccpcrw ) then
         FLIHIS = 3
      else if ( ind .eq. ccpgat ) then
         FLIHIS = 1
      endif
c
      end
