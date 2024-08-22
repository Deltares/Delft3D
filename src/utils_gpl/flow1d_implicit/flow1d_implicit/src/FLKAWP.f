      subroutine FLKAWP(nbran  ,ngrid  ,branch ,wfrict ,tauwi  ,pw     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLKAWP (FLow KAlman Wind correction Parameter)
c
c Module description: Correction on wind friction due to uncertain
c                     correction parameter.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  6 pw                I  Uncertain wind stress parameter.
c  5 tauwi(ngrid)      IO Calculated wind friction for each gridpoint.
c  4 wfrict(3,nbran)   I  Wind friction parameters in branch.
c                         (1,i) = Indicates wind defined for branch:
c                                 cnwndf (0) : No wind defined
c                                 cywndf (1) : Wind defined
c                         (2,i) = Table pointer for wind direction as a
c                                 function of time.
c                         (3,i) = Table pointer for wind velocity as a
c                                 function of time.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flkawp.pf,v $
c Revision 1.2  1996/04/12  13:04:03  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:23:35  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Declaration of Parameters:
c
      integer nbran, ngrid
      integer branch(4,nbran), wfrict(3,nbran)
      real    pw(1), tauwi(ngrid)
c
c     Declaration of local variables:
c
      integer i, i1, i2, ibr
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
      do 20 ibr = 1, nbran
         if (wfrict(1,ibr) .eq. cywndf) then
            i1 = branch (3,ibr)
            i2 = branch (4,ibr)
            do 10 i = i1, i2-1
               tauwi(i) = tauwi(i) * pw(1)
   10       continue
         endif
   20 continue
c
      end
