      subroutine flqasm (nbran  ,ngrid  ,branch ,grid ,hq1  ,hq2)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLQASM (FLow QqA will be SMoothed)
c
c Module description: Smoothing of parameter Q * Q/A in all interior
c                     grid points, structure cells excluded
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  4 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c  5 hq1(ngrid)        IO (to be) smoothed array
c  6 hq2(ngrid)        IO scratch array
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flqasm.pf,v $
c Revision 1.3  1999/03/15  15:50:29  kuipe_j
c tabs removed
c
c Revision 1.2  1995/10/18  08:59:24  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.1  1995/09/29  10:36:16  kuipe_j
c Improvement of autostart and simple weir
c
c
c***********************************************************************
c
c
c     Declaration of parameters
c
      integer    nbran ,ngrid
      integer    branch(4,nbran), grid(ngrid)
      double precision hq1(ngrid)     ,hq2(ngrid)
c
c     Declaration of local variables
c
      integer    i1  ,i2  ,ibr   ,igr

      do 30 ibr = 1, nbran
         i1 = branch(3,ibr)
         i2 = branch(4,ibr)
         do 10 igr = i1+1, i2-1
            if (grid(igr) .eq. 1 .and. grid(igr-1) .eq. 1) then
               hq2(igr) = .25*hq1(igr-1) + .5*hq1(igr) + .25*hq1(igr+1)
            else
               hq2(igr) = hq1(igr)
            endif
 10      continue
         do 20 igr = i1+1, i2-1
            hq1(igr) = hq2(igr)
 20      continue
 30   continue

      end

