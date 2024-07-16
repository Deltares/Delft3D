      subroutine gsdunl (lenopt ,depth  ,duncof ,dunele )

c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsdunl.F,v $
c Revision 1.2  1995/09/27  10:12:25  kuipe_j
c Maintenance
c
c
c***********************************************************************
c     Graded Sediment Calculation of DUNe Lenght

c
c     Declaration of parameters
c
      real       duncof(*)
      integer    lenopt
      real       depth  ,dunele
c
c     Declaration of local parameters
c
      real       factor (2)
c                        Yalin  Van Rijn
      data       factor /5.5   ,7.3     /

      if (lenopt .le. 2) then
         dunele = factor(lenopt) * depth
      else
         dunele = duncof(3)
      endif

      end
