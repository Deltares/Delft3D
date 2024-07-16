      subroutine gslari (dunehe ,deffec, redfac ,defmin)

c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gslari.F,v $
c Revision 1.3  1996/06/07  11:56:29  kuipe_j
c multi  +  fixed layer
c
c Revision 1.2  1995/09/27  10:12:31  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Graded Sediment calculate effective transport LAyer thickness
c     according to Ribberink

c     Declaration of parameters
c
      real       dunehe ,deffec ,redfac ,defmin
c
c     Declaration of local parameters
c
      real       epseff
      parameter (epseff=.5)

      deffec = max(epseff * dunehe, defmin*redfac)

      end
