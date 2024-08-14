      subroutine gslati(lathic ,dunehe ,deffec ,redfac ,defmin)

c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gslati.F,v $
c Revision 1.3  1996/06/07  11:56:31  kuipe_j
c multi  +  fixed layer
c
c Revision 1.2  1995/09/27  10:12:32  kuipe_j
c Maintenance
c
c
c***********************************************************************
c     Graded Sediment calculate effective transport LAyer ThIckness

c     Declaration of parameters
c
      integer    lathic
      real       dunehe ,deffec ,redfac ,defmin

c
c     Declaration of local parameters and constants
c
c                Layer thickness options
c                Ribberink  Karim & Kennedy
      integer    layrib    ,laykar
      parameter (layrib=1  ,laykar=2)

      if (lathic .eq. layrib) then

c        Thickness according to Ribberink

         call gslari (dunehe ,deffec ,redfac ,defmin)

      else if (lathic .eq. laykar) then

c        Thickness according to Karim and Kennedy

      endif

      end
