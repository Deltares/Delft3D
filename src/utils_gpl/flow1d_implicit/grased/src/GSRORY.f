      subroutine gsrory (d90   ,hrad  ,dunehe ,dunele ,chezy)
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsrory.F,v $
c Revision 1.2  1995/09/27  10:12:47  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Graded Sediment ROughness predictor according to van RIJn
c

c
c     Declaration of parameters
c
      real       d90   ,hrad  ,dunehe ,dunele ,chezy
c
c     Declaration of local parameters
c
      real       ks
c
      ks    = 3. * d90 + 1.1 * dunehe * (1. - exp(-25.*dunehe/dunele))
      chezy = 18. * log10 (12. * hrad / ks)

      end
