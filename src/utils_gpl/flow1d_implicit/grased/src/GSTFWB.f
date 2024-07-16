      subroutine gstfwb (initra ,nfrac  ,pacfac ,dmed   ,velo  ,wacofb ,
     &                   sedtra ,sedexp )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSTFWB (Grad Sed Transp WaalBocht )
c
c Module description: Calculate the sediment transport according to
c                     Waalbocht formulae.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 chezy             I  Chezy value
c  5 dmed              I  Dmedium
c  2 g                 I  Acceleration of gravity.
c  3 pacfac            I  packing factor (porosity)
c 11 sedtra            O  calculated sediment transport
c  8 velo              I  velocity (without sign)
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gstfwb.F,v $
c Revision 1.3  1996/06/07  11:56:39  kuipe_j
c multi  +  fixed layer
c
c Revision 1.2  1995/09/27  10:12:59  kuipe_j
c Maintenance
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nfrac
      real       pacfac ,dmed   ,velo  ,sedexp
      real       wacofb (3)     ,
     &           sedtra (nfrac)
c
      logical    initra
c
c     Declaration of local parameters
c
      integer    i
      real       m      ,n      ,l     ,s
c
c     Calculation of transport.
c     Start with the computation of fraction independent constants
c
      m = wacofb(1)
      n = wacofb(2)
      l = wacofb(3)
c
      sedexp = n / 2.
      if (initra) return

      s = m * velo**n / (dmed**l * (1. - pacfac))

      do 10 i=1,nfrac
         sedtra(i) = s
   10 continue

      end
