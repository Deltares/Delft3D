      subroutine sedise (sedbr  ,maxtab ,ntabm  ,stot  ,qtot  ,
     &                   sdrdbf ,ntab   ,table  ,dissdb )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SEDISE (SEdiment DIstribute SEdredge)
c
c Module description: Distribute sediment for a Sedredge nodal
c                     gridpoint.
c
c                     This routine distributes the total incoming sedi-
c                     ment transport of a sedredge branch over the left
c                     and right channel.
c
c Precondition:       The total incoming transport en discharge in the
c                     branch have signs corresponding to the branch
c                     direction. Coefficient ALPHA-s is positive.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 dissdb(2)         O  (1) distributed sediment in left channel
c                         (2) distributed sediment in right channel
c  2 maxtab            I  Maximum number of defined tables.
c  7 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                         to maxtab. For a specific table number k and
c                         function Y = f (X) the following definitions
c                         exist:
c                         (1,k) = Length of table k.
c                         (2,k) = Start address X in table.
c                         (3,k) = Start address Y in table.
c                         (4,k) = Access method and period control: xy
c                                 x = ctbnpf (0) : No period defined
c                                 x = ctbpfu (1) : Period defined
c                                 y = ctbico (0) : Continue interpltn
c                                 y = ctbidi (1) : Discrete interpltn
c  3 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  5 qtot              I  Total discharge flowing towards node.
c  6 sdrdbf(2,nsedrd)  I  Defines for each sedredge branch the pointer
c                         to the sediment distribution function and the
c                         flow distribution function.
c                         (1,j) = Table pointer Q distribution table for
c                                 sedredge branch in case the sedredge
c                                 branch starts at an internal node. If
c                                 the sedredge branch starts at a boun-
c                                 dary the table pointer points to a
c                                 table with boundary condition for the
c                                 right channel.
c                         (2,j) = Table pointer S distribution table for
c                                 sedredge branch j.
c  1 sedbr             I  Sedredge branch number
c  4 stot              I  Total sediment transport going towards node.
c  8 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c inttab  INTerpolate in TABle
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sedise.pf,v $
c Revision 1.3  1995/10/18  09:00:38  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:07:19  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:21  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:47:34  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:34:45  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:20  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    sedbr ,maxtab,ntabm
      integer    sdrdbf(2,*)      ,ntab(4,maxtab)
      real       stot  
      real       table (ntabm)    ,dissdb(2)
	double precision qtot
c
c     Declaration of local parameters
c
      integer    itab
      real       alphas
c
      itab = sdrdbf(2,sedbr)
c
c     Interpolate in table and get alpha-s = f(Q)
c
      call inttab (ntab (1,itab)      ,ntab(4,itab),
     &             table(ntab(2,itab)),
     &             table(ntab(3,itab)),
     &             dble(qtot)  ,alphas    )
c
      dissdb(1) = stot * alphas
      dissdb(2) = stot * (1.-alphas)
c
      end
