      subroutine KAPP (il     ,ir     ,ngrid  ,istru  ,strclo ,
     +                 strpar ,h2     ,maxtab ,ntabm  ,ntab   ,table  ,
     +                 a2     ,b2     ,c2     ,d2     ,m2     ,ea2    ,
     +                 eb2    ,ec2    ,ed2    ,em2    ,teken  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAPP (KAlman structure PumP)
c
c Module description: In subroutine KAPP the ABCDE coefficients are computed
c                     for a pump.
c
c                     The coefficients are described in S-FO-004 Par 7.5
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 12 a2(ngridm)        IO A2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 13 b2(ngridm)        O  B2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 14 c2(ngridm)        O  C2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 15 d2(ngridm)        O  D2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 17 ea2(ngrid)        O  EA2 right hand side coefficient of momentum
c 18 eb2(ngrid)        O  EB2 right hand side coefficient of momentum
c 19 ec2(ngrid)        O  EC2 right hand side coefficient of momentum
c 20 ed2(ngrid)        O  ED2 right hand side coefficient of momentum
c 21 em2(ngrid)        O  EM2 right hand side coefficient of Q-h relation
c  7 h2(ngrid)         I  Water level in every grid point at time
c                         t(n+1).
c  1 il                P  -
c  2 ir                P  -
c  4 istru             I  Number of structure.
c 16 m2(ngrid)         O  M2 coefficient of Q-h relation
c  8 maxtab            I  Maximum number of defined tables.
c  3 ngrid             I  Number of grid points in network.
c 10 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c  9 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 22 teken             I  Flow direction (+/-).
c  5 strclo(nstru)     I  True if structure is closed.
c  6 strpar            P  -
c 11 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flppar  FLow get PumP ARguments
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
c $Log: kapp.pf,v $
c Revision 1.3  1999/03/15  15:52:04  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:14  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:47  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters:
c
      integer il, ir, istru, ngrid, maxtab, ntabm, ntab(4,maxtab)
      logical strclo(*)
      real    strpar(dmstrpar,*)
      real    table(ntabm)
      real    a2, b2, c2, d2, m2, ea2, eb2, ec2, ed2, em2, teken
      double precision h2(ngrid)
c
c     Declaration of local variables:
c
      integer itab, iup, idown
      real    cap, capold, hstart, hstop, hact, dh
      real    hd, hu, head, f1, f2, dfdh
c
      parameter (dh = 0.001)
c
      call FLPPAR(istru  ,strpar ,ngrid  ,h2     ,il     ,ir     ,
     +            itab   ,cap    ,capold ,hstart ,hstop  ,
     +            iup    ,idown  ,teken  ,hact   )
c
      hu = h2(iup)
      hd = h2(idown)
c
      if ( strclo(istru) ) then
         a2  = 0.
         b2  = -1.
         c2  = 0.
         d2  = 0.
         m2  = 0.
         ea2 = 0.
         eb2 = 0.
         ec2 = 0.
         ed2 = 0.
         em2 = 0.
      else
c
         if ( capold .ge. 1.E-6) then
            head = hu - hd
            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)), dble(head), f1)
            head = hu - hd - dh*teken
            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)), dble(head), f2)
            dfdh = (f1 - f2) / dh*teken
         else
            dfdh = 0.
         endif
c
         a2  = -capold * dfdh
         b2  = 0.
         c2  = -a2
         d2  = 0.
         m2  = 0.
         ea2 = 0.
         eb2 = 0.
         ec2 = 0.
         ed2 = 0.
         em2 = 0.
      endif
      end
