      subroutine KAERES(nbran  ,ngrid  ,branch ,h1     ,h      ,
     +                  theta2 ,maxtab ,ntabm  ,ntab   ,table  ,
     +                  x      ,nexres ,exres  ,eta    ,detadh )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAERES (KAlman Extra RESistance)
c
c Module description: Calculate derivative of extra resistance and extra
c                     resistance ETA in network.
c
c                     The derivative of the extra resistance is deter-
c                     mined by numerical differentiation.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 15 detadh(ngrid)     O  Derivative of extra resistance coefficient ETA
c                         to waterlevel in every grid point i+1/2 on time
c                         n+1/2 (d(ETA)/dh). Value at i+1/2 is stored at
c                         index i.
c 14 eta(ngrid)        I  Extra resistance coefficient ETA in every grid
c                         point i+1/2 on time n+1/2. Value at i+1/2 is
c                         stored at index i.
c 13 exres(3,nexres)   I  Extra resistance definition.
c                         (1,i) = Branch number.
c                         (2,i) = X-coordinate.
c                         (3,i) = Table pointer to function of water
c                                 level.
c  5 h(ngrid)          I  Contains water levels in every grid point. It is
c                         stored on index 2 of the packed array hpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c  4 h1(ngrid)         I  Water level in every grid point at time t(n).
c  7 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c 12 nexres            I  Number of extra resistance points.
c  2 ngrid             I  Number of grid points in network.
c  9 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c  8 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 10 table             P  -
c  6 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c 11 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c inttab  INTerpolate in TABle
c=======================================================================
c
c     Declaration of Parameters:
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kaeres.pf,v $
c Revision 1.4  1999/03/15  15:51:42  kuipe_j
c tabs removed
c
c Revision 1.3  1997/06/04  11:18:14  kuipe_j
c Initialize arrays
c
c Revision 1.2  1996/04/12  13:04:48  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:24  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer maxtab, ntabm, ntab(4,maxtab),
     +        nbran, branch(4,nbran),
     +        ngrid, nexres
      real    theta2
      real    table(ntabm), exres(3,*),
     +        x(ngrid), eta(ngrid), detadh(ngrid)

      double precision  h1(ngrid), h(ngrid)

c
c     Declaration of local variables:
c
      integer i,    i1,   i2,   ibr,   j, itab
      real    hact, etaacc, dh
c
      parameter (dh = 0.001)
c
c     Loop over branches
c
      do 100 i = 1, ngrid
         detadh(i) = 0.
         eta(i)    = 0.
  100 continue

      if (nexres .gt. 0) then
c
         do 400 ibr = 1, nbran
c
c           i1 = global grid point number at node n1 of branch
c           i2 = global grid point number at node n2 of branch
c
            i1 = branch (3,ibr)
            i2 = branch (4,ibr)
c
c           Loop over grid points in branch
c
            do 300 i = i1, i2-1
               do 200 j = 1, nexres
                  if ( int(exres(1,j)) .eq. ibr ) then
c
c                    Compute extra resistance in this branch
c                    Resistance as function of h at (i+1/2,n+theta)
c                    Resistance as function of h at (i+1/2,n+theta)+dh
c
                     if ( exres(2,j) .ge. x(i) .and.
     +                    exres(2,j) .le. x(i+1) ) then
                        itab = int(exres(3,j))
                        hact = theta2*(h(i)+h(i+1))/2. +
     +                         (1.-theta2)*(h1(i)+h1(i+1))/2
c
c                       Interpolate value for eta
c
                        call INTTAB (ntab(1,itab), ntab(4,itab),
     +                               table(ntab(2,itab)),
     +                               table(ntab(3,itab)),
     +                               dble(hact)  , eta(i) )
c
c                       Interpolate value for eta'
c
                        call INTTAB (ntab(1,itab), ntab(4,itab),
     +                               table(ntab(2,itab)),
     +                               table(ntab(3,itab)),
     +                               dble(hact+dh)      , etaacc)
                        detadh(i) = (etaacc - eta(i)) / dh
                     endif
                  endif
  200          continue
  300       continue
  400    continue
      endif
      return
c
      end
