      subroutine FLERES(ngrid  ,
     +                  h1     ,h      ,q1     ,q      ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,x      ,
     +                  nexres ,exres  ,ksi    ,ksip   ,exrstp ,
     +                  omega  ,iter   ,juer   ,ker    ,theta2 )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLERES (FLow Extra RESistance)
c
c Module description: Calculate extra resistance in network.
c
c                     It is possible to define extra resistance in the
c                     network. This resistance is a table of the water
c                     level and is calculated in this routine.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 14 exres(3,nexres)   I  Extra resistance definition.
c                         (1,i) = Branch number.
c                         (2,i) = X-coordinate.
c                         (3,i) = Table pointer to function of water
c                                 level.
c  4 h1(ngrid)         I  Water level in every grid point at time t(n).
c  5 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c 16 juer              P  -
c 17 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 15 ksi(ngrid)        O  Actual extra resistance in grid points i+1/2.
c  8 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c 13 nexres            I  Number of extra resistance points.
c  2 ngrid             I  Number of grid points in network.
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
c  6 q1(ngrid)         I  Discharge in every grid point at time t(n).
c  7 q(ngrid)          I  Discharge in every grid point at the latest
c                         iteration.
c 11 table             P  -
c 18 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c 12 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
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
c $Log: fleres.pf,v $
c Revision 1.10  1999/06/01  13:42:16  kuipe_j
c names in messages substituted + message template
c
c Revision 1.9  1999/03/15  15:49:47  kuipe_j
c tabs removed
c
c Revision 1.8  1996/10/31  10:30:18  kuipe_j
c Extra resistance finished
c
c Revision 1.7  1996/09/03  14:51:54  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.6  1996/04/11  08:23:14  kuipe_j
c Kalman module added
c
c Revision 1.5  1995/09/22  10:01:16  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:10:52  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1994/12/02  13:18:20  kuipe_j
c Prevention against devide by zero.
c
c Revision 1.3  1994/11/28  08:37:26  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:30:49  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:49  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer maxtab, ntabm, ntab(4,maxtab),
     +        ngrid, nexres, exrstp, juer, ker
      real    table(ntabm), exres(3,*),
     +        x(ngrid), ksi(ngrid), ksip(ngrid)
      real    theta2, omega     
      double precision h1(ngrid), h(ngrid), q1(ngrid), q(ngrid)

c
c     Include sobek error code file
c
      include '../include/errcod.i'
c
c     Declaration of local variables:
c
      integer  i,    j,    itab ,igr,   ibr, iter, lbrnam
      real     eta,  dxeta, xc
      double precision hact, qact
      character*40   branam
      character*10   xtxt
      logical  epsequ
      external epsequ
c
c     Loop over branches
c
      do 100 i = 1, ngrid
         ksi(i) = 0.
  100 continue
c
      do 200 j = 1, nexres
c
c        Get extra resistance in this branch from table.
c        Resistance as function of h at (i+1/2,n+theta2)
c        Add all extra resistances of ane grid cell.
c
         i    = int(exres(2,j))
         itab = int(exres(3,j))
         hact = (theta2      * (h(i)+h(i+1)) +
     +           (1.-theta2) * (h1(i)+h1(i+1)))*0.5
c
c        Interpolate value for eta
c
         call INTTAB (ntab(1,itab), ntab(4,itab),
     +                table(ntab(2,itab)),
     +                table(ntab(3,itab)),
     +                dble(hact)  , eta )
c
         ksi(i) = ksi(i) + eta
  200 continue
c
c     Under relaxation of extra resistance coefficient
c
      if (nexres .gt. 0) then
         if (iter .eq. 1) then
            do 300 i = 1,ngrid
               ksip(i) = ksi(i)
  300       continue
         else
            do 400 i = 1,ngrid
               ksi(i)  = omega * ksi(i) + (1.-omega) * ksip(i)
               ksip(i) = ksi(i)
  400       continue
         endif
c
c        Extra resistance type = 'slope'
c        Transform 'ETA' to 'KSI'
c
         if ( exrstp .eq. 0 ) then
            do 500 i = 1,ngrid
               if (.not.EPSEQU(ksi(i),0.0,1.e-8)) then
c
c                 Compute discharge in this cell (i+1/2,n+1/2)
c
                  qact = (theta2      * (q(i)+q(i+1)) +
     +                    (1.-theta2) * (q1(i)+q1(i+1)))*0.5
c
c                 Test for Q=0. In that case eta should be zero.
c
                  dxeta  = ksi(i) * (x(i+1) - x(i))
                  if (abs(qact) .lt. .001) then
                     if (dxeta .lt. .001) then
c
c                       Make ksi zero
c
                        ksi(i) = 0.
                     else
                        igr = i
                        goto 1000
                     endif
                  else
c
c                    Compute ksi
c
                     ksi(i) = dxeta /(qact*abs(qact))
                  endif
               endif
  500       continue
         endif
      endif
c
      return
c
c     Exceptions
c
 1000 continue
c
CC removed as <getloc> and <getbrn> are not accessible
cc   
cc      call getloc (igr,ibr,xc)
cc      write (xtxt,'(f10.2)') xc
cc      call getbrn (ibr,branam,lbrnam)
cc      ker = fatal
cc      call sre_error (juer,'FLERES Extra resistance at branch @'//
cc     +                  branam(:lbrnam)//'@ X= @' //xtxt//
cc     +                 '@ undefined for zero discharge',
cc     +                  eflexr,fatal)
c
      end
