subroutine FLERES(ngrid  ,&
&h1     ,h      ,q1     ,q      ,&
&maxtab ,ntabm  ,ntab   ,table  ,x      ,&
&nexres ,exres  ,ksi    ,ksip   ,exrstp ,&
&omega  ,iter   ,juer   ,ker    ,theta2 )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLERES (FLow Extra RESistance)
!
! Module description: Calculate extra resistance in network.
!
!                     It is possible to define extra resistance in the
!                     network. This resistance is a table of the water
!                     level and is calculated in this routine.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 14 exres(3,nexres)   I  Extra resistance definition.
!                         (1,i) = Branch number.
!                         (2,i) = X-coordinate.
!                         (3,i) = Table pointer to function of water
!                                 level.
!  4 h1(ngrid)         I  Water level in every grid point at time t(n).
!  5 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
! 16 juer              P  -
! 17 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 15 ksi(ngrid)        O  Actual extra resistance in grid points i+1/2.
!  8 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
! 13 nexres            I  Number of extra resistance points.
!  2 ngrid             I  Number of grid points in network.
! 10 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
!  9 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  6 q1(ngrid)         I  Discharge in every grid point at time t(n).
!  7 q(ngrid)          I  Discharge in every grid point at the latest
!                         iteration.
! 11 table             P  -
! 18 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
! 12 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! inttab  INTerpolate in TABle
!=======================================================================
!
!     Declaration of Parameters:
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: fleres.pf,v $
! Revision 1.10  1999/06/01  13:42:16  kuipe_j
! names in messages substituted + message template
!
! Revision 1.9  1999/03/15  15:49:47  kuipe_j
! tabs removed
!
! Revision 1.8  1996/10/31  10:30:18  kuipe_j
! Extra resistance finished
!
! Revision 1.7  1996/09/03  14:51:54  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.6  1996/04/11  08:23:14  kuipe_j
! Kalman module added
!
! Revision 1.5  1995/09/22  10:01:16  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:10:52  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.4  1994/12/02  13:18:20  kuipe_j
! Prevention against devide by zero.
!
! Revision 1.3  1994/11/28  08:37:26  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:30:49  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:49  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer maxtab, ntabm, ntab(4,maxtab),&
   &ngrid, nexres, exrstp, juer, ker
   real    table(ntabm), exres(3,*),&
   &x(ngrid), ksi(ngrid), ksip(ngrid)
   real    theta2, omega
   double precision h1(ngrid), h(ngrid), q1(ngrid), q(ngrid)

!
!     Include sobek error code file
!
   include '../include/errcod.i'
!
!     Declaration of local variables:
!
   integer  i,    j,    itab ,igr,   iter
   real     eta,  dxeta
   double precision hact, qact
   logical  epsequ
   external epsequ
!
!     Loop over branches
!
   do 100 i = 1, ngrid
      ksi(i) = 0.
100 continue
!
   do 200 j = 1, nexres
!
!        Get extra resistance in this branch from table.
!        Resistance as function of h at (i+1/2,n+theta2)
!        Add all extra resistances of ane grid cell.
!
      i    = int(exres(2,j))
      itab = int(exres(3,j))
      hact = (theta2      * (h(i)+h(i+1)) +&
      &(1.-theta2) * (h1(i)+h1(i+1)))*0.5
!
!        Interpolate value for eta
!
      call INTTAB (ntab(1,itab), ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &dble(hact)  , eta )
!
      ksi(i) = ksi(i) + eta
200 continue
!
!     Under relaxation of extra resistance coefficient
!
   if (nexres .gt. 0) then
      if (iter .eq. 1) then
         do 300 i = 1,ngrid
            ksip(i) = ksi(i)
300      continue
      else
         do 400 i = 1,ngrid
            ksi(i)  = omega * ksi(i) + (1.-omega) * ksip(i)
            ksip(i) = ksi(i)
400      continue
      endif
!
!        Extra resistance type = 'slope'
!        Transform 'ETA' to 'KSI'
!
      if ( exrstp .eq. 0 ) then
         do 500 i = 1,ngrid
            if (.not.EPSEQU(ksi(i),0.0,1.e-8)) then
!
!                 Compute discharge in this cell (i+1/2,n+1/2)
!
               qact = (theta2      * (q(i)+q(i+1)) +&
               &(1.-theta2) * (q1(i)+q1(i+1)))*0.5
!
!                 Test for Q=0. In that case eta should be zero.
!
               dxeta  = ksi(i) * (x(i+1) - x(i))
               if (abs(qact) .lt. .001) then
                  if (dxeta .lt. .001) then
!
!                       Make ksi zero
!
                     ksi(i) = 0.
                  else
                     igr = i
                     goto 1000
                  endif
               else
!
!                    Compute ksi
!
                  ksi(i) = dxeta /(qact*abs(qact))
               endif
            endif
500      continue
      endif
   endif
!
   return
!
!     Exceptions
!
1000 continue
!
!C removed as <getloc> and <getbrn> are not accessible
!c
!c      call getloc (igr,ibr,xc)
!c      write (xtxt,'(f10.2)') xc
!c      call getbrn (ibr,branam,lbrnam)
!c      ker = fatal
!c      call sre_error (juer,'FLERES Extra resistance at branch @'//
!c     +                  branam(:lbrnam)//'@ X= @' //xtxt//
!c     +                 '@ undefined for zero discharge',
!c     +                  eflexr,fatal)
!
end
