subroutine KAERES(nbran  ,ngrid  ,branch ,h1     ,h      ,&
&theta2 ,maxtab ,ntabm  ,ntab   ,table  ,&
&x      ,nexres ,exres  ,eta    ,detadh )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAERES (KAlman Extra RESistance)
!
! Module description: Calculate derivative of extra resistance and extra
!                     resistance ETA in network.
!
!                     The derivative of the extra resistance is deter-
!                     mined by numerical differentiation.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 15 detadh(ngrid)     O  Derivative of extra resistance coefficient ETA
!                         to waterlevel in every grid point i+1/2 on time
!                         n+1/2 (d(ETA)/dh). Value at i+1/2 is stored at
!                         index i.
! 14 eta(ngrid)        I  Extra resistance coefficient ETA in every grid
!                         point i+1/2 on time n+1/2. Value at i+1/2 is
!                         stored at index i.
! 13 exres(3,nexres)   I  Extra resistance definition.
!                         (1,i) = Branch number.
!                         (2,i) = X-coordinate.
!                         (3,i) = Table pointer to function of water
!                                 level.
!  5 h(ngrid)          I  Contains water levels in every grid point. It is
!                         stored on index 2 of the packed array hpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
!  4 h1(ngrid)         I  Water level in every grid point at time t(n).
!  7 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
! 12 nexres            I  Number of extra resistance points.
!  2 ngrid             I  Number of grid points in network.
!  9 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  8 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 10 table             P  -
!  6 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
! 11 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
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
! $Log: kaeres.pf,v $
! Revision 1.4  1999/03/15  15:51:42  kuipe_j
! tabs removed
!
! Revision 1.3  1997/06/04  11:18:14  kuipe_j
! Initialize arrays
!
! Revision 1.2  1996/04/12  13:04:48  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:24  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer maxtab, ntabm, ntab(4,maxtab),&
   &nbran, branch(4,nbran),&
   &ngrid, nexres
   real    theta2
   real    table(ntabm), exres(3,*),&
   &x(ngrid), eta(ngrid), detadh(ngrid)

   double precision  h1(ngrid), h(ngrid)

!
!     Declaration of local variables:
!
   integer i,    i1,   i2,   ibr,   j, itab
   real    hact, etaacc, dh
!
   parameter (dh = 0.001)
!
!     Loop over branches
!
   do 100 i = 1, ngrid
      detadh(i) = 0.
      eta(i)    = 0.
100 continue

   if (nexres .gt. 0) then
!
      do 400 ibr = 1, nbran
!
!           i1 = global grid point number at node n1 of branch
!           i2 = global grid point number at node n2 of branch
!
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
!
!           Loop over grid points in branch
!
         do 300 i = i1, i2-1
            do 200 j = 1, nexres
               if ( int(exres(1,j)) .eq. ibr ) then
!
!                    Compute extra resistance in this branch
!                    Resistance as function of h at (i+1/2,n+theta)
!                    Resistance as function of h at (i+1/2,n+theta)+dh
!
                  if ( exres(2,j) .ge. x(i) .and.&
                  &exres(2,j) .le. x(i+1) ) then
                     itab = int(exres(3,j))
                     hact = theta2*(h(i)+h(i+1))/2. +&
                     &(1.-theta2)*(h1(i)+h1(i+1))/2
!
!                       Interpolate value for eta
!
                     call INTTAB (ntab(1,itab), ntab(4,itab),&
                     &table(ntab(2,itab)),&
                     &table(ntab(3,itab)),&
                     &dble(hact)  , eta(i) )
!
!                       Interpolate value for eta'
!
                     call INTTAB (ntab(1,itab), ntab(4,itab),&
                     &table(ntab(2,itab)),&
                     &table(ntab(3,itab)),&
                     &dble(hact+dh)      , etaacc)
                     detadh(i) = (etaacc - eta(i)) / dh
                  endif
               endif
200         continue
300      continue
400   continue
   endif
   return
!
end
