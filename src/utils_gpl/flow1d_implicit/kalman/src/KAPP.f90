subroutine KAPP (il     ,ir     ,ngrid  ,istru  ,strclo ,&
&strpar ,h2     ,maxtab ,ntabm  ,ntab   ,table  ,&
&a2     ,b2     ,c2     ,d2     ,m2     ,ea2    ,&
&eb2    ,ec2    ,ed2    ,em2    ,teken  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAPP (KAlman structure PumP)
!
! Module description: In subroutine KAPP the ABCDE coefficients are computed
!                     for a pump.
!
!                     The coefficients are described in S-FO-004 Par 7.5
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 12 a2(ngridm)        IO A2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 13 b2(ngridm)        O  B2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 14 c2(ngridm)        O  C2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 15 d2(ngridm)        O  D2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 17 ea2(ngrid)        O  EA2 right hand side coefficient of momentum
! 18 eb2(ngrid)        O  EB2 right hand side coefficient of momentum
! 19 ec2(ngrid)        O  EC2 right hand side coefficient of momentum
! 20 ed2(ngrid)        O  ED2 right hand side coefficient of momentum
! 21 em2(ngrid)        O  EM2 right hand side coefficient of Q-h relation
!  7 h2(ngrid)         I  Water level in every grid point at time
!                         t(n+1).
!  1 il                P  -
!  2 ir                P  -
!  4 istru             I  Number of structure.
! 16 m2(ngrid)         O  M2 coefficient of Q-h relation
!  8 maxtab            I  Maximum number of defined tables.
!  3 ngrid             I  Number of grid points in network.
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
! 22 teken             I  Flow direction (+/-).
!  5 strclo(nstru)     I  True if structure is closed.
!  6 strpar            P  -
! 11 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flppar  FLow get PumP ARguments
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kapp.pf,v $
! Revision 1.3  1999/03/15  15:52:04  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:14  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:47  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   integer il, ir, istru, ngrid, maxtab, ntabm, ntab(4,maxtab)
   logical strclo(*)
   real    strpar(dmstrpar,*)
   real    table(ntabm)
   real    a2, b2, c2, d2, m2, ea2, eb2, ec2, ed2, em2, teken
   double precision h2(ngrid)
!
!     Declaration of local variables:
!
   integer itab, iup, idown
   real    cap, capold, hstart, hstop, hact, dh
   real    hd, hu, head, f1, f2, dfdh
!
   parameter (dh = 0.001)
!
   call FLPPAR(istru  ,strpar ,ngrid  ,h2     ,il     ,ir     ,&
   &itab   ,cap    ,capold ,hstart ,hstop  ,&
   &iup    ,idown  ,teken  ,hact   )
!
   hu = h2(iup)
   hd = h2(idown)
!
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
!
      if ( capold .ge. 1.E-6) then
         head = hu - hd
         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)), dble(head), f1)
         head = hu - hd - dh*teken
         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)), dble(head), f2)
         dfdh = (f1 - f2) / dh*teken
      else
         dfdh = 0.
      endif
!
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
