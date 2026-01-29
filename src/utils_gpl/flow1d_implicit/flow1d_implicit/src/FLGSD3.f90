subroutine FLGSD3 (wsd    ,wstr   ,zs     ,w2     ,zb2    ,&
&ds1    ,ds2    ,elu    ,hd     ,rhoast ,&
&cwd    ,ds     ,lambda )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLGSD3 (FLow Gen. Struct. Depth sill 3rd ord. eq.)
!
! Module description: Compute water depth ds at the sill by solving a
!                     third order algebraic equation.
!
!                     In case of drowned weir flow the water level at
!                     the sill is required. The water depth is calcu-
!                     lated in this routine.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 cwd               I  Correction coefficient for drowned weir flow.
!  6 ds1               I  Delta s1 general structure.
!  7 ds2               I  Delta s2 general structure.
! 12 ds                IO Water level immediately downstream the gate.
!  8 elu               I  Upstream energy level.
!  9 hd                I  Downstream water level.
! 13 lambda            I  Extra resistance in general structure.
! 10 rhoast            I  Downstream water density divided by upstream
!                         water density.
!  4 w2(ngrid)         I  W2 coefficient of momentum equation
!  4 w2                I  Width at right side of structure.
!  4 w2(ngrid)         I  Total width at (n+theta2) in every grid point.
!  1 wsd               I  Width structure right or left side.
!  2 wstr              I  Width at centre of structure.
!  5 zb2               I  Bed level at right side of structure.
!  3 zs                I  Bed level at centre of structure.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flgsd3.pf,v $
! Revision 1.11  1999/03/15  15:49:54  kuipe_j
! tabs removed
!
! Revision 1.10  1996/04/12  13:03:46  kuipe_j
! headers, minor changes
!
! Revision 1.9  1996/01/17  14:38:24  kuipe_j
! header update
!
! Revision 1.8  1995/11/21  11:07:51  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.7  1995/10/18  08:59:19  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.6  1995/09/22  10:01:20  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/08/30  12:36:35  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:18  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:55:00  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:57  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:42  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/12/02  13:13:44  kuipe_j
! Error in width improved (W2 = Ws)
!
! Revision 1.1.1.1  1993/07/21  14:43:49  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   double precision  wsd  ,wstr ,zs     ,w2   ,zb2  ,ds1    ,ds2  ,&
   &elu  ,hd   ,rhoast ,cwd  ,ds   ,lambda
!
!     Declaration of local variables:
!
   double precision  aw   ,bw   ,cw     ,d2   ,term ,hsl    ,p    ,&
   &phi  ,r60  ,fac    ,h2a  ,h2b
   double precision  h2c  ,u    ,v      ,c23  ,c13
   double precision  hulp ,hulp1,q
   parameter        (c23=2.0D0/3.0D0 ,c13=1.0D0/3.0D0)

   LOGICAL uitput
   COMMON /UITPUT/uitput
!
!     Calculate Aw, Bw and Cw according to appendix I ,section I.1 of
!     the functional design of the water flow document S-FO-001.5KV.
!
!     De structure formulering is verbeterd in Lelystad workshop
!     van 10-7 tm 22-7-1995.
!
!     Door Flokstra is in de week van 24-7 tm 28-7-1995 deze formulering
!     opnieuw verbeterd m.b.t. de dichtheid.
!
!JK   WRITE (99,*) 'FLGSD3'
!JK   WRITE (99,*)      'wsd,wstr,zs ,w2 ,zb2,ds1 ,ds2 ,elu ,hd'    ,
!JK  +                   wsd,wstr,zs ,w2 ,zb2,ds1 ,ds2 ,elu ,hd
   d2   = hd - zb2
   hsl  = elu - zs
!JK   WRITE (99,*)  'hsl',hsl
   term = ((4.0D0*cwd*cwd*rhoast*wstr*wstr) / (w2*d2))&
   &* (1.0D0+lambda/d2)

   aw = ( - term*hsl - 4.0D0*cwd*wstr&
   &+ (1.0D0-rhoast) * (w2/12.0D0 + wsd/4.0D0)&
   &+ 0.5D0*(rhoast+1.0D0) * (c13*w2 + c23*wsd)&
   &) /  term

   bw = (   4.0D0*cwd*wstr*hsl&
   &+ (1.0D0-rhoast)&
   &* ((d2 + ds1) * (w2 + wsd )/6.D0 + ds1 * wsd *c13 )&
   &+ 0.5D0*(rhoast+1.0D0)&
   &* ((ds1 + ds2 -d2) * (c13*w2 + c23*wsd)&
   &+ (c23*d2 + c13*ds1) * w2 + (c13*d2 + c23*ds1) * wsd )&
   &) / term

   cw = (   (1.0D0-rhoast)&
   &* ((d2 + ds1)**2 * (w2 + wsd )/12.D0 + ds1**2 * wsd/6.0D0)&
   &+ 0.5D0*(rhoast+1.0D0)&
   &* (ds1 + ds2 -d2)&
   &* ((c23*d2 + c13*ds1) * w2 + (c13*d2 + c23*ds1) * wsd )&
   &) / term
!
!     Solve the equation ds**3 + aw*ds**2 + bw*ds +cw to get the water
!     level at the sill
!
   p    = bw/3.0D0 - aw*aw/9.0D0
   q    = aw*aw*aw/27.0D0 - aw*bw/6.0D0 + cw/2.0D0
   hulp = q*q + p*p*p
!
   if ( hulp .lt. 0.0D0 ) then
      p   = abs(p)
      phi = acos(abs(q)/p/sqrt(p)) / 3.0D0
      r60 = acos(0.5D0)
      fac = sign(2.D0,q) * sqrt(p)
      h2a = -fac * cos(phi)
      h2b =  fac * cos(r60-phi)
      h2c =  fac * cos(r60+phi)
      ds  = max(h2a,h2b,h2c) - aw/3.0D0
   else
      hulp  = sqrt(hulp)
      hulp1 = -q + hulp
      u     = abs(hulp1)**c13 * sign(1.0D0,hulp1)
      hulp1 = -q - hulp
      v     = abs(hulp1)**c13 * sign(1.0D0,hulp1)
      ds    = u + v - aw/3.0D0
   endif

   if (uitput) then
      WRITE (99,*) 'd2,hsl,term',d2,hsl,term
      WRITE (99,*) 'A,B,C',aw,bw,cw
      WRITE (99,*) 'hulp',hulp
      WRITE (99,*) 'DS-weir',ds
   endif
!
end
