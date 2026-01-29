subroutine FLGSD2 (wsd    ,wstr   ,zs     ,w2     ,zb2    ,&
&dg     ,ds1    ,ds2    ,elu    ,hd     ,&
&rhoast ,cgd    ,imag   ,ds     ,lambda )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLGSD2 (FLow Gen. Struct. Depth sill 2nd ord. eq.)
!
! Module description: Compute water depth ds at the sill by a second
!                     order algebraic equation.
!
!                     In case of drowned gate flow the water level at
!                     the sill is required. The water depth is calcu-
!                     lated in this routine.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 12 cgd               I  Correction coefficient for drowned gate flow.
!  6 dg                I  Gate opening height.
!  7 ds1               I  Delta s1 general structure.
!  8 ds2               I  Delta s2 general structure.
! 14 ds                IO Water level immediately downstream the gate.
!  9 elu               I  Upstream energy level.
! 10 hd                I  Downstream water level.
! 13 imag              O  Logical indicator, = TRUE when determinant of
!                         second order algebraic equation less than
!                         zero.
! 15 lambda            I  Extra resistance in general structure.
! 11 rhoast            I  Downstream water density divided by upstream
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
! $Log: flgsd2.pf,v $
! Revision 1.11  1999/03/15  15:49:52  kuipe_j
! tabs removed
!
! Revision 1.10  1996/04/12  13:03:45  kuipe_j
! headers, minor changes
!
! Revision 1.9  1996/01/17  14:38:22  kuipe_j
! header update
!
! Revision 1.8  1995/11/21  11:07:50  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.7  1995/10/18  08:59:18  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.6  1995/09/22  10:01:19  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/08/30  12:36:34  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:17  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:54:59  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:56  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:41  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:49  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   logical          imag
   double precision wsd  ,wstr ,zs     ,w2   ,zb2 ,dg    ,ds1  ,ds2 ,&
   &elu  ,hd   ,rhoast ,cgd  ,ds  ,lambda
!
!     Declaration of local variables:
!
   double precision ag   ,bg   ,cg   ,d2   ,terma ,termb ,hsl  ,det
   double precision c23  ,c13

   parameter       (c23=2.0D0/3.0D0, c13=1.0D0/3.0D0)

   LOGICAL uitput
   COMMON /UITPUT/uitput
!
!     Calculate Ag, Bg and Cg according to appendix I ,section I.2 of
!     the functional design of the water flow document S-FO-001.5KV.
!JK   WRITE  (99,*)  'IN FLGSD2 ----'
!
   ag =     (1.0D0-rhoast) * (w2/12.0D0 + wsd/4.0D0)&
   &+ 0.5D0*(rhoast+1.0D0) * (c13*w2 + c23*wsd)
   d2 = hd - zb2

   terma = ( 4.0D0*rhoast*cgd*cgd*dg*dg*wstr*wstr ) / (w2*d2)&
   &* (1.0D0+lambda/d2)
   termb = 4.0D0*cgd*dg*wstr

   bg =     (1.0D0-rhoast)&
   &* ((d2 + ds1) * (w2 + wsd )/6.D0 + ds1 * wsd *c13 )&
   &+ 0.5D0*(rhoast+1.0D0)&
   &* ((ds1 + ds2 -d2) * (c13*w2 + c23*wsd)&
   &+ (c23*d2 + c13*ds1) * w2 + (c13*d2 + c23*ds1) * wsd )&
   &+ terma - termb

   hsl = elu - zs

   cg =     (1.0D0-rhoast)&
   &* ((d2 + ds1)**2 * (w2 + wsd )/12.D0 + ds1**2 * wsd/6.0D0)&
   &+ 0.5D0*(rhoast+1.0D0)&
   &* (ds1 + ds2 -d2)&
   &* ((c23*d2 + c13*ds1) * w2 + (c13*d2 + c23*ds1) * wsd )&
   &- terma*hsl + termb*hsl

   det = bg*bg - 4.0D0*ag*cg
   if ( det .lt. 0.0D0 ) then
      imag = .true.
!JK      WRITE (99,*) 'Det=',det
   else
      imag = .false.
      ds = ( -bg + sqrt(det) ) / (2.0D0*ag)
   endif
   if (uitput) then
      WRITE (99,*) 'Gate: a b c',ag,bg,cg
      WRITE (99,*)  'DS-gate=',ds
   endif

end
