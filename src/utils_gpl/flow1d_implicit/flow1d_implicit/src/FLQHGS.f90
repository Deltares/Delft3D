function FLQHGS (g      ,istru  ,strsta ,strclo ,hu     ,hd     ,&
&uu     ,zs     ,wstr   ,w2     ,wsd    ,zb2    ,&
&dg     ,ds1    ,ds2    ,rhoast ,cgf    ,cgd    ,&
&cwf    ,cwd    ,mugf   ,ds     ,formno ,lambda ,&
&dhstru )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLQHGS (FLow QH relation for General Structure)
!
! Module description: Function FLQHGS defines the QH-relationship for a
!                     general structure.
!
!                     In subroutine FLQHGS for given upstream and down-
!                     stream water levels the discharge across the gen-
!                     eral structure will be computed according to the
!                     specific stage-discharge equation (QH-relation)
!                     for that structure.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 18 cgd               P  -
! 17 cgf               P  -
! 20 cwd               I  Correction coefficient for drowned weir flow.
! 19 cwf               I  Correction coefficient for free weir flow.
! 13 dg                I  Gate opening height.
! 25 dhstru            I  Delta h used for numerical differentation.
! 22 ds                I  Water level immediately downstream the gate.
! 14 ds1               P  -
! 15 ds2               P  -
!  0 flqhgs            O  Discharge across general structure.
! 23 formno            IO Flow condition of general structure:
!                         formno = 0, closed or other structure
!                         formno = 1, free weir
!                         formno = 2, drowned weir
!                         formno = 3, free gate
!                         formno = 4, drowned gate
!  1 g                 I  Acceleration of gravity.
!  6 hd                I  Downstream water level.
!  5 hu                I  Upstream water level.
!  2 istru             I  Number of structure.
! 24 lambda            P  -
! 21 mugf              P  -
! 16 rhoast            P  -
!  4 strclo(nstru)     O  True if structure is closed.
!  3 strsta            I  Logical indicator.
!  7 uu                I  Upstream velocity.
! 10 w2(ngrid)         I  W2 coefficient of momentum equation
! 10 w2                I  Width at right side of structure.
! 10 w2(ngrid)         I  Total width at (n+theta2) in every grid point.
! 11 wsd               P  -
!  9 wstr              I  Width at centre of structure.
! 12 zb2               I  Bed level at right side of structure.
!  8 zs                I  Bed level at centre of structure.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flccgs  FLow Corr. Coefficients for General Structure
! flgsd2  FLow Gen. Struct. Depth sill 2nd ord. eq.
! flgsd3  FLow Gen. Struct. Depth sill 3rd ord. eq.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flqhgs.pf,v $
! Revision 1.18  1999/03/16  13:00:24  kuipe_j
! remove discontinuty around zero
!
! Revision 1.16  1998/07/01  11:21:27  kuipe_j
! Improve test E < crest
!
! Revision 1.15  1997/11/26  14:53:03  kuipe_j
! contraction zero allowed now
!
! Revision 1.14  1997/07/10  14:22:41  kuipe_j
! E at structure must be > crest
!
! Revision 1.13  1996/05/30  09:56:40  kuipe_j
! general structure dlim, controllers
!
! Revision 1.12  1996/04/12  13:04:17  kuipe_j
! headers, minor changes
!
! Revision 1.11  1996/04/11  08:23:49  kuipe_j
! Kalman module added
!
! Revision 1.10  1995/12/11  13:39:02  hoeks_a
! Error in functionname repaired
!
! Revision 1.9  1995/12/06  08:48:40  hoeks_a
! Aux. output redirected to file
!
! Revision 1.8  1995/11/21  11:08:00  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.7  1995/09/22  10:02:07  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:11:00  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:36:48  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:27  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:55:21  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:21  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:01  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:45  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1994/05/17  09:11:26  kuipe_j
! Prevention against negative water depth added.
!
! Revision 1.1.1.1  1993/07/21  14:43:54  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Function declaration:
!
   double precision    FLQHGS
   logical             DEQUAL, DPSEQU
!
!     Declaration of parameters:
!
   integer          formno ,istru
   logical          strsta ,strclo(*)
   double precision g      ,hu    ,hd    ,uu    ,wstr    ,zs    ,&
   &w2     ,wsd   ,zb2   ,dg    ,ds1     ,ds2   ,&
   &rhoast ,cgf   ,cgd   ,cwf   ,cwd     ,mugf  ,&
   &lambda ,dhstru,ds
!
!     Declaration of local variables:
!
   logical imag
   double precision  dc    ,elu   ,cgda  ,cgfa   ,cwfa   ,&
   &mugfa ,cgd2  ,dhstr1 ,hd1   ,dlim   ,&
   &hs1    ,shead
!
   elu  = hu + uu*uu / (2.0D0*g)
   hs1 = elu - zs
   dlim = hs1*(wstr/w2*2./3.*sqrt(2./3.))**(2.0/3.0)
   hd1  =  max(hd,zb2+dlim*0.9D0)
   if ( hs1 .le. 0.0D0 .or. wstr .le. 0.0D0 .or. cwd .le. 0.) then
      if ( strsta ) then
         strclo(istru) = .true.
      endif
      FLQHGS = 0.D0
      formno = 0
   else
      if ( strsta ) then
         strclo(istru) = .false.
      endif
      dhstr1 = dhstru * 1.D0
!
!       Compute upstream energy level H1, critical water depth at the
!       sill, dc and water depth at the sill,ds
!
!       [Doc: S-DO-001.2AK Eqs. 37 - 42]
!
      dc  = 2.0D0/3.0D0 * hs1
!
!       Calculate ds by solving third order algebraic equation
!
      call FLGSD3 (wsd    ,wstr   ,zs     ,w2     ,zb2    ,&
      &ds1    ,ds2    ,elu    ,hd1    ,rhoast ,&
      &cwd    ,ds     ,lambda )
!
      if (ds .ge. dc) then
         if (dg .ge. ds) then
!
!             - drowned weir -
!
            formno = 2
         else
!
!             - gate flow -
!
            formno = 3
!
!             adapt coefficients on basis of Ds & Cwd
!
            call FLCCGS (dg     ,ds     ,cgd    ,cgf    ,cwd    ,&
            &mugf   ,cgda   ,cgfa   ,mugfa  )
         endif
      else
!
!          Adapt Cwf coefficient
!
         if (cwf .lt. cwd) then
            if (DPSEQU ( dc, 0.0D0, 1.0D-20)) then
               cwfa = cwf
            else
               cwfa = max ( ds/dc * cwd, cwf )
            endif
         elseif (ds .gt. 0.0D0) then
            cwfa = min ( dc/ds * cwd, cwf )
         else
            cwfa = cwf
         endif
!
         if (dg .ge. dc) then
!
!             - free weir -
!
            formno = 1
         else
!
!             - gate flow -
!
            formno = 3
!
!             adapt coefficients on basis of Dc & Cwf
!
            call FLCCGS (dg     ,dc     ,cgd    ,cgf    ,cwfa   ,&
            &mugf   ,cgda   ,cgfa   ,mugfa  )
         endif
      endif
!
!       In case of gate flow determine type of gate flow
!       (drowned or free)
!
      if (formno .eq. 3) then
         dc = mugfa * dg
!
!          Cgd for second order equation = Cgd' * Mu'
!
         cgd2 = cgda * mugfa
!
         call FLGSD2 (wsd    ,wstr   ,zs     ,w2     ,zb2    ,&
         &dg     ,ds1    ,ds2    ,elu    ,hd1    ,&
         &rhoast ,cgd2   ,imag   ,ds     ,lambda )
!
         if ( imag ) then
!
!             - free gate -
!
            formno = 3
         else if ( ds .le. dc ) then
!
!             - free gate -
!
            formno = 3
!
!             Adapt coefficients
!
            if (cgda .gt. cgfa) then
               if (.not. DPSEQU ( dc, 0.0D0, 1.0D-20)) then
                  cgfa = max ( ds/dc * cgda, cgfa )
               endif
            else if ( ds .gt. 0.0D0) then
               cgfa = min ( dc/ds * cgda, cgfa )
            endif
         else
!
!             - drowned gate -
!
            formno = 4
         endif
      endif
!
!       Calculate the flow through structure depending on formno
!
      if      ( formno .eq. 1 ) then
!
!          - free weir -
!
         FLQHGS = cwfa * wstr * sqrt(2.0D0/3.0D0*g) *&
         &2.0D0/3.0D0*hs1*sqrt(abs(hs1))
      else if ( formno .eq. 2 ) then
!
!          -drowned weir -
!
         shead  = SIGN(1.0D0,hs1-ds)
         FLQHGS = cwd * wstr * ds * shead&
         &* (- sqrt(2.0D0*g*dhstr1)&
         &+ sqrt( ABS(2.0D0*g * ( dhstr1*shead&
         &+ hs1-ds))))
      else if ( formno .eq. 3 ) then
!
!          - free gate -
!
         shead  = SIGN(1.0D0,hs1-dc)
         FLQHGS = mugfa * cgfa * wstr * dg * shead&
         &* (- sqrt(2.0D0*g*dhstr1)&
         &+ sqrt( ABS(2.0D0*g * ( dhstr1*shead&
         &+ hs1-dc))))
      else if ( formno .eq. 4 ) then
!
!          -drowned gate -
!
         shead  = SIGN(1.0D0,hs1-ds)
         FLQHGS = mugfa * cgda * wstr * dg * shead&
         &* (- sqrt(2.0D0*g*dhstr1)&
         &+ sqrt( ABS(2.0D0*g * ( dhstr1*shead&
         &+ hs1-ds))))
      endif
!
!       Structure open or closed ?
!
      if ( strsta ) then
         if ( formno .lt. 3 ) then
            if ( hu .lt. zs ) then
!                - closed weir -
               strclo(istru) = .true.
               FLQHGS = 0.0D0
            else
!                - open weir -
               strclo(istru) = .false.
            endif
         else
            if ( DEQUAL(dg , 0.0D0) ) then
!                - closed gate -
               strclo(istru) = .true.
               FLQHGS = 0.0D0
            else
!                - open gate -
               strclo(istru) = .false.
            endif
         endif
      endif
   endif

!JK   WRITE (99,*) 'optie  ',formno
!JK   WRITE (99,*) 'IN FLQHGS (q,hu,hd)',flqhgs,hu,hd
!
end
