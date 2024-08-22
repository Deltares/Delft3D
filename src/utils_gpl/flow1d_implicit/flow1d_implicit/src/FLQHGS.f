      function FLQHGS (g      ,istru  ,strsta ,strclo ,hu     ,hd     ,
     +                 uu     ,zs     ,wstr   ,w2     ,wsd    ,zb2    ,
     +                 dg     ,ds1    ,ds2    ,rhoast ,cgf    ,cgd    ,
     +                 cwf    ,cwd    ,mugf   ,ds     ,formno ,lambda ,
     +                 dhstru )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLQHGS (FLow QH relation for General Structure)
c
c Module description: Function FLQHGS defines the QH-relationship for a
c                     general structure.
c
c                     In subroutine FLQHGS for given upstream and down-
c                     stream water levels the discharge across the gen-
c                     eral structure will be computed according to the
c                     specific stage-discharge equation (QH-relation)
c                     for that structure.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 18 cgd               P  -
c 17 cgf               P  -
c 20 cwd               I  Correction coefficient for drowned weir flow.
c 19 cwf               I  Correction coefficient for free weir flow.
c 13 dg                I  Gate opening height.
c 25 dhstru            I  Delta h used for numerical differentation.
c 22 ds                I  Water level immediately downstream the gate.
c 14 ds1               P  -
c 15 ds2               P  -
c  0 flqhgs            O  Discharge across general structure.
c 23 formno            IO Flow condition of general structure:
c                         formno = 0, closed or other structure
c                         formno = 1, free weir
c                         formno = 2, drowned weir
c                         formno = 3, free gate
c                         formno = 4, drowned gate
c  1 g                 I  Acceleration of gravity.
c  6 hd                I  Downstream water level.
c  5 hu                I  Upstream water level.
c  2 istru             I  Number of structure.
c 24 lambda            P  -
c 21 mugf              P  -
c 16 rhoast            P  -
c  4 strclo(nstru)     O  True if structure is closed.
c  3 strsta            I  Logical indicator.
c  7 uu                I  Upstream velocity.
c 10 w2(ngrid)         I  W2 coefficient of momentum equation
c 10 w2                I  Width at right side of structure.
c 10 w2(ngrid)         I  Total width at (n+theta2) in every grid point.
c 11 wsd               P  -
c  9 wstr              I  Width at centre of structure.
c 12 zb2               I  Bed level at right side of structure.
c  8 zs                I  Bed level at centre of structure.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flccgs  FLow Corr. Coefficients for General Structure
c flgsd2  FLow Gen. Struct. Depth sill 2nd ord. eq.
c flgsd3  FLow Gen. Struct. Depth sill 3rd ord. eq.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flqhgs.pf,v $
c Revision 1.18  1999/03/16  13:00:24  kuipe_j
c remove discontinuty around zero
c
c Revision 1.16  1998/07/01  11:21:27  kuipe_j
c Improve test E < crest
c
c Revision 1.15  1997/11/26  14:53:03  kuipe_j
c contraction zero allowed now
c
c Revision 1.14  1997/07/10  14:22:41  kuipe_j
c E at structure must be > crest
c
c Revision 1.13  1996/05/30  09:56:40  kuipe_j
c general structure dlim, controllers
c
c Revision 1.12  1996/04/12  13:04:17  kuipe_j
c headers, minor changes
c
c Revision 1.11  1996/04/11  08:23:49  kuipe_j
c Kalman module added
c
c Revision 1.10  1995/12/11  13:39:02  hoeks_a
c Error in functionname repaired
c
c Revision 1.9  1995/12/06  08:48:40  hoeks_a
c Aux. output redirected to file
c
c Revision 1.8  1995/11/21  11:08:00  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.7  1995/09/22  10:02:07  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:11:00  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:36:48  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:27  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:55:21  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:21  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:01  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:45  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1994/05/17  09:11:26  kuipe_j
c Prevention against negative water depth added.
c
c Revision 1.1.1.1  1993/07/21  14:43:54  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Function declaration:
c
      double precision    FLQHGS
      logical             DEQUAL, DPSEQU
c
c     Declaration of parameters:
c
      integer          formno ,istru
      logical          strsta ,strclo(*)
      double precision g      ,hu    ,hd    ,uu    ,wstr    ,zs    ,
     &                 w2     ,wsd   ,zb2   ,dg    ,ds1     ,ds2   ,
     &                 rhoast ,cgf   ,cgd   ,cwf   ,cwd     ,mugf  ,
     &                 lambda ,dhstru,ds
c
c     Declaration of local variables:
c
      logical imag
      double precision  dc    ,elu   ,cgda  ,cgfa   ,cwfa   ,
     &                  mugfa ,cgd2  ,dhstr1 ,hd1   ,dlim   ,
     &                  hs1    ,shead
c
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
c
c       Compute upstream energy level H1, critical water depth at the
c       sill, dc and water depth at the sill,ds
c
c       [Doc: S-DO-001.2AK Eqs. 37 - 42]
c
        dc  = 2.0D0/3.0D0 * hs1
c
c       Calculate ds by solving third order algebraic equation
c
        call FLGSD3 (wsd    ,wstr   ,zs     ,w2     ,zb2    ,
     +               ds1    ,ds2    ,elu    ,hd1    ,rhoast ,
     +               cwd    ,ds     ,lambda )
c
        if (ds .ge. dc) then
           if (dg .ge. ds) then
c
c             - drowned weir -
c
              formno = 2
           else
c
c             - gate flow -
c
              formno = 3
c
c             adapt coefficients on basis of Ds & Cwd
c
              call FLCCGS (dg     ,ds     ,cgd    ,cgf    ,cwd    ,
     +                     mugf   ,cgda   ,cgfa   ,mugfa  )
           endif
        else
c
c          Adapt Cwf coefficient
c
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
c
           if (dg .ge. dc) then
c
c             - free weir -
c
             formno = 1
           else
c
c             - gate flow -
c
             formno = 3
c
c             adapt coefficients on basis of Dc & Cwf
c
              call FLCCGS (dg     ,dc     ,cgd    ,cgf    ,cwfa   ,
     +                     mugf   ,cgda   ,cgfa   ,mugfa  )
           endif
        endif
c
c       In case of gate flow determine type of gate flow
c       (drowned or free)
c
        if (formno .eq. 3) then
           dc = mugfa * dg
c
c          Cgd for second order equation = Cgd' * Mu'
c
           cgd2 = cgda * mugfa
c
           call FLGSD2 (wsd    ,wstr   ,zs     ,w2     ,zb2    ,
     +                  dg     ,ds1    ,ds2    ,elu    ,hd1    ,
     +                  rhoast ,cgd2   ,imag   ,ds     ,lambda )
c
          if ( imag ) then
c
c             - free gate -
c
             formno = 3
          else if ( ds .le. dc ) then
c
c             - free gate -
c
             formno = 3
c
c             Adapt coefficients
c
              if (cgda .gt. cgfa) then
                if (.not. DPSEQU ( dc, 0.0D0, 1.0D-20)) then
                    cgfa = max ( ds/dc * cgda, cgfa )
                 endif
              else if ( ds .gt. 0.0D0) then
                 cgfa = min ( dc/ds * cgda, cgfa )
              endif
          else
c
c             - drowned gate -
c
             formno = 4
          endif
        endif
c
c       Calculate the flow through structure depending on formno
c
        if      ( formno .eq. 1 ) then
c
c          - free weir -
c
          FLQHGS = cwfa * wstr * sqrt(2.0D0/3.0D0*g) *
     +             2.0D0/3.0D0*hs1*sqrt(abs(hs1))
        else if ( formno .eq. 2 ) then
c
c          -drowned weir -
c
          shead  = SIGN(1.0D0,hs1-ds)
          FLQHGS = cwd * wstr * ds * shead
     +             * (- sqrt(2.0D0*g*dhstr1)
     +                + sqrt( ABS(2.0D0*g * ( dhstr1*shead
     +                                      + hs1-ds))))
        else if ( formno .eq. 3 ) then
c
c          - free gate -
c
          shead  = SIGN(1.0D0,hs1-dc)
           FLQHGS = mugfa * cgfa * wstr * dg * shead
     +              * (- sqrt(2.0D0*g*dhstr1)
     +                 + sqrt( ABS(2.0D0*g * ( dhstr1*shead
     +                                       + hs1-dc))))
        else if ( formno .eq. 4 ) then
c
c          -drowned gate -
c
          shead  = SIGN(1.0D0,hs1-ds)
           FLQHGS = mugfa * cgda * wstr * dg * shead
     +              * (- sqrt(2.0D0*g*dhstr1)
     +                 + sqrt( ABS(2.0D0*g * ( dhstr1*shead
     +                                       + hs1-ds))))
        endif
c
c       Structure open or closed ?
c
        if ( strsta ) then
           if ( formno .lt. 3 ) then
             if ( hu .lt. zs ) then
c                - closed weir -
                strclo(istru) = .true.
                FLQHGS = 0.0D0
             else
c                - open weir -
                strclo(istru) = .false.
             endif
           else
              if ( DEQUAL(dg , 0.0D0) ) then
c                - closed gate -
                strclo(istru) = .true.
                FLQHGS = 0.0D0
             else
c                - open gate -
                strclo(istru) = .false.
             endif
           endif
        endif
      endif

CJK   WRITE (99,*) 'optie  ',formno
CJK   WRITE (99,*) 'IN FLQHGS (q,hu,hd)',flqhgs,hu,hd
c
      end
