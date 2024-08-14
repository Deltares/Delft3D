      subroutine FLCCGS (dg  , dsc , cgd   ,cgf ,cw ,mugf,
     +                   cgda, cgfa, mugfa )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLCCGS (FLow Corr. Coefficients for General Structure)
c
c Module description: Correct coefficients for gate flow
c
c                     In the formulas for the gate and weir several
c                     coefficients are applied. To avoid discontinuities
c                     in the transition from weir to gate flow, the
c                     correction coefficient cgd should be corrected.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 cgd               I  Correction coefficient for drowned gate flow.
c  7 cgda              O  Adapted correction coefficient for drowned
c                         gate flow.
c  4 cgf               I  Correction coefficient for free gate flow.
c  8 cgfa              O  Adapted correction coefficient for free gate
c                         flow.
c  5 cw                I  Correction coefficient for weir flow.
c  1 dg                I  Gate opening height.
c  2 dsc               I  Depth at sill or critical depth.
c  6 mugf              I  Contraction coefficient for free gate flow.
c  9 mugfa             O  Adapted contraction coefficient for free gate
c                         flow.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flccgs.pf,v $
c Revision 1.5  1999/03/15  15:49:37  kuipe_j
c tabs removed
c
c Revision 1.4  1995/09/22  10:01:02  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:54:50  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:46  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:33  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:47  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      double precision dg,   dsc,  cgd,  cgf, cw, mugf ,
     +                 cgda, cgfa, mugfa
c
c     Logical function
c
      logical DPSEQU
c
c     dsc contains ds or dc
c
      if ( .not. DPSEQU(dsc, 0.0D0, 1.D-20) ) then
c
         if (dg/dsc .gt. mugf) then
            mugfa = dg/dsc
         else
            mugfa = mugf
         endif
c
         if (cgd .gt. cw) then
            if (DPSEQU(dg, 0.0D0, 1.0D-20) ) then
               cgda = cgd
            else
               cgda = min ( dsc/dg * cw, cgd )
            endif
         else
            cgda = max ( dg/dsc * cw, cgd )
         endif
c
         if (cgf .gt. cw) then
            if (DPSEQU(dg, 0.0D0, 1.0D-20) ) then
               cgfa = cgf
            else
               cgfa = min ( dsc/dg * cw, cgf )
            endif
         else
            cgfa = max ( dg/dsc * cw, cgf )
         endif
c
      else
         mugfa = mugf
         cgda  = cgd
         cgfa  = cgf
      endif
c
      end
