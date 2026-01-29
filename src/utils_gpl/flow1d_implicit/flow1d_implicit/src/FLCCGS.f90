subroutine FLCCGS (dg  , dsc , cgd   ,cgf ,cw ,mugf,&
&cgda, cgfa, mugfa )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLCCGS (FLow Corr. Coefficients for General Structure)
!
! Module description: Correct coefficients for gate flow
!
!                     In the formulas for the gate and weir several
!                     coefficients are applied. To avoid discontinuities
!                     in the transition from weir to gate flow, the
!                     correction coefficient cgd should be corrected.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 cgd               I  Correction coefficient for drowned gate flow.
!  7 cgda              O  Adapted correction coefficient for drowned
!                         gate flow.
!  4 cgf               I  Correction coefficient for free gate flow.
!  8 cgfa              O  Adapted correction coefficient for free gate
!                         flow.
!  5 cw                I  Correction coefficient for weir flow.
!  1 dg                I  Gate opening height.
!  2 dsc               I  Depth at sill or critical depth.
!  6 mugf              I  Contraction coefficient for free gate flow.
!  9 mugfa             O  Adapted contraction coefficient for free gate
!                         flow.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flccgs.pf,v $
! Revision 1.5  1999/03/15  15:49:37  kuipe_j
! tabs removed
!
! Revision 1.4  1995/09/22  10:01:02  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:54:50  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:46  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:33  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:47  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   double precision dg,   dsc,  cgd,  cgf, cw, mugf ,&
   &cgda, cgfa, mugfa
!
!     Logical function
!
   logical DPSEQU
!
!     dsc contains ds or dc
!
   if ( .not. DPSEQU(dsc, 0.0D0, 1.D-20) ) then
!
      if (dg/dsc .gt. mugf) then
         mugfa = dg/dsc
      else
         mugfa = mugf
      endif
!
      if (cgd .gt. cw) then
         if (DPSEQU(dg, 0.0D0, 1.0D-20) ) then
            cgda = cgd
         else
            cgda = min ( dsc/dg * cw, cgd )
         endif
      else
         cgda = max ( dg/dsc * cw, cgd )
      endif
!
      if (cgf .gt. cw) then
         if (DPSEQU(dg, 0.0D0, 1.0D-20) ) then
            cgfa = cgf
         else
            cgfa = min ( dsc/dg * cw, cgf )
         endif
      else
         cgfa = max ( dg/dsc * cw, cgf )
      endif
!
   else
      mugfa = mugf
      cgda  = cgd
      cgfa  = cgf
   endif
!
end
