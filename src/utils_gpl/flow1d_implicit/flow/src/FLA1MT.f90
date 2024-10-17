subroutine FLA1MT(i1     ,i2     ,ngrid  ,h1     ,h      ,&
&maxlev ,nlev   ,hlev   ,&
&wft    ,af     ,izwft  ,a1m    ,theta2 )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLA1MT (FLow A1M Table)
!
! Module description: Compute first order momentum cross section A1m for
!                     each grid point in a table branch.
!
!                     For a table cross section the first order momentum
!                     will be calculated by interpolation. In case the
!                     water level is below the preissmann level the
!                     first order momemtum will be set to zero. This re-
!                     sults in a density term of zero (No influence of
!                     salt on flow).
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 13 a1m               O  parameter a1m
! 11 af(ngrid)         I  Flow area at every grid point at time t(n+1)
!  5 h1(ngrid)         I  Water level in every grid point at time t(n).
!  6 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!  9 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  1 i1                I  Index of first grid point in actual branch.
!  2 i2                I  Index of last grid point in actual branch.
! 12 izwft(ngrid,      I  Table containing integrated widths.
!       maxlev)
!  7 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  4 ngrid             I  Number of grid points in network.
!  8 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
! 14 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
! 10 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width of main section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! indwgh  Compute INDex and WeiGHt factor
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: fla1mt.pf,v $
! Revision 1.8  1998/06/11  11:46:54  kuipe_j
! Estuary special integrated
!
! Revision 1.7  1997/09/30  09:25:23  kuipe_j
! density term improved for Preisman slot
!
! Revision 1.6  1997/01/23  08:28:52  kuipe_j
! Make flow module robust
!
! Revision 1.5  1995/09/22  10:00:43  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:10:41  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.2  1993/11/26  15:30:23  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:45  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Declaration of Parameters:
!
   integer   i1, i2, ngrid, maxlev
   integer   nlev(ngrid)
   double precision      hlev(ngrid,maxlev)
   double precision      h1(ngrid), h(ngrid)
   real      wft  (ngrid,maxlev), af (ngrid)
   real      izwft(ngrid,maxlev), a1m(ngrid)
   real      theta2
!
!     Declaration of local variables:
!
   integer  i, ilev, ntmax
   real     ht1, ht2, wf1, wf2, z, term1, term2,&
   &hmax, wfmax, izwf
   double precision wght, hint
!
!     Do for each gridpoint in branch
!
   do 100 i = i1, i2
!       hint = ( h1(i) + h(i) ) / 2.
      hint = dble( theta2*h(i) + (1.-theta2)*h1(i) )
      if ( hint .le. hlev(i,1) ) then
         a1m(i) = 0.
      else
!
!           Compute index (ilev) and weight factor (wght) w.r.t.
!           tables of widths.
!
         call INDWGH (ngrid  ,i      ,&
         &maxlev ,nlev   ,hlev   ,&
         &hint   ,ilev   ,wght   )
!
!           Determine actual flow width Wfz
!           [ Doc. S-FO-001.5KV / Eq. 5-19 ]
!
         if (ilev .gt. 0) then
!
            ht1 = sngl(hlev(i,ilev  ))
            ht2 = sngl(hlev(i,ilev+1))
            wf1 = wft (i,ilev  )
            wf2 = wft (i,ilev+1)
!
!              Evaluation of right hand side term of eq. (5-19) for
!              z = h(t1) and h*.    (excl. integral itself)
!
!              1. For z = h(t1)
!
            z     = ht1
            term1 = ( (z*z*z)/3.0 - (ht1 * z*z)/2.0 ) /(ht2 - ht1)&
            &* (wf2 - wf1) + z*z * wf1/2.
!
!              2. For z = h* ( =h(n+theta2) )
!
            z     = hint
            term2 = ( (z*z*z)/3.0 - (ht1 * z*z)/2.0 ) /(ht2 - ht1)&
            &* (wf2 - wf1) + z*z * wf1/2.
!
!              Evaluation of actual integral [z * Wf(z) * dz]
!              for z = h*
!
            izwf   = izwft(i,ilev) + (term2 - term1)

         else
!
!              table overflow
!              [ Doc. S-FO-001.5KV / Eq. 5-18 ]
!
            ntmax = nlev(i)
            hmax  = sngl(hlev(i,ntmax))
            wfmax = wft (i,ntmax)
            izwf  = izwft(i,ntmax) + wfmax * (hint*hint-hmax*hmax)/2.

         endif
!
!           Evaluation of actual coefficient A1m
!           [ Doc. S-FO-001.5KV / Eq. 5-17 ]
!
         a1m(i) = hint * af(i) - izwf
      endif
100 continue
!
end
