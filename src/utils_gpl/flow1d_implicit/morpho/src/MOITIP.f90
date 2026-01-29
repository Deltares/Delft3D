subroutine MOITIP ( igp    ,isec   ,ngrid  ,&
&x      ,dtm    ,alphac ,&
&celer  ,sedtr  ,&
&alphad ,intcel&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOITIP (MOrphology InTegral on Intermediate grid Point)
!
! Module description: Calculate integral on point i+1/2
!
!                     The integral Ii+1/2 is determined according to
!                     the formulas described in the Functional Design
!                     Sediment transport and Morphology (document
!                     S-FO-002.3KV Appendix B).
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 alphac            I  Stability factor for bottom scheme (>1)
!  9 alphad            I  Limiter constant
!  7 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
!               1|2       - Normal branches:
!                         (i,1) = Celerity in gridpoint i in main secti-
!                                 on.
!                         - Sedredge branches:
!                         (i,1) = Celerity in gridpoint i,
!                                 left channel.
!                         (i,2) = Celerity in gridpoint i,
!                                 right channel.
!  5 dtm               I  Morphology time step.
!  1 igp               I  Gridpoint number
! 10 intcel            O  Calculated integral value for first or last
!                         cel of a branch
!  2 isec              I  Section number (1 or 2)
!  3 ngrid             I  Number of grid points in network.
!  8 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
!  4 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moitip.pf,v $
! Revision 1.3  1998/06/11  11:47:16  kuipe_j
! Estuary special integrated
!
! Revision 1.2  1996/03/08  09:39:11  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.1  1996/03/07  10:44:21  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
!
!***********************************************************************
!
!     Parameters
!
   integer    igp    ,isec   ,ngrid

   real       alphac ,intcel, alphad

   real       x      (ngrid),&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*)

   double     precision  dtm

!
!     Local variables
!
   real       spos1, spos2, spos3, sneg1, sneg2, sneg3,&
   &cpos1, cpos2, cpos3, cneg1, cneg2, cneg3,&
   &dx1, dx2, dx3,&
   &sipos1, sipos2, sineg1, sineg2,&
   &flwpos, flwneg, fuppos, fupneg,&
   &rpos, rneg, ipos, ineg, dtms
!
   logical    epsequ
   external   epsequ
!
!     Include sobek constants
!
   include '..\include\sobcon.i'

   dtms = sngl(dtm)
!
!     First the positive and negative values of Sediment transport
!     and celerities are determined using MIN and MAX functions
!
   cpos1 = MAX(celer(igp-1,isec),0.0)
   cpos2 = MAX(celer(igp,isec),0.0)
   cpos3 = MAX(celer(igp+1,isec),0.0)
   cneg1 = MIN(celer(igp,isec),0.0)
   cneg2 = MIN(celer(igp+1,isec),0.0)
   cneg3 = MIN(celer(igp+2,isec),0.0)
!
! wijziging 18 juni 1997: zie MOITEP.PF
!
!     if ( cpos1 .ge. 0. ) then
   if ( cpos1 .gt. 0. ) then
      spos1 = sedtr(igp-1,isec)
   else
      spos1 = 0.
   endif
   if ( cneg1 .lt. 0. ) then
      sneg1 = sedtr(igp,isec)
   else
      sneg1 = 0.
   endif
!
! wijziging 18 juni 1997: zie MOITEP.PF
!
!     if ( cpos2 .ge. 0. ) then
   if ( cpos2 .gt. 0. ) then
      spos2 = sedtr(igp,isec)
   else
      spos2 = 0.
   endif
   if ( cneg2 .lt. 0. ) then
      sneg2 = sedtr(igp+1,isec)
   else
      sneg2 = 0.
   endif
!
! wijziging 18 juni 1997: zie MOITEP
!
!     if ( cpos3 .ge. 0. ) then
   if ( cpos3 .gt. 0. ) then
      spos3 = sedtr(igp+1,isec)
   else
      spos3 = 0.
   endif
   if ( cneg3 .lt. 0. ) then
      sneg3 = sedtr(igp+2,isec)
   else
      sneg3 = 0.
   endif
!
!     Calculate delta x
!
   dx1 = x(igp) - x(igp-1)
   dx2 = x(igp+1) - x(igp)
   dx3 = x(igp+2) - x(igp+1)
!
!     Calculate sigma and flux, positive and negative
!
   sipos1 = 0.5 * alphac * (cpos2 + cpos1) * dtms / dx1
   sipos2 = 0.5 * alphac * (cpos3 + cpos2) * dtms / dx2
   sineg1 = 0.5 * alphac * (cneg2 + cneg1) * dtms / dx2
   sineg2 = 0.5 * alphac * (cneg3 + cneg2) * dtms / dx3
   flwpos = (1. - sipos2) * (spos3 - spos2)
   flwneg = (1. + sineg1) * (sneg2 - sneg1)
   fuppos = (dx2 / dx1 - sipos1) * (spos2 - spos1)
   fupneg = (dx2 / dx3 + sineg2) * (sneg3 - sneg2)
!
!     Calculate positive attribute of integral
!
   if (epsequ(flwpos, 0. , cdchk)) then
      if (flwpos .lt. 0.) then
         flwpos = flwpos - cdval
      else
         flwpos = flwpos + cdval
      endif
   endif
   rpos = fuppos / flwpos
   if (rpos .le. 0.0) then
      ipos = spos2
   elseif (rpos .ge. alphad) then
      ipos = spos2 + alphad * flwpos / 2.0
   else
      ipos = spos2 + fuppos / 2.0
   endif
!
!     Calculate negative component of integral
!
   if (epsequ(flwneg, 0. , cdchk)) then
      if (flwneg .lt. 0.) then
         flwneg = flwneg - cdval
      else
         flwneg = flwneg + cdval
      endif
   endif
   rneg = fupneg / flwneg
   if (rneg .le. 0.0) then
      ineg = sneg2
   elseif (rneg .ge. alphad) then
! Voorstel Sloff/Borsboom 7-8-1997
! ideetje:         ineg = sneg2 + alphad * flwneg / 2.0
      ineg = sneg2 - alphad * flwneg / 2.0
   else
! Voorstel Sloff/Borsboom 7-8-1997
! ideetje:         ineg = sneg2 + fupneg / 2.0
      ineg = sneg2 - fupneg / 2.0
   endif
!
!     Finally the integral is calculated
!
   intcel = (ineg + ipos) * dtms
   return
end
