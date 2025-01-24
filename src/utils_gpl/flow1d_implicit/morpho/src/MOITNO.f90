subroutine MOITNO ( igp    ,isec   ,ngrid  ,x      ,&
&dtm    ,alphac ,inode  ,branch ,&
&ibr    ,nbran  ,grid   ,&
&mopta  ,moptb  ,moptc  ,moptd  ,&
&celer  ,sedtr  ,alphad ,flwdir  ,&
&juer   ,ker    ,intcel&
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
! Module:             MOITNO (MOrphology InTegral at a NOde)
!
! Module description: Calculate time integral in outflow conditions for
!                     a begin or end point of a branch.
!
!                     This routine processes the calculation of the
!                     time integral at a node
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 alphac            I  Stability factor for bottom scheme (>1)
! 18 alphad            I  Limiter constant
!  8 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 16 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
!               1|2       - Normal branches:
!                         (i,1) = Celerity in gridpoint i in main secti-
!                                 on.
!                         - Sedredge branches:
!                         (i,1) = Celerity in gridpoint i,
!                                 left channel.
!                         (i,2) = Celerity in gridpoint i,
!                                 right channel.
!  5 dtm               I  Morphology time step.
! 19 flwdir(ngrid)     I  Indicator for flow direction at each grid
!                         point      1 = positive flow
!                                    0 = zero flow
!                                   -1 = negative flow
! 11 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
!  9 ibr               I  Branch number
!  1 igp               I  Gridpoint number
!  7 inode             I  Node number to be processed
! 22 intcel            O  Calculated integral value for first or last
!                         cel of a branch
!  2 isec              I  Section number (1 or 2)
! 20 juer              P  -
! 21 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 12 mopta             I  Switch used in formulas
! 13 moptb             I  Switch used in formulas
! 14 moptc             I  Switch used in formulas
! 15 moptd             I  Switch used in formulas
! 10 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
! 17 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
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
! error   write an ERROR to the error file.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moitno.pf,v $
! Revision 1.3  1998/06/11  11:47:17  kuipe_j
! Estuary special integrated
!
! Revision 1.2  1996/03/08  09:39:13  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.1  1996/03/07  10:44:22  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
!
!***********************************************************************
!
!     Parameters
!
   integer    igp    ,isec   ,ngrid  ,inode  ,nbran  ,&
   &ibr    ,juer   ,ker

   real       alphac ,intcel, alphad

   integer    grid   (ngrid),&
   &flwdir (ngrid),&
   &branch (4,nbran)

   real       x      (ngrid),&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*)

   double     precision  dtm

   logical    mopta, moptb, moptc, moptd

!
!     Local variables
!
   real       spos1, spos2, spos3, sneg1, sneg2, sneg3,&
   &cpos1, cpos2, cpos3, cneg1, cneg2, cneg3,&
   &dx1, dx2, dx3,&
   &sipos1, sipos2, sineg1, sineg2,&
   &flwpos, flwneg, fuppos, fupneg,&
   &rpos, rneg, ipos, ineg, dtms,&
   &int1, ister
!
   logical    epsequ
   external   epsequ
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
   include '..\include\errcod.i'

   dtms = sngl(dtm)
!
!     Test if at begin or end of a branch ( if there is a structure
!     at igp, then if flow is positive this point is to considered
!     as an end point, otherwise it is to be treated as the beginning
!     of a branch)
!
   if ((branch(2,ibr) .eq. inode) .or. ((grid(igp) .eq. cstrcl) .and.&
!
   &(flwdir(igp) .ge. 0))) then
!
!     End of branch
!
!     First the positive and negative values of
!     the celerities are determined using MIN and MAX functions
!
      cpos1 = MAX(celer(igp-2,isec),0.0)
      cpos2 = MAX(celer(igp-1,isec),0.0)
      cpos3 = MAX(celer(igp,isec),0.0)
      cneg1 = MIN(celer(igp-1,isec),0.0)
      cneg2 = MIN(celer(igp,isec),0.0)
!
! wijziging 18 juni 1997 zie MOITEP
!
!        if ( cpos1 .ge. 0. ) then
      if ( cpos1 .gt. 0. ) then
         spos1 = sedtr(igp-2,isec)
      else
         spos1 = 0.
      endif
      if ( cneg1 .lt. 0. ) then
         sneg1 = sedtr(igp-1,isec)
      else
         sneg1 = 0.
      endif
!
! wijziging zie MOITEP
!
!        if ( cpos2 .ge. 0. ) then
      if ( cpos2 .gt. 0. ) then
         spos2 = sedtr(igp-1,isec)
      else
         spos2 = 0.
      endif
      if ( cneg2 .lt. 0. ) then
         sneg2 = sedtr(igp,isec)
      else
         sneg2 = 0.
      endif
!
! wijziging zie MOITEP
!
!        if ( cpos3 .ge. 0.) then
      if ( cpos3 .gt. 0.) then
         spos3 = sedtr(igp,isec)
      else
         spos3 = 0.
      endif
!
!     Calculate delta x
!
      dx1 = x(igp-1) - x(igp-2)
      dx2 = x(igp) - x(igp-1)
      dx3 = dx2
!
!     Calculate sigma and flux, positive and negative
!
      sipos1 = 0.5 * alphac * (cpos2 + cpos1) * dtms / dx1
      sipos2 = 0.5 * alphac * (cpos3 + cpos2) * dtms / dx2
      sineg1 = 0.5 * alphac * (cneg2 + cneg1) * dtms / dx2
      flwpos = (1. - sipos2) * (spos3 - spos2)
      flwneg = (1. + sineg1) * (sneg2 - sneg1)
      fuppos = (dx2 / dx1 - sipos1) * (spos2 - spos1)
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
      if (moptc) then
         rneg = -0.5
      else
         rneg = alphad + 0.5
      endif
      if (rneg .le. 0.0) then
         ineg = sneg2
      elseif (rneg .ge. alphad) then
!  Voorstel Sloff/Borsboom 7-8-1997
! ideetje:            ineg = sneg2 + alphad * flwneg / 2.0
         ineg = sneg2 - alphad * flwneg / 2.0
      else
         ker = fatal
         call ERROR(juer, 'MOITNO ', emofor, ker)
      endif
!
!     Integral n-1/2 is calculated
!
      int1 = (ineg + ipos) * dtms
!
!     Now Ister is to be determined
!
      fuppos = (dx3 / dx2 - sipos2) * (spos3 - spos2)
      if (mopta) then
         rpos = -0.5
      else
         rpos =  0.5
      endif
      if (rpos .le. 0.0) then
         ipos = spos3
      elseif (rpos .ge. alphad) then
         ker = fatal
         call ERROR(juer, 'MOITNO ', emofor, ker)
      else
         ipos = spos3 + fuppos / 2.0
      endif
      ineg = 0.0
      ister = (ineg + ipos) * dtms

      intcel = (int1 + ister) / 2.0

   else
!
!     Begin of branch
!
!     First the positive and negative values of Sediment transport
!     and celerities are determined using MIN and MAX functions
!
      cpos1 = MAX(celer(igp,isec),0.0)
      cpos2 = MAX(celer(igp+1,isec),0.0)
      cneg1 = MIN(celer(igp,isec),0.0)
      cneg2 = MIN(celer(igp+1,isec),0.0)
      cneg3 = MIN(celer(igp+2,isec),0.0)
!
! wijziging 18 juni 1997 zie MOITEP
!
!        if ( cpos1 .ge. 0. ) then
      if ( cpos1 .gt. 0. ) then
         spos1 = sedtr(igp,isec)
      else
         spos1 = 0.
      endif
      if ( cneg1 .lt. 0. ) then
         sneg1 = sedtr(igp,isec)
      else
         sneg1 = 0.
      endif
!        if ( cpos2 .ge. 0. ) then
!
! wijziging zie MOITEP
!
      if ( cpos2 .gt. 0. ) then
         spos2 = sedtr(igp+1,isec)
      else
         spos2 = 0.
      endif
      if ( cneg2 .lt. 0. ) then
         sneg2 = sedtr(igp+1,isec)
      else
         sneg2 = 0.
      endif
      if ( cneg3 .lt. 0. ) then
         sneg3 = sedtr(igp+2,isec)
      else
         sneg3 = 0.
      endif
!
!     Calculate delta x
!
      dx2 = x(igp+1) - x(igp)
      dx3 = x(igp+2) - x(igp+1)
      dx1 = dx2
!
!     Calculate sigma and flux, positive and negative
!
      sipos1 = 0.5 * alphac * (cpos2 + cpos1) * dtms / dx2
      sineg1 = 0.5 * alphac * (cneg2 + cneg1) * dtms / dx2
      sineg2 = 0.5 * alphac * (cneg3 + cneg2) * dtms / dx3
      flwpos = (1. - sipos1) * (spos2 - spos1)
      flwneg = (1. + sineg1) * (sneg2 - sneg1)
! Aanpassing Kees Sloff 8-8-1997
!         fupneg = (dx2 / dx3 - sineg2) * (sneg3 - sneg2)
      fupneg = (dx2 / dx3 + sineg2) * (sneg3 - sneg2)
!
!     Calculate positive attribute of integral
!
      if (moptd) then
         rpos = -0.5
      else
         rpos = alphad + 0.5
      endif
      if (rpos .le. 0.0) then
         ipos = spos1
      elseif (rpos .ge. alphad) then
         ipos = spos1 + alphad * flwpos / 2.0
      else
         ker = fatal
         call ERROR(juer, 'MOITNO ', emofor, ker)
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
!  Voorstel Sloff/Borsboom 7-8-1997
! ideetje:            ineg = sneg2 + alphad * flwneg / 2.0
         ineg = sneg2 - alphad * flwneg / 2.0
      else
!  Voorstel Sloff/Borsboom 7-8-1997
! ideetje:            ineg = sneg2 + fupneg / 2.0
         ineg = sneg2 - fupneg / 2.0
      endif
!
!     Integral i+1/2 is calculated
!
      int1 = (ineg + ipos) * dtms
!
!     Now Ister is to be determined

!     Aanpassing Kees Sloff 8 juli 1997 in fupneg
!         fupneg = (dx1 / dx2 - sineg1) * (sneg2 - sneg1)
      fupneg = (dx1 / dx2 + sineg1) * (sneg2 - sneg1)

      if (moptb) then
         rneg = -0.5
      else
         rneg =  0.5
      endif
      if (rneg .le. 0.0) then
         ineg = sneg1
      elseif (rneg .ge. alphad) then
         ker = fatal
         call ERROR(juer, 'MOITNO ', emofor, ker)
      else
!  Voorstel Sloff/Borsboom 7-8-1997
!            ineg = sneg1 + fupneg / 2.0
         ineg = sneg1 - fupneg / 2.0
      endif

      ipos = 0.0
      ister = (ineg + ipos) * dtms

      intcel = (int1 + ister) / 2.0
   endif

   return
end
