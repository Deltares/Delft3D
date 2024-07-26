      subroutine MOITNO ( igp    ,isec   ,ngrid  ,x      ,
     +                    dtm    ,alphac ,inode  ,branch ,
     +                    ibr    ,nbran  ,grid   ,
     +                    mopta  ,moptb  ,moptc  ,moptd  ,
     +                    celer  ,sedtr  ,alphad ,flwdir  ,
     +                    juer   ,ker    ,intcel
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOITNO (MOrphology InTegral at a NOde)
c
c Module description: Calculate time integral in outflow conditions for
c                     a begin or end point of a branch.
c
c                     This routine processes the calculation of the
c                     time integral at a node
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 alphac            I  Stability factor for bottom scheme (>1)
c 18 alphad            I  Limiter constant
c  8 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 16 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
c               1|2       - Normal branches:
c                         (i,1) = Celerity in gridpoint i in main secti-
c                                 on.
c                         - Sedredge branches:
c                         (i,1) = Celerity in gridpoint i,
c                                 left channel.
c                         (i,2) = Celerity in gridpoint i,
c                                 right channel.
c  5 dtm               I  Morphology time step.
c 19 flwdir(ngrid)     I  Indicator for flow direction at each grid
c                         point      1 = positive flow
c                                    0 = zero flow
c                                   -1 = negative flow
c 11 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c  9 ibr               I  Branch number
c  1 igp               I  Gridpoint number
c  7 inode             I  Node number to be processed
c 22 intcel            O  Calculated integral value for first or last
c                         cel of a branch
c  2 isec              I  Section number (1 or 2)
c 20 juer              P  -
c 21 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 12 mopta             I  Switch used in formulas
c 13 moptb             I  Switch used in formulas
c 14 moptc             I  Switch used in formulas
c 15 moptd             I  Switch used in formulas
c 10 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c 17 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c  4 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c error   write an ERROR to the error file.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: moitno.pf,v $
c Revision 1.3  1998/06/11  11:47:17  kuipe_j
c Estuary special integrated
c
c Revision 1.2  1996/03/08  09:39:13  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.1  1996/03/07  10:44:22  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c
c***********************************************************************
c
c     Parameters
c
      integer    igp    ,isec   ,ngrid  ,inode  ,nbran  ,
     +           ibr    ,juer   ,ker

      real       alphac ,intcel, alphad

      integer    grid   (ngrid),
     +           flwdir (ngrid),
     +           branch (4,nbran)

      real       x      (ngrid),
     +           celer  (ngrid,*),
     +           sedtr  (ngrid,*)

      double     precision  dtm

      logical    mopta, moptb, moptc, moptd

c
c     Local variables
c
      real       spos1, spos2, spos3, sneg1, sneg2, sneg3,
     +           cpos1, cpos2, cpos3, cneg1, cneg2, cneg3,
     +           dx1, dx2, dx3,
     +           sipos1, sipos2, sineg1, sineg2,
     +           flwpos, flwneg, fuppos, fupneg,
     +           rpos, rneg, ipos, ineg, dtms,
     +           int1, ister
c
      logical    epsequ
      external   epsequ
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
      include '..\include\errcod.i'

      dtms = sngl(dtm)
c
c     Test if at begin or end of a branch ( if there is a structure
c     at igp, then if flow is positive this point is to considered
c     as an end point, otherwise it is to be treated as the beginning
c     of a branch)
c
      if ((branch(2,ibr) .eq. inode) .or. ((grid(igp) .eq. cstrcl) .and.
c
     +                  (flwdir(igp) .ge. 0))) then
c
c     End of branch
c
c     First the positive and negative values of
c     the celerities are determined using MIN and MAX functions
c
         cpos1 = MAX(celer(igp-2,isec),0.0)
         cpos2 = MAX(celer(igp-1,isec),0.0)
         cpos3 = MAX(celer(igp,isec),0.0)
         cneg1 = MIN(celer(igp-1,isec),0.0)
         cneg2 = MIN(celer(igp,isec),0.0)
c
c wijziging 18 juni 1997 zie MOITEP
c
c        if ( cpos1 .ge. 0. ) then
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
c
c wijziging zie MOITEP
c
c        if ( cpos2 .ge. 0. ) then
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
c
c wijziging zie MOITEP
c
c        if ( cpos3 .ge. 0.) then
         if ( cpos3 .gt. 0.) then
            spos3 = sedtr(igp,isec)
         else
            spos3 = 0.
         endif
c
c     Calculate delta x
c
         dx1 = x(igp-1) - x(igp-2)
         dx2 = x(igp) - x(igp-1)
         dx3 = dx2
c
c     Calculate sigma and flux, positive and negative
c
         sipos1 = 0.5 * alphac * (cpos2 + cpos1) * dtms / dx1
         sipos2 = 0.5 * alphac * (cpos3 + cpos2) * dtms / dx2
         sineg1 = 0.5 * alphac * (cneg2 + cneg1) * dtms / dx2
         flwpos = (1. - sipos2) * (spos3 - spos2)
         flwneg = (1. + sineg1) * (sneg2 - sneg1)
         fuppos = (dx2 / dx1 - sipos1) * (spos2 - spos1)
c
c     Calculate positive attribute of integral
c
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
c
c     Calculate negative component of integral
c
         if (moptc) then
            rneg = -0.5
         else
            rneg = alphad + 0.5
         endif
         if (rneg .le. 0.0) then
            ineg = sneg2
         elseif (rneg .ge. alphad) then
c  Voorstel Sloff/Borsboom 7-8-1997
c ideetje:            ineg = sneg2 + alphad * flwneg / 2.0
            ineg = sneg2 - alphad * flwneg / 2.0
         else
            ker = fatal
            call ERROR(juer, 'MOITNO ', emofor, ker)
         endif
c
c     Integral n-1/2 is calculated
c
         int1 = (ineg + ipos) * dtms
c
c     Now Ister is to be determined
c
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
c
c     Begin of branch
c
c     First the positive and negative values of Sediment transport
c     and celerities are determined using MIN and MAX functions
c
         cpos1 = MAX(celer(igp,isec),0.0)
         cpos2 = MAX(celer(igp+1,isec),0.0)
         cneg1 = MIN(celer(igp,isec),0.0)
         cneg2 = MIN(celer(igp+1,isec),0.0)
         cneg3 = MIN(celer(igp+2,isec),0.0)
c
c wijziging 18 juni 1997 zie MOITEP
c
c        if ( cpos1 .ge. 0. ) then
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
c        if ( cpos2 .ge. 0. ) then
c
c wijziging zie MOITEP
c
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
c
c     Calculate delta x
c
         dx2 = x(igp+1) - x(igp)
         dx3 = x(igp+2) - x(igp+1)
         dx1 = dx2
c
c     Calculate sigma and flux, positive and negative
c
         sipos1 = 0.5 * alphac * (cpos2 + cpos1) * dtms / dx2
         sineg1 = 0.5 * alphac * (cneg2 + cneg1) * dtms / dx2
         sineg2 = 0.5 * alphac * (cneg3 + cneg2) * dtms / dx3
         flwpos = (1. - sipos1) * (spos2 - spos1)
         flwneg = (1. + sineg1) * (sneg2 - sneg1)
c Aanpassing Kees Sloff 8-8-1997
c         fupneg = (dx2 / dx3 - sineg2) * (sneg3 - sneg2)
         fupneg = (dx2 / dx3 + sineg2) * (sneg3 - sneg2)
c
c     Calculate positive attribute of integral
c
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
c
c     Calculate negative component of integral
c
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
c  Voorstel Sloff/Borsboom 7-8-1997
c ideetje:            ineg = sneg2 + alphad * flwneg / 2.0
            ineg = sneg2 - alphad * flwneg / 2.0
         else
c  Voorstel Sloff/Borsboom 7-8-1997
c ideetje:            ineg = sneg2 + fupneg / 2.0
            ineg = sneg2 - fupneg / 2.0
         endif
c
c     Integral i+1/2 is calculated
c
         int1 = (ineg + ipos) * dtms
c
c     Now Ister is to be determined

c     Aanpassing Kees Sloff 8 juli 1997 in fupneg
c         fupneg = (dx1 / dx2 - sineg1) * (sneg2 - sneg1)
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
c  Voorstel Sloff/Borsboom 7-8-1997
c            ineg = sneg1 + fupneg / 2.0
            ineg = sneg1 - fupneg / 2.0
         endif

         ipos = 0.0
         ister = (ineg + ipos) * dtms

         intcel = (int1 + ister) / 2.0
      endif

      return
      end
