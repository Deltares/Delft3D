      subroutine MOITEP ( igpbou ,igpcel ,isec   ,ngrid  ,nbran  ,
     +                    nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +                    node   ,ibrtyp ,mbdpar ,hlev   ,
     +                    grid   ,maxtab ,ntabm  ,ntab   ,
     +                    table  ,h      ,wf     ,wfh0   ,
     +                    ws     ,wft    ,afs    ,dissed ,
     +                    x      ,time   ,dtm    ,alphac ,
     +                    celer  ,sedtr  ,intbou ,flwdir ,
     +                    alphad ,moptc  ,moptf  ,intcel ,
     +                    iextra ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOITEP (MOrphology InTegral on End Point)
c
c Module description: Calculate integral on point n-1/2
c
c                     The integral In-1/2 is determined according to the
c                     formulas described in the Funtional Design
c                     Sediment Transport and Morphology (document
c                     S-FO-002.3KV Appendix B).
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 25 afs(ngrid,2)      I  Actual flow area per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c 30 alphac            I  Stability factor for bottom scheme (>1)
c 35 alphad            I  Limiter constant
c  9 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 31 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
c               1|2       - Normal branches:
c                         (i,1) = Celerity in gridpoint i in main secti-
c                                 on.
c                         - Sedredge branches:
c                         (i,1) = Celerity in gridpoint i,
c                                 left channel.
c                         (i,2) = Celerity in gridpoint i,
c                                 right channel.
c 26 dissed(4,nbran)   I  Redistributed sediment transport at begin and
c                         end of branches. At the outflow side of the
c                         branch the calculated transports are stored.
c                         At the inflow side the redistributed trans-
c                         ports are stored.
c                         (1,i)   Transport at section 1 (main or left
c                                 channel)  at begin of branch.
c                         (2,i)   Transport at section 2 (right channel)
c                                 at begin of branch.
c                         (3,i)   Transport at section 1 (main or left
c                                 channel)  at end of branch.
c                         (4,i)   Transport at section 2 (right channel)
c                                 at end of branch.
c 29 dtm               I  Morphology time step.
c 34 flwdir(ngrid)     I  Indicator for flow direction at each grid
c                         point      1 = positive flow
c                                    0 = zero flow
c                                   -1 = negative flow
c 15 grid              P  -
c 20 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c 14 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c 10 ibr               I  Branch number
c 12 ibrtyp            I  Type of branch
c                           ccrtab (1) : tabulated branch
c                           ccrcir (2) : circle branch
c                           ccrsed (3) : sedredge branch
c  1 igpbou            I  Calculated integral value on boundary
c  2 igpcel            I  Calculated integral for first or last cell in
c                         branch
c 33 intbou            I  Integral value for begin or end point of a
c                         branch
c 40 intcel            O  Calculated integral value for first or last
c                         cel of a branch
c  3 isec              I  Section number (1 or 2)
c 38 juer              P  -
c 39 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 16 maxtab            I  Maximum number of defined tables.
c 13 mbdpar(5,nboun)   I  Morphodynamic boundary conditions:
c                         (1,i) = Type of boundary condition:
c                                 cmbsft (1) : Condition S=f(t).
c                                 cmbsfq (2) : Condition S=f(Q).
c                                 cmbzft (3) : Condition z=f(t).
c                         (2,i) = Location (node number).
c                         (3,i) = Branch number that is connected.
c                         (4,i) = Table pointer for boundary table. In
c                                 case of a connected sedredge branch
c                                 the pointer will be assigned to the
c                                 left channel.
c                         (5,i) = Table pointer for boundary table. In
c                                 case of a connected sedredge branch
c                                 the pointer will be assigned to the
c                                 right channel. In other cases undefi-
c                                 ned.
c 36 moptc             I  Switch used in formulas
c 37 moptf             I  Switch used in formulas
c  6 nboun             I  Number of boundary nodes.
c  5 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  7 nnode             I  Number of nodes.
c 11 node(4,nnode)     I  Definition of nodes:
c                         (1,i) = Type of node i:
c                                 cintnd (1) : Internal node
c                                 chbou  (2) : H-boundary
c                                 cqbou  (3) : Q-boundary
c                                 cqhbou (4) : QH-boundary
c                                 chqbou (5) : HQ-boundary
c                         (2,i) = Gridpoint in case of boundary, else
c                                 undefined.
c                         (3,i) = Station number for boundary, undefined
c                                 for internal nodes:
c                                 HQ, H-boundary: station nr H-station.
c                                 QH, Q-boundary: station nr Q-station.
c                         (4,i) = Boundary number in case of boundary.
c 18 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                         to maxtab. For a specific table number k and
c                         function Y = f (X) the following definitions
c                         exist:
c                         (1,k) = Length of table k.
c                         (2,k) = Start address X in table.
c                         (3,k) = Start address Y in table.
c                         (4,k) = Access method and period control: xy
c                                 x = ctbnpf (0) : No period defined
c                                 x = ctbpfu (1) : Period defined
c                                 y = ctbico (0) : Continue interpltn
c                                 y = ctbidi (1) : Discrete interpltn
c 17 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 32 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
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
c 19 table             P  -
c 28 time              I  Actual time level tn+1. in sec.
c 21 wf(ngrid)         I  Actual flow width at every grid point.
c 22 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
c                         grid point.
c 24 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
c                                 point i.
c                         - For a circle cross section:
c                         (i,1) = Radius of the circle.
c                         - For a sedredge cross section:
c                         (i,1) = Width of main section (i.e. left chan-
c                                 nel).
c                         (i,2) = Width of sub section 1 (i.e. right
c                                 channel).
c 23 ws(ngrid)         I  Sediment transporting width for each grid
c                         point.
c 27 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c error   write an ERROR to the error file.
c inttab  INTerpolate in TABle
c moiflh  MOrphology Integral on First or Last halve point
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: moitep.pf,v $
c Revision 1.7  1999/04/22  14:23:22  kuipe_j
c Correct bug in overflow correction
c
c Revision 1.6  1999/04/22  08:43:45  kuipe_j
c Avoid overflow if transport is zero
c
c Revision 1.5  1999/03/15  15:52:56  kuipe_j
c tabs removed
c
c Revision 1.4  1998/06/11  11:47:15  kuipe_j
c Estuary special integrated
c
c Revision 1.3  1996/05/28  13:30:08  kuipe_j
c Error message courant nr added
c
c Revision 1.2  1996/03/08  09:39:10  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.1  1996/03/07  10:44:20  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c
c***********************************************************************
c
c     Parameters
c
      integer    igpbou ,igpcel ,isec   ,ngrid  ,ibr    ,nbran  ,
     +           nboun  ,nnode  ,maxtab ,ntabm  ,ibrtyp ,
     +           maxlev ,juer   ,ker
      integer    branch (4,nbran),
     +           grid   (ngrid),
     +           node   (4,nnode),
     +           mbdpar (5,nboun),
     +           ntab   (4,maxtab),
     +           flwdir (ngrid)

      real       alphac ,intcel, alphad, intbou, iextra

      real       x      (ngrid),
     +           table  (ntabm),
     +           celer  (ngrid,*),
     +           sedtr  (ngrid,*),
     +           dissed (4,nbran),
     +           wf     (ngrid),
     +           wfh0   (ngrid),
     +           ws     (ngrid),
     +           wft    (ngrid,maxlev),
     +           afs    (ngrid)

      double precision  time, dtm, hlev(ngrid,maxlev), h(ngrid)

      logical    moptc, moptf

c
c     Local variables
c
      integer    ixdis, inode, iboun, itab

      real       spos1, spos2, spos3, sneg1, sneg2,
     +           cpos1, cpos2,cpos3, cneg1, cneg2,
     +           dx1, dx2, iori ,depth ,sedtrw ,srat ,
     +           bb ,rbb ,alpcel ,cboun ,siboun ,
     +           sipos1, sipos2, sineg1,
     +           flwpos, flwneg, fuppos, fupneg,
     +           rpos, rneg, ipos, ineg, dtms,
     +           sinp, sindt, snegin, snegdt
      double precision     rnegd
c
      logical    epsequ,equal
      external   epsequ,equal
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
      include '..\include\errcod.i'
c
      dtms = sngl(dtm)
c
c     First the positive and negative values of the
c     celerities are determined using MIN and MAX functions
c     igpcel is the last gridpoint before the node, igpbou is the
c     gridpoint number of the node
c
      cpos1 = MAX(celer(igpcel-1,isec),0.)
      cpos2 = MAX(celer(igpcel,isec),0.)
      cpos3 = MAX(celer(igpbou,isec),0.)
      cneg1 = MIN(celer(igpcel,isec),0.)
      cneg2 = MIN(celer(igpbou,isec),0.)
c
c wijziging Robbert Fokkink 18 juni 1997
c het criterium moet groter dan zijn
c
c     if ( cpos1 .ge. 0. ) then
      if ( cpos1 .gt. 0. ) then
         spos1 = sedtr(igpcel-1,isec)
      else
         spos1 = 0.
      endif
      if ( cneg1 .lt. 0. ) then
         sneg1 = sedtr(igpcel,isec)
      else
         sneg1 = 0.
      endif
c
c wijziging als hierboven
c
c     if (cpos2 .ge. 0. ) then
      if (cpos2 .gt. 0. ) then
         spos2 = sedtr(igpcel,isec)
      else
         spos2 = 0.
      endif
      if (cneg2 .lt. 0. ) then
         sneg2 = sedtr(igpbou,isec)
      else
         sneg2 = 0.
      endif
c
c wijziging 18 juni 1997, als boven
c
c     if ( cpos3 .ge. 0. ) then
      if ( cpos3 .gt. 0. ) then
         spos3 = sedtr(igpbou,isec)
      else
         spos3 = 0.
      endif
c
c     Calculate delta x
c
      dx1 = x(igpcel) - x(igpcel-1)
      dx2 = x(igpbou) - x(igpcel)
c
c     Calculate sigma and flux, positive and negative
c
      sipos1 = 0.5 * alphac * (cpos2 + cpos1) * dtms / dx1
      sineg1 = 0.5 * alphac * (cneg2 + cneg1) * dtms / dx2
      sipos2 = 0.5 * alphac * (cpos3 + cpos2) * dtms / dx2
      flwpos = (1. - sipos2) * (spos3 - spos2)
      flwneg = (1. + sineg1) * (sneg2 - sneg1)
      fuppos = (dx2 / dx1 - sipos1) * (spos2 - spos1)
c
      if (flwdir(igpbou) .ge. 0) then        
c
c        Positive flow direction
c
c        Calculate positive attribute of integral
c
         if (epsequ(flwpos, 0. , cdchk)) then
            if (flwpos .lt. 0.) then
               flwpos = flwpos - cdval
            else
               flwpos = flwpos + cdval
            endif
         endif
c       Abs(Fuppos) will be probably always < 1. (Kuipers)
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
            call ERROR(juer, 'MOITEP ', emofor, ker)
         endif
c
c     Finally the integral is calculated
c
         intcel = (ineg + ipos) * dtms
c
      else                 
c
c        Negative flow direction
c
c        Calculate positive component of integral
c
         if (epsequ(flwpos, 0. , cdchk)) then
            if (flwpos .lt. 0.) then
               flwpos = flwpos - cdval
            else
               flwpos = flwpos + cdval
            endif
         endif
c       Abs(Fuppos) will be probably always < 1. (Kuipers)
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
         if (moptf) then     
c
c           Old formulas
c
            call MOIFLH(ibr    ,igpbou ,igpcel ,isec   ,
     +                  ngrid  ,nbran  ,nboun  ,nnode  ,
     +                  maxlev ,branch ,node   ,ibrtyp ,
     +                  mbdpar ,x      ,hlev   ,grid   ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,
     +                  time   ,dtm    ,alphac ,h      ,
     +                  wf     ,wfh0   ,ws     ,wft    ,
     +                  afs    ,celer  ,sedtr  ,dissed ,
     +                  intbou ,iori   ,flwdir
     +                  )

         else
c
c     Determine imposed sediment transport
c
            if (branch(4,ibr) .eq. igpbou) then
c
c              node
c
               ixdis = isec + 2                 
               inode = branch(2,ibr)
               iboun = node(4,inode)
            else
c
c              structure
c
               ixdis = 0 
               inode = 0
               iboun = -1
            endif
            if (iboun .gt. 0) then
               if (mbdpar(1,iboun) .eq. cmbsft) then
                  itab = mbdpar(3+isec,iboun)
                  call inttab ( ntab(1,itab),
     +                    ntab(4,itab),
     +                    table(ntab(2,itab)),
     +                    table(ntab(3,itab)),
     +                    time,
     +                    sinp
     +                  )
                  call inttab (ntab(1,itab),
     +                    ntab(4,itab),
     +                    table(ntab(2,itab)),
     +                    table(ntab(3,itab)),
     +                    time + dtm,
     +                    sindt
     +                  )
               elseif(mbdpar(1,iboun) .eq. cmbsfq) then
                  sinp = dissed(ixdis,ibr)
                  sindt = sinp
               elseif(mbdpar(1,iboun) .eq. cmbzft) then
                  sinp = sedtr(igpbou,isec)
                  sindt = sinp
               else
                  sinp = sedtr(igpbou,isec)
                  sindt = sinp
               endif
            else
               sinp = intbou/dtms
               sindt = sinp
            endif
            if (iboun.gt.0) then
c              For boundaries
               snegin = MIN(sinp,0.0)
               snegdt = MIN(sindt,0.0)
            else
c              For inflow in branch at internal nodes and structures
               if (iboun.eq.-1) then
                  snegin = sedtr(igpbou,isec)
               else
                  snegin = dissed(ixdis,ibr)
               endif         
               snegdt = 2.0 * iextra / dtms - sneg2
            endif
c
c        Determine depth
c
            if (ibrtyp .eq. ccrtab) then
c
c        Tabulated cross section
c
               if (wf(igpbou) .ge. wfh0(igpbou)) then
                  depth = afs(igpbou) / wfh0(igpbou)
               else
                  depth = afs(igpbou) / wf(igpbou)
               endif
c
c        Determine sediment transport width
c
               if (wf(igpbou) .gt. ws(igpbou)) then
                  sedtrw = ws(igpbou)
               else
                  sedtrw = wf(igpbou)
               endif

            elseif (ibrtyp .eq. ccrsed) then
c
c        Sedredge cross section
c
               depth = h(igpbou) - hlev(igpbou,isec)
c
c        Determine sediment transport width
c
               sedtrw = wft(igpbou,isec)

            endif
c
c        Determine power bb
c
c wijziging 20 juni Robbert Fokkink Kees Sloff
c
c           if (abs(sedtr(igpbou,isec)) .gt. 1.e-10) then
            if 
     &        ((abs(sedtr(igpbou,isec)) .gt. 1.e-10)
     &          .AND. .not.equal(cneg2,0.0)) then
               srat = snegin / sneg2
               bb   = depth * cneg2 / (sneg2 / sedtrw)
               rbb  = 1./bb

c        Prevention for overflow of alpcel
               if (srat .gt.1e-5) then
c
                  if (alog10(srat)*rbb .lt.10.) then
                     alpcel = srat**rbb
                  else
                     alpcel = 1.e10
                  endif
               else
c
c wijziging: alpcel moet in dit geval heel klein zijn
c
c                 alpcel = 1.e10
                  alpcel = 1.e-10
               endif
            else
c
c        Local transport at boundary point is zero
c
               alpcel = 1.e10
            endif
c
c        Determine shock celerity
c
            if (abs(alpcel-1.) .lt. 0.001) then
               cboun = cneg2
            else
               cboun = (alpcel/(alpcel-1.))*
     +              (snegin/sedtrw - sneg2/sedtrw) / depth
            endif
c
c        Determine negative integral
c
c Wijziging Kees Sloff 11 juli 1997
c            siboun =  cboun * dtms / dx1
            siboun =  cboun * dtms / dx2

            if (epsequ(siboun, 0., cdchk)) then
               if (siboun .lt. 0.) then
                  siboun = siboun - cdval
               else
                  siboun = siboun + cdval
               endif
            endif

c Tijdelijke uitvoer Kees Sloff
c           if (abs(siboun) .lt. 0.1) then
c               write(*,*) '  Courant Endp.: brn=',
c    +      ibr,' value:',siboun,' dtm ',dtm,dtms,' time (s):',time
c           endif

c Wijziging  Kees Sloff 8 juli  1997 (let op siboun is negatief)
c             fupneg = -2. * (snegin - sneg2) + (1./siboun - 1.) *
c     +                     (snegin - snegdt)
            siboun = abs(siboun)
c           Oorspronkelijk aangepaste statement weer terug
c           (kuipers 16-8-97)
            fupneg = -2. * (snegin - sneg2) - (1./siboun - 1.) *
     +                     (snegin - snegdt)
c
            if (epsequ(flwneg, 0. , cdchk)) then
               if (flwneg .lt. 0.) then
                  flwneg = flwneg - cdval
               else
                  flwneg = flwneg + cdval
               endif
            endif
c provision against overflow (Kuipers 16-4-99)
            rnegd = dble(fupneg) / dble(flwneg)
            if (rnegd.gt.0.d0) then
               rneg = min(rnegd,1.d20)
            else
               rneg = max(rnegd,-1.d20)
            endif

            if (rneg .le. 0.0) then
               ineg = sneg2
            elseif (rneg .ge. alphad) then
c  Voorstel Sloff/Borsboom 7-8-1997
c ideetje:               ineg = sneg2 + flwneg / 2.0
               ineg = sneg2 - alphad * flwneg / 2.0
            else
c  Voorstel Sloff/Borsboom 7-8-1997
c ideetje:               ineg = sneg2 + fupneg / 2.0
               ineg = sneg2 - fupneg / 2.0
            endif

            iori = ineg * dtms
         endif

         intcel = iori + ipos * dtms

      endif

      return
      end
