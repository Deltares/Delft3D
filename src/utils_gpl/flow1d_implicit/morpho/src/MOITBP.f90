subroutine MOITBP ( igpbou ,igpcel ,isec   ,ngrid  ,nbran  ,&
&nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
&node   ,ibrtyp ,mbdpar ,hlev   ,&
&grid   ,maxtab ,ntabm  ,ntab   ,&
&table  ,h      ,wf     ,wfh0   ,&
&ws     ,wft    ,afs    ,dissed ,&
&x      ,time   ,dtm    ,alphac ,&
&celer  ,sedtr  ,intbou ,flwdir ,&
&alphad ,moptd  ,mopte  ,intcel ,&
&iextra ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOITBP (MORPHology InTegral on Begin Point)
!
! Module description: Calculate integral on point 3/2.
!
!                     The integral I 3/2 is calculated according to the
!                     formulas described in the Functional Design
!                     Sediment Transport and Morphology (document
!                     S-FO-002.3KV Appendix B).
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 25 afs(ngrid,2)      I  Actual flow area per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
! 30 alphac            I  Stability factor for bottom scheme (>1)
! 35 alphad            I  Limiter constant
!  9 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 31 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
!               1|2       - Normal branches:
!                         (i,1) = Celerity in gridpoint i in main secti-
!                                 on.
!                         - Sedredge branches:
!                         (i,1) = Celerity in gridpoint i,
!                                 left channel.
!                         (i,2) = Celerity in gridpoint i,
!                                 right channel.
! 26 dissed(4,nbran)   I  Redistributed sediment transport at begin and
!                         end of branches. At the outflow side of the
!                         branch the calculated transports are stored.
!                         At the inflow side the redistributed trans-
!                         ports are stored.
!                         (1,i)   Transport at section 1 (main or left
!                                 channel)  at begin of branch.
!                         (2,i)   Transport at section 2 (right channel)
!                                 at begin of branch.
!                         (3,i)   Transport at section 1 (main or left
!                                 channel)  at end of branch.
!                         (4,i)   Transport at section 2 (right channel)
!                                 at end of branch.
! 29 dtm               I  Morphology time step.
! 34 flwdir(ngrid)     I  Indicator for flow direction at each grid
!                         point      1 = positive flow
!                                    0 = zero flow
!                                   -1 = negative flow
! 15 grid              P  -
! 20 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
! 14 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
! 10 ibr               I  Branch number
! 12 ibrtyp            I  Type of branch
!                           ccrtab (1) : tabulated branch
!                           ccrcir (2) : circle branch
!                           ccrsed (3) : sedredge branch
!  1 igpbou            I  Calculated integral value on boundary
!  2 igpcel            I  Calculated integral for first or last cell in
!                         branch
! 33 intbou            I  Integral value for begin or end point of a
!                         branch
! 40 intcel            O  Calculated integral value for first or last
!                         cel of a branch
!  3 isec              I  Section number (1 or 2)
! 38 juer              P  -
! 39 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 16 maxtab            I  Maximum number of defined tables.
! 13 mbdpar(5,nboun)   I  Morphodynamic boundary conditions:
!                         (1,i) = Type of boundary condition:
!                                 cmbsft (1) : Condition S=f(t).
!                                 cmbsfq (2) : Condition S=f(Q).
!                                 cmbzft (3) : Condition z=f(t).
!                         (2,i) = Location (node number).
!                         (3,i) = Branch number that is connected.
!                         (4,i) = Table pointer for boundary table. In
!                                 case of a connected sedredge branch
!                                 the pointer will be assigned to the
!                                 left channel.
!                         (5,i) = Table pointer for boundary table. In
!                                 case of a connected sedredge branch
!                                 the pointer will be assigned to the
!                                 right channel. In other cases undefi-
!                                 ned.
! 36 moptd             I  Switch used in formulas
! 37 mopte             I  Switch used in formulas
!  6 nboun             I  Number of boundary nodes.
!  5 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
!  7 nnode             I  Number of nodes.
! 11 node(4,nnode)     I  Definition of nodes:
!                         (1,i) = Type of node i:
!                                 cintnd (1) : Internal node
!                                 chbou  (2) : H-boundary
!                                 cqbou  (3) : Q-boundary
!                                 cqhbou (4) : QH-boundary
!                                 chqbou (5) : HQ-boundary
!                         (2,i) = Gridpoint in case of boundary, else
!                                 undefined.
!                         (3,i) = Station number for boundary, undefined
!                                 for internal nodes:
!                                 HQ, H-boundary: station nr H-station.
!                                 QH, Q-boundary: station nr Q-station.
!                         (4,i) = Boundary number in case of boundary.
! 18 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
! 17 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 32 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
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
! 19 table             P  -
! 28 time              I  Actual time level tn+1. in sec.
! 21 wf(ngrid)         I  Actual flow width at every grid point.
! 22 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
!                         grid point.
! 24 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width of main section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
! 23 ws(ngrid)         I  Sediment transporting width for each grid
!                         point.
! 27 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
! error   write an ERROR to the error file.
! inttab  INTerpolate in TABle
! moiflh  MOrphology Integral on First or Last halve point
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moitbp.pf,v $
! Revision 1.6  1999/04/22  08:43:44  kuipe_j
! Avoid overflow if transport is zero
!
! Revision 1.5  1999/03/15  15:52:54  kuipe_j
! tabs removed
!
! Revision 1.4  1998/06/11  11:47:14  kuipe_j
! Estuary special integrated
!
! Revision 1.3  1996/05/28  13:30:07  kuipe_j
! Error message courant nr added
!
! Revision 1.2  1996/03/08  09:39:09  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.1  1996/03/07  10:44:19  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
!
!***********************************************************************
!
!     Parameters
!
   integer    igpbou ,igpcel ,isec   ,ngrid  ,ibr    ,nbran  ,&
   &nboun  ,nnode  ,maxtab ,ntabm  ,ibrtyp ,&
   &maxlev ,juer   ,ker
   integer    branch (4,nbran),&
   &grid   (ngrid),&
   &node   (4,nnode),&
   &mbdpar (5,nboun),&
   &ntab   (4,maxtab),&
   &flwdir (ngrid)

   real       alphac ,intcel, alphad, intbou, iextra

   real       x      (ngrid),&
   &table  (ntabm),&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*),&
   &dissed (4,nbran),&
   &wf     (ngrid),&
   &wfh0   (ngrid),&
   &ws     (ngrid),&
   &wft    (ngrid,maxlev),&
   &afs    (ngrid)

   double precision  time, dtm, hlev(ngrid,maxlev), h(ngrid)


   logical    moptd, mopte

!
!     Local variables
!
   integer    ixdis, inode, iboun, itab

   real       spos1, spos2, sneg1, sneg2, sneg3,&
   &cpos1, cpos2, cneg1, cneg2, cneg3,&
   &dx1, dx2,&
   &iori, sedtrw ,srat ,bb ,rbb ,alpcel ,&
   &depth ,cboun ,siboun ,&
   &sipos1,  sineg1, sineg2,&
   &flwpos, flwneg, fuppos, fupneg,&
   &rpos, rneg, ipos, ineg, dtms,&
   &sinp, sindt, sposin, sposdt
   double precision     rposd
!
   logical    epsequ,equal
   external   epsequ,equal
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
   include '..\include\errcod.i'

   dtms = sngl(dtm)
!
!     First the positive and negative values of
!     celerities are determined using MIN and MAX functions
!
   cpos1 = MAX(celer(igpbou,isec),0.)
   cpos2 = MAX(celer(igpcel,isec),0.)
   cneg1 = MIN(celer(igpbou,isec),0.)
   cneg2 = MIN(celer(igpcel,isec),0.)
   if ( igpcel == ngrid ) then
      cneg3 = 0
   else
      cneg3 = MIN(celer(igpcel+1,isec),0.)
   endif
!     if ( cpos1 .ge. 0. ) then
!
! wijziging 17 juni 1997, Robbert Fokkink Kees Sloff
!
   if ( cpos1 .gt. 0. ) then
      spos1 = sedtr(igpbou,isec)
   else
      spos1 = 0.
   endif
   if ( cneg1 .lt. 0. ) then
      sneg1 = sedtr(igpbou,isec)
   else
      sneg1 = 0.
   endif
!     if ( cpos2 .ge. 0. ) then
!
! wijziging 17 juni 1997, Robbert Fokkink Kees Sloff
!
   if ( cpos2 .gt. 0. ) then
      spos2 = sedtr(igpcel,isec)
   else
      spos2 = 0.
   endif
   if (cneg2 .lt. 0. ) then
      sneg2 = sedtr(igpcel,isec)
   else
      sneg2 = 0.
   endif
   if ( cneg3 .lt. 0. ) then
      sneg3 = sedtr(igpcel+1,isec)
   else
      sneg3 = 0.
   endif
!
!     Calculate delta x
!
   dx1 = x(igpcel) - x(igpbou)
   if ( igpcel == ngrid ) then
      dx2 = dx1
   else
      dx2 = x(igpcel+1) - x(igpcel)
   end if
!
!     Calculate sigma and flux, positive and negative
!
   sipos1 = 0.5 * alphac * (cpos2 + cpos1) * dtms / dx1
   sineg1 = 0.5 * alphac * (cneg2 + cneg1) * dtms / dx1
   sineg2 = 0.5 * alphac * (cneg3 + cneg2) * dtms / dx2
   flwpos = (1. - sipos1) * (spos2 - spos1)
   flwneg = (1. + sineg1) * (sneg2 - sneg1)
   fupneg = (dx1 / dx2 + sineg2) * (sneg3 - sneg2)
!
!
!
   if (flwdir(igpbou) .lt. 0) then
!
!        Negative flow direction
!
!        Calculate positive attribute of integral
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
         call ERROR(juer, 'MOITBP ', emofor, ker)
      endif
!
!        Calculate negative component of integral
!
      if (epsequ(flwneg, 0. , cdchk)) then
         if (flwneg .lt. 0.) then
            flwneg = flwneg - cdval
         else
            flwneg = flwneg + cdval
         endif
      endif
!        Abs(Fupneg) will be probably always < 1. (Kuipers)
      rneg = fupneg / flwneg
      if (rneg .le. 0.0) then
         ineg = sneg2
      elseif (rneg .ge. alphad) then
! Voorstel Sloff/Borsboom 7-8-1997
!ideetje :            ineg = sneg2 + alphad * flwneg / 2.0
         ineg = sneg2 - alphad * flwneg / 2.0
      else
! Voorstel Sloff/Borsboom 7-8-1997
! ideetje:            ineg = sneg2 + fupneg / 2.0
         ineg = sneg2 - fupneg / 2.0
      endif
!
!     Finally the integral is calculated
!
      intcel = (ineg + ipos) * dtms
!
   else

!
!        Positive flow direction
!
!        Calculate negative component of integral
!
      if (epsequ(flwneg, 0. , cdchk)) then
         if (flwneg .lt. 0.) then
            flwneg = flwneg - cdval
         else
            flwneg = flwneg + cdval
         endif
      endif
!        Abs(Fupneg) will be probably always < 1. (Kuipers)
      rneg = fupneg / flwneg
      if (rneg .le. 0.0) then
         ineg = sneg2
      elseif (rneg .ge. alphad) then
! Voorstel Sloff/Borsboom 7-8-1997
! ideetje:            ineg = sneg2 + alphad * flwneg / 2.0
         ineg = sneg2 - alphad * flwneg / 2.0
      else
! Voorstel Sloff/Borsboom 7-8-1997
! ideetje:            ineg = sneg2 + fupneg / 2.0
         ineg = sneg2 - fupneg / 2.0
      endif
!
!     Calculate positive component of integral
!
      if (mopte) then
!
!           Old formulas
!
         call MOIFLH(ibr    ,igpbou ,igpcel ,isec   ,&
         &ngrid  ,nbran  ,nboun  ,nnode  ,&
         &maxlev ,branch ,node   ,ibrtyp ,&
         &mbdpar ,x      ,hlev   ,grid   ,&
         &maxtab ,ntabm  ,ntab   ,table  ,&
         &time   ,dtm    ,alphac ,h      ,&
         &wf     ,wfh0   ,ws     ,wft    ,&
         &afs    ,celer  ,sedtr  ,dissed ,&
         &intbou ,iori   ,flwdir&
         &)

      else
!
!     Determine imposed sediment transport
!
         if (branch(3,ibr) .eq. igpbou) then
!
!              At a node
!
            ixdis = isec
            inode = branch(1,ibr)
            iboun = node(4,inode)
         else
!
!              At a structure
!
            ixdis = 0
            inode = 0
            iboun = -1
         endif

         if (iboun .gt. 0) then
            if (mbdpar(1,iboun) .eq. cmbsft) then
               itab = mbdpar(3+isec,iboun)
               call inttab ( ntab(1,itab),&
               &ntab(4,itab),&
               &table(ntab(2,itab)),&
               &table(ntab(3,itab)),&
               &time,&
               &sinp&
               &)
               call inttab ( ntab(1,itab),&
               &ntab(4,itab),&
               &table(ntab(2,itab)),&
               &table(ntab(3,itab)),&
               &time + dtm,&
               &sindt&
               &)
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
!
         if (iboun.gt.0) then
!              For boundaries
            sposin = MAX(sinp,0.0)
            sposdt = MAX(sindt,0.0)
         else
!              For inflow in branch at internal nodes and structures
            if (iboun.eq.-1) then
               sposin = sedtr(igpbou,isec)
            else
               sposin = dissed(ixdis,ibr)
            endif
            sposdt = 2.0 * iextra / dtms - spos1
         endif
!
!        Determine depth
!
         if (ibrtyp .eq. ccrtab) then
!
!        Tabulated cross section
!
            if (wf(igpbou) .ge. wfh0(igpbou)) then
               depth = afs(igpbou) / wfh0(igpbou)
            else
               depth = afs(igpbou) / wf(igpbou)
            endif
!
!        Determine sediment transport width
!
            if (wf(igpbou) .gt. ws(igpbou)) then
               sedtrw = ws(igpbou)
            else
               sedtrw = wf(igpbou)
            endif

         elseif (ibrtyp .eq. ccrsed) then
!
!        Sedredge cross section
!
            depth = h(igpbou) - hlev(igpbou,isec)
!
!        Determine sediment transport width
!
            sedtrw = wft(igpbou,isec)

         endif
!
!        Determine power bb
!
!           !!! condition changed !!! Koster - sept. 1996 !!!!
         if (abs(sedtr(igpbou,isec)) .gt. 1.e-10 .and.&
         &.not.equal(cpos1,0.0)                      ) then
            srat = sposin / spos1
            bb   = depth * cpos1 / (spos1 / sedtrw)
            rbb  = 1./bb

!        Prevention for overflow of alpcel
            if (srat .gt.1e-5) then
!
               if (alog10(srat)*rbb .lt.10.) then
                  alpcel = srat**rbb
               else
                  alpcel = 1.e10
               endif
            else
!
! wijziging juni 1997 Robbert Fokkink Kees Sloff:
! alpcel moet in dit geval heel klein zijn
!
!                 alpcel = 1.e10
               alpcel = 1.e-10
            endif
         else
!
!        Local transport at boundary point is zero
!
            alpcel = 1.e10
         endif
!
!        Determine shock celerity
!
         if (abs(alpcel-1.) .lt. 0.001) then
            cboun = cpos1
         else
            cboun = (alpcel/(alpcel-1.))*&
            &(sposin/sedtrw - spos1/sedtrw) / depth
         endif
!
!        Determine positive integral
!
         siboun =  cboun * dtms / dx1
         if (epsequ(siboun, 0., cdchk)) then
            if (siboun .lt. 0.) then
               siboun = siboun - cdval
            else
               siboun = siboun + cdval
            endif
         endif

! Tijdelijke uitvoer Kees Sloff
!           if (abs(siboun) .lt. 0.1) then
!                write(*,*) 'Courant Beginp.: brn=',
!     +      ibr,' value:',siboun,' dtm ',dtm,dtms,' time (s):',time
!           endif

! Aanpassing (Sloff 8-8-1997) weer terug gezet (Kuipers 16-8-97)
         fuppos =  2. * (sposin - spos1) + (1./siboun - 1.) *&
         &(sposin - sposdt)

         if (epsequ(flwpos, 0. , cdchk)) then
            if (flwpos .lt. 0.) then
               flwpos = flwpos - cdval
            else
               flwpos = flwpos + cdval
            endif
         endif
! provision against overflow (Kuipers 16-4-99)
         rposd = dble(fuppos) / dble(flwpos)
         if (rposd.gt.0.d0) then
            rpos = min(rposd,1.d20)
         else
            rpos = max(rposd,-1.d20)
         endif

         if (rpos .le. 0.0) then
            ipos = spos1
         elseif (rpos .ge. alphad) then
! wijziging 20 juni 1997 Robbert Fokkink Kees Sloff
!              ipos = spos1 + flwpos / 2.0
            ipos = spos1 + alphad * flwpos / 2.0
         else
            ipos = spos1 + fuppos / 2.0
         endif

         iori = ipos * dtms
      endif

      intcel = iori + ineg * dtms

   endif

   return
end
