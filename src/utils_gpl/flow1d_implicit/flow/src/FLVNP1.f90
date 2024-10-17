subroutine FLVNP1(nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,&
&h2     ,q2     ,maxlev ,hlev   ,wft    ,maxtab ,&
&ntabm  ,ntab   ,table  ,subsec ,secth0 ,secth1 ,&
&wf     ,wfh0   ,wfh1   ,grsize ,engpar ,&
&af     ,o      ,afh0   ,afh1   ,oh0    ,oh1    ,&
&asubsc ,prslot ,psltvr ,c      ,r      ,cs     ,&
&rs     ,afs    ,wfs    ,alfab  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLVNP1 (FLow Variables on time level N+1)
!
! Module description: In subroutine FLVNP1 the Chezy coefficients and
!                     the hydraulic radius will be computed on time
!                     level n+1 as well as the different flow areas and
!                     widths for each gridpoint and section.
!
!                     This routine looks much alike FLBOCH except that
!                     the Boussinesq's constant alfab is not calculated.
!                     Output of this routine are the Chezy coefficients
!                     and the hydraulic radius for each gridpoint and
!                     section.
!
!                     See also routine FLBOCH.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 24 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 26 afh0(ngrid)       I  Flow area Af at water level h=h0 for every
!                         grid point.
! 27 afh1(ngrid)       I  Flow area Af at water level h=h1 for every
!                         grid point.
! 37 afs(ngrid,2)      O  Actual flow area per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
! 39 alfab(ngrid)      O  Actual Bousinessq coefficient in grid point i.
! 30 asubsc(ngrid)     IO Defines the actual number of sub sections for
!                         avery cross section (depending on the actual
!                         water level):
!                         c1sec (0) : Main section only (0 sub sections)
!                         c2sec (1) : 1 sub section
!                         c3sec (2) : 2 sub sections
!  6 bfricp            P  -
!  5 bfrict            P  -
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 33 c(ngrid)          O  Actual Chezy coefficient for total channel in
!                         every grid point.
! 35 cs(ngrid,3)       O  Actual Chezy coefficient in a section (main,
!                         sub 1, sub 2) for every grid point.
! 23 engpar            P  -
! 22 grsize(4,ngrid,*) I  Grain sizes for each gridpoint and section.
!                  1|2    Depending on the transport formula chosen one
!                         or more of the grain sizes will be defined.
!                         When a branch has been marked as a sedredge
!                         branch the D50 value should always be defined,
!                         no matter what transport formula has been
!                         chosen. For normal branches only section 1
!                         will be used being the D size in the main and
!                         sub sections. For sedredge branches section 1
!                         will be the D size in the left channel and
!                         section 2 will be the D size in the right
!                         channel.
!                         (1,i,j) =     D35 value for section j, grid-
!                                       point i.
!                         (2,i,j) =     D50 value for section j, grid-
!                                       point i.
!                         (3,i,j) =     D90 value for section j, grid-
!                                       point i.
!                         (4,i,j) =     Dmedium value for section j,
!                                       gridpoint i.
!  7 h2(ngrid)         I  Water level in every grid point at time
!                         t(n+1).
! 10 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  9 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 12 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
! 14 ntab              P  -
! 13 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 25 o(ngrid)          I  Wetted perimeter for total cross section.
! 28 oh0(ngrid)        I  Wetted perimeter Ot at water level h=h0 for
!                         every grid point.
! 29 oh1(ngrid)        I  Wetted perimeter Ot at water level h=h1 for
!                         every grid point.
! 31 prslot(3,nbran)   I  Contains information concerning Preissmann
!                         slot (assuring positive water depths).
!                         (1,i) = Create slot indicator
!                                 csldis (0) : No slot in this branch.
!                                 cslena (1) : Create slot in branch.
!                         (2,i) = Reference area for slot generation.
!                         (3,i) = Depth of slot below lowest bottom of
!                                 branch.
! 32 psltvr(7,ngrid)   I  Preissmann slot variables for every grid point
!                         i (assuring positive water depths):
!                         (1,i) = Value for C**2*R for positive flow.
!                         (2,i) = Value for C**2*R for negative flow.
!                         (3,i) = Bottom of slot (funnel)
!                         (4,i) = Division level between trapezium and
!                                 rectangle of slot (top of rectangle
!                                 and bottom of trapezium)
!                         (5,i) = Top of slot
!                         (6,i) = Bottom width of slot (width of
!                                 rectangle)
!                         (7,i) = Top width of slot (top of trapezium)
!  8 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 34 r(ngrid)          IO Actual hydraulic radius for total channel in
!                         every grid point.
! 36 rs(ngrid,3)       O  Actual hydraulic radius in a section (main,
!                         sub 1, sub 2) for every grid point.
! 17 secth0(ngrid)     I  H0-value (for 1 or 2 sub sections) for every
!                         grid point.
! 18 secth1(ngrid)     I  H0-value (for 2 sub section) for every grid
!                         point.
! 16 subsec(ngrid)     I  Defines the number of sub sections for every
!                         cross section:
!                         c1sec (0) : Main section only (0 sub sections)
!                         c2sec (1) : 1 sub section
!                         c3sec (2) : 2 sub sections
!                         (For a circle cross section   : 0 ;
!                          For a sedredge cross section : 1 )
! 15 table             P  -
!  4 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 19 wf(ngrid)         I  Actual flow width at every grid point.
! 20 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
!                         grid point.
! 21 wfh1(ngrid)       I  Flow width Wf at water level h=h1 for every
!                         grid point.
! 38 wfs(ngrid,2)      O  Actual flow width per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
! 11 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
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
! flchzt  FLow compute CHeZy Time dependent
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flvnp1.pf,v $
! Revision 1.9  1999/03/15  14:27:45  kuipe_j
! bed friction table general
!
! Revision 1.8  1998/02/13  12:12:33  kuipe_j
! Adapt to CMT
!
! Revision 1.7  1997/09/30  09:26:30  kuipe_j
! minor change
!
! Revision 1.6  1997/08/21  10:55:03  kuipe_j
! Check for negative areas
!
! Revision 1.5  1997/05/06  11:40:40  kuipe_j
! Protec against zero wet perimeter
!
! Revision 1.4  1997/01/23  08:29:18  kuipe_j
! Make flow module robust
!
! Revision 1.3  1995/05/30  09:55:36  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:38  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:16  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/12/02  13:17:23  kuipe_j
! Take care that if sub section 1 = 0 then always sub section2 = 0.
!
! Revision 1.2  1993/11/26  15:31:47  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:56  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer maxtab, ntabm, ntab(4,maxtab)
   integer nbran, ngrid, maxlev, branch(4,nbran)
   integer typcr(nbran), bfrict(3,nbran)
   real    wft(ngrid,maxlev)
   real    table(ntabm), subsec(ngrid), asubsc(ngrid)
   real    wf(ngrid), af(ngrid), o(ngrid)
   real    cs(ngrid,3), rs(ngrid,3)
   real    afs(ngrid,2), wfs(ngrid,2)
   real    afh0(ngrid), afh1(ngrid), oh0(ngrid), oh1(ngrid)
   real    c(ngrid), r(ngrid)
   real    grsize(4,ngrid,*), engpar(9)
   real    secth0(ngrid), secth1(ngrid)
   real    wfh0(ngrid), wfh1(ngrid), alfab(ngrid)
   real    bfricp(6,ngrid), prslot(3,nbran), psltvr(7,ngrid)
   double precision hlev(ngrid,maxlev), h2(ngrid), q2(ngrid)
!
!     Declaration of local variables:
!
   integer ibr, i1, i2, i, j, juerd, kerd
   real    h0, hh1, dz, ui,&
   &af0, o0, r0, c0,&
   &af1, o1, r1, c1,&
   &af2, o2, r2, c2,&
   &p1,  p2, c2r, d90,&
   &zbmain, wmain, amain, omain, rmain, cmain,&
   &zbsub1, wsub1, asub1, osub1, rsub1, csub1,&
   &cdum
   double precision hi
   logical lslot
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Initialize for Rougness checking
!
   call flroulim (-2 ,cdum ,juerd,kerd)
!
   do 400 ibr = 1, nbran
!
!        i1 = global grid point number at node n1
!        i2 = global grid point number at node n2
!
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!
      lslot = int(prslot(1,ibr)) .eq. cslena
!
      if ( typcr(ibr) .eq. ccrsed ) then
!
!           Sedredge branch
!
         do 100 i = i1, i2
            asubsc(i) = 1.
!
!              hi : waterlevel h=h(n+1)
!
            hi  = h2(i)
            ui  = q2(i)/af(i)
            d90 = grsize(3,i,1)

            zbmain = hlev(i,1)
            zbsub1 = hlev(i,2)
            wmain  = wft(i,1)
            wsub1  = wft(i,2)

            dz = zbmain - zbsub1
            if (dz .lt. 0) then
               omain = wmain + (hi - zbmain) - dz
               osub1 = wsub1 + (hi - zbsub1)
            else
               omain = wmain + (hi - zbmain)
               osub1 = wsub1 + (hi - zbsub1) + dz
            endif

            amain = (hi - zbmain) * wmain
            rmain = amain / omain
            cmain = cs(i,1)
!
!              Compute Chezy coefficient cmain from hydraulic radius rmain
!
            call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,bfrict ,&
            &bfricp ,maxtab ,ntabm  ,ntab   ,table  ,&
            &d90    ,engpar ,0      ,hi     ,q2(i)  ,&
            &ui     ,rmain  ,cmain  )
!
!              Store R, C, Af and Wf for main section.
!
            rs (i,1) = rmain
            cs (i,1) = cmain
            afs(i,1) = amain
            wfs(i,1) = wmain

            asub1 = (hi - zbsub1) * wsub1
            rsub1 = asub1 / osub1
            d90   = grsize(3,i,2)
            csub1 = cs(i,2)
!
!              Compute Chezy coefficient csub1 from hydraulic radius rsub1
!
            call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,bfrict ,&
            &bfricp ,maxtab ,ntabm  ,ntab   ,table  ,&
            &d90    ,engpar ,1      ,hi     ,q2(i)  ,&
            &ui     ,rsub1  ,csub1  )
!
!              Store R, C, Af and Wf for sub section.
!
            rs (i,2) = rsub1
            cs (i,2) = csub1
            afs(i,2) = asub1
            wfs(i,2) = wsub1
!
!              **********************************************
!              * Computation for total cross section        *
!              * 1. Chezy coefficient C                     *
!              * 2. Hydraulic radius R                      *
!              * 3. Bousinessq constant alfab               *
!              **********************************************
!
            p1       = ( cmain * amain * sqrt(rmain) +&
            &csub1 * asub1 * sqrt(rsub1) ) / Af(i)
            c2r      = p1*p1
!
            p1       = cmain * cmain * amain * rmain +&
            &csub1 * csub1 * asub1 * rsub1
            p2       = c2r * af(i)
            alfab(i) = p1/p2
!
!              Compute R and C for total cross section
!
            r(i) = Af(i) / o(i)
            c(i) = sqrt ( c2r   / r(i) )
100      continue

      else
!
!           Loop over cross sections (grid points)
!
         do 300 i = i1, i2
!
!              hi : waterlevel h=h(n+1)
!
            hi  = h2(i)
            ui  = q2(i)/af(i)
            d90 = grsize(3,i,1)
!
!              Define actual number of subsections
!
            call FLNSEC(sngl(hi),i ,asubsc(i),subsec ,secth0 ,secth1,&
            &wfh0 ,wfh1 ,af     ,afh0   ,afh1   ,ngrid )
!
            h0  = secth0(i)
            hh1 = secth1(i)
!
!              *****************************************
!              * Reset old values for non used sections*
!              *****************************************
!
            do 200 j = 1, 3
               rs (i,j) = 0.
               if (j .gt. 1) cs (i,j) = 0.
               if (j .le. 2) then
                  afs(i,j) = 0.
                  wfs(i,j) = 0.
               endif
200         continue
!
!              * Situation *****************************
!              * 1. main section                       *
!              * 2. 1 or 2 sub sections, but h < h0    *
!              *****************************************
!
            if   ( int( asubsc(i) ) .eq. 0 ) then
!
!                 ****************
!                 * main section *
!                 ****************
!                 Doc: S-FO-001.5KV  Eqs. 5-6 / 5-7 / 5-9
!
!                 r    : hydraulic radius
!                 c0   : Chezy coeffient for main section
!
               r0  = Af(i) / o(i)
               c0  = cs(i,1)
!
!                 Compute Chezy coefficient c0 from hydraulic radius r0
!
               if (lslot .and. hi .lt. psltvr(5,i) ) then
                  if (q2(i) .gt. 0) then
                     c0 = sqrt (psltvr(1,i)/r0)
                  else
                     c0 = sqrt (psltvr(2,i)/r0)
                  endif
               else
                  call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,bfrict,&
                  &bfricp ,maxtab ,ntabm  ,ntab   ,table ,&
                  &d90    ,engpar ,0      ,hi     ,q2(i) ,&
                  &ui     ,r0     ,c0     )
               endif
!
               alfab(i) = 1.0
!
!                 Store R, C, Af and Wf for sub section.
!
               rs (i,1) = r0
               cs (i,1) = c0
               afs(i,1) = af(i)
               wfs(i,1) = wf(i)
!
!                 Store R and C for total cross section
!
               r  (i)   = r0
               c  (i)   = c0

            else if ( int( asubsc (i) ) .eq. 1 ) then
!
!                 ** Situation ***********
!                 *  1 or 2 sub sections *
!                 *  h0 <= h < hh1        *
!                 ************************
!
!                 ****************
!                 * main section *
!                 ****************
!                 Doc: S-FO-001.5KV  Eqs. 5-8 / 5-10
!
!                 Af0  = flow area in main section
!                 Afh0 = flow area in main section for h = h0
!                 oh0  = wetted perimeter in main section for h = h0
!                 r0   = hydraulic radius for main section
!
               Af0    = Afh0(i) + (hi-h0) * Wfh0(i)
               o0     = oh0(i)
               r0     = Af0 / o0
               c0     = cs(i,1)
!
!                 Compute Chezy coefficient c0 from hydraulic radius r0
!                 according to the user selected Chezy formula.
!
               if (lslot .and. hi .lt. psltvr(5,i) ) then
                  if ( q2(i) .gt. 0 ) then
                     c0 = sqrt ( psltvr(1,i)/r0 )
                  else
                     c0 = sqrt ( psltvr(2,i)/r0 )
                  endif
               else
                  call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,bfrict,&
                  &bfricp ,maxtab ,ntabm  ,ntab   ,table ,&
                  &d90    ,engpar ,0      ,hi     ,q2(i) ,&
                  &ui     ,r0     ,c0     )
               endif
!
!                 Store R, C, Af and Wf for main section.
!
               rs (i,1) = r0
               cs (i,1) = c0
               afs(i,1) = af0
               wfs(i,1) = wfh0(i)
!
!                 *****************
!                 * sub section 1 *
!                 *****************
!                 Doc: S-FO-001.5KV  Eq. 5-8
!
!                 Actual parameters:
!
!                 Af(i)= actual flow area total cross section
!                 o(i) = wetted perimeter for total cross section
!
!                 Af1  = flow area in sub section 1
!                 r1   = hydraulic radius for sub section 1
!                 c1   = Chezy coefficient for sub section 1
!
               Af1    = Af(i) - Af0
               o1     = o(i) - oh0(i)

!                 Provision against too small wetted perimeter
!                 and negative area
               if (o1 .le. .001  .or. af1 .le. 1e-6) then
                  r1 = (hi-h0) *.5
               else
                  r1 = Af1 / o1
               endif
               c1     = cs(i,2)
!
!                 Compute Chezy coefficient c1 from hydraulic radius r1
!
               if (lslot .and. hi .lt. psltvr(5,i) ) then
                  if ( q2(i) .gt. 0 ) then
                     c1 = sqrt ( psltvr(1,i)/r1 )
                  else
                     c1 = sqrt ( psltvr(2,i)/r1 )
                  endif
               else
                  call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,bfrict,&
                  &bfricp ,maxtab ,ntabm  ,ntab   ,table ,&
                  &d90    ,engpar ,1      ,hi     ,q2(i) ,&
                  &ui     ,r1     ,c1     )
               endif
!
!                 Store R, C, Af and Wf for sub section.
!
               rs (i,2) = r1
               cs (i,2) = c1
               afs(i,2) = af1
               wfs(i,2) = wf(i) - wfh0(i)
!
!                 **********************************************
!                 * Computation for total cross section        *
!                 * 1. Chezy coefficient C                     *
!                 * 2. Hydraulic radius R                      *
!                 * 3. Bousinessq constant alfab               *
!                 **********************************************
!
               p1       = ( c0*Af0*sqrt(r0) +&
               &c1*Af1*sqrt(r1) ) / Af(i)
               c2r      = p1*p1

               p1       = c0*c0 * Af0 * r0 + c1*c1 * Af1 * r1
               p2       = c2r * af(i)
               alfab(i) = p1/p2
!
!                 Compute R and C for total cross section
!
               r(i) = Af(i) / o(i)
               c(i) = sqrt ( c2r   / r(i) )

            else if ( int( asubsc(i) ) .eq. 2 ) then
!
!                 ** Situation ***********
!                 *  2 sub sections      *
!                 *  h > hh1              *
!                 ************************
!
!                 ****************
!                 * main section *
!                 ****************
!                 Doc: S-FO-001.5KV  Eq. 5-11
!
!                 Afh0 = flow area in main section for h = h0
!                 oh0  = wetted perimeter in main section for h = h0
!
!                 Af0  = flow area in main section
!                 r0   = hydraulic radius for main section
!
               Af0    = Afh0(i) + (hi-h0) * Wfh0(i)
               o0     = oh0(i)
               r0     = Af0 / o0
               c0     = cs(i,1)
!
!                 Compute Chezy coefficient c0 from hydraulic radius r0
!
               if (lslot .and. hi .lt. psltvr(5,i) ) then
                  if ( q2(i) .gt. 0 ) then
                     c0 = sqrt ( psltvr(1,i)/r0 )
                  else
                     c0 = sqrt ( psltvr(2,i)/r0 )
                  endif
               else
                  call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,bfrict,&
                  &bfricp ,maxtab ,ntabm  ,ntab   ,table ,&
                  &d90    ,engpar ,0      ,hi     ,q2(i) ,&
                  &ui     ,r0     ,c0     )
               endif
!
!                 Store R, C, Af and Wf for main section.
!
               rs (i,1) = r0
               cs (i,1) = c0
               afs(i,1) = af0
               wfs(i,1) = wfh0(i)
!
!                 *****************
!                 * sub section 1 *
!                 *****************
!                 Doc: S-FO-001.5KV  Eq. 5-11
!
!                 Actual parameters:
!                 Af1  = flow area in sub section 1
!                 r1   = hydraulic radius for sub section 1
!                 c1   = Chezy coefficient for sub section 1

               Af1     = Afh1(i) + (hi-hh1) * Wfh1(i) - Af0
               o1      = oh1(i)
!
!                 Provision against too small wetted perimeter
!                 and negative area
               if (o1 .le. .001  .or. af1 .le. 1e-6) then
                  r1 = (hi-h0) *.5
               else
                  r1 = Af1 / o1
               endif
               c1    = cs(i,2)
!
!                 Compute Chezy coefficient c1 from hydraulic radius r1
!
               if (lslot .and. hi .lt. psltvr(5,i) ) then
                  if ( q2(i) .gt. 0 ) then
                     c1 = sqrt ( psltvr(1,i)/r1 )
                  else
                     c1 = sqrt ( psltvr(2,i)/r1 )
                  endif
               else
                  call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,bfrict,&
                  &bfricp ,maxtab ,ntabm  ,ntab   ,table ,&
                  &d90    ,engpar ,1      ,hi     ,q2(i) ,&
                  &ui     ,r1     ,c1     )
               endif
!
!                 Store R, C, Af and Wf for sub section.
!
               rs (i,2) = r1
               cs (i,2) = c1
               afs(i,2) = af1
               wfs(i,2) = wfh1(i) - wfh0(i)
!
!                 *****************
!                 * sub section 2 *
!                 *****************
!                 Doc: S-FO-001.5KV  Eq. 5-11
!
!                 Actual parameters:
!
!                 Af(i)= actual flow area total cross section
!                 o(i) = wetted perimeter for total cross section
!
!                 Af2  = flow area in sub section 2
!                 r2   = hydraulic radius for sub section 2
!                 c2   = Chezy coefficient for sub section 2

               Af2    = Af(i) - Af1 - Af0
               o2     = o(i) - oh1(i) - oh0(i)

!                 Provision against too small wetted perimeter
!                 and negative area
               if (o2 .le. .001 .or. af2 .le. 1e-6 ) then
                  r2 = (hi-hh1) *.5
               else
                  r2 = Af2 / o2
               endif
               c2     = cs(i,3)
!
!                 Compute Chezy coefficient c2 from hydraulic radius r2
!
               if (lslot .and. hi .lt. psltvr(5,i) ) then
                  if ( q2(i) .gt. 0 ) then
                     c2 = sqrt ( psltvr(1,i)/r2 )
                  else
                     c2 = sqrt ( psltvr(2,i)/r2 )
                  endif
               else
                  call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,bfrict,&
                  &bfricp ,maxtab ,ntabm  ,ntab   ,table ,&
                  &d90    ,engpar ,2      ,hi     ,q2(i) ,&
                  &ui     ,r2     ,c2     )
               endif
!
!                 Store R, C, Af and Wf for sub section 2
!
               rs (i,3) = r2
               cs (i,3) = c2
!
!                 **********************************************
!                 * Computation for total cross section        *
!                 * 1. Chezy coefficient C                     *
!                 * 2. Hydraulic radius R                      *
!                 * 3. Bousinessq constant alfab               *
!                 **********************************************
!
               p1       = ( c0*Af0*sqrt(r0) +&
               &c1*Af1*sqrt(r1) +&
               &c2*Af2*sqrt(r2) ) / Af(i)
               c2r      = p1*p1

               p1       = c0*c0 * af0 * r0  +&
               &c1*c1 * af1 * r1  +&
               &c2*c2 * af2 * r2

               p2       = c2r * af(i)
               alfab(i) = p1/p2
!
!                 Compute R and C for total cross section
!
               r(i) = Af(i) / o(i)
               c(i) = sqrt ( c2r / r(i) )

            endif
300      continue
      endif
400 continue
!
!     Rougness checking
!
   call flroulim (-3 ,cdum ,juerd,kerd)
!
end
