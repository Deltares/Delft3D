      subroutine FLBOCH(nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,
     +                  h1     ,h      ,q1     ,q      ,maxlev ,hlev   ,
     +                  wft    ,maxtab ,ntabm  ,ntab   ,table  ,subsec ,
     +                  secth0 ,secth1 ,wfh0   ,wfh1   ,grsize ,engpar ,
     +                  af     ,o      ,afh0   ,afh1   ,oh0    ,oh1    ,
     +                  prslot ,psltvr ,asubsc ,c      ,r      ,cs     ,
     +                  rs     ,alfab  ,
     +                  iter   ,theta2 ,omalfa ,omr    ,omw    ,
     +                  alfabp ,c2rp   ,wfp    ,wf     ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLBOCH (FLow BOussinesq and CHezy coefficient)
c
c Module description: In subroutine FLBOCH the Boussinesq's constant and
c                     the Chezy coefficients will be computed.
c
c                     The computation of Boussinesq's constant alfab
c                     will be done in the same way as in WENDY. The
c                     cross sections may be divided into a main section,
c                     sub section 1 and sub section 2. Each (sub-) sec-
c                     tion has its own Chezy-formula (Nikuradse, Man-
c                     ning, etc.). In each section the Chezy coefficient
c                     and the hydraulic radius will be computed.
c
c                     Since this item is elaborated extensively in the
c                     Functional Design Report [S-FO-001.5KV], reference
c                     is made to this report. See formulae (5-6) until
c                     (5-11) in S-FO-001.5KV.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 25 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c 27 afh0(ngrid)       I  Flow area Af at water level h=h0 for every
c                         grid point.
c 28 afh1(ngrid)       I  Flow area Af at water level h=h1 for every
c                         grid point.
c 38 alfab(ngrid)      IO Actual Bousinessq coefficient in grid point i.
c 44 alfabp(ngrid)     IO Bousinessq coefficient in grid point i.
c                         Calculated with underrelaxation.
c                         Input parameter on the former iteration level.
c                         Output parameter on the actual iteration level
c 33 asubsc(ngrid)     IO Defines the actual number of sub sections for
c                         avery cross section (depending on the actual
c                         water level):
c                         c1sec (0) : Main section only (0 sub sections)
c                         c2sec (1) : 1 sub section
c                         c3sec (2) : 2 sub sections
c  6 bfricp            P  -
c  5 bfrict            P  -
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 34 c(ngrid)          IO Actual Chezy coefficient for total channel in
c                         every grid point.
c 45 c2rp(ngrid)       IO C**2*R for the total channel in every grid
c                         point. (C=Chezy coefficient, R=Hydraulic
c                         Radius)
c                         Calculated with underrelaxation.
c                         Input parameter on the former iteration level.
c                         Output parameter on the actual iteration level
c 36 cs(ngrid,3)       O  Actual Chezy coefficient in a section (main,
c                         sub 1, sub 2) for every grid point.
c 24 engpar            P  -
c 23 grsize(4,ngrid,*) I  Grain sizes for each gridpoint and section.
c                  1|2    Depending on the transport formula chosen one
c                         or more of the grain sizes will be defined.
c                         When a branch has been marked as a sedredge
c                         branch the D50 value should always be defined,
c                         no matter what transport formula has been
c                         chosen. For normal branches only section 1
c                         will be used being the D size in the main and
c                         sub sections. For sedredge branches section 1
c                         will be the D size in the left channel and
c                         section 2 will be the D size in the right
c                         channel.
c                         (1,i,j) =     D35 value for section j, grid-
c                                       point i.
c                         (2,i,j) =     D50 value for section j, grid-
c                                       point i.
c                         (3,i,j) =     D90 value for section j, grid-
c                                       point i.
c                         (4,i,j) =     Dmedium value for section j,
c                                       gridpoint i.
c  7 h1(ngrid)         I  Water level in every grid point at time t(n).
c  8 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c 12 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c 39 iter              I  Iteration step.
c 11 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 14 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c 16 ntab              P  -
c 15 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 26 o(ngrid)          I  Wetted perimeter for total cross section.
c 29 oh0(ngrid)        I  Wetted perimeter Ot at water level h=h0 for
c                         every grid point.
c 30 oh1(ngrid)        I  Wetted perimeter Ot at water level h=h1 for
c                         every grid point.
c 41 omalfa            I  underrelaxation parameter Boussinesq ALFAB
c 42 omr               I  Underrelaxation factor omega for the hydraulic
c                         radius R.
c 43 omw               I  underrelaxation parameter Flow width Wf
c 31 prslot(3,nbran)   I  Contains information concerning Preissmann
c                         slot (assuring positive water depths).
c                         (1,i) = Create slot indicator
c                                 csldis (0) : No slot in this branch.
c                                 cslena (1) : Create slot in branch.
c                         (2,i) = Reference area for slot generation.
c                         (3,i) = Depth of slot below lowest bottom of
c                                 branch.
c 32 psltvr(7,ngrid)   I  Preissmann slot variables for every grid point
c                         i (assuring positive water depths):
c                         (1,i) = Value for C**2*R for positive flow.
c                         (2,i) = Value for C**2*R for negative flow.
c                         (3,i) = Bottom of slot (funnel)
c                         (4,i) = Division level between trapezium and
c                                 rectangle of slot (top of rectangle
c                                 and bottom of trapezium)
c                         (5,i) = Top of slot
c                         (6,i) = Bottom width of slot (width of
c                                 rectangle)
c                         (7,i) = Top width of slot (top of trapezium)
c  9 q1(ngrid)         I  Discharge in every grid point at time t(n).
c 10 q(ngrid)          I  Discharge in every grid point at the latest
c                         iteration.
c 35 r(ngrid)          IO Actual hydraulic radius for total channel in
c                         every grid point.
c 37 rs(ngrid,3)       O  Actual hydraulic radius in a section (main,
c                         sub 1, sub 2) for every grid point.
c 19 secth0(ngrid)     I  H0-value (for 1 or 2 sub sections) for every
c                         grid point.
c 20 secth1(ngrid)     I  H0-value (for 2 sub section) for every grid
c                         point.
c 18 subsec(ngrid)     I  Defines the number of sub sections for every
c                         cross section:
c                         c1sec (0) : Main section only (0 sub sections)
c                         c2sec (1) : 1 sub section
c                         c3sec (2) : 2 sub sections
c                         (For a circle cross section   : 0 ;
c                          For a sedredge cross section : 1 )
c 17 table             P  -
c 40 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c  4 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 47 wf(ngrid)         I  Actual flow width at every grid point.
c 21 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
c                         grid point.
c 22 wfh1(ngrid)       I  Flow width Wf at water level h=h1 for every
c                         grid point.
c 46 wfp(ngrid)        IO Flow width in every grid point.
c                         Calculated with underrelaxation.
c                         Input parameter on the former iteration level.
c                         Output parameter on the actual iteration level
c 13 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
c                                 point i.
c                         - For a circle cross section:
c                         (i,1) = Radius of the circle.
c                         - For a sedredge cross section:
c                         (i,1) = Width of main section (i.e. left chan-
c                                 nel).
c                         (i,2) = Width of sub section 1 (i.e. right
c                                 channel).
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flchzt  FLow compute CHeZy Time dependent
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flboch.pf,v $
c Revision 1.15  1999/06/01  13:42:12  kuipe_j
c names in messages substituted + message template
c
c Revision 1.14  1999/03/15  14:27:41  kuipe_j
c bed friction table general
c
c Revision 1.13  1997/08/21  10:55:01  kuipe_j
c Check for negative areas
c
c Revision 1.12  1997/08/08  10:53:47  kuipe_j
c Change check limits
c
c Revision 1.11  1997/05/26  07:43:32  kuipe_j
c Check on R andC added
c
c Revision 1.10  1997/05/06  11:39:59  kuipe_j
c Protec against zero wet perimeter
c
c Revision 1.9  1997/01/23  08:28:59  kuipe_j
c Make flow module robust
c
c Revision 1.8  1996/04/11  08:23:11  kuipe_j
c Kalman module added
c
c Revision 1.7  1995/09/22  10:01:00  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:10:49  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:36:26  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:12  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:54:46  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:43  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:30  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/12/02  13:17:21  kuipe_j
c Take care that if sub section 1 = 0 then always sub section2 = 0.
c
c Revision 1.2  1993/11/26  15:30:33  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:46  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer maxtab, ntabm, ntab(4,maxtab)
      integer nbran, ngrid, maxlev, branch(4,nbran)
      integer typcr(nbran), bfrict(3,nbran)
      integer iter ,juer  ,ker
      real    wft(ngrid,maxlev)
      real    table(ntabm), subsec(ngrid), asubsc(ngrid)
      real    af(ngrid), o(ngrid)
      real    cs(ngrid,3), rs(ngrid,3), alfab(ngrid)
      real    afh0(ngrid), afh1(ngrid), oh0(ngrid), oh1(ngrid)
      real    c(ngrid), r(ngrid)
      real    grsize(4,ngrid,*), engpar(9)
      real    secth0(ngrid), secth1(ngrid)
      real    wfh0(ngrid), wfh1(ngrid)
      real    bfricp(6,ngrid), prslot(3,nbran), psltvr(7,ngrid)
      real    alfabp(ngrid),c2rp(ngrid),wfp(ngrid),wf(ngrid)
      real    theta2 ,omalfa ,omr    ,omw
      double precision hlev(ngrid,maxlev) 
      double precision h1(ngrid), h(ngrid), q(ngrid), q1(ngrid)

c
c     Declaration of local variables:
c
      logical lslot, loop
      integer ibr, i1, i2, i, lbrnam
      double precision hi, qi
      real    ui, h0, hh1, dz,
     +        af0, o0, r0, c0,
     +        af1, o1, r1, c1,
     +        af2, o2, r2, c2,
     +        p1, p2, c2r, d90,
     +        zbmain, wmain, amain, omain, rmain, cmain,
     +        zbsub1, wsub1, asub1, osub1, rsub1, csub1,
     +        xc
      character*40    branam
      character*11    xtxt
c
c     Include sobek constants
c
      include '../include/sobcon.i'
      include '../include/errcod.i'
c
      do 200 ibr = 1, nbran
c
c        i1 = global grid point number at node n1
c        i2 = global grid point number at node n2
c
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
c
         lslot = int(prslot(1,ibr)) .eq. cslena
c
         if ( typcr(ibr) .eq. ccrsed ) then
c
c           Sedredge branch
c
            do 50 i = i1, i2
c
c              hi : waterlevel h=h(n+theta2)
c              qi : discharge  q=q(n+theta2)
c
               hi  = theta2*h(i)+(1.-theta2)*h1(i)
               qi  = theta2*q(i)+(1.-theta2)*q1(i)
               ui  = qi / af(i)
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
c
c              Compute Chezy coefficient cmain from hydraulic radius
c              rmain
c
               call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,bfrict ,
     +                     bfricp ,maxtab ,ntabm  ,ntab   ,table  ,
     +                     d90    ,engpar ,0      ,hi     ,qi     ,
     +                     ui     ,rmain  ,cmain  )
c
c              Store R and C for main section.
c
               rs(i,1) = rmain
               cs(i,1) = cmain
               d90     = grsize(3,i,2)

               asub1 = (hi - zbsub1) * wsub1
               rsub1 = asub1 / osub1
               csub1 = cs(i,2) 
c
c              Compute Chezy coefficient csub1 from hydraulic radius
c              rsub1
c
               call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,bfrict ,
     +                     bfricp ,maxtab ,ntabm  ,ntab   ,table  ,
     +                     d90    ,engpar ,1      ,hi     ,qi     ,
     +                     ui     ,rsub1  ,csub1  )
c
c              Store R and C for sub1 section. (sub section 1)
c
               rs(i,2) = rsub1
               cs(i,2) = csub1
c
c              **********************************************
c              * Computation for total cross section        *
c              * 1. Chezy coefficient C                     *
c              * 2. Hydraulic radius R                      *
c              * 3. Boussinesq constant alfab               *
c              **********************************************
c
               p1       = ( cmain * amain * sqrt(rmain) +
     +                      csub1 * asub1 * sqrt(rsub1) ) / Af(i)
               c2r      = p1*p1

               p1       = cmain*cmain * amain * rmain +
     +                    csub1*csub1 * asub1 * rsub1
               p2       = c2r   * Af(i)
               alfab(i) = p1/p2
c
c              Compute R and C for total cross section
c
               r(i) = Af(i) / o(i)
               c(i) = sqrt ( c2r / r(i) )
   50       continue

         else
c
c           Loop over cross sections (grid points)
c
            do 100 i = i1, i2
c
c              hi : waterlevel h=h(n+theta2)
c              qi : discharge  q=q(n+theta2)
c
               hi  = theta2*h(i)+(1.-theta2)*h1(i)
               qi  = theta2*q(i)+(1.-theta2)*q1(i)
               ui  = qi / af(i)
               d90 = grsize(3,i,1)
c
c              Define actual number of subsections
c
               call FLNSEC(sngl(hi),i,asubsc(i) ,subsec ,secth0 ,secth1,
     +                     wfh0 ,wfh1 ,af     ,afh0   ,afh1   ,ngrid )
c
               h0  = secth0(i)
               hh1 = secth1(i)
c
c              * Situation *****************************
c              * 1. main section                       *
c              * 2. 1 or 2 sub sections, but h < h0    *
c              *****************************************
c
               if   ( int( asubsc(i) ) .eq. 0 ) then
c
c                 ****************
c                 * main section *
c                 ****************
c                 Doc: S-FO-001.5KV  Eq. 5-6 / 5-7 / 5-9
c
c                 r    : hydraulic radius
c                 c0   : Chezy coeffient for main section
c
                  r0  = Af(i) / o(i)
                  c0  = cs(i,1)
c
c                 Compute Chezy coefficient c0 from hydraulic radius r0
c
                  if ( lslot .and. hi .lt. psltvr(5,i) ) then
                     if ( qi .gt. 0D0 ) then
                        c0 = sqrt (psltvr(1,i)/r0)
                     else
                        c0 = sqrt (psltvr(2,i)/r0)
                     endif
                  else
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                           bfrict ,bfricp ,maxtab ,ntabm  ,
     +                           ntab   ,table  ,d90    ,engpar ,
     +                           0      ,hi     ,qi     ,ui     ,
     +                           r0     ,c0     )
                  endif
c
                  alfab(i) = 1.0
c
c                 Store R and C for total cross section and per sub
c                 section
c
                  r (i)   = r0
                  c (i)   = c0
                  rs(i,1) = r0
                  cs(i,1) = c0
c
               else if ( int( asubsc(i) ) .eq. 1 ) then
c
c                 ** Situation ***********
c                 *  1 or 2 sub sections *
c                 *  h0 <= h < hh1       *
c                 ************************
c
c                 ****************
c                 * main section *
c                 ****************
c                 Doc: S-FO-001.5KV  Eq. 5-8 / 5-10
c
c                 Af0  = flow area in main section
c                 Afh0 = flow area in main section for h = h0
c                 oh0  = wetted perimeter in main section for h = h0
c                 r0   = hydraulic radius for main section
c
                  Af0    = Afh0(i) + (hi-h0) * Wfh0(i)
                  o0     = oh0(i)
                  r0     = Af0 / o0
                  c0     = cs(i,1)
c
c                 Compute Chezy coefficient c0 from hydraulic radius r0
c                 according to the user selected Chezy formula.
c
                  if ( lslot .and. hi .lt. psltvr(5,i) ) then
                     if ( qi .gt. 0D0 ) then
                        c0 = sqrt (psltvr(1,i)/r0)
                     else
                        c0 = sqrt (psltvr(2,i)/r0)
                     endif
                  else
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                           bfrict ,bfricp ,maxtab ,ntabm  ,
     +                           ntab   ,table  ,d90    ,engpar ,
     +                           0      ,hi     ,qi     ,ui     ,
     +                           r0     ,c0     )
                  endif
c
c                 Store R and C for main section. (sub section 0)
c
                  rs(i,1) = r0
                  cs(i,1) = c0
c
c                 *****************
c                 * sub section 1 *
c                 *****************
c                 Doc: S-FO-001.5KV  Eq. 5-8
c
c                 Actual parameters:
c
c                 Af(i)= actual flow area total cross section
c                 o(i) = wetted perimeter for total cross section
c
c                 Af1  = flow area in sub section 1
c                 r1   = hydraulic radius for sub section 1
c                 c1   = Chezy coefficient for sub section 1
c
                  Af1    = Af(i) - Af0
                  o1     = o(i) - oh0(i)
                  c1     = cs(i,2)
                  
c                 Provision against too small wetted perimeter
c                 and negative area
                  if (o1 .le. .001 .or. af1 .le. 1.e-6) then
                     r1 = (hi-h0) *.5
                  else
                     r1 = Af1 / o1
                  endif
c
c                 Compute Chezy coefficient c1 from hydraulic radius r1
c
                  if ( lslot .and. hi .lt. psltvr(5,i) ) then
                     if ( qi .gt. 0. ) then
                        c1 = sqrt (psltvr(1,i)/r1)
                     else
                        c1 = sqrt (psltvr(2,i)/r1)
                     endif
                  else
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                           bfrict ,bfricp ,maxtab ,ntabm  ,
     +                           ntab   ,table  ,d90    ,engpar ,
     +                           1      ,hi     ,qi     ,ui     ,
     +                           r1     ,c1     )
                  endif
c
c                 Store R and C for sub section 1
c
                  rs(i,2) = r1
                  cs(i,2) = c1
c
c                 **********************************************
c                 * Computation for total cross section        *
c                 * 1. Chezy coefficient C                     *
c                 * 2. Hydraulic radius R                      *
c                 * 3. Boussinesq constant alfab               *
c                 **********************************************
c
                  p1       = ( c0*Af0*sqrt(r0) +
     +                         c1*Af1*sqrt(r1) ) / Af(i)
                  c2r      = p1*p1

                  p1       = c0*c0 * Af0 * r0 + c1*c1 * Af1 * r1
                  p2       = c2r   * Af(i)
                  alfab(i) = p1/p2
c
c                 Compute R and C for total cross section
c
                  r(i) = Af(i) / o(i)
                  c(i) = sqrt ( c2r   / r(i) )

               else if ( int( asubsc(i) ) .eq. 2 ) then
c
c                 ** Situation ***********
c                 *  2 sub sections      *
c                 *  h > hh1             *
c                 ************************
c
c                 ****************
c                 * main section *
c                 ****************
c                 Doc: S-FO-001.5KV  Eq. 5-11
c
c                 Afh0 = flow area in main section for h = h0
c                 oh0  = wetted perimeter in main section for h = h0
c
c                 Af0  = flow area in main section
c                 r0   = hydraulic radius for main section
c
                  Af0    = Afh0(i) + (hi-h0) * Wfh0(i)
                  o0     = oh0(i)
                  r0     = Af0 / o0
                  c0     = cs(i,1)
c
c                 Compute Chezy coefficient c0 from hydraulic radius r0
c
                  if ( lslot .and. hi .lt. psltvr(5,i) ) then
                     if ( qi .gt. 0D0 ) then
                        c0 = sqrt (psltvr(1,i)/r0)
                     else
                        c0 = sqrt (psltvr(2,i)/r0)
                     endif
                  else
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                           bfrict ,bfricp ,maxtab ,ntabm  ,
     +                           ntab   ,table  ,d90    ,engpar ,
     +                           0      ,hi     ,qi     ,ui     ,
     +                           r0     ,c0     )
                  endif
c
c                 Store R and C for main section
c
                  rs(i,1) = r0
                  cs(i,1) = c0
c
c                 *****************
c                 * sub section 1 *
c                 *****************
c                 Doc: S-FO-001.5KV  Eq. 5-11
c
c                 Actual parameters:
c                 Af1  = flow area in sub section 1
c                 r1   = hydraulic radius for sub section 1
c                 c1   = Chezy coefficient for sub section 1

                  Af1     = Afh1(i) + (hi-hh1) * Wfh1(i) - Af0
                  o1      = oh1(i)
c
c                 Provision against too small wetted perimeter
c                 and negative area
                  if (o1 .le. .001 .or. af1 .le. 1.e-6) then
                     r1 = (hi-h0) *.5
                  else
                     r1 = Af1 / o1
                  endif

                  c1      = cs(i,2)
c
c                 Compute Chezy coefficient c1 from hydraulic radius r1
c
                  if ( lslot .and. hi .lt. psltvr(5,i) ) then
                     if ( qi .gt. 0D0 ) then
                        c1 = sqrt (psltvr(1,i)/r1)
                     else
                        c1 = sqrt (psltvr(2,i)/r1)
                     endif
                  else
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                           bfrict ,bfricp ,maxtab ,ntabm  ,
     +                           ntab   ,table  ,d90    ,engpar ,
     +                           1      ,hi     ,qi     ,ui     ,
     +                           r1     ,c1     )
                  endif
c
c                 Store R and C for sub section 1
c
                  rs(i,2) = r1
                  cs(i,2) = c1
c
c                 *****************
c                 * sub section 2 *
c                 *****************
c                 Doc: S-FO-001.5KV  Eq. 5-11
c
c                 Actual parameters:
c
c                 Af(i)= actual flow area total cross section
c                 o(i) = wetted perimeter for total cross section
c
c                 Af2  = flow area in sub section 2
c                 r2   = hydraulic radius for sub section 2
c                 c2   = Chezy coefficient for sub section 2

                  Af2    = Af(i) - Af1 - Af0
                  o2     = o(i) - oh1(i) - oh0(i)
                  c2     = cs(i,3)

c                 Provision against too small wetted perimeter
c                 and negative area
                  if (o2 .le. .001 .or. af2 .le. 1.e-6) then
                     r2 = (hi-hh1) *.5
                  else
                     r2 = Af2 / o2
                  endif
c
c                 Compute Chezy coefficient c2 from hydraulic radius r2
c
                  if ( lslot .and. hi .lt. psltvr(5,i) ) then
                     if ( qi .gt. 0D0 ) then
                        c2 = sqrt (psltvr(1,i)/r2)
                     else
                        c2 = sqrt (psltvr(2,i)/r2)
                     endif
                  else
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                           bfrict ,bfricp ,maxtab ,ntabm  ,
     +                           ntab   ,table  ,d90    ,engpar ,
     +                           2      ,hi     ,qi     ,ui     ,
     +                           r2     ,c2     )
                  endif
c
c                 Store R and C for sub section 2
c
                  rs(i,3) = r2
                  cs(i,3) = c2
c
c                 **********************************************
c                 * Computation for total cross section        *
c                 * 1. Chezy coefficient C                     *
c                 * 2. Hydraulic radius R                      *
c                 * 3. Boussinesq constant alfab               *
c                 **********************************************
c
                  p1       = ( c0*Af0*sqrt(r0) +
     +                         c1*Af1*sqrt(r1) +
     +                         c2*Af2*sqrt(r2) ) / Af(i)
                  c2r      = p1*p1
c
                  p1       = c0*c0 * Af0 * r0 + c1*c1 * Af1 * r1 +
     +                       c2*c2 * Af2 * r2
                  p2       = c2r * Af(i)
                  alfab(i) = p1/p2
c
c                 Compute R and C for total cross section
c
                  r(i) = Af(i) / o(i)
                  c(i) = sqrt ( c2r / r(i) )
               endif
  100       continue
            endif
c
c remove writing to file
c
c         loop = .true.
c         do 130 i = i1,i2
c            if (loop .and.
c     +         (c(i) .lt. 1. .or. c(i) .gt. 1000. .or.
c     +          r(i) .lt. .001 .or. r(i) .gt. 1000.)) then
c               loop = .false.
c               ker  = warnng
c               call getloc (i,ibr,xc)
c               write (xtxt,'(f10.2)') xc
c               call getbrn (ibr,branam,lbrnam)
c               call sre_error (juer ,
c     +           'FLBOCH roughness or hydraulic radius out of limit'//
c     +           ' at branch @'//branam(:lbrnam)//'@ X= @' //xtxt//'@',
c     +           eflcrl , ker )
c            endif
  130    continue
c
cmb - replace underrelaxation of r by underrelaxation of c2r
c
         do 150 i = i1,i2
            c2r = c(i)**2 * r(i)
            if (iter .eq. 1) then
               alfabp(i) = alfab(i)
               c2rp  (i) = c2r
               wfp   (i) = wf   (i)
            endif
            alfab (i) = omalfa*alfab(i) + (1.-omalfa)*alfabp(i)
            alfabp(i) = alfab(i)
            c2r       = omr*c2r  + (1.-omr)*c2rp(i)
            c2rp  (i) = c2r
C
Cmb - recompute c using underrelaxed c2r
            c(i) = sqrt ( c2r / r(i) )
C
            wfp   (i) = omw*wf(i) + (1.-omw)*wfp(i)
  150    continue
c
  200 continue
c
      end
