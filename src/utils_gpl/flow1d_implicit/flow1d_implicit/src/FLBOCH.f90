subroutine FLBOCH(nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,&
&h1     ,h      ,q1     ,q      ,maxlev ,hlev   ,&
&wft    ,maxtab ,ntabm  ,ntab   ,table  ,subsec ,&
&secth0 ,secth1 ,wfh0   ,wfh1   ,grsize ,engpar ,&
&af     ,o      ,afh0   ,afh1   ,oh0    ,oh1    ,&
&prslot ,psltvr ,asubsc ,c      ,r      ,cs     ,&
&rs     ,alfab  ,&
&iter   ,theta2 ,omalfa ,omr    ,omw    ,&
&alfabp ,c2rp   ,wfp    ,wf     ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLBOCH (FLow BOussinesq and CHezy coefficient)
!
! Module description: In subroutine FLBOCH the Boussinesq's constant and
!                     the Chezy coefficients will be computed.
!
!                     The computation of Boussinesq's constant alfab
!                     will be done in the same way as in WENDY. The
!                     cross sections may be divided into a main section,
!                     sub section 1 and sub section 2. Each (sub-) sec-
!                     tion has its own Chezy-formula (Nikuradse, Man-
!                     ning, etc.). In each section the Chezy coefficient
!                     and the hydraulic radius will be computed.
!
!                     Since this item is elaborated extensively in the
!                     Functional Design Report [S-FO-001.5KV], reference
!                     is made to this report. See formulae (5-6) until
!                     (5-11) in S-FO-001.5KV.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 25 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 27 afh0(ngrid)       I  Flow area Af at water level h=h0 for every
!                         grid point.
! 28 afh1(ngrid)       I  Flow area Af at water level h=h1 for every
!                         grid point.
! 38 alfab(ngrid)      IO Actual Bousinessq coefficient in grid point i.
! 44 alfabp(ngrid)     IO Bousinessq coefficient in grid point i.
!                         Calculated with underrelaxation.
!                         Input parameter on the former iteration level.
!                         Output parameter on the actual iteration level
! 33 asubsc(ngrid)     IO Defines the actual number of sub sections for
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
! 34 c(ngrid)          IO Actual Chezy coefficient for total channel in
!                         every grid point.
! 45 c2rp(ngrid)       IO C**2*R for the total channel in every grid
!                         point. (C=Chezy coefficient, R=Hydraulic
!                         Radius)
!                         Calculated with underrelaxation.
!                         Input parameter on the former iteration level.
!                         Output parameter on the actual iteration level
! 36 cs(ngrid,3)       O  Actual Chezy coefficient in a section (main,
!                         sub 1, sub 2) for every grid point.
! 24 engpar            P  -
! 23 grsize(4,ngrid,*) I  Grain sizes for each gridpoint and section.
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
!  7 h1(ngrid)         I  Water level in every grid point at time t(n).
!  8 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
! 12 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
! 39 iter              I  Iteration step.
! 11 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 14 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
! 16 ntab              P  -
! 15 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 26 o(ngrid)          I  Wetted perimeter for total cross section.
! 29 oh0(ngrid)        I  Wetted perimeter Ot at water level h=h0 for
!                         every grid point.
! 30 oh1(ngrid)        I  Wetted perimeter Ot at water level h=h1 for
!                         every grid point.
! 41 omalfa            I  underrelaxation parameter Boussinesq ALFAB
! 42 omr               I  Underrelaxation factor omega for the hydraulic
!                         radius R.
! 43 omw               I  underrelaxation parameter Flow width Wf
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
!  9 q1(ngrid)         I  Discharge in every grid point at time t(n).
! 10 q(ngrid)          I  Discharge in every grid point at the latest
!                         iteration.
! 35 r(ngrid)          IO Actual hydraulic radius for total channel in
!                         every grid point.
! 37 rs(ngrid,3)       O  Actual hydraulic radius in a section (main,
!                         sub 1, sub 2) for every grid point.
! 19 secth0(ngrid)     I  H0-value (for 1 or 2 sub sections) for every
!                         grid point.
! 20 secth1(ngrid)     I  H0-value (for 2 sub section) for every grid
!                         point.
! 18 subsec(ngrid)     I  Defines the number of sub sections for every
!                         cross section:
!                         c1sec (0) : Main section only (0 sub sections)
!                         c2sec (1) : 1 sub section
!                         c3sec (2) : 2 sub sections
!                         (For a circle cross section   : 0 ;
!                          For a sedredge cross section : 1 )
! 17 table             P  -
! 40 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
!  4 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 47 wf(ngrid)         I  Actual flow width at every grid point.
! 21 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
!                         grid point.
! 22 wfh1(ngrid)       I  Flow width Wf at water level h=h1 for every
!                         grid point.
! 46 wfp(ngrid)        IO Flow width in every grid point.
!                         Calculated with underrelaxation.
!                         Input parameter on the former iteration level.
!                         Output parameter on the actual iteration level
! 13 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
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
! $Log: flboch.pf,v $
! Revision 1.15  1999/06/01  13:42:12  kuipe_j
! names in messages substituted + message template
!
! Revision 1.14  1999/03/15  14:27:41  kuipe_j
! bed friction table general
!
! Revision 1.13  1997/08/21  10:55:01  kuipe_j
! Check for negative areas
!
! Revision 1.12  1997/08/08  10:53:47  kuipe_j
! Change check limits
!
! Revision 1.11  1997/05/26  07:43:32  kuipe_j
! Check on R andC added
!
! Revision 1.10  1997/05/06  11:39:59  kuipe_j
! Protec against zero wet perimeter
!
! Revision 1.9  1997/01/23  08:28:59  kuipe_j
! Make flow module robust
!
! Revision 1.8  1996/04/11  08:23:11  kuipe_j
! Kalman module added
!
! Revision 1.7  1995/09/22  10:01:00  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:10:49  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:36:26  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:12  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:54:46  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:43  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:30  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/12/02  13:17:21  kuipe_j
! Take care that if sub section 1 = 0 then always sub section2 = 0.
!
! Revision 1.2  1993/11/26  15:30:33  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:46  kuipe_j
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

!
!     Declaration of local variables:
!
   logical lslot
   integer ibr, i1, i2, i
   double precision hi, qi
   real    ui, h0, hh1, dz,&
   &af0, o0, r0, c0,&
   &af1, o1, r1, c1,&
   &af2, o2, r2, c2,&
   &p1, p2, c2r, d90,&
   &zbmain, wmain, amain, omain, rmain, cmain,&
   &zbsub1, wsub1, asub1, osub1, rsub1, csub1
!
!     Include sobek constants
!
   include '../include/sobcon.i'
   include '../include/errcod.i'
!
   do 200 ibr = 1, nbran
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
         do 50 i = i1, i2
!
!              hi : waterlevel h=h(n+theta2)
!              qi : discharge  q=q(n+theta2)
!
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
!
!              Compute Chezy coefficient cmain from hydraulic radius
!              rmain
!
            call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,bfrict ,&
            &bfricp ,maxtab ,ntabm  ,ntab   ,table  ,&
            &d90    ,engpar ,0      ,hi     ,qi     ,&
            &ui     ,rmain  ,cmain  )
!
!              Store R and C for main section.
!
            rs(i,1) = rmain
            cs(i,1) = cmain
            d90     = grsize(3,i,2)

            asub1 = (hi - zbsub1) * wsub1
            rsub1 = asub1 / osub1
            csub1 = cs(i,2)
!
!              Compute Chezy coefficient csub1 from hydraulic radius
!              rsub1
!
            call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,bfrict ,&
            &bfricp ,maxtab ,ntabm  ,ntab   ,table  ,&
            &d90    ,engpar ,1      ,hi     ,qi     ,&
            &ui     ,rsub1  ,csub1  )
!
!              Store R and C for sub1 section. (sub section 1)
!
            rs(i,2) = rsub1
            cs(i,2) = csub1
!
!              **********************************************
!              * Computation for total cross section        *
!              * 1. Chezy coefficient C                     *
!              * 2. Hydraulic radius R                      *
!              * 3. Boussinesq constant alfab               *
!              **********************************************
!
            p1       = ( cmain * amain * sqrt(rmain) +&
            &csub1 * asub1 * sqrt(rsub1) ) / Af(i)
            c2r      = p1*p1

            p1       = cmain*cmain * amain * rmain +&
            &csub1*csub1 * asub1 * rsub1
            p2       = c2r   * Af(i)
            alfab(i) = p1/p2
!
!              Compute R and C for total cross section
!
            r(i) = Af(i) / o(i)
            c(i) = sqrt ( c2r / r(i) )
50       continue

      else
!
!           Loop over cross sections (grid points)
!
         do 100 i = i1, i2
!
!              hi : waterlevel h=h(n+theta2)
!              qi : discharge  q=q(n+theta2)
!
            hi  = theta2*h(i)+(1.-theta2)*h1(i)
            qi  = theta2*q(i)+(1.-theta2)*q1(i)
            ui  = qi / af(i)
            d90 = grsize(3,i,1)
!
!              Define actual number of subsections
!
            call FLNSEC(real(hi),i,asubsc(i) ,subsec ,secth0 ,secth1,&
            &wfh0 ,wfh1 ,af     ,afh0   ,afh1   ,ngrid )
!
            h0  = secth0(i)
            hh1 = secth1(i)
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
!                 Doc: S-FO-001.5KV  Eq. 5-6 / 5-7 / 5-9
!
!                 r    : hydraulic radius
!                 c0   : Chezy coeffient for main section
!
               r0  = Af(i) / o(i)
               c0  = cs(i,1)
!
!                 Compute Chezy coefficient c0 from hydraulic radius r0
!
               if ( lslot .and. hi .lt. psltvr(5,i) ) then
                  if ( qi .gt. 0D0 ) then
                     c0 = sqrt (psltvr(1,i)/r0)
                  else
                     c0 = sqrt (psltvr(2,i)/r0)
                  endif
               else
                  call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                  &bfrict ,bfricp ,maxtab ,ntabm  ,&
                  &ntab   ,table  ,d90    ,engpar ,&
                  &0      ,hi     ,qi     ,ui     ,&
                  &r0     ,c0     )
               endif
!
               alfab(i) = 1.0
!
!                 Store R and C for total cross section and per sub
!                 section
!
               r (i)   = r0
               c (i)   = c0
               rs(i,1) = r0
               cs(i,1) = c0
!
            else if ( int( asubsc(i) ) .eq. 1 ) then
!
!                 ** Situation ***********
!                 *  1 or 2 sub sections *
!                 *  h0 <= h < hh1       *
!                 ************************
!
!                 ****************
!                 * main section *
!                 ****************
!                 Doc: S-FO-001.5KV  Eq. 5-8 / 5-10
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
               if ( lslot .and. hi .lt. psltvr(5,i) ) then
                  if ( qi .gt. 0D0 ) then
                     c0 = sqrt (psltvr(1,i)/r0)
                  else
                     c0 = sqrt (psltvr(2,i)/r0)
                  endif
               else
                  call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                  &bfrict ,bfricp ,maxtab ,ntabm  ,&
                  &ntab   ,table  ,d90    ,engpar ,&
                  &0      ,hi     ,qi     ,ui     ,&
                  &r0     ,c0     )
               endif
!
!                 Store R and C for main section. (sub section 0)
!
               rs(i,1) = r0
               cs(i,1) = c0
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
               c1     = cs(i,2)

!                 Provision against too small wetted perimeter
!                 and negative area
               if (o1 .le. .001 .or. af1 .le. 1.e-6) then
                  r1 = (hi-h0) *.5
               else
                  r1 = Af1 / o1
               endif
!
!                 Compute Chezy coefficient c1 from hydraulic radius r1
!
               if ( lslot .and. hi .lt. psltvr(5,i) ) then
                  if ( qi .gt. 0. ) then
                     c1 = sqrt (psltvr(1,i)/r1)
                  else
                     c1 = sqrt (psltvr(2,i)/r1)
                  endif
               else
                  call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                  &bfrict ,bfricp ,maxtab ,ntabm  ,&
                  &ntab   ,table  ,d90    ,engpar ,&
                  &1      ,hi     ,qi     ,ui     ,&
                  &r1     ,c1     )
               endif
!
!                 Store R and C for sub section 1
!
               rs(i,2) = r1
               cs(i,2) = c1
!
!                 **********************************************
!                 * Computation for total cross section        *
!                 * 1. Chezy coefficient C                     *
!                 * 2. Hydraulic radius R                      *
!                 * 3. Boussinesq constant alfab               *
!                 **********************************************
!
               p1       = ( c0*Af0*sqrt(r0) +&
               &c1*Af1*sqrt(r1) ) / Af(i)
               c2r      = p1*p1

               p1       = c0*c0 * Af0 * r0 + c1*c1 * Af1 * r1
               p2       = c2r   * Af(i)
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
!                 *  h > hh1             *
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
               if ( lslot .and. hi .lt. psltvr(5,i) ) then
                  if ( qi .gt. 0D0 ) then
                     c0 = sqrt (psltvr(1,i)/r0)
                  else
                     c0 = sqrt (psltvr(2,i)/r0)
                  endif
               else
                  call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                  &bfrict ,bfricp ,maxtab ,ntabm  ,&
                  &ntab   ,table  ,d90    ,engpar ,&
                  &0      ,hi     ,qi     ,ui     ,&
                  &r0     ,c0     )
               endif
!
!                 Store R and C for main section
!
               rs(i,1) = r0
               cs(i,1) = c0
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
               if (o1 .le. .001 .or. af1 .le. 1.e-6) then
                  r1 = (hi-h0) *.5
               else
                  r1 = Af1 / o1
               endif

               c1      = cs(i,2)
!
!                 Compute Chezy coefficient c1 from hydraulic radius r1
!
               if ( lslot .and. hi .lt. psltvr(5,i) ) then
                  if ( qi .gt. 0D0 ) then
                     c1 = sqrt (psltvr(1,i)/r1)
                  else
                     c1 = sqrt (psltvr(2,i)/r1)
                  endif
               else
                  call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                  &bfrict ,bfricp ,maxtab ,ntabm  ,&
                  &ntab   ,table  ,d90    ,engpar ,&
                  &1      ,hi     ,qi     ,ui     ,&
                  &r1     ,c1     )
               endif
!
!                 Store R and C for sub section 1
!
               rs(i,2) = r1
               cs(i,2) = c1
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
               c2     = cs(i,3)

!                 Provision against too small wetted perimeter
!                 and negative area
               if (o2 .le. .001 .or. af2 .le. 1.e-6) then
                  r2 = (hi-hh1) *.5
               else
                  r2 = Af2 / o2
               endif
!
!                 Compute Chezy coefficient c2 from hydraulic radius r2
!
               if ( lslot .and. hi .lt. psltvr(5,i) ) then
                  if ( qi .gt. 0D0 ) then
                     c2 = sqrt (psltvr(1,i)/r2)
                  else
                     c2 = sqrt (psltvr(2,i)/r2)
                  endif
               else
                  call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                  &bfrict ,bfricp ,maxtab ,ntabm  ,&
                  &ntab   ,table  ,d90    ,engpar ,&
                  &2      ,hi     ,qi     ,ui     ,&
                  &r2     ,c2     )
               endif
!
!                 Store R and C for sub section 2
!
               rs(i,3) = r2
               cs(i,3) = c2
!
!                 **********************************************
!                 * Computation for total cross section        *
!                 * 1. Chezy coefficient C                     *
!                 * 2. Hydraulic radius R                      *
!                 * 3. Boussinesq constant alfab               *
!                 **********************************************
!
               p1       = ( c0*Af0*sqrt(r0) +&
               &c1*Af1*sqrt(r1) +&
               &c2*Af2*sqrt(r2) ) / Af(i)
               c2r      = p1*p1
!
               p1       = c0*c0 * Af0 * r0 + c1*c1 * Af1 * r1 +&
               &c2*c2 * Af2 * r2
               p2       = c2r * Af(i)
               alfab(i) = p1/p2
!
!                 Compute R and C for total cross section
!
               r(i) = Af(i) / o(i)
               c(i) = sqrt ( c2r / r(i) )
            endif
100      continue
      endif
!
! remove writing to file
!
!         loop = .true.
!         do 130 i = i1,i2
!            if (loop .and.
!     +         (c(i) .lt. 1. .or. c(i) .gt. 1000. .or.
!     +          r(i) .lt. .001 .or. r(i) .gt. 1000.)) then
!               loop = .false.
!               ker  = warnng
!               call getloc (i,ibr,xc)
!               write (xtxt,'(f10.2)') xc
!               call getbrn (ibr,branam,lbrnam)
!               call sre_error (juer ,
!     +           'FLBOCH roughness or hydraulic radius out of limit'//
!     +           ' at branch @'//branam(:lbrnam)//'@ X= @' //xtxt//'@',
!     +           eflcrl , ker )
!            endif
130   continue
!
!mb - replace underrelaxation of r by underrelaxation of c2r
!
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
!
!mb - recompute c using underrelaxed c2r
         c(i) = sqrt ( c2r / r(i) )
!
         wfp   (i) = omw*wf(i) + (1.-omw)*wfp(i)
150   continue
!
200 continue
!
end
