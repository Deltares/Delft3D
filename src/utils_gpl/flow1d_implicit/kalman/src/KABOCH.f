      subroutine KABOCH(nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,
     +                  h1     ,h      ,q1     ,q      ,theta2 ,dh     ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,
     +                  subsec ,secth0 ,secth1 ,wfh0   ,wfh1   ,grsize ,
     +                  engpar ,afacc  ,oacc   ,afh0   ,afh1   ,oh0    ,
     +                  oh1    ,prslot ,asubsc ,c      ,r      ,cs     ,
     +                  alfab  ,dcdh   ,drdh   ,dalfdh ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KABOCH (KAlman Boussinesq and Ch‚zy coefficient)
c
c Module description: In subroutine KABOCH derivatives to water levels of the
c                     Boussinesq's constant, the Ch‚zy coefficients and the
c                     hydraulic radius will be computed.
c
c                     The derivatives of the coefficients are determined
c                     by numerical differentiation. In the flow module
c                     (FLBOCH) the coefficients have been calculated for
c                     the current water level. In KABOCH the calculation
c                     is done again for an incremented water level.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 26 afacc(ngrid)      I  Flow area in every grid point i on time
c                         n+1/2+dh.
c 28 afh0(ngrid)       I  Flow area Af at water level h=h0 for every
c                         grid point.
c 29 afh1(ngrid)       I  Flow area Af at water level h=h1 for every
c                         grid point.
c 37 alfab(ngrid)      IO Actual Bousinessq coefficient in grid point i.
c 33 asubsc(ngrid)     IO Defines the actual number of sub sections for
c                         avery cross section (depending on the actual
c                         water level):
c                         c1sec (0) : Main section only (0 sub sections)
c                         c2sec (1) : 1 sub section
c                         c3sec (2) : 2 sub sections
c  6 bfricp            P  -
c  5 bfrict(3,nbran)   I  Bed friction in sections of branch.
c                         For each branch and section a different fric-
c                         tion type can be defined.
c                         (1,i) = Friction type in main section in
c                                 branch i:
c                                 cfrchc (1) : Chezy constant
c                                 cfrchq (2) : Chezy function of Q
c                                 cfrchh (3) : Chezy function of h
c                                 cfrman (4) : Manning constant
c                                 cfrskn (5) : Strickler 1 constant Kn
c                                 cfrsks (6) : Strickler 2 constant Ks
c                                 cfrnik (7) : Nikuradze constant
c                                 cfreng (8) : Engelund predictor
c                         (2,i) = Friction type in sub section 1 in
c                                 branch i:
c                                 cfrchc (1) : Chezy constant
c                                 cfrman (4) : Manning constant
c                                 cfrskn (5) : Strickler 1 constant Kn
c                                 cfrsks (6) : Strickler 2 constant Ks
c                                 cfrnik (7) : Nikuradze constant
c                         (3,i) = Friction type in sub section 2 in
c                                 branch i:
c                                 Same definitions as bfrict(2,i)
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 34 c(ngrid)          IO Actual Chezy coefficient for total channel in
c                         every grid point.
c 36 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
c                         sub 1, sub 2) for every grid point.
c 40 dalfdh(ngrid)     O  Derivative of Bousinesq constant ALFAb to
c                         waterlevel in every grid point i on time n+1/2
c                         (d(ALFAb)/dh).
c 38 dcdh(ngrid)       O  Derivative of Chezy value to waterlevel in every
c                         grid point i on time n+1/2 (dC/dh).
c 12 dh                I  -
c 39 drdh(ngrid)       O  Derivative of hydraulic radius to waterlevel in
c                         every grid point i on time n+1/2 (dR/dh).
c 25 engpar            P  -
c 24 grsize(4,ngrid,*) I  Grain sizes for each gridpoint and section.
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
c  8 h(ngrid)          I  Contains water levels in every grid point. It is
c                         stored on index 2 of the packed array hpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c 15 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c 17 ntab              P  -
c 16 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 27 oacc(ngrid)      I  wetted perimeter in every grid point i on time
c                         n+1/2+dh.
c 30 oh0(ngrid)        I  Wetted perimeter Ot at water level h=h0 for
c                         every grid point.
c 31 oh1(ngrid)        I  Wetted perimeter Ot at water level h=h1 for
c                         every grid point.
c 32 prslot(3,nbran)   I  Contains information concerning Preissmann
c                         slot (assuring positive water depths).
c                         (1,i) = Create slot indicator
c                                 csldis (0) : No slot in this branch.
c                                 cslena (1) : Create slot in branch.
c                         (2,i) = Reference area for slot generation.
c                         (3,i) = Depth of slot below lowest bottom of
c                                 branch.
c 41 psltvr(7,ngrid)   I  Preissmann slot variables for every grid point
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
c 10 q(ngrid)          I  Contains discharges in every grid point. It is
c                         stored on index 2 of the packed array qpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c 35 r(ngrid)          IO Actual hydraulic radius for total channel in
c                         every grid point.
c 20 secth0(ngrid)     I  H0-value (for 1 or 2 sub sections) for every
c                         grid point.
c 21 secth1(ngrid)     I  H0-value (for 2 sub section) for every grid
c                         point.
c 19 subsec(ngrid)     I  Defines the number of sub sections for every
c                         cross section:
c                         c1sec (0) : Main section only (0 sub sections)
c                         c2sec (1) : 1 sub section
c                         c3sec (2) : 2 sub sections
c                         (For a circle cross section   : 0 ;
c                          For a sedredge cross section : 1 )
c 18 table             P  -
c 11 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c  4 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 22 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
c                         grid point.
c 23 wfh1(ngrid)       I  Flow width Wf at water level h=h1 for every
c                         grid point.
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
c $Log: kaboch.pf,v $
c Revision 1.8  1999/03/15  13:52:48  kuipe_j
c table for bed friction general
c
c Revision 1.7  1997/08/21  11:11:04  kuipe_j
c Check for negative areas and wetted per.
c
c Revision 1.6  1997/06/17  11:23:42  kuipe_j
c Initialize vars
c
c Revision 1.5  1997/01/23  08:29:33  kuipe_j
c Make flow module robust
c
c Revision 1.4  1996/04/12  15:18:20  kuipe_j
c *** empty log message ***
c
c Revision 1.3  1996/04/12  14:57:35  kuipe_j
c Comment leader
c
c Revision 1.2  1996/04/12  13:04:40  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:16  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer maxtab, ntabm, ntab(4,maxtab)
      integer nbran, ngrid, branch(4,nbran)
      integer typcr(nbran), bfrict(3,nbran)
      real    theta2, dh
      real    table(ntabm), subsec(ngrid), asubsc(ngrid)
      real    afacc(ngrid), oacc(ngrid)
      real    cs(ngrid,3), alfab(ngrid)
      real    afh0(ngrid), afh1(ngrid), oh0(ngrid), oh1(ngrid)
      real    c(ngrid), r(ngrid)
      real    grsize(4,ngrid,*), engpar(9)
      real    secth0(ngrid), secth1(ngrid)
      real    wfh0(ngrid), wfh1(ngrid)
      real    bfricp(6,ngrid), prslot(3,nbran)
      real    dcdh(ngrid), drdh(ngrid), dalfdh(ngrid)
      real    psltvr(7,ngrid)

      double precision h1(ngrid), h(ngrid), q1(ngrid), q(ngrid)

c
c     Declaration of local variables:
c
      logical lslot
      integer ibr, i1, i2, i  ,nsubs
      real    hi, qi, ui, h0, hh1,
     +        af0, o0, r0, c0,
     +        af1, o1, r1, c1,
     +        af2, o2, r2, c2,
     +        p1, p2, c2r, d90,
     +        alfacc, racc, cacc, asub
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      do 200 ibr = 1, nbran
c
c       i1 = global grid point number at node n1
c       i2 = global grid point number at node n2
c
        i1 = branch (3,ibr)
        i2 = branch (4,ibr)
c
        lslot = int(prslot(1,ibr)) .eq. cslena
c
        if ( typcr(ibr) .eq. ccrsed ) then
c
c          Sedredge branch
c
        else
c
c          Loop over cross sections (grid points)
c
           do 100 i = i1, i2
c
c             hi : waterlevel h=h(n+theta)+dh
c
              hi  = theta2*h(i)+(1.-theta2)*h1(i) + dh
              qi  = theta2*q(i)+(1.-theta2)*q1(i)
              ui  = qi / afacc(i)
              d90 = grsize(3,i,1)
c
              if ( lslot .and. hi .lt. psltvr(5,i) ) then
                 dcdh(i)   = 0.
                 drdh(i)   = 0.
                 dalfdh(i) = 0.
              else
c
c                Define actual number of subsections
c
                 call FLNSEC(hi   ,i    ,asub ,subsec ,secth0 ,secth1,
     +                       wfh0 ,wfh1 ,afacc  ,afh0   ,afh1   ,ngrid )
                 if (dh .lt. 1.e-10) then
                     nsubs = int(asub)
                     asubsc(i) = asub
                 else
                     nsubs = int(asub)
                 endif
                 h0     = secth0(i)
                 hh1    = secth1(i)

c
c                * Situation *****************************
c                * 1. main section                       *
c                * 2. 1 or 2 sub sections, but h < h0    *
c                *****************************************
c
                 if   ( nsubs .eq. 0 ) then
c
c                   ****************
c                   * main section *
c                   ****************
c                   Doc: S-FO-001.5KV  Eq. 5-6 / 5-7 / 5-9
c
c                   r    : hydraulic radius
c                   c0   : Chezy coeffient for main section
c
                    r0  = Afacc(i) / oacc(i)
c
c                   Compute Chezy coefficient c0 from hydraulic
c                   radius r0
c
                    if (     bfrict(1,ibr)     .ne. cfrchc .and.
     +                   mod(bfrict(1,ibr),10) .ne. cfrchq )     then
                        call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                              bfrict ,bfricp ,maxtab ,ntabm  ,
     +                              ntab   ,table  ,d90    ,engpar ,
     +                              0      ,hi     ,qi     ,ui     ,
     +                              r0     ,c0     )
                    else
                       c0 = cs(i,1)
                    endif
c
                    alfacc = 1.0
c
c                   Compute R' and C' for total cross section
c
                    racc   = r0
                    cacc   = c0
c
                 else if ( nsubs .eq. 1 ) then
c
c                   ** Situation ***********
c                   *  1 or 2 sub sections *
c                   *  h0 <= h < hh1        *
c                   ************************
c
c                   ****************
c                   * main section *
c                   ****************
c                   Doc: S-FO-001.5KV  Eq. 5-8 / 5-10
c
c                   Af0  = flow area in main section
c                   Afh0 = flow area in main section for h = h0
c                   oh0  = wetted perimeter in main section for h = h0
c                   r0   = hydraulic radius for main section
c
                    Af0  = Afh0(i) + (hi-h0) * Wfh0(i)
                    o0   = oh0(i)
                    r0   = Af0 / o0
c
c                   Compute Chezy coefficient c0 from hydraulic radius
c                   r0
c                   according to the user selected Chezy formula.
c
                    if (     bfrict(1,ibr)     .ne. cfrchc .and.
     +                   mod(bfrict(1,ibr),10) .ne. cfrchq )     then
                        call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                              bfrict ,bfricp ,maxtab ,ntabm  ,
     +                              ntab   ,table  ,d90    ,engpar ,
     +                              0      ,hi     ,qi     ,ui     ,
     +                              r0     ,c0     )
                    else
                       c0 = cs(i,1)
                    endif
c
c                   *****************
c                   * sub section 1 *
c                   *****************
c                   Doc: S-FO-001.5KV  Eq. 5-8
c
c                   Actual parameters:
c
c                   Af(i)= actual flow area total cross section
c                   o(i) = wetted perimeter for total cross section
c
c                   Af1  = flow area in sub section 1
c                   r1   = hydraulic radius for sub section 1
c                   c1   = Chezy coefficient for sub section 1
c
                    Af1  = Afacc(i) - Af0
                    o1   = oacc(i) - oh0(i)
c                   Provision against too small wetted perimeter
c                   and negative area
                    if (o1 .le. .001 .or. af1 .le. 1.e-6) then
                       r1 = (hi-h0) *.5
                    else
                       r1 = Af1 / o1
                    endif
c
c                   Compute Chezy coefficient c1 from hydraulic radius
c                   r1
c
                    if (     bfrict(2,ibr)     .ne. cfrchc .and.
     +                   mod(bfrict(2,ibr),10) .ne. cfrchq )     then
                        call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                             bfrict ,bfricp ,maxtab ,ntabm  ,
     +                             ntab   ,table  ,d90    ,engpar ,
     +                             1      ,hi     ,qi     ,ui     ,
     +                             r1     ,c1     )
                    else
                        c1 = cs(i,2)
                    endif
c
c                   **********************************************
c                   * Computation for total cross section        *
c                   * 1. Chezy coefficient C                     *
c                   * 2. Hydraulic radius R                      *
c                   * 3. Boussinesq constant alfab               *
c                   **********************************************
c
                    p1     = ( c0*Af0*sqrt(r0) +
     +                          c1*Af1*sqrt(r1) ) / Afacc(i)
                    c2r    = p1*p1

                    p1     = c0*c0 * Af0 * r0 + c1*c1 * Af1 * r1
                    p2     = c2r   * Afacc(i)
c
                    alfacc = p1/p2
c
c                   Compute R' and C' for total cross section
c
                    racc   = Afacc(i) / oacc(i)
                    cacc   = sqrt ( c2r / racc )

                 else if ( nsubs .eq. 2 ) then
c
c                   ** Situation ***********
c                   *  2 sub sections      *
c                   *  h > hh1              *
c                   ************************
c
c                   ****************
c                   * main section *
c                   ****************
c                   Doc: S-FO-001.5KV  Eq. 5-11
c
c                   Afh0 = flow area in main section for h = h0
c                   oh0  = wetted perimeter in main section for h = h0
c
c                   Af0  = flow area in main section
c                   r0   = hydraulic radius for main section
c
                    Af0  = Afh0(i) + (hi-h0) * Wfh0(i)
                    o0   = oh0(i)
                    r0   = Af0 / o0
c
c                   Compute Chezy coefficient c0 from hydraulic radius
c                   r0
c
                    if (     bfrict(1,ibr)     .ne. cfrchc .and.
     +                   mod(bfrict(1,ibr),10) .ne. cfrchq )     then
                         call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                             bfrict ,bfricp ,maxtab ,ntabm  ,
     +                             ntab   ,table  ,d90    ,engpar ,
     +                             0      ,hi     ,qi     ,ui     ,
     +                             r0     ,c0     )
                    else
                       c0 = cs(i,1)
                    endif
c
c                   *****************
c                   * sub section 1 *
c                   *****************
c                   Doc: S-FO-001.5KV  Eq. 5-11
c
c                   Actual parameters:
c                   Af1  = flow area in sub section 1
c                   r1   = hydraulic radius for sub section 1
c                   c1   = Chezy coefficient for sub section 1

                    Af1  = Afh1(i) + (hi-hh1) * Wfh1(i) - Af0
                    o1   = oh1(i)
                    r1   = Af1 / o1
c
c                   Compute Chezy coefficient c1 from hydraulic radius
c                   r1
c
                    if (     bfrict(2,ibr)     .ne. cfrchc .and.
     +                   mod(bfrict(2,ibr),10) .ne. cfrchq )     then
                        call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                              bfrict ,bfricp ,maxtab ,ntabm  ,
     +                              ntab   ,table  ,d90    ,engpar ,
     +                              1      ,hi     ,qi     ,ui     ,
     +                              r1     ,c1     )
                    else
                        c1 = cs(i,2)
                    endif
c
c                   *****************
c                   * sub section 2 *
c                   *****************
c                   Doc: S-FO-001.5KV  Eq. 5-11
c
c                   Actual parameters:
c
c                   Af(i)= actual flow area total cross section
c                   o(i) = wetted perimeter for total cross section
c
c                   Af2  = flow area in sub section 2
c                   r2   = hydraulic radius for sub section 2
c                   c2   = Chezy coefficient for sub section 2

                    Af2  = Afacc(i) - Af1 - Af0
                    o2   = oacc(i) - oh1(i) - oh0(i)
c                   Provision against too small wetted perimeter
c                   and negative area
                    if (o2 .le. .001 .or. af2 .le. 1.e-6) then
                       r2 = (hi-hh1) *.5
                    else
                       r2 = Af2 / o2
                    endif
c
c                   Compute Chezy coefficient c2 from hydraulic radius
c                   r2
c
                    if (     bfrict(3,ibr)     .ne. cfrchc .and.
     +                   mod(bfrict(3,ibr),10) .ne. cfrchq )     then
                        call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                              bfrict ,bfricp ,maxtab ,ntabm  ,
     +                              ntab   ,table  ,d90    ,engpar ,
     +                              2      ,hi     ,qi     ,ui     ,
     +                              r2     ,c2     )
                    else
                        c2 = cs(i,3)
                    endif
c
c                   **********************************************
c                   * Computation for total cross section        *
c                   * 1. Chezy coefficient C                     *
c                   * 2. Hydraulic radius R                      *
c                   * 3. Boussinesq constant alfab               *
c                   **********************************************
c
                    p1     = ( c0*Af0*sqrt(r0) +
     +                         c1*Af1*sqrt(r1) +
     +                         c2*Af2*sqrt(r2) ) / Afacc(i)
                    c2r    = p1*p1
c
                    p1     = c0*c0 * Af0 * r0 + c1*c1 * Af1 * r1 +
     +                       c2*c2 * Af2 * r2
                    p2     = c2r * Afacc(i)
                    alfacc = p1/p2
c
c                   Compute R' and C' for total cross section
c
                    racc = Afacc(i) / oacc(i)
                    cacc = sqrt ( c2r / racc )

                 endif
                 if (dh .gt. 0.) then
                    dcdh(i)   = (cacc - c(i)) / dh
                    drdh(i)   = (racc - r(i)) / dh
                    dalfdh(i) = (alfacc - alfab(i)) / dh
                 else
                    c(i)     = cacc
                    r(i)     = racc
                    alfab(i) = alfacc
                 endif
              endif
  100       continue
        endif
  200 continue
c
      end
