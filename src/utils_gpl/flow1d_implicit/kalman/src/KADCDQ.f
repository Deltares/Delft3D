      subroutine KADCDQ(nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,
     +                  h1     ,h      ,q1     ,q      ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,theta2 ,dh     ,
     +                  secth0 ,secth1 ,wfh0   ,wfh1   ,grsize ,engpar ,
     +                  af     ,o      ,afh0   ,afh1   ,
     +                  prslot ,asubsc ,c      ,r      ,cs     ,
     +                  rs     ,dcdq   ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KADCDQ (KAlman Derivative of Ch‚zy to Discharge Q)
c
c Module description: In this routine derivatives of the Che‚zy coe-
c                     ficients to the discharge will be computed.
c
c                     The derivatives are determined by numerical diffe-
c                     rentiation. In the flow module (FLBOCH) the Chezy
c                     coefficients have been calculated for the current
c                     discharge. In KADCDQ the calculation is done again
c                     for an incremented discharge.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 25 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c 27 afh0(ngrid)       I  Flow area Af at water level h=h0 for every
c                         grid point.
c 28 afh1(ngrid)       I  Flow area Af at water level h=h1 for every
c                         grid point.
c 30 asubsc(ngrid)     I  Defines the actual number of sub sections for
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
c 31 c(ngrid)          IO Actual Chezy coefficient for total channel in
c                         every grid point.
c 33 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
c                         sub 1, sub 2) for every grid point.
c 35 dcdq(ngrid)       O  Derivative of Chezy value to discharge in every
c                         grid point i on time n+1/2 (dC/dQ).
c 18 dh                I  -
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
c  8 h(ngrid)          I  Contains water levels in every grid point. It is
c                         stored on index 2 of the packed array hpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c 13 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c 15 ntab              P  -
c 14 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 26 o(ngrid)          I  Wetted perimeter for total cross section.
c 29 prslot(3,nbran)   I  Contains information concerning Preissmann
c                         slot (assuring positive water depths).
c                         (1,i) = Create slot indicator
c                                 csldis (0) : No slot in this branch.
c                                 cslena (1) : Create slot in branch.
c                         (2,i) = Reference area for slot generation.
c                         (3,i) = Depth of slot below lowest bottom of
c                                 branch.
c 36 psltvr            I  Preissmann slot variables for every grid point
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
c 32 r(ngrid)          IO Actual hydraulic radius for total channel in
c                         every grid point.
c 34 rs(ngrid,3)       I  Actual hydraulic radius in a section (main,
c                         sub 1, sub 2) for every grid point.
c 19 secth0(ngrid)     I  H0-value (for 1 or 2 sub sections) for every
c                         grid point.
c 20 secth1(ngrid)     I  H0-value (for 2 sub section) for every grid
c                         point.
c 16 table             P  -
c 17 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c  4 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 21 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
c                         grid point.
c 22 wfh1(ngrid)       I  Flow width Wf at water level h=h1 for every
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
c $Log: kadcdq.pf,v $
c Revision 1.6  1999/03/15  13:52:50  kuipe_j
c table for bed friction general
c
c Revision 1.5  1997/07/10  14:24:49  kuipe_j
c Bug in C=f(Q) solved
c
c Revision 1.4  1997/06/17  11:23:43  kuipe_j
c Initialize vars
c
c Revision 1.3  1997/01/23  08:29:34  kuipe_j
c Make flow module robust
c
c Revision 1.2  1996/04/12  13:04:43  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:19  kuipe_j
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
      real    theta2 ,dh
      real    table(ntabm), asubsc(ngrid)
      real    af(ngrid)   ,o(ngrid)
      real    cs(ngrid,3), rs(ngrid,3), dcdq(ngrid)
      real    afh0(ngrid), afh1(ngrid)
      real    c(ngrid), r(ngrid)
      real    grsize(4,ngrid,*), engpar(9)
      real    secth0(ngrid), secth1(ngrid)
      real    wfh0(ngrid), wfh1(ngrid)
      real    bfricp(6,ngrid), prslot(3,nbran),psltvr(7,ngrid)

      double precision h1(ngrid), h(ngrid), q1(ngrid), q(ngrid)

c
c     Declaration of local variables:
c
      logical lslot
      integer ibr, i1, i2, i
      real    hi, ui, h0, hh1,
     +        af0, r0, c0,
     +        af1, r1, c1,
     +        af2, r2, c2,
     +        p1, c2r, d90,
     +        cacc, dq, qi
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
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
c            Sedredge branch
c
         else
c
c            Loop over cross sections (grid points)
c
             do 100 i = i1, i2
c
c               hi : waterlevel h=h(n+theta)
c
                hi  = theta2*h(i)+(1.-theta2)*h1(i)
                dq  = dh * af(i)
                qi  = theta2*q(i)+(1.-theta2)*q1(i) + dq
                ui  = qi / af(i)
                d90 = grsize(3,i,1)
c
                dcdq(i) = 0.
c
c               Test if "wet"
c
                if ( .not. lslot .or. hi .ge. psltvr(5,i) ) then
c
c
c                    Actual subsections: asubsc
c                    = 0 : only main section
c                    = 1 : 1 sub section  (main + sub section 1)
c                    = 2 : 2 sub sections (main + sub sections 1+2)
c
c                    * Situation *****************************
c                    * 1. main section                       *
c                    * 2. 1 or 2 sub sections, but h < h0    *
c                    *****************************************
c
                     if   ( int( asubsc(i) ) .eq. 0 ) then
c
c                       ****************
c                       * main section *
c                       ****************
c                       Doc: S-FO-001.5KV  Eq. 5-6 / 5-7 / 5-9
c
c                       r    : hydraulic radius
c                       c0   : Chezy coeffient for main section
c
                        r0  = rs(i,1)
c
c                       Compute Chezy coefficient c0 from hydraulic
c                       radius r0
c
                        if (    bfrict(1,ibr)     .eq. cfreng .or.
     +                      mod(bfrict(1,ibr),10) .eq. cfrchq )     then
                            call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                                  bfrict ,bfricp ,maxtab ,ntabm  ,
     +                                  ntab   ,table  ,d90    ,engpar ,
     +                                  0      ,hi     ,qi     ,ui     ,
     +                                  r0     ,c0     )
                        else
                            c0 = cs(i,1)
                        endif
c
                        c2r = c0**2*r0
c
                     else if ( int( asubsc(i) ) .eq. 1 ) then
c
c                       ** Situation ***********
c                       *  1 or 2 sub sections *
c                       *  h0 <= h < hh1        *
c                       ************************
c
c                       ****************
c                       * main section *
c                       ****************
c                       Doc: S-FO-001.5KV  Eq. 5-8 / 5-10
c
c                       Af0  = flow area in main section
c                       Afh0 = flow area in main section for h=h0
c                       r0   = hydraulic radius for main section
c
c                       secth0(i)  :  h0-value for grid point i
c
                        h0   = secth0(i)
c
                        Af0  = Afh0(i) + (hi-h0) * Wfh0(i)
                        r0   = rs(i,1)
c
c                       Compute Chezy coefficient c0 from hydraulic
c                       radius r0 according to the user selected Chezy
c                       formula.
c
                        if (     bfrict(1,ibr)     .ne. cfreng .and.
     +                      mod(bfrict(1,ibr),10) .ne. cfrchq )     then
                            call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                                  bfrict ,bfricp ,maxtab ,ntabm  ,
     +                                  ntab   ,table  ,d90    ,engpar ,
     +                                  0      ,hi     ,qi     ,ui     ,
     +                                  r0     ,c0     )
                        else
                            c0 = cs(i,1)
                        endif
c
c                       *****************
c                       * sub section 1 *
c                       *****************
c                       Doc: S-FO-001.5KV  Eq. 5-8
c
c                       Actual parameters:
c
c                       Af(i)= actual flow area total cross section
c
c                       Af1  = flow area in sub section 1
c                       r1   = hydraulic radius for sub section 1
c                       c1   = Chezy coefficient for sub section 1
c
                        Af1  = Af(i) - Af0
                        r1   = rs(i,2)
c
c                       Compute Chezy coefficient c1 from hydraulic
c                       radius r1
c
                        if (    bfrict(2,ibr)     .eq. cfreng .or.
     +                      mod(bfrict(2,ibr),10) .eq. cfrchq )     then
                            call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                                  bfrict ,bfricp ,maxtab ,ntabm  ,
     +                                  ntab   ,table  ,d90    ,engpar ,
     +                                  1      ,hi     ,qi     ,ui     ,
     +                                  r1     ,c1     )
                        else
                            c1 = cs(i,2)
                        endif
c
c                       **********************************************
c                       * Computation for total cross section        *
c                       * 1. Chezy coefficient C'                    *
c                       **********************************************
c
                        p1  = ( c0*Af0*sqrt(r0) +
     +                          c1*Af1*sqrt(r1) ) / Af(i)
                        c2r = p1*p1
c
                     else if ( int( asubsc(i) ) .eq. 2 ) then
c
c                       ** Situation ***********
c                       *  2 sub sections      *
c                       *  h > hh1              *
c                       ************************
c
c                       ****************
c                       * main section *
c                       ****************
c                       Doc: S-FO-001.5KV  Eq. 5-11
c
c                       Afh0 = flow area in main section for h=h0
c
c                       Af0  = flow area in main section
c                       r0   = hydraulic radius for main section
c                       secth0(i)  :  h0-value for grid point i
c
                        h0   = secth0(i)
                        Af0  = Afh0(i) + (hi-h0) * Wfh0(i)
                        r0   = rs(i,1)
c
c                       Compute Chezy coefficient c0 from hydraulic
c                       radius r0
c
                        if (    bfrict(1,ibr)     .eq. cfreng .or.
     +                      mod(bfrict(1,ibr),10) .eq. cfrchq )     then
                            call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                                  bfrict ,bfricp ,maxtab ,ntabm  ,
     +                                  ntab   ,table  ,d90    ,engpar ,
     +                                  0      ,hi     ,qi     ,ui     ,
     +                                  r0     ,c0     )
                        else
                            c0 = cs(i,1)
                        endif
c
c                       *****************
c                       * sub section 1 *
c                       *****************
c                       Doc: S-FO-001.5KV  Eq. 5-11
c
c                       Actual parameters:
c                       Af1  = flow area in sub section 1
c                       r1   = hydraulic radius for sub section 1
c                       c1   = Chezy coefficient for sub section 1
c                       secth1(i)  : hh1-value for grid point i
c
                        hh1 = secth1(i)
c
                        Af1  = Afh1(i) + (hi-hh1) * Wfh1(i) - Af0
                        r1   = rs(i,2)
c
c                       Compute Chezy coefficient c1 from hydraulic
c                       radius r1
c
                        if (    bfrict(2,ibr)     .eq. cfreng .or.
     +                      mod(bfrict(2,ibr),10) .eq. cfrchq )     then
                            call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                                  bfrict ,bfricp ,maxtab ,ntabm  ,
     +                                  ntab   ,table  ,d90    ,engpar ,
     +                                  1      ,hi     ,qi     ,ui     ,
     +                                  r1     ,c1     )
                        else
                           c1 = cs(i,2)
                        endif
c
c                       *****************
c                       * sub section 2 *
c                       *****************
c                       Doc: S-FO-001.5KV  Eq. 5-11
c
c                       Actual parameters:
c
c                       Af(i)= actual flow area total cross section
c
c                       Af2  = flow area in sub section 2
c                       r2   = hydraulic radius for sub section 2
c                       c2   = Chezy coefficient for sub section 2

                        Af2  = Af(i) - Af1 - Af0
                        r2   = rs(i,3)
c
c                       Compute Chezy coefficient c2 from hydraulic radius
c                       r2
c
                        if (     bfrict(3,ibr)     .eq. cfreng .or.
     +                      mod(bfrict(3,ibr),10) .eq. cfrchq )     then
                            call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                                  bfrict ,bfricp ,maxtab ,ntabm  ,
     +                                  ntab   ,table  ,d90    ,engpar ,
     +                                  2      ,hi     ,qi     ,ui     ,
     +                                  r2     ,c2     )
                        else
                            c2 = cs(i,3)
                        endif

c
c                       **********************************************
c                       * Computation for total cross section        *
c                       * 1. Chezy coefficient C                     *
c                       **********************************************
c
                        p1     = ( c0*Af0*sqrt(r0) +
     +                             c1*Af1*sqrt(r1) +
     +                             c2*Af2*sqrt(r2) ) / Af(i)
                        c2r    = p1*p1
                     endif

                     if (dh .gt. 0.) then
c
c                       Compute C' for total cross section
c
                        cacc    = sqrt ( c2r / r(i) )
                        dcdq(i) = (cacc - c(i)) / dq
                     else
c
c                       Compute C and R for total cross section
c
                        r(i) = af(i)/o(i)
                        c(i) = sqrt ( c2r / r(i) )
                    endif
                endif
  100        continue
         endif
  200 continue
c
      end
