subroutine KADCDQ(nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,&
&h1     ,h      ,q1     ,q      ,&
&maxtab ,ntabm  ,ntab   ,table  ,theta2 ,dh     ,&
&secth0 ,secth1 ,wfh0   ,wfh1   ,grsize ,engpar ,&
&af     ,o      ,afh0   ,afh1   ,&
&prslot ,asubsc ,c      ,r      ,cs     ,&
&rs     ,dcdq   ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KADCDQ (KAlman Derivative of Ch‚zy to Discharge Q)
!
! Module description: In this routine derivatives of the Che‚zy coe-
!                     ficients to the discharge will be computed.
!
!                     The derivatives are determined by numerical diffe-
!                     rentiation. In the flow module (FLBOCH) the Chezy
!                     coefficients have been calculated for the current
!                     discharge. In KADCDQ the calculation is done again
!                     for an incremented discharge.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 25 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 27 afh0(ngrid)       I  Flow area Af at water level h=h0 for every
!                         grid point.
! 28 afh1(ngrid)       I  Flow area Af at water level h=h1 for every
!                         grid point.
! 30 asubsc(ngrid)     I  Defines the actual number of sub sections for
!                         avery cross section (depending on the actual
!                         water level):
!                         c1sec (0) : Main section only (0 sub sections)
!                         c2sec (1) : 1 sub section
!                         c3sec (2) : 2 sub sections
!  6 bfricp            P  -
!  5 bfrict(3,nbran)   I  Bed friction in sections of branch.
!                         For each branch and section a different fric-
!                         tion type can be defined.
!                         (1,i) = Friction type in main section in
!                                 branch i:
!                                 cfrchc (1) : Chezy constant
!                                 cfrchq (2) : Chezy function of Q
!                                 cfrchh (3) : Chezy function of h
!                                 cfrman (4) : Manning constant
!                                 cfrskn (5) : Strickler 1 constant Kn
!                                 cfrsks (6) : Strickler 2 constant Ks
!                                 cfrnik (7) : Nikuradze constant
!                                 cfreng (8) : Engelund predictor
!                         (2,i) = Friction type in sub section 1 in
!                                 branch i:
!                                 cfrchc (1) : Chezy constant
!                                 cfrman (4) : Manning constant
!                                 cfrskn (5) : Strickler 1 constant Kn
!                                 cfrsks (6) : Strickler 2 constant Ks
!                                 cfrnik (7) : Nikuradze constant
!                         (3,i) = Friction type in sub section 2 in
!                                 branch i:
!                                 Same definitions as bfrict(2,i)
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 31 c(ngrid)          IO Actual Chezy coefficient for total channel in
!                         every grid point.
! 33 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
!                         sub 1, sub 2) for every grid point.
! 35 dcdq(ngrid)       O  Derivative of Chezy value to discharge in every
!                         grid point i on time n+1/2 (dC/dQ).
! 18 dh                I  -
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
!  8 h(ngrid)          I  Contains water levels in every grid point. It is
!                         stored on index 2 of the packed array hpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
! 13 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
! 15 ntab              P  -
! 14 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 26 o(ngrid)          I  Wetted perimeter for total cross section.
! 29 prslot(3,nbran)   I  Contains information concerning Preissmann
!                         slot (assuring positive water depths).
!                         (1,i) = Create slot indicator
!                                 csldis (0) : No slot in this branch.
!                                 cslena (1) : Create slot in branch.
!                         (2,i) = Reference area for slot generation.
!                         (3,i) = Depth of slot below lowest bottom of
!                                 branch.
! 36 psltvr            I  Preissmann slot variables for every grid point
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
! 10 q(ngrid)          I  Contains discharges in every grid point. It is
!                         stored on index 2 of the packed array qpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
! 32 r(ngrid)          IO Actual hydraulic radius for total channel in
!                         every grid point.
! 34 rs(ngrid,3)       I  Actual hydraulic radius in a section (main,
!                         sub 1, sub 2) for every grid point.
! 19 secth0(ngrid)     I  H0-value (for 1 or 2 sub sections) for every
!                         grid point.
! 20 secth1(ngrid)     I  H0-value (for 2 sub section) for every grid
!                         point.
! 16 table             P  -
! 17 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
!  4 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 21 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
!                         grid point.
! 22 wfh1(ngrid)       I  Flow width Wf at water level h=h1 for every
!                         grid point.
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
! $Log: kadcdq.pf,v $
! Revision 1.6  1999/03/15  13:52:50  kuipe_j
! table for bed friction general
!
! Revision 1.5  1997/07/10  14:24:49  kuipe_j
! Bug in C=f(Q) solved
!
! Revision 1.4  1997/06/17  11:23:43  kuipe_j
! Initialize vars
!
! Revision 1.3  1997/01/23  08:29:34  kuipe_j
! Make flow module robust
!
! Revision 1.2  1996/04/12  13:04:43  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:19  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
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

!
!     Declaration of local variables:
!
   logical lslot
   integer ibr, i1, i2, i
   real    hi, ui, h0, hh1,&
   &af0, r0, c0,&
   &af1, r1, c1,&
   &af2, r2, c2,&
   &p1, c2r, d90,&
   &cacc, dq, qi
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
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
!            Sedredge branch
!
      else
!
!            Loop over cross sections (grid points)
!
         do 100 i = i1, i2
!
!               hi : waterlevel h=h(n+theta)
!
            hi  = theta2*h(i)+(1.-theta2)*h1(i)
            dq  = dh * af(i)
            qi  = theta2*q(i)+(1.-theta2)*q1(i) + dq
            ui  = qi / af(i)
            d90 = grsize(3,i,1)
!
            dcdq(i) = 0.
!
!               Test if "wet"
!
            if ( .not. lslot .or. hi .ge. psltvr(5,i) ) then
!
!
!                    Actual subsections: asubsc
!                    = 0 : only main section
!                    = 1 : 1 sub section  (main + sub section 1)
!                    = 2 : 2 sub sections (main + sub sections 1+2)
!
!                    * Situation *****************************
!                    * 1. main section                       *
!                    * 2. 1 or 2 sub sections, but h < h0    *
!                    *****************************************
!
               if   ( int( asubsc(i) ) .eq. 0 ) then
!
!                       ****************
!                       * main section *
!                       ****************
!                       Doc: S-FO-001.5KV  Eq. 5-6 / 5-7 / 5-9
!
!                       r    : hydraulic radius
!                       c0   : Chezy coeffient for main section
!
                  r0  = rs(i,1)
!
!                       Compute Chezy coefficient c0 from hydraulic
!                       radius r0
!
                  if (    bfrict(1,ibr)     .eq. cfreng .or.&
                  &mod(bfrict(1,ibr),10) .eq. cfrchq )     then
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                     &bfrict ,bfricp ,maxtab ,ntabm  ,&
                     &ntab   ,table  ,d90    ,engpar ,&
                     &0      ,hi     ,qi     ,ui     ,&
                     &r0     ,c0     )
                  else
                     c0 = cs(i,1)
                  endif
!
                  c2r = c0**2*r0
!
               else if ( int( asubsc(i) ) .eq. 1 ) then
!
!                       ** Situation ***********
!                       *  1 or 2 sub sections *
!                       *  h0 <= h < hh1        *
!                       ************************
!
!                       ****************
!                       * main section *
!                       ****************
!                       Doc: S-FO-001.5KV  Eq. 5-8 / 5-10
!
!                       Af0  = flow area in main section
!                       Afh0 = flow area in main section for h=h0
!                       r0   = hydraulic radius for main section
!
!                       secth0(i)  :  h0-value for grid point i
!
                  h0   = secth0(i)
!
                  Af0  = Afh0(i) + (hi-h0) * Wfh0(i)
                  r0   = rs(i,1)
!
!                       Compute Chezy coefficient c0 from hydraulic
!                       radius r0 according to the user selected Chezy
!                       formula.
!
                  if (     bfrict(1,ibr)     .ne. cfreng .and.&
                  &mod(bfrict(1,ibr),10) .ne. cfrchq )     then
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                     &bfrict ,bfricp ,maxtab ,ntabm  ,&
                     &ntab   ,table  ,d90    ,engpar ,&
                     &0      ,hi     ,qi     ,ui     ,&
                     &r0     ,c0     )
                  else
                     c0 = cs(i,1)
                  endif
!
!                       *****************
!                       * sub section 1 *
!                       *****************
!                       Doc: S-FO-001.5KV  Eq. 5-8
!
!                       Actual parameters:
!
!                       Af(i)= actual flow area total cross section
!
!                       Af1  = flow area in sub section 1
!                       r1   = hydraulic radius for sub section 1
!                       c1   = Chezy coefficient for sub section 1
!
                  Af1  = Af(i) - Af0
                  r1   = rs(i,2)
!
!                       Compute Chezy coefficient c1 from hydraulic
!                       radius r1
!
                  if (    bfrict(2,ibr)     .eq. cfreng .or.&
                  &mod(bfrict(2,ibr),10) .eq. cfrchq )     then
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                     &bfrict ,bfricp ,maxtab ,ntabm  ,&
                     &ntab   ,table  ,d90    ,engpar ,&
                     &1      ,hi     ,qi     ,ui     ,&
                     &r1     ,c1     )
                  else
                     c1 = cs(i,2)
                  endif
!
!                       **********************************************
!                       * Computation for total cross section        *
!                       * 1. Chezy coefficient C'                    *
!                       **********************************************
!
                  p1  = ( c0*Af0*sqrt(r0) +&
                  &c1*Af1*sqrt(r1) ) / Af(i)
                  c2r = p1*p1
!
               else if ( int( asubsc(i) ) .eq. 2 ) then
!
!                       ** Situation ***********
!                       *  2 sub sections      *
!                       *  h > hh1              *
!                       ************************
!
!                       ****************
!                       * main section *
!                       ****************
!                       Doc: S-FO-001.5KV  Eq. 5-11
!
!                       Afh0 = flow area in main section for h=h0
!
!                       Af0  = flow area in main section
!                       r0   = hydraulic radius for main section
!                       secth0(i)  :  h0-value for grid point i
!
                  h0   = secth0(i)
                  Af0  = Afh0(i) + (hi-h0) * Wfh0(i)
                  r0   = rs(i,1)
!
!                       Compute Chezy coefficient c0 from hydraulic
!                       radius r0
!
                  if (    bfrict(1,ibr)     .eq. cfreng .or.&
                  &mod(bfrict(1,ibr),10) .eq. cfrchq )     then
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                     &bfrict ,bfricp ,maxtab ,ntabm  ,&
                     &ntab   ,table  ,d90    ,engpar ,&
                     &0      ,hi     ,qi     ,ui     ,&
                     &r0     ,c0     )
                  else
                     c0 = cs(i,1)
                  endif
!
!                       *****************
!                       * sub section 1 *
!                       *****************
!                       Doc: S-FO-001.5KV  Eq. 5-11
!
!                       Actual parameters:
!                       Af1  = flow area in sub section 1
!                       r1   = hydraulic radius for sub section 1
!                       c1   = Chezy coefficient for sub section 1
!                       secth1(i)  : hh1-value for grid point i
!
                  hh1 = secth1(i)
!
                  Af1  = Afh1(i) + (hi-hh1) * Wfh1(i) - Af0
                  r1   = rs(i,2)
!
!                       Compute Chezy coefficient c1 from hydraulic
!                       radius r1
!
                  if (    bfrict(2,ibr)     .eq. cfreng .or.&
                  &mod(bfrict(2,ibr),10) .eq. cfrchq )     then
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                     &bfrict ,bfricp ,maxtab ,ntabm  ,&
                     &ntab   ,table  ,d90    ,engpar ,&
                     &1      ,hi     ,qi     ,ui     ,&
                     &r1     ,c1     )
                  else
                     c1 = cs(i,2)
                  endif
!
!                       *****************
!                       * sub section 2 *
!                       *****************
!                       Doc: S-FO-001.5KV  Eq. 5-11
!
!                       Actual parameters:
!
!                       Af(i)= actual flow area total cross section
!
!                       Af2  = flow area in sub section 2
!                       r2   = hydraulic radius for sub section 2
!                       c2   = Chezy coefficient for sub section 2

                  Af2  = Af(i) - Af1 - Af0
                  r2   = rs(i,3)
!
!                       Compute Chezy coefficient c2 from hydraulic radius
!                       r2
!
                  if (     bfrict(3,ibr)     .eq. cfreng .or.&
                  &mod(bfrict(3,ibr),10) .eq. cfrchq )     then
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                     &bfrict ,bfricp ,maxtab ,ntabm  ,&
                     &ntab   ,table  ,d90    ,engpar ,&
                     &2      ,hi     ,qi     ,ui     ,&
                     &r2     ,c2     )
                  else
                     c2 = cs(i,3)
                  endif

!
!                       **********************************************
!                       * Computation for total cross section        *
!                       * 1. Chezy coefficient C                     *
!                       **********************************************
!
                  p1     = ( c0*Af0*sqrt(r0) +&
                  &c1*Af1*sqrt(r1) +&
                  &c2*Af2*sqrt(r2) ) / Af(i)
                  c2r    = p1*p1
               endif

               if (dh .gt. 0.) then
!
!                       Compute C' for total cross section
!
                  cacc    = sqrt ( c2r / r(i) )
                  dcdq(i) = (cacc - c(i)) / dq
               else
!
!                       Compute C and R for total cross section
!
                  r(i) = af(i)/o(i)
                  c(i) = sqrt ( c2r / r(i) )
               endif
            endif
100      continue
      endif
200 continue
!
end
