subroutine KABOCH(nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,&
&h1     ,h      ,q1     ,q      ,theta2 ,dh     ,&
&maxtab ,ntabm  ,ntab   ,table  ,&
&subsec ,secth0 ,secth1 ,wfh0   ,wfh1   ,grsize ,&
&engpar ,afacc  ,oacc   ,afh0   ,afh1   ,oh0    ,&
&oh1    ,prslot ,asubsc ,c      ,r      ,cs     ,&
&alfab  ,dcdh   ,drdh   ,dalfdh ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KABOCH (KAlman Boussinesq and Ch‚zy coefficient)
!
! Module description: In subroutine KABOCH derivatives to water levels of the
!                     Boussinesq's constant, the Ch‚zy coefficients and the
!                     hydraulic radius will be computed.
!
!                     The derivatives of the coefficients are determined
!                     by numerical differentiation. In the flow module
!                     (FLBOCH) the coefficients have been calculated for
!                     the current water level. In KABOCH the calculation
!                     is done again for an incremented water level.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 26 afacc(ngrid)      I  Flow area in every grid point i on time
!                         n+1/2+dh.
! 28 afh0(ngrid)       I  Flow area Af at water level h=h0 for every
!                         grid point.
! 29 afh1(ngrid)       I  Flow area Af at water level h=h1 for every
!                         grid point.
! 37 alfab(ngrid)      IO Actual Bousinessq coefficient in grid point i.
! 33 asubsc(ngrid)     IO Defines the actual number of sub sections for
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
! 34 c(ngrid)          IO Actual Chezy coefficient for total channel in
!                         every grid point.
! 36 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
!                         sub 1, sub 2) for every grid point.
! 40 dalfdh(ngrid)     O  Derivative of Bousinesq constant ALFAb to
!                         waterlevel in every grid point i on time n+1/2
!                         (d(ALFAb)/dh).
! 38 dcdh(ngrid)       O  Derivative of Chezy value to waterlevel in every
!                         grid point i on time n+1/2 (dC/dh).
! 12 dh                I  -
! 39 drdh(ngrid)       O  Derivative of hydraulic radius to waterlevel in
!                         every grid point i on time n+1/2 (dR/dh).
! 25 engpar            P  -
! 24 grsize(4,ngrid,*) I  Grain sizes for each gridpoint and section.
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
! 15 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
! 17 ntab              P  -
! 16 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 27 oacc(ngrid)      I  wetted perimeter in every grid point i on time
!                         n+1/2+dh.
! 30 oh0(ngrid)        I  Wetted perimeter Ot at water level h=h0 for
!                         every grid point.
! 31 oh1(ngrid)        I  Wetted perimeter Ot at water level h=h1 for
!                         every grid point.
! 32 prslot(3,nbran)   I  Contains information concerning Preissmann
!                         slot (assuring positive water depths).
!                         (1,i) = Create slot indicator
!                                 csldis (0) : No slot in this branch.
!                                 cslena (1) : Create slot in branch.
!                         (2,i) = Reference area for slot generation.
!                         (3,i) = Depth of slot below lowest bottom of
!                                 branch.
! 41 psltvr(7,ngrid)   I  Preissmann slot variables for every grid point
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
! 35 r(ngrid)          IO Actual hydraulic radius for total channel in
!                         every grid point.
! 20 secth0(ngrid)     I  H0-value (for 1 or 2 sub sections) for every
!                         grid point.
! 21 secth1(ngrid)     I  H0-value (for 2 sub section) for every grid
!                         point.
! 19 subsec(ngrid)     I  Defines the number of sub sections for every
!                         cross section:
!                         c1sec (0) : Main section only (0 sub sections)
!                         c2sec (1) : 1 sub section
!                         c3sec (2) : 2 sub sections
!                         (For a circle cross section   : 0 ;
!                          For a sedredge cross section : 1 )
! 18 table             P  -
! 11 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
!  4 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 22 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
!                         grid point.
! 23 wfh1(ngrid)       I  Flow width Wf at water level h=h1 for every
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
! $Log: kaboch.pf,v $
! Revision 1.8  1999/03/15  13:52:48  kuipe_j
! table for bed friction general
!
! Revision 1.7  1997/08/21  11:11:04  kuipe_j
! Check for negative areas and wetted per.
!
! Revision 1.6  1997/06/17  11:23:42  kuipe_j
! Initialize vars
!
! Revision 1.5  1997/01/23  08:29:33  kuipe_j
! Make flow module robust
!
! Revision 1.4  1996/04/12  15:18:20  kuipe_j
! *** empty log message ***
!
! Revision 1.3  1996/04/12  14:57:35  kuipe_j
! Comment leader
!
! Revision 1.2  1996/04/12  13:04:40  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:16  kuipe_j
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

!
!     Declaration of local variables:
!
   logical lslot
   integer ibr, i1, i2, i  ,nsubs
   real    hi, qi, ui, h0, hh1,&
   &af0, o0, r0, c0,&
   &af1, o1, r1, c1,&
   &af2, o2, r2, c2,&
   &p1, p2, c2r, d90,&
   &alfacc, racc, cacc, asub
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   do 200 ibr = 1, nbran
!
!       i1 = global grid point number at node n1
!       i2 = global grid point number at node n2
!
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!
      lslot = int(prslot(1,ibr)) .eq. cslena
!
      if ( typcr(ibr) .eq. ccrsed ) then
!
!          Sedredge branch
!
      else
!
!          Loop over cross sections (grid points)
!
         do 100 i = i1, i2
!
!             hi : waterlevel h=h(n+theta)+dh
!
            hi  = theta2*h(i)+(1.-theta2)*h1(i) + dh
            qi  = theta2*q(i)+(1.-theta2)*q1(i)
            ui  = qi / afacc(i)
            d90 = grsize(3,i,1)
!
            if ( lslot .and. hi .lt. psltvr(5,i) ) then
               dcdh(i)   = 0.
               drdh(i)   = 0.
               dalfdh(i) = 0.
            else
!
!                Define actual number of subsections
!
               call FLNSEC(hi   ,i    ,asub ,subsec ,secth0 ,secth1,&
               &wfh0 ,wfh1 ,afacc  ,afh0   ,afh1   ,ngrid )
               if (dh .lt. 1.e-10) then
                  nsubs = int(asub)
                  asubsc(i) = asub
               else
                  nsubs = int(asub)
               endif
               h0     = secth0(i)
               hh1    = secth1(i)

!
!                * Situation *****************************
!                * 1. main section                       *
!                * 2. 1 or 2 sub sections, but h < h0    *
!                *****************************************
!
               if   ( nsubs .eq. 0 ) then
!
!                   ****************
!                   * main section *
!                   ****************
!                   Doc: S-FO-001.5KV  Eq. 5-6 / 5-7 / 5-9
!
!                   r    : hydraulic radius
!                   c0   : Chezy coeffient for main section
!
                  r0  = Afacc(i) / oacc(i)
!
!                   Compute Chezy coefficient c0 from hydraulic
!                   radius r0
!
                  if (     bfrict(1,ibr)     .ne. cfrchc .and.&
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
                  alfacc = 1.0
!
!                   Compute R' and C' for total cross section
!
                  racc   = r0
                  cacc   = c0
!
               else if ( nsubs .eq. 1 ) then
!
!                   ** Situation ***********
!                   *  1 or 2 sub sections *
!                   *  h0 <= h < hh1        *
!                   ************************
!
!                   ****************
!                   * main section *
!                   ****************
!                   Doc: S-FO-001.5KV  Eq. 5-8 / 5-10
!
!                   Af0  = flow area in main section
!                   Afh0 = flow area in main section for h = h0
!                   oh0  = wetted perimeter in main section for h = h0
!                   r0   = hydraulic radius for main section
!
                  Af0  = Afh0(i) + (hi-h0) * Wfh0(i)
                  o0   = oh0(i)
                  r0   = Af0 / o0
!
!                   Compute Chezy coefficient c0 from hydraulic radius
!                   r0
!                   according to the user selected Chezy formula.
!
                  if (     bfrict(1,ibr)     .ne. cfrchc .and.&
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
!                   *****************
!                   * sub section 1 *
!                   *****************
!                   Doc: S-FO-001.5KV  Eq. 5-8
!
!                   Actual parameters:
!
!                   Af(i)= actual flow area total cross section
!                   o(i) = wetted perimeter for total cross section
!
!                   Af1  = flow area in sub section 1
!                   r1   = hydraulic radius for sub section 1
!                   c1   = Chezy coefficient for sub section 1
!
                  Af1  = Afacc(i) - Af0
                  o1   = oacc(i) - oh0(i)
!                   Provision against too small wetted perimeter
!                   and negative area
                  if (o1 .le. .001 .or. af1 .le. 1.e-6) then
                     r1 = (hi-h0) *.5
                  else
                     r1 = Af1 / o1
                  endif
!
!                   Compute Chezy coefficient c1 from hydraulic radius
!                   r1
!
                  if (     bfrict(2,ibr)     .ne. cfrchc .and.&
                  &mod(bfrict(2,ibr),10) .ne. cfrchq )     then
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                     &bfrict ,bfricp ,maxtab ,ntabm  ,&
                     &ntab   ,table  ,d90    ,engpar ,&
                     &1      ,hi     ,qi     ,ui     ,&
                     &r1     ,c1     )
                  else
                     c1 = cs(i,2)
                  endif
!
!                   **********************************************
!                   * Computation for total cross section        *
!                   * 1. Chezy coefficient C                     *
!                   * 2. Hydraulic radius R                      *
!                   * 3. Boussinesq constant alfab               *
!                   **********************************************
!
                  p1     = ( c0*Af0*sqrt(r0) +&
                  &c1*Af1*sqrt(r1) ) / Afacc(i)
                  c2r    = p1*p1

                  p1     = c0*c0 * Af0 * r0 + c1*c1 * Af1 * r1
                  p2     = c2r   * Afacc(i)
!
                  alfacc = p1/p2
!
!                   Compute R' and C' for total cross section
!
                  racc   = Afacc(i) / oacc(i)
                  cacc   = sqrt ( c2r / racc )

               else if ( nsubs .eq. 2 ) then
!
!                   ** Situation ***********
!                   *  2 sub sections      *
!                   *  h > hh1              *
!                   ************************
!
!                   ****************
!                   * main section *
!                   ****************
!                   Doc: S-FO-001.5KV  Eq. 5-11
!
!                   Afh0 = flow area in main section for h = h0
!                   oh0  = wetted perimeter in main section for h = h0
!
!                   Af0  = flow area in main section
!                   r0   = hydraulic radius for main section
!
                  Af0  = Afh0(i) + (hi-h0) * Wfh0(i)
                  o0   = oh0(i)
                  r0   = Af0 / o0
!
!                   Compute Chezy coefficient c0 from hydraulic radius
!                   r0
!
                  if (     bfrict(1,ibr)     .ne. cfrchc .and.&
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
!                   *****************
!                   * sub section 1 *
!                   *****************
!                   Doc: S-FO-001.5KV  Eq. 5-11
!
!                   Actual parameters:
!                   Af1  = flow area in sub section 1
!                   r1   = hydraulic radius for sub section 1
!                   c1   = Chezy coefficient for sub section 1

                  Af1  = Afh1(i) + (hi-hh1) * Wfh1(i) - Af0
                  o1   = oh1(i)
                  r1   = Af1 / o1
!
!                   Compute Chezy coefficient c1 from hydraulic radius
!                   r1
!
                  if (     bfrict(2,ibr)     .ne. cfrchc .and.&
                  &mod(bfrict(2,ibr),10) .ne. cfrchq )     then
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                     &bfrict ,bfricp ,maxtab ,ntabm  ,&
                     &ntab   ,table  ,d90    ,engpar ,&
                     &1      ,hi     ,qi     ,ui     ,&
                     &r1     ,c1     )
                  else
                     c1 = cs(i,2)
                  endif
!
!                   *****************
!                   * sub section 2 *
!                   *****************
!                   Doc: S-FO-001.5KV  Eq. 5-11
!
!                   Actual parameters:
!
!                   Af(i)= actual flow area total cross section
!                   o(i) = wetted perimeter for total cross section
!
!                   Af2  = flow area in sub section 2
!                   r2   = hydraulic radius for sub section 2
!                   c2   = Chezy coefficient for sub section 2

                  Af2  = Afacc(i) - Af1 - Af0
                  o2   = oacc(i) - oh1(i) - oh0(i)
!                   Provision against too small wetted perimeter
!                   and negative area
                  if (o2 .le. .001 .or. af2 .le. 1.e-6) then
                     r2 = (hi-hh1) *.5
                  else
                     r2 = Af2 / o2
                  endif
!
!                   Compute Chezy coefficient c2 from hydraulic radius
!                   r2
!
                  if (     bfrict(3,ibr)     .ne. cfrchc .and.&
                  &mod(bfrict(3,ibr),10) .ne. cfrchq )     then
                     call FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
                     &bfrict ,bfricp ,maxtab ,ntabm  ,&
                     &ntab   ,table  ,d90    ,engpar ,&
                     &2      ,hi     ,qi     ,ui     ,&
                     &r2     ,c2     )
                  else
                     c2 = cs(i,3)
                  endif
!
!                   **********************************************
!                   * Computation for total cross section        *
!                   * 1. Chezy coefficient C                     *
!                   * 2. Hydraulic radius R                      *
!                   * 3. Boussinesq constant alfab               *
!                   **********************************************
!
                  p1     = ( c0*Af0*sqrt(r0) +&
                  &c1*Af1*sqrt(r1) +&
                  &c2*Af2*sqrt(r2) ) / Afacc(i)
                  c2r    = p1*p1
!
                  p1     = c0*c0 * Af0 * r0 + c1*c1 * Af1 * r1 +&
                  &c2*c2 * Af2 * r2
                  p2     = c2r * Afacc(i)
                  alfacc = p1/p2
!
!                   Compute R' and C' for total cross section
!
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
100      continue
      endif
200 continue
!
end
