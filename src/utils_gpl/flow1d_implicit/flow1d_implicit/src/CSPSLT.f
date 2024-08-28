       subroutine cspslt ( ibr    ,ngrid  ,nbran  ,branch ,
     +                     prslot ,psltvr ,
     +                     bfrict ,bfricp ,engpar ,grsize ,
     +                     maxtab ,ntabm  ,ntab   ,table  ,
     +                     maxlev ,nlev   ,hlev   ,
     +                     wft    ,aft    ,wtt    ,att    ,
     +                     of     ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Cross Sectional Table Module
c
c Programmer:         J.Brouwer
c
c Module:             CSPSLT (Cross Section Preissmann SLoT)
c
c Module description: Generate Preissmann slot for branches where user
c                     selected this option.
c
c                     This routine creates a Preissmann slot for each
c                     branch where the user selected this option. In
c                     case a Preissmann slot is wanted the following
c                     Chezy formulations may not be used:
c
c                     o     C = f(Q)
c                     o     C = roughness predictor
c
c                     The shape of the slot (funnel) is :
c
c
c                          \       /
c                            \   /
c                             | |
c                             | |
c                             | |
c                             | |
c                             | |
c                             ---
c
c
c                     The slot (funnel) is split up into two parts
c                     with each 50 % of the area of the slot. The bottom
c                     part has a rectangular (thin) shape. The upper part
c                     is a trapezium that has the same bottom width as the
c                     width of the rectangle. The top of the trapezium has
c                     width of one fifth of the width of the defined
c                     crossection just above the top of the slot.
c
c                     The adapted profile will be used in the flow mod-
c                     ule. If the water level is below top slot the value
c                     for C2R will be kept constant to the value of C2R
c                     on level z(1*). This value will be calculated
c                     using the original cross section profile. The user
c                     selects for each branch if a preissmann slot must
c                     be generated. If this is the case the following
c                     data must be defined once per branch:
c
c                     o     Bslot (mean width of funnel)
c                     o     depth of slot below lowest bottom in branch
c
c                     top slot will be determined in such a way that Af(1*)
c                     equals A*. The value of A* is determined by:
c
c                           A* = Depth slot * Bslot
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 20 aft(ngrid,maxlev) IO (i,j) = flow area at h = hlev(i,j) for cross
c                                 section i.
c 22 att(ngrid,maxlev) IO (i,j) = total area at h = hlev(i,j) for cross
c                                 section i.
c  9 bfricp(6,ngrid)   I  Bed friction parameters:
c                         (1,i) = Parameter for positive flow direction
c                                 in main section (depending on friction
c                                 type):
c                                 =     Chezy constant value
c                                 =     Table pointer (Q or h table)
c                                 =     Nikuradse parameter kn for Ni-
c                                       kuradse formula
c                                 =     Manning parameter nm for Manning
c                                       formula
c                                 =     Strickler coefficient ks for
c                                       Strickler formula
c                         (2,i) = Parameter for negative flow direction
c                                 in main section (depending on friction
c                                 type) Same definitions as bfricp(1,i).
c                         (3,i) = Parameter for positive flow direction
c                                 in sub sec 1 (depending on friction
c                                 type):
c                                 =     Chezy constant value
c                                 =     Nikuradse parameter kn for Niku-
c                                       radse formula
c                                 =     Manning parameter nm for Manning
c                                       formula
c                                 =     Strickler coefficient ks for
c                                       Strickler formula
c                         (4,i) = Parameter for negative flow direction
c                                 in sub sec 1 (depending on friction
c                                 type) Same definition as bfricp (3,i):
c                         (5,i) = Parameter for positive flow direction
c                                 in sub sec 2 (depending on friction
c                                 type) Same definition as bfricp (3,i).
c                         (6,i) = Parameter for negative flow direction
c                                 in sub sec 2 (depending on friction
c                                 type) Same definition as bfricp (3,i).
c  8 bfrict(3,nbran)   I  Bed friction in sections of branch.
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
c  4 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 10 engpar            P  -
c 11 grsize(4,ngrid,*) I  Grain sizes for each gridpoint and section.
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
c 18 hlev(ngrid,       IO (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  1 ibr               I  Branch number.
c 25 juer              P  -
c 26 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 16 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 12 maxtab            I  Maximum number of defined tables.
c  3 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c 17 nlev(ngrid)       IO Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c 14 ntab              P  -
c 13 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 23 of(ngrid)         IO Actual wetted perimeter at every cross secti-
c                         on.
c  5 prslot(3,nbran)   I  Contains information concerning Preissmann
c                         slot (assuring positive water depths).
c                         (1,i) = Create slot indicator
c                                 csldis (0) : No slot in this branch.
c                                 cslena (1) : Create slot in branch.
c                         (2,i) = Reference area for slot generation.
c                         (3,i) = Depth of slot below lowest bottom of
c                                 branch.
c  6 psltvr(7,ngrid)   O  Preissmann slot variables for every grid point
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
c 15 table             P  -
c 19 wft(ngrid,maxlev) IO (i,j) = flow width at h = hlev(i,j) for grid
c                                 point i.
c                         - For a circle cross section:
c                         (i,1) = Radius of the circle.
c                         - For a sedredge cross section:
c                         (i,1) = Width of main section (i.e. left chan-
c                                 nel).
c                         (i,2) = Width of sub section 1 (i.e. right
c                                 channel).
c 21 wtt(ngrid,maxlev) IO (i,j) = total width at h = hlev(i,j) for grid
c                                 point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flchzt  FLow compute CHeZy Time dependent
c flinaw  FLow INterpolate Area and Width
c flperi  FLow PERImeter
c indwgh  Compute INDex and WeiGHt factor
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: cspslt.pf,v $
c Revision 1.11  1999/06/01  13:42:07  kuipe_j
c names in messages substituted + message template
c
c Revision 1.10  1999/03/15  15:49:07  kuipe_j
c tabs removed
c
c Revision 1.9  1998/02/25  12:48:47  kuipe_j
c Check on grain size added
c
c Revision 1.8  1997/09/30  09:23:00  kuipe_j
c density term improved for Preisman slot
c
c Revision 1.7  1997/03/04  16:11:41  kuipe_j
c Temorarily set of slot width removed
c
c Revision 1.6  1997/01/23  08:28:44  kuipe_j
c Make flow module robust
c
c Revision 1.5  1996/05/30  09:59:52  kuipe_j
c comment char
c
c Revision 1.4  1995/10/11  10:20:06  hoeks_a
c Preissman slot improved
c
c Revision 1.3  1995/05/30  09:54:24  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:55:30  hoeks_a
c files changed from dos-file to unix-files
c
c Revision 1.1  1995/04/13  06:58:32  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:29:51  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:40  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer   ibr ,ngrid ,nbran ,maxlev, maxtab, ntabm, juer, ker
c
      integer   branch (4,nbran),
     +          bfrict (3,nbran),
     +          nlev   (ngrid),
     +          ntab   (4,maxtab)
c
      real      prslot (3,nbran),
     +          psltvr (7,ngrid),
     +          bfricp (6,ngrid),
     +          engpar (9),
     +          grsize (4,ngrid,*),
     +          wft    (ngrid,maxlev),
     +          aft    (ngrid,maxlev),
     +          wtt    (ngrid,maxlev),
     +          att    (ngrid,maxlev),
     +          of     (ngrid,maxlev),
     +          table  (ntabm)
c
      double precision hlev(ngrid,maxlev)
c
c     Local variables
c
      integer  i1 ,i2 ,igp ,ilev ,isec, iilev, lbrnam
c
      logical  oke   ,lslot  ,epsequ
c
      real     aster ,z1 ,z2 ,w1 ,w2 ,term1 ,term2 ,
     +         u     ,z1ad  ,nikpos ,nikneg ,c ,
     +         d90   ,rster ,wt ,at, wf ,af ,o     ,
     +         alpha ,hster ,wster ,wst

      double precision wght, z0br, q

      character(len=40) branam
c
c     Include sobek error code file
c
      include '../include/errcod.i'
      include '../include/sobcon.i'
c
c     Set lslot to false to use original profile
c
      lslot = .false.
c
c     Read first and last gridpoint of branch
c
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
c
c     Determine lowest bottom of branch
c
      z0br = hlev(i1,1)
      do 100 igp = i1, i2
         z0br = min ( z0br, hlev(igp,1) )
 100  continue
c
c     Calculate depth of slot and determine w*
c
      z0br  = z0br - prslot(3,ibr)
      wst   = prslot(2,ibr)
c
c     Do for each cross section
c
      do 400 igp = i1, i2
c
         oke   = .true.
         iilev = 1
c
 200     continue
         aster = wst * (real(hlev(igp,iilev)) - z0br)
         if ( aster .ge. aft(igp,iilev+1) ) then
            iilev = iilev + 1
            if ( iilev .lt. nlev(igp) ) then
               goto 200
            else
               oke = .false.
            endif
         endif
c
         z1 = real( hlev(igp,iilev) )
         z2 = real( hlev(igp,iilev+1) - hlev(igp,iilev) )
         w1 = wft(igp,iilev)
         w2 = wft(igp,iilev+1)
         if (epsequ(w2-w1,0.0,1E-6)) then
c
c           w1 = w2
c
            z1ad = z1 + ( aster-aft(igp,iilev) ) / w1
         else
c
c           w2 > w1
c
            term1 = -w1+sqrt(w1*w1+((2/z2)*(w2-w1))*
     +              (aster-aft(igp,iilev)))
            term2 = ( w2 - w1 ) / z2
            z1ad  = z1 + ( term1 / term2 )
         endif
         wster = aster/(z1ad-z0br)
c
c        Determine At, Wt, Af and Wf on level z1ad
c
         call indwgh ( ngrid  ,igp    ,
     +                 maxlev ,nlev   ,hlev   ,
     +                 dble(z1ad)     ,ilev   ,wght   )
c
         call flinaw ( ngrid  ,igp    ,lslot  ,
     +                 maxlev ,nlev   ,hlev   ,
     +                 dble(z1ad)   ,ilev   ,wght   ,
     +                 wtt    ,att    ,
     +                 wt     ,at     ,psltvr )
c
         call flinaw ( ngrid  ,igp    ,lslot  ,
     +                 maxlev ,nlev   ,hlev   ,
     +                 dble(z1ad)   ,ilev   ,wght   ,
     +                 wft    ,aft    ,
     +                 wf     ,af     ,psltvr )
c
         call flperi ( ngrid  ,igp    ,lslot  ,
     +                 maxlev ,nlev   ,hlev   ,
     +                 dble(z1ad)   ,ilev   ,
     +                 wft    ,wf     ,
     +                 of     ,o      ,psltvr )
c
c        Determine R* on level z1ad
c
         rster = af / o
c
c        Check if C = nikuradze, if so check 12R > nikuradze
c
         if (bfrict(1,ibr) .eq. cfrnik) then
            nikpos = bfricp(1,igp)/12.*3.
            nikneg = bfricp(2,igp)/12.*3.
            if (rster .le. min(nikneg,nikpos)) then
               oke = .false.
               ker = fatal
               call getbrn (ibr,branam,lbrnam)
               call sre_error (juer,
     +         'CSPSLT Nikuradse-value is too high due to slot @' //
     +         branam(:lbrnam)// '@',
     +         ecsnik, ker)
            endif
         endif
c
c        Check oke
c
         if (.not. oke) then
            ker = fatal
            call getbrn (ibr,branam,lbrnam)
            call sre_error (juer,
     +                  'CSPSLT Unable to create slot @' //
     +                   branam(:lbrnam)// '@',
     +                   ecsslt, ker)
            goto 900
         endif
c
c        Calculate C for positive flow on level z(1*)
c
         u    = 0.
         q    = 1D0
c
c        Avoid zero grain size. Later on in FLINI a check will
c        carried out
c
         d90  = max(grsize ( 3, igp, 1 ),1.e-6)
         isec = 0
c
         call flchzt ( ibr    ,igp    ,nbran  ,ngrid  ,
     +                 bfrict ,bfricp ,
     +                 maxtab ,ntabm  ,ntab   ,table  ,d90    ,
     +                 engpar ,isec   ,dble( z1ad )   ,q      ,
     +                 u      ,rster  ,c      )
c
c        Calculate C2R positive direction
c
         psltvr(1,igp) = c * c * rster
c
c        Calculate C for negative flow on level z(1*)
c
         q = -1D0
c
         call flchzt ( ibr    ,igp    ,nbran  ,ngrid  ,
     +                 bfrict ,bfricp ,
     +                 maxtab ,ntabm  ,ntab   ,table  ,d90    ,
     +                 engpar ,isec   ,dble(z1ad)     ,q      ,
     +                 u      ,rster  ,c      )
c
c        Calculate C2R negative direction
c
         psltvr(2,igp) = c * c * rster
c
c        calculation of division-level H* of  Preismann-'trechter'
c
         alpha = max(.2 , 1.5*wster/wt)
         psltvr(3,igp) = z0br
         hster = ((alpha*wt/2.-0.75*wster)*z1ad+wster/2.*z0br)/
     +           (alpha*wt/2.-wster/4.)
         psltvr(4,igp) = hster
         psltvr(5,igp) = z1ad
         psltvr(6,igp) = wster/2.
         psltvr(7,igp) = alpha*wt
c
 400  continue

      return
c
 900  continue
c
      return
      end
