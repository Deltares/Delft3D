subroutine cspslt ( ibr    ,ngrid  ,nbran  ,branch ,&
&prslot ,psltvr ,&
&bfrict ,bfricp ,engpar ,grsize ,&
&maxtab ,ntabm  ,ntab   ,table  ,&
&maxlev ,nlev   ,hlev   ,&
&wft    ,aft    ,wtt    ,att    ,&
&of     ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Cross Sectional Table Module
!
! Programmer:         J.Brouwer
!
! Module:             CSPSLT (Cross Section Preissmann SLoT)
!
! Module description: Generate Preissmann slot for branches where user
!                     selected this option.
!
!                     This routine creates a Preissmann slot for each
!                     branch where the user selected this option. In
!                     case a Preissmann slot is wanted the following
!                     Chezy formulations may not be used:
!
!                     o     C = f(Q)
!                     o     C = roughness predictor
!
!                     The shape of the slot (funnel) is :
!
!
!                          \       /
!                            \   /
!                             | |
!                             | |
!                             | |
!                             | |
!                             | |
!                             ---
!
!
!                     The slot (funnel) is split up into two parts
!                     with each 50 % of the area of the slot. The bottom
!                     part has a rectangular (thin) shape. The upper part
!                     is a trapezium that has the same bottom width as the
!                     width of the rectangle. The top of the trapezium has
!                     width of one fifth of the width of the defined
!                     crossection just above the top of the slot.
!
!                     The adapted profile will be used in the flow mod-
!                     ule. If the water level is below top slot the value
!                     for C2R will be kept constant to the value of C2R
!                     on level z(1*). This value will be calculated
!                     using the original cross section profile. The user
!                     selects for each branch if a preissmann slot must
!                     be generated. If this is the case the following
!                     data must be defined once per branch:
!
!                     o     Bslot (mean width of funnel)
!                     o     depth of slot below lowest bottom in branch
!
!                     top slot will be determined in such a way that Af(1*)
!                     equals A*. The value of A* is determined by:
!
!                           A* = Depth slot * Bslot
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 20 aft(ngrid,maxlev) IO (i,j) = flow area at h = hlev(i,j) for cross
!                                 section i.
! 22 att(ngrid,maxlev) IO (i,j) = total area at h = hlev(i,j) for cross
!                                 section i.
!  9 bfricp(6,ngrid)   I  Bed friction parameters:
!                         (1,i) = Parameter for positive flow direction
!                                 in main section (depending on friction
!                                 type):
!                                 =     Chezy constant value
!                                 =     Table pointer (Q or h table)
!                                 =     Nikuradse parameter kn for Ni-
!                                       kuradse formula
!                                 =     Manning parameter nm for Manning
!                                       formula
!                                 =     Strickler coefficient ks for
!                                       Strickler formula
!                         (2,i) = Parameter for negative flow direction
!                                 in main section (depending on friction
!                                 type) Same definitions as bfricp(1,i).
!                         (3,i) = Parameter for positive flow direction
!                                 in sub sec 1 (depending on friction
!                                 type):
!                                 =     Chezy constant value
!                                 =     Nikuradse parameter kn for Niku-
!                                       radse formula
!                                 =     Manning parameter nm for Manning
!                                       formula
!                                 =     Strickler coefficient ks for
!                                       Strickler formula
!                         (4,i) = Parameter for negative flow direction
!                                 in sub sec 1 (depending on friction
!                                 type) Same definition as bfricp (3,i):
!                         (5,i) = Parameter for positive flow direction
!                                 in sub sec 2 (depending on friction
!                                 type) Same definition as bfricp (3,i).
!                         (6,i) = Parameter for negative flow direction
!                                 in sub sec 2 (depending on friction
!                                 type) Same definition as bfricp (3,i).
!  8 bfrict(3,nbran)   I  Bed friction in sections of branch.
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
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 10 engpar            P  -
! 11 grsize(4,ngrid,*) I  Grain sizes for each gridpoint and section.
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
! 18 hlev(ngrid,       IO (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  1 ibr               I  Branch number.
! 25 juer              P  -
! 26 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 16 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 12 maxtab            I  Maximum number of defined tables.
!  3 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
! 17 nlev(ngrid)       IO Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
! 14 ntab              P  -
! 13 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 23 of(ngrid)         IO Actual wetted perimeter at every cross secti-
!                         on.
!  5 prslot(3,nbran)   I  Contains information concerning Preissmann
!                         slot (assuring positive water depths).
!                         (1,i) = Create slot indicator
!                                 csldis (0) : No slot in this branch.
!                                 cslena (1) : Create slot in branch.
!                         (2,i) = Reference area for slot generation.
!                         (3,i) = Depth of slot below lowest bottom of
!                                 branch.
!  6 psltvr(7,ngrid)   O  Preissmann slot variables for every grid point
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
! 15 table             P  -
! 19 wft(ngrid,maxlev) IO (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width of main section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
! 21 wtt(ngrid,maxlev) IO (i,j) = total width at h = hlev(i,j) for grid
!                                 point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flchzt  FLow compute CHeZy Time dependent
! flinaw  FLow INterpolate Area and Width
! flperi  FLow PERImeter
! indwgh  Compute INDex and WeiGHt factor
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: cspslt.pf,v $
! Revision 1.11  1999/06/01  13:42:07  kuipe_j
! names in messages substituted + message template
!
! Revision 1.10  1999/03/15  15:49:07  kuipe_j
! tabs removed
!
! Revision 1.9  1998/02/25  12:48:47  kuipe_j
! Check on grain size added
!
! Revision 1.8  1997/09/30  09:23:00  kuipe_j
! density term improved for Preisman slot
!
! Revision 1.7  1997/03/04  16:11:41  kuipe_j
! Temorarily set of slot width removed
!
! Revision 1.6  1997/01/23  08:28:44  kuipe_j
! Make flow module robust
!
! Revision 1.5  1996/05/30  09:59:52  kuipe_j
! comment char
!
! Revision 1.4  1995/10/11  10:20:06  hoeks_a
! Preissman slot improved
!
! Revision 1.3  1995/05/30  09:54:24  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:55:30  hoeks_a
! files changed from dos-file to unix-files
!
! Revision 1.1  1995/04/13  06:58:32  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:29:51  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:40  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer   ibr ,ngrid ,nbran ,maxlev, maxtab, ntabm, juer, ker
!
   integer   branch (4,nbran),&
   &bfrict (3,nbran),&
   &nlev   (ngrid),&
   &ntab   (4,maxtab)
!
   real      prslot (3,nbran),&
   &psltvr (7,ngrid),&
   &bfricp (6,ngrid),&
   &engpar (9),&
   &grsize (4,ngrid,*),&
   &wft    (ngrid,maxlev),&
   &aft    (ngrid,maxlev),&
   &wtt    (ngrid,maxlev),&
   &att    (ngrid,maxlev),&
   &of     (ngrid,maxlev),&
   &table  (ntabm)
!
   double precision hlev(ngrid,maxlev)
!
!     Local variables
!
   integer  i1 ,i2 ,igp ,ilev ,isec, iilev, lbrnam
!
   logical  oke   ,lslot  ,epsequ
!
   real     aster ,z1 ,z2 ,w1 ,w2 ,term1 ,term2 ,&
   &u     ,z1ad  ,nikpos ,nikneg ,c ,&
   &d90   ,rster ,wt ,at, wf ,af ,o     ,&
   &alpha ,hster ,wster ,wst

   double precision wght, z0br, q

   character*40  branam
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
!     Set lslot to false to use original profile
!
   lslot = .false.
!
!     Read first and last gridpoint of branch
!
   i1 = branch(3,ibr)
   i2 = branch(4,ibr)
!
!     Determine lowest bottom of branch
!
   z0br = real( hlev(i1,1) )
   do 100 igp = i1, i2
      z0br = min ( z0br, real( hlev(igp,1) ) )
100 continue
!
!     Calculate depth of slot and determine w*
!
   z0br  = z0br - prslot(3,ibr)
   wst   = prslot(2,ibr)
!
!     Do for each cross section
!
   do 400 igp = i1, i2
!
      oke   = .true.
      iilev = 1
!
200   continue
      aster = wst * (real(hlev(igp,iilev)) - z0br)
      if ( aster .ge. aft(igp,iilev+1) ) then
         iilev = iilev + 1
         if ( iilev .lt. nlev(igp) ) then
            goto 200
         else
            oke = .false.
         endif
      endif
!
      z1 = real( hlev(igp,iilev) )
      z2 = real( hlev(igp,iilev+1) - hlev(igp,iilev) )
      w1 = wft(igp,iilev)
      w2 = wft(igp,iilev+1)
      if (epsequ(w2-w1,0.0,1E-6)) then
!
!           w1 = w2
!
         z1ad = z1 + ( aster-aft(igp,iilev) ) / w1
      else
!
!           w2 > w1
!
         term1 = -w1+sqrt(w1*w1+((2/z2)*(w2-w1))*&
         &(aster-aft(igp,iilev)))
         term2 = ( w2 - w1 ) / z2
         z1ad  = z1 + ( term1 / term2 )
      endif
      wster = aster/(z1ad-z0br)
!
!        Determine At, Wt, Af and Wf on level z1ad
!
      call indwgh ( ngrid  ,igp    ,&
      &maxlev ,nlev   ,hlev   ,&
      &dble(z1ad)     ,ilev   ,wght   )
!
      call flinaw ( ngrid  ,igp    ,lslot  ,&
      &maxlev ,nlev   ,hlev   ,&
      &dble(z1ad)   ,ilev   ,wght   ,&
      &wtt    ,att    ,&
      &wt     ,at     ,psltvr )
!
      call flinaw ( ngrid  ,igp    ,lslot  ,&
      &maxlev ,nlev   ,hlev   ,&
      &dble(z1ad)   ,ilev   ,wght   ,&
      &wft    ,aft    ,&
      &wf     ,af     ,psltvr )
!
      call flperi ( ngrid  ,igp    ,lslot  ,&
      &maxlev ,nlev   ,hlev   ,&
      &dble(z1ad)   ,ilev   ,&
      &wft    ,wf     ,&
      &of     ,o      ,psltvr )
!
!        Determine R* on level z1ad
!
      rster = af / o
!
!        Check if C = nikuradze, if so check 12R > nikuradze
!
      if (bfrict(1,ibr) .eq. cfrnik) then
         nikpos = bfricp(1,igp)/12.*3.
         nikneg = bfricp(2,igp)/12.*3.
         if (rster .le. min(nikneg,nikpos)) then
            oke = .false.
            ker = fatal
            call getbrn (ibr,branam,lbrnam)
            call error (juer,&
            &'CSPSLT Nikuradse-value is too high due to slot @' //&
            &branam(:lbrnam)// '@',&
            &ecsnik, ker)
         endif
      endif
!
!        Check oke
!
      if (.not. oke) then
         ker = fatal
         call getbrn (ibr,branam,lbrnam)
         call error (juer,&
         &'CSPSLT Unable to create slot @' //&
         &branam(:lbrnam)// '@',&
         &ecsslt, ker)
         goto 900
      endif
!
!        Calculate C for positive flow on level z(1*)
!
      u    = 0.
      q    = 1D0
!
!        Avoid zero grain size. Later on in FLINI a check will
!        carried out
!
      d90  = max(grsize ( 3, igp, 1 ),1.e-6)
      isec = 0
!
      call flchzt ( ibr    ,igp    ,nbran  ,ngrid  ,&
      &bfrict ,bfricp ,&
      &maxtab ,ntabm  ,ntab   ,table  ,d90    ,&
      &engpar ,isec   ,dble( z1ad )   ,q      ,&
      &u      ,rster  ,c      )
!
!        Calculate C2R positive direction
!
      psltvr(1,igp) = c * c * rster
!
!        Calculate C for negative flow on level z(1*)
!
      q = -1D0
!
      call flchzt ( ibr    ,igp    ,nbran  ,ngrid  ,&
      &bfrict ,bfricp ,&
      &maxtab ,ntabm  ,ntab   ,table  ,d90    ,&
      &engpar ,isec   ,dble(z1ad)     ,q      ,&
      &u      ,rster  ,c      )
!
!        Calculate C2R negative direction
!
      psltvr(2,igp) = c * c * rster
!
!        calculation of division-level H* of  Preismann-'trechter'
!
      alpha = max(.2 , 1.5*wster/wt)
      psltvr(3,igp) = z0br
      hster = ((alpha*wt/2.-0.75*wster)*z1ad+wster/2.*z0br)/&
      &(alpha*wt/2.-wster/4.)
      psltvr(4,igp) = hster
      psltvr(5,igp) = z1ad
      psltvr(6,igp) = wster/2.
      psltvr(7,igp) = alpha*wt
!
400 continue

   return
!
900 continue
!
   return
end
