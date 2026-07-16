subroutine saini1 (dsopt  ,nmouth ,nboun  ,nbran  ,ngrid  ,maxtab,&
&ntabm  ,juer   ,g      ,rhow   ,time   ,tp    ,&
&le     ,dispf  ,mouth  ,branch ,bramrl ,ntab  ,&
&table  ,sbdpar ,moupar ,grid   ,x      ,af    ,&
&wf     ,c      ,q2     ,csa2   ,distmp ,disgr ,&
&csd2   ,rho    ,cdcdx0 ,cdcdx1 ,cdcdx2 ,thasca,&
&mouqpu ,tw     ,thcsum ,timout ,sbdscr ,ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SAINI1 (SAlt INItialise subroutine 1)
!
! Module description: Set the initial salt concentration values for each
!                     gridpoint.
!
!                     The user specifies the initial concentrations for
!                     each gridpoint in the user interface. These con-
!                     centrations are needed for the advection diffusion
!                     equation. The initial diffusion (c's) must be cal-
!                     culated in every grid point. Also the salt concen-
!                     tration at outflow boundaries must be stored.
!
!                     In the case that the Thatcher-Harleman or Zwendl
!                     formulation has been chosen as the dispersion
!                     formulation a.o. the following will be calculated:
!                     -   The constant term of the Thatcher-Harleman
!                         formulation.
!                     -   The initial values for the fresh water di-
!                         scharge, the flood volume and maximum flood
!                         velocity.
!                     -   The initial value of the term <c/c0*dc/dx>.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 24 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 17 bramrl(nmouth+1,  I  Branch-Mouth relation table. The first index
!        ,nbran)          contains the number of related mouths (index
!                         1). The second index contains the first rela-
!                         ted mouth number etc.
! 16 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 26 c                 P  -
! 33 cdcdx0(ngrid)     O  Contains for every grid point on time t=n+1
!                         the sum of the term c/c0*dc/dx over the cur-
!                         rent tide.
! 34 cdcdx1(ngrid)     IO Contains for every grid point the averaged
!                         value of term <c/c0*dc/dx> over the last tide.
! 35 cdcdx2(ngrid)     O  Contains for every grid point the averaged
!                         value of term <c/c0*dc/dx> over the tide befo-
!                         re the last tide.
! 28 csa2(ngrid)       I  Salt concentration in every grid point at time
!                         t(n+1).
! 31 csd2(ngrid)       O  Diffusion (c s) in every grid point at time
!                         t(n+1).
! 30 disgr(ngrid)      I  Dispersion coefficient in every grid point at
!                         time t(n+1).
! 14 dispf             P  -
! 29 distmp(ngrid)     I  Scratch array for dispersion calculation.
!  1 dsopt             I  Option for dispersion for the whole network:
!                           cds1fu (1) : One function of place or time
!                           cds2fu (2) : Two functions of place or time
!                           cdsthh (3) : Thatcher-Harleman formulation
!                           cdsemp (4) : Empirical formulation
!  9 g                 I  Acceleration of gravity.
! 22 grid              P  -
!  8 juer              P  -
! 42 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 13 le                I  Estuary length (salt module).
!  6 maxtab            I  Maximum number of defined tables.
! 21 moupar(5,nmouth)  I  Mouth parameters (for Thatcher-Harleman or
!                         Empirical formulation).
!                         (1,i) = Reference salt concentration at mouth
!                                 i.
!                         (2,i) = Reference water depth at mouth i.
!                         (3,i) = Characteristic flood velocity at mouth
!                                 i.
!                         (4,i) = Density of sea water at mouth i.
!                         (5,i) = Initial fresh water discharge at mouth
!                                 i.
! 37 mouqpu(3,0:2,     IO Contains auxilliary data for the Thatcher
!        nmouth)          Harleman or ZWENDL dispersion formulation.
!                         - First index:
!                         (1,,) = Fresh water discharge.
!                         (2,,) = Flood volume.
!                         (3,,) = Maximum flood velocity.
!                         - Second index:
!                         (,0,) = For current tide. Mouqpu(i,0,j) con-
!                                 tains the actual sum or maximum on the
!                                 current time.
!                         (,1,) = For the last tide.
!                         (,2,) = For the tide before the last tide.
!                         - Third index:
!                         (,,i) = Mouth number.
! 15 mouth(2,nmouth)   I  Node numbers which are mouths:
!                         (1,i) = Node number j which is a mouth.
!                         (2,i) = Number of the branch that contains the
!                                 mouth.
!  3 nboun             I  Number of boundary nodes.
!  4 nbran             I  Number of branches.
!  5 ngrid             I  Number of grid points in network.
!  2 nmouth            I  Maximum number of mouths in the network.
! 18 ntab              P  -
!  7 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 27 q2                P  -
! 32 rho               P  -
! 10 rhow              I  Density of fresh water.
! 20 sbdpar(5,nboun)   I  Definition of salt boundary conditions:
!                         (1,i) = Option for boundary condition:
!                                 csbusr (1) : User specified concen-
!                                              tration at inflow
!                                 csbthh (2) : Thatcher-Harleman for-
!                                              mulation at inflow
!                                 csbflx (3) : Zero flux
!                         (2,i) = Location (node number).
!                         (3,i) = Branch number that is connected.
!                         (4,i) = Table number (Options 1 and 2).
!                         (5,i) = T0 period for option 2, else undefi-
!                                 ned.
! 41 sbdscr(3,nboun)   O  Intermediate results at salt boundaries:
!                         (1,i) = Last time of outflow (option = 2)
!                         (2,i) = Concentration at last time of outflow
!                                 (option = 2)
!                         (3,i) = Concentration at inflow (time n+1) if
!                                 option is 1 or 2
! 19 table             P  -
! 36 thasca(3)         O  Administration for the calculation of
!                         <c/c0*dc/dx>:
!                         (1) =   End time of current tide.
!                         (2) =   Number of time steps that contribute
!                                 in current sum.
!                         (3) =   0 : Frst tidal period not started yet.
!                                 1 : Fist tidal period has started.
! 39 thcsum(2,nbran)   O  Contains Thatcher-Harleman sum per branch:
!                         (1,i) = Constant part of Thatcher-Harleman
!                                 sum.
!                         (2,i) = Part of Thatcher-Harleman sum that is
!                                 constant in one tidal period.
! 11 time              I  Actual time level tn+1. in sec.
! 40 timout(2,nmouth)  O  Administration for the calculation of fresh
!                         water discharge, flood volume and maximum
!                         flood velocity for every mouth:
!                         (1,i) = Starting time of current tide.
!                         (2,i) = 0 : Fist tidal period not started yet.
!                                 1 : Fist tidal period has started.
! 12 tp                I  Tidal period   (salt module).
! 38 tw                P  -
! 25 wf                P  -
! 23 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! sadcdx  SAlt DC/DX calculation
! sadens  SAlt DENSity
! sadspc  SAlt DiSPersion Coefficient
! satbpa  SAlt Thatcher harleman Branch PArameters
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: saini1.pf,v $
! Revision 1.7  1999/06/01  13:42:36  kuipe_j
! names in messages substituted + message template
!
! Revision 1.6  1995/10/18  09:00:24  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.5  1995/08/30  12:37:19  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:42  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:56:11  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:06  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:48  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:13  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:46  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:14  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer dsopt ,nmouth  ,nboun ,nbran   ,ngrid    ,maxtab  ,ntabm ,&
   &juer  ,ker
   integer dispf (2,3)    ,mouth (2,*)       ,branch(4,nbran) ,&
   &bramrl(nmouth+1,nbran)            ,ntab(4,maxtab)
   integer grid  (ngrid)
   real    g     ,rhow      ,le
   real    table (ntabm)    ,sbdpar(5,nboun) ,moupar(5,*)   ,&
   &x     (ngrid)    ,af    (ngrid)   ,wf    (ngrid) ,&
   &c     (ngrid)    ,csa2  (ngrid)   ,&
   &distmp(ngrid)    ,disgr (ngrid)   ,csd2  (ngrid) ,&
   &cdcdx0(ngrid)    ,cdcdx1(ngrid)   ,cdcdx2(ngrid) ,&
   &rho   (ngrid)    ,&
   &thasca(3    )    ,mouqpu(3,0:2,*) ,&
   &tw    (nbran)    ,thcsum(2,nbran) ,&
   &timout(2,*)      ,sbdscr(3,nboun)
   double  precision  time  , tp, q2(ngrid)
!
!     Declaration of local variables
!
   integer   ibr   ,igr    ,i1   ,i2    ,nmj    ,im     ,i   ,j   ,&
   &ib    ,lbrnam
   real      c0j   ,confac ,d0j  ,rhoj  ,uastj  ,castj  ,pi
   character*40     branam
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
!     Calculate dc/dx
!                                                               dc/dx
   call sadcdx (nbran  ,ngrid  ,branch ,grid   ,csa2   ,x   ,distmp)
!
!---- Initialize in case of Thatcher Harleman or ZWENDL dispersion ----
!     formulation.
!
   if (dsopt .eq. cdsthh .or. dsopt .eq. cdsemp) then
!
      pi = atan(1.)*4.
      do 10 im = 1,nmouth
!
!           Calculate grid point at mouth
!
         ibr = mouth(2,im)
         if (branch(1,ibr) .eq. mouth(1,im)) then
!
!              Mouth at begin of branch
            igr = branch(3,ibr)
         else
!
!              Mouth at end of branch
            igr = branch(4,ibr)
         endif
!
!           Initial fresh water discharge, flood volume, and maximum
!           flood velocity are determined.
!           [ Doc. S-FO-001.5KV / Eq. 19-'after 10' ]
!
         mouqpu(1,1,im) = moupar(5,im)
         mouqpu(2,1,im) = af(igr) * moupar(3,im) * sngl(tp) / pi
         mouqpu(3,1,im) = moupar(3,im)
10    continue
!
      do 30 im=1,nmouth
         timout(1,im) = 0.
         timout(2,im) = 0.
         do 20 i = 1,3
            mouqpu(i,2,im) = mouqpu(i,1,im)
            mouqpu(i,0,im) = 0.
20       continue
30    continue
!
      do 40 i = 1,3
         thasca(i) = 0.
40    continue
!
!        Calculate constant part of Thatcher Harleman sum.
!
      confac = le * le * (sngl(tp) * g / rhow ) **.25
      do 60 ibr = 1,nbran
         uastj = 0.
         castj = 0.
         d0j   = 0.
         rhoj  = 0.
         nmj   = bramrl(1,ibr)
         if (nmj .eq. 0) then
!
!              No mouth defined for this branch
!
            ker = fatal
            call getbrn (ibr,branam,lbrnam)
            call error (juer,'SAINI1 branch @'//branam(:lbrnam)//&
            &'@',esabrm,ker)
            goto 1000
         endif
         do 50 i = 2,nmj+1
            im    = bramrl(i,ibr)
            uastj = uastj + moupar(3,im)
            castj = castj + moupar(1,im)
            d0j   = d0j   + moupar(2,im)
            rhoj  = rhoj  + moupar(4,im)
50       continue
         d0j   = d0j  / real(nmj)
         rhoj  = rhoj / real(nmj)
         thcsum(1,ibr) = confac * uastj / castj *&
         &(d0j * (rhoj - rhow)) ** .25
60    continue
!
!        Calculate part of Thatcher Harleman sum that is constant in
!        a tidal period.
!
      call satbpa (nmouth ,nbran ,juer ,bramrl ,mouqpu ,thcsum ,ker)
      if (ker .eq. fatal) goto 1000
!
!        Calculate the initial value of <c/c0*dc/dx>
!
      do 90 ibr = 1,nbran
!
!           First c0(j,0) , which is constant in a branch,
!           will be calculated.
!
         nmj  = bramrl(1,ibr)
         c0j  = 0.
         do 70 i = 2,nmj+1
            im = bramrl(i,ibr)
            j  = mouth(2,im)
            if (branch(1,j) .eq. mouth(1,im)) then
!
!                 Mouth at begin of branch
               igr = branch(3,j)
            else
!
!                 Mouth at end of branch
               igr = branch(4,j)
            endif
            c0j = c0j + csa2(igr)
70       continue
         c0j = c0j / real(nmj)
!
!           Check on zero concentrations at mouths
!
         if (c0j .lt. 1.e-10) then
            ker = fatal
            call getbrn (ibr,branam,lbrnam)
            call error (juer,'SAINI1 branch @'//branam(:lbrnam)//&
            &'@',esacon,ker)
            goto 1000
         endif
!
!           Calculation per grid point.
!
         i1 = branch(3,ibr)
         i2 = branch(4,ibr)
         do 80 igr = i1,i2
!                                               dc/dx
            cdcdx1(igr) = csa2(igr) / c0j * abs(distmp(igr))
80       continue
!
90    continue
      do 100 igr = 1,ngrid
         cdcdx2(igr) = cdcdx1(igr)
         cdcdx0(igr) = 0.
100   continue
   endif
!
!---- Calculate intial C-s ( i.e. A*D*dc/dx) and Rho -----------------
!     [ Doc. S-FO-001.5KV / Eq. 22-2 ]
!
   call sadspc (dsopt  ,nbran  ,ngrid  ,maxtab ,ntabm ,g      ,&
   &time   ,dispf  ,branch ,ntab   ,table ,thcsum ,&
   &grid   ,x      ,wf     ,q2     ,c      ,csa2  ,&
   &cdcdx1 ,cdcdx2 ,csd2   ,disgr  )
!                                 temporary
!
   do 110 igr = 1,ngrid
!                                       dc/dx
      csd2(igr) = af(igr)*disgr(igr)*distmp(igr)
110 continue
!
   call sadens (nbran ,ngrid ,juer  ,branch ,tw   ,csa2   ,&
   &rho   ,ker   )
!
!---- Initialize array SBDSCR for the boundaries ---------------------
!
   do 130 ib = 1,nboun
      do 120 i = 1,3
         sbdscr(i,ib) = 0.
120   continue
      if (int(sbdpar(1,ib)).eq.2) then
         sbdscr(1,ib) = sngl(time) - sbdpar(5,ib)
      endif
130 continue
!
1000 continue
!
end
