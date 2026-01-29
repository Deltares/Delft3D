subroutine SORIMO ( istep  ,time   ,itim   ,dtf    ,steady ,&
&juer   ,juresi ,jufrou ,juresd ,justrd ,&
&ker    ,inocon ,jusold ,lfrou  ,itstat ,&
&frobuf ,lrest)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SORIMO (SObek RIver MOrphology)
!
! Module description: This routine iterates one time step for rivers
!                     including the morphology module.
!
!                     When this module is called an initial flow must be
!                     available. This is the true in the following ca-
!                     ses:
!
!                     o      re-start has been executed;
!                     o      auto-start has been executed;
!                     o      steady flow step has been executed.
!
!                     The first step is the calculation of the courant
!                     number. The calculated celereties are used to
!                     determine the courant number (MOCOUR). From this
!                     routine an integer M (M >= 1) is returned which is
!                     used to reduce the current time step.
!
!                     The calculated sediment transports are used in the
!                     morphology module (MORPH) to calculate the changes
!                     of the cross sections. The next step is a call to
!                     CSTABL to calculate the derived cross sectional
!                     variables. After this a new flow step is calcula-
!                     ted using the new determined time level.
!
!                     The loop continues until the new end time has been
!                     reached or a maximum number of iterations has been
!                     reached.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 dtf               I  Time step flow module
! 12 inocon            P  -
!  1 istep             P  -
!  3 itim(2)           IO Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
! 15 itstat            P  -
!  6 juer              P  -
!  8 jufrou            P  -
!  9 juresd            P  -
!  7 juresi            P  -
! 13 jusold            P  -
! 10 justrd            P  -
! 11 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 14 lfrou             P  -
!  5 steady            P  -
!  2 time              IO Actual time level tn+1. in sec.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! cstabl  Cross Sectional TABLe module
! error   write an ERROR to the error file.
! gtipnt  GeT Integer PoiNTer
! gtrpnt  GeT Real PoiNTer
! mocour  MOrphology COURant number
! morph   MORPHology module
! sedim   SEDIMent module
! soflow  SObek FLOW main routine
! soipar  SObek Integer PARameter
! sorpar  SObek Real PARameter
! sosdir  SObek Sediment flow DIRection
! sotime  SObek TIME
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
! $Log: sorimo.pf,v $
! Revision 1.24  1999/03/15  15:05:19  kuipe_j
! Improve Froude file
!
! Revision 1.23  1998/06/11  11:47:48  kuipe_j
! Estuary special integrated
!
! Revision 1.22  1997/05/26  07:37:03  kuipe_j
! statistic of iteration improved
!
! Revision 1.21  1997/02/17  10:09:40  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.20  1997/01/23  08:30:16  kuipe_j
! Make flow module robust
!
! Revision 1.19  1996/12/04  11:59:31  kuipe_j
! declarations / undefined vars
!
! Revision 1.18  1996/12/02  10:03:49  kuipe_j
! avoid negative pointers
!
! Revision 1.17  1996/05/28  13:29:27  kuipe_j
! Error message courant nr added
!
! Revision 1.16  1996/04/12  13:06:10  kuipe_j
! headers, minor changes
!
! Revision 1.15  1996/04/11  08:16:37  kuipe_j
! Kalman module added
!
! Revision 1.14  1996/03/08  09:40:50  kuipe_j
! Headers
!
! Revision 1.13  1996/03/07  10:44:31  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.12  1996/02/09  15:13:38  kuipe_j
! a.o. Restart improvements
!
! Revision 1.11  1995/11/21  11:09:14  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.10  1995/10/18  10:51:30  hoeks_a
! Some small changes
!
! Revision 1.9  1995/10/18  09:01:06  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.8  1995/10/11  12:24:14  kuipe_j
! Remove aux output temp
!
! Revision 1.7  1995/09/29  10:36:43  kuipe_j
! Improvement of autostart and simple weir
!
! Revision 1.6  1995/09/22  10:04:28  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/09/12  08:11:34  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.4  1995/08/30  12:37:37  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:57:03  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:57  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:23  hoeks_a
! Initial check-in
!
! Revision 1.5  1994/12/02  13:33:31  kuipe_j
! Improvement of message handling.
!
! Revision 1.4  1994/11/28  08:28:43  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.3  1993/12/13  15:49:56  kuipe_j
! Improved declaration of DSTEP.
! Pointers for Salt module are only set if module is present.
!
! Revision 1.2  1993/11/26  15:10:04  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:45  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer  istep ,itim(2), ker   ,juer, juresi ,jufrou ,juresd
   integer  justrd,inocon ,jusold ,itstat(4)
   double   precision       time  ,dtf
   logical  steady, lfrou, lrest
   real     frobuf (8)
!
!     Local variables
!
   integer aft   ,afwfqs,alfab ,att   ,bfricp,bfrict,bgout ,&
   &branch,brnode,celer ,cpack ,dissed,e     ,engpar,&
   &forcon,flwdir,grid  ,grsize,hpack ,hlev  ,izwft ,&
   &maxlev,maxtab,mbdpar,mltpar,morcon,nboun ,nbran ,&
   &nbrnod,nellvl,ngrid ,nlev  ,nmlat ,nnelvl,nnode ,&
   &node  ,nonall,nqlat ,nsedrd,ntab  ,ntabm ,ntmpgr,&
   &nucoef,of    ,prslot,psltvr,qpack ,qlat  ,qltpar,&
   &rc    ,rpack ,sdrdbf,sectc ,sectv ,seddb ,sedinf,&
   &sedpar,sedtr ,slat  ,table ,tmpgr ,trform,sumda ,&
   &typcr ,uscoef,waoft ,wft   ,ws    ,wtt   ,x     ,&
   &morpar
   integer h2
!
!     Run parameters flow module
!
   real     g
   integer  filstp,cpredn
!
!     Run parameters morphology module
!
   real     alphac
   integer  moitmx
!
!     and other variables
!
   integer  ixpar  ,timnp1(2)
!
   double   precision       tnp1   ,ldtf
   integer  cnt    ,m    , igpmc
   logical  lastts ,lsalt, lboun, lkalm,&
!     mozart   variables
   &lmozad, lgrwt
   integer  nstepd
!
   character*4      txt
   character*18     txt1
!
!     External functions
!
   integer  gtipnt, gtrpnt, gtdpnt, soipar
   real     sorpar
   external gtipnt, gtrpnt, gtdpnt, soipar, sorpar
!
!     Include memory pool
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
!
!     Sediment module should process morphodynamic boundary conditions
!
   lboun  = .true.
!
!     Extract parameters from flwpar
!
   ixpar = gtrpnt ( 'FLWPAR' )
!
   g      = sorpar ( rp(ixpar), 1 )
!
!     Extract parameters from morpar
!
   ixpar  = gtrpnt ( 'MORPAR' )
!
   moitmx = soipar ( rp(ixpar), 2 )
   alphac = sorpar ( rp(ixpar), 3 )
!
!     Find starting addresses of working arrays
!
!     Single variables are read from the memory pool to simplify the
!     call for the Microsoft Fortran compiler v5
!
   aft    =     gtrpnt ( 'AFT'   )
   afwfqs =     gtrpnt ( 'AFWFQS')
   alfab  =     gtrpnt ( 'ALFAB' )
   att    =     gtrpnt ( 'ATT'   )
   bfricp =     gtrpnt ( 'BFRICP')
   bfrict =     gtipnt ( 'BFRICT')
   bgout  =     gtipnt ( 'BGOUT' )
   branch =     gtipnt ( 'BRANCH')
   brnode =     gtipnt ( 'BRNODE')
   celer  =     gtrpnt ( 'CELER' )
   cpack  =     gtrpnt ( 'CPACK' )
   dissed =     gtrpnt ( 'DISSED')
   e      =     gtrpnt ( 'E'     )
   engpar =     gtrpnt ( 'ENGPAR')
   flwdir =     gtipnt ( 'FLWDIR')
   forcon =     gtrpnt ( 'FORCON')
   grid   =     gtipnt ( 'GRID'  )
   grsize =     gtrpnt ( 'GRSIZE')
   hlev   =     gtdpnt ( 'HLEV'  )
   hpack  =     gtdpnt ( 'HPACK')
   izwft  =     gtrpnt ( 'IZWFT' )
   maxlev = ip (gtipnt ( 'MAXLEV'))
   maxtab = ip (gtipnt ( 'MAXTAB'))
   mbdpar =     gtipnt ( 'MBDPAR')
   mltpar =     gtrpnt ( 'MLTPAR')
   morcon =     gtrpnt ( 'MORCON')
   morpar =     gtrpnt ( 'MORPAR')
   nboun  = ip (gtipnt ( 'NBOUN' ))
   nbran  = ip (gtipnt ( 'NBRAN' ))
   nbrnod = ip (gtipnt ( 'NBRNOD'))
   nellvl =     gtrpnt ( 'NELLVL')
   ngrid  = ip (gtipnt ( 'NGRID' ))
   nlev   =     gtipnt ( 'NLEV'  )
   nmlat  = ip (gtipnt ( 'NMLAT' ))
   nnelvl = ip (gtipnt ( 'NNELVL'))
   nnode  = ip (gtipnt ( 'NNODE' ))
   node   =     gtipnt ( 'NODE'  )
   nonall =     gtipnt ( 'NONALL')
   nqlat  = ip (gtipnt ( 'NQLAT' ))
   nsedrd = ip (gtipnt ( 'NSEDRD'))
   ntab   =     gtipnt ( 'NTAB'  )
   ntabm  = ip (gtipnt ( 'NTABM' ))
   ntmpgr = ip (gtipnt ( 'NTMPGR'))
   nucoef = ip (gtipnt ( 'NUCOEF'))
   of     =     gtrpnt ( 'OF'    )
   prslot =     gtrpnt ( 'PRSLOT')
   psltvr =     gtrpnt ( 'PSLTVR')
   qlat   =     gtrpnt ( 'QLAT'  )
   qltpar =     gtrpnt ( 'QLTPAR')
   qpack  =     gtdpnt ( 'QPACK')
   rc     =     gtrpnt ( 'RC'    )
   rpack  =     gtrpnt ( 'RPACK' )
   sdrdbf =     gtipnt ( 'SDRDBF')
   sectc  =     gtrpnt ( 'SECTC' )
   sectv  =     gtrpnt ( 'SECTV' )
   seddb  =     gtipnt ( 'SEDDB' )
   sedinf =     gtipnt ( 'SEDINF')
   sedpar =     gtrpnt ( 'SEDPAR')
   sedtr  =     gtrpnt ( 'SEDTR' )
   slat   =     gtrpnt ( 'SLAT'  )
   sumda  =     gtrpnt ( 'SUMDA' )
   table  =     gtrpnt ( 'TABLE' )
   tmpgr  =     gtrpnt ( 'TMPGR' )
   trform =     gtrpnt ( 'TRFORM')
   typcr  =     gtipnt ( 'TYPCR' )
   uscoef =     gtrpnt ( 'USCOEF')
   waoft  =     gtrpnt ( 'WAOFT' )
   wft    =     gtrpnt ( 'WFT'   )
   ws     =     gtrpnt ( 'WS'    )
   wtt    =     gtrpnt ( 'WTT'   )
   x      =     gtrpnt ( 'X'     )
!     h2 = hp(,3)
   h2 = hpack + ngrid * 2
!
!     Determine time N+1
!
   tnp1 = time + dtf
   timnp1(1) = itim(1)
   timnp1(2) = itim(2)
   call sotime ( timnp1, dtf )
   cnt    = 0
!
!     Repeat
!
100 continue
!
!        Calculate current delta time step
!
   ldtf = tnp1 - time
!
!        Determine courant number and max step morphology module
!
   call mocour (    nbran  ,   ngrid  ,&
   &ip(branch),ip(typcr) ,&
   &ip(grid)  ,rp(celer) ,rp(x)     ,&
   &alphac ,sngl(ldtf),&
   &m      ,igpmc     )
!
!        Check if time step should be reduced
!
   if (m .gt. 1) then
!
!           Reduce time step
!
      ldtf    = ldtf / m
      time    = time + ldtf
      call sotime ( itim, ldtf )
      cnt     = cnt + 1
      lastts  = .false.
   else
      time    = tnp1
      itim(1) = timnp1(1)
      itim(2) = timnp1(2)
      lastts  = .true.
   endif
!
!        Determine flow direction at each grid point
!
   call SOSDIR( dp(qpack) , ngrid , ip(flwdir) )
!
!        Adapt bottoms
!
   call morph (    ngrid  ,   nbran  ,   nboun  ,   nnode  ,&
   &nbrnod ,   ntmpgr ,   time   ,   ldtf   ,&
   &rp(morpar),ip(flwdir),rp(sumda) ,&
   &ip(grid)  ,ip(branch),ip(node)  ,ip(typcr) ,&
   &ip(brnode),ip(bgout) ,rp(tmpgr) ,&
   &maxtab ,   ntabm  ,ip(ntab)  ,rp(table) ,&
   &ip(mbdpar),rp(x)     ,&
   &maxlev ,ip(nlev)  ,dp(hlev)  ,dp(h2)    ,&
   &rp(waoft) ,rp(wft)   ,rp(ws)    ,&
   &rp(sectc) ,rp(afwfqs),&
   &rp(celer) ,rp(sedtr) ,rp(dissed),rp(slat)  ,&
   &itim   ,   juer   ,   ker    ,rp(prslot))
!
!        Test for fault
!
   if (ker .eq. fatal) then
      goto 900
   endif
!
!        Calculate derived variables
!
   lsalt = .false.
   call cstabl (    ngrid  ,   maxlev ,ip(nlev)  ,dp(hlev)  ,&
   &rp(wft)   ,rp(aft)   ,rp(wtt)   ,rp(att)   ,&
   &rp(of)    ,   lsalt  ,rp(izwft) ,   nbran  ,&
   &ip(branch),ip(typcr) ,rp(sectc) ,rp(sectv) ,&
   &rp(prslot),rp(psltvr),ip(bfrict),rp(bfricp),&
   &rp(engpar),rp(grsize),   maxtab ,   ntabm  ,&
   &ip(ntab)  ,rp(table) ,rp(ws)    ,   .true. ,&
   &juer   ,   ker    )
!
!        Test for fault
!
   if (ker .eq. fatal) then
      goto 900
   endif
!
!        Kalman filter is switched off
!
   lkalm  = .false.
   filstp = 0
   cpredn = 0
!        mozart parameters
   lmozad = .false.
   lgrwt  = .false.
   nstepd = 0
!
   call  SOFLOW ( istep  ,time   ,itim   ,ldtf   ,filstp ,&
   &cpredn ,steady ,lsalt  ,lkalm  ,&
!                       mozart parameters
   &lmozad ,lgrwt  ,lrest  ,nstepd ,&
   &juresi ,&
   &jufrou ,juresd ,justrd ,juer   ,ker    ,&
   &inocon ,jusold ,lfrou  ,itstat ,frobuf )
!
!        Test for fault
!
   if (ker .eq. fatal) then
      goto 900
   endif
!
!        Sediment step
!
   call sedim (    nbran  ,   nnode  ,   nbrnod ,   nboun  ,&
   &maxlev ,   nsedrd ,   nnelvl ,&
   &nqlat  ,   nmlat  ,   ngrid  ,   maxtab ,&
   &ntabm  ,   juer   ,   itim   ,   time   ,&
   &ldtf   ,   g      ,   lboun  ,   ntmpgr ,&
   &rp(tmpgr) ,ip(branch),ip(sedinf),ip(bfrict),&
   &ip(nonall),ip(node  ),ip(seddb ),ip(brnode),&
   &ip(bgout ),ip(sdrdbf),ip(mbdpar),&
   &ip(ntab  ),rp(afwfqs),rp(waoft ),rp(ws    ),&
   &dp(hlev  ),ip(nlev  ),rp(wft   ),rp(psltvr),&
   &rp(sectv ),rp(alfab ),rp(rpack ),rp(x     ),&
   &rp(cpack ),dp(qpack ),dp(hpack ),rp(grsize),&
   &rp(forcon),rp(slat  ),rp(sedtr ),rp(celer ),&
   &rp(trform),rp(prslot),rp(dissed),rp(nellvl),&
   &rp(rc    ),rp(e     ),   nucoef ,rp(uscoef),&
   &rp(engpar),rp(sedpar),rp(morcon),rp(mltpar),&
   &rp(qltpar),rp(qlat  ),rp(table ),  .true.  ,&
   &ker    )
!
!        Test for fault
!
   if (ker .eq. fatal) then
      goto 900
   endif
!
!
!     If last time step not reached, try again
!
   istep = istep + 1
   if ((cnt .lt. moitmx) .and. (.not. lastts) ) then
      goto 100
   else if (cnt .eq. moitmx) then
      ker = fatal
      call error (juer,'SORIMO Morp iteration failed',emrncv,ker)
      istep = istep - 1
   else if (cnt .ge. 1) then
!
!        Inform the user
!
      write (txt,'(i4)') cnt
      call error ( juer,'SORIMO @'//txt//&
      &'@ reductions of morphological time step',&
      &erdmst, info )
      write (txt1,'(2(1x,i8))') itim
      call error (juer,'SORIMO timestep@'//txt1//'@',emomes,info)

   endif
!
900 continue
!
end
