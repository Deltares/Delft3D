subroutine SOESMO ( time   ,itim   ,steps  ,fp     ,&
&dtm    ,lastts ,lsalt  ,lkalm  ,&
&nreduc ,juer   ,ker&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOESMO (SObek EStuary MOrphology)
!
! Module description: This routine performs the morphodynamic time step
!                     for an estuary. It is possible that the time step
!                     defined by the user is too large. In that case the
!                     time step will be adapted.
!
!                     First the aggregated sediment transports and cele-
!                     rities are avaraged by routine SOSCAV. The calcu-
!                     lated results is used to determine the courant
!                     number (MOCOUR). This routine delivers an integer
!                     M which is used to determine the maximum time step
!                     to be made by the morphodynamic process. If the
!                     initial time step is too large the reduction will
!                     be as follows:
!
!                     o   division of time step by courant number;
!                     o   rounding down to a whole number of flow steps;
!
!                     The reason for this way of adapting the time step
!                     is the possibility to calculate new tidal periods.
!
!                     After the morphodynamic step (MORPH) has been made
!                     routine CSTABL is called to calculate the derived
!                     cross sectional information.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  5 dtm               IO Morphology time step.
!  4 fp                I  Flow period [s]
!  2 itim(2)           I  Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
!  9 juer              P  -
! 10 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  6 lastts            IO True if last part of a reduced traject has
!                         been calculated
!  7 lsalt             P  -
!  8 nreduc            I  Number of time step reductions
!  3 steps             P  -
!  1 time              IO Actual time level tn+1. in sec.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! cstabl  Cross Sectional TABLe module
! error   write an ERROR to the error file.
! gtipnt  GeT Integer PoiNTer
! gtrpnt  GeT Real PoiNTer
! mocour  MOrphology COURant number
! morph   MORPHology module
! sorpar  SObek Real PARameter
! soscav  SObek Sediment Celerity AVarage
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
! $Log: soesmo.pf,v $
! Revision 1.18  1999/06/01  13:42:47  kuipe_j
! names in messages substituted + message template
!
! Revision 1.17  1999/03/15  15:19:45  kuipe_j
! tabs removed
!
! Revision 1.16  1998/06/11  11:47:38  kuipe_j
! Estuary special integrated
!
! Revision 1.15  1997/10/28  09:13:28  kuipe_j
! Improve reductions handling estuary morph.
!
! Revision 1.14  1997/02/17  10:09:38  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.13  1997/01/23  08:30:09  kuipe_j
! Make flow module robust
!
! Revision 1.12  1996/12/02  10:03:47  kuipe_j
! avoid negative pointers
!
! Revision 1.11  1996/09/03  14:33:41  kuipe_j
! frequency time hist, run in time est. morp
!
! Revision 1.10  1996/05/28  13:29:26  kuipe_j
! Error message courant nr added
!
! Revision 1.9  1996/04/12  13:06:02  kuipe_j
! headers, minor changes
!
! Revision 1.8  1996/04/11  08:16:29  kuipe_j
! Kalman module added
!
! Revision 1.7  1996/03/07  10:44:29  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.6  1995/11/21  11:09:09  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.5  1995/10/18  09:00:59  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/09/22  10:04:07  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:52  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:45  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:10  hoeks_a
! Initial check-in
!
! Revision 1.4  1994/12/02  13:33:27  kuipe_j
! Improvement of message handling.
!
! Revision 1.3  1994/11/28  08:28:34  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:09:45  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:42  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!
!     Parameters
!
   double   precision       time   ,fp    ,dtm
   integer  steps ,itim(2) ,nreduc ,juer  ,ker
   logical  lastts,lsalt,lkalm
!
!     Variables
!
   real     alphac,  xc , overlp, g
   double   precision       dtma   , deltat
   integer  ixpar  ,m   ,ibr, igpmc, moitmx, lbrnam
   character*4      txt
   character*18     txt1
   character*11     xtxt
   character*40    branam
!
!     Pointers to memory pool and single variables
!
   integer  adissd ,aft    ,afwfqs ,asedtr ,aslat  ,att    ,&
   &bfrict ,bfricp ,bgout  ,branch ,brnode ,celer  ,dissed ,&
   &engpar ,grid   ,grsize ,hlev   ,izwft  ,maxlev ,maxtab ,&
   &mbdpar ,nboun  ,nbran  ,nbrnod ,ngrid  ,nlev   ,nnode  ,&
   &node   ,ntab   ,ntabm  ,ntmpgr ,nucoef ,of     ,prslot ,&
   &psltvr ,sectc  ,sectv  ,sedtr  ,slat   ,table  ,tmpgr  ,&
   &typcr  ,waoft  ,wft    ,ws     ,wtt    ,x      ,morpar ,&
   &flwdir ,alfab  ,uscoef ,trform ,forcon ,sedpar ,ahpack ,&
   &cpack  ,rpack  ,scifri ,pfa    ,arex   ,arexcn ,sumda  ,&
   &arexop ,nnf    ,hqav
!
!     External functions
!
   integer  gtipnt, gtrpnt, gtdpnt, soipar
   real     sorpar
   external gtipnt, gtrpnt, gtdpnt, sorpar, soipar
!
!     Include memory pool
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
!
!     Read parameters for morphology module
!
   ixpar  = gtrpnt ( 'MORPAR')
   moitmx = soipar ( rp(ixpar), 2 )
   alphac = sorpar ( rp(ixpar), 3 )
!
   ixpar  = gtrpnt ( 'FLWPAR' )
   g      = sorpar ( rp(ixpar), 1 )
   overlp = sorpar ( rp(ixpar), 16)
!
!     Determine start addresses of arrays in memory pools
!
   adissd =     gtrpnt ( 'ADISSD')
   aft    =     gtrpnt ( 'AFT'   )
   afwfqs =     gtrpnt ( 'AFWFQS')
   asedtr =     gtrpnt ( 'ASEDTR')
   aslat  =     gtrpnt ( 'ASLAT' )
   att    =     gtrpnt ( 'ATT'   )
   bfrict =     gtipnt ( 'BFRICT')
   bfricp =     gtrpnt ( 'BFRICP')
   bgout  =     gtipnt ( 'BGOUT' )
   branch =     gtipnt ( 'BRANCH')
   brnode =     gtipnt ( 'BRNODE')
   celer  =     gtrpnt ( 'CELER' )
   dissed =     gtrpnt ( 'DISSED')
   engpar =     gtrpnt ( 'ENGPAR')
   flwdir =     gtipnt ( 'FLWDIR')
   grid   =     gtipnt ( 'GRID'  )
   grsize =     gtrpnt ( 'GRSIZE')
   hlev   =     gtdpnt ( 'HLEV'  )
   izwft  =     gtrpnt ( 'IZWFT' )
   maxlev = ip (gtipnt ( 'MAXLEV'))
   maxtab = ip (gtipnt ( 'MAXTAB'))
   mbdpar =     gtipnt ( 'MBDPAR')
   morpar =     gtrpnt ( 'MORPAR')
   nboun  = ip (gtipnt ( 'NBOUN' ))
   nbran  = ip (gtipnt ( 'NBRAN' ))
   nbrnod = ip (gtipnt ( 'NBRNOD'))
   ngrid  = ip (gtipnt ( 'NGRID' ))
   nlev   =     gtipnt ( 'NLEV'  )
   nnode  = ip (gtipnt ( 'NNODE' ))
   node   =     gtipnt ( 'NODE'  )
   ntab   =     gtipnt ( 'NTAB'  )
   ntabm  = ip (gtipnt ( 'NTABM' ))
   ntmpgr = ip (gtipnt ( 'NTMPGR'))
   nucoef = ip (gtipnt ( 'NUCOEF'))
   of     =     gtrpnt ( 'OF'    )
   prslot =     gtrpnt ( 'PRSLOT')
   psltvr =     gtrpnt ( 'PSLTVR')
   sectc  =     gtrpnt ( 'SECTC' )
   sectv  =     gtrpnt ( 'SECTV' )
   sedtr  =     gtrpnt ( 'SEDTR' )
   slat   =     gtrpnt ( 'SLAT'  )
   sumda  =     gtrpnt ( 'SUMDA' )
   table  =     gtrpnt ( 'TABLE' )
   tmpgr  =     gtrpnt ( 'TMPGR' )
   typcr  =     gtipnt ( 'TYPCR' )
   waoft  =     gtrpnt ( 'WAOFT' )
   wft    =     gtrpnt ( 'WFT'   )
   ws     =     gtrpnt ( 'WS'    )
   wtt    =     gtrpnt ( 'WTT'   )
   x      =     gtrpnt ( 'X'     )
!
   alfab  =     gtrpnt ( 'ALFAB' )
   sedpar =     gtrpnt ( 'SEDPAR')
   uscoef =     gtrpnt ( 'USCOEF')
   trform =     gtrpnt ( 'TRFORM')
   forcon =     gtrpnt ( 'FORCON')
   ahpack =     gtrpnt ( 'AHPACK')
   cpack  =     gtrpnt ( 'CPACK' )
   rpack  =     gtrpnt ( 'RPACK' )
   scifri =     gtipnt ( 'SCIFRI')
   nnf    =  ip(gtipnt ( 'NNF')  )
   pfa    =     gtrpnt ( 'PFA'   )
   arex   =     gtrpnt ( 'AREX'  )
   arexcn =     gtipnt ( 'AREXCN')
   arexop =     gtipnt ( 'AREXOP')
   hqav   =     gtdpnt ( 'HQAV'  )

!     Average water levels and related hydraulic parameters
!     and calculate bed celerities.
!
   call soscav (    steps   ,   nbran   ,   ngrid   ,ip(flwdir),&
   &rp(celer)  ,rp(sedtr)  ,rp(dissed) ,rp(slat)  ,&
   &rp(asedtr) ,rp(adissd) ,rp(aslat)  ,   juer   ,&
   &maxlev  ,ip(nlev)   ,dp(hlev)   ,rp(wft)   ,&
   &rp(ws)     ,rp(alfab)  ,rp(prslot) ,ip(branch),&
   &rp(sedpar) ,   g       ,rp(grsize) ,   nucoef ,&
   &rp(uscoef) ,rp(trform) ,rp(forcon) ,rp(engpar),&
   &rp(sectv)  ,rp(ahpack) ,rp(cpack)  ,&
   &rp(rpack)  ,rp(waoft)  ,rp(afwfqs) ,   lkalm  ,&
   &ip(typcr)  ,ip(bfrict) ,rp(bfricp) ,rp(aft)   ,&
   &overlp     ,rp(arex)   ,ip(arexcn) ,ip(arexop),&
   &rp(of)     ,   maxtab  ,   ntabm   ,ip(ntab)  ,&
   &rp(table)  ,rp(psltvr) ,ip(scifri) ,   nnf    ,&
   &rp(pfa)    ,dp(hqav )  ,rp(sectc)  ,rp(wtt)   ,&
   &rp(att)    ,   ker     )
!
   if (ker .eq. fatal) go to 900
!
!     Determine maximum courant number
!
   call mocour (    nbran  ,   ngrid  ,&
   &ip(branch),ip(typcr) ,&
   &ip(grid)  ,rp(celer) ,rp(x)     ,&
   &alphac ,sngl(dtm) ,&
   &m      ,igpmc     )
!
!     Check if time step should be reduced
!
   if (m .gt. 1) then
!
!        Adapt morphological time step
!
      dtma = dtm / m
!
!        Round down to a multiple of flow period
!
      if (int(dtma / fp) .eq. 0) then
!
         ker = info
         call getloc (igpmc,ibr,xc)
         write (xtxt,'(f10.2)') xc
         call getbrn (ibr,branam,lbrnam)
         call ERROR (juer,'SOESMO Maximum Courant number at @'&
         &//branam(:lbrnam)//'@ @'&
         &//xtxt//'@' ,&
         &emcour , ker )
         ker = fatal
         call error ( juer, 'SOESMO Dt Morph < 1 * Fp', emtsfp, ker )
         write (txt1,'(2(1x,i8))') itim
         call error (juer,'SOESMO timestep@'//txt1//'@',emomes,info)
         goto 900
      else
         dtma = int (dtma / fp) * fp
      endif
!
!        This is not the last morphological time step
!
      lastts = .false.
   else
!
!        This is the last morphological time step, back on original
!        calculation scheme.
!
      dtma   = dtm
      lastts = .true.
   endif
!
!     Adapt bottoms
!
   call morph (    ngrid  ,   nbran  ,   nboun  ,   nnode  ,&
   &nbrnod ,   ntmpgr ,   time   ,   dtma   ,&
   &rp(morpar),ip(flwdir),rp(sumda) ,&
   &ip(grid)  ,ip(branch),ip(node)  ,ip(typcr) ,&
   &ip(brnode),ip(bgout) ,rp(tmpgr) ,&
   &maxtab ,   ntabm  ,ip(ntab)  ,rp(table) ,&
   &ip(mbdpar),rp(x)     ,&
   &maxlev ,ip(nlev)  ,dp(hlev)  ,dp(hqav)  ,&
   &rp(waoft) ,rp(wft)   ,rp(ws)    ,&
   &rp(sectc) ,rp(afwfqs),&
   &rp(celer) ,rp(sedtr) ,rp(dissed),rp(slat)  ,&
   &itim   ,   juer   ,   ker    ,rp(prslot))
!
!     Calculate derived variables (cstabl)
!
   if (ker .eq. fatal) then
      goto 900
   endif
!
   call cstabl (    ngrid  ,   maxlev ,ip(nlev)  ,dp(hlev)  ,&
   &rp(wft)   ,rp(aft)   ,rp(wtt)   ,rp(att)   ,&
   &rp(of)    ,   lsalt  ,rp(izwft) ,   nbran  ,&
   &ip(branch),ip(typcr) ,rp(sectc) ,rp(sectv) ,&
   &rp(prslot),rp(psltvr),ip(bfrict),rp(bfricp),&
   &rp(engpar),rp(grsize),   maxtab ,   ntabm  ,&
   &ip(ntab)  ,rp(table) ,rp(ws)    ,   .true. ,&
   &juer   ,   ker    )
!
!     Update time
!
   deltat = dtma - fp
   time   = time + deltat
   call sotime ( itim, deltat )
!      WRITE (*,*) 'Morp step, Reduc=',nreduc
!
   if (nreduc .ge. 1 .and. lastts ) then
      ker = max ( ker, info )
      write (txt,'(i4)') nreduc
      call error ( juer,'SOESMO @'//txt//&
      &'@ reductions of morphological time step',&
      &erdmst, ker )
      write (txt1,'(2(1x,i8))') itim
      call error (juer,'SOESMO timestep@'//txt1//'@',emomes,info)
   else if (nreduc .ge. moitmx) then
      ker = fatal
      call error ( juer,'SOESMO Morp iteration failed',&
      &emrncv, ker )
      write (txt1,'(2(1x,i8))') itim
      call error (juer,'SOESMO timestep@'//txt1//'@',emomes,info)
   endif
!
!     Calculate new morphological time step
!
   dtm = dtm - dtma
!
900 continue
!
   return
end
