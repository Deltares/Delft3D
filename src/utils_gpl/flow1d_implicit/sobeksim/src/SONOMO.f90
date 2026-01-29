subroutine SONOMO (istep  ,time   ,itim   ,dtf    ,filstp ,cpredn,&
&steady ,itp    ,lkalm  ,lsalt  ,lsedt  ,lmorp ,&
!                        mozart parameters
&lmoza  ,lgrwt  ,lrest  ,nstep  ,&
&lrivr  ,juer   ,juresi ,jufrou ,juresd ,justrd,&
&ker    ,inocon ,jusold ,lfrou  ,itstat ,frobuf)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SONOMO (SObek NO MOrphology)
!
! Module description: This routine iterates one time step for rivers or
!                     estuaries without the morphology module.
!
!                     First a flow step is calculated by a number of
!                     iteration steps. If the flow step has been calcu-
!                     lated routine FLNP1 will be called. This routine
!                     calculates a number of variables on time level
!                     n+1. This is necessary because other modules and
!                     results use these variables on this time level
!                     whereas the flow module uses these variables on
!                     time level n+1/2.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 cpredn            P  -
!  4 dtf               I  Time step flow module
!  5 filstp            P  -
! 19 inocon            P  -
!  1 istep             P  -
!  3 itim              P  -
!  8 itp               I  Tidal period in whole numbers of flow step
! 22 itstat            P  -
! 13 juer              P  -
! 15 jufrou            P  -
! 14 juresi            P  -
! 20 jusold            P  -
! 16 justru            P  -
! 18 ker               I  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 21 lfrou             P  -
!  9 lkalm             P  -
! 12 lmorp             P  -
! 10 lsalt             I  Logical indicator for salt computation
!                         = .true.  : with salt computation
!                         = .false. : no salt computation
! 11 lsedt             I  Switch to enable sediment transport module
!  7 steady            P  -
!  2 time              P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! gtdpnt  GeT Double PoiNTer
! gtipnt  GeT Integer PoiNTer
! gtlpnt  GeT Logical PoiNTer
! gtrpnt  GeT Real PoiNTer
! salt    SALT module
! sedim   SEDIMent module
! soflow  SObek FLOW main routine
! sorpar  SObek Real PARameter
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
! $Log: sonomo.pf,v $
! Revision 1.21  1999/03/15  15:05:17  kuipe_j
! Improve Froude file
!
! Revision 1.20  1998/06/11  11:47:46  kuipe_j
! Estuary special integrated
!
! Revision 1.19  1997/11/26  14:56:30  kuipe_j
! diffusion zero for free flow
!
! Revision 1.18  1997/05/26  07:37:02  kuipe_j
! statistic of iteration improved
!
! Revision 1.17  1997/01/23  08:30:15  kuipe_j
! Make flow module robust
!
! Revision 1.16  1996/12/04  11:59:30  kuipe_j
! declarations / undefined vars
!
! Revision 1.15  1996/04/12  13:06:09  kuipe_j
! headers, minor changes
!
! Revision 1.14  1996/04/11  08:16:36  kuipe_j
! Kalman module added
!
! Revision 1.13  1996/01/17  14:47:39  kuipe_j
! header update
!
! Revision 1.12  1995/11/21  11:09:12  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.11  1995/10/18  10:51:29  hoeks_a
! Some small changes
!
! Revision 1.10  1995/10/18  09:01:05  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.9  1995/10/11  12:24:13  kuipe_j
! Remove aux output temp
!
! Revision 1.8  1995/09/29  10:36:41  kuipe_j
! Improvement of autostart and simple weir
!
! Revision 1.7  1995/09/22  10:04:25  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:11:33  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:37:35  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:52  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:57:02  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:55  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:21  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:28:42  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:10:01  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:44  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer  itim(2),istep  ,juer   ,ker    ,itp   ,juresi, jufrou,&
   &juresd, justrd ,filstp ,cpredn ,inocon,jusold,itstat(4)
   logical  lsalt  ,lsedt  ,lmorp  ,steady ,lkalm ,lfrou ,lrivr
!     mozart declarations
   logical  lmoza, lgrwt, lrest
   integer  nstep

   double   precision       time   ,dtf
   real     frobuf (8)
!
!     Local variables (pointers to arrays)
!
   integer abcd1 ,abcd2 ,afwfqs,alfab ,bfrict,branch,cpack ,&
   &engpar,grid  ,grsize,hpack ,hbdpar,hlev  ,indx  ,&
   &mat   ,maxlev,maxtab,nbran ,ngrid ,ngridm,nhstat,&
   &nlev  ,nnode ,node  ,nqlat ,nstru ,ntab  ,ntabm ,&
   &prslot,psltvr,qpack ,qlat  ,qltpar,rpack ,rfv1  ,&
   &rfv2  ,rho   ,rhsvv ,sectv ,strclo,strtyp,strhis,&
   &table ,waoft ,wft   ,x
!
!     Additional variables for the salt module (pointers to arrays)
!
   integer bramrl,brnode,cdcdx ,csa   ,csd   ,disgr ,dispf ,&
   &dsopt ,emppar,mouqpu,mouth ,nboun ,nbrnod,nmouth,&
   &nqfloq,nslat ,ntmpgr,qfloq ,salstr,sbdpar,sbdscr,&
   &sltpar,thasca,thcsum,timout,tmpgr ,tw
!
!     Additional variables for the sediment module
!
   integer bgout ,celer ,dissed,e     ,forcon,mbdpar,mltpar,&
   &morcon,nellvl,nmlat ,nnelvl,nonall,nsedrd,nucoef,&
   &rc    ,sdrdbf,seddb ,sedinf,sedpar,sedtr ,slat  ,&
   &trform,uscoef,ws
!
!     Single variables
!
   real    psi    ,theta  ,thetas,  g
!
   double  precision   ltp
!
   integer ixpar
!
!     External functions
!
   integer  gtdpnt, gtipnt, gtlpnt, gtrpnt
   real     sorpar
   external gtdpnt, gtipnt, gtlpnt, gtrpnt, sorpar
!
!     Include memory pool
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
!
!     Extract parameters from flwpar
!
   ixpar = gtrpnt ( 'FLWPAR' )
!
   g      = sorpar ( rp(ixpar), 1 )
   psi    = sorpar ( rp(ixpar), 2 )
   theta  = sorpar ( rp(ixpar), 3 )
!
!     Find starting addresses of working arrays
!
!     Single variables are read from the memory pool to simplify the
!     call for the Microsoft Fortran compiler v5
!
   abcd1  =     gtdpnt ( 'ABCD1' )
   abcd2  =     gtdpnt ( 'ABCD2' )
   afwfqs =     gtrpnt ( 'AFWFQS')
   alfab  =     gtrpnt ( 'ALFAB' )
   bfrict =     gtipnt ( 'BFRICT')
   branch =     gtipnt ( 'BRANCH')
   cpack  =     gtrpnt ( 'CPACK' )
   engpar =     gtrpnt ( 'ENGPAR')
   grid   =     gtipnt ( 'GRID'  )
   grsize =     gtrpnt ( 'GRSIZE')
   hpack  =     gtdpnt ( 'HPACK')
   hbdpar =     gtipnt ( 'HBDPAR')
   hlev   =     gtdpnt ( 'HLEV'  )
   indx   =     gtipnt ( 'INDX'  )
   mat    =     gtdpnt ( 'MAT'   )
   maxlev = ip (gtipnt ( 'MAXLEV'))
   maxtab = ip (gtipnt ( 'MAXTAB'))
   nbran  = ip (gtipnt ( 'NBRAN' ))
   ngrid  = ip (gtipnt ( 'NGRID' ))
   ngridm = ip (gtipnt ( 'NGRIDM'))
   nhstat = ip (gtipnt ( 'NHSTAT'))
   nlev   =     gtipnt ( 'NLEV'  )
   nnode  = ip (gtipnt ( 'NNODE' ))
   node   =     gtipnt ( 'NODE'  )
   nqlat  = ip (gtipnt ( 'NQLAT' ))
   nstru  = ip (gtipnt ( 'NSTRU' ))
   ntab   =     gtipnt ( 'NTAB'  )
   ntabm  = ip (gtipnt ( 'NTABM' ))
   prslot =     gtrpnt ( 'PRSLOT')
   psltvr =     gtrpnt ( 'PSLTVR')
   qpack  =     gtdpnt ( 'QPACK')
   qlat   =     gtrpnt ( 'QLAT'  )
   qltpar =     gtrpnt ( 'QLTPAR')
   rpack  =     gtrpnt ( 'RPACK' )
   rfv1   =     gtdpnt ( 'RFV1'  )
   rfv2   =     gtdpnt ( 'RFV2'  )
   rho    =     gtrpnt ( 'RHO'   )
   rhsvv  =     gtdpnt ( 'RHSVV' )
   sectv  =     gtrpnt ( 'SECTV' )
   strclo =     gtlpnt ( 'STRCLO')
   strtyp =     gtipnt ( 'STRTYP')
   strhis =     gtrpnt ( 'STRHIS')
   table  =     gtrpnt ( 'TABLE' )
   waoft  =     gtrpnt ( 'WAOFT' )
   wft    =     gtrpnt ( 'WFT'   )
   x      =     gtrpnt ( 'X'     )
!
   call  SOFLOW ( istep  ,time   ,itim   ,dtf    ,filstp ,&
   &cpredn ,steady ,lsalt  ,lkalm  ,&
!                    mozart parameters
   &lmoza  ,lgrwt  ,lrest  ,nstep  ,&
   &juresi ,jufrou ,juresd ,justrd ,juer   ,ker    ,&
   &inocon ,jusold ,lfrou  ,itstat ,frobuf )
!
!     If salt or sediment in application
!
   if (ker.ne.fatal) then
      if (lsalt) then
!
!          Extract parameters from salpar
!
         ixpar  = gtrpnt ( 'SALPAR' )
!
         thetas = sorpar ( rp(ixpar), 3 )
         if (thetas .le. 1.e-6) then
!             Use theta of flow module
            thetas = theta
         endif
!
!          Determine pointers to arrays in memory pools
!
         bramrl =     gtipnt ('BRAMRL')
         cdcdx  =     gtrpnt ('CDCDX' )
         csa    =     gtrpnt ('CSA'   )
         csd    =     gtrpnt ('CSD'   )
         disgr  =     gtrpnt ('DISGR' )
         dispf  =     gtipnt ('DISPF' )
         dsopt  = ip (gtipnt ('DSOPT' ))
         emppar =     gtrpnt ('EMPPAR')
         mouqpu =     gtrpnt ('MOUQPU')
         mouth  =     gtipnt ('MOUTH' )
         nboun  = ip (gtipnt ('NBOUN' ))
         nmouth = ip (gtipnt ('NMOUTH'))
         nqfloq = ip (gtipnt ('NQFLOC'))
         nslat  = ip (gtipnt ('NSLAT' ))
         ntmpgr = ip (gtipnt ('NTMPGR'))
         qfloq  =     gtrpnt ('QFLOC' )
         salstr =     gtrpnt ('SALSTR')
         sbdpar =     gtrpnt ('SBDPAR')
         sbdscr =     gtrpnt ('SBDSCR')
         sltpar =     gtrpnt ('SLTPAR')
         thasca =     gtrpnt ('THASCA')
         thcsum =     gtrpnt ('THCSUM')
         timout =     gtrpnt ('TIMOUT')
         tmpgr  =     gtrpnt ('TMPGR' )
         tw     =     gtrpnt ('TW'    )
!
!          Calculate Tp
!
         ltp = itp * dtf
!

         call salt (    nbran  ,   nnode  ,   nboun  ,&
         &nmouth ,   nstru  ,   nhstat ,   nqfloq ,&
         &nqlat  ,   nslat  ,   ngrid  ,   ngridm ,&
         &maxtab ,   ntabm  ,   ntmpgr ,   itim   ,&
         &dsopt  ,   juer   ,   g      ,&
         &time   ,   dtf    ,   thetas ,   psi    ,&
         &ltp    ,ip(branch),ip(mouth ),ip(bramrl),&
         &ip(node  ),ip(indx  ),ip(strtyp),&
         &ip(hbdpar),ip(dispf ),ip(ntab  ),ip(grid  ),&
         &rp(x     ),rp(rho   ),rp(disgr) ,dp(abcd1 ),&
         &dp(abcd2 ),rp(cpack ),dp(qpack ),rp(waoft ),&
         &rp(csa   ),rp(csd   ),dp(rfv1  ),dp(rfv2  ),&
         &rp(tmpgr ),rp(cdcdx ),rp(tw    ),rp(thcsum),&
         &rp(sbdpar),rp(sbdscr),rp(emppar),rp(mouqpu),&
         &rp(timout),rp(qfloq ),rp(salstr),rp(thasca),&
         &rp(sltpar),rp(qltpar),rp(qlat  ),rp(table ),&
         &dp(mat   ),dp(rhsvv ),lp(strclo),rp(strhis),&
         &ker    )

      endif

      if (lsedt .and. ker.ne.fatal) then
!
!           Determine pointers to arrays in memory pools
!
         afwfqs =     gtrpnt ( 'AFWFQS')
         bgout  =     gtipnt ( 'BGOUT' )
         brnode =     gtipnt ( 'BRNODE')
         celer  =     gtrpnt ( 'CELER' )
         dissed =     gtrpnt ( 'DISSED')
         e      =     gtrpnt ( 'E'     )
         forcon =     gtrpnt ( 'FORCON')
         mbdpar =     gtipnt ( 'MBDPAR')
         mltpar =     gtrpnt ( 'MLTPAR')
         morcon =     gtrpnt ( 'MORCON')
         nboun  = ip (gtipnt ( 'NBOUN' ))
         nbrnod = ip (gtipnt ( 'NBRNOD'))
         nellvl =     gtrpnt ( 'NELLVL')
         nmlat  = ip (gtipnt ( 'NMLAT' ))
         nnelvl = ip (gtipnt ( 'NNELVL'))
         nonall =     gtipnt ( 'NONALL')
         nsedrd = ip (gtipnt ( 'NSEDRD'))
         ntmpgr = ip (gtipnt ( 'NTMPGR'))
         nucoef = ip (gtipnt ( 'NUCOEF'))
         rc     =     gtrpnt ( 'RC'    )
         sdrdbf =     gtipnt ( 'SDRDBF')
         seddb  =     gtipnt ( 'SEDDB' )
         sedinf =     gtipnt ( 'SEDINF')
         sedpar =     gtrpnt ( 'SEDPAR')
         sedtr  =     gtrpnt ( 'SEDTR' )
         slat   =     gtrpnt ( 'SLAT'  )
         tmpgr  =     gtrpnt ( 'TMPGR' )
         trform =     gtrpnt ( 'TRFORM')
         uscoef =     gtrpnt ( 'USCOEF')
         ws     =     gtrpnt ( 'WS'    )
!
         call sedim (  nbran  ,   nnode  ,   nbrnod ,   nboun  ,&
         &maxlev ,   nsedrd ,   nnelvl ,&
         &nqlat  ,   nmlat  ,   ngrid  ,   maxtab ,&
         &ntabm  ,   juer   ,   itim   ,   time   ,&
         &dtf    ,   g      ,   lmorp  ,   ntmpgr ,&
         &rp(tmpgr) ,ip(branch),ip(sedinf),ip(bfrict),&
         &ip(nonall),ip(node  ),ip(seddb ),ip(brnode),&
         &ip(bgout ),ip(sdrdbf),ip(mbdpar),&
         &ip(ntab  ),rp(afwfqs),rp(waoft ),rp(ws    ),&
         &dp(hlev ),ip(nlev  ),rp(wft   ),rp(psltvr),&
         &rp(sectv ),rp(alfab ),rp(rpack ),rp(x     ),&
         &rp(cpack ),dp(qpack ),dp(hpack ),rp(grsize),&
         &rp(forcon),rp(slat  ),rp(sedtr ),rp(celer ),&
         &rp(trform),rp(prslot),rp(dissed),rp(nellvl),&
         &rp(rc    ),rp(e     ),   nucoef ,rp(uscoef),&
         &rp(engpar),rp(sedpar),rp(morcon),rp(mltpar),&
         &rp(qltpar),rp(qlat  ),rp(table ),   lrivr  ,&
         &ker    )
      endif
   endif

end
