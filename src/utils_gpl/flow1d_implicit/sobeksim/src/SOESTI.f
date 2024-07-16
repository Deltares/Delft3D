      subroutine SOESTI ( istep  ,time   ,itim   ,dtf    ,
     +                    steady ,itp    ,steps  ,lsalt  ,
     +                    juer   ,juresi ,jufrou ,juresd ,
     +                    justrd ,ker    ,inocon ,jusold ,
     +                    lfrou  ,itstat ,frobuf ,lrest  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOESTI (SObek EStuary TIdal calculation)
c
c Module description: This routine iterates one time step of the tidal
c                     period.
c
c                     First a flow step is iterated. If the flow step
c                     has reached convergence optional a salt step is
c                     performed followed by a sediment transport calcu-
c                     lation. The next step is the calculation of the
c                     celereties. The calculated sediment transports and
c                     celereties are aggregated by routine SOSCAG.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 dtf               I  Time step flow module
c 15 inocon            P  -
c  1 istep             P  -
c  3 itim              P  -
c  6 itp               I  Tidal period in whole numbers of flow step
c 18 itstat            P  -
c  9 juer              P  -
c 11 jufrou            P  -
c 12 juresd            P  -
c 10 juresi            P  -
c 16 jusold            P  -
c 13 justrd            P  -
c 14 ker               I  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 17 lfrou             P  -
c  8 lsalt             I  Logical indicator for salt computation
c                         = .true.  : with salt computation
c                         = .false. : no salt computation
c  5 steady            P  -
c  7 steps             P  -
c  2 time              P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c gtdpnt  GeT Double PoiNTer
c gtipnt  GeT Integer PoiNTer
c gtlpnt  GeT Logical PoiNTer
c gtrpnt  GeT Real PoiNTer
c salt    SALT module
c sedim   SEDIMent module
c soflow  SObek FLOW main routine
c sorpar  SObek Real PARameter
c soscag  SObek Sediment Celerity AGgregate
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: soesti.pf,v $
c Revision 1.17  1999/03/15  15:05:14  kuipe_j
c Improve Froude file
c
c Revision 1.16  1998/06/11  11:47:39  kuipe_j
c Estuary special integrated
c
c Revision 1.15  1997/11/26  14:56:28  kuipe_j
c diffusion zero for free flow
c
c Revision 1.14  1997/05/26  07:36:59  kuipe_j
c statistic of iteration improved
c
c Revision 1.13  1997/01/23  08:30:10  kuipe_j
c Make flow module robust
c
c Revision 1.12  1996/12/04  11:59:28  kuipe_j
c declarations / undefined vars
c
c Revision 1.11  1996/04/12  13:06:03  kuipe_j
c headers, minor changes
c
c Revision 1.10  1996/04/11  08:16:30  kuipe_j
c Kalman module added
c
c Revision 1.9  1995/11/21  11:09:10  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.8  1995/10/18  09:01:01  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.7  1995/09/29  10:36:37  kuipe_j
c Improvement of autostart and simple weir
c
c Revision 1.6  1995/09/22  10:04:09  kuipe_j
c variable dimensions, new headers
c
c Revision 1.5  1995/09/12  08:11:30  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1995/08/30  12:37:32  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:56:52  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:46  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:11  hoeks_a
c Initial check-in
c
c Revision 1.4  1994/11/28  08:28:36  kuipe_j
c Time and timestep in double precision.

c Revision 1.3  1993/12/13  15:49:53  kuipe_j
c Improved declaration of DSTEP.
c Pointers for Salt module are only set if module is present.
c
c Revision 1.2  1993/11/26  15:09:47  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:42  kuipe_j
c Initial version
c
c
c***********************************************************************
c

c
c     Parameters
c
      integer  itim(2),istep ,juer  ,ker ,itp, steps, juresi ,jufrou ,
     +         juresd ,justrd,inocon,jusold, itstat(4)
      double precision     time   ,dtf
      logical  lsalt  ,steady, lfrou, lrest
      real     frobuf (8)

c
c     Local variables (pointers to arrays)
c
      integer abcd1 ,abcd2 ,adissd,afwfqs,alfab ,
     +        asedtr,aslat ,bfrict,bgout ,bramrl,branch,brnode,
     +        cdcdx ,celer ,cpack ,csa   ,csd   ,disgr ,dispf ,
     +        dissed,dsopt ,e     ,emppar,engpar,forcon,grid  ,
     +        grsize,hbdpar,hlev  ,hpack ,indx  ,mat   ,maxlev,
     +        maxtab,mbdpar,mltpar,morcon,mouqpu,mouth ,nboun ,
     +        nbran ,nbrnod,nellvl,ngrid ,ngridm,nhstat,nlev  ,
     +        nmlat ,nmouth,nnelvl,nnode ,node  ,nonall,nqfloq,
     +        nqlat ,nsedrd,nslat ,nstru ,ntab  ,ntabm ,ntmpgr,
     +        nucoef,prslot,psltvr,qfloq ,qlat  ,qltpar,qpack ,
     +        rc    ,rfv1  ,rfv2  ,rho   ,rhsvv ,rpack ,salstr,
     +        sbdpar,sbdscr,sdrdbf,sectv ,seddb ,sedinf,sedpar,
     +        sedtr ,slat  ,sltpar,strclo,strhis,strtyp,table ,
     +        thasca,thcsum,timout,tmpgr ,trform,tw    ,uscoef,
     +        waoft ,wft   ,ws    ,x     ,ahpack
c
c     Single variables
c
      real    psi  , theta , thetas, g     
      integer ixpar, filstp, cpredn
      logical lboun, lkalm,
c     mozart  variables
     +        lmozad, lgrwt
      integer nstepd

      double precision  ltp
c
c     External functions
c
      integer  gtdpnt, gtipnt, gtlpnt, gtrpnt 
      real     sorpar
      external gtdpnt, gtipnt, gtlpnt, gtrpnt, sorpar
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
c     Sediment module should process morphodynamic boundary conditions
c
      lboun  = .true.
c
c     Extract parameters from flwpar
c
      ixpar = gtrpnt ( 'FLWPAR' )
      g      = sorpar ( rp(ixpar), 1 )
      psi    = sorpar ( rp(ixpar), 2 )
      theta  = sorpar ( rp(ixpar), 3 )
c
c     omega instead of urelax, urelax dummy now
c
c     Find starting addresses of working arrays
c
c     Single variables are read from the memory pool to simplify the
c     call for the Microsoft Fortran compiler v5
c
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
      hpack  =     gtdpnt ( 'HPACK ')
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
      ntab   =     gtipnt ( 'NTAB'  )
      ntabm  = ip (gtipnt ( 'NTABM' ))
      prslot =     gtrpnt ( 'PRSLOT')
      psltvr =     gtrpnt ( 'PSLTVR')
      qpack  =     gtdpnt ( 'QPACK' )
      qlat   =     gtrpnt ( 'QLAT'  )
      qltpar =     gtrpnt ( 'QLTPAR')
      rpack  =     gtrpnt ( 'RPACK' )
      rfv1   =     gtdpnt ( 'RFV1'  )
      rfv2   =     gtdpnt ( 'RFV2'  )
      rho    =     gtrpnt ( 'RHO'   )
      rhsvv  =     gtdpnt ( 'RHSVV' )
      sectv  =     gtrpnt ( 'SECTV' )
      strclo =     gtlpnt ( 'STRCLO')
      table  =     gtrpnt ( 'TABLE' )
      waoft  =     gtrpnt ( 'WAOFT' )
      wft    =     gtrpnt ( 'WFT'   )
      x      =     gtrpnt ( 'X'     )

      filstp = 0
      cpredn = 0
      lkalm  = .false.
c     mozart  parameters
      lmozad = .false.
      lgrwt  = .false.
      nstepd = 0
c
      call  SOFLOW ( istep  ,time   ,itim   ,dtf    ,filstp ,
     +               cpredn ,steady ,lsalt  ,lkalm  ,
c                    mozart parameters
     +               lmozad ,lgrwt  ,lrest  ,nstepd ,juresi ,
     +               jufrou ,juresd ,justrd ,juer   ,ker    ,
     +               inocon ,jusold ,lfrou  ,itstat ,frobuf )

      if (lsalt .and. ker.ne.fatal) then
c
c       Extract parameters from salpar
c
        thetas = sorpar ( rp(ixpar), 3 )
        if (thetas .le. 1.e-6) then
c          Use theta of flow module 
           thetas = theta
        endif   
c
c       Determine pointers to arrays in memory pools
c
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
        nstru  = ip (gtipnt ('NSTRU' ))
        ntmpgr = ip (gtipnt ('NTMPGR'))
        qfloq  =     gtrpnt ('QFLOC' )
        salstr =     gtrpnt ('SALSTR')
        sbdpar =     gtrpnt ('SBDPAR')
        sbdscr =     gtrpnt ('SBDSCR')
        sltpar =     gtrpnt ('SLTPAR')
        strhis =     gtrpnt ('STRHIS')
        strtyp =     gtipnt ('STRTYP')
        thasca =     gtrpnt ('THASCA')
        thcsum =     gtrpnt ('THCSUM')
        timout =     gtrpnt ('TIMOUT')
        tmpgr  =     gtrpnt ('TMPGR' )
        tw     =     gtrpnt ('TW'    )
c
c        Calculate Tp
c
        ltp = itp * dtf
c
        call salt (    nbran  ,   nnode  ,   nboun  ,
     +                 nmouth ,   nstru  ,   nhstat ,   nqfloq ,
     +                 nqlat  ,   nslat  ,   ngrid  ,   ngridm ,
     +                 maxtab ,   ntabm  ,   ntmpgr ,   itim   ,
     +                 dsopt  ,   juer   ,   g      ,
     +                 time   ,   dtf    ,   thetas ,   psi    ,
     +                 ltp    ,ip(branch),ip(mouth ),ip(bramrl),
     +              ip(node  ),ip(indx  ),ip(strtyp),
     +              ip(hbdpar),ip(dispf ),ip(ntab  ),ip(grid  ),
     +              rp(x     ),rp(rho   ),rp(disgr) ,dp(abcd1 ),
     +              dp(abcd2 ),rp(cpack ),dp(qpack ),rp(waoft ),
     +              rp(csa   ),rp(csd   ),dp(rfv1  ),dp(rfv2  ),
     +              rp(tmpgr ),rp(cdcdx ),rp(tw    ),rp(thcsum),
     +              rp(sbdpar),rp(sbdscr),rp(emppar),rp(mouqpu),
     +              rp(timout),rp(qfloq ),rp(salstr),rp(thasca),
     +              rp(sltpar),rp(qltpar),rp(qlat  ),rp(table ),
     +              dp(mat   ),dp(rhsvv ),lp(strclo),rp(strhis),
     +                 ker    )
c
      endif
c
      if (ker .ne. fatal) then
c
c        Sediment step
c
c        Determine pointers to arrays in memory pools
c
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
c
         call sedim (    nbran  ,   nnode  ,   nbrnod ,   nboun  ,
     +                   maxlev ,   nsedrd ,   nnelvl ,
     +                   nqlat  ,   nmlat  ,   ngrid  ,   maxtab ,
     +                   ntabm  ,   juer   ,   itim   ,   time   ,
     +                   dtf    ,   g      ,   lboun  ,   ntmpgr ,
     +                rp(tmpgr) ,ip(branch),ip(sedinf),ip(bfrict),
     +                ip(nonall),ip(node  ),ip(seddb ),ip(brnode),
     +                ip(bgout ),ip(sdrdbf),ip(mbdpar),
     +                ip(ntab  ),rp(afwfqs),rp(waoft ),rp(ws    ),
     +                dp(hlev ),ip(nlev  ),rp(wft   ),rp(psltvr),
     +                rp(sectv ),rp(alfab ),rp(rpack ),rp(x     ),
     +                rp(cpack ),dp(qpack ),dp(hpack ),rp(grsize),
     +                rp(forcon),rp(slat  ),rp(sedtr ),rp(celer ),
     +                rp(trform),rp(prslot),rp(dissed),rp(nellvl),
     +                rp(rc    ),rp(e     ),   nucoef ,rp(uscoef),
     +                rp(engpar),rp(sedpar),rp(morcon),rp(mltpar),
     +                rp(qltpar),rp(qlat  ),rp(table ),   .false.,
     +                   ker    )
c
c        Aggregate sediment transports, lateral sediment and
c        water levels.
c
         adissd =  gtrpnt ( 'ADISSD')
         asedtr =  gtrpnt ( 'ASEDTR')
         aslat  =  gtrpnt ( 'ASLAT' )
         ahpack =  gtrpnt ( 'AHPACK' )
         call soscag (    steps  ,
     +                    nbran  ,   ngrid  ,
     +                 rp(sedtr) ,rp(dissed), rp(slat)  ,
     +                 rp(asedtr),rp(adissd), rp(aslat) ,
     +                 rp(ahpack),dp(hpack)             )
c
      endif
c
      end
