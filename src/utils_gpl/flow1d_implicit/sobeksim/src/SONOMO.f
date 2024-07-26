      subroutine SONOMO (istep  ,time   ,itim   ,dtf    ,filstp ,cpredn,
     +                   steady ,itp    ,lkalm  ,lsalt  ,lsedt  ,lmorp ,
c                        mozart parameters
     +                   lmoza  ,lgrwt  ,lrest  ,nstep  ,
     +                   lrivr  ,juer   ,juresi ,jufrou ,juresd ,justrd,
     +                   ker    ,inocon ,jusold ,lfrou  ,itstat ,frobuf)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SONOMO (SObek NO MOrphology)
c
c Module description: This routine iterates one time step for rivers or
c                     estuaries without the morphology module.
c
c                     First a flow step is calculated by a number of
c                     iteration steps. If the flow step has been calcu-
c                     lated routine FLNP1 will be called. This routine
c                     calculates a number of variables on time level
c                     n+1. This is necessary because other modules and
c                     results use these variables on this time level
c                     whereas the flow module uses these variables on
c                     time level n+1/2.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 cpredn            P  -
c  4 dtf               I  Time step flow module
c  5 filstp            P  -
c 19 inocon            P  -
c  1 istep             P  -
c  3 itim              P  -
c  8 itp               I  Tidal period in whole numbers of flow step
c 22 itstat            P  -
c 13 juer              P  -
c 15 jufrou            P  -
c 14 juresi            P  -
c 20 jusold            P  -
c 16 justru            P  -
c 18 ker               I  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 21 lfrou             P  -
c  9 lkalm             P  -
c 12 lmorp             P  -
c 10 lsalt             I  Logical indicator for salt computation
c                         = .true.  : with salt computation
c                         = .false. : no salt computation
c 11 lsedt             I  Switch to enable sediment transport module
c  7 steady            P  -
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
c $Log: sonomo.pf,v $
c Revision 1.21  1999/03/15  15:05:17  kuipe_j
c Improve Froude file
c
c Revision 1.20  1998/06/11  11:47:46  kuipe_j
c Estuary special integrated
c
c Revision 1.19  1997/11/26  14:56:30  kuipe_j
c diffusion zero for free flow
c
c Revision 1.18  1997/05/26  07:37:02  kuipe_j
c statistic of iteration improved
c
c Revision 1.17  1997/01/23  08:30:15  kuipe_j
c Make flow module robust
c
c Revision 1.16  1996/12/04  11:59:30  kuipe_j
c declarations / undefined vars
c
c Revision 1.15  1996/04/12  13:06:09  kuipe_j
c headers, minor changes
c
c Revision 1.14  1996/04/11  08:16:36  kuipe_j
c Kalman module added
c
c Revision 1.13  1996/01/17  14:47:39  kuipe_j
c header update
c
c Revision 1.12  1995/11/21  11:09:12  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.11  1995/10/18  10:51:29  hoeks_a
c Some small changes
c
c Revision 1.10  1995/10/18  09:01:05  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.9  1995/10/11  12:24:13  kuipe_j
c Remove aux output temp
c
c Revision 1.8  1995/09/29  10:36:41  kuipe_j
c Improvement of autostart and simple weir
c
c Revision 1.7  1995/09/22  10:04:25  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:11:33  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:37:35  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:52  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:57:02  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:55  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:21  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:28:42  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:10:01  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:44  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer  itim(2),istep  ,juer   ,ker    ,itp   ,juresi, jufrou,
     +         juresd, justrd ,filstp ,cpredn ,inocon,jusold,itstat(4)
      logical  lsalt  ,lsedt  ,lmorp  ,steady ,lkalm ,lfrou ,lrivr
c     mozart declarations
      logical  lmoza, lgrwt, lrest
      integer  nstep
       
      double   precision       time   ,dtf
      real     frobuf (8)
c
c     Local variables (pointers to arrays)
c
      integer abcd1 ,abcd2 ,afwfqs,alfab ,bfrict,branch,cpack ,
     +        engpar,grid  ,grsize,hpack ,hbdpar,hlev  ,indx  ,
     +        mat   ,maxlev,maxtab,nbran ,ngrid ,ngridm,nhstat,
     +        nlev  ,nnode ,node  ,nqlat ,nstru ,ntab  ,ntabm ,
     +        prslot,psltvr,qpack ,qlat  ,qltpar,rpack ,rfv1  ,
     +        rfv2  ,rho   ,rhsvv ,sectv ,strclo,strtyp,strhis,
     +        table ,waoft ,wft   ,x
c
c     Additional variables for the salt module (pointers to arrays)
c
      integer bramrl,brnode,cdcdx ,csa   ,csd   ,disgr ,dispf ,
     +        dsopt ,emppar,mouqpu,mouth ,nboun ,nbrnod,nmouth,
     +        nqfloq,nslat ,ntmpgr,qfloq ,salstr,sbdpar,sbdscr,
     +        sltpar,thasca,thcsum,timout,tmpgr ,tw
c
c     Additional variables for the sediment module
c
      integer bgout ,celer ,dissed,e     ,forcon,mbdpar,mltpar,
     +        morcon,nellvl,nmlat ,nnelvl,nonall,nsedrd,nucoef,
     +        rc    ,sdrdbf,seddb ,sedinf,sedpar,sedtr ,slat  ,
     +        trform,uscoef,ws
c
c     Single variables
c
      real    psi    ,theta  ,thetas,  g
c
      double  precision   ltp
c
      integer ixpar
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
c     Extract parameters from flwpar
c
      ixpar = gtrpnt ( 'FLWPAR' )
c
      g      = sorpar ( rp(ixpar), 1 )
      psi    = sorpar ( rp(ixpar), 2 )
      theta  = sorpar ( rp(ixpar), 3 )
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
c
      call  SOFLOW ( istep  ,time   ,itim   ,dtf    ,filstp ,
     +               cpredn ,steady ,lsalt  ,lkalm  ,
c                    mozart parameters
     +               lmoza  ,lgrwt  ,lrest  ,nstep  ,
     +               juresi ,jufrou ,juresd ,justrd ,juer   ,ker    ,
     +               inocon ,jusold ,lfrou  ,itstat ,frobuf )
c
c     If salt or sediment in application
c
      if (ker.ne.fatal) then
        if (lsalt) then
c
c          Extract parameters from salpar
c
           ixpar  = gtrpnt ( 'SALPAR' )
c
           thetas = sorpar ( rp(ixpar), 3 )
           if (thetas .le. 1.e-6) then
c             Use theta of flow module 
              thetas = theta
           endif   
c
c          Determine pointers to arrays in memory pools
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
c
c          Calculate Tp
c
           ltp = itp * dtf
c

           call salt (    nbran  ,   nnode  ,   nboun  ,
     +                    nmouth ,   nstru  ,   nhstat ,   nqfloq ,
     +                    nqlat  ,   nslat  ,   ngrid  ,   ngridm ,
     +                    maxtab ,   ntabm  ,   ntmpgr ,   itim   ,
     +                    dsopt  ,   juer   ,   g      ,
     +                    time   ,   dtf    ,   thetas ,   psi    ,
     +                    ltp    ,ip(branch),ip(mouth ),ip(bramrl),
     +                 ip(node  ),ip(indx  ),ip(strtyp),
     +                 ip(hbdpar),ip(dispf ),ip(ntab  ),ip(grid  ),
     +                 rp(x     ),rp(rho   ),rp(disgr) ,dp(abcd1 ),
     +                 dp(abcd2 ),rp(cpack ),dp(qpack ),rp(waoft ),
     +                 rp(csa   ),rp(csd   ),dp(rfv1  ),dp(rfv2  ),
     +                 rp(tmpgr ),rp(cdcdx ),rp(tw    ),rp(thcsum),
     +                 rp(sbdpar),rp(sbdscr),rp(emppar),rp(mouqpu),
     +                 rp(timout),rp(qfloq ),rp(salstr),rp(thasca),
     +                 rp(sltpar),rp(qltpar),rp(qlat  ),rp(table ),
     +                 dp(mat   ),dp(rhsvv ),lp(strclo),rp(strhis),
     +                   ker    )

        endif

        if (lsedt .and. ker.ne.fatal) then
c
c           Determine pointers to arrays in memory pools
c
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
c
            call sedim (  nbran  ,   nnode  ,   nbrnod ,   nboun  ,
     +                    maxlev ,   nsedrd ,   nnelvl ,
     +                    nqlat  ,   nmlat  ,   ngrid  ,   maxtab ,
     +                    ntabm  ,   juer   ,   itim   ,   time   ,
     +                    dtf    ,   g      ,   lmorp  ,   ntmpgr ,
     +                 rp(tmpgr) ,ip(branch),ip(sedinf),ip(bfrict),
     +                 ip(nonall),ip(node  ),ip(seddb ),ip(brnode),
     +                 ip(bgout ),ip(sdrdbf),ip(mbdpar),
     +                 ip(ntab  ),rp(afwfqs),rp(waoft ),rp(ws    ),
     +                 dp(hlev ),ip(nlev  ),rp(wft   ),rp(psltvr),
     +                 rp(sectv ),rp(alfab ),rp(rpack ),rp(x     ),
     +                 rp(cpack ),dp(qpack ),dp(hpack ),rp(grsize),
     +                 rp(forcon),rp(slat  ),rp(sedtr ),rp(celer ),
     +                 rp(trform),rp(prslot),rp(dissed),rp(nellvl),
     +                 rp(rc    ),rp(e     ),   nucoef ,rp(uscoef),
     +                 rp(engpar),rp(sedpar),rp(morcon),rp(mltpar),
     +                 rp(qltpar),rp(qlat  ),rp(table ),   lrivr  ,
     +                    ker    )
        endif
      endif

      end
