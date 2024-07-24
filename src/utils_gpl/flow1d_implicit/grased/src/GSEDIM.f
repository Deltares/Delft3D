      subroutine gsedim (nbran  ,nnode  ,nbrnod ,nboun  ,maxlev ,
     &                   nqlat  ,nmlat  ,ngrid  ,maxtab ,ntabm  ,
     &                   nfrac  ,nlayer ,ntmpfr ,juer   ,itim   ,
     &                   time   ,dt     ,g      ,nunlay ,jugraut,
     &                   jugralg,branch ,sedinf ,prslot ,psltvr ,
     &                   disgse ,nonall ,seddb  ,brnode ,bgout  ,
     &                   sdrdbf ,mbdpar ,ntab   ,gsopts ,afwfqs ,
     &                   waoft  ,ws     ,hlev   ,nlev   ,wft    ,
     &                   sectv  ,alfab  ,rp     ,x      ,cp     ,
     &                   qp     ,hp     ,grsize ,forcon ,source ,
     &                   sedtr  ,celer  ,trform ,nucoef ,uscoef ,
     &                   duncon ,sedpar ,morcon ,mltpar ,
     &                   qltpar ,qlat   ,dfrac  ,pdiacc ,ptrla  ,
     &                   pexla  ,p0la   ,dmed0  ,depos  ,duneh  ,
     &                   dunel  ,deff   ,tmpfr  ,table  ,celert ,
     &                   nrdzdl ,sedexp ,zbave  ,zbfl   ,lastts ,
     &                   istep  ,ker    )


c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsedim.F,v $
c Revision 1.5  1996/06/07  11:56:19  kuipe_j
c multi  +  fixed layer
c
c Revision 1.4  1996/01/08  13:29:53  kuipe_j
c Multi layer option for under layer added
c
c Revision 1.2  1995/09/27  10:12:27  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer    nbran  ,nnode    ,nbrnod  ,nboun   ,maxlev ,
     &           ntmpfr  ,nqlat   ,nmlat   ,ngrid   ,jugraut,jugralg,
     &           maxtab ,ntabm    ,nfrac   ,nlayer  ,juer   ,ker    ,
     &           nunlay ,istep    ,nucoef 
      integer    branch(4,nbran)  ,sedinf(2,nbran)  ,
     &           nonall(3,nbran)  ,
     &           seddb(3,nnode)   ,
     &           brnode(nbrnod+1,nnode)             ,
     &           bgout (3,nbrnod) ,
     &           sdrdbf(2,*)      ,
     &           mbdpar(5,*)      ,
     &           itim  (2)        ,
     &           ntab  (4,maxtab) ,
     &           nlev  (ngrid)    ,
     &           gsopts(*)
      integer    nrdzdl(ngrid)
c
      real       g
      real       afwfqs(ngrid,8 ) ,waoft (ngrid,6)  ,ws    (ngrid)   ,
     &           wft   (ngrid,maxlev)          ,sectv (ngrid,dmsecv) ,
     &           rp    (ngrid,4)  ,x     (ngrid)    ,cp    (ngrid,4) ,
     &           alfab (ngrid)   ,
     &           grsize(4,ngrid,nlayer+1),forcon(nfrac+1,4,nbran)    ,
     &           source(ngrid,nfrac+2)   ,celert(ngrid)              ,
     &           sedtr (ngrid,nfrac+2)   ,celer (ngrid,nfrac+2)      ,
     &           duneh (ngrid)           ,dunel(ngrid)               ,
     &           dmed0 (ngrid)           ,deff (ngrid,2)             ,
     &           zbave (ngrid)           ,zbfl (ngrid)               ,
     &           sedexp(ngrid)           ,
     &           trform(3,nbran)         ,prslot(3,nbran)            ,
     &           disgse(nfrac,2,nbran)   ,tmpfr(nfrac+2,ntmpfr)      ,
     &           ptrla (ngrid,nfrac,2)   ,pexla (ngrid,nfrac,2)      ,
     &           p0la(ngrid,nfrac,nunlay),
     &           pdiacc(nfrac)           ,dfrac(nfrac)               
      real       uscoef(nrcoefs,nucoef)  ,
     &           sedpar(*)               ,duncon(5)                  ,
     &           morcon(2+2*nfrac,*)     ,mltpar(9,*)                ,
     &           qltpar(9,*)             ,qlat(*)                    ,
     &           table (ntabm)           ,
     &           psltvr(7,ngrid)
      double precision  time  ,dt,  hlev  (ngrid,maxlev)
      double precision hp(ngrid,3) , qp(ngrid,3)
      logical    lastts      
      logical    depos(ngrid)
c
c     Declaration of local parameters
c
      integer    heiopt ,rouopt ,lathic ,extout ,nvast
      real       pacfac ,relden ,kinvis ,fdi ,
     &           alffl  ,redfac
      real       duncof(3)
      character  txt*18
      logical    yesout
c
      ker = ok
c
      kinvis    = sedpar(1)
      relden    = sedpar(2)
      pacfac    = sedpar(3)
      alffl     = sedpar(4)
      duncof(1) = sedpar(6)
      duncof(2) = sedpar(7)
      duncof(3) = sedpar(8)
      fdi       = sedpar(10)
      redfac    = sedpar(12)

      heiopt    = gsopts(1)
      rouopt    = gsopts(3)
      lathic    = gsopts(4)
      extout    = gsopts(6)
      nvast     = gsopts(9)
c
c     Lateral sediment is calculated                  
c
      call gslsed (ngrid  ,nqlat  ,nmlat  ,nbran  ,nfrac  ,maxtab,
     &             ntabm  ,time   ,dt     ,branch ,x      ,
     &             qltpar ,mltpar ,qlat   ,ntab   ,table  ,
     &             source ,dfrac  ,sedtr  )
c
      call gsnetw (nbran  ,ngrid  ,maxlev ,nfrac  ,nlayer ,nunlay,
     &             heiopt ,lathic ,rouopt ,juer   ,jugralg,fdi   ,
     &             g      ,pacfac ,relden ,kinvis ,redfac ,nvast ,
     &             alffl  ,nonall ,branch ,sedinf ,duncof ,pdiacc,
     &             dfrac  ,duneh  ,dunel  ,nucoef ,uscoef ,
c                                         <af>       <afs>
     &             trform ,prslot ,psltvr ,waoft(1,3),afwfqs(1,1),
c                       <Wf>      <Wfs>
     &             waoft(1,1)     ,afwfqs(1,3)    ,nlev   ,hlev  ,
c                                 <secths>        <Rs>
     &             wft    ,ws     ,sectv(1,8)     ,rp(1,2),
c                 <Cs>            <Q2>    <Qs>           <h2>
     &             cp(1,2),alfab  ,qp(1,3),afwfqs(1,7)   ,hp(1,3),
c                                                 <ptrla2>
     &             grsize ,dmed0  ,x      ,source ,ptrla(1,1,2)  ,
c                 <pexla2>                <deff2>
     &             pexla(1,1,2)   ,p0la   ,deff(1,2)      ,forcon,
     &             duncon ,tmpfr  ,sedtr  ,depos  ,celer  ,celert,
c                 <asubsc>
     &             sectv(1,1)     ,nrdzdl ,sedexp ,zbave  ,zbfl  ,
     &             ker            )
c
      if (ker .eq. fatal) goto 1000
c
c     Non-alluvial layers are not implemented
c
c     Nodes are treated before boundaries as array DISGSE is
c     initialized in GSNODE.
c
      call gsnode (nnode  ,nbran  ,nbrnod ,ngrid  ,maxtab ,ntabm ,
     &             nfrac  ,branch ,brnode ,bgout  ,sedinf,sdrdbf ,
c                                         <q2>
     &             seddb  ,ntab   ,morcon ,qp(1,3),ws    ,table  ,
c                         <stotfr>
     &             sedtr  ,tmpfr(1,1)     ,disgse ,trform)
c
      call gsboun (nnode  ,nboun  ,nbran  ,ngrid  ,nbrnod ,maxtab ,
     &             ntabm  ,nfrac  ,time   ,
c                 <q2>
     &             qp(1,3),sedtr  ,mbdpar ,branch ,brnode ,ntab   ,
     &             table  ,disgse )
c
      if (extout.gt.0) then
         if (istep.gt.extout.and. lastts) then
           yesout = .true.
        else
           yesout = .false.
         endif
      else if (extout.lt.0) then
        yesout = .true.
      else
        yesout = .false.
      endif
      if (yesout) then
        call gstout (ngrid ,nfrac ,jugraut ,time ,
     &              'Sediment transport ',sedtr )
      endif
c
      return
c
 1000 continue
c
      write (txt,'(2(1x,i8))') itim
      call error (juer,'GSEDT timestep@'//txt//'@',esemes,info)
c
      end
