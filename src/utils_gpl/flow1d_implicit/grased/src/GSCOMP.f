      subroutine gscomp (ngrid  ,nfrac  ,nbran  ,nnode  ,nboun  ,nlayer,
     &                   maxtab ,ntabm  ,ngrain ,submin ,subplus,time  ,
     &                   dt     ,g      ,nunlay ,jugraut,jugralg,
     &                   gsopts ,branch ,node   ,mbdpar ,sedpar ,deltaa,
     &                   cp     ,qp     ,afwfqs ,ntab   ,table  ,grain ,
     &                   ddis   ,dfrac  ,grsizmun       ,sedtr  ,trform,
     &                   duncon ,p0la   ,pdiacc ,tmpfr  ,duneh  ,dunel ,
     &                   deff   ,ptrla  ,pexla  ,grsize ,dmed0  ,dzr   ,
     &                   nrdzdl ,ws     ,wfsold ,deltar ,zbave  ,zbfl  ,
     &                   sedexp ,levunl ,lastts ,istep  ,lanrinbt      ,
     &                   juer   ,ker    )
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gscomp.F,v $
c Revision 1.4  1996/06/07  11:55:57  kuipe_j
c multi  +  fixed layer
c
c Revision 1.3  1996/01/08  13:29:49  kuipe_j
c Multi layer option for under layer added
c
c Revision 1.2  1995/09/27  10:11:58  kuipe_j
c Maintenance
c
c
c***********************************************************************
c     Graded Sediment calculate COMPosition of layers

c     Declaration of parameters
c
      integer    ngrid ,nfrac  ,nbran   ,nnode ,nboun  ,nlayer ,maxtab ,
     &           ntabm ,juer   ,nunlay  ,istep ,jugraut,jugralg,ker    ,
     &           ngrain,submin ,subplus
      integer    branch(4,nbran)        ,ntab  (4,maxtab)      ,
     &           mbdpar(5,nboun)        ,node  (4,nnode)       ,
     &           nrdzdl(ngrid)          ,lanrinbt(ngrid)       ,
     &           gsopts(*)              ,grain(4) 
      real       g
      real       sedtr (ngrid,nfrac+2)  ,
     &           p0la  (ngrid,nfrac,nunlay)                    ,
     &           grsizmun(ngrid,ngrain,submin:subplus)         ,     
     &           ptrla (ngrid,nfrac,2)  ,pexla (ngrid,nfrac,2) ,
     &           deltar(ngrid,nfrac)   ,
     &           grsize(4,ngrid,nlayer+1),
     &           cp    (ngrid,4)        ,deff  (ngrid,2)       ,
     &           afwfqs(ngrid,8)        ,duneh (ngrid)         ,
     &           dunel (ngrid)          ,dmed0 (ngrid)         ,
     &           trform(3,nbran)        ,tmpfr (nfrac+2,*)     ,
     &           ddis  (nfrac+1)        ,dfrac (nfrac)         ,
     &           table (ntabm)          ,
     &           sedpar(*)              ,duncon(*)             ,
     &           pdiacc(nfrac)          ,levunl(ngrid)         ,
     &           zbave (ngrid)          ,zbfl  (ngrid)         ,
     &           ws    (ngrid)          ,dzr   (ngrid)         ,
     &           wfsold(ngrid)          ,sedexp(ngrid)
      double precision   time           ,dt                    ,
     &                   deltaa(ngrid,nfrac+1)
      double precision  qp(ngrid,3)
      logical    lastts
c
      include '..\include\errcod.i'
c
c     Declaration of local parameters
c
      integer    ibr    ,igr    ,lathic ,heiopt ,lenopt ,adnoml ,
     &           extout ,nonngp ,nvast  ,lbrnam ,gperr 
      real       relden ,kinvis ,thexfa ,fdi    ,zbeps  ,redfac 
      real       xc     ,duncof (3)
      logical    initra ,sedim  ,yesout
      character*10       txtx
      character*40       branam
c
      integer    dmed
      parameter (dmed=4)

      kinvis    = sedpar(1)
      relden    = sedpar(2)
      duncof(1) = sedpar(6)
      duncof(2) = sedpar(7)
      duncof(3) = sedpar(8)
      thexfa    = sedpar(9)
      fdi       = sedpar(10)
      redfac    = sedpar(12)
      zbeps     = sedpar(13)
c
      heiopt    = gsopts(1)
      lenopt    = gsopts(2)
      lathic    = gsopts(4)
      adnoml    = gsopts(5)
      extout    = gsopts(6)
      sedim     = gsopts(7).eq.1
      nonngp    = gsopts(8)
      nvast     = gsopts(9)
c     
      initra    = .false.

      call gscbou (nboun ,nbran  ,ngrid  ,maxtab ,ntabm  ,nfrac  ,
c                                <q2>
     &             nlayer,time   ,qp(1,3),mbdpar ,branch ,ntab   ,
c                        <ptrla2>        <pexla2>
     &             table ,ptrla(1,1,2)   ,pexla(1,1,2)   )

      do 20 ibr=1,nbran

c        Calculate dune height and length

         call gsdune (ngrid  ,nbran  ,ibr    ,initra ,heiopt ,lenopt ,
c                                                            <cs> 
     &                g      ,relden ,kinvis ,branch ,grsize ,cp(1,2),
c                    <qs>            <afs>           <wfs>  
     &                afwfqs(1,7)    ,afwfqs(1,1)    ,afwfqs(1,3)    ,
     &                duncof ,trform ,duncon ,sedexp ,duneh  ,dunel  )

c        Calculate layer thickness
         
         do 10 igr = branch(3,ibr),branch(4,ibr)
c                                        <deff2(igr)> 
           call gslati(lathic,duneh(igr),deff(igr,2),redfac,
     &                 grsize(dmed,igr,1))
   10    continue

c        Calculate composition

         if (nlayer.eq.1) then

            call gscom1(ngrid  ,nfrac ,nbran  ,nnode ,nboun  ,ibr   ,
c                                             <ptrla1>
     &                  branch ,node  ,deltaa ,ptrla ,
c                              <deff1> <deff2>       <wfs>
     &                  p0la   ,deff  ,deff(1,2)     ,afwfqs(1,3)   ,
c                      <ws>                   <ptrla2>
     &                  ws            ,mbdpar ,ptrla(1,1,2)         ,
     &                  nunlay ,nonngp,nvast  ,zbeps ,nrdzdl,
c                      <q2>                          <deltai>
     &                  qp(1,3),deltar,zbave  ,zbfl  ,tmpfr(1,1)    ,
     &                  wfsold ,sedpar,dzr    ,time  ,jugralg       )
         else

            call gscom2(ngrid  ,nfrac  ,nbran ,nnode  ,nboun ,ibr   ,
     &                  sngl(dt)       ,thexfa,sedim  ,fdi   ,pdiacc,
     &                  branch ,node   ,deltaa,
c                      <ptrla1>        <pexla1>
     &                  ptrla          ,pexla ,p0la   ,dunel ,
c                              <deff1> <deff2>        <wfs>
     &                  sedtr  ,deff   ,deff(1,2)     ,afwfqs(1,3)  ,
c                      <ws>
     &                  ws             ,mbdpar        ,
c                      <ptrla2>       <pexla2>
     &                  ptrla(1,1,2)   ,pexla(1,1,2)  ,
c                      <a11>           <a12>          <a21>
     &                  tmpfr(1,1)     ,tmpfr(1,2)    ,tmpfr(1,3)   ,
c                      <a22>           <b1>           <b2>
     &                  tmpfr(1,4)     ,tmpfr(1,5)    ,tmpfr(1,6)   ,
     &                  nunlay ,nonngp ,nvast         ,zbeps        ,
c                                       <q2>
     &                  nrdzdl         ,qp(1,3)       ,deltar       ,
c                                                     <deltai>
     &                  zbave          ,zbfl          ,tmpfr(1,7)   ,
     &                  wfsold         ,sedpar        ,dzr          ,
     &                  time           ,jugralg       )
         if (ker.eq.fatal) goto 100
         endif
c
      if (nunlay .gt. 1) then
c
            call gscom3(ngrid   ,nfrac  ,nbran  ,nnode  ,nboun ,ngrain ,
     &                  submin  ,subplus,ibr    ,
c                                               <ptrla1>      <deff1>
     &                  branch  ,node   ,deltaa ,ptrla  ,p0la  ,deff   ,
c                      <deff2>   <wfs>                     
     &                  deff(1,2),afwfqs(1,3)   ,ws     ,mbdpar        ,
c                      <ptrla2>                 <pexla1><pexla2>
     &                  ptrla(1,1,2)    ,thexfa ,pexla  ,pexla(1,1,2)  ,
     &                  dzr    ,nrdzdl  ,sedpar ,nunlay ,nlayer,nonngp ,
c                      <q2>                     <deltai>
     &                  qp(1,3),wfsold  ,deltar ,tmpfr(1,1)            ,
c                      <deltib>        <deltiz> 
     &                  tmpfr(1,3)     ,tmpfr(1,4)      ,levunl,zbfl   ,
     &                  jugralg,lanrinbt        ,grain  ,ddis  ,dfrac  ,
     &                  grsizmun       ,juer    ,ker    )
c
      endif
   20 continue

      call normp (ptrla(1,1,2),ngrid,nfrac,1.e-6,gperr)
      if (gperr.gt.0) then
         ker = fatal
         call getloc (gperr,ibr,xc)
         write (txtx,'(f10.2)') xc
         call getbrn (ibr,branam,lbrnam)
         call error ( juer,'NORMP Transport layer is empty '//
     +                     'in @'//branam(:lbrnam)//'@ @'//
     +                      txtx//'@' ,egnorm1, ker )
      else if (nlayer .gt. 1) then
         call normp (pexla(1,1,2),ngrid,nfrac,1.e-6,gperr)
         if (gperr.gt.0) then
             ker = fatal
             call getloc (gperr,ibr,xc)
             write (txtx,'(f10.2)') xc
             call getbrn (ibr,branam,lbrnam)
             call error ( juer,'NORMP Exchange layer is empty '//
     +                              'in @'//branam(:lbrnam)//'@ @'//
     +                               txtx//'@' ,egnorm2, ker )
         endif
      endif
      
      if (ker.eq.fatal) goto 100

      do 30 ibr=1,nbran

c        Calculate new characteristic grain sizes

         call gschar (ibr    ,nfrac  ,nlayer ,nbran ,ngrid  ,branch ,
c                                    <ptrla2>       <pexla2>
     &                ddis   ,dfrac  ,ptrla(1,1,2)  ,pexla(1,1,2)   ,
     &                grsize ,dmed0  ,p0la(1,1,1)   ,nrdzdl ,trform ,
     &                sedexp ,nunlay )
c
c        Adapt non moving layer for testing purposes.
c
         if (adnoml .eq. 1) then

            call gsnoml (ibr    ,nfrac  ,nlayer ,nbran  ,ngrid ,branch ,
c                       <ptrla2>        <pexla2>
     &                   ptrla(1,1,2)   ,pexla(1,1,2)   ,p0la  ,grsize ,
     &                   nunlay         ,nrdzdl         )
        endif

   30 continue
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
     &              'P-transport layer',ptrla(1,1,2))
         if (nlayer.eq.2) then
           call gstout (ngrid ,nfrac ,jugraut ,time ,
     &                 'P-exchange layer',pexla(1,1,2))
         endif
      endif
c      
 100  continue     
      end
