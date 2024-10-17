subroutine gscomp (ngrid  ,nfrac  ,nbran  ,nnode  ,nboun  ,nlayer,&
&maxtab ,ntabm  ,ngrain ,submin ,subplus,time  ,&
&dt     ,g      ,nunlay ,jugraut,jugralg,&
&gsopts ,branch ,node   ,mbdpar ,sedpar ,deltaa,&
&cp     ,qp     ,afwfqs ,ntab   ,table  ,grain ,&
&ddis   ,dfrac  ,grsizmun       ,sedtr  ,trform,&
&duncon ,p0la   ,pdiacc ,tmpfr  ,duneh  ,dunel ,&
&deff   ,ptrla  ,pexla  ,grsize ,dmed0  ,dzr   ,&
&nrdzdl ,ws     ,wfsold ,deltar ,zbave  ,zbfl  ,&
&sedexp ,levunl ,lastts ,istep  ,lanrinbt      ,&
&juer   ,ker    )
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gscomp.F,v $
! Revision 1.4  1996/06/07  11:55:57  kuipe_j
! multi  +  fixed layer
!
! Revision 1.3  1996/01/08  13:29:49  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.2  1995/09/27  10:11:58  kuipe_j
! Maintenance
!
!
!***********************************************************************
!     Graded Sediment calculate COMPosition of layers

!     Declaration of parameters
!
   integer    ngrid ,nfrac  ,nbran   ,nnode ,nboun  ,nlayer ,maxtab ,&
   &ntabm ,juer   ,nunlay  ,istep ,jugraut,jugralg,ker    ,&
   &ngrain,submin ,subplus
   integer    branch(4,nbran)        ,ntab  (4,maxtab)      ,&
   &mbdpar(5,nboun)        ,node  (4,nnode)       ,&
   &nrdzdl(ngrid)          ,lanrinbt(ngrid)       ,&
   &gsopts(*)              ,grain(4)
   real       g
   real       sedtr (ngrid,nfrac+2)  ,&
   &p0la  (ngrid,nfrac,nunlay)                    ,&
   &grsizmun(ngrid,ngrain,submin:subplus)         ,&
   &ptrla (ngrid,nfrac,2)  ,pexla (ngrid,nfrac,2) ,&
   &deltar(ngrid,nfrac)   ,&
   &grsize(4,ngrid,nlayer+1),&
   &cp    (ngrid,4)        ,deff  (ngrid,2)       ,&
   &afwfqs(ngrid,8)        ,duneh (ngrid)         ,&
   &dunel (ngrid)          ,dmed0 (ngrid)         ,&
   &trform(3,nbran)        ,tmpfr (nfrac+2,*)     ,&
   &ddis  (nfrac+1)        ,dfrac (nfrac)         ,&
   &table (ntabm)          ,&
   &sedpar(*)              ,duncon(*)             ,&
   &pdiacc(nfrac)          ,levunl(ngrid)         ,&
   &zbave (ngrid)          ,zbfl  (ngrid)         ,&
   &ws    (ngrid)          ,dzr   (ngrid)         ,&
   &wfsold(ngrid)          ,sedexp(ngrid)
   double precision   time           ,dt                    ,&
   &deltaa(ngrid,nfrac+1)
   double precision  qp(ngrid,3)
   logical    lastts
!
   include '..\include\errcod.i'
!
!     Declaration of local parameters
!
   integer    ibr    ,igr    ,lathic ,heiopt ,lenopt ,adnoml ,&
   &extout ,nonngp ,nvast  ,lbrnam ,gperr
   real       relden ,kinvis ,thexfa ,fdi    ,zbeps  ,redfac
   real       xc     ,duncof (3)
   logical    initra ,sedim  ,yesout
   character*10       txtx
   character*40       branam
!
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
!
   heiopt    = gsopts(1)
   lenopt    = gsopts(2)
   lathic    = gsopts(4)
   adnoml    = gsopts(5)
   extout    = gsopts(6)
   sedim     = gsopts(7).eq.1
   nonngp    = gsopts(8)
   nvast     = gsopts(9)
!
   initra    = .false.

   call gscbou (nboun ,nbran  ,ngrid  ,maxtab ,ntabm  ,nfrac  ,&
!                                <q2>
   &nlayer,time   ,qp(1,3),mbdpar ,branch ,ntab   ,&
!                        <ptrla2>        <pexla2>
   &table ,ptrla(1,1,2)   ,pexla(1,1,2)   )

   do 20 ibr=1,nbran

!        Calculate dune height and length

      call gsdune (ngrid  ,nbran  ,ibr    ,initra ,heiopt ,lenopt ,&
!                                                            <cs>
      &g      ,relden ,kinvis ,branch ,grsize ,cp(1,2),&
!                    <qs>            <afs>           <wfs>
      &afwfqs(1,7)    ,afwfqs(1,1)    ,afwfqs(1,3)    ,&
      &duncof ,trform ,duncon ,sedexp ,duneh  ,dunel  )

!        Calculate layer thickness

      do 10 igr = branch(3,ibr),branch(4,ibr)
!                                        <deff2(igr)>
         call gslati(lathic,duneh(igr),deff(igr,2),redfac,&
         &grsize(dmed,igr,1))
10    continue

!        Calculate composition

      if (nlayer.eq.1) then

         call gscom1(ngrid  ,nfrac ,nbran  ,nnode ,nboun  ,ibr   ,&
!                                             <ptrla1>
         &branch ,node  ,deltaa ,ptrla ,&
!                              <deff1> <deff2>       <wfs>
         &p0la   ,deff  ,deff(1,2)     ,afwfqs(1,3)   ,&
!                      <ws>                   <ptrla2>
         &ws            ,mbdpar ,ptrla(1,1,2)         ,&
         &nunlay ,nonngp,nvast  ,zbeps ,nrdzdl,&
!                      <q2>                          <deltai>
         &qp(1,3),deltar,zbave  ,zbfl  ,tmpfr(1,1)    ,&
         &wfsold ,sedpar,dzr    ,time  ,jugralg       )
      else

         call gscom2(ngrid  ,nfrac  ,nbran ,nnode  ,nboun ,ibr   ,&
         &sngl(dt)       ,thexfa,sedim  ,fdi   ,pdiacc,&
         &branch ,node   ,deltaa,&
!                      <ptrla1>        <pexla1>
         &ptrla          ,pexla ,p0la   ,dunel ,&
!                              <deff1> <deff2>        <wfs>
         &sedtr  ,deff   ,deff(1,2)     ,afwfqs(1,3)  ,&
!                      <ws>
         &ws             ,mbdpar        ,&
!                      <ptrla2>       <pexla2>
         &ptrla(1,1,2)   ,pexla(1,1,2)  ,&
!                      <a11>           <a12>          <a21>
         &tmpfr(1,1)     ,tmpfr(1,2)    ,tmpfr(1,3)   ,&
!                      <a22>           <b1>           <b2>
         &tmpfr(1,4)     ,tmpfr(1,5)    ,tmpfr(1,6)   ,&
         &nunlay ,nonngp ,nvast         ,zbeps        ,&
!                                       <q2>
         &nrdzdl         ,qp(1,3)       ,deltar       ,&
!                                                     <deltai>
         &zbave          ,zbfl          ,tmpfr(1,7)   ,&
         &wfsold         ,sedpar        ,dzr          ,&
         &time           ,jugralg       )
         if (ker.eq.fatal) goto 100
      endif
!
      if (nunlay .gt. 1) then
!
         call gscom3(ngrid   ,nfrac  ,nbran  ,nnode  ,nboun ,ngrain ,&
         &submin  ,subplus,ibr    ,&
!                                               <ptrla1>      <deff1>
         &branch  ,node   ,deltaa ,ptrla  ,p0la  ,deff   ,&
!                      <deff2>   <wfs>
         &deff(1,2),afwfqs(1,3)   ,ws     ,mbdpar        ,&
!                      <ptrla2>                 <pexla1><pexla2>
         &ptrla(1,1,2)    ,thexfa ,pexla  ,pexla(1,1,2)  ,&
         &dzr    ,nrdzdl  ,sedpar ,nunlay ,nlayer,nonngp ,&
!                      <q2>                     <deltai>
         &qp(1,3),wfsold  ,deltar ,tmpfr(1,1)            ,&
!                      <deltib>        <deltiz>
         &tmpfr(1,3)     ,tmpfr(1,4)      ,levunl,zbfl   ,&
         &jugralg,lanrinbt        ,grain  ,ddis  ,dfrac  ,&
         &grsizmun       ,juer    ,ker    )
!
      endif
20 continue

   call normp (ptrla(1,1,2),ngrid,nfrac,1.e-6,gperr)
   if (gperr.gt.0) then
      ker = fatal
      call getloc (gperr,ibr,xc)
      write (txtx,'(f10.2)') xc
      call getbrn (ibr,branam,lbrnam)
      call error ( juer,'NORMP Transport layer is empty '//&
      &'in @'//branam(:lbrnam)//'@ @'//&
      &txtx//'@' ,egnorm1, ker )
   else if (nlayer .gt. 1) then
      call normp (pexla(1,1,2),ngrid,nfrac,1.e-6,gperr)
      if (gperr.gt.0) then
         ker = fatal
         call getloc (gperr,ibr,xc)
         write (txtx,'(f10.2)') xc
         call getbrn (ibr,branam,lbrnam)
         call error ( juer,'NORMP Exchange layer is empty '//&
         &'in @'//branam(:lbrnam)//'@ @'//&
         &txtx//'@' ,egnorm2, ker )
      endif
   endif

   if (ker.eq.fatal) goto 100

   do 30 ibr=1,nbran

!        Calculate new characteristic grain sizes

      call gschar (ibr    ,nfrac  ,nlayer ,nbran ,ngrid  ,branch ,&
!                                    <ptrla2>       <pexla2>
      &ddis   ,dfrac  ,ptrla(1,1,2)  ,pexla(1,1,2)   ,&
      &grsize ,dmed0  ,p0la(1,1,1)   ,nrdzdl ,trform ,&
      &sedexp ,nunlay )
!
!        Adapt non moving layer for testing purposes.
!
      if (adnoml .eq. 1) then

         call gsnoml (ibr    ,nfrac  ,nlayer ,nbran  ,ngrid ,branch ,&
!                       <ptrla2>        <pexla2>
         &ptrla(1,1,2)   ,pexla(1,1,2)   ,p0la  ,grsize ,&
         &nunlay         ,nrdzdl         )
      endif

30 continue
!
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
      call gstout (ngrid ,nfrac ,jugraut ,time ,&
      &'P-transport layer',ptrla(1,1,2))
      if (nlayer.eq.2) then
         call gstout (ngrid ,nfrac ,jugraut ,time ,&
         &'P-exchange layer',pexla(1,1,2))
      endif
   endif
!
100 continue
end
