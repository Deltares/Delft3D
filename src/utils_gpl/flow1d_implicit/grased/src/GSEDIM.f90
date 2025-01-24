subroutine gsedim (nbran  ,nnode  ,nbrnod ,nboun  ,maxlev ,&
&nqlat  ,nmlat  ,ngrid  ,maxtab ,ntabm  ,&
&nfrac  ,nlayer ,ntmpfr ,juer   ,itim   ,&
&time   ,dt     ,g      ,nunlay ,jugraut,&
&jugralg,branch ,sedinf ,prslot ,psltvr ,&
&disgse ,nonall ,seddb  ,brnode ,bgout  ,&
&sdrdbf ,mbdpar ,ntab   ,gsopts ,afwfqs ,&
&waoft  ,ws     ,hlev   ,nlev   ,wft    ,&
&sectv  ,alfab  ,rp     ,x      ,cp     ,&
&qp     ,hp     ,grsize ,forcon ,source ,&
&sedtr  ,celer  ,trform ,nucoef ,uscoef ,&
&duncon ,sedpar ,morcon ,mltpar ,&
&qltpar ,qlat   ,dfrac  ,pdiacc ,ptrla  ,&
&pexla  ,p0la   ,dmed0  ,depos  ,duneh  ,&
&dunel  ,deff   ,tmpfr  ,table  ,celert ,&
&nrdzdl ,sedexp ,zbave  ,zbfl   ,lastts ,&
&istep  ,ker    )


!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsedim.F,v $
! Revision 1.5  1996/06/07  11:56:19  kuipe_j
! multi  +  fixed layer
!
! Revision 1.4  1996/01/08  13:29:53  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.2  1995/09/27  10:12:27  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer    nbran  ,nnode    ,nbrnod  ,nboun   ,maxlev ,&
   &ntmpfr  ,nqlat   ,nmlat   ,ngrid   ,jugraut,jugralg,&
   &maxtab ,ntabm    ,nfrac   ,nlayer  ,juer   ,ker    ,&
   &nunlay ,istep    ,nucoef
   integer    branch(4,nbran)  ,sedinf(2,nbran)  ,&
   &nonall(3,nbran)  ,&
   &seddb(3,nnode)   ,&
   &brnode(nbrnod+1,nnode)             ,&
   &bgout (3,nbrnod) ,&
   &sdrdbf(2,*)      ,&
   &mbdpar(5,*)      ,&
   &itim  (2)        ,&
   &ntab  (4,maxtab) ,&
   &nlev  (ngrid)    ,&
   &gsopts(*)
   integer    nrdzdl(ngrid)
!
   real       g
   real       afwfqs(ngrid,8 ) ,waoft (ngrid,6)  ,ws    (ngrid)   ,&
   &wft   (ngrid,maxlev)          ,sectv (ngrid,dmsecv) ,&
   &rp    (ngrid,4)  ,x     (ngrid)    ,cp    (ngrid,4) ,&
   &alfab (ngrid)   ,&
   &grsize(4,ngrid,nlayer+1),forcon(nfrac+1,4,nbran)    ,&
   &source(ngrid,nfrac+2)   ,celert(ngrid)              ,&
   &sedtr (ngrid,nfrac+2)   ,celer (ngrid,nfrac+2)      ,&
   &duneh (ngrid)           ,dunel(ngrid)               ,&
   &dmed0 (ngrid)           ,deff (ngrid,2)             ,&
   &zbave (ngrid)           ,zbfl (ngrid)               ,&
   &sedexp(ngrid)           ,&
   &trform(3,nbran)         ,prslot(3,nbran)            ,&
   &disgse(nfrac,2,nbran)   ,tmpfr(nfrac+2,ntmpfr)      ,&
   &ptrla (ngrid,nfrac,2)   ,pexla (ngrid,nfrac,2)      ,&
   &p0la(ngrid,nfrac,nunlay),&
   &pdiacc(nfrac)           ,dfrac(nfrac)
   real       uscoef(nrcoefs,nucoef)  ,&
   &sedpar(*)               ,duncon(5)                  ,&
   &morcon(2+2*nfrac,*)     ,mltpar(9,*)                ,&
   &qltpar(9,*)             ,qlat(*)                    ,&
   &table (ntabm)           ,&
   &psltvr(7,ngrid)
   double precision  time  ,dt,  hlev  (ngrid,maxlev)
   double precision hp(ngrid,3) , qp(ngrid,3)
   logical    lastts
   logical    depos(ngrid)
!
!     Declaration of local parameters
!
   integer    heiopt ,rouopt ,lathic ,extout ,nvast
   real       pacfac ,relden ,kinvis ,fdi ,&
   &alffl  ,redfac
   real       duncof(3)
   character  txt*18
   logical    yesout
!
   ker = ok
!
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
!
!     Lateral sediment is calculated
!
   call gslsed (ngrid  ,nqlat  ,nmlat  ,nbran  ,nfrac  ,maxtab,&
   &ntabm  ,time   ,dt     ,branch ,x      ,&
   &qltpar ,mltpar ,qlat   ,ntab   ,table  ,&
   &source ,dfrac  ,sedtr  )
!
   call gsnetw (nbran  ,ngrid  ,maxlev ,nfrac  ,nlayer ,nunlay,&
   &heiopt ,lathic ,rouopt ,juer   ,jugralg,fdi   ,&
   &g      ,pacfac ,relden ,kinvis ,redfac ,nvast ,&
   &alffl  ,nonall ,branch ,sedinf ,duncof ,pdiacc,&
   &dfrac  ,duneh  ,dunel  ,nucoef ,uscoef ,&
!                                         <af>       <afs>
   &trform ,prslot ,psltvr ,waoft(1,3),afwfqs(1,1),&
!                       <Wf>      <Wfs>
   &waoft(1,1)     ,afwfqs(1,3)    ,nlev   ,hlev  ,&
!                                 <secths>        <Rs>
   &wft    ,ws     ,sectv(1,8)     ,rp(1,2),&
!                 <Cs>            <Q2>    <Qs>           <h2>
   &cp(1,2),alfab  ,qp(1,3),afwfqs(1,7)   ,hp(1,3),&
!                                                 <ptrla2>
   &grsize ,dmed0  ,x      ,source ,ptrla(1,1,2)  ,&
!                 <pexla2>                <deff2>
   &pexla(1,1,2)   ,p0la   ,deff(1,2)      ,forcon,&
   &duncon ,tmpfr  ,sedtr  ,depos  ,celer  ,celert,&
!                 <asubsc>
   &sectv(1,1)     ,nrdzdl ,sedexp ,zbave  ,zbfl  ,&
   &ker            )
!
   if (ker .eq. fatal) goto 1000
!
!     Non-alluvial layers are not implemented
!
!     Nodes are treated before boundaries as array DISGSE is
!     initialized in GSNODE.
!
   call gsnode (nnode  ,nbran  ,nbrnod ,ngrid  ,maxtab ,ntabm ,&
   &nfrac  ,branch ,brnode ,bgout  ,sedinf,sdrdbf ,&
!                                         <q2>
   &seddb  ,ntab   ,morcon ,qp(1,3),ws    ,table  ,&
!                         <stotfr>
   &sedtr  ,tmpfr(1,1)     ,disgse ,trform)
!
   call gsboun (nnode  ,nboun  ,nbran  ,ngrid  ,nbrnod ,maxtab ,&
   &ntabm  ,nfrac  ,time   ,&
!                 <q2>
   &qp(1,3),sedtr  ,mbdpar ,branch ,brnode ,ntab   ,&
   &table  ,disgse )
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
      &'Sediment transport ',sedtr )
   endif
!
   return
!
1000 continue
!
   write (txt,'(2(1x,i8))') itim
   call error (juer,'GSEDT timestep@'//txt//'@',esemes,info)
!
end
