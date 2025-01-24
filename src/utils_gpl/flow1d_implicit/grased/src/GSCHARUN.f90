subroutine gscharun (laynr1 ,laynr2 ,nfrac  ,nbran   ,ngrid  ,&
&ngrain ,submin ,subplus,nunlay  ,grain  ,&
&branch ,ddis   ,dfrac  ,p0la    ,nrdzdl ,&
&lanrinbt       ,grsizmun        )
!
!     Graded Sediment calculate CHARacteristic grain sizes in underlayer
!
!     Calculate characteristic grain sizes, i.e. d50 ,d90 etc.
!     for transport layer.
!
!     Declaration of parameters
!
   integer    laynr1  ,laynr2    ,nfrac   ,nbran  ,ngrid   ,ngrain  ,&
   &submin  ,subplus   ,nunlay
   integer    branch  (4,nbran)           ,grain(4)       ,&
   &nrdzdl  (ngrid)             ,lanrinbt(ngrid)
   real       ddis    (nfrac+1)           ,dfrac(nfrac)   ,&
   &p0la    (ngrid,nfrac,nunlay),&
   &grsizmun(ngrid,ngrain,submin:subplus)
!
!     Declaration of local parameters
!
   integer    ibr     ,igr  ,i  ,laygu, laypu, toplay, kgrain,&
   &lay1    ,lay2 ,igrain
   real       per(3)
!
!     Constants
!
!     Store d-10 at d-35
!
   integer     dmed
   parameter  (dmed=4)
   real        p10     ,p50     ,p90
   parameter  (p10=0.1 ,p50=0.5 ,p90=0.9)
!
!     Calculate characteristic grain sizes, i.e. d50 ,d90 etc.
!     for transport layer.
!
   per(1) = p10
   per(2) = p50
   per(3) = p90
!
   lay1 = min(laynr1,laynr2)
   lay2 = max(laynr1,laynr2)
   lay1 = max(lay1,submin)
   lay2 = min(lay2,subplus)
   do laygu = lay1,lay2
      do ibr=1,nbran
         do igr = branch(3,ibr),branch(4,ibr)
            laypu  = lanrinbt(igr) + laygu
            toplay = nrdzdl(igr)
            if (laypu.gt.toplay .or. laypu.lt.1) then
!
!                 Set characteristic grain zise to zero for
!                 non existing sub under layers
!
               do i=1,ngrain
                  grsizmun(igr,i,laygu) = 0.
               enddo
            else
               do kgrain=1,4
                  igrain = grain(kgrain)
                  if (igrain .gt. 0) then
                     if (kgrain .eq. dmed) then
!
!                          Calculate d-med
!
                        call gsdmed(ngrid   ,nfrac ,igr  ,dfrac ,&
                        &p0la    (1,1,laypu)      ,&
                        &grsizmun(igr,igrain,laygu)  )
!
                     else
!
!                          Calculate other characteristic grain sizes
!
                        call gschad(ngrid     ,nfrac   ,igr     ,&
                        &per(kgrain)        ,ddis    ,&
                        &p0la(1,1,laypu),&
                        &grsizmun(igr,igrain,laygu)  )
!
                     endif
                  endif
               enddo
            endif
         enddo
      enddo
   enddo

end
