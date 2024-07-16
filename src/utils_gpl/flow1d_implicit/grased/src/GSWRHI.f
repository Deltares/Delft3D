      subroutine gswrhi (nseman ,nsemap ,nsetim ,ngrid ,nlayer ,ngrain ,
     &                   submin ,subplus,itim   ,istep ,nstep  ,sedmap ,
     &                   sedtim ,sedtr  ,grsize ,grain ,grsizmun       ,
     &                   deff   ,secpre ,dt     ,dzunla,dzr    ,levunl ,
     &                   nrdzdl ,sedini ,buffer ,gridnm,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         A.W.J.Koster
c
c Module:             SEWRHI(SEdiment WRite sediment HIS files)
c
c Module description: Writing of user defined sediment results to the
c                     HIS file.
c
c                     The user selected sediment results at user sup-
c                     plied locations and time levels will be stored on
c                     the result file. The stored data can be processed
c                     further by the User Interface.
c
c                     The user can select functions of place and of
c                     time. Writing can start on a new file or in case
c                     the data model is unchanged an existing file can
c                     be extended.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 19 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c  7 istep             I  Current time step number (t(n+1)).
c  6 itim              P  -
c  4 ngrid             I  Number of grid points in network.
c  5 nsedrd            I  Number of defined sedredge branches.
c  1 nseman            I  Number of main codes of sediment results.
c  2 nsemap            I  Number of entries in sedmap.
c  3 nsetim            I  Number of entries in sedtim.
c 18 secpre(nseman)    I  secpre(i) = index in block tabel (1..nentri)
c                         for main code i of sediment results.
c 14 sedmap(nsemap)    I  Parameter list for MAP block with sediment
c                         results:
c                         (1)      = Report begin time
c                         (2)      = Report end time
c                         (3)      = Report time step
c                         (4)      = Report parameter 1 main code
c                         (5)      = Report parameter 1 sub code
c                         (i)      = Report parameter 1 main code
c                         (i+1)    = Report parameter 1 sub code
c                         (nsemap) = Report parameter n sub code
c 15 sedtim(nsetim)    I  Parameter list for HIST block with sediment
c                         results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nsetim) = Report parameter n sub code
c 16 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sewrhi.pf,v $
c Revision 1.3  1998/02/13  12:12:57  kuipe_j
c Adapt to CMT
c
c Revision 1.2  1997/06/17  11:54:32  kuipe_j
c Add CVS keys
c
c 
c***********************************************************************
c
      include '..\include\filsim.i'
      include '..\include\sobdim.i'
      include '..\include\errcod.i'
c
c     Declaration of parameters
c
      integer      nentri    ,main1unla 
      parameter   (nentri=19 ,main1unla=4 )
c     
      integer      nseman ,nsemap ,nsetim ,ngrid  ,nlayer  ,istep  ,
     &             nstep  ,ngrain ,submin ,subplus,juer    ,ker    
      integer      sedmap(nsemap) ,sedtim(nsetim) ,secpre(nseman)  ,
     &             nrdzdl(ngrid)  ,itim  (2)      ,
     &             sedini(*)      ,grain(4)
      real         dzunla
      real         sedtr (ngrid)  ,buffer(dmbuffer,ngrid)          ,
     &             deff  (ngrid,2),grsize(4,ngrid,*)               ,
     &             dzr   (ngrid)  ,levunl(ngrid)                   ,
     &             grsizmun       (ngrid,ngrain,submin:subplus) 
      double       precision dt
      character*40 gridnm(*)
c
c     Declaration of local variables
c
      integer      i     ,j      ,ie    ,ie1   ,k     ,isub   ,sub    ,
     &             nlc   ,nsk    ,imain ,main  ,entri1unla    ,mul    ,
     &             ijaar ,imaand ,idag  ,iuur  ,imin  ,isec   ,iscu   ,
     &             lun   ,ivar   ,nvar  ,igrid ,ifil  ,istepf ,istphf ,
     &             kgrain,klay   ,lay   ,iv
      double precision    scudt
      character*40 idmap (nentri) ,idhis(nentri)
      character*20 parnam(nentri)  
      logical      new   ,newuit
      save         istepf,istphf  ,scudt
c
c     Declaration of external functions
c
      real         topunl
      logical      yesmap
      external     yesmap, topunl
c
c     Initialize.
c
      idmap(1)  = 'SOBEK                                   '
      idmap(2)  = 'Maps results at gridpoints              '
      idmap(3)  = '                                        '
      idmap(4)  = '                                        '
      parnam(1) = 'Sediment transport'       
      parnam(2) = 'Grain size D10'           
      parnam(3) = 'Grain size D50'           
      parnam(4) = 'Grain size D90'           
      parnam(5) = 'Grain size Dmed'          
      parnam(6) = 'Grn size D10 ex-lay' 
      parnam(7) = 'Grn size D50 ex-lay' 
      parnam(8) = 'Grn size D90 ex-lay' 
      parnam(9) = 'Grn size Dmed exlay'
      parnam(10)= 'Grn size D10 un-lay' 
      parnam(11)= 'Grn size D50 un-lay' 
      parnam(12)= 'Grn size D90 un-lay' 
      parnam(13)= 'Grn size Dmed un-lay'
      parnam(14)= 'Eff layer thickness'
      parnam(15)= 'Top of under layer'
      parnam(16)= 'Grn Sz D10 U0 Lay' 
      parnam(17)= 'Grn Sz D50 U0 Lay'
      parnam(18)= 'Grn Sz D90 U0 Lay'
      parnam(19)= 'Grn Sz Dmed U0 Lay'
      idhis(1)  = 'SOBEK                                   '
      idhis(2)  = 'History results at gridpoints           '
      idhis(3)  = '                                        '
      idhis(4)  = '                                        '
c
c     Write Map results
c
      if (nsemap .gt. 3) then
         if (yesmap(sedmap(1),sedmap(2),sedmap(3),istep)) then
            ifil = 1 
            lun  = 110
            if ( sedini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = gsedmap , form = 'binary')      
#else
#if defined (USE_HPUX)
               open(lun , file = gsedmap , form = 'unformatted') 
#else
               open(lun , file = gsedmap , form = 'unformatted') 
#endif
#endif
               nvar = 0
               do imain = 4,nsemap,2
                  main = sedmap(imain)
                  if (main.lt.main1unla) then
                     nvar = nvar + 1
                  else
                     nvar = nvar + sedmap(imain+1)
                  endif
               enddo
               if (nvar.gt.dmbuffer) goto 9000
               iscu = nstep*dt/1.0d9+1.d0 
               scudt  = dt/dble(iscu) 
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idmap(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idmap(i) , i = 1,4)
               write(lun) nvar,ngrid
               entri1unla = secpre(main1unla)
               do imain = 4,nsemap,2
                  main = sedmap(imain)
                  if (main.lt.main1unla) then
                     write(lun) parnam(secpre(main)+sedmap(imain+1))
                  else if (main.lt.main1unla+4) then
                     write(lun) parnam(entri1unla+main-main1unla)
                  else
                     call gsfnam (imain, sedmap, 
     +                    parnam(entri1unla), main1unla, lun)
                  endif
               enddo
               write(lun) (igrid , gridnm(igrid)(:20) ,
     +                     igrid = 1 , ngrid )
               sedini(ifil) = 1
               istepf       = istep
            endif
c
            ivar = 0
            do i = 4,nsemap,2
              main = sedmap(i)
              sub  = sedmap(i+1)
              if (main.lt.main1unla) then
                ivar = ivar + 1
                ie = secpre(main)+sub
c
                if (ie.eq.1) then
                  do igrid=1,ngrid
                     buffer(ivar,igrid)=sedtr(igrid)
                  enddo
                else if (ie.lt.14) then
                  if (ie .gt. 9) then
                     ie1 = ie - 9
                     k   = nlayer+1        
                  else if (ie .gt. 5) then
                     ie1 = ie - 5
                     k   = 2
                  else
                     ie1 = ie - 1
                     k   = 1
                  endif
                  do igrid=1,ngrid
                     buffer(ivar,igrid) = grsize(ie1,igrid,k)
                  enddo
                else if (ie.eq.14) then
                  do igrid=1,ngrid
                     buffer(ivar,igrid) = deff(igrid,2)
                  enddo
                else if (ie.eq.15) then
                  do igrid=1,ngrid
                     buffer(ivar,igrid) = topunl (igrid ,
     &                      dzr ,levunl ,nrdzdl ,ngrid ,dzunla)
                  enddo
                endif
              else
                kgrain = grain(mod(main-main1unla,4)+1)
                klay   = (main-main1unla)/4
                if (klay.eq.0) then
                   mul = 0
                else if (klay.eq.1) then
                   mul = 1
                else if (klay.eq.2) then
                   mul = -1
                endif
                do isub=1,sub
                   ivar = ivar + 1
                   lay = mul * isub
                   do igrid=1,ngrid
                      buffer(ivar,igrid)= grsizmun(igrid,kgrain,lay) 
                   enddo
                enddo 
              endif
              
            enddo
            write(lun) nint((istep-istepf)*scudt),
     +                 ((buffer(iv,igrid),
     +                 iv = 1 , ivar),
     +                 igrid = 1 , ngrid)
         endif
      endif
c
c     Write History results.
c
      nlc = sedtim(1)
      new = mod(nsetim-nlc,2) .eq. 0
      if (new) then 
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nsetim .gt. 1+nsk) then
         if (new) then
            newuit = yesmap(sedtim(nlc+2),sedtim(nlc+3),sedtim(nlc+4),
     &                      istep)
         else
            newuit = .false.
         endif
         if ( (sedtim(1).gt.0 .and. .not. new) .or. newuit ) then
            ifil = 2
            lun  = 111
            if ( sedini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = gsedhis , form = 'binary')      
#else
#if defined (USE_HPUX)
               open(lun , file = gsedhis , form = 'unformatted') 
#else
               open(lun , file = gsedhis , form = 'unformatted') 
#endif
#endif
               nvar = 0
               do imain = sedtim(1)+2+nsk,nsetim,2
                  main = sedtim(imain)
                  if (main.lt.main1unla) then
                     nvar = nvar + 1
                  else
                     nvar = nvar + sedtim(imain+1)
                  endif
               enddo
               if (nvar.gt.dmbuffer) goto 9000
               iscu = nstep*dt/1.0d9+1.d0 
               scudt  = dt/dble(iscu) 
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idhis(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idhis(i) , i = 1,4)
               write(lun) nvar , sedtim(1)
               entri1unla = secpre(main1unla)
               do imain = sedtim(1)+2+nsk,nsetim,2
                  main = sedtim(imain)
                  if (main.lt.main1unla) then
                     write(lun) parnam(secpre(main)+sedtim(imain+1))
                  else if (main.lt.main1unla+4) then
                     write(lun) parnam(entri1unla+main-main1unla)
                  else
                     call gsfnam (imain, sedtim, 
     +                    parnam(entri1unla), main1unla, lun)
                  endif
               enddo
               write(lun) (sedtim(igrid),gridnm(sedtim(igrid))(:20),
     +                     igrid = 2 , sedtim(1)+1)
               sedini(ifil) = 1
               istphf       = istep 
            endif
c
            ivar = 0
            do i = sedtim(1)+2+nsk,nsetim,2
              main = sedtim(i)
              sub  = sedtim(i+1)
              if (main.lt.main1unla) then 
                ivar = ivar + 1
                ie = secpre(sedtim(i))+sedtim(i+1)
c
                if (ie.eq.1) then
                  do j=1,sedtim(1)
                     buffer(ivar,j)=sedtr(sedtim(j+1))
                  enddo
                else if (ie.lt.14) then
                  if (ie .gt. 9) then
                     ie1 = ie - 9
                     k   = nlayer+1   
                  else if (ie .gt. 5) then
                     ie1 = ie - 5
                     k   = 2
                  else
                     ie1 = ie - 1
                     k   = 1
                  endif
                  do j=1,sedtim(1)
                     buffer(ivar,j) = grsize(ie1,sedtim(j+1),k)
                  enddo
                else if (ie.eq.14) then
                  do j=1,sedtim(1)
                     buffer(ivar,j) = deff(sedtim(j+1),2)
                  enddo
                else if (ie.eq.15) then
                  do j=1,sedtim(1)
                     buffer(ivar,j) = topunl (sedtim(j+1) , 
     &                      dzr ,levunl ,nrdzdl ,ngrid ,dzunla)
                  enddo
                endif  
              else
                 kgrain = grain(mod(main-main1unla,4)+1)
                 klay   = (main-main1unla)/4
                 if (klay.eq.0) then
                    mul = 0
                 else if (klay.eq.1) then
                    mul = 1
                 else if (klay.eq.2) then
                    mul = -1
                 endif
                 do isub=1,sub
                    ivar = ivar + 1
                    lay = mul * isub
                    do j=1,sedtim(1)
                       buffer(ivar,j)= grsizmun(sedtim(j+1),kgrain,lay) 
                    enddo
                 enddo 
              endif                  
            enddo
            write(lun) nint((istep-istphf)*scudt),
     +                 ((buffer(iv,j),
     +                  iv = 1,ivar),
     +                  j    = 1,sedtim(1))
            
         endif
      endif
      return

 9000 continue      
      ker   = fatal
      call error (juer ,'GSWRHI Too many output functions',egoutm,ker)    
c
 1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,
     +        '  (scu=',i8,'s)')
c
      end
      
      real function topunl (igr ,dzr ,levunl ,nrdzdl ,ngrid ,dzunla)
c
c     Calculate top of under layer
c
c
c     Declaration of parameters

      integer   igr   ,ngrid
      integer   nrdzdl(ngrid)
      real      dzunla  
      real      dzr   (ngrid) ,levunl(ngrid)       
      
      topunl = levunl(igr) + (nrdzdl(igr)-1)*dzunla + dzr(igr)

      end
      
      subroutine gsfnam (imain, timmap, parnam, main1unla, lun)
c
c     Make and write function name
c
c     Declaration of parameters
c
      integer            imain, lun, main1unla
      integer            timmap(*)
      character*20       parnam(*) 
c
c     Declaration of local variables
c
      integer            main, nsub, sub, indh, inddig, klay,
     &                   kgrain
      character*25       partxt
      character*5        numtxt
      character*1        teken         
c    
      main = timmap(imain)-main1unla    
      nsub = timmap(imain+1)
      klay = main/4
      if (klay .eq. 1) then
         teken = '+'
      else if (klay .eq. 2) then  
         teken = '-'
      else   
         teken = '0'   
      endif
      kgrain = mod(main,4)+1
      do sub=1,nsub
         partxt = parnam(kgrain)
         indh   = index (partxt,'U0')+1
         write (numtxt,'(i4,1x)') sub
         inddig = 1
         do while (numtxt(inddig:inddig).eq.' ')
            inddig = inddig + 1
         enddo         
         partxt(indh:) = teken//numtxt(inddig:5)//'Lay'
         write(lun) partxt(1:20)
      enddo
      end
