      subroutine sewrhi(nseman ,nsemap ,nsetim ,ngrid  ,nbran  ,nsedrd ,
     &                  itim   ,istep  ,nstep  ,sedmap ,sedtim ,branch ,
     &                  sedtr  ,q2s    ,afs    ,cs     ,trform ,grsize ,
     &                  secpre ,dt     ,delta  ,g      ,sedini ,buffer ,
     &                  gridnm )

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
c
c     Declaration of parameters
c
      integer      nentri
      parameter   (nentri=6)
      integer      nseman  ,nsemap ,nsetim ,ngrid ,istep  ,nstep,
     &             nsedrd  ,nbran
      integer      sedmap(nsemap),
     &             sedtim(nsetim)  ,secpre(nseman)  ,
     &             itim  (2)       ,branch(4,nbran) ,
     &             sedini(*)
      real         delta ,g
      real         sedtr (ngrid,*) ,buffer(nentri,ngrid), 
     &             q2s   (ngrid,2) ,afs   (ngrid,2)     ,
     &             cs    (ngrid,3) ,grsize(4,ngrid,*)   ,
     &             trform(3,nbran)
      double       precision dt
      character*40 gridnm(*)

c     Declaration of local variables
c
      integer      i      ,j     ,ie       ,ind   ,ibr ,
     &             nlc    ,nsk
      integer      ijaar ,imaand ,idag ,iuur ,imin ,isec, iscu
      integer      lun   ,ivar   ,nvar ,igrid,ifil ,istepf, istphf
      double precision    scudt
      character*40 idmap(4)      , idhis(4)
      character*20 parnam(nentri)  
      logical      new           , newuit
      save         istepf, istphf, scudt
c
c     Declaration of external functions
c
      real         shields ,transport 
      logical      yesmap
      external     yesmap ,shields ,transport 
c
c     Initialize.
c
      idmap(1)  = 'SOBEK                                   '
      idmap(2)  = 'Maps results at gridpoints              '
      idmap(3)  = '                                        '
      idmap(4)  = '                                        '
      parnam(1) = 'Sed transport       '
      parnam(2) = 'Sed transport left  '
      parnam(3) = 'Sed transport right '
      parnam(4) = 'Sed transport exch. '
      parnam(5) = 'Shields parameter   '
      parnam(6) = 'Transport parameter '
      idhis(1)  = 'SOBEK                                   '
      idhis(2)  = 'History results at gridpoints           '
      idhis(3)  = '                                        '
      idhis(4)  = '                                        '
c
c     Write Map results
c
      if (nsemap .gt. 3) then
         if (yesmap(sedmap(1),sedmap(2),sedmap(3),istep)) then
            nvar = nint(0.5*(nsemap-3))
            ifil = 1 
            lun  = 56
            if ( sedini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = sdtmap , form = 'binary')      
#else
#if defined (USE_HPUX)
               open(lun , file = sdtmap , form = 'unformatted') 
#else
               open(lun , file = sdtmap , form = 'unformatted') 
#endif
#endif
               iscu = nstep*dt/1.0d9+1.d0 
               scudt  = dt/dble(iscu) 
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idmap(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idmap(i) , i = 1,4)
               write(lun) nvar,ngrid
               write(lun) ( parnam(secpre(sedmap(i))+
     +                      sedmap(i+1)) ,  i = 4,nsemap,2 )
               write(lun) (igrid , gridnm(igrid)(:20) ,
     +                     igrid = 1 , ngrid )
               sedini(ifil) = 1
               istepf       = istep
            endif
c
            ivar = 0
            do 50 i = 4,nsemap,2
               ivar = ivar + 1
               ie = secpre(sedmap(i))+sedmap(i+1)
c
               if (ie.eq.1) then
                  if (nsedrd .eq. 0) then
c
c                    There are no Sedredge branches.
c
                     do 20 igrid=1,ngrid
                        buffer(ivar,igrid)=sedtr(igrid,1)
   20                continue
c
                  else 
c
c                    Sedredge branches are present. The total transport
c                    must be calculated.
c
                     do 30 igrid =1,ngrid
                        buffer(ivar,igrid) =
     +                              sedtr(igrid,1) + sedtr(igrid,2)
   30                continue
                  endif
               else if (ie.eq.5) then
c
                  do ibr = 1,nbran 
                     do igrid = branch(3,ibr),branch(4,ibr)
                        buffer(ivar,igrid) = 
     +                  shields (q2s(igrid,1),afs(igrid,1),cs(igrid,1),
     +                           grsize(1,igrid,1),delta,trform(1,ibr))
                     enddo
                  enddo
               else if (ie.eq.6) then
c
                  do ibr = 1,nbran
                     do igrid = branch(3,ibr),branch(4,ibr)
                        buffer(ivar,igrid) =                
     +                  transport (sedtr(igrid,1),grsize(1,igrid,1),
     +                             delta ,g ,trform(1,ibr))
                     enddo
                  enddo     
               else if (ie.gt.1) then
c
                     do 40 igrid=1,ngrid
                        buffer(ivar,igrid) = sedtr(igrid,ie-1)
   40                continue
               endif
   50       continue
            write(lun) nint((istep-istepf)*scudt),
     +                 ((buffer(ivar,igrid),
     +                 ivar = 1 , nvar),
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
            nvar = nint(.5*(nsetim-sedtim(1)-2-nsk+1))
            ifil = 2
            lun  = 57
            if ( sedini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = sdthis , form = 'binary')      
#else
#if defined (USE_HPUX)
               open(lun , file = sdthis , form = 'unformatted') 
#else
               open(lun , file = sdthis , form = 'unformatted') 
#endif
#endif
               iscu = nstep*dt/1.0d9+1.d0 
               scudt  = dt/dble(iscu) 
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idhis(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idhis(i) , i = 1,4)
               write(lun) nvar , sedtim(1)
               write(lun) (parnam(secpre(sedtim(i))+sedtim(i+1)),
     +                     i = sedtim(1)+2+nsk,nsetim,2)
               write(lun) (sedtim(igrid),gridnm(sedtim(igrid))(:20),
     +                     igrid = 2 , sedtim(1)+1)
               sedini(ifil) = 1
               istphf       = istep 
            endif
c
            ivar = 0
            do 100 i = sedtim(1)+2+nsk,nsetim,2
               ivar = ivar + 1
               ie = secpre(sedtim(i))+sedtim(i+1)
               if (ie.eq.1) then
                  if (nsedrd .eq. 0) then
c
c                    There are no Sedredge branches.
c
                     do 70 j=1,sedtim(1)
                        buffer(ivar,j) = sedtr(sedtim(j+1),1)
   70                continue
                  else
c
c                    Sedredge branches are present. The total transport
c                    must be calculated.
c
                     do 80 j=1,sedtim(1)
                        ind    = sedtim(j+1)
                        buffer(ivar,j) = sedtr(ind,1) + sedtr(ind,2)
   80                continue
                  endif
               else if (ie.eq.5) then
c
                  do j=1,sedtim(1)
                     igrid = sedtim(j+1)
                     call mofdbr ( nbran ,branch ,igrid ,ibr)
                     buffer(ivar,j) = 
     +               shields (q2s(igrid,1),afs(igrid,1),cs(igrid,1),
     +                           grsize(1,igrid,1),delta,trform(1,ibr))
                  enddo
               else if (ie.eq.6) then
c
                  do j=1,sedtim(1)
                     igrid = sedtim(j+1)
                     call mofdbr ( nbran ,branch ,igrid ,ibr)
                     buffer(ivar,j) =                
     +               transport (sedtr(igrid,1),grsize(1,igrid,1),
     +                          delta ,g ,trform(1,ibr))
                  enddo     
               else if (ie.gt.1) then
                  do 90 j=1,sedtim(1)
                     buffer(ivar,j) = sedtr(sedtim(j+1),ie-1)
   90             continue
               endif
  100       continue
            write(lun) nint((istep-istphf)*scudt),
     +                 ((buffer(ivar,j),
     +                 ivar = 1,nvar),
     +                 j    = 1,sedtim(1))
         endif
      endif
c
 1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,
     +        '  (scu=',i8,'s)')
      return
c
      end
      real function shields (qm,am,cm,grsize,delta,trform)
c
c     calculate Shields parameter
c
      include '..\include\sobcon.i'
c      
      real     qm ,am ,cm ,delta ,trform ,grsize(4) ,um
      integer  d
      integer, parameter :: d50=2 , dmed=4
      real     velocity
      external velocity      
c     
      um = velocity(qm,am)
      if (int(trform).eq.ctrfmm) then
         d = dmed
      else 
         d = d50
      endif
      shields = (um/cm)**2 / (delta*grsize(d))
      end
      real function transport (s ,grsize ,delta ,g ,trform)
c
c     calculate sediment transport parameter
c
      include '..\include\sobcon.i'
c
      real     s ,delta ,trform ,g ,grsize(4) 
      integer  d
      integer, parameter :: d50=2 , dmed=4
c
      if (int(trform).eq.ctrfmm) then
         d = dmed
      else 
         d = d50
      endif
      transport = s / sqrt(g*delta*grsize(d)**3)
      end
