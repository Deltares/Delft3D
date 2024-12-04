subroutine gsfrrhi (nfrcmap ,nfrctim ,ngrid  ,nunlay ,nfrac   ,&
&itim    ,istep   ,nstep  ,frcmap ,frctim  ,&
&sedtr   ,dt      ,sedini ,buffer ,gridnm  ,&
&lanrinbt,nrdzdl  ,p0la   ,ptrla  ,pexla   ,&
&juer    ,ker     )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSFRRHI(Graded SEdiment FRaction Results HIs files)
!
! Module description: Writing of user defined graded sediment results to
!                     the HIS file.
!
!                     The user selected sediment results at user sup-
!                     plied locations and time levels will be stored on
!                     the result file. For every fraction a value will
!                     be stored. The stored data can be processed
!                     further by the User Interface.
!
!                     Instead of output of values per fraction as they
!                     are calculated also output of accumalated values
!                     is possible.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 19 buf(ngrid)        O  Buffer for results to be written to NEFIS.
!  7 istep             I  Current time step number (t(n+1)).
!  6 itim              P  -
!  4 ngrid             I  Number of grid points in network.
!  5 nsedrd            I  Number of defined sedredge branches.
!  2 nsemap            I  Number of entries in sedmap.
!  3 nsetim            I  Number of entries in sedtim.
!                         for main code i of sediment results.
! 14 sedmap(nsemap)    I  Parameter list for MAP block with sediment
!                         results:
!                         (1)      = Report begin time
!                         (2)      = Report end time
!                         (3)      = Report time step
!                         (4)      = Report parameter 1 main code
!                         (5)      = Report parameter 1 sub code
!                         (i)      = Report parameter 1 main code
!                         (i+1)    = Report parameter 1 sub code
!                         (nsemap) = Report parameter n sub code
! 15 sedtim(nsetim)    I  Parameter list for HIST block with sediment
!                         results:
!                         (1)      = Number of places
!                         (2)      = Report grid point 1
!                         (3)      = Report grid point 2
!                         (i)      = Report grid point n
!                         (i+1)    = Report parameter code 1
!                         (i+2)    = Report parameter sub code 1
!                         (nsetim) = Report parameter n sub code
! 16 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
   include '..\include\filsim.i'
   include '..\include\sobdim.i'
   include '..\include\errcod.i'
!
!     Declaration of parameters
!
   integer      nentri
   parameter   (nentri=6)
   integer      nfrcmap,nfrctim ,ngrid ,nfrac ,nunlay ,&
   &istep  ,nstep   ,juer  ,ker
   integer      frcmap(nfrcmap) ,&
   &frctim(nfrctim) ,&
   &lanrinbt(ngrid) ,nrdzdl(ngrid) ,&
   &itim  (2)       ,&
   &sedini(*)
   real         sedtr (ngrid,*)  ,&
   &buffer(dmbuffer*ngrid)
   real         p0la  (ngrid,nfrac,nunlay),&
   &ptrla (ngrid,nfrac,2)     ,pexla (ngrid,nfrac,2)
   double       precision dt
   character*40 gridnm(*)
!
!     Declaration of local variables
!
   integer      i      ,nlc    ,nsk
   integer      ijaar  ,imaand ,idag ,iuur ,imin ,isec   ,iscu
   integer      lun    ,ivar   ,nvar ,igrid,ifil ,istepf ,istphf
   integer      loc    ,imain  ,main ,nsub ,sub  ,frac   ,lay
   real         result
   double precision     scudt
   character*40 idmap  (4)     ,idhis(4)
   character*20 parnam (nentri)
   logical      new    ,newuit,  accu
   save         istepf ,istphf ,scudt
!
!     Declaration of external functions
!
   logical      yesmap
   external     yesmap
!
!     Write Map results
!
   if (nfrcmap .gt. 3) then
      if (yesmap(frcmap(1),frcmap(2),frcmap(3),istep)) then
         nvar = 0
         do imain = 4,nfrcmap,2
            nvar = nvar + abs(frcmap(imain+1))
         enddo
         nvar = nvar * nfrac
         if (nvar.gt.dmbuffer*ngrid) goto 9000
         ifil = 3
         lun  = 114
         if ( sedini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
            open(lun , file = gfrcmap , form = 'binary')
#else
#if defined (USE_HPUX)
            open(lun , file = gfrcmap , form = 'unformatted')
#else
            open(lun , file = gfrcmap , form = 'unformatted')
#endif
#endif
            idmap(1)  = 'SOBEK                                   '
            idmap(2)  = 'Maps results at gridpoints              '
            idmap(3)  = '                                        '
            idmap(4)  = '                                        '
            call defparnam (parnam)
            iscu  = nstep*dt/1.0d9+1.d0
            scudt = dt/dble(iscu)
            call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
            write ( idmap(4),1000 ) ijaar,imaand,idag,&
            &iuur,imin,isec,iscu
            write(lun) (idmap(i) , i = 1,4)
            write(lun) nvar,ngrid
            do imain = 4,nfrcmap,2
               call frcnam (imain, frcmap, parnam, nfrac, lun)
            enddo
            write(lun) (igrid , gridnm(igrid)(:20) ,&
            &igrid = 1 , ngrid )
            sedini(ifil) = 1
            istepf       = istep
         endif
!
         write(lun) nint((istep-istphf)*scudt)
         do loc = 1,ngrid
            ivar = 1
            do imain = 4,nfrcmap,2
               nsub = frcmap(imain+1)
               accu = nsub .lt. 0
               main = frcmap(imain)
               nsub = abs(nsub)
               do sub=1,nsub
                  do frac =1,nfrac
                     if (main .eq. 1) then
                        call frcres (result,sedtr ,&
                        &loc   ,frac ,ngrid ,accu )
                     else if (main .eq. 2) then
                        call frcres (result,ptrla(1,1,2),&
                        &loc   ,frac ,ngrid ,accu )
                     else if (main .eq. 3) then
                        call frcres (result,pexla(1,1,2),&
                        &loc   ,frac ,ngrid ,accu )
                     else if (main .gt. 3) then
                        if (main.eq.4) then
                           lay = lanrinbt(loc)
                        else if (main.eq.5) then
                           lay = lanrinbt(loc) + sub
                        else if (main.eq.6) then
                           lay = lanrinbt(loc) - sub
                        endif
                        if (lay.lt.1 .or. lay.gt.nrdzdl(loc)) then
                           result = 0.
                        else
                           call frcres (result,p0la(1,1,lay),&
                           &loc   ,frac ,ngrid ,accu )
                        endif
                     endif
                     buffer(ivar+nfrac-frac) = result
                  enddo
                  ivar = ivar + nfrac
               enddo
            enddo
            write(lun) (buffer(ivar),ivar = 1,nvar)
         enddo
      endif
   endif
!
!     Write History results.
!
   nlc = frctim(1)
   new = mod(nfrctim-nlc,2) .eq. 0
   if (new) then
      nsk = 3
   else
      nsk = 0
   endif
!
   if (nfrctim .gt. 1+nsk) then
      if (new) then
         newuit = yesmap(frctim(nlc+2),frctim(nlc+3),frctim(nlc+4),&
         &istep)
      else
         newuit = .false.
      endif
      if ( (frctim(1).gt.0 .and. .not. new) .or. newuit ) then
         nvar = 0
         do imain = frctim(1)+2+nsk,nfrctim,2
            nvar = nvar + abs(frctim(imain+1))
         enddo
         nvar = nvar * nfrac
         if (nvar.gt.dmbuffer*ngrid) goto 9000
         ifil = 4
         lun  = 115
         if ( sedini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
            open(lun , file = gfrchis , form = 'binary')
#else
#if defined (USE_HPUX)
            open(lun , file = gfrchis , form = 'unformatted')
#else
            open(lun , file = gfrchis , form = 'unformatted')
#endif
#endif
            idhis(1)  = 'SOBEK                                   '
            idhis(2)  = 'History results at gridpoints           '
            idhis(3)  = '                                        '
            idhis(4)  = '                                        '
            call defparnam (parnam)
            iscu  = nstep*dt/1.0d9+1.d0
            scudt = dt/dble(iscu)
            call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
            write ( idhis(4),1000 ) ijaar,imaand,idag,&
            &iuur,imin,isec,iscu
            write(lun) (idhis(i) , i = 1,4)
            write(lun) nvar , frctim(1)
            do imain = frctim(1)+2+nsk,nfrctim,2
               call frcnam (imain, frctim, parnam, nfrac, lun)
            enddo
            write(lun) (frctim(igrid),gridnm(frctim(igrid))(:20),&
            &igrid = 2 , frctim(1)+1)
            sedini(ifil) = 1
            istphf       = istep
         endif
!
         write(lun) nint((istep-istphf)*scudt)
         do loc = 1,frctim(1)
            igrid = frctim(loc+1)
            ivar  = 1
            do imain = frctim(1)+2+nsk,nfrctim,2
               nsub = frctim(imain+1)
               accu = nsub .lt. 0
               main = frctim(imain)
               nsub = abs(nsub)
               do sub=1,nsub
                  do frac =1,nfrac
                     if (main .eq. 1) then
                        call frcres (result,sedtr ,&
                        &igrid ,frac ,ngrid ,accu )
                     else if (main .eq. 2) then
                        call frcres (result,ptrla(1,1,2),&
                        &igrid ,frac ,ngrid ,accu )
                     else if (main .eq. 3) then
                        call frcres (result,pexla(1,1,2),&
                        &igrid ,frac ,ngrid ,accu )
                     else if (main .gt. 3) then
                        if (main.eq.4) then
                           lay = lanrinbt(igrid)
                        else if (main.eq.5) then
                           lay = lanrinbt(igrid) + sub
                        else if (main.eq.6) then
                           lay = lanrinbt(igrid) - sub
                        endif
                        if (lay.lt.1 .or. lay.gt.nrdzdl(igrid)) then
                           result = 0.
                        else
                           call frcres (result,p0la(1,1,lay),&
                           &igrid ,frac ,ngrid ,accu )
                        endif
                     endif
                     buffer(ivar+nfrac-frac) = result
                  enddo
                  ivar = ivar + nfrac
               enddo
            enddo
            write(lun) (buffer(ivar),ivar = 1,nvar)
         enddo
!
      endif
   endif
   return

9000 continue
   ker   = fatal
   call error (juer ,'GSFRRHI Too many output functions',egoutm,ker)
!
1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,&
   &'  (scu=',i8,'s)')
end

subroutine frcres (value ,singlea ,loc  ,frac  ,ngrid, accu )
!
!     Get value of a function for a specific fraction and location.
!     If requested the function value is accumulated.
!
!     Declaration of parameters
!
   integer            loc   ,frac    ,ngrid
   real               value ,singlea (ngrid,*)
   logical            accu
!
   if (.not.accu .or. frac.eq.1 ) then
      value = singlea (loc, frac)
   else
      value = value + singlea (loc, frac)
   endif

end

subroutine frcnam (imain, timmap, parnam, nfrac, lun)
!
!     Make and write function name
!
!     Declaration of parameters
!
   integer            imain, nfrac, lun
   integer            timmap(*)
   character*20       parnam(*)
!
!     Declaration of local variables
!
   integer            main, nsub, sub, indh, inddig, frac
   logical            accu
   character*25       partxt
   character*5        numtxt
!
   main = timmap(imain)
   nsub = timmap(imain+1)
   accu = nsub .lt. 0
   nsub = abs(nsub)
   do sub=1,nsub
      partxt = ' '
      if (accu) then
         partxt = 'Cum '//parnam(main)(1:16)
      else
         partxt = parnam(main)
      endif
      indh = index (partxt,'(')
      if (main .gt. 4) then
         write (numtxt,'(i4,1x)') sub
         inddig = 1
         do while (numtxt(inddig:inddig).eq.' ')
            inddig = inddig + 1
         enddo
         partxt(indh:) = numtxt(inddig:5)//'Lay ('
         indh = index (partxt,'(')
      endif
      do frac =nfrac,1,-1
         write (numtxt,'(i4,1x)') frac
         inddig = 1
         do while (numtxt(inddig:inddig).eq.' ')
            inddig = inddig + 1
         enddo
         partxt(indh:) = 'F'//numtxt(inddig:5)
         write(lun) partxt(1:20)
      enddo
   enddo
end

subroutine defparnam (parnam)
!
!     Define parameter names
!
   character*20 parnam(*)
!
   parnam(1) = 'Sed Trp ('
   parnam(2) = 'P Trp Lay ('
   parnam(3) = 'P Ex Lay ('
   parnam(4) = 'P Un Lay ('
   parnam(5) = 'P U+( Lay ('
   parnam(6) = 'P U-( Lay ('

end
