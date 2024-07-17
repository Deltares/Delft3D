      subroutine mowrhh( nmoman, nmomap, nmotim, ngrid , nbran , itim  ,
     +                   istep , nstep , mormap, mortim, maxlev, hlev  ,
     +                   sedtr , dissed, branch, typcr , mocpre, dt    ,
     +                   morini, buffer, sectc , sectv , wft   , sumda ,
     +                   hlev0 , gridnm)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         A.W.J. Koster
c
c Module:             MOWRHI(MOrphology WRite Morphology Results to
c                            HIs files)
c
c Module description: Subroutine MOWRHI writes the user selected morholo-
c                     gy results to the HIS file.
c
c                     In subroutine MOWRHI the user selected morp. results
c                     at user supplied locations and time levels (System
c                     Specifications for 1D Modelling System Front End)
c                     will be stored on the result file. The stored data
c                     can be processed further by the User Interface.
c                     The user can select functions of place and of
c                     time.
c
c                     Notice that the option for 'adapted cross sections'
c                     is NOT implemented for HIS files.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 20 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 25 buffer(nvar,ngrid)O  Buffer for results to be written to HIS file
c 19 dissed(4,nbran)   I  Redistributed sediment transport at begin and
c                         end of branches. At the outflow side of the
c                         branch the calculated transports are stored.
c                         At the inflow side the redistributed trans-
c                         ports are stored.
c                         (1,i)   Transport at section 1 (main or left
c                                 channel)  at begin of branch.
c                         (2,i)   Transport at section 2 (right channel)
c                                 at begin of branch.
c                         (3,i)   Transport at section 1 (main or left
c                                 channel)  at end of branch.
c                         (4,i)   Transport at section 2 (right channel)
c                                 at end of branch.
c 24 gridnm            I  Identifiers for grid points
c 17 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  9 istep             I  Current time step number (t(n+1)).
c  8 itim              P  -
c 16 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 23 mocpre            I  Reference table for main codes off output
c                         system
c 14 mormap(nmomap)    I  parameter list for MAP block with morphology
c                         results:
c                         (1)      = Report begin time
c                         (2)      = Report end time
c                         (3)      = Report time step
c                         (4)      = Report parameter 1 main code
c                         (5)      = Report parameter 1 sub code
c                         (i)      = Report parameter 1 main code
c                         (i+1)    = Report parameter 1 sub code
c                         (nmomap) = Report parameter n sub code
c 15 mortim(nmotim)    I  Parameter list for HIST block with morphology
c                         results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nmotim) = Report parameter n sub code
c  7 nbran             I  Number of branches.
c  6 ngrid             I  Number of grid points in network.
c  3 nmoman            I  Number of main codes morphology module
c  4 nmomap            I  Number of entries in sedmap.
c  5 nmotim            I  Number of entries in sedtim.
c 18 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
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
c error   write an ERROR to the error file.
c mofdbr  MOrphology FinD BRanch number for a gridpoint
c parsdt  Parse SOBEK date for year, etc.
c resadm  RESults; make ADMinistration for element selection
c resdes  RESults; DEScription group is defined
c resini  RESults; writing is INItialized
c restim  RESults; writing of current TIMe
c yesmap  YES MAP results are written
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: mowrhi.pf,v $
c Revision 1.6  1998/11/13  08:59:24  kuipe_j
c output error 2d morphology
c
c Revision 1.5  1998/02/13  12:12:51  kuipe_j
c Adapt to CMT
c
c Revision 1.4  1997/11/26  14:55:42  kuipe_j
c index error while writing cross-section headers
c
c Revision 1.3  1997/11/04  14:10:21  kuipe_j
c write cross-sections in His format
c
c Revision 1.2  1997/06/17  11:56:15  kuipe_j
c Add CVS keys
c
c
c
c***********************************************************************
c
      include '..\include\filsim.i'
      include '..\include\sobcon.i'
      include '..\include\sobdim.i'
c
c     Parameters
c
      integer ,parameter ::   nentri = 6 ,mincros = 5
      integer ,parameter ::   maxpar = nentri + 2*mincros - 1
      integer       nmoman, nmomap ,nmotim, nbran, ngrid, istep,nstep,
     +              maxlev
      integer       itim   (2),
     +              mormap (nmomap),
     +              mortim (nmotim),
     +              mocpre (nmoman),
     +              branch (4,nbran),    typcr(ngrid) ,
     +              morini (2)
      double precision dt, hlev (ngrid,maxlev)
      real          wft(ngrid,maxlev),
     +              sedtr  (ngrid,*),   
     +              dissed (4,nbran),
     +              buffer (maxpar,ngrid),
     +              sectc  (ngrid,3),sectv(ngrid,dmsecv),sumda (ngrid),
     +              hlev0  (ngrid) 
c
c     Local variables
c
      integer       i, j, k, ie, nlc
      integer       ifil , lun , nsk
      character*40  gridnm(*)
      double precision    scudt
      character*20  parnam(nentri+2*mincros-1)
      character*40  idmap(4) , idhis(4)
      integer       ibr   , iscudt
      integer       ijaar ,imaand ,idag  ,iuur ,imin   ,isec  ,iscu
      integer       nvar  ,igrid ,ivar ,istepf ,istphf

      logical       new   , newuit
      integer                                     :: mvar = 0
      save          istepf, istphf, scudt

c
c     External functions
c
      logical       yesmap
      external      yesmap

c     Clear array buffer:

      do i = 1, maxpar
         do j = 1, ngrid
            buffer( i, j ) = 0
         end do
      end do
c
c     Initialise
c
      idmap(1)  = 'SOBEK                                   '
      idmap(2)  = 'Maps results at gridpoints              '
      idmap(3)  = '                                        '
      idmap(4)  = '                                        '
      parnam(1) = 'Bed level           '
      parnam(2) = 'Mean bed level Main '
      parnam(3) = 'Increase of X-sect. '
      parnam(4) = 'Bed level increase  '
      parnam(5) = 'Integr. sed. transp.'
      parnam(6) = 'X-section level 1   '
      parnam(7) = 'X-section level 2   '
      parnam(8) = 'X-section level 3   '
      parnam(9) = 'X-section level 4   '
      parnam(10)= 'X-section level 5   '
      parnam(11)= 'X-section level 6   '
      parnam(12)= 'X-section level 7   '
      parnam(13)= 'X-section level 8   '
      parnam(14)= 'X-section level 9   '
      parnam(15)= 'X-section level 10  '
      
      idhis(1)  = 'SOBEK                                   '
      idhis(2)  = 'History results at gridpoints           '
      idhis(3)  = '                                        '
      idhis(4)  = '                                        '
c
c     Write MAP results
c
      if (nmomap .gt. 3 .and. 0 > 1) then
         if (yesmap(mormap(1), mormap(2), mormap(3), istep )) then
            nvar = min(nint(0.5*(nmomap-3)),maxpar)
            ifil = 1
            lun  = 58

            if ( morini(ifil) .gt. 0 ) then
c           File exists already hence open again:
               open ( unit = lun, file = mrpmap, form = 'binary',
     +                         access = 'APPEND' )
         end if


            if ( morini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = mrpmap , form = 'binary')
#else
#if defined (USE_HPUX)
               open(lun , file = mrpmap , form = 'unformatted')
#else
               open(lun , file = mrpmap , form = 'unformatted')
#endif
#endif
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               iscu  = nstep*dt/1.0d9+1.d0
               scudt = dt/dble(iscu)
               write ( idmap(4),1000 ) ijaar,imaand,idag,
     +                                 iuur,imin,isec,iscu
               write(lun) (idmap(i) , i = 1,4)

               ivar = 0
               do 33 i = 4, nmomap, 2

                  ie = mocpre(mormap(i)) + mormap(i+1)
               if ( ie .ge. 1 .and. ie .lt. 16 ) ivar = ivar + 1
               
33             continue

               mvar = ivar
               write(lun) mvar,ngrid
               do 34 i = 4, nmomap, 2

                  ie = mocpre(mormap(i)) + mormap(i+1)

               if ( ie .ge. 1 .and. ie .lt. 16 ) then
                  write(lun) ( parnam(ie) )
               end if
34             continue
               write(lun) (igrid , gridnm(igrid)(:20) ,
     +                     igrid = 1 , ngrid )
               morini(ifil) = 1
               istepf       = istep
            endif
c
            ivar = 0
            do 50 i = 4, nmomap, 2

               ie = mocpre(mormap(i)) + mormap(i+1)
c
               if (ie .lt. 16) ivar = ivar + 1

c              Find quantities to be written:
               if (ie .eq. 1) then
c
c                 Write bottoms (= min of two first levels)
c
c                 Bottom is min of two lowest levels
c
                  do 10 igrid=1,ngrid
                     buffer(ivar,igrid) = 
     +                  real( min(hlev(igrid,1), hlev(igrid,2)) )
   10             continue
c
               else if (ie .eq. 2) then
c
c                 Write Mean Bed level
c
                  do ibr = 1,nbran
                     if (typcr(ibr).ne.ccrsed) then
                        do igrid = branch(3,ibr),branch(4,ibr)
                           buffer(ivar,igrid) = 
     +                        sectv(igrid,2)-sectv(igrid,4) /               
     +                        sectc(igrid,2)
                        enddo
                     else 
                        do igrid = branch(3,ibr),branch(4,ibr)
                           buffer(ivar,igrid) = 
     +                        real( hlev(igrid,2) - wft(igrid,1) *
     +                        (hlev(igrid,2) - hlev(igrid,1)) /
     +                        (wft(igrid,1) + wft(igrid,2)) )              
                        enddo 
                     endif
                  enddo     

               else if (ie .eq. 3) then
c
c                 Increase in Area
c
                  do 14 igrid=1,ngrid
                     buffer(ivar,igrid) = sumda(igrid)
   14             continue

               else if (ie .eq. 4) then
c
c                 Increase of bed level
c
                  do igrid=1,ngrid
                     buffer(ivar,igrid) = real (min(hlev(igrid,1),
     +                                             hlev(igrid,2)) - 
     +                                             hlev0(igrid) )
                  enddo                             
               else if (ie .eq. 5) then
c
c              Write integrated sediment transports
c
               do 30 ibr = 1, nbran
c
c                 S_int on first and last point from dissed
c
                  buffer(ivar,branch(3,ibr)) = dissed(1,ibr)
                  buffer(ivar,branch(4,ibr)) = dissed(3,ibr)
c
                  do 20 igrid = branch(3,ibr)+1, branch(4,ibr)-1
                     buffer(ivar,igrid) = sedtr(igrid,1)
 20               continue
 30            continue
c
               else if (ie .ge. 6 .and. ie .lt. 16 ) then
c
c                 Write cross sections
c
                  k = mormap(i+1) + 1
                  do 40 igrid=1,ngrid
                     buffer(ivar,igrid) = real( hlev(igrid,k) )
 40               continue
c
               endif
 50         continue
c
c           Compute integer timestep for maps:

         iscudt = nint( (istep-istepf) * scudt )

            write(lun) iscudt
         write(lun) ((buffer(ivar,igrid), ivar  = 1 , mvar),
     +                 igrid = 1 , ngrid)
            close ( lun )

         endif
      endif
c
c     Write HIStory results
c
      nlc = mortim(1)
      new = mod(nmotim-nlc,2) .eq. 0
      if (new) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nmotim .gt. 1+nsk ) then
         if (new) then
            newuit = yesmap(mortim(nlc+2),mortim(nlc+3),mortim(nlc+4),
     &                      istep)
         else
            newuit = .false.
         endif
         if ( (mortim(1).gt.0 .and. .not. new) .or. newuit ) then
            nvar = min(nint(.5*(nmotim-mortim(1)-1-nsk)),maxpar)
            ifil = 2
            lun  = 59

            if ( morini(ifil) .gt. 0 ) then
c           File exists already hence open again:
               open ( unit = lun, file = mrphis, form = 'binary',
     +                         access = 'APPEND' )
         end if

            if ( morini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = mrphis , form = 'binary')
#else
#if defined (USE_HPUX)
               open(lun , file = mrphis , form = 'unformatted')
#else
               open(lun , file = mrphis , form = 'unformatted')
#endif
#endif
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               iscu  = nstep*dt/1.0d9+1.d0
               scudt = dt/dble(iscu)
               write ( idhis(4),1000 ) ijaar,imaand,idag,
     +                                 iuur,imin,isec,iscu
               write(lun) (idhis(i) , i = 1,4)

               ivar = 0
               do 133 i = mortim(1)+2+nsk, nmotim, 2

                  ie = mocpre(mortim(i)) + mortim(i+1)

               if ( ie .ge. 1 .and. ie .lt. 16 ) ivar = ivar + 1
               
133            continue

               mvar = ivar
               write(lun) mvar, mortim(1)

                do 134 i = mortim(1)+2+nsk, nmotim, 2

                  ie = mocpre(mortim(i)) + mortim(i+1)
               if ( ie .ge. 1 .and. ie .lt. 16 ) then
               write(lun) ( parnam(ie) )
               end if
134            continue
c
               write(lun) (mortim(j),gridnm(mortim(j))(:20),
     +                     j = 2 , mortim(1)+1)
               morini(ifil) = 1
               istphf       = istep
            endif
c
            ivar = 0
            do 130 i = mortim(1)+2+nsk, nmotim, 2

               ie = mocpre(mortim(i))+mortim(i+1)

               if ( ie .lt. 16 ) ivar = ivar + 1

               if ( ie .eq. 1) then
c
c                 Bottom for selected grid points
c
                  do 100 j = 1, mortim(1)
                     buffer(ivar,j) = real(
     +                 min ( hlev(mortim(j+1),1),hlev(mortim(j+1),2)) )
  100             continue
c
               else if (ie .eq. 2) then
c
c                 Write Mean Bed level
c
                  do j=1,mortim(1)
                     igrid = mortim(j+1)
                     call mofdbr ( nbran ,branch ,igrid ,ibr)
                     if (typcr(ibr).ne.ccrsed) then
                        buffer(ivar,j) = 
     +                     sectv(igrid,2)-sectv(igrid,4) /               
     +                     sectc(igrid,2)
                     else 
                        buffer(ivar,j) = real(  
     +                     hlev(igrid,2) - wft(igrid,1) *
     +                     (hlev(igrid,2) - hlev(igrid,1)) /
     +                     (wft(igrid,1) + wft(igrid,2))  )            
                     endif
                  enddo     
c     
               elseif (ie .eq. 3) then
c
c                 Increase in Area
c
                  do 102 j = 1, mortim(1)
                     buffer(ivar,j) = sumda(mortim(j+1))
  102             continue
c
               else if (ie .eq. 4) then
c
c                Increase of bed level
c
                  do  j = 1, mortim(1)
                     k = mortim(j+1)
                     buffer(ivar,j) = real( min(hlev(k,1),
     +                                    hlev(k,2)) - 
     +                                    hlev0(k)  )
                  enddo
               else if (ie .eq. 5) then
c
c                 S_int for selected grid points
c
                  do 110 j = 1, mortim(1)
c
c                    Find branch number of this gridpoint
c
                     call mofdbr ( nbran, branch, mortim(j+1), ibr )
c
c                    Test for first or last point
c
                     if      (mortim(j+1) .eq. branch(3,ibr)) then
                        buffer(ivar,j)= dissed(1,ibr)
                     else if (mortim(j+1) .eq. branch(4,ibr)) then
                        buffer(ivar,j)= dissed(3,ibr)
                     else
                        buffer(ivar,j)= sedtr(mortim(j+1),1)
                     endif
c
  110             continue
c
               elseif (ie .ge. 6 .and. ie .lt. 16 ) then
c
c                 Cross sections for selected grid points
c
                  k = mortim(i+1) + 1
                  do 120 j = 1, mortim(1)
c
                     call mofdbr ( nbran, branch, mortim(j+1), ibr )
c
                     buffer(ivar,j) = real( hlev(mortim(j+1),k)  )
 120              continue

               endif
 130        continue
c
c           Compute integer timestep for histories:

         iscudt = nint( (istep-istphf) * scudt )

            write(lun) iscudt
            write(lun) ((buffer(ivar,j),
     +                 ivar = 1,mvar),
     +                 j    = 1,mortim(1))

            close ( lun )

         endif
      endif
c
      return
c
 1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,
     +        '  (scu=',i8,'s)')
c
      end
