      subroutine gshidexp (initra ,nfrac  ,ngrid  ,grn   ,dfrac  ,
     &                     relden ,chezy  ,velo   ,p     ,hidexpp,
     &                     igr    ,hidexpf)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSHIDEXP (Grad Sed Transp Form HIDing and EXPosure)
c
c Module description: Calculate the Hiding and exposure factor.
c
c                     [ Laguzzi,Febr 1994,Q1660 / par 6.2.1 ]
c
c-----------------------------------------------------------------------
c Parameters:
c NAME              IO DESCRIPTION
c chezy             I  Chezy value
c dfrac             I  Grain sizes per fraction
c g                 I  Acceleration of gravity.
c grn               I  Contains grain sizes Dm, D50. D35 and D90. 
c hidexpf           I  Hiding and exposure factor per fraction
c hidexpp           IO parameters to calculate hiding and exposure factor.
c                      (1) = type of hiding and exposure effect.
c                            0 : no hiding and exposure
c                            1 : Egiazaroff, Ashida & Michiue (standard)
c                            2 : Egiazaroff, Ashida & Michiue (experimental)
c                            3 : Proffitt & Sutherland
c                            4 : Wu , Wang & Jia
c                      For type 1 + 2:
c                      (2) = flag for grainsize (type 2)
c                            0 : Dref = Dmed  
c                            1 : Dref = D50
c                      (3) = coeff alpha (0.4 for type 1)
c                      (4) = exponent gamma (2 for type 1)
c                      (5) = User defined grain size; overrules par 2 (type 2) 
c                      (8) = log(19)
c                      (9) = ddswitch 
c                      (10)= beta
c                      For type 4
c                      (2) = 0 : hiding and exp power = 0.6 by default 
c                            1 : hiding and exp power specified by user
c                      (3) = non default hiding and exp power
c hrad              I  hydraulic radius
c initra            I  True for initialization of transport formulas
c                      (calculation of constants) else False.
c nfrac             I  number of fractions 
c ngrid             I  number of grid points
c p(ngrid,nfrac)    I  probability of transport layer
c relden            I  relative density
c velo              I  velocity (without sign)
c=======================================================================
c
c     Declaration of parameters
c
      integer    nfrac  ,ngrid ,igr
      real       relden ,chezy ,velo  
      real       hidexpp (10)  ,hidexpf(nfrac) ,dfrac(nfrac),
     &           grn    (4)    ,p(ngrid,nfrac) 
      logical    initra
c
c     Declaration of local parameters
c
      integer    hidextyp,if      ,jf
      real       dref    ,shields ,da    ,dida   ,phi  ,eps
      integer     d35    ,d50     ,d90   ,dmed  
      parameter  (d35=1  ,d50=2   ,d90=3 ,dmed=4)      

      hidextyp = nint(hidexpp(1))+1
      if (initra) then
c
c        Calculation of constants in time.
c
         goto (90,10,20,90,40) hidextyp  
         
c        Egiazaroff, Ashida and Michiu hiding and exposure factor(standard)     
   10    continue
            hidexpp(3) = 0.4
            hidexpp(4) = 2.0
            call egiazarofa (hidexpp) 
         goto 90 
         
c        Egiazaroff, Ashida and Michiu hiding and exposure factor(experimental)     
   20    continue
            call egiazarofa (hidexpp)
         goto 90 

c        Wu , Wang and Jia hiding and exposure (set default)
   40    continue
            if (nint(hidexpp(2)) .eq. 0) hidexpp(3) = 0.6     

   90    continue

      else
      
         goto (100,110,120,130,140) hidextyp  

c        no hiding and exposure factor     
  100    continue
            do jf=1,nfrac
               hidexpf(jf) = 1.0
            enddo
         goto 190
         
c        Egiazaroff, Ashida and Michiu hiding and exposure factor(standard)     
  110    continue
            dref = grn(dmed)
            call egiazarofb (hidexpp ,dref ,dfrac, nfrac, hidexpf)
         goto 190 
         
c        Egiazaroff, Ashida and Michiu hiding and exposure factor(experimental)     
  120    continue
            if (nint(hidexpp(2)).eq.0) then
               dref = grn(dmed)
            else
               dref = grn(d50)
            endif
            if (hidexpp(5) .gt. 0.) dref = hidexpp(5)
            call egiazarofb (hidexpp ,dref ,dfrac, nfrac, hidexpf) 
         goto 190
         
c        Proffitt & Sutherland hiding and exposure factor
  130    continue
            shields = (velo/chezy)**2 / (grn(d50) * relden)
            if (shields .le. 0.04) then
               da = grn(d50) * 1.08
            else if (shields .lt. 0.045829) then   
               da = grn(d50) * (1.944 -21.6 * shields)
            else if (shields .lt. 0.097019) then
               da = grn(d50) * (1.40 - 9.73 * shields)
            else
               da = grn(d50) * 0.456
            endif
c
            do jf=1,nfrac
               dida = dfrac(jf) / da
               if (dida .le. 0.075) then
                  eps = 0.40378
               else if (dida .lt. 3.7) then   
                  eps = 0.53 * log10(dida) + 1.0
               else
                  eps = 1.301146
               endif              
               hidexpf(jf) = 1./eps**2 
            enddo   
         goto 190 

c        hiding and exposure according to Wu , Wang and Jia
  140    continue
         do if=1,nfrac 
               phi = 0.
               do jf=1,nfrac 
                  phi = phi + 
     +                  p(igr,jf)*dfrac(jf)/(dfrac(if)+dfrac(jf))
               enddo
               hidexpf(if) = (phi/(1.0-phi))**hidexpp(3)
            enddo
  190    continue
      endif   
      end           
c            
      subroutine egiazarofa (hidexpp)
c   
c     Initialize time independent constants in the
c     Egiazaroff, Ashida and Michiu hiding and exposure factor
c
      real       hidexpp (10) 

c     log19 
      hidexpp(8)  = log10(19.)
c     ddswitch
      hidexpp(9)  = max(hidexpp(3),0.1)
c     beta          ddswitch         log19            ddswitch
      hidexpp(10) = hidexpp(9)*(hidexpp(8)/log10(19.*hidexpp(9)))
     &             **hidexpp(4)   
      end

      subroutine egiazarofb (hidexpp ,dref ,dfrac, nfrac, hidexpf)
c
c     Calculate hiding and exposure factor according to
c     Egiazaroff, Ashida and Michiu
c
      integer    nfrac
      real       dref
      real       hidexpp (10),hidexpf(nfrac) ,dfrac(nfrac)
c 
c     local  variables
c
      integer    jf
      real       log19, ddswitch, beta, dd
c         
      log19    = hidexpp(8)
      ddswitch = hidexpp(9)
      beta     = hidexpp(10)
        
      do jf=1,nfrac
         dd = dfrac(jf) / dref
         if (dd .lt. ddswitch   ) then
            hidexpf(jf) = beta / dd
         else
            hidexpf(jf) = (log19 / log10(19. * dd)) ** hidexpp(4)
         endif
      enddo
      end
