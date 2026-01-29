subroutine intlin(nmax, mmax, n,  m,   s,   xnod,   ynod,   bigd,&
&x,    y,    f,  dxf, dyf, xunder, yunder ,xover,&
&yover)
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         H.Petit
!
! Module:             INTLIN (INTerpolation LINear)
!
! Module description: The subroutine approximates two dimensional data
!                     given in a table s.
!                     At points (xnod(i),ynod(j)) data are available in
!                     s(i,j) (i=0(1)n, j=0(1)m).
!                     Bilinear interpolation is used in the regions where
!                     abs(x-xnod(i)) and abs(y-ynod(j)) are not too small
!                     for all i and j.
!                     Near to the grid lines x=xnod(i) and the grid lines
!                     y=ynod(j) parabolic functions are used for approximation
!
!-----------------------------------------------------------------------
! Parameters:
! NAME             IO DESCRIPTION
! bigd             I  Parameter which determines the size of the strip
!                     round the grid lines in which a parabolic approximation
!                     is used
! dxf              O  derivative of the approximating function to x
! dyf              O  derivative of the approximating function to y
! f                O  value of the approximating function
! m                I  number of columns-1 in the table
! mmax             I  maximum number of columns-1 in the table (dimension)
! n                I  number of rows-1 in the table
! nmax             I  maximum number of rows-1 in the table (dimension)
! s                I  table containing the discrete relation
!                     s(i,j)=Q(xnod(i),ynod(j)) for i=0(1)n, j=0(1)m
! xnod             I  xnod(j)= h1(i) for i=0(1)n
! ynod             I  ynod(j)= h2(j) or (h1-h2)(j)  for j=0(1)m
! x                I  x value of point where approximation is required
! y                I  y value of point where approximation is required
!-----------------------------------------------------------------------
!
!     Declaration of parameters:
!
   integer nmax,mmax,n,m
   real    s(0:nmax,0:mmax),xnod(0:nmax),ynod(0:mmax)
   real    bigd,x,y,f,dxf,dyf
   logical xunder ,yunder ,xover ,yover
!
!     Declaration of local variables
!
   integer ix,iy
   real pix,pixp1,qiy,qiyp1,tau,lambda,lq,lmq,tp,tmp,fxp,fxmp,&
   &ax,bx,cx,ay,by,cy,f1,dxf1,dyf1,dxlin,dylin,f2,dxf2,dyf2,blx,&
   &brx,bly,bry,mixxp,mixxm,mixyp,mixym,dfxm,dfxp,dyax,dybx,dycx,&
   &dxay,dxby,dxcy,dyfxp,dyfxmp,lin,dfyp,dfym,fyq,fymq,dxfyq,&
   &dxfymq
!
! The arguments x and y for which data are available in the table S are
! stored in arrays x (xnod) and y (ynod).
! The values x and y need to be given in climbing order:
! x(i)<x(i+1) for i=0(1)n-1 and y(j)<y(j+1) for j=0(1)m-1.
!
   xunder = .false.
   yunder = .false.
   xover  = .false.
   yover  = .false.
!
   if(x.ge.xnod(0) .and. x.lt.xnod(n) .and.&
   &y.ge.ynod(0) .and. y.lt.ynod(m) ) then
!
! point is in range [x0,xn[ X [y0,ym[
!
!     |   |
!  -----------
!     | x |
!  -----------
!     |   |
!
      ix=n
100   ix=ix-1
      if(x.lt.xnod(ix))goto 100
      iy=m
200   iy=iy-1
      if(y.lt.ynod(iy))goto 200
!
! point is in range [xnod(ix),xnod(ix+1)[ X [ynod(iy),ynod(iy+1)[
!
! Determination of band width pix and pixp1 for x direction and qiy
! and qiyp1 for the y direction:
      pix=1.0
      if(ix.gt.0)then
         pix=min(xnod(ix+1)-xnod(ix),xnod(ix)-xnod(ix-1))/bigd
      endif
      pixp1=1.0
      if(ix.lt.n-1)then
         pixp1=min(xnod(ix+2)-xnod(ix+1),xnod(ix+1)-xnod(ix))/bigd
      endif
      qiy=1.0
      if(iy.gt.0)then
         qiy=min(ynod(iy+1)-ynod(iy),ynod(iy)-ynod(iy-1))/bigd
      endif
      qiyp1=1.0
      if(iy.lt.m-1)then
         qiyp1=min(ynod(iy+2)-ynod(iy+1),ynod(iy+1)-ynod(iy))/bigd
      endif
      blx=1.0
      if(ix.eq.0)blx=0.0
      brx=1.0
      if(ix.eq.n-1)brx=0.0
      bly=1.0
      if(iy.eq.0)bly=0.0
      bry=1.0
      if(iy.eq.m-1)bry=0.0
! local coordinates:
      tau   = (x-xnod(ix))/(xnod(ix+1)-xnod(ix))
      lambda= (y-ynod(iy))/(ynod(iy+1)-ynod(iy))
      lin   = tau*(lambda*s(ix+1,iy+1)+(1.0-lambda)*s(ix+1,iy))+&
      &(1.0-tau)*(lambda*s(ix,iy+1)+(1.0-lambda)*s(ix,iy))
      dxlin =(lambda*s(ix+1,iy+1)+(1.0-lambda)*s(ix+1,iy)&
      &-lambda*s(ix,iy+1)-(1.0-lambda)*s(ix,iy))/&
      &(xnod(ix+1)-xnod(ix))
      dylin =(tau*(s(ix+1,iy+1)-s(ix+1,iy))+&
      &(1.0-tau)*(s(ix,iy+1)-s(ix,iy)))/(ynod(iy+1)-ynod(iy))
      if(x.lt.xnod(ix)+blx*pix)then
         dfxp =(lambda*(s(ix+1,iy+1)-s(ix,iy+1))+&
         &(1.0-lambda)*(s(ix+1,iy)-s(ix,iy)))/&
         &(xnod(ix+1)-xnod(ix))
         dfxm =(lambda*(s(ix,iy+1)-s(ix-1,iy+1))+&
         &(1.0-lambda)*(s(ix,iy)-s(ix-1,iy)))/&
         &(xnod(ix)-xnod(ix-1))
         tp  =pix/(xnod(ix+1)-xnod(ix))
         tmp =1.0-pix/(xnod(ix)-xnod(ix-1))
         fxp =tp*(lambda*s(ix+1,iy+1)+(1.0-lambda)*s(ix+1,iy))+&
         &(1.0-tp)*(lambda*s(ix,iy+1)+(1.0-lambda)*s(ix,iy))
         fxmp=tmp*(lambda*s(ix,iy+1)+(1.0-lambda)*s(ix,iy))+&
         &(1.0-tmp)*(lambda*s(ix-1,iy+1)+(1.0-lambda)*s(ix-1,iy))
         ax  =(dfxp-dfxm)/(4.0*pix)
         bx  =0.75*(fxp-fxmp)/pix-0.25*(dfxp+dfxm)
         cx  =0.5*(fxp+fxmp)-0.25*(dfxp-dfxm)*pix
!
         f1  =ax*(x-xnod(ix))**2+bx*(x-xnod(ix))+cx
!
         dxf1  =2.0*ax*(x-xnod(ix))+bx
!
         mixxp =(s(ix+1,iy+1)-s(ix,iy+1)-s(ix+1,iy)+s(ix,iy))/&
         &((xnod(ix+1)-xnod(ix))*(ynod(iy+1)-ynod(iy)))
         mixxm =(s(ix,iy+1)-s(ix-1,iy+1)-s(ix,iy)+s(ix-1,iy))/&
         &((xnod(ix)-xnod(ix-1))*(ynod(iy+1)-ynod(iy)))
         dyax  =(mixxp-mixxm)/(4.0*pix)
         dyfxp =(tp *(s(ix+1,iy+1)-s(ix+1,iy))+(1.0-tp )*&
         &(s(ix  ,iy+1)-s(ix  ,iy)))/(ynod(iy+1)-ynod(iy))
         dyfxmp=(tmp*(s(ix  ,iy+1)-s(ix  ,iy))+(1.0-tmp)*&
         &(s(ix-1,iy+1)-s(ix-1,iy)))/(ynod(iy+1)-ynod(iy))
         dybx  =0.75*(dyfxp-dyfxmp)/pix-0.25*(mixxp+mixxm)
         dycx  =0.5*(dyfxp+dyfxmp)-0.25*(mixxp-mixxm)*pix
         dyf1  =dyax*(x-xnod(ix))**2+dybx*(x-xnod(ix))+dycx
      else if(x.le.xnod(ix+1)-brx*pixp1)then
         f1=lin
         dxf1=dxlin
         dyf1=dylin
      else
         dfxp  = (lambda*(s(ix+2,iy+1)-s(ix+1,iy+1))+&
         &(1.0-lambda)*(s(ix+2,iy)-s(ix+1,iy)))/&
         &(xnod(ix+2)-xnod(ix+1))
         dfxm  = (lambda*(s(ix+1,iy+1)-s(ix,iy+1))+&
         &(1.0-lambda)*(s(ix+1,iy)-s(ix,iy)))/&
         &(xnod(ix+1)-xnod(ix))
         tp  =pixp1/(xnod(ix+2)-xnod(ix+1))
         tmp =1.0-pixp1/(xnod(ix+1)-xnod(ix))
         fxp =tp*(lambda*s(ix+2,iy+1)+(1.0-lambda)*s(ix+2,iy))+&
         &(1.0-tp)*(lambda*s(ix+1,iy+1)+(1.0-lambda)*s(ix+1,iy))
         fxmp=tmp*(lambda*s(ix+1,iy+1)+(1.0-lambda)*s(ix+1,iy))+&
         &(1.0-tmp)*(lambda*s(ix,iy+1)+(1.0-lambda)*s(ix,iy))
         ax  =(dfxp-dfxm)/(4.0*pixp1)
         bx  =0.75*(fxp-fxmp)/pixp1-0.25*(dfxp+dfxm)
         cx  =0.5*(fxp+fxmp)-0.25*(dfxp-dfxm)*pixp1
!
         f1  =ax*(x-xnod(ix+1))**2+bx*(x-xnod(ix+1))+cx
!
         dxf1=2.0*ax*(x-xnod(ix+1))+bx
!
         mixxp =(s(ix+2,iy+1)-s(ix+1,iy+1)-s(ix+2,iy)+s(ix+1,iy))/&
         &((xnod(ix+2)-xnod(ix+1))*(ynod(iy+1)-ynod(iy)))
         mixxm =(s(ix+1,iy+1)-s(ix,iy+1)-s(ix+1,iy)+s(ix,iy))/&
         &((xnod(ix+1)-xnod(ix))*(ynod(iy+1)-ynod(iy)))
         dyax  =(mixxp-mixxm)/(4.0*pixp1)
         dyfxp =(tp*(s(ix+2,iy+1)-s(ix+2,iy))+(1.0-tp)*&
         &(s(ix+1,iy+1)-s(ix+1,iy)))/(ynod(iy+1)-ynod(iy))
         dyfxmp=(tmp*(s(ix+1,iy+1)-s(ix+1,iy))+(1.0-tmp)*&
         &(s(ix,iy+1)-s(ix,iy)))/(ynod(iy+1)-ynod(iy))
         dybx  =0.75*(dyfxp-dyfxmp)/pixp1-0.25*(mixxp+mixxm)
         dycx  =0.5*(dyfxp+dyfxmp)-0.25*(mixxp-mixxm)*pixp1
         dyf1  =dyax*(x-xnod(ix+1))**2+dybx*(x-xnod(ix+1))+dycx
      endif
!
      if(y.lt.ynod(iy)+bly*qiy)then
         dfyp  = (tau*(s(ix+1,iy+1)-s(ix+1,iy))+&
         &(1.0-tau)*(s(ix,iy+1)-s(ix,iy)))/&
         &(ynod(iy+1)-ynod(iy))
         dfym  = (tau*(s(ix+1,iy)-s(ix+1,iy-1))+&
         &(1.0-tau)*(s(ix,iy)-s(ix,iy-1)))/&
         &(ynod(iy)-ynod(iy-1))
         lq  =qiy/(ynod(iy+1)-ynod(iy))
         lmq =1.0-qiy/(ynod(iy)-ynod(iy-1))
         fyq =lq*(tau*s(ix+1,iy+1)+(1.0-tau)*s(ix,iy+1))+&
         &(1.0-lq)*(tau*s(ix+1,iy)+(1.0-tau)*s(ix,iy))
         fymq=lmq*(tau*s(ix+1,iy)+(1.0-tau)*s(ix,iy))+&
         &(1.0-lmq)*(tau*s(ix+1,iy-1)+(1.0-tau)*s(ix,iy-1))
         ay  =(dfyp-dfym)/(4.0*qiy)
         by  =0.75*(fyq-fymq)/qiy-0.25*(dfyp+dfym)
         cy  =0.5*(fyq+fymq)-0.25*(dfyp-dfym)*qiy
!
         f2  =ay*(y-ynod(iy))**2+by*(y-ynod(iy))+cy
!
         dyf2 =2.0*ay*(y-ynod(iy))+by
!
         mixyp =(s(ix+1,iy+1)-s(ix,iy+1)-s(ix+1,iy)+s(ix,iy))/&
         &((xnod(ix+1)-xnod(ix))*(ynod(iy+1)-ynod(iy)))
         mixym =(s(ix+1,iy  )-s(ix,iy  )-s(ix+1,iy-1)+s(ix,iy-1))/&
         &((xnod(ix+1)-xnod(ix))*(ynod(iy)-ynod(iy-1)))
         dxay  =(mixyp-mixym)/(4.0*qiy)
         dxfyq =(lq*(s(ix+1,iy+1)-s(ix,iy+1))+(1.0-lq)*&
         &(s(ix+1,iy)-s(ix,iy)))/(xnod(ix+1)-xnod(ix))
         dxfymq=(lmq*(s(ix+1,iy)-s(ix,iy))+(1.0-lmq)*&
         &(s(ix+1,iy-1)-s(ix,iy-1)))/(xnod(ix+1)-xnod(ix))
         dxby  =0.75*(dxfyq-dxfymq)/qiy-0.25*(mixyp+mixym)
         dxcy  =0.5*(dxfyq+dxfymq)-0.25*(mixyp-mixym)*qiy
         dxf2  =dxay*(y-ynod(iy))**2+dxby*(y-ynod(iy))+dxcy
      else if(y.le.ynod(iy+1)-bry*qiyp1)then
         f2=lin
         dxf2=dxlin
         dyf2=dylin
      else
         dfyp  = (tau*(s(ix+1,iy+2)-s(ix+1,iy+1))+&
         &(1.0-tau)*(s(ix,iy+2)-s(ix,iy+1)))/&
         &(ynod(iy+2)-ynod(iy+1))
         dfym  = (tau*(s(ix+1,iy+1)-s(ix+1,iy))+&
         &(1.0-tau)*(s(ix,iy+1)-s(ix,iy)))/&
         &(ynod(iy+1)-ynod(iy))
         lq  =qiyp1/(ynod(iy+2)-ynod(iy+1))
         lmq =1.0-qiyp1/(ynod(iy+1)-ynod(iy))
         fyq =lq*(tau*s(ix+1,iy+2)+(1.0-tau)*s(ix,iy+2))+&
         &(1.0-lq)*(tau*s(ix+1,iy+1)+(1.0-tau)*s(ix,iy+1))
         fymq=lmq*(tau*s(ix+1,iy+1)+(1.0-tau)*s(ix,iy+1))+&
         &(1.0-lmq)*(tau*s(ix+1,iy)+(1.0-tau)*s(ix,iy))
         ay  =(dfyp-dfym)/(4.0*qiyp1)
         by  =0.75*(fyq-fymq)/qiyp1-0.25*(dfyp+dfym)
         cy  =0.5*(fyq+fymq)-0.25*(dfyp-dfym)*qiyp1
!
         f2  =ay*(y-ynod(iy+1))**2+by*(y-ynod(iy+1))+cy
!
         dyf2 =2.0*ay*(y-ynod(iy+1))+by
!
         mixyp =(s(ix+1,iy+2)-s(ix+1,iy+1)-s(ix,iy+2)+s(ix,iy+1))/&
         &((xnod(ix+1)-xnod(ix))*(ynod(iy+2)-ynod(iy+1)))
         mixym =(s(ix+1,iy+1)-s(ix+1,iy)-s(ix,iy+1)+s(ix,iy))/&
         &((xnod(ix+1)-xnod(ix))*(ynod(iy+1)-ynod(iy)))
         dxay  =(mixyp-mixym)/(4.0*qiyp1)
         dxfyq =(lq*(s(ix+1,iy+2)-s(ix,iy+2))+(1.0-lq)*&
         &(s(ix+1,iy+1)-s(ix,iy+1)))/(xnod(ix+1)-xnod(ix))
         dxfymq=(lmq*(s(ix+1,iy+1)-s(ix,iy+1))+(1.0-lmq)*&
         &(s(ix+1,iy)-s(ix,iy)))/(xnod(ix+1)-xnod(ix))
         dxby  =0.75*(dxfyq-dxfymq)/qiyp1-0.25*(mixyp+mixym)
         dxcy  =0.5*(dxfyq+dxfymq)-0.25*(mixyp-mixym)*qiyp1
         dxf2  =dxay*(y-ynod(iy+1))**2+dxby*(y-ynod(iy+1))+dxcy
      endif
      f=f1+f2-lin
      dxf=dxf1+dxf2-dxlin
      dyf=dyf1+dyf2-dylin
   else
!
! point is outside the range of data points;
! bi-linear extrapolation is used now
      if(x.gt.xnod(n))then
         xover = .true.
         if(y.gt.ynod(m))then
!
!     |   | x
!  -----------
!     |   |
!  -----------
!     |   |
!
            yover = .true.
            dxf=(s(n,m)-s(n-1,m))/(xnod(n)-xnod(n-1))
            dyf=(s(n,m)-s(n,m-1))/(ynod(m)-ynod(m-1))
            f  =s(n,m)+dxf*(x-xnod(n))+dyf*(y-ynod(m))
         else if(y.lt.ynod(0))then
            yunder = .true.
!
!     |   |
!  -----------
!     |   |
!  -----------
!     |   | x
!
            dxf=(s(n,0)-s(n-1,0))/(xnod(n)-xnod(n-1))
            dyf=(s(n,1)-s(n,0  ))/(ynod(1)-ynod(0  ))
            f  =s(n,0)+dxf*(x-xnod(n))+dyf*(y-ynod(0))
         else
!
!     |   |
!  -----------
!     |   | x
!  -----------
!     |   |
!
            iy=m
450         iy=iy-1
            if(y.lt.ynod(iy))goto 450
            lambda=(y-ynod(iy))/(ynod(iy+1)-ynod(iy))
            dxf=(lambda*(s(n,iy+1)-s(n-1,iy+1))+(1.0-lambda)*&
            &(s(n,iy)-s(n-1,iy)))/(xnod(n)-xnod(n-1))
            dyf=(s(n,iy+1)-s(n,iy))/(ynod(iy+1)-ynod(iy))
            f  =s(n,iy)+dxf*(x-xnod(n))+dyf*(y-ynod(iy))
         endif
      else if(x.lt.xnod(0))then
         xunder = .true.
         if(y.gt.ynod(m))then
!
!   x |   |
!  -----------
!     |   |
!  -----------
!     |   |
!
            yover = .true.
            dxf=(s(1,m)-s(0,m))/(xnod(1)-xnod(0))
            dyf=(s(0,m)-s(0,m-1))/(ynod(m)-ynod(m-1))
            f  =s(0,m)+dxf*(x-xnod(0))+dyf*(y-ynod(m))
         else if(y.lt.ynod(0))then
!
!     |   |
!  -----------
!     |   |
!  -----------
!   x |   |
!
            yunder = .true.
            dxf=(s(1,0)-s(0,0))/(xnod(1)-xnod(0))
            dyf=(s(0,1)-s(0,0))/(ynod(1)-ynod(0))
            f  =s(0,0)+dxf*(x-xnod(0))+dyf*(y-ynod(0))
         else
!
!     |   |
!  -----------
!   x |   |
!  -----------
!     |   |
!
            iy=m
500         iy=iy-1
            if(y.lt.ynod(iy))goto 500
            lambda=(y-ynod(iy))/(ynod(iy+1)-ynod(iy))
            dxf=(lambda*(s(1,iy+1)-s(0,iy+1))+(1.0-lambda)*&
            &(s(1,iy)-s(0,iy)))/(xnod(1)-xnod(0))
            dyf=(s(0,iy+1)-s(0,iy))/(ynod(iy+1)-ynod(iy))
            f  =s(0,iy)+dxf*(x-xnod(0))+dyf*(y-ynod(iy))
         endif
      else
         ix=n
550      ix=ix-1
         if(x.lt.xnod(ix))goto 550
         tau=(x-xnod(ix))/(xnod(ix+1)-xnod(ix))
         if(y.gt.ynod(m))then
!
!     | x |
!  -----------
!     |   |
!  -----------
!     |   |
!
            yover = .true.
            dxf=(s(ix+1,m)-s(ix,m))/(xnod(ix+1)-xnod(ix))
            dyf=(tau*(s(ix+1,m)-s(ix+1,m-1))+(1.0-tau)*&
            &(s(ix,m)-s(ix,m-1)))/(ynod(m)-ynod(m-1))
            f  =s(ix,m)+dxf*(x-xnod(ix))+dyf*(y-ynod(m))
         else if(y.lt.ynod(0))then
!
!     |   |
!  -----------
!     |   |
!  -----------
!     | x |
!
            yunder = .true.
            dxf=(s(ix+1,0)-s(ix,0))/(xnod(ix+1)-xnod(ix))
            dyf=(tau*(s(ix+1,1)-s(ix+1,0))+(1.0-tau)*&
            &(s(ix,1)-s(ix,0)))/(ynod(1)-ynod(0))
            f  =s(ix,0)+dxf*(x-xnod(ix))+dyf*(y-ynod(0))
         endif
      endif
   endif
!
   return
end
