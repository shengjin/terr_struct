******************************************************************************
*******************************************************************************
*******************************************************************************
*******************************************************************************
*******************************************************************************
*******************************************************************************

      SUBROUTINE rkqs_planete(y,dydx,n,x,htry,eps,yscal,hdid,hnext,derivs,n_frontieres,hmin)
c------------------------------------------------------------------
      INTEGER n,NMAX
      REAL*8 eps,hdid,hnext,htry,x,dydx(n),y(n),yscal(n)
      PARAMETER (NMAX=50)
      external derivs,rkck
      INTEGER i
      integer nfmax
      parameter (nfmax=10000)
      real*8 hmin
      integer n_frontieres
      dimension Z(1:nfmax)
      REAL*8 errmax,h,htemp,xnew,yerr(NMAX),ytemp(NMAX),SAFETY,PGROW,
     *PSHRNK,ERRCON
      PARAMETER (SAFETY=0.9,PGROW=-.2,PSHRNK=-.25,ERRCON=1.89e-4)
      
      LOGICAL       DEBUG
      PARAMETER     (DEBUG=.FALSE.)
      !PARAMETER     (DEBUG=.FALSE.)

      h=htry
1     call rkck(y,dydx,n,x,h,ytemp,yerr,derivs,n_frontieres)
      errmax=0.
      do 11 i=1,n
        errmax=max(errmax,abs(yerr(i)/yscal(i)))
11    continue
      errmax=errmax/eps

      if((errmax.gt.1D0).AND.(ABS(h/hmin)<1D0))THEN !like in dga
            IF(DEBUG)WRITE(*,*)
            IF(DEBUG)WRITE(*,*)'Maxloc ',maxloc(abs(yerr(1:n)/yscal(1:n)))
            IF(DEBUG)WRITE(*,*) 'WARNING: sbr rkqs: h,errmax=',h,errmax
            IF(DEBUG)WRITE(*,*) 'unphysical smaller than abs. min.=',hmin
            IF(DEBUG)WRITE(*,*) 'ytemp',ytemp(1:n)
            IF(DEBUG)WRITE(*,*) 'yerr ',yerr(1:n)
            IF(DEBUG)WRITE(*,*) 'yscal',yscal(1:n)
            IF(DEBUG)WRITE(*,*) 'ye/ys',(yerr(1:n)/yscal(1:n))/eps
            h=SIGN(hmin,h)
            DO    !check if this is a significant quantity, otherwise enhance h.   
               IF(DEBUG)WRITE(*,*)'xnew,x,h',xnew,x,h,hmin
               xnew=x+h
               IF (xnew /= x)THEN
                  EXIT
               END IF
               IF(DEBUG)WRITE(*,*)'rkqs:h not signi, incr by fact 10',10D0*h
               h=10D0*h
            END DO
            !setting errmax to 0.5 makes that hnext= approx h 
            errmax=0.5D0
            !assume that step was ok (!) (code like below)
            hnext=SAFETY*h*(errmax**PGROW)
            hdid=h
            x=x+h
            do 13 i=1,n
               y(i)=ytemp(i)
 13         continue
            return
 
      ELSE if(errmax.gt.1.)then
        htemp=SAFETY*h*(errmax**PSHRNK)
        h=sign(max(abs(htemp),0.1*abs(h)),h)
        xnew=x+h
        if(xnew.eq.x)then
           write(*,*) 'WARNING : RKQS xnew=x',xnew,x,h,hmin
           STOP !should never occurr ?
        end if
        goto 1

      else
        if(errmax.gt.ERRCON)then
          hnext=SAFETY*h*(errmax**PGROW)
        else
          hnext=5.*h
        endif
        hdid=h
        x=x+h
        do 12 i=1,n
          y(i)=ytemp(i)
12      continue
        return
      endif
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .

*******************************************************************************
*******************************************************************************
*******************************************************************************

      SUBROUTINE rkck(y,dydx,n,x,h,yout,yerr,derivs,n_frontieres)
c----------------------------------------------------
      INTEGER n,NMAX
      REAL*8 h,x,dydx(n),y(n),yerr(n),yout(n)
      PARAMETER (NMAX=50)
      integer nfmax
      parameter (nfmax=10000) 
      external derivs
      INTEGER i
      integer n_frontieres
      REAL*8 ak2(NMAX),ak3(NMAX),ak4(NMAX),ak5(NMAX),ak6(NMAX),
     *ytemp(NMAX),A2,A3,A4,A5,A6,B21,B31,B32,B41,B42,B43,B51,B52,B53,
     *B54,B61,B62,B63,B64,B65,C1,C3,C4,C6,DC1,DC3,DC4,DC5,DC6
      PARAMETER (A2=.2,A3=.3,A4=.6,A5=1.,A6=.875,B21=.2,B31=3./40.,
     *B32=9./40.,B41=.3,B42=-.9,B43=1.2,B51=-11./54.,B52=2.5,
     *B53=-70./27.,B54=35./27.,B61=1631./55296.,B62=175./512.,
     *B63=575./13824.,B64=44275./110592.,B65=253./4096.,C1=37./378.,
     *C3=250./621.,C4=125./594.,C6=512./1771.,DC1=C1-2825./27648.,
     *DC3=C3-18575./48384.,DC4=C4-13525./55296.,DC5=-277./14336.,
     *DC6=C6-.25)
      do 11 i=1,n
        ytemp(i)=y(i)+B21*h*dydx(i)
11    continue
      call derivs(x+A2*h,ytemp,ak2,n_frontieres)
      do 12 i=1,n
        ytemp(i)=y(i)+h*(B31*dydx(i)+B32*ak2(i))
12    continue
      call derivs(x+A3*h,ytemp,ak3,n_frontieres)
      do 13 i=1,n
        ytemp(i)=y(i)+h*(B41*dydx(i)+B42*ak2(i)+B43*ak3(i))
13    continue
      call derivs(x+A4*h,ytemp,ak4,n_frontieres)
      do 14 i=1,n
        ytemp(i)=y(i)+h*(B51*dydx(i)+B52*ak2(i)+B53*ak3(i)+B54*ak4(i))
14    continue
      call derivs(x+A5*h,ytemp,ak5,n_frontieres)
      do 15 i=1,n
        ytemp(i)=y(i)+h*(B61*dydx(i)+B62*ak2(i)+B63*ak3(i)+B64*ak4(i)+
     *B65*ak5(i))
15    continue
      call derivs(x+A6*h,ytemp,ak6,n_frontieres)
      do 16 i=1,n
        yout(i)=y(i)+h*(C1*dydx(i)+C3*ak3(i)+C4*ak4(i)+C6*ak6(i))
16    continue
      do 17 i=1,n
        yerr(i)=h*(DC1*dydx(i)+DC3*ak3(i)+DC4*ak4(i)+DC5*ak5(i)+DC6*
     *ak6(i))
17    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .


*******************************************************************************
*******************************************************************************
*******************************************************************************


      SUBROUTINE odeint_planete2(ystart,nvariables,x1,x2,eps,h1,hmin,nok,nbad,derivs,
     *rkqs,xp,yp,dxsav,kount,n_frontieres)
 
      USE PARAMETERS

      INTEGER nbad,nok,nvariables,MAXSTP
      REAL*8 eps,h1,hmin,x1,x2,ystart(nvar),TINY,omeg
      EXTERNAL derivs,rkqs
      PARAMETER (MAXSTP=90000000,TINY=1.e-30)
      INTEGER i,kmax,kount,nstp
      REAL*8 dxsav,h,hdid,hnext,x,xsav,dydx(NMAX),xp(KMAXX),y(NMAX),
     *yp(NMAX,KMAXX),yscal(NMAX)

      real*8 precision M

      integer n_frontieres

      LOGICAL       DEBUG
      PARAMETER     (DEBUG=.FALSE.)
      !PARAMETER     (DEBUG=.TRUE.)


      real*8 RJ,mearth
      PARAMETER(RJ=7.14e9,Mearth=5.97424e27)
      

      IF(DEBUG)THEN
         WRITE(*,*)
         WRITE(*,*)'SBR ODEINT'
         WRITE(*,*)'ystart',ystart(1),ystart(2)/Mearth,ystart(3)
         ! y1 Pressure, y2 Mtot, y3 density
         WRITE(*,*)'nvariables, hmin',nfmax,nvariables,hmin
         WRITE(*,*)'x1,x2',x1/RJ,x2/RJ
         read(*,*)
      END IF



      kmax=kmaxx

Cwinisdo 05/02/03
      do i=1,NMAX
         y(i)=0.d0
         dydx(i)=0.d0
      enddo   
Cwinisdo 05/02/03

      x=x1
      h=sign(h1,x2-x1)
      nok=0
      nbad=0
      kount=0
      do 11 i=1,nvariables
        y(i)=ystart(i)
11    continue
      if (kmax.gt.0) xsav=x-2.*dxsav
c      write(*,*) 'xsav',xsav
      do 16 nstp=1,MAXSTP
        IF(DEBUG)WRITE(*,*)'before calling derivs for yscal'
        call derivs(x,y,dydx,n_frontieres)
        do 12 i=1,nvariables
          yscal(i)=abs(y(i))+abs(h*dydx(i))+TINY
12      continue
c       CM 11.11.10
        
        if(kmax.gt.0)then
          if(abs(x-xsav).gt.abs(dxsav)) then
            if(kount.lt.kmax-1)then
              kount=kount+1
              xp(kount)=x
              do 13 i=1,nvariables
                yp(i,kount)=y(i)
13            continue
              xsav=x
            endif
          endif
        endif
        if((x+h-x2)*(x+h-x1).gt.0.) h=x2-x
Cwinisdo 05/02/03
c        write(*,*) 'rkqs1',y,dydx,nvariables,x,h,eps,yscal,hdid,hnext
        call rkqs(y,dydx,nvariables,x,h,eps,yscal,hdid,hnext,derivs,
     &  n_frontieres,hmin)
c        write(*,*) 'rkqs2',y,dydx
Cwinisdo 05/02/03
c        write(*,*) x,log10(y(1)),log10(y(2)),log10(y(3)),log10(y(4))
        if(hdid.eq.h)then
          nok=nok+1
        else
          nbad=nbad+1
        endif
        if((x-x2)*(x2-x1).ge.0.)then
          do 14 i=1,nvariables
            ystart(i)=y(i)
14        continue
          if(kmax.ne.0)then
            kount=kount+1
            xp(kount)=x
            do 15 i=1,nvariables
              yp(i,kount)=y(i)
15          continue
          endif
          IF(DEBUG)WRITE(*,*)'Odeint exit first kind. nstp',nstp,nok,nbad,h
          return
        endif
!        if ((y(2)-0.5*Mcore).lt.-precision M) then          
!           kount=kount+1                        !
!           xp(kount)=x                          !
!           do i=1,nvar                          !
!              yp(i,kount)=y(i)                  !
!           enddo                                !
!           goto 2                               !
!        endif                                   !
        if(abs(hnext).lt.abs(hmin))then
           IF(DEBUG)THEN
           write(*,*) 'hnext, hmin',hnext,hmin 
           WRITE(*,*) 'hmin reached',MAXSTP,nstp,nok,nbad
           WRITE(*,*) 'x,x1,x2,h',x/RJ,x1/RJ,x2/RJ,h/RJ
           WRITE(*,*) 'y(1),y(2),y(3)',y(1),y(2)/Mearth,y(3)
           write(*,*) 'WARNING : ODEINT_PLANETE 2 hmin'
           END IF
           hnext=hmin
        end if
        h=hnext
16    continue
      write(*,*)'not die'
      WRITE(*,*) 'MAXSTEP reached',MAXSTP,nstp,nok,nbad
      WRITE(*,*) 'x,x1,x2,h',x/RJ,x1/RJ,x2/RJ,h/RJ
      WRITE(*,*) 'y(1),y(2)',y(1),y(2)
      write(*,*) 'PROBLEME : ODEINT_PLANETE 2 BIS' 
      stop 
! 2    continue
      IF(DEBUG)WRITE(*,*)'Odeint exit second kind, nstp',nstp,nok,nbad,h
      RETURN
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .


*******************************************************************************
*******************************************************************************
*******************************************************************************
