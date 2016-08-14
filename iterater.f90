SUBROUTINE iterater(Pout,X,N_frontieres,Rstart)


USE PARAMETERS
USE FRACT

IMPLICIT NONE

EXTERNAL  derivee_core,rkqs_planete

!IO
INCLUDE 'units.h90'
DOUBLE PRECISION,INTENT(IN)                           ::Pout
INTEGER,INTENT(OUT)                                   ::n_frontieres
DOUBLE PRECISION,DIMENSION(nvar+1,nfmax),INTENT(INOUT)  ::X

!Params
INTEGER, PARAMETER            ::Nshootmax=1000,nvariables=2

!numerical values for integration
DOUBLE PRECISION              ::eps,dr_min,dr_start,dxsav

!various
INTEGER                                ::compteur_shooting,nbad,nok,kount,ii,iboundary
DOUBLE PRECISION                       ::Rstart,Xstart,Xend,diff,Rinf,Rsup
DOUBLE PRECISION                       ::Rinfmin,Rsupmax,Rtotal,Rtotalold
DOUBLE PRECISION                       ::density_out
DOUBLE PRECISION                       ::diffMendinf,diffMendsup,dx,Rmid,Diffmax
DOUBLE PRECISION,DIMENSION(nvariables) ::ystart 
DOUBLE PRECISION,DIMENSION(KMAXX)      ::xp
DOUBLE PRECISION,DIMENSION(NMAX,KMAXX) ::yp


!last structure
LOGICAL,PARAMETER                      ::DEBUG=.FALSE.
LOGICAL,PARAMETER                      ::DDEBUG=.TRUE.

!probleme limite maxi/mini
INTEGER                                ::iatmin,iatmax
LOGICAL                                ::Rinfok,Rsupok



iatmin=0
iatmax=0
n_frontieres=2


IF(DEBUG)THEN
   WRITE(*,*)'SBR ITERATER'
   WRITE(*,*)'Mtot/mearth       ',Mtot/mearth
   WRITE(*,*)'Pout/Bar          ',Pout/Bar
   WRITE(*,*)
END IF


IF(DDEBUG)WRITE(*,'(1A5,15A11)')' MAX ','Rmid','diff/Me','Pout','nok','nbad','kount'

!------------------------------
! divers ingredients numeriques
!------------------------------
eps         =  1.d-8                      ! facteur multiplicatif de toutes les precisions
!eps         =  1.d-10                      ! facteur multiplicatif de toutes les precisions
dr_min      =  -RJ/7D8                     ! minimum -0.1 meters. Don't set to 0d0!
dr_start    =  -RJ/2D6                     ! pas de depart -35 meters
dxsav       =  1D-5 * Rstart
Diffmax     =  1D-6                        !No difference if it was set to 1D-8


!Boundaries for root search.
!absolutly smallest and largest allowed ones. Used on the first two steps
!It is not a priory excluded that the prg will fail with these boundaries. 
Rinfmin=0.001D0*Rstart 

Rsupmax=100D0*Rstart

Rinf=Rstart 
Rsup=Rsupmax
Rtotalold=Rstart

IF(DEBUG)THEN
   WRITE(*,*)'Rstart/Rearth    ',Rstart/Rearth    
   WRITE(*,*)'Rinfmin/Rearth   ',Rinfmin/Rearth
   WRITE(*,*)'Rinf/Rearth      ',Rinf/Rearth
   WRITE(*,*)'Rsupmax/Rearth   ',Rsupmax/Rearth 
   WRITE(*,*)'Rtotalold/Rearth ',Rtotalold/Rearth
   WRITE(*,*)
END IF

iboundary=1    !we first try with smaller limits. If there is no root in the first try, we take large limits

Rinfok=.FALSE.
Rsupok=.FALSE.

!to be removed at the last step
100 CONTINUE


IF(.NOT.Rinfok)THEN
     Rinf=0.95**iboundary*Rtotalold !0.95 used before
END IF

   
IF(.NOT.Rsupok)THEN
   Rsup=1.05**iboundary*Rtotalold !1.05 used before
END IF


!---------------
!The hard limits
!---------------
IF(Rinf<Rinfmin)THEN
   IF(DEBUG)WRITE(*,*)'Rinf<Rinfmin',Rinf/Rearth,Rinfmin/Rearth,iatmin
   Rinf=Rinfmin
   iatmin=iatmin+1
END IF

IF(Rsup>Rsupmax)THEN
   IF(DEBUG)WRITE(*,*)'Rsup>Rsupmax',Rsup/Rearth,Rsupmax/Rearth,iatmax
   Rsup=Rsupmax
   iatmax=iatmax+1
END IF

IF(Rsup<Rinf)THEN
   WRITE(*,*)'Rsup<Rinf: should never occur'
   Rsup=1.1*Rinf
   READ(*,*)
END IF



IF(.NOT.Rinfok)THEN
IF(DEBUG)WRITE(*,*)'Evaluate at Rinf/Rearth',Rinf/Rearth
!-----------------
!1)evalute at Rinf
!-----------------
!lower limit for iteration

!Density at the outmost shell
density_out=0d0

!valeurs exterieures
ystart(1) = Pout           ! premiere variable = pression
ystart(2) = Mtot           ! deuxieme variable = Mass

Xstart = Rinf
Xend   = 10d0

IF(DEBUG)THEN
   WRITE(*,*)'Pout  ',Pout
   WRITE(*,*)'Mtot  ',Mtot/Mearth
   WRITE(*,*)'Rinf  ',Rinf/Rearth
   WRITE(*,*)'Xstart',Xstart/Rearth
   WRITE(*,*)
END IF

   
CALL ODEINT_PLANETE2(ystart,nvariables,Xstart,Xend,eps,dr_start,dr_min,nok,nbad,derivee_core,&
     rkqs_planete,xp,yp,dxsav,kount,n_frontieres)

diffMendinf = yp(2,kount)

IF(DDEBUG)WRITE(*,'(1A4,1G15.6,14G11.3)')' LOW:',Xstart/Rearth,diffMendinf/Mearth,Pout,nok,nbad,kount


IF(diffMendinf>0d0)THEN
   Rinfok=.TRUE.
END IF

END IF !Rinfok



!-----------------
!2)evalute at Rsup
!-----------------
!upper limit for iteration

IF(.NOT.Rsupok)THEN
IF(DEBUG)WRITE(*,*)'Evaluate at Rsup/Rearth',Rsup/Rearth

!Density at the outmost shell
density_out=0d0

!valeurs exterieures
ystart(1) = Pout           ! premiere variable = pression
ystart(2) = Mtot           ! deuxieme variable = Mass


Xstart = Rsup
Xend   = 10d0

IF(DEBUG)THEN
   WRITE(*,*)'Pout  ',Pout
   WRITE(*,*)'Mtot  ',Mtot/Mearth
   WRITE(*,*)'Rinf  ',Rinf/Rearth
   WRITE(*,*)'Xstart',Xstart/Rearth
   WRITE(*,*)
END IF

CALL ODEINT_PLANETE2(ystart,nvariables,Xstart,Xend,eps,dr_start,dr_min,nok,nbad,derivee_core,&
     rkqs_planete,xp,yp,dxsav,kount,n_frontieres)

diffMendsup = yp(2,kount)


IF(DDEBUG)WRITE(*,'(1A4,1G15.6,14G11.3)')' UP:',Xstart/Rearth,diffMendsup/Mearth,Pout,nok,nbad,kount


IF(diffMendsup<0d0)THEN
   Rsupok=.TRUE.
END IF

END IF !Rsupok


!----------------------------
!check if bracketing the root
!----------------------------
IF(diffMendinf*diffMendsup>=0D0)THEN ! 
   IF(DEBUG)WRITE(*,*)'Failed with boundaries. Take larger boundaires.',iboundary
   iboundary=iboundary+1
   IF((Rinf==Rinfmin).OR.(Rsup==Rsupmax))THEN
      WRITE(*,*)'Root not bracketed in iterater with max allowed values',iatmin,iatmax
      WRITE(*,*)'Mtot/Me,Pout',Mtot/Mearth,Pout
      WRITE(*,*)diffMendinf/Mearth,diffMendsup/Mearth
      WRITE(*,*)iboundary
      WRITE(*,*)Rinf/Rearth,Rinfmin/Rearth,Rsup/Rearth,Rsupmax/Rearth
      WRITE(*,*)kount,nok,nbad
      WRITE(*,*)'PROBLEME : ITERATERINIT limites'
      STOP
   END IF
   GOTO 100  ! we go back and try again, but now with larger boundaries.
END IF


!"direction" of the bisection
IF (diffMendinf < 0.0) THEN
   Rtotal=Rinf
   dx=Rsup-Rinf
ELSE
   Rtotal=Rsup
   dx=Rinf-Rsup
END IF



!--------------
!3)Do bisection
!--------------
DO compteur_shooting = 1, Nshootmax

   dx=dx*0.5D0
   Rmid=Rtotal+dx

   !---------------------
   ! valeurs exterieures
   !---------------------

   !Density at the outmost shell
   density_out=0d0

   !valeurs exterieures
   ystart(1) = Pout           ! premiere variable = pression
   ystart(2) = Mtot           ! deuxieme variable = Mass

   !-----------------------
   ! calcul de la structure
   !-----------------------
   Xstart = Rmid
   Xend   = 10d0
 

   CALL ODEINT_PLANETE2(ystart,nvariables,Xstart,Xend,eps,dr_start,dr_min,nok,nbad,derivee_core,&
        rkqs_planete,xp,yp,dxsav,kount,n_frontieres)
 
   diff = yp(2,kount)
   
   IF (diff <= 0.0) Rtotal=Rmid

   IF(DDEBUG)THEN
   !   write(*,'(1A4,13G12.4)')'MAX',kount,nok,nbad
      WRITE(*,'(1A4,1G15.6,14G11.3)')'MAX:',Rmid/Rearth,diff/Mearth,Pout,nok,nbad,kount
   END IF


   !Found root to sufficient precision
   IF ((ABS(diff) < Diffmax*(Mtot) .OR. diff == 0.0) .AND. ABS(dx)<0.005D0*Rstart )THEN
       EXIT 
   END IF
   !high precision is not so important here
   !but avoid spurios convergence for very massive planets (dx<Rearth term).
END DO


IF(compteur_shooting==Nshootmax)THEN
   WRITE(*,*)'Max: no structure found'
   WRITE(*,*)'PROBLEME : ITERATER Nshootmax'
   WRITE(*,*)Rmid/Rearth,diff/Mearth,Pout,nok,nbad,kount
   STOP
END IF

!fin du calcul
IF(DDEBUG)THEN
   WRITE(*,*)'Structure converged: diff/Mearth, dx/Rearth',diff/Mearth,dx/Rearth
   WRITE(*,*)'Rtotal/Rearth',Rtotal/Rearth
   WRITE(*,*)'iboundary,nstructures',iboundary,2*iboundary+compteur_shooting
END IF


!force yp(3,kount)=mcore to the "right" value (it is not exactly that because of diff>0.
!yp(2,kount)=0d0


!Set the new n_frontieres
n_frontieres=kount

DO ii=1,n_frontieres
   X(1,(ii-1)+ 1 ) = xp(n_frontieres-ii+1)   !r
   X(2,(ii-1)+ 1 ) = yp(1,n_frontieres-ii+1) !P 
   X(3,(ii-1)+ 1 ) = yp(2,n_frontieres-ii+1) !M
END DO

IF(DDEBUG)THEN
   WRITE(*,*)
   WRITE(*,*)'SBR ITERATER'
   WRITE(*,*)'Structure found'
   WRITE(*,*)'Mtot/Me  ',X(3,n_frontieres)/Mearth
   WRITE(*,*)'Rtot/Rearth',X(1,n_frontieres)/Rearth 
   WRITE(*,*)'Pout/bar',X(2,n_frontieres)/1D6 
   WRITE(*,*)
END IF

RETURN
END SUBROUTINE iterater

