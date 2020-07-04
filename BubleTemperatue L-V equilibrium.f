!       programme pour calculer la temperature d'ebullition
!        de l equilibre L-V du melange benzene /toluene et la composition
!        du vapeur en fonction du liquide
!        a P0=2280 mmhg
!         les pressions de vapeur saturante de B et T sont donnees a T=100øc et 125øc pour
!        la methode iterative.
        program principal
          common A1, B1, A2, B2
!        evaluation de A1, B1, A2, B2 A T=100 et 125c
           A1=log(2600./1360.)/((1./(125.+273.))-(1./(100.+273.)))
           B1=log(2600.)-A1/(125+273)
           A2=log(1140./550.)/((1./(125.+273.))-(1./(100.+273.)))
           B2=log(1140.)-A2/(125+273)
!        conditions initiales
           write(*,*) 'donner x et Pat'
           read(*,*) x, Pat
           P0=760*Pat
           write(6,3)X, P0
3      format('X=',F 8.5,'P0=', F 8.2/)
          T0=80
          Do  100 NT=1,10
          dT0=2
          do 50 NOT=1,1
!          write(6,1) T0, dT0
!1      format('T=', F 7.2,' dT0=', F 7.1)
          T=T0
          dT=dT0
          P=P0
          call Bubpt(X,T,dT,P,Y,loop)
          write(6,2)T,X,Y,loop
2     Format(F7.2,5x, F7.5,5x,F7.5,5x,I3)
50    dT0=dT0*2
100        X=X+0.1
          stop
          end
        Subroutine Bubpt(X,T,dT,P,Y,loop)
         common A1,B1,A2,B2
         loop=0
         FlagM=-1
         FlagP=-1
!        interaction sur la temperature
100    loop=loop+1
         If (loop.GT.100)then
         write(6,1)
1        Format('le nombre d''iteration elevee')
         Stop
         Endif
         PS1=exp(A1/(T+273)+B1)
         PS2=exp(A2/(T+273)+B2)
         Pcalc=X*PS1+(1-X)*PS2
!        Test de convergence
         If(Abs(P-Pcalc).LT.P/10000) Go To 50
         If(P-Pcalc) 20,20,30
!        Temperature initiale elevee
20       IF(FlagM.GT.0.) dT=dT/2
         T=T-dT
         FlagP=1
         Go To 100
!        Temperature initiale basse
30     IF(FlagP.GT.0.) dT=dT/2
          T=T+dT
          FlagM=1
          Go To 100
50     Y=X*PS1/P
        return
        end
       
        
