!CK Revell, March 2016
!Subroutine containing module to iterate system over all timesteps

module davinci_2_iterate

  use davinci_0_arrays
  use davinci_0_variables
  use davinci_1_densityrelation
  use davinci_1_frictionrelation
  use davinci_1_plugtreatment
  use davinci_1_output

  implicit none

contains

  subroutine iterate

    !Calculate velocity profile at all remaining timesteps and write to file.
    DO t=2, TotalTime+1									!Time ticker must run from 1 since this is the index of the first array element

      !Refresh PlugArray for each new timestep
      Do n=1, TotalLayers+2
        PlugArray(n)=.FALSE.
      END DO

      !Refresh acceleration array
      Do n=1, TotalLayers+2
        Acceleration(n)=0.0
      END DO


      DO n=2, TotalLayers+1		!Note starts from 2 and ends at TotalLayers+1 because the n=1 and n=TotalLayers+2 layers are boundaries whose velocities are controlled externally and do not change.

        !Compute velocity differences
        dv_1=v(n)-v(n-1)
        dv_2=v(n+1)-v(n)

        !Compute signs of upper and lower velocity differences
        s_1=SIGN(1.0,dv_1)
        s_2=SIGN(1.0,dv_2)


        !Compute normal force on upper edge of layer n
        MassAboveLayer=0
        Do i=(n+1), TotalLayers+1
          MassAboveLayer = MassAboveLayer + rho(i)*d
        END DO
        P=P_0+MassAboveLayer*g

        IF (PlugArray(n)) THEN
          !Do nothing and skip to next layer
          go to 70
        ELSE
          IF (ABS(dv_2).LE.epsilon) THEN	!Plug, so use plug treatment to find acceleration
            CALL PlugTreatment(n, TotalLayers, v, Acceleration, PlugArray, mu_1)
          ELSE							!Not plug, so compute acceleration from forces
            F_1=-s_1*(P+rho(n)*g*d)*mu_1(n-1)
            F_2=s_2*P*mu_1(n)
            Acceleration(n) = (F_1+F_2)/(rho(n)*d)
          END IF
        END IF
        !Now we should have an acceleration array with values correct up to at least layer n and possibly higher
        70	continue
      END DO

      !The following terms prevent layers from overshooting plug formation
      Do n=2, TotalLayers+1
        IF (PlugArray(n)) THEN
          GO TO 17
        ELSE
          dv_1 = v(n)-v(n-1)
          dv_2 = v(n+1)-v(n)
          dv_1_prime = dv_1 + Acceleration(n)*dt
          dv_2_prime = dv_2 - Acceleration(n)*dt

          IF ((dv_1*dv_1_prime).GE.0) THEN
            GO TO 1
          ELSE
            GO TO 2
          END IF

          1 CONTINUE
          IF ((dv_2*dv_2_prime).GE.0) THEN
            v(n)=v(n)+Acceleration(n)*dt
          ELSE
            v(n)=v(n+1)
          END IF
          GO TO 3

          2 CONTINUE
          IF ((dv_2*dv_2_prime).GE.0) THEN
            v(n)=v(n-1)
            GO TO 3
          ELSE
            IF ((ABS(dv_2)).LE.(ABS(dv_1))) THEN
              v(n)=v(n+1)
            ELSE
              v(n)=v(n-1)
            END IF
          END IF

          3 CONTINUE
        END IF
        17 CONTINUE


      END DO

      !Recalculate density array
      Do n=2, TotalLayers+1
        rho(n) = DensityRelation(n)
      END DO

      !Recalculate dynamic friction coefficient array
      Do n=1, TotalLayers+1
        mu_1(n) = FrictionRelation(rho(n), rho(n+1))
      END DO

      !Output data to file
      WRITE (6,*) (v(n), n=1,TotalLayers+2)

    END DO

    call output 

  end subroutine iterate
end module davinci_2_iterate
