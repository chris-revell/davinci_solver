!CK Revell, March 2016
!Contains plug treatment subroutine.

module davinci_1_plugtreatment

  use davinci_0_arrays
  use davinci_0_variables

  implicit none

contains

  SUBROUTINE PlugTreatment(n, TotalLayers, v, Acceleration, PlugArray, mu_1)

		INTEGER TotalLayers
		INTEGER n
		REAL, DIMENSION(TotalLayers+2), INTENT(INOUT) :: v
		REAL, DIMENSION(TotalLayers+2), INTENT(INOUT) :: Acceleration
		LOGICAL, DIMENSION(TotalLayers+2), INTENT(INOUT):: PlugArray
		REAL, DIMENSION(TotalLayers+1), INTENT(IN) :: mu_1

		INTEGER i
		INTEGER :: a=0				      	!Plug layer counter. Number of layers in plug.
		INTEGER :: b=0					      !Plug counter after split
		REAL :: PlugCondition = 0.0		!Effectively dv_2. Value allows us to determine the end of a plug.
		REAL PlugAcceleration
		REAL W							          !Pressure applied to top surface of plug
		INTEGER s_bottom				      !Redefine s_1 inside subroutine for the bottom layer of the plug
		INTEGER s_top					        !Redefine s_2 inside subroutine for the top layer of the plug
		REAL F_bottom					        !Force on bottom of plug
		REAL F_top						        !Force on top of plug
		REAL PlugMass					        !PlugMass is the mass of the plug
		REAL MassAbovePlug				    !MassAbovePlug is the mass of fluid above the plug
		REAL RequiredForce				    !Force that must be applied to the top of a layer in the plug to maintain the same acceleration for this layer as for the rest of the plug
		REAL FrictionalForce			    !Maximum force that can be applied at the top of the layer within the plug
		REAL F_F_1						        !Frictional force expression too long, so expression broken into parts
		REAL R_F_1						        !Same for required force

		a=0
		b=0
		PlugCondition=0.0

		!Calculate number of layers in plug
  81	IF ( a .LE. (TotalLayers-n+1)) then
			IF (PlugCondition.LT.epsilon) then
				PlugCondition = ABS(v(n+a+1)-v(n+a))
				a=a+1
				go to 81
			ELSE
			go to 82
			ENDIF
		ELSE
		   go to 82
		ENDIF
 82		continue



		!Set W and MassAbovePlug
		MassAbovePlug = 0
		DO i=n+a, TotalLayers+1
			MassAbovePlug = MassAbovePlug + rho(i)*d
		END DO
		W = MassAbovePlug*g + P_0

		!Set PlugMass
		PlugMass = 0
		DO i=n, (n+a-1)
			PlugMass = PlugMass +rho(i)*d
		END DO


		s_bottom=SIGN(1.0,(v(n)-v(n-1)))						!Signs of velocity differences at the top and bottom of the plug
		s_top=SIGN(1.0,(v(n+a)-v(n+a-1)))

		IF (((n+a-1).EQ.TotalLayers+1).AND.(ABS(v(n+a)-v(n+a-1)).LT.epsilon)) THEN	!Plug stationary relative to upper boundary
			F_bottom=-REAL(s_bottom)*(W+PlugMass*g)*mu_1(n-1)
			F_top=-F_bottom
		ELSE
			F_top=REAL(s_top)*W*mu_1(n+a-1)
		END IF

		IF ((n.EQ.2).AND.(ABS(v(n)-v(n-1)).LT.epsilon)) THEN 		!Condition for plug stationary relative to lower boundary
			IF (ABS(F_top).GT.ABS((W+PlugMass*g)*mu_2)) THEN
				F_bottom=-REAL(s_top)*(W+PlugMass*g)*mu_2
			ELSE
				F_bottom=-F_top			!This is the max value F_bottom can take in this case, as required for splitting calculations. It is not necessarily the force to use for calculating the plug acceleration.
			END IF
		ELSE
			F_bottom=-REAL(s_bottom)*(W+PlugMass*g)*mu_1(n-1)	!Force on bottom of plug for general plug moving relative to lower boundary or not touching lower boundary
		END IF

		PlugAcceleration = (F_top + F_bottom)/PlugMass


		!Also need to consider behaviour if both boundaries are in contact with plug and stationary relative to it
		IF ((((n+a-1).EQ.TotalLayers+1).AND.(ABS(v(n+a)-v(n+a-1)).LT.epsilon)).AND.((n.EQ.1).AND.(ABS(v(n)-v(n-1)).LT.epsilon))) THEN
			PlugAcceleration=0
		END IF


		DO i=0, (a-1)

			F_F_1 = (EXP(-alpha*g*d*(n+i))-EXP(-alpha*g*d*(TotalLayers+1)))/(1-EXP(-alpha*g*d))
			FrictionalForce = mu_2*P_0+mu_2*d*g*K*EXP(alpha*g*d*TotalLayers)*F_F_1								 !Frictional force that actually is applied on the top of layer

			R_F_1 = (EXP(-alpha*g*d*n)-EXP(-alpha*g*d*(n+i+1)))/(1-EXP(-alpha*g*d))
			RequiredForce = d*PlugAcceleration*K*EXP(alpha*g*d*(TotalLayers+1))*R_F_1 - F_bottom 	!Stress that must be applied to the top of the layer under consideration in order to maintain the same acceleration as the rest of the plug give some stress on the bottom

			IF (ABS(FrictionalForce).LT.ABS(RequiredForce)) THEN		!If max frictional force is smaller than that which would be required to maintain the same acceleration in this layer as the rest of the plug
				PlugArray(n+i) = .TRUE.									!This layer is still part of the lower plug
				b=b+1													!Lower plug layer counter incremented
				GO TO 83												!Exit DO statement and do not consider the remaining layers in the original plug or set their PlugArray values to zero.
			ELSE
				PlugArray(n+i) = .TRUE.
				b=b+1
			END IF
		END DO

		!Redefine signs and forces for the new bottom plug. If the plug does not split, a=b so these will work out the same anyway.
		83 CONTINUE
		MassAbovePlug = 0
		DO i=n+b, TotalLayers+1
			MassAbovePlug = MassAbovePlug + rho(i)*d
		END DO
		W = MassAbovePlug*g + P_0

		!Set new PlugMass
		PlugMass = 0
		DO i=n, (n+b-1)
			PlugMass = PlugMass +rho(i)*d
		END DO

		s_bottom=SIGN(1.0,(v(n)-v(n-1)))
		s_top=SIGN(1.0,(v(n+b)-v(n+b-1)))

		F_top=REAL(s_bottom)*W*mu_1(n+b-1)		!No need to redefine F_bottom

		PlugAcceleration = (F_top + F_bottom)/PlugMass


		!The following terms prevent layers from overshooting plug formation

		dv_1 = v(n)-v(n-1)
		dv_2 = v(n+b)-v(n+b-1)
		dv_1_prime = dv_1 + PlugAcceleration*dt
		dv_2_prime = dv_2 - PlugAcceleration*dt

		IF ((dv_1*dv_1_prime).GE.0) THEN
			GO TO 1
		ELSE
			GO TO 2
		END IF

		1 CONTINUE
		IF ((dv_2*dv_2_prime).GE.0) THEN
			DO i=0, b-1
				v(n+i)=v(n+i)+PlugAcceleration*dt
			END DO
		ELSE
			DO i=0, b-1
				v(n+i)=v(n+b)
			END DO
		END IF
		GO TO 3

		2 CONTINUE
		IF ((dv_2*dv_2_prime).GE.0) THEN
			DO i=0, b-1
				v(n+i)=v(n-1)
			END DO
			GO TO 3
		ELSE
			IF ((ABS(dv_2)).LE.(ABS(dv_1))) THEN		!Forms plug with the first one it catches up to
				DO i=0, b-1
					v(n+i)=v(n+b)
				END DO
			ELSE
				DO i=0, b-1
					v(n+i)=v(n-1)
				END DO
			END IF
		END IF

		3 CONTINUE


	END SUBROUTINE PlugTreatment

end module davinci_1_plugtreatment
