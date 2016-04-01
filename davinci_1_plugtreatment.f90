!CK Revell, March 2016
!Contains plug treatment subroutine.

module davinci_1_plugtreatment

  use davinci_0_arrays
  use davinci_0_variables

  implicit none

contains

  SUBROUTINE PlugTreatment(n)

    INTEGER, INTENT(IN) :: n      !Layer at which PlugTreatment is called
		INTEGER :: i
    INTEGER :: j
		INTEGER :: a=0				      	!Plug layer counter. Number of layers in plug.
		INTEGER :: b=0					      !Plug counter after split
		REAL    :: PlugCondition  		!Effectively dv_upper. Value allows us to determine the end of a plug.
		REAL    :: PlugAcceleration
		REAL    :: normalforce		    !Pressure applied to top surface of plug
		INTEGER :: s_bottom				    !Redefine sign_lower inside subroutine for the bottom layer of the plug
		INTEGER :: s_top					    !Redefine sign_upper inside subroutine for the top layer of the plug
		REAL    :: F_bottom					  !Force on bottom of plug
		REAL    :: F_top						  !Force on top of plug
		REAL    :: PlugMass					  !PlugMass is the mass of the plug
		REAL    :: MassAbovePlug			!MassAbovePlug is the mass of fluid above the plug


		REAL    :: RequiredForce			!Force that must be applied to the top of a layer in the plug to maintain the same acceleration for this layer as for the rest of the plug
!		REAL    :: FrictionalForce		!Maximum force that can be applied at the top of the layer within the plug
!		REAL    :: F_F_1						  !Frictional force expression too long, so expression broken into parts
!		REAL    :: R_F_1						  !Same for required force
    REAL    :: MaxForce

		a=1
		b=0
!		PlugCondition=0.0

		!Calculate number of layers in plug. Keep adding incrementing a for as many
    !layers as satisfy the condition that upper boundary velocity difference is less than epsilon (and so approcimately equal to zero)
    DO WHILE(ABS(v(n+a)-v(n+a-1)).LT.epsilon)
      PlugArray(n+a) = .TRUE.
      a=a+1
    ENDDO
    !a now equals the number of layers in the plug
    !All elements from n to n+a in PlugArray are now = .TRUE.

		!Find mass above plug and calculate normalforce on plug
		MassAbovePlug = 0
		DO i=n+a, TotalLayers+1
			MassAbovePlug = MassAbovePlug + rho(i)*d
		END DO
		Normalforce = MassAbovePlug*g + P_0

		!Set PlugMass
		PlugMass = 0
		DO i=n, (n+a-1)
			PlugMass = PlugMass +rho(i)*d
		END DO

    !Signs of velocity differences at the top and bottom of the plug
		s_bottom=SIGN(1.0,(v(n)-v(n-1)))
		s_top=SIGN(1.0,(v(n+a)-v(n+a-1)))

    !When calculating acceleration of plug, must consider several conditions
    !Firstly, plug stationary relative to upper boundary
		IF (((n+a).EQ.TotalLayers+2).AND.(ABS(v(n+a)-v(n+a-1)).LT.epsilon)) THEN
      F_top    = REAL(s_bottom)*MIN(ABS(F_bottom), normalforce*mu_2)         !Force on top of plug is equal to -force on bottom of plug, up to maximum of normalforce*mu_2 (static friction)
			F_bottom = -REAL(s_bottom)*(normalforce+PlugMass*g)*mu_1(n-1)          !Force on bottom of plug is as usual
    !Secondly, plug stationary relative to lower boundary
    ELSEIF ((n.EQ.2).AND.(ABS(v(n)-v(n-1)).LT.epsilon)) THEN
      F_top    = REAL(s_top)*normalforce*mu_1(n+a-1)                        !Force on top of plug from dynamic friction with layer moving relative to plug.
      F_bottom = REAL(s_top)*MIN(ABS(F_top), (normalforce+PlugMass*g)*mu_2) !Force on bottom of plug from static frictio, equal and opposite to force on top of plug up to the maximum value of static friction.
    !Thirdly, plug stationary relative to and in contact with both boundaries
    ELSEIF ((((n+a).EQ.TotalLayers+2).AND.(ABS(v(n+a)-v(n+a-1)).LT.epsilon)).AND.((n.EQ.2).AND.(ABS(v(n)-v(n-1)).LT.epsilon))) THEN
      F_bottom = 0      !No force on either edge of the plug
      F_top    = 0
    !Otherwise, no interactions with boundaries so can calculate acceleeration as usual
    ELSE
      F_top    = REAL(s_top)*normalforce*mu_1(n+a-1)
      F_bottom = -REAL(s_bottom)*(normalforce+PlugMass*g)*mu_1(n-1)
    ENDIF

    !Calculate plug acceleration from forces on either edge of plug.
		PlugAcceleration = (F_top + F_bottom)/PlugMass

!This section checks for plug splitting
!****************************************************************************************

		DO i=0, (a-1)

!			F_F_1 = (EXP(-alpha*g*d*(n+i))-EXP(-alpha*g*d*(TotalLayers+1)))/(1-EXP(-alpha*g*d))
!			FrictionalForce = mu_2*P_0+mu_2*d*g*K*EXP(alpha*g*d*TotalLayers)*F_F_1								 !Frictional force that actually is applied on the top of layer

!			R_F_1 = (EXP(-alpha*g*d*n)-EXP(-alpha*g*d*(n+i+1)))/(1-EXP(-alpha*g*d))
!			RequiredForce = d*PlugAcceleration*K*EXP(alpha*g*d*(TotalLayers+1))*R_F_1 - F_bottom 	!Stress that must be applied to the top of the layer under consideration in order to maintain the same acceleration as the rest of the plug give some stress on the bottom

      !Force on the top of a given layer in the plug required to combine with the force on the bottom of the plug and give this layer the same acceleration as the plug.
      RequiredForce = ABS(PlugAcceleration*rho(n+i)*d-F_bottom)

      !Calculate the max possible force on the top of the layer from static friction
      !First compute normal force on upper edge of layer
      MassAboveLayer=0
      Do j=(n+i+1), TotalLayers+1
        MassAboveLayer = MassAboveLayer + rho(j)*d
      END DO
      P = P_0 + MassAboveLayer*g
      !Calculate max force using normal force and static friction
      MaxForce = mu_1*P

			IF (MaxForce.LT.RequiredForce) THEN		!If max frictional force is smaller than that which would be required to maintain the same acceleration in this layer as the rest of the plug
				PlugArray(n+i) = .TRUE.									!This layer is still part of the lower plug
				b=b+1													!Lower plug layer counter incremented
				EXIT												!Exit DO statement and do not consider the remaining layers in the original plug or set their PlugArray values to zero.
			ELSE
				PlugArray(n+i) = .TRUE.
				b=b+1
			END IF
		END DO

		!Redefine signs and forces for the new bottom plug. If the plug does not split, a=b so these will work out the same anyway.
		MassAbovePlug = 0
		DO i=n+b, TotalLayers+1
			MassAbovePlug = MassAbovePlug + rho(i)*d
		END DO
		normalforce = MassAbovePlug*g + P_0

		!Set new PlugMass
		PlugMass = 0
		DO i=n, (n+b-1)
			PlugMass = PlugMass +rho(i)*d
		END DO

		s_bottom=SIGN(1.0,(v(n)-v(n-1)))
		s_top=SIGN(1.0,(v(n+b)-v(n+b-1)))

		F_top=REAL(s_bottom)*normalforce*mu_1(n+b-1)		!No need to redefine F_bottom

		PlugAcceleration = (F_top + F_bottom)/PlugMass

    !Set acceleration of layers within the plug
    do i = n, n+b-1
      Acceleration(i) = PlugAcceleration
    enddo
    !Note that this sets acceleration for all plug layers to be equal, but *does not* set their speeds to be equal. This will not cause splitting because relative velocities are preserved, but should perhaps be changed. Exactly what speed to choose to set all layers to remains undetermined.


!This section repeated in velocityupdate?
!***************************************************************************
		!The following terms prevent layers from overshooting plug formation

		dv_lower = v(n)-v(n-1)
		dv_upper = v(n+b)-v(n+b-1)
		dv_lower_prime = dv_lower + PlugAcceleration*dt
		dv_upper_prime = dv_upper - PlugAcceleration*dt

		IF ((dv_lower*dv_lower_prime).GE.0) THEN
			GO TO 1
		ELSE
			GO TO 2
		END IF

		1 CONTINUE
		IF ((dv_upper*dv_upper_prime).GE.0) THEN
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
		IF ((dv_upper*dv_upper_prime).GE.0) THEN
			DO i=0, b-1
				v(n+i)=v(n-1)
			END DO
			GO TO 3
		ELSE
			IF ((ABS(dv_upper)).LE.(ABS(dv_lower))) THEN		!Forms plug with the first one it catches up to
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
