program pendulum

        use mcf_tipos

        !EULER METHOD

        real(kind=dp) , parameter :: pi=acos(-1.0_dp) , g=9.81_dp , R=1.0_dp
        real(kind=dp) , parameter :: theta0 = pi/2.0_dp , w0=0.0_dp ! Initial angle and velocity
        real(kind=dp) , parameter :: ta=0.0_dp , tb=10.0_dp ! Time
        real(kind=dp) :: h , theta , w , t
        integer , parameter :: N=300
        integer :: i

        !INITIAL CONDITIONS

        theta = theta0
        w = w0
        t = ta

        h = (tb-ta)/N !step

        open(unit=11,file="pendulum_euler.dat",action="write",status="replace")

        do i=1,N+1

        t = ta + (i-1)*h
        theta = theta + h*w
        w = w + h*(F(theta))

        write(unit=11,fmt="(3f16.8)") t , theta , w

        end do

        close(11)

        contains

        function F(theta)

        real(kind=dp) , intent(in) :: theta
        real(kind=dp) :: F
        
        F = - g / R * sin(theta)

        end function F

end program pendulum

