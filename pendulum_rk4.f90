module funtzioa

        use mcf_tipos

        public :: F

        contains

        function F(t,y) result(yprima)

        real(kind=dp) , intent(in) :: t
        real(kind=dp) , dimension(:) , intent(in) :: y
        real(kind=dp) , dimension(size(y)) :: yprima
        real(kind=dp) , parameter :: g=9.81_dp , R=1.0_dp

        yprima(1) = y(2)
        yprima(2) = - g / R * sin(y(1))

        end function F

end module funtzioa

program pendulum_rk4

        use mcf_tipos
        use rk4
        use funtzioa

        real(kind=dp) , dimension(2) :: theta
        real(kind=dp) , parameter :: ta=0.0_dp , tb=10.0_dp
        integer , parameter :: N = 300
        real(kind=dp) :: t , h
        real(kind=dp) , parameter :: pi=acos(-1.0_dp)
        real(kind=dp) :: theta1_0=pi/2.0_dp , theta2_0=0.0_dp
        integer :: i

        !HB
        theta(1)=theta1_0
        theta(2)=theta2_0
        t = 0.0_dp
        h = (tb-ta) / N

        open(unit=11,file="pendulum_rk4.dat",action="write",status="replace")

        do i=1,N

        call rk4_pausoa(t,theta,F,h)
        write(unit=11,fmt="(3f16.8)") t , theta(1) , theta(2)

        end do

        close(11)

end program pendulum_rk4
