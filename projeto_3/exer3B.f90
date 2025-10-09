program exer3

    implicit none
    
    integer              :: i, nsteps
    real(16)             :: l, g, m, dt, t, t_tot
    real(16)             :: theta, omega, E
    real(16), parameter  :: PI = 3.141592653589793238462643383279502884_16

    omega = 0.0_16
    g  = 9.8_16 

    read(*,*) m
    read(*,*) l
    read(*,*) theta
    read(*,*) dt
    read(*,*) t_tot
    
    open(unit=1, file='exer3B_out.dat', status='replace', action='write')
    open(unit=2, file='exer3B_energia.dat', status='replace', action='write')

    nsteps = CEILING(t_tot/dt)
    t = 0.0_16
    E = energia_total(theta, omega, l, m, g)
    write(1,*) t, theta
    write(2,*) t, E

    do i = 1, nsteps
        omega = omega + ( - (g/L)*sin(theta) ) * dt       ! atualiza velocidade angular
        theta = theta + omega * dt                        ! atualiza angulo com a nova velocidade

        if (theta >  PI) theta = theta - 2.0_16*PI
        if (theta < -PI) theta = theta + 2.0_16*PI

        t = t + dt
        E = energia_total(theta, omega, L, m, g)
        write(1,*) t, theta
        write(2,*) t, E

    end do

    close(1)
    close(2)

contains

    real(16) function energia_total(theta, omega, L, m, g) 
        ! Energia total do pêndulo simples (sem amortecimento)
        real(16), intent(in) :: theta, omega, L, m, g
        real(16) :: E

        E = 0.5_16*m*(L*omega)**2 + m*g*L*(1.0_16 - cosd(theta))

        energia_total = E

    end function energia_total


end program exer3
