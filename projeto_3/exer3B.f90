program exer3B

    implicit none
    
    integer :: i, nsteps
    real(8) :: l, g, m, dt, t, t_tot
    real(8) :: theta, omega, E
    real(8), parameter  :: PI = 3.141592653589793238462643383279502884_8

    omega = 0.0_8
    g  = 9.8_8 

    read(*,*) m
    read(*,*) l
    read(*,*) theta
    read(*,*) dt
    read(*,*) t_tot
    
    open(unit=1, file='exer3B_out.dat', status='replace', action='write')
    ! open(unit=2, file='exer3B_energia.dat', status='replace', action='write')

    nsteps = CEILING(t_tot/dt)
    t = 0.0_8
    theta = theta * (PI/180.0_8)
    E = energia_total(theta, omega, l, m, g)
    write(1,'(F12.6,1X,E26.16)') t, theta
    ! write(2,'(F12.6,1X,E26.16)') t, E

    do i = 1, nsteps
    
        omega = omega + ( - (g/L)*theta ) * dt
        theta = theta + omega * dt

        if (theta >  PI) theta = theta - 2.0_8*PI
        if (theta < -PI) theta = theta + 2.0_8*PI

        t = t + dt
        E = energia_total(theta, omega, L, m, g)
        ! write(1,*) t, theta
        ! write(2,*) t, E
        write(1,'(F12.6,1X,E26.16)') t, theta
        ! write(2,'(F12.6,1X,E26.16)') t, E
    end do

    close(1)
    ! close(2)

contains

    real(8) function energia_total(theta, omega, L, m, g) 
        ! Energia total do pêndulo simples (sem amortecimento)
        real(8), intent(in) :: theta, omega, L, m, g
        real(8) :: E

        E = 0.5_8*m*(L*omega)**2 + m*g*L*(1.0_8 - cos(theta))

        energia_total = E

    end function energia_total


end program exer3B
