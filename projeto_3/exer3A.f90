program exer3

    implicit none
    
    integer              :: i, nsteps
    real(16)             :: l, g, m, dt, t, t_tot
    real(16)             :: theta, omega, E, theta_old
    real(16), parameter  :: PI = 3.141592653589793238462643383279502884_16

    omega = 0.0_16
    g  = 9.8_16 

    read(*,*) m
    read(*,*) l
    read(*,*) theta
    read(*,*) dt
    read(*,*) t_tot
    
    open(unit=1, file='exer3A_out.dat', status='replace', action='write')
    open(unit=2, file='exer3A_energia.dat', status='replace', action='write')

    nsteps = CEILING(t_tot/dt)
    t = 0.0_16
    E = energia_total(theta, omega, l, m, g)
    write(1,*) t, theta
    write(2,*) t, E

    do i = 1, nsteps
        theta_old = theta
        theta = theta + omega * dt                               
        omega = omega + ( - (g/L)*sind(theta_old) ) * dt          

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


program exer3A
  implicit none
  ! ===== Precisão dupla =====
  integer, parameter :: dp = kind(1.0d0)
  real(dp), parameter :: PI = 3.1415926535897932384626433832795_dp

  ! ===== Parâmetros físicos e numéricos =====
  real(dp), parameter :: m = 1.0_dp      ! massa [kg]
  real(dp), parameter :: L = 1.0_dp      ! comprimento [m]
  real(dp), parameter :: g = 9.81_dp     ! gravidade [m/s^2]
  real(dp), parameter :: dt = 0.04_dp    ! passo de tempo [s]
  real(dp), parameter :: t_end = 10.0_dp ! duração [s]

  ! ===== Estado dinâmico =====
  real(dp) :: t, theta, omega, E
  integer  :: n, nsteps

  ! ===== Arquivo de saída =====
  integer, parameter :: u = 10

  ! Condições iniciais (theta0 = 10 graus; omega0 = 0)
  theta = 10.0_dp * (PI/180.0_dp)   ! rad
  omega = 0.0_dp

  ! Número de passos (incluindo t=0 na saída)
  nsteps = int(t_end/dt + 0.5_dp)

  open(unit=u, file='output3A.txt', status='replace', action='write')
  write(u,'(A)') '# t   theta(rad)   omega(rad/s)   Energia(J)'

  t = 0.0_dp
  E = energia_total(theta, omega, L, m, g)
  write(u,'(4ES25.16)') t, theta, omega, E

  do n = 1, nsteps
     ! ===== Método de EULER (explícito) =====
     ! Sistema de 1ª ordem:
     !   dtheta/dt = omega
     !   domega/dt = - (g/L) * sin(theta)
     theta = theta + dt * omega
     omega = omega + dt * ( - (g/L) * sin(theta - dt*omega) )
     ! Observação: a linha acima usa theta_{n} na aceleração.
     ! Se preferir a forma escrita literalmente em Euler:
     !   theta = theta + dt * omega
     !   omega = omega + dt * ( - (g/L) * sin(theta_old) )
     ! basta guardar theta_old antes de atualizá-lo.

     t = t + dt
     E = energia_total(theta, omega, L, m, g)
     write(u,'(4ES25.16)') t, theta, omega, E
  end do

  close(u)
  print *, 'OK! Trajetória (Euler) salva em output3A.txt'

contains

  pure function energia_total(theta, omega, L, m, g) result(E)
    ! Energia T + V do pêndulo simples (sem amortecimento)
    real(dp), intent(in) :: theta, omega, L, m, g
    real(dp) :: E
    E = 0.5_dp*m*(L*omega)**2 + m*g*L*(1.0_dp - cos(theta))
  end function energia_total

end program exer3A
