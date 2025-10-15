module coords_module
    implicit none
    type coords
        real(8) :: x
        real(8) :: y
        real(8) :: z
        real(8) :: vx
        real(8) :: vy
        real(8) :: vz
    end type coords    

end module coords_module

program exercicio
    implicit none
    type (coords) :: trave_e, trave_d, traj

    ! passando a velocidade inicial de km por hora para metros por segundo
    real(8) :: v = (100.0_8/3.6_8), omega = 39 , x_prev, y_prev, z_prev

    ! parametros gamma
    real(8) :: a1 = 0.0039_8, a2 = 0.0058_8, vd = 35.0_8, delta = 5.0_8

    real(8) :: dt = 0.01_8, g = 9.8_8 , t = 0
    real(8) :: m = 1.0_8, param, theta, phi, gamma
    integer :: i = 1

    open(unit=1, file='chute_out.dat', status='replace', action='write')

    ! definindo limites do travessao do gol de 6m
    trave_e = cords(40.0_8, 10.0_8, 2.5_8)
    trave_d = cords(40.0_8, 4.0_8, 2.5_8)

    ! definindo inicio da trajetoria da bola

    ! theta e phi sao em radianos
    read(*,*) param
    read(*,*) theta
    read(*,*) phi

    traj%x = 0
    traj%y = 0
    traj%z = 0
    x_prev= traj%x
    y_prev= traj%y
    z_prev= traj%z

    traj%vx = v*sin(theta)*cos(phi)
    traj%vy = v*sin(theta)*sin(phi)
    traj%vz = v*cos(theta)
    
    
    do 
        x_prev= traj%x
        y_prev= traj%y
        z_prev= traj%z
        
        traj%x = traj%x + traj%vx * dt
        traj%y = traj%y + traj%vy * dt
        traj%z = traj%z + traj%vz * dt

        ! calculando modulo da velocidade do passo anterior
        v = sqrt(traj%vx**2 + traj%vy**2 + traj%vz**2)

        ! calculando o gamma baseado no v do passo anterior
        gamma = a1 + (a2/(1 + exp((v - vd)/delta)))

        traj%vx = traj%vx - (gamma*v*traj%vx/m + param*omega*traj%vy/m)*dt
        traj%vy = traj%vy - (gamma*v*traj%vy/m - param*omega*traj%vx/m)*dt
        traj%vz = traj%vz - (g + gamma*v*traj%vz/m)*dt

        if ( traj%x >= 40.0_8 ) exit    
        t = t + i*dt
    end do


    
end program exercicio

