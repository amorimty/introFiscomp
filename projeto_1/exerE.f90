program exerE
    ! https://ergodic.ugr.es/cphys/lecciones/fortran/power_method.pdf
    implicit none
    real(8) :: epsilon, lambda, normx, lambdatemp = 0.0_8
    real(8), allocatable :: A(:, :), x(:), y(:)
    integer :: dim, i, j

    read(*,*) epsilon
    
    read(*,*) dim
    
    if(dim > 0) allocate(A(dim, dim), x(dim), y(dim))

    x = 1

    ! call random_number(x)
    
    ! normx = sqrt(dot_product(x,x))
    ! ! normalizando x para evitar um vetor com numeros muito pequenos
    ! x = x/normx


    read *, A


    do i = 1, 100000, 1

        ! Ax
        y = matmul(A, x)
        

        ! calculo do cociente de rayleigh para encontrar o autovalor
        lambda = DOT_PRODUCT(x, y)/ DOT_PRODUCT(x,x)

        ! normalização para proxima iteração
        normx = sqrt(dot_product(y,y))

        if(normx == 0.0_8) then
            write(*,*) "deu ruim na multiplicação de matriz e a matriz y anulou a normalização"
            exit
        end if

        
        ! comparando o incremento do lambda calculado com a iteração anterior
        if((abs(lambda - lambdatemp)) <= epsilon) exit
        
        x = y/normx
        

        lambdatemp = lambda

    end do 

    write (*,*) lambda
    write(*,*) x

    

end program exerE
