program exer1

    implicit none
    real(16) :: resultados(7), coef = (1 + (1/2)**2), d1, d2, d3
    real(16), allocatable :: h(:)
    integer :: n, i

    d1 = 2*(1/2)/(1+(1/2)**2)
    d2 = 2*(1-(1/2)**2)/(1+(1/2)**2)**2
    d3 = 4*(1/2)*((1/2)**3 - 3)/(coef)**3

    open(1, file='/home/a11795871/Documentos/introFiscomp/projeto_2/tab1_in.dat', status='old')
    open(2, file='/home/a11795871/Documentos/introFiscomp/projeto_2/tab1_out.dat', status='replace')
    
    
    
    read(1,*) n
    write(*,*) n

    allocate(h(n))

    read(1,*) h

    write(*,*) h

    do i = 1, n, 1
        resultados(1) = h(i)
        resultados(3) = abs(derivadaFrente2(h(i), coef) - d1)
        resultados(4) = abs(derivadaTras2(h(i), coef) - d1)
        resultados(2) = abs(derivadaSimetrica3(h(i), coef) - d1)
        resultados(5) = abs(derivadaSegundaSimetrica3(h(i), coef) - d2)
        resultados(6) = abs(derivadaSegundaSimetrica5(h(i), coef) - d2)
        resultados(7) = abs(derivadaTerceiraAntisimetrica5(h(i), coef) - d3)
        write(2, *) resultados
    end do

    close(1)
    close(2)

contains

    real(16) function derivadaFrente2(h, coef) 
        real(16), intent(in) :: h, coef
        real(16) :: derivada


        derivada = (log(coef + h)-log(coef))/h
        
        derivadaFrente2 = derivada

    end function derivadaFrente2

    real(16) function derivadaTras2(h, coef) 
        real(16), intent(in) :: h, coef
        real(16) :: derivada

        derivada = (log(coef) - log(coef - h))/h
        
        derivadaTras2 = derivada


    end function derivadaTras2

    real(16) function derivadaSimetrica3(h,coef) 
        real(16), intent(in) :: h, coef
        real(16) :: derivada

        derivada = (log(coef + h) - log(coef - h))/(2*h)
        
        derivadaSimetrica3 = derivada

    end function derivadaSimetrica3

    real(16) function derivadaSimetrica5(h,coef) 
        real(16), intent(in) :: h, coef
        real(16) :: derivada

        derivada = (8*log(coef + h) - 8*log(coef - h) - log(coef + 2*h) + log(coef - 2*h))/(12*h)
        
        derivadaSimetrica5 = derivada

    end function derivadaSimetrica5

    real(16) function derivadaSegundaSimetrica3(h,coef) 
        real(16), intent(in) :: h, coef
        real(16) :: derivada

        derivada = (log(coef + h) + log(coef - h) - 2*log(coef))/(h**2)
        
        derivadaSegundaSimetrica3 = derivada

    end function derivadaSegundaSimetrica3

    real(16) function derivadaSegundaSimetrica5(h,coef) 
        real(16), intent(in) :: h, coef
        real(16) :: derivada

        derivada = (16*log(coef + h) + 16*log(coef - h) - 30*log(coef) - log(coef + 2*h)- log(coef - 2*h))/(12*(h**2))
        
        derivadaSegundaSimetrica5 = derivada

    end function derivadaSegundaSimetrica5


    real(16) function derivadaTerceiraAntisimetrica5(h,coef) 
        real(16), intent(in) :: h, coef
        real(16) :: derivada

        derivada = (2*log(coef - h) - 2*log(coef + h) + log(coef + 2*h)- log(coef - 2*h))/(2*(h**3))
        
        derivadaTerceiraAntisimetrica5 = derivada

    end function derivadaTerceiraAntisimetrica5

end program exer1
