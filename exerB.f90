program exerB

    implicit none

    ! write(*,*) 0.1_8, expansaoTaylorErro(0.1_8)
    ! write(*,*) 0.2_8, expansaoTaylorErro(0.2_8)
    ! write(*,*) 0.3_8, expansaoTaylorErro(0.3_8)
    ! write(*,*) 0.4_8, expansaoTaylorErro(0.4_8)

    real(8) :: erro
    integer :: iteracoes

    call expansaoTaylorErroSubroutine(0.1_8, erro, iteracoes)
    write(*,*) 0.1_8, erro, iteracoes
    call expansaoTaylorErroSubroutine(0.2_8, erro, iteracoes)
    write(*,*) 0.2_8, erro, iteracoes
    call expansaoTaylorErroSubroutine(0.3_8, erro, iteracoes)
    write(*,*) 0.3_8, erro, iteracoes
    call expansaoTaylorErroSubroutine(0.4_8, erro, iteracoes)
    write(*,*) 0.4_8, erro, iteracoes
    write(*,*) "Como podemos observar, a precisão chega na casa dos 1E-016%, uma precisão muito boa para um método numérico de aproximacao por series. No entanto, o número de iterações para a convergencia tabem ha de ser levado em conta. Apesar de convergir rapido para 0,1 e 0,2, eh possivel identificar uma tendencia de aumento de iteracoes para convergir, detalhe que pode se tornar determinante em uma analise mais minuciosa da precisao em relacao ao tempo para convergencia."



contains
    real(8) function expansaoTaylorErro(a)
        implicit none
        real(8), intent(in) :: a
        integer :: i
        real(8) :: S = 0.0_8, c = 0.0_8, term, y, temp, eps 

        eps = epsilon(1.0_8)


        ! implementando a soma de kahan para reduzir a propagação de erro na série
        do i = 1, 100, 1
            term = ((-1.0_8)**(i+1))*(a**i)/real(i, 8)

            if (abs(term) <= eps*max(1.0_8, abs(S))) exit

            y = term - c 
            temp = S + y
            c = (temp - S) - y

            
            S = temp
            
        end do
        expansaoTaylorErro = abs(term/S)

    end function expansaoTaylorErro

    subroutine expansaoTaylorErroSubroutine(a, erro, iteracoes)
        implicit none
        real(8), intent(in) :: a
        real(8), intent(out) :: erro
        integer, intent(out) :: iteracoes
        integer :: i
        real(8) :: S = 0.0_8, c = 0.0_8, term, y, temp, eps 

        eps = epsilon(1.0_8)


        ! implementando a soma de kahan para reduzir a propagação de erro na série
        do i = 1, 100, 1
            term = ((-1.0_8)**(i+1))*(a**i)/real(i, 8)

            if (abs(term) <= eps*max(1.0_8, abs(S))) exit

            y = term - c 
            temp = S + y
            c = (temp - S) - y

            
            S = temp
            
        end do
        erro = abs(term/S)
        iteracoes = i

    end subroutine expansaoTaylorErroSubroutine

end program exerB

