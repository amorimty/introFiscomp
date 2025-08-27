program exerB

    implicit none
    
    


end program exerB

real function expansaoTaylor(x)
    real, intent(in) :: x
    real(4) :: 
    
    do i = 1, 1000, 1
        
    end do

end function expansaoTaylor

integer function fatorial(n)
    integer, intent(in) :: n
    integer :: temp = 1

    do i = 1, n, 1
        temp = temp*i
    end do

    fatorial = temp

end function fatorial