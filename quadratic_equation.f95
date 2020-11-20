program test_quadratic_equation
    implicit none
    real(16) :: a ,b ,c, x_int_1, x_int_2
    a = 1
    b = 3
    c = -18
    call quad_eq(a ,b, c, x_int_1, x_int_2)
    print *, x_int_1, x_int_2
    
end program test_quadratic_equation


subroutine quad_eq(a, b, c, result1, result2) 
    !-b(+-)sqrt(b^2-4ac)
    !------------------
    !      2a
    implicit none
    real(16), intent(in) :: a, b, c
    real(16), intent(out) :: result1, result2
    real(16) :: inside_sqrt
    
    if ((b**2 - (4 * a * c)) < 0) then
        print *, "the result will be imaginary"
        return
    end if
    
    inside_sqrt = b**2 - 4 * a * c
    
    result1 = (-b + sqrt(inside_sqrt))/2 * a
    result2 = (-b - sqrt(inside_sqrt))/2 * a
    
end subroutine quad_eq



