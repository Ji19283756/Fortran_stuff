program find_greatest_common_denominator
    implicit none
    integer :: gcd, temp
    integer :: number_1, number_2, x, y
    number_1 = 10
    number_2 = 15
    do x = 1,20
        do y = 1,20
            print *, "the gcd of ",y,x,"is ",gcd(y,x)
        end do
        
    end do
    temp = gcd(number_1,number_2)
    
    print *, temp, "is the greatest common denominator"

end program find_greatest_common_denominator

function gcd(a, b) result (tempb)
    implicit none
    integer, intent(in) :: a, b
    integer :: tempa, tempb, place_holder
    
    tempa = a
    tempb = b
    
    do while (.not.tempa.eq.0)
    
        place_holder = tempa
    
        if (tempa.eq.0) then
            exit
        end if
    
        tempa = mod(tempb,tempa)
        tempb = place_holder
    
    end do
    
end function gcd
