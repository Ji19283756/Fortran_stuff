program test_randint
  integer(16) :: test, randint, num1, num2
  num1 = 1
  num2 = 100
  
  test = randint(num1, num2)
  
  print *, test, " is the random number that was generated"

end program test_randint


function randint(from, to) result (random_int)
    implicit none
    integer(16), intent(in) :: from, to
    integer(16) :: random_int
    real :: RNG1
    
    call random_seed()
    call random_number(RNG1)
    
    random_int = int(from + int(RNG1 * (to - from + 1)))
    
end function randint



