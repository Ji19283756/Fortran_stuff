!gfortran, gcc version 7.4.0

program random
    implicit none

    ! they also have derived data types aka classes (i think)
    type Student
        character (len = 50) :: name
        character (len = 60) :: school
        integer (kind = 8) :: gpa
    end type
    character*20 :: name
    
    !declaring variables
    ! V real number with decimals
    real, parameter :: PI = 3.1415
    
    ! V real, stores floating point numbers, 
    real :: r_num1 = 0.0, r_num2 = 0.0
    real :: main_area
    
    ! will have 15 decimal places
    double precision :: dbl_num = 1.111111111111d+0
    
    ! V normal integers
    integer :: i_num1= 0, inum2 = 0
    ! you can specify how many bytes they have too
    integer(kind = 16) :: sixteen_byte_int = 10
    integer (kind = 4) :: int_grade, x, y ,x2
    integer, dimension (:,:), allocatable :: python_int_list
    
    ! booleans
    logical :: pineapples_on_pizzas_is_good = .true.
    logical :: pineapples_are_bad = .false.
    
    ! making characters/ strings
    character (len = 10) :: month, sliced_month
    character :: grade
    character (len = 20) :: str_concate
    
    ! complex i think tuples?
    complex :: com_num = (2.0, 4.0)
    
    ! what about arrays tho?
    ! thing to mind about fortran arraays is that the first index is 1
    real, dimension(5) :: real_array
    ! V makes a double array
    integer, dimension(2,2) :: int_double_array
    
    !creating instances of classes
    type(Student) :: student1
    type(Student) :: student2
    
    ! end of declaring variable types
    
    main_area = area_of_circle(10.0)
    
    !how to assign attributes of a class
    student1%name = "eric the g"
    student1%school = "a pretty average school"
    student1%gpa = 5
    
    student2%name = "anyone thats not eric the g"
    student2%school = "b prus school"
    student2%gpa = 1
    
    print *, student1%name
    print *, student1%school
    print *, student1%gpa
    
    print *, student2%name
    print *, student2%school
    print *, student2%gpa
    
    
    print *, python_int_list
    allocate(python_int_list(1,4))
    do x2 = 1,4
        python_int_list(1,x) = x2
    end do
    print *, python_int_list
    
    ! yikes time for if elses
    
    
    !int_double_array = reshape( (/5,9,6/), (/3,2,4/), (/5,9,6/)) 
    ! assigning values is pretty much just like python/ java
    do x = 1,2
        do y = 1,2
            int_double_array(x,y) = 10.0
        end do
    end do
    
    print *, int_double_array
    !print *, Extent	(int_double_array)
    
    ! however direct declarations are a bit different
    real_array = (/1.0, 1.1, 1.2, 1.3, 1.4/)
    print *, real_array
    ! tho you can do some pretty nifty triks like this alternates the numbers with 2's
    real_array(1:5:2) = 2
    print *, real_array
    ! or maybe you just wan everyting to be the same
    real_array(1:) = 0
    print *, real_array
    ! nah
    real_array(1:5) = 3.14
    print *, real_array
    
    ! some of the fucntions regarding arrays are the same as python tho
    print *, all(real_array==3.14), "<- if all the values in real_array are equal to 3.14"
    print *, any(real_array > 4), "<- if any of the values in real array are greater than 4"
    print *, count(real_array == 3.14), "<- I guess this is a way of counting the amount of 3.14's in real array"
    real_array = (/1.0,1.1,1.2,1.3,1.4/)
    print *, "i wonder what the min value is ", minval(real_array)
    print *, "i wonder what the max value is ", maxval(real_array)
    print *, "i wonder would happen if you multiplied all values ", product(real_array)
    print *, "i wonder what the sum of all values is ", sum(real_array)
    print *, "i wonder how long the array is ", Size(real_array)
    
    
    ! yikes time for if elses
    
    if (pineapples_are_bad) then
        print *, "I guess the first if statement made it!"
    else if (pineapples_on_pizzas_is_good) then
        print *, "I guess the second if statement made it!"
    else 
        print *, "I guess all of the statements are false!"
    ! you gotta have an end if
    end if 
    ! what if you want a nested if statement tho?
    if (.true.) then
        if (.false.) then
            print *, "ya yeet"
        else
            print *, "ya yote"
        end if
    end if
    
    
    
    ! dang, fortrans got switch statements too?
    grade = "B"
    select case (grade)
        case ("A")
            print *, "Dangggg and A? nice!"
        case ("B")
            print *, "You suck!"
        ! kind of an else thing
        case default
            print *, "i hope you never make it to this level mate"
    ! like if statements you also gotta write an end select
    end select
    
    
    
    ! you can also use switch ranges
    int_grade = 69
    select case (int_grade)
        case (91:100)
            print *, "dang another A?"
        case (81:90)
            print *, "no lie you kinda still suck"
        case (20:80)
            print *, "WHAT! how are you so bad?"
        ! kind of an else thing
        case default
            print *, "i hope you never make it to this level mate"
    end select
    
    ! ugh but where are the for loops tho? btw you can label them like bruh
    ! for x in range(1, 11)
    reee : do x = 1,10
        ! if you ever wanna break out of the reee loop
        if (x == 5) then
            exit reee
        end if
        
        ! if you ever wanna do a continue
        if (x == 2) then 
            cycle
        end if
        
        ! also somehow if you ever wanna like yknow, end the ENTIRE program you could do that
        if (.false.) then
            stop
        end if
        
        print *, x, ": pineapple belongs on pizza"
        
    end do reee
    
    ! where my for loops at tho?
    do while (x > 0)
        x = x - 1
        print *, x, ": pineapple probably belongs on pizza"
    end do
    
    
    month = "january"
    sliced_month = month(1:3)
    print *, "You can slice january into ",sliced_month
    print *, "biggest real number", huge(r_num1)
    !print *, "biggest int", huge(i_num1)
    print *, "smallest real number", tiny(r_num1)
    !print *, "smallest int", tiny(i_num1)
    
    !i wonder how string concatenation works?
    str_concate = month//sliced_month
    print *, str_concate, "<- concatenated string"
    
    !some more string fucntions
    print *, len(str_concate)
    print *, index(str_concate,"j"), " basically .index()"
    print *, achar(100), " basically chr()"
    print *, iachar("a"), " basicaly ord()"
    print *, trim("     hi     "), " basically .strip(), but only does the left side "
    print *, "I wonder if the letter a is in januaray? ", scan("januaray","a"), "I guess it is "
    print *, "i think this is pretty much like multiplying strings bugt with extra steps ", repeat("a",10)
    
    print *, "What's your name"
    ! inputing strings
    read *, name
    print *, "hello", name
    
    ! lets test some defualt functions of fortran
    print *, abs(-420)
    print *, anint(3.15)
    print *, dble(3)
    print *, dprod(10,3)
    print *, int(3.14)
    print * mod(10,1)
    ! they also have cons, sin, tan their inverses, exponential values of x, logs of x, sqrt, degree to radians
    print *, len("hello")
    

end program random

_______________________________________________________________________________________________________________
program random
    real :: result_of_func_1,result_of_func_2 
    real :: a, b
    integer(kind = 16) :: big
    ! calling functions
    result_of_func_1 = area_of_circle(10.0)
    result_of_func_2 = circle_area(10.0)
    
    print *, result_of_func_1
    print *, result_of_func_2
    
    print *, huge(big)
    a = 2.0
    b = 3.0
    
    print *, a , " a ", b , " b "
    
    call swap (a,b)
    print *, a , " a ", b , " b "
    
    
end program random
!ayyy we finally made it to functions, btw fucnions arent supposed to modify arguments
! you either have to specify which variable is returned or have a variable that is the 
! same name as the fucntion
function area_of_circle (r)  
   implicit none      
      
   real :: area_of_circle   
   

   real :: r     
   real :: pi
   
   pi = 4 * atan (1.0)     
   area_of_circle = pi * r**2  
   
end function area_of_circle

function circle_area(radius) result (area)
    implicit none
    real :: area
    real :: radius
    real :: pi
    
    pi = 3.1415
    area = (radius ** 2) * pi
   
end function circle_area

! they also have subroutines that are invoked by call, can change the value of its argument
subroutine swap (x, y)
    implicit none
    real :: x, y ,temp
    temp = x
    x = y
    y = temp
end subroutine swap

subroutine test (a, b, c, d)
    implicit none
    real, intent (in) :: a
    real, intent (in) :: b
    real, intent (in) :: c
    real, intent (out) :: d
    
    d = b * b -4.0 * a * c
end subroutine test

