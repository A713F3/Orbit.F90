! vector2d.f90
!
! 2D Vector definition and utils.
!
! Author:  Ali Efe Karagul
! GitHub:  https://github.com/A713F3/Orbit.F90

module vector2d_mod 
implicit none
    type :: vector2d
        real :: x, y
    contains
        procedure :: distance
        procedure :: add
        procedure :: sub
        procedure :: mul
        procedure :: dot
        procedure :: norm
    end type

contains

    function distance(self, other)
        class(vector2d), intent(in) :: self
        class(vector2d), intent(in) :: other
        real                        :: distance

        distance = sqrt(real( (self%x - other%x)**2 +  (self%y - other%y)**2 ))
    end function distance

    function add(self, other) result(res)
        class(vector2d), intent(inout) :: self
        class(vector2d), intent(in)    :: other
        type(vector2d)                 :: res

        res%x = self%x + other%x
        res%y = self%y + other%y
    end function add

    function sub(self, other) result(res)
        class(vector2d), intent(inout) :: self
        class(vector2d), intent(in)    :: other
        type(vector2d)                 :: res

        res%x = self%x - other%x
        res%y = self%y - other%y
    end function sub

    function mul(self, constant) result(res)
        class(vector2d), intent(inout) :: self
        real, intent(in)               :: constant
        type(vector2d)                 :: res

        res%x = self%x * constant
        res%y = self%y * constant
    end function mul

    function dot(self, other) result(res)
        class(vector2d), intent(inout) :: self
        class(vector2d), intent(in)    :: other
        type(real)                     :: res

        res = (self%x * other%x) + (self%y * other%y)
    end function dot

    function norm(self) result(res)
        class(vector2d), intent(inout) :: self
        type(real)                     :: res

        res = sqrt((self%x ** 2 )+ (self%y ** 2))
    end function norm

end module vector2d_mod