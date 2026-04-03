! Fortran JSON AST Unit Tests
program test
    use fjson
    implicit none

    ! Tests
    call test_string()
    call test_number()
    call test_bool()

    contains
        subroutine assert(condition, message)
            logical :: condition
            character(len=*) :: message
            if (.not.(condition)) then
                print *, "Assertion failed: ", message
                error stop
            end if
        end subroutine assert

        subroutine test_string()
            ! Given
            type(json_node) :: result
            character(len=*), parameter :: input = '"test-string"'
            ! When
            result = parse_json(input)
            ! Then
            call assert(result%node_type == "STRING", "String: Node type wrong - " // result%node_type // " " // result%value_string)
            call assert(result%value_string == "test-string", "String: Node value wrong - " // result%value_string)
            print *, "String Test successful"
        end subroutine test_string

        subroutine test_number()
            ! Given
            type(json_node) :: result_int, result_float
            character(len=*), parameter :: input_int = '-67'
            character(len=*), parameter :: input_float = '-67.67'
            ! When
            result_int = parse_json(input_int)
            result_float = parse_json(input_float)
            ! Then
            call assert(result_int%node_type == "INT", "Integer: Node type wrong - " // result_int%node_type // " " // result_int%value_string)
            call assert(result_int%value_string == input_int, "Integer: Node string value wrong - " // result_int%value_string)
            call assert(result_int%value_int == -67, "Integer: Node int value wrong")

            call assert(result_float%node_type == "FLOAT", "Float: Node type wrong - " // result_float%node_type)
            call assert(result_float%value_string == input_float, "Float: Node string value wrong - " // result_float%value_string)
            call assert(result_float%value_float == -67.67, "Float: Node real value wrong")
            print *, "Number Test successful"
        end subroutine test_number

        subroutine test_bool()
            ! Given
            type(json_node) :: result_true, result_false, result_null
            character(len=*), parameter :: input_true = "true"
            character(len=*), parameter :: input_false = "false"
            character(len=*), parameter :: input_null = "null"
            ! When
            result_true = parse_json(input_true)
            result_false = parse_json(input_false)
            result_null = parse_json(input_null)
            ! Then
            call assert(result_true%node_type == "BOOL", "Bool-True: Node type wrong - " // result_true%node_type)
            call assert(result_true%value_string == input_true, "Bool-True: Node string value wrong - " // result_true%value_string)
            call assert(result_true%value_bool, "Bool-True: Node bool value wrong")

            call assert(result_false%node_type == "BOOL", "Bool-False: Node type wrong - " // result_false%node_type)
            call assert(result_false%value_string == input_false, "Bool-False: Node string value wrong - " // result_false%value_string)
            call assert(.not. result_false%value_bool, "Bool-False: Node bool value wrong")

            call assert(result_null%node_type == "NULL", "Null: Node type wrong - " // result_null%node_type)
            call assert(result_null%value_string == input_null, "Null: Node string value wrong - " // result_null%value_string)
            print *, "Boolean Test successful"
        end subroutine test_bool

end program test