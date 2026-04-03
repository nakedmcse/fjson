! Fortran JSON AST Unit Tests
program test
    use fjson
    implicit none

    ! Tests
    call test_string()
    call test_number()
    call test_bool()
    call test_array()
    call test_object()
    call test_to_string()

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
            character(len=*), parameter :: input = '"test-\"string\""'
            ! When
            result = parse_json(input)
            ! Then
            call assert(result%node_type == "STRING", "String: Node type wrong - " // result%node_type // " " // result%value_string)
            call assert(result%value_string == 'test-\"string\"', "String: Node value wrong - " // result%value_string)
            print *, "String Test successful"
        end subroutine test_string

        subroutine test_number()
            ! Given
            type(json_node) :: result_int, result_float
            character(len=*), parameter :: input_int = '-67'
            character(len=*), parameter :: input_float = '-67.67'
            ! When
            result_int = parse_json(input_int)
            result_float =  parse_json(input_float)
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

        subroutine test_array()
            ! Given
            type(json_node) :: result_simple
            character(len=*), parameter :: input_simple = '[6, -7, "x", true, null]'
            ! When
            result_simple = parse_json(input_simple)
            ! Then
            call assert(result_simple%node_type == "ARRAY", "Array-Simple: Node type wrong - " // result_simple%node_type)
            call assert(result_simple%child_nodes_count == 5, "Array-Simple: Child node count wrong")
            call assert(result_simple%child_nodes(1)%value_int == 6, "Array-Simple: Element 1 wrong value")
            call assert(result_simple%child_nodes(2)%value_int == -7, "Array-Simple: Element 2 wrong value")
            call assert(result_simple%child_nodes(3)%value_string == "x", "Array-Simple: Element 3 wrong value")
            call assert(result_simple%child_nodes(4)%value_bool, "Array-Simple: Element 4 wrong value")
            call assert(result_simple%child_nodes(5)%value_string == "null", "Array-Simple: Element 5 wrong value")
            print *, "Array Test successful"
        end subroutine test_array

        subroutine test_object()
            ! Given
            type(json_node) :: result_simple, result_complex, first, second
            character(len=*), parameter :: input_simple = '{"number":-7, "string":"x", "bool":true, "null":null}'
            character(len=*), parameter :: input_complex = '{"subobject":{"a":1,"b":2}, "subarray":[3,4]}'
            ! When
            result_simple = parse_json(input_simple)
            result_complex = parse_json(input_complex)
            ! Then
            call assert(result_simple%node_type == "OBJECT", "Object-Simple: Node type wrong - " // result_simple%node_type)
            call assert(result_simple%child_nodes_count == 4, "Object-Simple: Child node count wrong")
            call assert(result_simple%child_nodes(1)%name == "number", "Object-Simple: Element 1 wrong name")
            call assert(result_simple%child_nodes(1)%value_int == -7, "Object-Simple: Element 1 wrong value")
            call assert(result_simple%child_nodes(2)%name == "string", "Object-Simple: Element 2 wrong name")
            call assert(result_simple%child_nodes(2)%value_string == "x", "Object-Simple: Element 2 wrong value")
            call assert(result_simple%child_nodes(3)%name == "bool", "Object-Simple: Element 3 wrong name")
            call assert(result_simple%child_nodes(3)%value_bool, "Object-Simple: Element 3 wrong value")
            call assert(result_simple%child_nodes(4)%name == "null", "Object-Simple: Element 4 wrong name")
            call assert(result_simple%child_nodes(4)%value_string == "null", "Object-Simple: Element 4 wrong value")

            call assert(result_complex%node_type == "OBJECT", "Object-Complex: Root node type wrong - " // result_complex%node_type)
            call assert(result_complex%child_nodes_count == 2, "Object-Complex: Root child node count wrong")
            call first%copy_from(result_complex%child_nodes(1))
            call assert(first%node_type == "OBJECT", "Object-Complex: Child Object node type wrong")
            call assert(first%name == "subobject", "Object-Complex: Child Object node name wrong")
            call assert(first%child_nodes_count == 2, "Object-Complex: Child Object child node count wrong")
            call assert(first%child_nodes(1)%name == "a", "Object-Complex: Child Object name a wrong")
            call assert(first%child_nodes(1)%value_int == 1, "Object-Complex: Child Object value a wrong")
            call assert(first%child_nodes(2)%name == "b", "Object-Complex: Child Object name b wrong")
            call assert(first%child_nodes(2)%value_int == 2, "Object-Complex: Child Object value b wrong")
            call second%copy_from(result_complex%child_nodes(2))
            call assert(second%node_type == "ARRAY", "Object-Complex: Child Array node type wrong")
            call assert(second%name == "subarray", "Object-Complex: Child Array node name wrong")
            call assert(second%child_nodes_count == 2, "Object-Complex: Child Array child node count wrong")
            call assert(second%child_nodes(1)%value_int == 3, "Object-Complex: Child Array first value wrong")
            call assert(second%child_nodes(2)%value_int == 4, "Object-Complex: Child Array second value wrong")
            print *, "Object Test successful"
        end subroutine test_object

        subroutine test_to_string()
            ! Given
            type(json_node) :: result_simple, result_complex, first, second
            character(len=*), parameter :: input_simple = '{"number":-7,"string":"x","bool":true,"null":null}'
            character(len=*), parameter :: input_complex = '{"subobject":{"a":1,"b":2},"subarray":[3,4]}'
            character(len=:), allocatable :: simple_json, complex_json
            ! When
            result_simple = parse_json(input_simple)
            simple_json = ""
            call result_simple%to_string(simple_json)
            result_complex = parse_json(input_complex)
            complex_json = ""
            call result_complex%to_string(complex_json)
            ! Then
            call assert(simple_json == input_simple, "ToString: Simple input does not match")
            call assert(complex_json == input_complex, "ToString: Complex input does not match")
            print *, "ToString test passed"
        end subroutine test_to_string
end program test