! Fortran JSON AST
module fjson
    use fjson_tokenizer
    implicit none

    type, public :: json_node
        integer :: value_int
        real :: value_float
        logical :: value_bool
        character(len=:), allocatable :: value_string
        character(len=:), allocatable :: node_type  ! INT|FLOAT|BOOL|STRING|NULL|OBJECT|ARRAY|ERROR
        character(len=:), allocatable :: name  ! Object property name
        integer :: child_nodes_count
        type(json_node), dimension(:), allocatable :: child_nodes
        contains
            procedure append_child_node
            procedure create_number_node
            procedure create_string_node
            procedure create_bool_node
            procedure create_array_node
            procedure create_object_node
    end type json_node

    contains
        function parse_json(s) result(res)
            character(len=*), intent(in) :: s
            type(json_node) :: res
            type(token_t) :: tok
            integer :: pos, err

            pos = 1
            call next_token(s, pos, tok)

            select case (tok%kind)
                case (token_type_string)
                    call res%create_string_node(tok%text)

                case (token_type_number)
                    call res%create_number_node(tok%text)

                case (token_type_bool)
                    call res%create_bool_node(tok%text)

                case (token_type_null)
                    call res%create_bool_node(tok%text)

                case default
                    res%node_type = "ERROR"
                    res%value_string = tok%text
            end select
        end function parse_json

        subroutine append_child_node(this, child)
            class(json_node) :: this, child
            type(json_node), dimension(:), allocatable :: temp
            if(size(this%child_nodes) - this%child_nodes_count == 0) then
                allocate(temp(this%child_nodes_count * 2))
                temp(1:size(this%child_nodes)) = this%child_nodes(1:size(this%child_nodes))
                call move_alloc(from=temp,to=this%child_nodes)
            end if
            this%child_nodes_count = this%child_nodes_count + 1
            this%child_nodes(this%child_nodes_count) = child
        end subroutine append_child_node

        subroutine create_number_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            integer :: err
            this%child_nodes_count = 0
            if(index(s,".") /= 0) then
                this%node_type = "FLOAT"
                this%value_string = s
                read(s,*,iostat=err) this%value_float
                if (err /= 0) this%node_type = "ERROR"
            else
                this%node_type = "INT"
                this%value_string = s
                read(s,*,iostat=err) this%value_int
                if (err /= 0) this%node_type = "ERROR"
            end if
        end subroutine create_number_node

        subroutine create_string_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            character :: first, last
            this%child_nodes_count = 0
            first = s(1:1)
            last = s(len(s):len(s))
            if(first == '"' .and. last == '"') then
                this%node_type = "STRING"
                this%value_string = s(2:len(s)-1)
            else
                this%node_type = "ERROR"
                this%value_string = s
            end if
        end subroutine create_string_node

        subroutine create_bool_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            this%child_nodes_count = 0
            if(s == "true" .or. s == "false") then
                this%node_type = "BOOL"
                this%value_bool = (s == "true")
                this%value_string = s
            elseif(s == "null") then
                this%node_type = "NULL"
                this%value_string = s
            else
                this%node_type = "ERROR"
                this%value_string = s
            end if
        end subroutine create_bool_node

        subroutine create_array_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            ! TODO - Implement
        end subroutine create_array_node

        subroutine create_object_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            ! TODO - Implement
        end subroutine create_object_node
end module fjson