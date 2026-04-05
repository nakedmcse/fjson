# FJSON

![tests](https://github.com/nakedmcse/fjson/actions/workflows/build.yml/badge.svg)
[![GitHub issues](https://img.shields.io/github/issues/nakedmcse/fjson.png)](https://github.com/nakedmcse/fjson/issues)
[![last-commit](https://img.shields.io/github/last-commit/nakedmcse/fjson)](https://github.com/nakedmcse/fjson/commits/master)

A lightweight, small, pure FORTRAN JSON module.

## Building
Clone the repository and then build the module:
```shell
git clone https://github.com/nakedmcse/fjson.git
cd fjson
make fjson
```

Then copy the `fjson.a` file to your projects directory.

You can then add the following to the top of your program:
```fortran
use fjson
```

Then to compile, use the following:
```shell
gfortran -o your_program fjson.a your_program.f90
```

### Testing
The repository comes with a set of unit tests in `test.f90`, that can be built and run using the following:
```shell
make all
./test
```

## AST Structure
At its heart, this module converts text JSON to and from an abstract syntax tree, representing the structure.
This is represented by a tree of nodes of type `json_node` with the following fields: 

| Field             | Type               | Description                                                                       |
|-------------------|--------------------|-----------------------------------------------------------------------------------|
| node_type         | string             | Node type - one of INT/FLOAT/BOOL/STRING/NULL/OBJECT/ARRAY/ERROR                  |
| name              | string             | Node name - if the node is part of an object it will be named, empty otherwise    |
| value_string      | string             | Node value as a string - available for all nodes                                  |
| value_int         | integer            | Node value as an integer - set for integer number nodes                           |
| value_float       | real               | Node value as floating point - set for floating point number nodes                |
| value_bool        | logical            | Node value as boolean - set for boolean nodes                                     |
| child_nodes_count | integer            | If the node is an OBJECT or ARRAY and has child nodes, this is the number of them |
| child_nodes       | array of json_node | If the node is an OBJECT or ARRAY, these are its child nodes                      |

## Parsing
This is simply passing the JSON as a string to `parse_json`, which will then return a `json_node` tree containing the parsed data.
Data can then be read from the AST using either `get_node` (passing it the tree and a fully qualified name), or by referencing the AST directly.

If there is an error during parsing then the `fjson_error` pointer will be set to the node that threw the error.
```fortran
program extract
    use fjson
    implicit none
    character(len=*), parameter :: input_json = '{"subobject":{"a":1,"b":2}, "subarray":[3,4]}'
    type(json_node) :: parsed_data, extract_data
    integer :: a_value, b_value, array_fist
    
    ! Parse string to AST
    parsed_data = parse_json(input_json)
    if (associated(fjson_error)) then
        print *, "Error parsing input"
        error stop
    end if
    
    ! Get data from AST using get_node
    extract_data = get_node(parsed_data,".subobject.a")
    a_value = extract_data%value_int
    extract_data = get_node(parsed_date,".subobject.b")
    b_value = extract_data%value_int
    
    ! Get data from AST directly
    array_first = parsed_data%child_nodes(2)%child_nodes(1)%value_int
end program extract
```

## Encoding
This is done by either manually building the AST to represent the JSON structure, or using `parse_json` on an existing 
base structure, and then using the `json_node%to_string` subroutine.
```fortran
program encode
    use fjson
    implicit none
    character(len=*), parameter :: base_json = '{"subobject":{"a":1,"b":2}, "subarray":[3,4]}'
    type(json_node) :: pre_built, manual_built
    character(len=:), allocatable :: pre_built_out, manual_built_out
    
    ! Prebuild JSON and modify tree
    pre_built = parse_json(base_json)
    pre_built%child_nodes(1)%child_nodes(1)%value_string = "6"
    pre_built%child_nodes(1)%child_nodes(2)%value_string = "7"
    call pre_built%to_string(pre_built_out)
    
    ! Manual build AST
    call manual_built%create_object_node('{"a":1,"b":2}')
    manual_built%child_nodes(1)%child_nodes(1)%value_string = "6"
    manual_built%child_nodes(1)%child_nodes(2)%value_string = "7"
    call manual_built%to_string(manual_built_out)
end program encode
```