Unittest Generator Specification
=============================

# Syntax of Specification File

This is just a draft. Current grammer is written in mpidl.peggy

TESTS := TEST TESTS | \0
TEST := { PROCS }
PROCS := PROC ; PROCS | PROCS | \0
PROC := STATE | ASSERT
STATE := var = EXP | EXP
CALL := func(ARGS)
ARGS := arg ARGS' | \0
ARGS' := , arg ARGS' | \0
ASSERT := assert ( EQUATION )
EQUATION := EXP OP EXP
EXP := CALL | var | CONST
OP :=  = | < | <= | > | >= | <> | ~
CONST := const_int | const_float | const_string | CONST_LIST |
CONST_MAP | CONST_STRUCT
CONST_LIST := [ ARGS ]
CONST_MAP := { KEY_VALS }
KEY_VALS := KEY_VAL KEY_VALS' | \0
KEY_VALS' := , KEY_VAL KEY_VALS'
KEY_VAL := EXP : EXP
CONST_STRUCT := new type ( ARGS )


