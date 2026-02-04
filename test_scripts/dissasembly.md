```asm
Disassembly of TestClass:
Disassembly of __init__:
  3           RESUME                   0

  5           LOAD_FAST_LOAD_FAST     16 (a, self)
              STORE_ATTR               0 (a)

  6           LOAD_FAST_LOAD_FAST     32 (b, self)
              STORE_ATTR               1 (b)

  7           LOAD_FAST_LOAD_FAST     48 (c, self)
              STORE_ATTR               2 (c)

  8           LOAD_FAST_LOAD_FAST     64 (d, self)
              STORE_ATTR               3 (d)

  9           LOAD_FAST_LOAD_FAST     80 (e, self)
              STORE_ATTR               4 (e)

 11           LOAD_GLOBAL             11 (print + NULL)
              BUILD_LIST               0
              LOAD_CONST               1 ((255, 1, -55, 1.0, -0.1, 10.0, 100000000000.0, 110.0, 15j, -12000000000.0, 1.2e-10, 10.0, (-1-13j)))
              LIST_EXTEND              1
              CALL                     1
              POP_TOP
              RETURN_CONST             2 (None)

Disassembly of __repr__:
 13           RESUME                   0

 14           LOAD_CONST               1 ('test (self.a, self.b)=')
              LOAD_FAST                0 (self)
              LOAD_ATTR                0 (a)
              LOAD_FAST                0 (self)
              LOAD_ATTR                2 (b)
              BUILD_TUPLE              2
              LOAD_FAST                0 (self)
              LOAD_ATTR                4 (c)
              FORMAT_SIMPLE
              FORMAT_WITH_SPEC
              LOAD_CONST               2 (' class\U0002f313adủ98347#794\\8\n3Ƿ🥔        poasoidjoasndimplicit concatconcat 2')
              BUILD_STRING             3
              RETURN_VALUE


Disassembly of main:
 18           RESUME                   0

 19           LOAD_GLOBAL              1 (TestClass + NULL)
              LOAD_CONST               1 ('Hello!')
              LOAD_CONST               2 (123456)
              LOAD_CONST               3 (123456.789)
              LOAD_CONST               4 ('idk')
              LOAD_CONST               5 (4)
              BUILD_MAP                1
              BUILD_LIST               0
              LOAD_CONST               6 ((2.3, 4.5, 6.0, 0.3))
              LIST_EXTEND              1
              CALL                     5
              STORE_FAST               0 (t)

 20           LOAD_GLOBAL              3 (print + NULL)
              LOAD_FAST                0 (t)
              CALL                     1
              POP_TOP

 22           LOAD_CONST               7 (<code object potato at 0x7fe14534d2f0, file "/mnt/d2/RustroverProjects/PyCrust/test_scripts/lexer_test.py", line 22>)
              MAKE_FUNCTION
              STORE_FAST               1 (potato)

 25           LOAD_CONST               8 (<code object gen at 0x7fe145361bd0, file "/mnt/d2/RustroverProjects/PyCrust/test_scripts/lexer_test.py", line 25>)
              MAKE_FUNCTION
              STORE_FAST               2 (gen)

 29           LOAD_FAST                2 (gen)
              PUSH_NULL
              CALL                     0
              GET_ITER
      L1:     FOR_ITER                17 (to L2)
              STORE_FAST               3 (x)

 30           LOAD_GLOBAL              3 (print + NULL)
              LOAD_CONST               9 ('x is ')
              LOAD_FAST                3 (x)
              FORMAT_SIMPLE
              BUILD_STRING             2
              CALL                     1
              POP_TOP
              JUMP_BACKWARD           19 (to L1)

 29   L2:     END_FOR
              POP_TOP

 32           LOAD_FAST                1 (potato)
              PUSH_NULL
              CALL                     0
              POP_TOP

 34           LOAD_CONST              10 (6)
              STORE_FAST               3 (x)

 37           LOAD_FAST                3 (x)
              LOAD_CONST              11 (50)
              COMPARE_OP              88 (bool(==))
              POP_JUMP_IF_FALSE       12 (to L3)

 39           LOAD_GLOBAL              3 (print + NULL)
              LOAD_CONST              12 ('x is 50')
              CALL                     1
              POP_TOP
              RETURN_CONST             0 (None)

 37   L3:     RETURN_CONST             0 (None)

Disassembly of <code object potato at 0x7fe14534d2f0, file "/mnt/d2/RustroverProjects/PyCrust/test_scripts/lexer_test.py", line 22>:
 22           RESUME                   0

 23           LOAD_GLOBAL              1 (print + NULL)
              LOAD_CONST               1 ('potato')
              CALL                     1
              POP_TOP
              RETURN_CONST             0 (None)

Disassembly of <code object gen at 0x7fe145361bd0, file "/mnt/d2/RustroverProjects/PyCrust/test_scripts/lexer_test.py", line 25>:
  25           RETURN_GENERATOR
               POP_TOP
       L1:     RESUME                   0

  26           LOAD_GLOBAL              1 (range + NULL)
               LOAD_CONST               1 (10)
               CALL                     1
               GET_ITER
       L2:     FOR_ITER                10 (to L3)
               STORE_FAST               0 (i)

  27           LOAD_FAST                0 (i)
               LOAD_CONST               2 (5)
               BINARY_OP                5 (*)
               YIELD_VALUE              0
               RESUME                   5
               POP_TOP
               JUMP_BACKWARD           12 (to L2)

  26   L3:     END_FOR
               POP_TOP
               RETURN_CONST             0 (None)

  --   L4:     CALL_INTRINSIC_1         3 (INTRINSIC_STOPITERATION_ERROR)
               RERAISE                  1
ExceptionTable:
  L1 to L4 -> L4 [0] lasti

```