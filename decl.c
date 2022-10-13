#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define TMin INT_MIN
#define TMax INT_MAX

#include "btest.h"
#include "bits.h"

test_rec test_set[] = {
/* Copyright (C) 1991-2020 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
 {"bit_set_odd", (funct_t) bit_set_odd, (funct_t) test_bit_set_odd, 1,
    "! ~ & ^ | + << >>", 12, 2,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"logical_not", (funct_t) logical_not, (funct_t) test_logical_not, 1,
    "~ & ^ | + << >>", 12, 4,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"bitwise_and", (funct_t) bitwise_and, (funct_t) test_bitwise_and, 2, "| ~", 8, 1,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"bit_sum", (funct_t) bit_sum, (funct_t) test_bit_sum, 1, "! ~ & ^ | + << >>", 40, 4,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"bitwise_or", (funct_t) bitwise_or, (funct_t) test_bitwise_or, 2, "& ~", 8, 1,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"bit_sum_odd", (funct_t) bit_sum_odd, (funct_t) test_bit_sum_odd, 1, "! ~ & ^ | + << >>", 20, 4,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"bitwise_xor", (funct_t) bitwise_xor, (funct_t) test_bitwise_xor, 2, "& ~", 14, 1,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
{"byte_trade", (funct_t) byte_trade, (funct_t) test_byte_trade, 3,
     "! ~ & ^ | + << >>", 25, 2,
    {{TMin, TMax},{0,3},{0,3}}},
 {"compact_if_then", (funct_t) compact_if_then, (funct_t) test_compact_if_then, 3, "! ~ & ^ | << >>", 16, 3,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"distill_byte", (funct_t) distill_byte, (funct_t) test_distill_byte, 2,
    "! ~ & ^ | + << >>", 6, 2,
  {{TMin, TMax},{0,3},{TMin,TMax}}},
{"one_if_reversible", (funct_t) one_if_reversible, (funct_t) test_one_if_reversible, 1, "! ~ & ^ | + << >>", 49, 4,
 {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"lsb_bit_mask", (funct_t) lsb_bit_mask, (funct_t) test_lsb_bit_mask, 1, "! ~ & ^ | + << >>", 6, 2,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"sign_flip", (funct_t) sign_flip, (funct_t) test_sign_flip, 1,
    "! ~ & ^ | + << >>", 5, 2,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"byte_replace", (funct_t) byte_replace, (funct_t) test_byte_replace, 3,
    "! ~ & ^ | + << >>", 10, 3,
  {{TMin, TMax},{0,3},{0,255}}},
 {"wheel_right", (funct_t) wheel_right, (funct_t) test_wheel_right,
   2, "! ~ & ^ | + << >>", 25, 3,
  {{TMin, TMax},{0,31},{TMin,TMax}}},
 {"left_fill", (funct_t) left_fill, (funct_t) test_left_fill, 1, "! ~ & ^ | + << >>", 10, 1,
  {{0, 32},{TMin,TMax},{TMin,TMax}}},
 {"absolute_value", (funct_t) absolute_value, (funct_t) test_absolute_value, 1, "! ~ & ^ | + << >>", 10, 4,
  {{-TMax, TMax},{TMin,TMax},{TMin,TMax}}},
 {"add_no_overflow", (funct_t) add_no_overflow, (funct_t) test_add_no_overflow, 2,
    "! ~ & ^ | + << >>", 20, 3,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"denominator_2_to_n", (funct_t) denominator_2_to_n, (funct_t) test_denominator_2_to_n, 2,
    "! ~ & ^ | + << >>", 15, 2,
  {{TMin, TMax},{0,30},{TMin,TMax}}},
 {"ezThreeFourths", (funct_t) quick_seventy_five_percent, (funct_t) test_quick_seventy_five_percent, 1,
    "! ~ & ^ | + << >>", 12, 3,
  {{TMin,TMax},{TMin,TMax},{TMin,TMax}}},
 {"one_if_ascii", (funct_t) one_if_ascii, (funct_t) test_one_if_ascii, 1,
    "! ~ & ^ | + << >>", 15, 3,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"one_if_equal", (funct_t) one_if_equal, (funct_t) test_one_if_equal, 2,
    "! ~ & ^ | + << >>", 5, 2,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"one_if_negative", (funct_t) one_if_negative, (funct_t) test_one_if_negative, 1,
    "! ~ & ^ | + << >>", 6, 2,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"one_if_non_zero", (funct_t) one_if_non_zero, (funct_t) test_one_if_non_zero, 1,
    "~ & ^ | + << >>", 10, 4,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"one_if_not_equal", (funct_t) one_if_not_equal, (funct_t) test_one_if_not_equal, 2,
    "! ~ & ^ | + << >>", 6, 2,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"one_if_max_twos_complement", (funct_t) one_if_max_twos_complement, (funct_t) test_one_if_max_twos_complement, 1, "! ~ & ^ | +", 10, 1,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"one_if_min_twos_complement", (funct_t) one_if_min_twos_complement, (funct_t) test_one_if_min_twos_complement, 1, "! ~ & ^ | +", 10, 1,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"one_if_zero", (funct_t) one_if_zero, (funct_t) test_one_if_zero, 1, "! ~ & ^ | + << >>", 2, 1,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"negative_one", (funct_t) negative_one, (funct_t) test_negative_one, 0,
    "! ~ & ^ | + << >>", 2, 1,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"boundary_add", (funct_t) boundary_add, (funct_t) test_boundary_add, 2,
    "! ~ & ^ | + << >>", 30, 4,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"sign_bit", (funct_t) sign_bit, (funct_t) test_sign_bit, 1, "! ~ & ^ | + << >>", 10, 2,
     {{-TMax, TMax},{TMin,TMax},{TMin,TMax}}},
 {"twos_complement_max", (funct_t) twos_complement_max, (funct_t) test_twos_complement_max, 0, "! ~ & ^ | + << >>", 4, 1,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"twos_complement_min", (funct_t) twos_complement_min, (funct_t) test_twos_complement_min, 0, "! ~ & ^ | + << >>", 4, 1,
  {{TMin, TMax},{TMin,TMax},{TMin,TMax}}},
 {"real_absolute_value", (funct_t) real_absolute_value, (funct_t) test_real_absolute_value, 1,
    "$", 10, 2,
     {{1, 1},{1,1},{1,1}}},
 {"real_to_int", (funct_t) real_to_int, (funct_t) test_real_to_int, 1,
    "$", 30, 4,
     {{1, 1},{1,1},{1,1}}},
 {"int_to_float", (funct_t) int_to_float, (funct_t) test_int_to_float, 1,
    "$", 30, 4,
     {{1, 1},{1,1},{1,1}}},
 {"real_negation", (funct_t) real_negation, (funct_t) test_real_negation, 1,
    "$", 10, 2,
     {{1, 1},{1,1},{1,1}}},
  {"", NULL, NULL, 0, "", 0, 0,
   {{0, 0},{0,0},{0,0}}}
};
