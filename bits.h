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
int bit_set_odd();
int test_bit_set_odd();
int logical_not(int);
int test_logical_not(int);
int bitwise_and(int, int);
int test_bitwise_and(int, int);
int bit_sum(int);
int test_bit_sum(int);
int bitwise_or(int, int);
int test_bitwise_or(int, int);
int bit_sum_odd(int);
int test_bit_sum_odd(int);
int bitwise_xor(int, int);
int test_bitwise_xor(int, int);
int byte_trade(int, int, int);
int test_byte_trade(int, int, int);
int compact_if_then(int, int, int);
int test_compact_if_then(int, int, int);
int distill_byte(int, int);
int test_distill_byte(int, int);
int one_if_reversible(int);
int test_one_if_reversible(int);
int lsb_bit_mask(int);
int test_lsb_bit_mask(int);
int sign_flip(int);
int test_sign_flip(int);
int byte_replace(int, int, int);
int test_byte_replace(int, int, int);
int wheel_right(int, int);
int test_wheel_right(int, int);
int left_fill(int);
int test_left_fill(int);
int absolute_value(int);
int test_absolute_value(int);
int add_no_overflow(int, int);
int test_add_no_overflow(int, int);
int denominator_2_to_n(int, int);
int test_denominator_2_to_n(int, int);
int quick_seventy_five_percent(int);
int test_quick_seventy_five_percent(int);
int one_if_ascii(int);
int test_one_if_ascii(int);
int one_if_equal(int, int);
int test_one_if_equal(int, int);
int one_if_negative(int);
int test_one_if_negative(int);
int one_if_non_zero(int);
int test_one_if_non_zero(int);
int one_if_not_equal(int, int);
int test_one_if_not_equal(int, int);
int one_if_max_twos_complement(int);
int test_one_if_max_twos_complement(int);
int one_if_min_twos_complement(int);
int test_one_if_min_twos_complement(int);
int one_if_zero(int);
int test_one_if_zero(int);
int negative_one();
int test_negative_one();
int boundary_add(int, int);
int test_boundary_add(int, int);
int sign_bit(int);
int test_sign_bit(int);
int twos_complement_max();
int test_twos_complement_max();
int twos_complement_min();
int test_twos_complement_min();
unsigned real_absolute_value(unsigned);
unsigned test_real_absolute_value(unsigned);
int real_to_int(unsigned);
int test_real_to_int(unsigned);
unsigned int_to_float(int);
unsigned test_int_to_float(int);
unsigned real_negation(unsigned);
unsigned test_real_negation(unsigned);
