/* Testing Code */

#include <limits.h>
#include <math.h>

/* Routines used by floation point test code */

/* Convert from bit level representation to floating point number */
float u2f(unsigned u) {
  union {
    unsigned u;
    float f;
  } a;
  a.u = u;
  return a.f;
}

/* Convert from floating point number to bit-level representation */
unsigned f2u(float f) {
  union {
    unsigned u;
    float f;
  } a;
  a.f = f;
  return a.u;
}

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
int test_bit_set_odd(int a) {
    int i;
    for (i = 1; i < 32; i+=2)
        if (a & (1<<i))
      return 1;
    return 0;
}
int test_logical_not(int a)
{
  return !a;
}
int test_bitwise_and(int a, int b)
{
  return a&b;
}
int test_bit_sum(int a) {
  int result = 0;
  int i;
  for (i = 0; i < 32; i++)
    result += (a >> i) & 0x1;
  return result;
}
int test_bitwise_or(int a, int b)
{
  return a|b;
}
int test_bit_sum_odd(int a) {
  int result = 0;
  int i;
  for (i = 0; i < 32; i++)
    result ^= (a >> i) & 0x1;
  return result;
}
int test_bitwise_xor(int a, int b)
{
  return a^b;
}
int test_byte_trade(int a, int i, int j)
{
    /* little endiamachine */
    /* least significant byte stored first */
    unsigned int imask, jmask;
    switch(i) {
    case 0:
      imask = a & 0xFF;
      a &= 0xFFFFFF00;
      break;
    case 1:
      imask = (a & 0xFF00) >> 8;
      a &= 0xFFFF00FF;
      break;
    case 2:
      imask = (a & 0xFF0000) >> 16;
      a &= 0xFF00FFFF;
      break;
    default:
      imask = ((unsigned int)(a & 0xFF000000)) >> 24;
      a &= 0x00FFFFFF;
      break;
    }
    switch(j) {
    case 0:
      jmask = a & 0xFF;
      a &= 0xFFFFFF00;
      break;
    case 1:
      jmask = (a & 0xFF00) >> 8;
      a &= 0xFFFF00FF;
      break;
    case 2:
      jmask = (a & 0xFF0000) >> 16;
      a &= 0xFF00FFFF;
      break;
    default:
      jmask = ((unsigned int)(a & 0xFF000000)) >> 24;
      a &= 0x00FFFFFF;
      break;
    }
    imask <<= 8*j;
    jmask <<= 8*i;
    return a | imask | jmask;
}
int test_compact_if_then(int a, int b, int c)
{
  return a?b:c;
}
int test_distill_byte(int a, int i)
{
    unsigned char byte;
    switch(i) {
    case 0:
      byte = a;
      break;
    case 1:
      byte = a >> 8;
      break;
    case 2:
      byte = a >> 16;
      break;
    default:
      byte = a >> 24;
      break;
    }
    return (int) (unsigned) byte;
}
int test_one_if_reversible(int a) {
    int result = 1;
    int i;
    int mask = 0xFFFF;
    int alo = a & mask;
    int ahi = (a >> 16) & mask;
    for (i = 0; i < 16; i++) {
 int flipi = 15-i;
 int bhigh = (ahi >> i) & 0x1;
 int blow = (alo>> flipi) & 0x1;
 result = result && (bhigh == blow);
    }
    return result;
}
int test_lsb_bit_mask(int a) {
  int mask = 1;
  if (a == 0)
    return 0;
  while (!(mask & a)) {
    mask = mask << 1;
  }
  return mask;
}
int test_sign_flip(int a) {
  return -a;
}
int test_byte_replace(int a, int i, int c)
{
    switch(i) {
    case 0:
      a = (a & 0xFFFFFF00) | c;
      break;
    case 1:
      a = (a & 0xFFFF00FF) | (c << 8);
      break;
    case 2:
      a = (a & 0xFF00FFFF) | (c << 16);
      break;
    default:
      a = (a & 0x00FFFFFF) | (c << 24);
      break;
    }
    return a;
}
int test_wheel_right(int a, int b) {
  unsigned u = (unsigned) a;
  int i;
  for (i = 0; i < b; i++) {
      unsigned lsb = (u & 1) << 31;
      unsigned rest = u >> 1;
      u = lsb | rest;
  }
  return (int) u;
}
int test_left_fill(int x) {
  int result = 0;
  int i;
  for (i = 0; i < x; i++)
    result |= (1 << (31-i));
  return result;
}
int test_absolute_value(int v) {
  return (v < 0) ? -v : v;
}
int test_add_no_overflow(int x, int y)
{
    long long lsum = (long long) x + y;
    return lsum == (int) lsum;
}
int test_denominator_2_to_n(int a, int n)
{
    int p2n = 1<<n;
    return a/p2n;
}
int test_quick_seventy_five_percent(int a)
{
  return (a*3)/4;
}
int test_one_if_ascii(int a) {
  return (0x30 <= a) && (a <= 0x39);
}
int test_one_if_equal(int a, int b)
{
  return a == b;
}
int test_one_if_negative(int a) {
  return a < 0;
}
int test_one_if_non_zero(int a)
{
  return a!=0;
}
int test_one_if_not_equal(int a, int b)
{
  return a != b;
}
int test_one_if_max_twos_complement(int a) {
    return a == 0x7FFFFFFF;
}
int test_one_if_min_twos_complement(int a) {
    return a == 0x80000000;
}
int test_one_if_zero(int a) {
  return a == 0;
}
int test_negative_one(void) {
  return -1;
}
int test_boundary_add(int a, int b)
{
  if (a > 0 && b > 0 && a+b < 0)
    return (0x7FFFFFFF);
  if (a < 0 && b < 0 && a+b >= 0)
    return (0x80000000);
  return a + b;
}
int test_sign_bit(int a) {
    if ( !a ) return 0;
    return (a < 0) ? -1 : 1;
}
int test_twos_complement_max(void) {
  return 0x7FFFFFFF;
}
int test_twos_complement_min(void) {
  return 0x80000000;
}
unsigned test_real_absolute_value(unsigned r) {
  float f = u2f(r);
  unsigned unf = f2u(-f);
  if (isnan(f))
    return r;
  /* An unfortunate hack to get around a limitation of the BDD Checker */
  if ((int) r < 0)
      return unf;
  else
      return r;
}
int test_real_to_int(unsigned r) {
  float f = u2f(r);
  int x = (int) f;
  return x;
}
unsigned test_int_to_float(int i) {
  float f = (float) i;
  return f2u(f);
}
unsigned test_real_negation(unsigned r) {
    float f = u2f(r);
    float nf = -f;
    if (isnan(f))
 return r;
    else
 return f2u(nf);
}
