/* 
 * CS 045 Datalab 
 * 
 * <Please put your name and userid here>
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting if the shift amount
     is less than 0 or greater than 31.


EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implement floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants. You can use any arithmetic,
logical, or comparison operations on int or unsigned data.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operations (integer, logical,
     or comparison) that you are allowed to use for your implementation
     of the function.  The max operator count is checked by dlc.
     Note that assignment ('=') is not counted; you may use as many of
     these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
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
/* 
 * bit_set_odd -
 *   return 1 if any odd-numbered bit in word set to 1
 *   where bits are numbered from 0 (least significant) to 31 (most significant)
 *   For example:
 *       bit_set_odd(0x5) = 0,
 *       bit_set_odd(0x7) = 1
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 12
 *   Difficulty: 2
 */

int bit_set_odd(int a) {
    int m8  = 0xaa;
    int m16 = (m8  << 8)  | m8;
    int m32 = (m16 << 16) | m16;
    return !!(a & m32);
}
/*
 * logical_not -
 *   Compute !a without using !
 *   For example:
 *      logical_not(3) = 0
 *      logical_not(0) = 1
 *   Allowed operators: ~ & ^ | + << >>
 *   Maximum operators: 12
 *   Difficulty: 4
 */
int logical_not(int a) {
    a = ((a | ((~a) + 1)) >> 31) + 1;

    return a;
}
/*
 * bitwise_and - a&b using only ~ and |
 *   For example:
 *       bitwise_and(6, 5) = 4
 *   Allowed operators: ~ |
 *   Maximum operators: 8
 *   Difficulty: 1
 */
int bitwise_and(int a, int b) {
    return (~(~a | ~b));
}
/*
 * bit_sum -
 *   returns count of number of 1's in word
 *   For example:
 *       bit_sum(5) = 2
 *       bit_sum(7) = 3
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 40
 *   Difficulty: 4
 */
int bit_sum(int a) {
    int mask2_8  = 0x55;
    int mask2_16 = mask2_8  | (mask2_8 << 8);
    int mask2_32 = mask2_16 | (mask2_16 << 16);
    int mask4_8  = 0x33;
    int mask4_16 = mask4_8 | (mask4_8 << 8);
    int mask4_32 = mask4_16 | (mask4_16 << 16);
    int mask8_8  = 0x0f;
    int mask8_16 = mask8_8  | (mask8_8 << 8);
    int mask8_32 = mask8_16 | (mask8_16 << 16);
    int mask16_32 = 0xff | (0xff << 16);
    int mask32_32 = 0xff | (0xff << 8);

    int sum2R = a & mask2_32;
    int sum2L = (a >> 1) & mask2_32;
    int sum2  = sum2R + sum2L;

    int sum4R = sum2 & mask4_32;
    int sum4L = (sum2 >> 2) & mask4_32;
    int sum4  = sum4R + sum4L;

    int sum8R = sum4 & mask8_32;
    int sum8L = (sum4 >> 4) & mask8_32;
    int sum8  = sum8R + sum8L;

    int sum16R = sum8 & mask16_32;
    int sum16L = (sum8 >> 8) & mask16_32;
    int sum16  = sum16R + sum16L;

    int sum32R = sum16 & mask32_32;
    int sum32L = (sum16 >> 16) & mask32_32;
    int sum32  = sum32R + sum32L;

    return sum32;
}
/*
 * bitwise_or -
 *   a|b using only ~ and &
 *   For example:
 *       bitwise_or(6, 5) = 7
 *   Allowed operators: ~ &
 *   Maximum operators: 8
 *   Difficulty: 1
 */
int bitwise_or(int a, int b) {
    return (~(~a & ~b));
}
/*
 * bit_sum_odd -
 *   returns 1 if x contains an odd number of 0's
 *   For example:
 *   	bit_sum_odd(5) = 0
 *   	bit_sum_odd(7) = 1
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 20
 *   Difficulty: 4
 */
int bit_sum_odd(int a) {
    int i16 = (a  ^ (a >> 16));
    int i8  = (i16 ^ (i16 >> 8));
    int i4  = (i8  ^ (i8 >> 4));
    int i2  = (i4  ^ (i4 >> 2));
    int i1  = (i2 ^ (i2 >> 1));
    return i1 & 0x01;
}
/*
 * bitwise_xor -
 *   a^b using only ~ and &
 *   For example:
 *   	bitwise_xor(4, 5) = 1
 *   Allowed operators: ~ &
 *   Maximum operators: 14
 *   Difficulty: 1
 */
int bitwise_xor(int a, int b) {
    return ~(~a & ~b) & ~(a & b);
}
/*
 * byte_trade - swaps the ith byte and the jth byte
 *  For example:
 *  	byte_trade(0x12345678, 1, 3) = 0x56341278
 *      byte_trade(0xDEADBEEF, 0, 2) = 0xDEEFBEAD
 *  You may assume that 0 <= i <= 3, 0 <= j <= 3
 *  Allowed operators: ! ~ & ^ | + << >>
 *  Maximum operators: 25
 *  Difficulty: 2
 */
int byte_trade(int a, int i, int j) {
    int iBits = i << 3;
    int jBits = j << 3;
    int ith = a >> iBits;
    int jth = a >> jBits;
    int swap = (ith ^ jth) & 0xff;

    a = a ^ (swap << iBits);
    a = a ^ (swap << jBits);

    return a;
}
/*
 * compact_if_then - same as a ? b : c
 *   For example:
 *     	compact_if_then(2,4,5) = 4
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 16
 *   Difficulty: 3
 */
int compact_if_then(int a, int b, int c) {
    int opp = !a;
    opp = ((opp << 31) >> 31);
    return (opp & c) | (~opp & b);
}
/*
 * distill_byte -
 *   Return byte i from word a
 *   Bytes numbered from 0 (least significant) to 3 (most significant)
 *   For example:
 *      distill_byte(0x12345678,1) = 0x56
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 6
 *   Difficulty: 2
 */
int distill_byte(int a, int i) {
    int dist = i << 3;
    a = (a >> dist) & 0xff;

    return a;
}
/*
 * one_if_reversible -
 *   Return 1 if bit pattern in a is equal to its mirror image
 *   For example:
 *     	 one_if_reversible(0x01234567E6AC2480) = 1
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 45
 *   Difficulty: 4
 */
int one_if_reversible(int a) {
    int one16 = 0xff | (0xff << 8);  // 0b00000000 00000000 11111111 11111111
    int one4 = 0xf0 | (0xf0 << 8);  // 0b00000000 00000000 11110000 11110000
    int one2 = 0xcc | (0xcc << 8);  // 0b00000000 00000000 11001100 11001100
    int one1 = 0xaa | (0xaa << 8);  // 0b00000000 00000000 10101010 10101010

    int b3_2 = (a >> 16) & one16;  // Of bytes 0-3
    int p;

    b3_2 = (b3_2 >> 8) | ((b3_2 & 0xff) << 8);  // Swap bytes 2 and 3
    b3_2 = ((b3_2 & one4) >> 4) | ((b3_2 & ~one4) << 4);  // Swap... kinda recursively you get it
    b3_2 = ((b3_2 & one2) >> 2) | ((b3_2 & ~one2) << 2);
    b3_2 = ((b3_2 & one1) >> 1) | ((b3_2 & ~one1) << 1);
    p = ((a & one16) ^ (b3_2)) + 1;
    return !(p + (~1 + 1));
}  //
/*
 * lsb_bit_mask -
 *    return a mask that marks the position of the
 *               least significant 1 bit. If a == 0, return 0
 *   For example:
 *       	lsb_bit_mask(96) = 0x20
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 6
 *   Difficulty: 2
 */
int lsb_bit_mask(int a) {
    int amin = ((~a) + 1);
    amin = amin & a;

    return amin;
}
/*
 * sign_flip -
 *   return -a
 *   For example:
 *       sign_flip(1) = -1.
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 5
 *   Difficulty: 2
 */
int sign_flip(int a) {
    return (~a) + 1;
}
/*
 * byte_replace(a,i,c) - Replace byte i in a with c
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   For example:
 *   : byte_replace(0x12345678,1,0xab) = 0x1234ab78
 *   You can assume 0 <= i <= 3 and 0 <= c <= 255
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 10
 *   Difficulty: 3
 */
int byte_replace(int a, int i, int c) {
    int iBits = (i << 3);
    int eraser;
    eraser = ~(0xff << iBits);
    a = (a & eraser) | (c << iBits);

    return a;
}
/*
 * wheel_right -
 *   Revolve a to the right by i
 *   You can assume that 0 <= i <= 31
 *   For example:
 *     	wheel_right(0x87654321,4) = 0x187654321
 *   Allowed operators: ~ & ^ | + << >> !
 *   Maximum operators: 25
 *   Difficulty: 3
 */
int wheel_right(int a, int i) {
    //int s = ((notI + 1) | i) >> 31;
    int thirtytwoMinusI = 0x41 + (~i);
    //int iMinusOne = i + (~1) + 1;

    int left = (a << thirtytwoMinusI);

    // GRAB BIT THAT WOULD BE LOST
    int lost = (a >> i) & 1;
    // SHIFT BY 1
    int right = a >> 1;
    // ZERO OUT FIRST
    right = right & (~(1 << 31));
    // SHIFT BY N
    right = right >> i;
    // SHIFT BACK ONE
    right = right << 1;
    // OR WITH LOST BIT
    right = right | lost;

    return left | right;
}
/*
 * left_fill -
 *  pads i upper bits with 1's
 *  You may assume 0 <= i <= 32
 *  For example:
 *     left_fill(4) = 0xF0000000
 *  Allowed operators: ! ~ & ^ | + << >>
 *  Maximum operators: 10
 *  Difficulty: 1
 */
int left_fill(int i) {
    int allOne = ~0;
    int iComp = 33 + (~i);
    int halfC1 = iComp >> 1;
    int halfC2 = (iComp + 1) >> 1;
    int mask = allOne << halfC1;
    mask = mask << halfC2;

    return mask;
}
/*
 * absolute_value -
 *   The absolute value of x
 *   For example:
 *   	absolute_value(-1) = 1
 *   	absolute_value(1) = 1
 *   You may assume -t_max <= x <= t_max
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 10
 *   Difficulty: 4
 */
int absolute_value(int a) {
    int isNegative = a >> 31;  // all 1 when a < 0, all 0 when a >= 0
    a = (a ^ isNegative) + (isNegative & 1);

    return a;
}
/*
 *   add_no_overflow - Determine if can compute a+b without overflow
 *   For example:
 *   		 add_no_overflow(0x80000000,0x80000000) = 0
 *		 add_no_overflow(0x80000000,0x70000000) = 1
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 20
 *   Difficulty: 3
 */
int add_no_overflow(int a, int b) {
    int rawSum = a + b;

    int aSign = a >> 31;
    int bSign = b >> 31;
    int sumSign = rawSum >> 31;

    int abDiff = aSign ^ bSign;
    int asDiff = aSign ^ sumSign;
    int bsDiff = bSign ^ sumSign;

    return !!abDiff | (!asDiff & !bsDiff);
}
/*
 *  denominator_2_to_n -
 *  Compute a/(2^n), for 0 <= n <= 30
 *  Round toward zero
 *   For example:
 *     denominator_2_to_n(15,1) = 7
 *     denominator_2_to_n(-33,4) = -2
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 15
 *   Difficulty: 2
 */
int denominator_2_to_n(int a, int n) {  // TODO: FIX THIS
    int isNeg = (a >> 31);
    int quotient = a >> n;
    int isNotEven = !!((quotient << n) ^ a);

    return quotient + (isNeg & isNotEven);
}
/*
 * quick_seventy_five_percent -
 *   multiplies by 75% rounding toward 0,
 *   Should exactly duplicate effect of C expression (a*3/4), including overflow behavior.
 *   For example:
 *	quick_seventy_five_percent(11) = 8
 *	quick_seventy_five_percent(-9) = -6
 *	quick_seventy_five_percent(1073741824) = -268435456 (overflow)
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 12
 *   Difficulty: 3
 */
int quick_seventy_five_percent(int a) {
    int a2 = a + a;
    int a3 = a2 + a;
    int isNeg = (a3 >> 31) & 1;
    int isEven = !!(a3 & 3);
    return (a3 >> 2) + (isNeg & isEven);
}
/*
 * one_if_ascii -
 *   return 1 if 0x30 <= a <= 0x39 (ASCII codes for characters '0' to '9')
 *
 *   For example:
 *            one_if_ascii(0x35) = 1
 *            one_if_ascii(0x3a) = 0
 *            one_if_ascii(0x05) = 0
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 15
 *   Difficulty: 3
 */
int one_if_ascii(int a) {
    int aVal = a + ~0x39;
    int aBig = aVal + 10;
    int lessThan = ((aVal) >> 31);
    int greaThan = !((aBig) >> 31);

    return lessThan & greaThan;
}
/*
 * one_if_equal -
 *   return 1 if a == b, and 0 otherwise
 *   For example:
 *     	 one_if_equal(5,5) = 1
 *     	 one_if_equal(4,5) = 0
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 5
 *   Difficulty: 2
 */
int one_if_equal(int a, int b) {
    return !(a ^ b);
}
/*
 * one_if_negative -
 *   return 1 if a < 0, return 0 otherwise
 *   For example:
 *       one_if_negative(-1) = 1
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 6
 *   Difficulty: 2
 */
int one_if_negative(int a) {
    return (a >> 31) & 1;
}
/* 
 * one_if_non_zero -
 *   Check whether a is nonzero using the legal operators except !
 *   For example:
 *      one_if_non_zero(3) = 1
 *      one_if_non_zero(0) = 0
 *   Allowed operators: ~ & ^ | + << >>
 *   Maximum operators: 10
 *   Difficulty: 4 
 */
int one_if_non_zero(int a) {
    return ((a | (~a + 1)) >> 31) & 1;
}
/* 
 * one_if_not_equal -
 *   return 0 if a == b, and 1 otherwise 
 *   For example:
 *    	one_if_not_equal(5,5) = 0
 *    	one_if_not_equal(4,5) = 1
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 6
 *   Difficulty: 2
 */
int one_if_not_equal(int a, int b) {
    return !!(a ^ b);
}
/*
 * one_if_max_twos_complement -
 *   returns 1 if a is the maximum, two's complement number, and 0 otherwise 
 *   Allowed operators: ! ~ & ^ | +
 *   Maximum operators: 10
 *   Difficulty: 1
 */
int one_if_max_twos_complement(int a) {
    return !(~(a + !(a+1)^(a+1)));
}
/*
 * one_if_min_twos_complement -
 *   returns 1 if a is the minimum, two's complement number,
 *     and 0 otherwise 
 *   Allowed operators: ! ~ & ^ | +
 *   Maximum operators: 10
 *   Difficulty: 1
 */
int one_if_min_twos_complement(int a) {
  int b = ~a;
    return !(~(b + !(b+1)^(b+1)));
}
/*
 * one_if_zero - 
 *   returns 1 if a == 0, and 0 otherwise 
 *   For example:
 *      one_if_zero(5) = 0
 *      one_if_zero(0) = 1
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 2
 *   Difficulty: 1
 */
int one_if_zero(int a) {
    return !a;
}
/* 
 * negative_one -
 *   return a value of -1 
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 2
 *   Difficulty: 1
 */
int negative_one(void) {
  int ans = 0x0;
  ans = ~ans;
  return ans;
}
/*
 *  boundary_add - 
 *          adds two numbers but when positive overflow occurs, returns
 *          maximum possible value, and when negative overflow occurs,
 *          it returns minimum negative value.
 *   For example:
 *          boundary_add(0x40000000,0x40000000) = 0x7fffffff
 *          boundary_add(0x80000000,0xffffffff) = 0x80000000
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 30
 *   Difficulty: 4
 */
int boundary_add(int a, int b) {
    return 2;
}
/* 
 *  sign_bit - 
 *  return 1 if positive, 0 if zero, and -1 if negative
 *  For example:
 *            sign_bit(130) = 1
 *            sign_bit(-23) = -1
 *  Allowed operators: ! ~ & ^ | + << >>
 *  Maximum operators: 10
 *  Difficulty: 2
 */
int sign_bit(int a) {
    int signBit = ((a >> 31) << 1) + 1;
    return signBit & (((!!a) << 31) >> 31);
}
/* 
 * twos_complement_max - return maximum two's complement integer 
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 4
 *   Difficulty: 1
 */
int twos_complement_max(void) {
  int ans = 0x80 << 24;
  return ~ans;
}
/* 
 * tmin - return minimum two's complement integer 
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 4
 *   Difficulty: 1
 */
int twos_complement_min(void) {
    return 1 << 31;
}
/* 
 * real_absolute_value -
 *   Return bit-level equivalent of absolute value of f for floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument..
 *   Allowed operators: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Maximum operators: 10
 *   Difficulty: 2
 */
unsigned real_absolute_value(unsigned r) {
    return 2;
}
/* 
 *   real_to_int -
 *   Return bit-level equivalent of expression (int) r
 *   for floating point argument r.
 *   Argument is passed as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point value.
 *   Anything out of range (including NaN and infinity) should return
 *   0x80000000u.
 *   Allowed operators: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Maximum operators: 30
 *   Difficulty: 4
 */
int real_to_int(unsigned r) {
    unsigned getSign = 0x80000000;
    unsigned getEx = 0x1000000;
    unsigned getFrac = 0x200;

    unsigned mySign = r / getSign;
    int myExponent = ((r * 2) / getEx); // don't forget to account for bias!!!
    int bias = 127;
    unsigned myFrac = (r * getFrac) / getFrac;
    
    int bigE = myExponent - bias;
    int bigM = 0;

    int i;

    if (myExponent == 0) { // if we are in denormalized form
        return 0;
    }
    if (myExponent == 255) { // if we are in a special form
        return 0x80000000u;
    }
    if (bigE > 127) {
        bigM += 1;
        for (i = 0; i < bigE; i ++) {
            bigM *= 2;
        }
    }
    if (bigE < 127) {
        bigM += 1;
        for (i = 0; i < -1 * bigE; i++) {
            bigM = bigM / 2;
        }
    }
    if (mySign) {
        return -1 * (bigM);
    }
    return bigM;
}
/* 
 *  int_to_float -
 *   Return bit-level equivalent of expression (float) i
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Allowed operators: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Maximum operators: 30
 *   Difficulty: 4
 */
unsigned int_to_float(int i) {
    return 2;
}
/* 
 * real_negation -
 *   Return bit-level equivalent of expression -r for
 *   floating point argument r.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument.
 *   Allowed operators: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Maximum operators: 10
 *   Difficulty: 2
 */
unsigned real_negation(unsigned r) {
    return 2;
}
