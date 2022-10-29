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
    int m8  = 0xaa;  // Mask of 10101010 (all odd bits)
    int m16 = (m8  << 8)  | m8;  // moves mask all the way across to cover 32 bits
    int m32 = (m16 << 16) | m16;
    return !!(a & m32);  // double logical not to return a 1 or 0
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
    // I noticed that a and ~a always have opposite most significant bits unless a=0
    // Therefore (a | (~a)) will always have a most significant bit of one unless a = 0
    // Dragging that bit to the end makes 0 -> 0xffffffff and anything else -> 0x00000000
    // Adding one makes 0xffffffff overflow and so 0 -> 1. anything else + 1 = 0

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
    return (~(~a | ~b));  // DeMorgan's Law
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
    int mask2_32 = mask2_16 | (mask2_16 << 16);  // 01010101...01010101
    int mask4_8  = 0x33;
    int mask4_16 = mask4_8 | (mask4_8 << 8);
    int mask4_32 = mask4_16 | (mask4_16 << 16);  // 00110011...00110011
    int mask8_8  = 0x0f;
    int mask8_16 = mask8_8  | (mask8_8 << 8);
    int mask8_32 = mask8_16 | (mask8_16 << 16);  // 00001111...00001111
    int mask16_32 = 0xff | (0xff << 16);  // 00000000 11111111 00000000 11111111
    int mask32_32 = 0xff | (0xff << 8);  // 00000000 00000000 11111111 11111111

    int sum2R = a & mask2_32;  // all even bits
    int sum2L = (a >> 1) & mask2_32;  // all odd bits
    int sum2  = sum2R + sum2L;  // makes every bit pair (0-1, 2-3, ... , 30-31) the sum of the two bits in the pair.

    int sum4R = sum2 & mask4_32;  // Similarly "recurses", but with 4 bits instead of a pair
    int sum4L = (sum2 >> 2) & mask4_32;
    int sum4  = sum4R + sum4L;

    int sum8R = sum4 & mask8_32;  // Then with 8...
    int sum8L = (sum4 >> 4) & mask8_32;
    int sum8  = sum8R + sum8L;

    int sum16R = sum8 & mask16_32;  // 16
    int sum16L = (sum8 >> 8) & mask16_32;
    int sum16  = sum16R + sum16L;

    int sum32R = sum16 & mask32_32;  // Until the whole number is the sum of the two sums of the four sums...
    int sum32L = (sum16 >> 16) & mask32_32;  // Of the original 32 bits!
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
    return (~(~a & ~b));  // DeMorgan's Law
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
    int i16 = (a  ^ (a >> 16));  // If [0-x] contains an odd number of bits, so will [0 - 1/2x] ^ [1/2x - x].
    int i8  = (i16 ^ (i16 >> 8));  // Apply this principle all the way down.
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
    // ~(~a & ~b) is a | b by DeMorgan's
    // ~(a & b) reveals where a != b
    // at least one bit from a and b must be 1, but the bits must not be the same
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
    int iBits = i << 3;  // Changes i and j from byte to bits by multiplying by 8
    int jBits = j << 3;
    int ith = a >> iBits;  // Obtains the i and j bytes...
    int jth = a >> jBits;
    int swap = (ith ^ jth) & 0xff;  // ...and xors them to find the byte "swap"
    // Note that if c = a ^ b...
    // a ^ c = b
    // b ^ c = a

    a = a ^ (swap << iBits);  // Apply that principle here!
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
    int opp = !a;  // a is 0 if true and 1 if false
    opp = ((opp << 31) >> 31);  // make opp a mask where every bit becomes what the least significant bit was
    return (opp & c) | (~opp & b);  // if a is true, opp = 0 and c is removed, if a is false, ~opp = 0 and b is removed
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
    int dist = i << 3;  // Make dist i * 8 to get bits from bytes
    a = (a >> dist) & 0xff;  // Shift over until byte you want is on the least significant edge, then 0 out the rest

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

    int b3_2 = (a >> 16) & one16;  // b3_2 is bytes 2 and 3 of bytes 0 through 3 where 0 is least significant
    int p;

    b3_2 = (b3_2 >> 8) | ((b3_2 & 0xff) << 8);  // Swap bytes 2 and 3
    b3_2 = ((b3_2 & one4) >> 4) | ((b3_2 & ~one4) << 4);  // Swap subbytes... kinda recursively you get it
    b3_2 = ((b3_2 & one2) >> 2) | ((b3_2 & ~one2) << 2);
    b3_2 = ((b3_2 & one1) >> 1) | ((b3_2 & ~one1) << 1);  // At the end, all of b3_2 has been reversed
    p = ((a & one16) ^ (b3_2));  // If reversed two most significant bytes are the same reversed as the two least
    // significant bytes, they xor to zero
    return !p;  // return the opposite of p (if p was 0, it means it was a palindrome, and we want to return 1)
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
    // Strange property I observed - and (-a) with a and all the digits will be different except the least sig 1 bit

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
    return (~a) + 1;  // Pretty simple formula
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
    int iBits = (i << 3);  // Change i from byte to bits
    int eraser = ~(0xff << iBits);  // Make a mask to erase the ith byte
    a = (a & eraser) | (c << iBits);  // Erase ith byte and or with c to get desired byte in desired spot

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
    int thirtytwoMinusI = 0x41 + (~i);  // As name suggests, 32 - i

    int left = (a << thirtytwoMinusI);  // Get the left side of the final product

    // GRAB BIT THAT WOULD BE LOST
    int lost = (a >> i) & 1;
    // SHIFT BY 1
    int right = a >> 1;
    // ZERO OUT FIRST so that we can shift by n and not have fill in with 1s (had to shift one first)
    right = right & (~(1 << 31));
    // SHIFT BY N
    right = right >> i;
    // SHIFT BACK ONE
    right = right << 1;
    // OR WITH LOST BIT to bring back the total data
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
    int iComp = 33 + (~i);  // 32 - i
    int halfC1 = iComp >> 1;  // Have to break how far you shift in two because going 32 in one go is unpredictable
    int halfC2 = (iComp + 1) >> 1;
    int mask = allOne << halfC1;  // Shift halfway twice and there you go!
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
    a = (a ^ isNegative) + (isNegative & 1);  // a ^ isNegative will ~a when a is negative; isNegative & 1 will add 1 to
    // a when a is negative, thus completing the negation formula but only when a is negative

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

    int abDiff = aSign ^ bSign;  // 0 when a and b same sign, all 1 otherwise
    int asDiff = aSign ^ sumSign;  // 0 when a and sum same sign, all 1 otherwise
    int bsDiff = bSign ^ sumSign;  // you get the idea...

    // If a and b are dif signs, works fine,
    // and if a and b both have same sign as their sum, it means it didn't over/underflow
    return (abDiff | ~(asDiff | bsDiff)) & 1;
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
    int a3 = a2 + a;  // Simply add it to itself 3 times
    int isNeg = (a3 >> 31) & 1;  // Checks to see if it was negative and needed to be rounded up
    int isntEven = !!(a3 & 3);
    return (a3 >> 2) + (isNeg & isntEven);  // Divides by 4 and adds back 1 if it needed to be rounded up
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
    int aVal = a + ~0x39;  // a minus max ascii value
    int aBig = aVal + 10;  // a minus minimum ascii value
    int lessThan = ((aVal) >> 31);  // checks to see if aVal is negative (a less than max ascii value)
    int greaThan = !((aBig) >> 31);  // checks to see if aBig is positive (a greater than min ascii value)

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
    return !(a ^ b);  // a ^ b will not equal zero if they are different
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
    return (a >> 31) & 1;  // most sig bit indicates sign of a
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
    return ((a | (~a + 1)) >> 31) & 1;  // As I observed above in logical_not, (a | (~a + 1) will always have the most
    // sig bit as 1 unless it is 0, allowing us to drag this bit over to observe whether or not a is zero
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
    return !!(a ^ b);  // Exactly like the test for equality, but you just have to not it
}
/*
 * one_if_max_twos_complement -
 *   returns 1 if a is the maximum, two's complement number, and 0 otherwise 
 *   Allowed operators: ! ~ & ^ | +
 *   Maximum operators: 10
 *   Difficulty: 1
 */
int one_if_max_twos_complement(int a) {
    return !(~(a + !(a+1)^(a+1)));  // Monty did this one, not me, so I have no clue how it works
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
    return !(~(b + !(b+1)^(b+1)));  // Monty also did this one
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
    return !a;  // Pretty easy, !0 is 1
}
/* 
 * negative_one -
 *   return a value of -1 
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 2
 *   Difficulty: 1
 */
int negative_one(void) {
  int ans = 0x0;  // all 0
  ans = ~ans;  // all 1 (= -1)
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
    // collaborated with another group to get this because it was awful
    // create the sum and get the sign of the sum
    int sum = a + b;
    int sums = sum >> 31;
    // get the signs of a and b
    int schka = a >> 31;
    int schkb = b >> 31;
    // compare whether 2 negative nums stay negative after sum and whether two positive nums saty positive after sum
    int nchk = schka & schkb;
    int nover = ~sums & nchk;
    int pchk = (schka | schkb) ^ ~0x00;
    int pover = sums & pchk;
    // if + overflow occurs set pover to max and if - overflow occurs set nover to min, if either occurs sum is set to 0
    int min = (0x01 << 31);
    sum = sum & (~pover) & (~nover);
    nover = nover & min;
    pover = pover & ~min;
    // fuse sum, pover, and nover
    return sum | nover | pover;
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
int sign_bit(int a) {  // Sign
    int signBit = ((a >> 31) << 1) + 1;  // Multiplies sign bit by 2 and adds 1 - -1 becomes -1 and 0 becomes 1
    return signBit & (((!!a) << 31) >> 31);  // ands with all 1s or 0 if a if not zero or 0
}
/* 
 * twos_complement_max - return maximum two's complement integer 
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 4
 *   Difficulty: 1
 */
int twos_complement_max(void) {
  int ans = 1 << 31;  // ~10000...000 = 01111...1111
  return ~ans;
}
/* 
 * tmin - return minimum two's complement integer 
 *   Allowed operators: ! ~ & ^ | + << >>
 *   Maximum operators: 4
 *   Difficulty: 1
 */
int twos_complement_min(void) {
    return 1 << 31;  // Pretty obvyous
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
unsigned real_absolute_value(unsigned r) {  // TODO
    int exp = (r<<1)>>24;
    if(!(exp^0xff) && r<<9){
        return r;
    }
    r = r<<1;
    return r>>1;
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
    // Monty did this one; I don't understand it
    int mySign = r >> 31;
    int myEx = (r >> 23) & 0xff;
    int myFrac = r & 0x007fffff;

    int ans = 0;

    myFrac = myFrac | 0x00800000;
    myEx = myEx - 127;

    if (myEx < 0) { 
        return 0;
    }
    if (myEx > 30) { // overflow
        return 0x80000000u;
    }
    if (myEx < 23) {
        ans = myFrac >> (23 - myEx);
    }
    if (myEx > 23) {
        ans = myFrac << (myEx - 23);
    }
    if (mySign) {
        ans = -1 * ans;
    }    
    
    // evil floating point bit level hacking
    return ans;
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
    unsigned sign;
    unsigned exponent = 150;
    unsigned missing = 0;
    int place = 0;
    int halfway;
    int roundup = 0;
    int missingIsHalf;
    int lastBitI;

    // SPECIAL CASES for i is 0 or minimum
    if (i == 0) {
        return 0;
    }
    if (i == 0x80000000) {
        return 0xcf000000;
    }

    // GETTING BIT SIGN AND MAKING I POSITIVE
    sign = i & 0x80000000;  // Gets sign bit but leaves in place
    if (sign) {
        i = -i;
    }

    // Making i 24 bits
    if (i > 0xffffff) {  // If i is too large and must be compressed by increasing exponent
        while (i > 0xffffff) {
            missing += ((i & 1) << place++);  // Add the missing bits to missing so we can round later
            i = i >> 1;
            exponent++;
        }
        lastBitI = i & 1;
        if (missing > 0) {  // If there were missing bits...
            halfway = (1 << (place - 1));
            missingIsHalf = missing == halfway;
            if (missing < halfway) {
            } else if (missingIsHalf) { // used to be if (missing < halfway) {}
                if (lastBitI) {
                    roundup = 1;
                }
            } else {
                roundup = 1;
            }
            if (roundup) {
                if (i != 0xffffff) {
                    if (missingIsHalf) {
                        if (i & 1) {
                            i++;
                        }
                    } else {
                        i++;
                    }
                } else {
                    if (missingIsHalf) {
                        if (i & 1) {  // Avoid overflow by adding to exponent manually
                            i = 1 << 23;
                            exponent++;
                        }
                    } else {
                        i = 1 << 23;
                        exponent++;
                    }
                }
            }
        }
    } else if (i < 0x800000) {  // If i is too small and must be expanded by subtracting from exponent
        while (i < 0x800000) {  // TODO: Replace if?
            i = i << 1;
            exponent--;
        }
    }

    i = i - 0x800000;

    exponent = exponent << 23;
    return sign + exponent + i;
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
    unsigned mask = 0x80000000;
    if (r - 0x7f800001 < 0x7fffff) {
        return r;
    }
    if (r > 0xff800000) {
        return r;
    }

    return r + mask;
}
