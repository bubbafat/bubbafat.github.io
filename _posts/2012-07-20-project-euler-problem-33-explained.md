---
title: "Project Euler Problem 33 Explained"
date: "2012-07-20"
categories: 
  - "Programming#Misc"
---

I haven’t been blogging about each answer but I wanted to say something about problem 33 because the wording of the problem is very confusing.

Here’s what they wrote with some comments about what they mean.

<blockquote>
The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
</blockquote>

The point here is that a curious fraction is one that has the following properties:

- The numerator and denominator have a common digit (in this case both contain a 9)
- If that common digit is removed the resulting fraction is identical
- That #2 is true is (in this case) coincidence and not a pattern that can be repeated (i.e., 48/74 != 8/7)

{% include tip.html content="We shall consider fractions like, 30/50 = 3/5, to be trivial examples." %}

- 30/50 = 3/5 is similar to 49/98 = 4/8
  - 0 is the common number
  - When 0 is removed the fraction remains the same
- They call this a _trivial example_ of a curious fraction. Basically they mean that using mod 10 makes this trivial.

<blockquote>
There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
</blockquote>

You need to find these four fractions. They gave you one of them above (49/98). Notice the three restrictions they applied.

1. The numerator and denominator will have two digits (10...99)
2. The fraction will be less than one (numerator < denominator)
3. They are non-trivial (i.e., they are not mod 10)

<blockquote>
If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
</blockquote>

Ultimately you will end up with four fractions – when you have them, multiply them all together and the answer is the denominator in lowest common terms.

Once I understood the problem the solution was trivial. Basically it was

- For each potential fraction (10..99 / 10..99 )
  - If the num or denom are mod 10, skip it
  - if the num and denom share a common digit
- divide num and denom and store the result in ‘expected’
- remove the common digit from num and denom (generating num’ and denom’)
- divide num’ and denom’ and compare the result to ‘expected’
- if the results match then I have found one of the four answers
  - next
- Multiply the four fractions
- Report the denominator in lowest common terms to the website

If you’ve done every problem to this point then you should have no trouble doing this – in fact I had to write almost no new code. The euler.py module I’ve been adding every time I reuse something had pretty much everything I needed.
