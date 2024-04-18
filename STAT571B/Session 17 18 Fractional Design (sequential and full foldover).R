# Refer to 6_Blocking_Fractional_Factorial
# Since data was not provided

# Aliasing: Setting the effects equal. i.e. confounding purposefully (avoid aliasing lower order effects with each other)
#.          A=BCDEF good, ABCD=CDEF good, but A=BC not so good

# Recall, you can choose n-1 confounding factors
# The last one depends on the exp mod 2 of the others
# For 1/2 fractional design, you would only do the positive for example
# and then multiply by -1. So half of the confounding factor aliases is 
# (I=ABC)=ABD=CD, (multiply by A/B/C sequentially) A=BC=BD=ACD, B=AC=AD=BCD;
# other half is (I = ABC) = -ABD = -CD, A= BC = -BD = -ACD, etc


# Design Resolution: How to alias
#     - Rule of thumb: The resolution of a 2-level fractional factorial design is 
#                    equal to the sum of the two orders
#          - e.g. Resolution III: 1st order aliased with 2nd order, but not 1st orders with each other
#                 Resolution IV: 1 with 3, 2 with 2 allowed
# Resolution III prevents 1st order effects from confounding and minimizes the sample size. Great if you are
#                willing to not consider 3-way and above interactions

# Fold-over fractional design



