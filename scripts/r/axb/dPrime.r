# dPrime example

library(devtools)
source_url("https://gist.githubusercontent.com/jvcasill/78300788c66e8cf570bf/raw/5d1eae28a50bf6d7f7281d7809bb53feeafeb3b4/dPrime")

# Ex. 
stimDiff0 <- c(rep("diff", 20), rep("same", 5))
stimSame0 <- c(rep("diff", 10), rep("same", 15))

stimDiff1 <- c(rep("diff", 10), rep("same", 15))
stimSame1 <- c(rep("diff", 15), rep("same", 10))

stimDiff2 <- c(rep("diff", 19), rep("same", 6))
stimSame2 <- c(rep("diff", 11), rep("same", 14))

stimDiff3 <- c(rep("diff", 21), rep("same", 4))
stimSame3 <- c(rep("diff",  9), rep("same", 16))

stimDiff4 <- c(rep("diff", 18), rep("same", 7))
stimSame4 <- c(rep("diff", 12), rep("same", 13))

stimDiff5 <- c(rep("diff", 22), rep("same", 3))
stimSame5 <- c(rep("diff", 14), rep("same", 11))

stimDiff <- c(stimDiff0, stimDiff1, stimDiff2,
              stimDiff3, stimDiff4, stimDiff5)
stimSame <- c(stimSame0, stimSame1, stimSame2,
              stimSame3, stimSame4, stimSame5)
subject <- rep(0:5, each = 25)
group   <- c(rep("bi", 75), rep("mo", 75))

x <- data.frame(
    subject  = subject,
    group    = group, 
    stimDiff = stimDiff, 
    stimSame = stimSame)

# Subset of bilinguals/monolinguals
bi <- x[x$group == 'bi', ]
mo <- x[x$group == 'mo', ]

# Calculate dPrime for each subject by group
dPrime.values.bi <- sapply(split(bi, bi$subject), dPrime)
dPrime.values.mo <- sapply(split(mo, mo$subject), dPrime)


# Construct a new dataframe
df <- data.frame(
    subject = names(c(dPrime.values.bi, dPrime.values.mo)),
    group   = c(rep(levels(x$group)[1], each = length(dPrime.values.bi)),
               rep(levels(x$group)[2], each = length(dPrime.values.mo))),
    dPrime  = c(dPrime.values.bi, dPrime.values.mo)
    )

print(df)




