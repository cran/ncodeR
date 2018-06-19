# Individual Pieces
data(RS.data)
rs = RS.data

# Generate a CodeSet for all Codes
newcode = create.code(name = "Data", expressions = c("number","data"), excerpts = rs$text)

# Handcode 4 excerpts from RSData
newcode.h = handcode(code = newcode, excerpts = rs$text, n = 4)

# Run test to see rho/kappa of current test set
newcode.t = test(code = newcode.h, kappaThreshold = 0.65)

# View the summary, with the calcuated statistics
summary(newcode.t)

# View the summary of just the test
summary(newcode.t$statistics)

# Resolve the differences in the test set
newcode.r = resolve(code = newcode.t)

summary(newcode.r)


newcode.h2 = handcode(code = newcode.r, excerpts = rs$text, n = 10)
newcode.t2 = test(code = newcode.h2, kappaThreshold = 0.65)
summary(newcode.t2)

newcode.h2 = handcode(code = newcode.h2, excerpts = rs$text, n = 10)
newcode.t2 = test(code = newcode.h2, kappaThreshold = 0.65)
summary(newcode.t2)

allcoded = autocode(code = newcode)#.t2)

# Convert the coded results to a data.frame
allcoded.data = as.data.frame(allcoded)
