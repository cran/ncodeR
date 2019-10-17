# Individual Pieces
data(RS.data)
rs = RS.data

# Generate a CodeSet for all Codes
newcode = create.code(name = "Data", expressions = c("number","data"), excerpts = rs$text)

# Handcode 4 excerpts from RSData
newcode = handcode(code = newcode, excerpts = rs$text, n = 4)

# Run test to see rho/kappa of current test set
newcode = test(code = newcode, kappa_threshold = 0.65)

# View the summary, with the calcuated statistics
summary(newcode)

# View the summary of just the test
summary(newcode$statistics)

# Resolve the differences in the test set
newcode = resolve(code = newcode)

summary(newcode)

newcode.h2 = handcode(code = newcode.r, excerpts = rs$text, n = 10)
newcode.t2 = test(code = newcode.h2, kappaThreshold = 0.65)
summary(newcode.t2)

newcode.h2 = handcode(code = newcode.h2, excerpts = rs$text, n = 10)
newcode.t2 = test(code = newcode.h2, kappaThreshold = 0.65)
summary(newcode.t2)

# Returns a data.frame 
allcoded = autocode(x = newcode.t2)

# Returns back a Code object, with an updated $computerSet 
allcoded = autocode(x = newcode.t2, simplify = F)

# Convert the Code object directly to a data.frame
allcoded.data = as.data.frame(newcode.t)
