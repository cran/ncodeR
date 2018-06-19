# Load some data
data(RS.data)
rs = RS.data

###
# Generate a CodeSet for all Codes
###
code.set = code.set("Demo RS CodeSet", "CodeSet made for the demo", excerpts = rs$text, codes = c())


###
# Create the Data code
###
code.data = create.code(name = "Data", expressions = c("number","data"), codeSet = code.set)

# Handcode 30 excerpts for Data code
code.data = handcode(code = code.data, n=5)

# Run test to see rho/kappa of current test set
code.data = test(code = code.data, kappaThreshold = 0.65)

# View the summary, with the calcuated statistics
summary(code.data)



# Create the People code
code.people = create.code(name = "People", expressions = c("people","he", "she", "they"), codeSet = code.set)

# Handcode 30 excerpts for People code
code.people = handcode(code = code.people, n=5)

# Run test
code.people = test(code = code.people, kappaThreshold = 0.65)

summary(code.people)



#  Autocde the full set of excerpts
allcoded = autocode(codeSet = code.set)


# Convert the coded results to a data.frame
allcoded.data = as.data.frame(allcoded)
