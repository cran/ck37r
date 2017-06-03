
data(Boston, package = "MASS")
library(randomForest)

set.seed(1)

outcome = Boston$medv
data = Boston[, -14]

# This works ok.
rf = randomForest(outcome ~ ., data = data, keep.forest = T)
pryr::object_size(rf)

# But not the SuperLearner version.
library(SuperLearner)
sl_rf = SuperLearner(outcome, data, family = "gaussian",
                  SL.library = c("SL.mean", "SL.randomForest"))
# Doesn't work.
object_size(sl_rf)
# object.size() works though.
object.size(sl_rf)

# With glmnet it does work though.
sl = SuperLearner(outcome, data, family = "gaussian",
                  SL.library = c("SL.mean", "SL.glmnet"))
object_size(sl)

# Dig into the sl_rf details.
obj = sl_rf$fitLibrary$SL.randomForest_All$object

# Fails on $terms element of type "LANGSXP"
for (name in names(obj)) {
  elm = obj[[name]]
  cat("Element:", name, "Type:", pryr::sexp_type(elm), "Size:")
  try(cat(pryr::object_size(elm)))
  cat("\n")
}

# Dig into the terms attributes
terms = obj$terms

# All of these are ok.
for (attr_name in names(attributes(terms))) {
  elm = attr(obj, attr_name, exact = T)
  cat("Attribute:", attr_name, "Type:", pryr::sexp_type(elm), "Size:")
  try(cat(pryr::object_size(elm)))
  cat("\n")
}
