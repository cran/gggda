## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 6, fig.align = "center"
)

## ----sort_by, echo=FALSE------------------------------------------------------
# define `sort_by()` if R version is < 4.4.0
if ( as.integer(R.Version()$major) < 4L ||
     (R.Version()$major == "4" && as.double(R.Version()$minor) < 4) ) {
  .formula2varlist <- 
    function(formula, data, warnLHS = TRUE, ignoreLHS = warnLHS) {
      if (!inherits(formula, "formula")) stop("'formula' must be a formula")
      if (!is.list(data) && !is.environment(data)) data <- as.data.frame(data)
      if (length(formula) == 3) {
        if (warnLHS) {
          if (ignoreLHS)
            warning("Unexpected LHS in 'formula' has been ignored.")
          else
            warning("Unexpected LHS in 'formula' has been combined with RHS.")
        }
        if (ignoreLHS) formula <- formula[-2]
      }
      ## If formula = ~list(...)
      if (length(formula[[2]]) > 1L && formula[[2]][[1]] == quote(list)) {
        ans <- eval(formula[[2]], data, environment(formula))
      }
      else {
        fterms <- stats::terms(formula)
        ans <- eval(attr(fterms, "variables"), data, environment(formula))
        names(ans) <- attr(fterms, "term.labels")
      }
      ans
    }
  sort_by <- function(x, y, ...) UseMethod("sort_by")
  sort_by.default <- function(x, y, ...) x[order(y, ...)]
  sort_by.data.frame <- function(x, y, ...) {
    if (inherits(y, "formula")) y <- .formula2varlist(y, x)
    if (!is.list(y)) y <- list(y)
    o <- do.call(order, c(unname(y), list(...)))
    x[o, , drop = FALSE]
  }
}

## ----setup--------------------------------------------------------------------
library(gggda)
theme_set(theme_bw())

## ----data---------------------------------------------------------------------
print(USJudgeRatings[sample(nrow(USJudgeRatings), 6L, replace = FALSE), ])

## ----frame--------------------------------------------------------------------
USJudgeRatings %>% 
  tibble::rownames_to_column(var = "NAME") ->
  judge_ratings
head(judge_ratings)

## ----rank---------------------------------------------------------------------
judge_ratings %>% 
  subset(select = c(NAME, RTEN)) %>% 
  sort_by(~ list(-RTEN)) %>% 
  transform(RANK = seq(nrow(judge_ratings))) %>% 
  head()

## ----text---------------------------------------------------------------------
judge_plot <- ggplot(judge_ratings, aes(x = INTG, y = DECI, label = NAME))
judge_plot +
  geom_text(aes(label = NAME), size = 3)

## ----peel---------------------------------------------------------------------
judge_plot +
  geom_text(size = 3, hjust = "outward", vjust = "outward") +
  stat_peel(num = Inf, color = "black", fill = "transparent")

## ----bar----------------------------------------------------------------------
judge_ratings %>% 
  subset(select = c(INTG, DECI)) %>% 
  peel_hulls(num = Inf) %>% 
  as.data.frame() %>% 
  merge(
    transform(judge_ratings, i = seq(nrow(judge_ratings))),
    by = "i"
  ) %>% 
  subset(select = -c(i, x, y, prop)) %>% 
  sort_by(~ hull + NAME) ->
  judge_hulls
judge_hulls %>% 
  subset(subset = hull %in% range(hull))

## ----order--------------------------------------------------------------------
judge_hulls %>% 
  transform(hull = factor(hull, levels = seq(max(hull)))) %>% 
  ggplot(aes(x = hull, y = RTEN)) +
  geom_boxplot()

## ----bag----------------------------------------------------------------------
judge_plot +
  stat_bagplot(fraction = 1/3, coef = 3) +
  geom_text(
    data = subset(judge_hulls, hull %in% range(hull)),
    size = 3, hjust = "outward", vjust = "outward"
  )

## ----pca----------------------------------------------------------------------
judge_ratings %>% 
  subset(select = -c(NAME, CONT, RTEN)) %>% 
  princomp(cor = FALSE) ->
  judge_pca

## ----quality------------------------------------------------------------------
summary(judge_pca)
print(judge_pca$loadings)

## ----vector, fig.height=2.5---------------------------------------------------
ggplot(mapping = aes(x = Comp.1, y = Comp.2)) +
  geom_point(data = judge_pca$scores) +
  geom_vector(data = unclass(judge_pca$loadings), color = "#007a3d") +
  coord_equal()

## ----square, fig.height=7-----------------------------------------------------
judge_pca$scores %>% 
  as.data.frame() %>% 
  transform(NAME = judge_ratings$NAME) ->
  judge_scores
judge_pca$loadings %>% 
  unclass() %>% as.data.frame() %>% 
  tibble::rownames_to_column(var = "NAME") ->
  judge_loadings
ggplot(mapping = aes(x = Comp.1, y = Comp.2, label = NAME)) +
  geom_text(data = judge_scores, size = 3) +
  geom_vector(data = judge_loadings, color = "#007a3d",
              stat = "scale", mult = 5) +
  theme(panel.grid = element_blank()) +
  coord_square() +
  scale_x_continuous(sec.axis = sec_axis(~ . / 5)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 5)) +
  expand_limits(x = c(-8, 6))

## ----axis, fig.height=7-------------------------------------------------------
ggplot(mapping = aes(x = Comp.1, y = Comp.2, label = NAME)) +
  geom_axis(data = judge_loadings, color = "#007a3d") +
  geom_text(data = judge_scores, size = 3) +
  theme(panel.grid = element_blank()) +
  coord_square() +
  expand_limits(x = c(-8, 6))

## ----lm-----------------------------------------------------------------------
judge_ratings %>% 
  subset(select = RTEN) %>% 
  cbind(judge_scores) %>% 
  lm(formula = RTEN ~ Comp.1 + Comp.2) ->
  judge_lm
summary(judge_lm)

## ----rule, fig.height=7-------------------------------------------------------
judge_lm %>% 
  coefficients() %>% 
  t() %>% as.data.frame() %>% 
  transform(NAME = "RTEN") ->
  judge_coef
ggplot(mapping = aes(x = Comp.1, y = Comp.2, label = NAME)) +
  geom_axis(data = judge_loadings, color = "#007a3d") +
  geom_text(data = judge_scores, size = 3) +
  stat_rule(data = judge_coef, referent = judge_scores, color = "#ce1126") +
  theme(panel.grid = element_blank()) +
  coord_square() +
  expand_limits(x = c(-8, 6))

## ----subset-------------------------------------------------------------------
judge_ratings %>% 
  subset(subset = grepl("SADEN|DRISCOLL", NAME))

