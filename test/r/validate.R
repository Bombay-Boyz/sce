#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# SCE Phase 3 — R Cross-Validation Script
# ---------------------------------------------------------------------------
# Run this script to obtain reference values that the Haskell tests are
# validated against.  Tolerance: 1e-6 on all test statistics and p-values.
#
# Usage:  Rscript test/r/validate.R
# ---------------------------------------------------------------------------

cat("=== SCE Phase 3 R Cross-Validation ===\n\n")

# ---------------------------------------------------------------------------
# 1. One-sample t-test
# ---------------------------------------------------------------------------
# Data: sepal lengths from iris, first 6 rows; mu0 = 2.5
x1 <- c(2.1, 3.4, 2.8, 3.1, 2.9, 3.5)
res1 <- t.test(x1, mu = 2.5)
cat("--- One-sample t-test (mu0=2.5) ---\n")
cat("t        =", res1$statistic, "\n")
cat("df       =", res1$parameter, "\n")
cat("p-value  =", res1$p.value, "\n")
cat("CI lower =", res1$conf.int[1], "\n")
cat("CI upper =", res1$conf.int[2], "\n\n")

# ---------------------------------------------------------------------------
# 2. Two-sample Welch t-test
# ---------------------------------------------------------------------------
g1 <- c(5.1, 4.9, 4.7, 4.6, 5.0)
g2 <- c(7.0, 6.4, 6.9, 6.5, 6.7)
res2 <- t.test(g1, g2)   # Welch by default
cat("--- Welch two-sample t-test ---\n")
cat("t        =", res2$statistic, "\n")
cat("df       =", res2$parameter, "\n")
cat("p-value  =", res2$p.value, "\n")
cat("CI lower =", res2$conf.int[1], "\n")
cat("CI upper =", res2$conf.int[2], "\n\n")

# ---------------------------------------------------------------------------
# 3. Paired t-test
# ---------------------------------------------------------------------------
before <- c(120, 130, 128, 115, 140)
after  <- c(115, 125, 120, 110, 135)
res3 <- t.test(before, after, paired = TRUE)
cat("--- Paired t-test ---\n")
cat("t        =", res3$statistic, "\n")
cat("df       =", res3$parameter, "\n")
cat("p-value  =", res3$p.value, "\n\n")

# ---------------------------------------------------------------------------
# 4. One-way ANOVA
# ---------------------------------------------------------------------------
vals  <- c(2.1, 3.4, 2.8, 4.1, 3.9, 4.5)
grp   <- factor(c("A","A","A","B","B","B"))
res4  <- summary(aov(vals ~ grp))[[1]]
cat("--- One-way ANOVA ---\n")
cat("F statistic =", res4["grp","F value"], "\n")
cat("df between  =", res4["grp","Df"], "\n")
cat("df within   =", res4["Residuals","Df"], "\n")
cat("p-value     =", res4["grp","Pr(>F)"], "\n\n")

# ---------------------------------------------------------------------------
# 5. Mann-Whitney U
# ---------------------------------------------------------------------------
xu <- c(1, 2, 3, 4, 5)
yu <- c(6, 7, 8, 9, 10)
res5 <- wilcox.test(xu, yu, exact = FALSE, correct = TRUE)
cat("--- Mann-Whitney U ---\n")
cat("W        =", res5$statistic, "\n")
cat("p-value  =", res5$p.value, "\n\n")

# ---------------------------------------------------------------------------
# 6. Wilcoxon signed-rank
# ---------------------------------------------------------------------------
xw <- c(1, 2, 3, 4, 5, 6, 7)
yw <- c(2, 3, 4, 5, 6, 7, 8)
res6 <- wilcox.test(xw, yw, paired = TRUE, exact = FALSE, correct = TRUE)
cat("--- Wilcoxon signed-rank ---\n")
cat("V        =", res6$statistic, "\n")
cat("p-value  =", res6$p.value, "\n\n")

# ---------------------------------------------------------------------------
# 7. Kruskal-Wallis
# ---------------------------------------------------------------------------
xkw <- list(c(1,2,3), c(4,5,6), c(7,8,9))
res7 <- kruskal.test(xkw)
cat("--- Kruskal-Wallis ---\n")
cat("H        =", res7$statistic, "\n")
cat("df       =", res7$parameter, "\n")
cat("p-value  =", res7$p.value, "\n\n")

# ---------------------------------------------------------------------------
# 8. Chi-square test of independence
# ---------------------------------------------------------------------------
mat8 <- matrix(c(15, 35, 30, 20), nrow = 2)
res8 <- chisq.test(mat8, correct = FALSE)
cat("--- Chi-square independence (2x2) ---\n")
cat("chi2     =", res8$statistic, "\n")
cat("df       =", res8$parameter, "\n")
cat("p-value  =", res8$p.value, "\n\n")

# ---------------------------------------------------------------------------
# 9. Fisher's exact test
# ---------------------------------------------------------------------------
mat9 <- matrix(c(6, 2, 2, 6), nrow = 2)
res9 <- fisher.test(mat9)
cat("--- Fisher's exact test ---\n")
cat("odds ratio =", res9$estimate, "\n")
cat("p-value    =", res9$p.value, "\n\n")

# ---------------------------------------------------------------------------
# 10. Pearson correlation
# ---------------------------------------------------------------------------
xp <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
yp <- c(2.1, 4.2, 5.9, 8.1, 10.0, 12.2, 14.1, 15.9, 18.0, 20.1)
res10 <- cor.test(xp, yp, method = "pearson")
cat("--- Pearson correlation ---\n")
cat("r        =", res10$estimate, "\n")
cat("t        =", res10$statistic, "\n")
cat("df       =", res10$parameter, "\n")
cat("p-value  =", res10$p.value, "\n")
cat("CI lower =", res10$conf.int[1], "\n")
cat("CI upper =", res10$conf.int[2], "\n\n")

# ---------------------------------------------------------------------------
# 11. Spearman correlation
# ---------------------------------------------------------------------------
res11 <- cor.test(xp, yp, method = "spearman", exact = FALSE)
cat("--- Spearman correlation ---\n")
cat("rho      =", res11$estimate, "\n")
cat("p-value  =", res11$p.value, "\n\n")

# ---------------------------------------------------------------------------
# 12. Kendall tau
# ---------------------------------------------------------------------------
res12 <- cor.test(xp, yp, method = "kendall", exact = FALSE)
cat("--- Kendall tau ---\n")
cat("tau      =", res12$estimate, "\n")
cat("p-value  =", res12$p.value, "\n\n")

# ---------------------------------------------------------------------------
# 13. Shapiro-Wilk
# ---------------------------------------------------------------------------
set.seed(42)
xsw <- rnorm(20)
res13 <- shapiro.test(xsw)
cat("--- Shapiro-Wilk ---\n")
cat("W        =", res13$statistic, "\n")
cat("p-value  =", res13$p.value, "\n\n")

# ---------------------------------------------------------------------------
# 14. Descriptive statistics cross-check
# ---------------------------------------------------------------------------
xd <- c(2.1, 3.4, 2.8, 3.1, 2.9, 3.5, 3.0, 2.7, 3.3, 2.6)
cat("--- Descriptive statistics ---\n")
cat("mean     =", mean(xd), "\n")
cat("median   =", median(xd), "\n")
cat("sd       =", sd(xd), "\n")
cat("var      =", var(xd), "\n")
cat("skewness =", e1071::skewness(xd, type = 2), "\n")
cat("kurtosis =", e1071::kurtosis(xd, type = 2), "\n")
cat("IQR      =", IQR(xd), "\n")
cat("Q1       =", quantile(xd, 0.25), "\n")
cat("Q3       =", quantile(xd, 0.75), "\n\n")

cat("=== Validation complete ===\n")
cat("Compare these values to SCE output with tolerance 1e-6.\n")
