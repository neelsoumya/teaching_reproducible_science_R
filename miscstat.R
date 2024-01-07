# miscstat.R

requireNamespace("data.table")
requireNamespace("lmerTest")
requireNamespace("lsmeans")
requireNamespace("multcomp")

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

miscstat = new.env()

miscstat$MAX_EXPONENT = log(.Machine$double.xmax)
miscstat$VERY_SMALL_NUMBER = 1e-323 # .Machine$double.xmin is 2.2e-308, but this seems to manage.

#==============================================================================
# Working with machine limits
#==============================================================================

miscstat$convert_zero_to_very_small_number <- function(x) {
    # for logs: or log(0) will give -Inf and crash the L-BFGS-B optimzer
    ifelse(x == 0, miscstat$VERY_SMALL_NUMBER, x)
}

miscstat$reset_rng_seed <- function() {
    set.seed(0xbeef)
}

#==============================================================================
# Efficient calculation with extremely small numbers
#==============================================================================

miscstat$log_of_mean_of_numbers_in_log_domain <- function(log_v) {
    # http://stackoverflow.com/questions/7355145/mean-of-very-small-values
    max_log = max(log_v)
    logsum = max_log + log(sum(exp(log_v - max_log)))
    logmean = logsum - log(length(log_v))
    return(logmean)
}

#==============================================================================
# Summary statistics
#==============================================================================

miscstat$sem <- function(x) {
    # Calculate the standard error of the mean (SEM).
    # - SEM = SD / sqrt(n) = sqrt(variance / n).
    # - Won't do anything silly with NA values since var() will return NA in
    #   that case... but does fail with NULL == c()
    if (is.null(x)) return(NA)  # as for mean(NULL)
    sqrt(var(x)/length(x))
}

miscstat$half_confidence_interval_t <- function(x, ci = 0.95) {
    n = length(x)
    df = n - 1
    sem = miscstat$sem(x)
    crit_p = 1 - ((1 - ci) / 2) # e.g. 0.975 for ci == 0.95
    crit_t = qt(crit_p, df = df)
    return(crit_t * sem)
    # confidence interval is mean +/- that
}

miscstat$confidence_interval_t <- function(x, ci = 0.95) {
    hci = half_confidence_interval_t(x, ci)
    m = mean(x)
    return(c(m - hci, m + hci))
}

miscstat$summarize_by_factors <- function(data, depvarname, factornames,
                                          na.rm = FALSE) {
    ddply(
        data,
        factornames,
        function(drow) {
            values <- drow[, depvarname]
            if (na.rm) {
                values <- values[!is.na(values)]
            }
            c(
                n = length(values),
                mean = mean(values),
                min = min(values),
                max = max(values),
                sd = sd(values),
                var = var(values),
                sem = miscstat$sem(values)
            )
        }
    )
}

miscstat$summarize_by_factors_datatable <- function(dt, depvarname, factornames) {
    # http://stackoverflow.com/questions/12391950/variably-selecting-assigning-to-fields-in-a-data-table
    dt[
        ,
        .(
            n = length(get(depvarname)),
            mean = mean(get(depvarname)),
            min = min(get(depvarname)),
            max = max(get(depvarname)),
            sd = sd(get(depvarname)),
            var = var(get(depvarname)),
            sem = miscstat$sem(get(depvarname))
        ),
        by = factornames
    ]
    # NOTE: doesn't care whether the "by" things are really factors or not.
}

#==============================================================================
# Simple comparisons of data
#==============================================================================

miscstat$pretty_two_group_t_test <- function(values_a, values_b,
                                             familywise_n = 1,
                                             alpha_variance = 0.05) {
    values_a <- values_a[!is.na(values_a)]
    values_b <- values_b[!is.na(values_b)]
    mean_a <- mean(values_a)
    mean_b <- mean(values_b)
    sem_a <- miscstat$sem(values_a)
    sem_b <- miscstat$sem(values_b)
    n_a <- length(values_a)
    n_b <- length(values_b)

    r <- list(
        t = NA,
        df = NA,
        uncorrected_p = NA,
        corrected_p = NA,

        mean_a = mean_a,
        mean_b = mean_b,
        sem_a = sem_a,
        sem_b = sem_b,

        equal_variances = NA,

        pretty_mean_sem_a = ifelse(
            n_a > 0,
            paste(miscmath$format_sf(mean_a), "±",
                  miscmath$format_sf(sem_a)),
            ""),
        pretty_mean_sem_b = ifelse(
            n_b > 0,
            paste(miscmath$format_sf(mean_b), "±",
                  miscmath$format_sf(sem_b)),
            ""),
        pretty_p = "",
        familywise_n = familywise_n,
        n_a = n_a,
        n_b = n_b
    )
    if (n_a == 0 || n_b == 0 || is.null(values_a) || is.null(values_b)) {
        return(r)
    }
    v <- var.test(values_a, values_b)
    r$equal_variances <- v$p.value <= alpha_variance
    t <- t.test(values_a, values_b, var.equal = r$equal_variances)
    r$t <- t$statistic
    r$df <- t$parameter
    r$uncorrected_p <- t$p.value
    r$corrected_p <- miscstat$sidak_p(r$uncorrected_p, familywise_n)
    r$pretty_p = miscmath$describe_p_value_with_stars(r$corrected_p)
    return(r)
}

miscstat$pretty_two_group_chisq_contingency_test <- function(
        values_a, values_b, familywise_n = 1, factor_levels = NULL,
        count_sep = ":",  # can also use ":n="
        DEBUG = FALSE) {
    values_a <- values_a[!is.na(values_a)]
    values_b <- values_b[!is.na(values_b)]
    n_a <- length(values_a)
    n_b <- length(values_b)
    r <- list(
        chisq = NA,
        df = NA,
        uncorrected_p = NA,
        corrected_p = NA,

        pretty_counts_a = "",
        pretty_counts_b = "",
        pretty_p = "",

        familywise_n = familywise_n,
        n_a = n_a,
        n_b = n_b
    )
    value_vec <- c(as.character(values_a), as.character(values_b))
    # if you don't use as.character, then existing factors get munged into
    # integers: http://stackoverflow.com/questions/3443576/how-to-concatenate-factors
    if (is.null(factor_levels)) {
        final_levels <- unique(value_vec)
    } else {
        final_levels <- factor_levels
    }
    relevelled <- factor(value_vec, levels = final_levels)
    dat <- data.table(
        group = c(rep("a", length(values_a)), rep("b", length(values_b))),
        value = relevelled
    )
    datsum <- dat[, .(count = .N), by = .(group, value)]
    setkey(datsum, group, value)
    if (DEBUG) {
        print(values_a)
        print(values_b)
        print(factor_levels)
        print(final_levels)
        print(relevelled)
        print(dat)
        print(datsum)
    }
    r$pretty_counts_a <- paste(
        datsum[group == "a",
               .(pretty = paste(value, count, sep=count_sep))]$pretty,
        collapse=", "
    )
    r$pretty_counts_b <- paste(
        datsum[group == "b",
               .(pretty = paste(value, count, sep=count_sep))]$pretty,
        collapse=", "
    )
    if (n_a == 0 || n_b == 0) {
        return(r)
    }
    datgrid <- cbind(
        datsum[group == "a", .(count_a = count)],
        datsum[group == "b", .(count_b = count)]
    )
    chi <- chisq.test(datgrid)
    r$chisq = chi$statistic
    r$df = chi$parameter
    r$uncorrected_p <- chi$p.value
    r$corrected_p <- miscstat$sidak_p(r$uncorrected_p, familywise_n)
    r$pretty_p = miscmath$describe_p_value_with_stars(r$corrected_p)

    return(r)
}

miscstat$pretty_two_group_paired_regression <- function(
        DT, ycolname, xcolname, groupcolname, grouplevel_a, grouplevel_b,
        overall_positive_symbol = "/",  overall_negative_symbol = "\\",
        overall_ns_symbol = "·",
        slope_positive_symbol = "/ ",  slope_negative_symbol = "\\ ",
        slope_ns_symbol = "· ",
        a_positive_symbol = "/", a_negative_symbol = "\\", a_ns_symbol = "·",
        b_positive_symbol = "/", b_negative_symbol = "\\", b_ns_symbol = "·",
        slopediff_b_bigger_symbol = "↑", slopediff_b_smaller_symbol = "↓",
        slopediff_ns_symbol = "·",
        b_bigger_symbol = ">", b_smaller_symbol = "<",
        groupdiff_ns_symbol = "·",
        simple_test_prefix = "      ",
        familywise_n = 1, alpha = 0.05, alpha_variance = 0.05, sep = "",
        DEBUG = FALSE, show_p = FALSE
) {
    # Returns a string with:
    # - overall (simple) correlation, y ~ x
    # - main linear effect (X) in y ~ x + group + x:group
    # - simple correlation y ~ x within subset where group == grouplevel_a
    # - simple correlation y ~ x within subset where group == grouplevel_b
    # - x:group interaction in y ~ x + group + x:group
    # - group effect in y ~ x + group + x:group

    # Spare characters:
    # –¶§

    groupvar <- DT[[groupcolname]]
    passed_levels <- sort(c(grouplevel_a, grouplevel_b))
    dt_levels <- sort(levels(groupvar))
    if (!all(passed_levels == dt_levels)) {
        stop(paste("Levels from parameters (",
                   paste(passed_levels, collapse = ", "),
                   ") don't match levels from data table (",
                   paste(dt_levels, collapse = ", "),
                   ")", sep = ""))
    }

    newdt <- data.table(x = DT[[xcolname]],
                        y = DT[[ycolname]],
                        group = groupvar)

    if (xcolname == ycolname) {
        # Comparing something to itself. Just a t-test.
        values_a <- newdt[group == grouplevel_a, x]
        values_b <- newdt[group == grouplevel_b, x]
        t_result <- miscstat$pretty_two_group_t_test(
            values_a, values_b,
            familywise_n = familywise_n,
            alpha_variance = alpha_variance)
        result <- paste(
            simple_test_prefix,
            ifelse(
                t_result$corrected_p <= alpha,
                ifelse(t_result$mean_b > t_result$mean_a,
                       b_bigger_symbol,
                       b_smaller_symbol),
                groupdiff_ns_symbol
            ),
            sep = sep
        )
        if (DEBUG) print(t_result)
        return(result)
    }

    # Could do the "overall" test ignoring group:
    overall_test <- cor.test(~ x + y, data = newdt)
    overall_p <- miscstat$sidak_p(overall_test$p.value, familywise_n)
    overall_positive <- overall_p <= alpha && overall_test$estimate > 0
    overall_negative <- overall_p <= alpha && overall_test$estimate < 0

    a_test <- cor.test(~ x + y, data = subset(newdt, group == grouplevel_a))
    a_p <- miscstat$sidak_p(a_test$p.value, familywise_n)
    a_positive <- a_p <= alpha && a_test$estimate > 0
    a_negative <- a_p <= alpha && a_test$estimate < 0

    b_test <- cor.test(~ x + y, data = subset(newdt, group == grouplevel_b))
    b_p <- miscstat$sidak_p(b_test$p.value, familywise_n)
    b_positive <- b_p <= alpha && b_test$estimate > 0
    b_negative <- b_p <= alpha && b_test$estimate < 0

    fullmodel <- glm(y ~ x + group + x:group, data = newdt)
    fullmodel_summ <- summary(fullmodel)

    COEFF_NUM_SLOPE <- 2
    # ... the effect of the linear predictor, x
    slope_coeff <- fullmodel$coefficients[COEFF_NUM_SLOPE]
    raw_slope_p <- fullmodel_summ$coefficients[COEFF_NUM_SLOPE, "Pr(>|t|)"]
    slope_p <- miscstat$sidak_p(raw_slope_p, familywise_n)
    slope_positive <- slope_p <= alpha && slope_coeff > 0
    slope_negative <- slope_p <= alpha && slope_coeff < 0

    # Group differences: are these particularly important here?
    # The "self-to-self" comparison does a plain t-test; not sure if this
    # adds important and useful information (an effect of Group in predicting
    # Y, over and above X, whether or not X differs by Group...). Maybe it
    # does. But maybe not for a giant summary plot.
    COEFF_NUM_GROUPDIFF <- 3
    # ... the effect of the factor, as group_level_b
    groupdiff_coeff <- fullmodel$coefficients[COEFF_NUM_GROUPDIFF]
    raw_groupdiff_p <- fullmodel_summ$coefficients[COEFF_NUM_GROUPDIFF,
                                                   "Pr(>|t|)"]
    groupdiff_p <- miscstat$sidak_p(raw_groupdiff_p, familywise_n)
    groupdiff_b_bigger <- groupdiff_p <= alpha && groupdiff_coeff > 0
    groupdiff_b_smaller <- groupdiff_p <= alpha && groupdiff_coeff < 0

    COEFF_NUM_SLOPEDIFF <- 4
    # ... the additive effect of x:group_level_b
    slopediff_coeff <- fullmodel$coefficients[COEFF_NUM_SLOPEDIFF]
    raw_slopediff_p <- fullmodel_summ$coefficients[COEFF_NUM_SLOPEDIFF,
                                                   "Pr(>|t|)"]
    slopediff_p <- miscstat$sidak_p(raw_slopediff_p, familywise_n)
    slopediff_b_bigger <- slopediff_p <= alpha && slopediff_coeff > 0
    slopediff_b_smaller <- slopediff_p <= alpha && slopediff_coeff < 0

    result <- paste(
        ifelse(overall_positive, overall_positive_symbol,
               ifelse(overall_negative, overall_negative_symbol,
                      overall_ns_symbol)),
        ifelse(slope_positive, slope_positive_symbol,
               ifelse(slope_negative, slope_negative_symbol, slope_ns_symbol)),
        ifelse(a_positive, a_positive_symbol,
               ifelse(a_negative, a_negative_symbol, a_ns_symbol)),
        ifelse(b_positive, b_positive_symbol,
               ifelse(b_negative, b_negative_symbol, b_ns_symbol)),
        ifelse(slopediff_b_bigger, slopediff_b_bigger_symbol,
               ifelse(slopediff_b_smaller,
                      slopediff_b_smaller_symbol, slopediff_ns_symbol)),
        ifelse(groupdiff_b_bigger, b_bigger_symbol,
               ifelse(groupdiff_b_smaller, b_smaller_symbol,
                      groupdiff_ns_symbol)),
        ifelse(
            show_p,
            paste(
                " (",
                paste(
                    miscmath$describe_p_value(overall_p),
                    miscmath$describe_p_value(slope_p),
                    miscmath$describe_p_value(a_p),
                    miscmath$describe_p_value(b_p),
                    miscmath$describe_p_value(slopediff_p),
                    miscmath$describe_p_value(groupdiff_p),
                    sep = ", "
                ),
                ")",
                sep = ""
            ),
            ""
        ),
        sep = sep
    )
    if (DEBUG) {
        print(newdt)
        cat("--- overall_test:\n")
        print(overall_test)
        cat("--- a_test:\n")
        print(a_test)
        cat("--- b_test:\n")
        print(b_test)
        cat("--- slopetest:\n")
        print(slopetest)
        print(slopetest_summ)
    }
    return(result)
}

miscstat$IGNORE_ME = '
DT <- data.table(
    x = c(1, 2, 3, 10, 11, 12),
    y = c(4, 5, 6, 20, 19, 18),
    g = factor(c(1, 1, 1, 2, 2, 2))
)

miscstat$pretty_two_group_paired_regression(DT, "y", "x", "g", 1, 2)
'

miscstat$two_group_multiple_regression_table <- function(
    DT, groupcolname, varcolnames,
    grouplevel_a, grouplevel_b,
    blank = "", upper = FALSE, DEBUG = FALSE,
    correct_multiple_comparisons = TRUE,  # only use FALSE for debugging!
    transpose = FALSE
) {
    # If correct_multiple_comparisons is TRUE (the default),
    # group-difference comparisons are treated as a family (ttest_k),
    # and all other comparisons are treated as a family (pairwise_k).
    # For example, for 59 groups, group differences are corrected for 59
    # comparisons, and other comparisons for 59 * 58 / 2 = 1711 comparisons.
    # For things other than "self-to-self = group test" comparisons,
    # Within each cell there are 6 tests (pretty_two_group_paired_regression)
    # but these are not additionally corrected for.
    n <- length(varcolnames)
    if (!correct_multiple_comparisons) {
        warning("Not correcting for multiple comparisons!")
    }
    ttest_k <- ifelse(correct_multiple_comparisons, n, 1)
    pairwise_k <- ifelse(correct_multiple_comparisons, n * (n - 1) / 2, 1)
    cat("miscstat$two_group_multiple_regression_table: ttest_k = ", ttest_k,
        ", pairwise_k = ", pairwise_k, "\n", sep = "")
    m <- matrix(blank, ncol = n, nrow = n)
    for (rownum in 1:n) {
        if (upper) {
            cstart <- rownum
            cend <- n
        } else {
            cstart <- 1
            cend <- rownum
        }
        if (DEBUG) cat(paste("cstart", cstart, "cend", cend, "\n"))
        for (colnum in cstart:cend) {
            xcolname <- varcolnames[rownum]
            ycolname <- varcolnames[colnum]
            familywise_n <- ifelse(rownum == colnum, ttest_k, pairwise_k)
            txt <- miscstat$pretty_two_group_paired_regression(
                DT, ycolname, xcolname,
                groupcolname, grouplevel_a, grouplevel_b,
                familywise_n = familywise_n
            )
            if (DEBUG) {
                cat(paste("row", rownum, "column", colnum, "text", txt, "\n"))
            }
            m[rownum, colnum] <- txt
        }
    }
    if (transpose) {
        m <- t(m)
    }
    if (DEBUG) print(m)
    DF <- data.frame(m)
    rownames(DF) <- varcolnames
    colnames(DF) <- varcolnames
    return(DF)
}

#==============================================================================
# Sanity checks (e.g. for refereeing), such as t-tests based on mean/SD without
# access to raw data.
#==============================================================================

miscstat$t_test_unpaired_eq_var <- function(mean1, mean2, sd1, sd2, n1, n2) {
    df <- n1 + n2 - 2
    var1 <- sd1 ^ 2
    var2 <- sd2 ^ 2
    pooled_var <- ((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2)
    t <- (mean1 - mean2) / sqrt((pooled_var / n1) + (pooled_var / n2))
    p <- 2 * pt(-abs(t), df)  # http://www.cyclismo.org/tutorial/R/pValues.html

    cat("Group 1: mean = ", mean1, ", sd = ", sd1, ", var = ", var1,
        ", n = ", n1, "\n", sep = "")
    cat("Group 2: mean = ", mean2, ", sd = ", sd2, ", var = ", var2,
        ", n = ", n2, "\n", sep = "")
    cat("Mean difference (mean2 - mean1) = ", mean2 - mean1, "\n", sep = "")
    cat("t =", t, "\n")
    cat("df =", df, "\n")
    cat("p =", p, "\n")
}


#==============================================================================
# p-values
#==============================================================================

miscstat$sidak_alpha <- function(familywise_alpha, n_comparisons) {
    # returns corrected alpha, which gets lower with n_comparisons
    1 - (1 - familywise_alpha) ^ (1 / n_comparisons)
}

miscstat$sidak_p <- function(uncorrected_p, n_comparisons) {
    # returns corrected p, which gets higher with n_comparisons
    # ... the problem here is equivalent to taking a corrected alpha
    #     and returning an uncorrected alpha, i.e. reversing the alpha
    #     calculation
    # ... that is, sidak_p(sidak_alpha(x, n), n) == x
    1 - ((1 - uncorrected_p) ^ n_comparisons)
}

miscstat$sidak_familywise_alpha <- function(alpha_per_test, n_comparisons) {
    1 - (1 - alpha_per_test) ^ (n_comparisons)
}

miscstat$sidak_corrected_p <- function(uncorrected_p, n_comparisons) {
    # returns corrected p value
    # http://v8doc.sas.com/sashtml/stat/chap43/sect14.htm
    1 - (1 - uncorrected_p) ^ (n_comparisons)
}

#==============================================================================
# Goodness of fit
#==============================================================================

miscstat$aic <- function(nLL, k) {
    # Akaike Information Criterion
    2 * k + 2 * nLL
    # = 2k - 2ln(L)
}

miscstat$nll_from_aic <- function(aic, k) {
    (aic - 2 * k ) / 2
}

# Strong argument to prefer AICc over AIC:
# http://en.wikipedia.org/wiki/Akaike_information_criterion
# http://www.sortie-nd.org/lme/Statistical%20Papers/Burnham_and_Anderson_2004_Multimodel_Inference.pdf

# k = num_parameters:

miscstat$aicc <- function(nLL, k, n) {
    # Akaike Information Criterion, corrected
    # Parameter meanings as for BIC; see below.

    aic(nLL, k) + 2 * k * (k + 1) / (n - k - 1)
}

miscstat$bic <- function(nLL, k, n) {
    # Bayesian Information Criterion
    # nLL = negative log-likelihood (where log likelihood = sum of ln(p) values)
    # ... since p(...) <= 1, ln(p) <= 0
    #     ... so the LL is negative and the nLL is positive
    # ... a good fit is where the p values are close to 1
    #     ... so the LL are negative but close to 0
    #     ... so the nLL values are positive but close to 0
    # ... a bigger nLL is bad
    # ... a bigger BIC is bad; smaller BIC means better fit (penalized for number of parameters)
    # k = number of parameters in the model
    # n = number of observations

    2 * nLL + k * log(n)
    # ... = -2 ln(L) + k ln(n)
}

miscstat$lr_test <- function(model1_nLL, model1_df, model2_nLL, model2_df) {
    # Ensure df2 > df1
    if (model2_df > model1_df) {
        df1 = model1_df
        df2 = model2_df
        nLL1 = model1_nLL
        nLL2 = model2_nLL
    }
    else {
        df1 = model2_df
        df2 = model1_df
        nLL1 = model2_nLL
        nLL2 = model1_nLL
    }
    # Don't work with exp(-nLL) -- numbers too small, get rounded to zero (see e.g. "exp(-3000)")
    D = 2 * nLL1 - 2 * nLL2
    # D = -2 ln(likelihood for null model) + 2 ln(likelihood for alternative model)
    df = df2 - df1
    p = 1 - pchisq(D, df)
    cat("df1 = ", df1, "\n")
    cat("df2 = ", df2, "\n")
    cat("D (= chi-square) = ", D, "\n")
    cat("df = ", df, "\n")
    cat("p = ", p, "\n")
    return(p)
}

#==============================================================================
# Distributions
#==============================================================================

# AVOID, use PDF instead for MAP
miscstat$p_data_or_more_extreme_from_normal <- function(x, means, sds) {
    ifelse(
        x > means,
        2 * (1 - pnorm(x, means, sds, lower.tail=TRUE) ),
        2 * (1 - pnorm(x, means, sds, lower.tail=FALSE) )
    )
}


#==============================================================================
# softmax function
#==============================================================================

miscstat$softmax <- function(x, b = 1, debug = TRUE) {
    # x: vector of values
    # b: exploration parameter, or inverse temperature [Daw2009], or 1/t where:
    # t: temperature (towards infinity: all actions equally likely; towards zero: probability of action with highest value tends to 1)
    # DO NOT USE TEMPERATURE DIRECTLY: the optimizer may take it to zero, giving an infinity.
    # vector may have NA values in
    # return value: vector of probabilities
    constant = mean(x, na.rm=TRUE)
    products = x * b - constant
    # ... softmax is invariant to addition of a constant: Daw article and http://www.faqs.org/faqs/ai-faq/neural-nets/part2/section-12.html#b
    if (max(products, na.rm=TRUE) > MAX_EXPONENT) {
        if (debug) cat("OVERFLOW in softmax(): x =", x, ", b =", b, ", constant =", constant, ", products=", products, "\n")
        answer = rep(0, length(x))
        answer[which.max(x)] = 1
        answer[is.na(x)] = NA
    }
    else {
        exponented = exp(products)
        answer = exponented / sum(exponented, na.rm=TRUE)
    }
    return(answer)
}

#==============================================================================
# proportion
#==============================================================================

miscstat$proportion_x_from_a_to_b <- function(x, a, b) {
    (1 - x) * a + x * b
}

#==============================================================================
# randomness
#==============================================================================

miscstat$coin <- function(p) {
    n <- length(p)
    return(p > runif(n))
}

miscstat$roulette <- function(p) {
    # p is a vector of probabilities that sum to 1
    # return value: vector of truth values: one TRUE, the rest FALSE, selected according to the probabilities
    n_options = length(p)
    cum_p = cumsum(p)
    r = runif(1) # random variable
    choice = rep(FALSE, n_options)
    choice[cum_p == min(cum_p[cum_p > r])] = TRUE
    return(choice)
}

#==============================================================================
# ANOVA/linear modelling
#==============================================================================

#------------------------------------------------------------------------------
# Diagnostic plots
#------------------------------------------------------------------------------

miscstat$rvfPlot <- function(model, FONTSIZE=10) {
    # https://rpubs.com/therimalaya/43190
    # Note that the other diagnostic plots shown there fail with lme models.
    return (
        ggplot(model, aes(.fitted, .resid))
        + geom_point()
        + stat_smooth(method="loess")
        + geom_hline(yintercept=0, col="red", linetype="dashed")
        + xlab("Fitted values")
        + ylab("Residuals")
        + ggtitle("Residual vs Fitted Plot")
        + theme_classic()
        + theme(
            text=element_text(size=FONTSIZE),
            plot.title=element_text(hjust=0, face="bold")  # left title
        )
    )
}

#------------------------------------------------------------------------------
# Post-hoc analysis; SEDs
#------------------------------------------------------------------------------

miscstat$pairwise_contrasts <- function(
        term, model, alternative=c("two.sided", "less", "greater"),
        DEBUG=FALSE) {
    alternative <- match.arg(alternative)
    # We'd normally do:
    #
    #   multcomp::glht(model, linfct = multcomp::mcp(area = "Tukey"))
    #
    # where "area" is a factor in the model.
    # But this can't do interactions, I don't think. An alternative is:
    #
    #   multcomp::glht(model, linfct = lsmeans::lsm(pairwise ~ area)
    #   multcomp::glht(model, linfct = lsmeans::lsm(pairwise ~ area:treatment)
    #
    # Some refs:
    #
    #   https://mailman.ucsd.edu/pipermail/ling-r-lang-l/2012-November/000393.html
    #   http://mindingthebrain.blogspot.co.uk/2013/04/multiple-pairwise-comparisons-for.html
    #   http://stats.stackexchange.com/questions/43664/mixed-model-multiple-comparisons-for-interaction-between-continuous-and-categori
    #   http://stats.stackexchange.com/questions/120604/which-post-hoc-is-more-valid-for-multiple-comparison-of-an-unbalanced-lmer-model
    #
    # However, we want the "area" or "area:treatment" thing to come in as a
    # variable. This is all a bit ugly...
    #
    #   http://adv-r.had.co.nz/Computing-on-the-language.html
    #   http://stackoverflow.com/questions/5542945/opposite-of-rs-deparsesubstitutevar
    #
    # Anyway, the answer in R is to eval it.

    expr <- paste(
        "multcomp::glht(model, linfct = lsmeans::lsm(pairwise ~ ", term, "), ",
        "alternative=\"", alternative, "\")",
        sep="")
    g <- eval(parse(text=expr))
    summ <- summary(g)
    test <- summ$test
    # test includes:
    #   coefficients = "Estimate"
    #   sigma = "Std. Error" (of the difference) = SED
    #   tstat = "t value"
    #   pvalues = "Pr(>|t|)"
    # ... for each of which, names() gives the tests
    d <- data.frame(
        term = term,
        comparison = names(test$coefficients),
        estimate = test$coefficients,
        sed = test$sigma,
        t = test$tstat,
        p = test$pvalues,
        alternative = alternative
    )
    return(d)
}

miscstat$get_n_for_factor <- function(term, model) {
    factors <- strsplit(term, ":", fixed=TRUE)[[1]]
    d <- model@frame
    n_list <- count(d, factors)$freq
    harmonic_mean_n <- miscmath$harmonic_mean(n_list)
    return(data.frame(
        term=term,
        n_list=paste(n_list, collapse=","),
        harmonic_mean_n=harmonic_mean_n
    ))
}

miscstat$are_predictors_factors <- function(model, predictors) {
    if (length(predictors) < 1) {
    } else if (length(predictors) == 1) {
        # if you use the sapply method with just one, it operate on every
        # row...
        is.factor(model@frame[, predictors])
    } else {
        sapply(model@frame[, predictors], function(col) is.factor(col))
    }
}

miscstat$predictor_names_from_term <- function(term) {
    strsplit(term, ":")[[1]]
}

miscstat$are_all_predictors_in_term_factors <- function(model, term) {
    predictors <- miscstat$predictor_names_from_term(term)
    all(miscstat$are_predictors_factors(model, predictors))
}

miscstat$do_terms_contain_only_factors <- function(model, terms) {
    sapply(
        terms,
        miscstat$are_all_predictors_in_term_factors,
        model=model
    )
}

miscstat$sed_info <- function(
        model, term=NULL,
        alternative=c("two.sided", "less", "greater"), DEBUG=FALSE) {
    # model: an lmer/lmerTest model.
    # term: e.g. "area:manipulation:csvalence"
    alternative <- match.arg(alternative)

    summ <- summary(model)
    an <- anova(model)
    # ... the underlying representation (see "class(an)") is a data frame
    all_terms <- rownames(an)

    # Eliminate things with covariates (continuous predictors) in:
    if (DEBUG) {
        cat("ALL TERMS:", all_terms, "\n")
    }
    useful_terms <- all_terms[
        which(miscstat$do_terms_contain_only_factors(model, all_terms))]
    if (DEBUG) {
        cat("FACTOR-ONLY TERMS:", useful_terms, "\n")
    }

    # Find the highest-order interaction
    n_colons <- lapply(useful_terms, misclang$n_char_occurrences, char=":")
    highest_order_interaction <- useful_terms[which.max(n_colons)]

    # Pairwise contrasts for factors
    comparisons <- ldply(
        useful_terms,
        miscstat$pairwise_contrasts,
        model,
        alternative=alternative
    )

    # std_error_fixed_effects_estimates <- sqrt(diag(vcov(model)))
    # names(std_error_fixed_effects_estimates) <- vcov(model)@Dimnames[[1]]
    #
    # ... Douglas Bates, https://stat.ethz.ch/pipermail/r-help/2006-July/109308.html
    # http://lme4.r-forge.r-project.org/slides/2009-07-16-Munich/Precision-4.pdf
    # NOTE, for example, that the "standard error of effect X" where X is
    # something like "treatment - control" is, obviously, an SED.

    n_by_factor <- ldply(
        useful_terms,
        miscstat$get_n_for_factor,
        model
    )
    an <- cbind(an[useful_terms, ], n_by_factor)
    an <- rename(an, c("Mean Sq"="ms_effect",
                       "F.value"="F",
                       "Pr(>F)"="p"))
    an <- within(an, {
        ms_error <- ms_effect / F  # since F = ms_effect / ms_error
        iffy_sed <- sqrt(2 * ms_error / harmonic_mean_n)
        # t_eq_sqrt_F_for_2_grps <- sqrt(F)
    })
    highest_interaction_iffy_sed = an[highest_order_interaction,
                                        "iffy_sed"]

    # *** Not sure that iffy_sed is always right. OK in simple situations, but
    # not so sure in complex ones...

    return(list(
        pairwise_contrasts = comparisons,
        highest_order_interaction = highest_order_interaction,
        coefficients = summ$coefficients,
        anova = an,
        notes = c(
            "There is no *one* SED appropriate for all comparisons! See e.g. Cardinal & Aitken 2006 p98.",
            "Pairwise contrasts use multcomp::glht(model, linfct = lsmeans::lsm(pairwise ~ FACTOR)).",
            "Least-squares means estimates (with SE of estimate) are from lmerTest::lsmeans().",
            "highest_interaction_iffy_sed is the iffy_sed for the highest_order_interaction term. But see note 1 above."
        ),
        highest_interaction_iffy_sed = highest_interaction_iffy_sed,
        lsmeans = lmerTest::lsmeans(model)
    ))
}

#------------------------------------------------------------------------------
# Effect size
#------------------------------------------------------------------------------

# http://stats.stackexchange.com/questions/95054/how-to-get-the-overall-effect-for-linear-mixed-model-in-lme4-in-r
# http://www.leeds.ac.uk/educol/documents/00002182.htm
# http://stackoverflow.com/questions/25084924/extracting-standard-deviation-of-random-effects-components-using-glmer
# https://www.researchgate.net/post/How_can_I_calculate_an_effect_size_cohens_d_preferably_from_a_linear_random_effects_model_beta
#
# THIS ONE?
# http://davidileitman.com/wp-content/uploads/2014/04/EffectSizeFormulas.pdf
# = http://www.soph.uab.edu/Statgenetics/People/MBeasley/Courses/EffectSize-Power.pdf
#
# http://www.bwgriffin.com/gsu/courses/edur9131/content/Effect_Sizes_pdf5.pdf
#
# Quick web calculator for T test:
# https://www.easycalculation.com/statistics/effect-size-t-test.php
#
# https://cran.r-project.org/web/packages/compute.es/compute.es.pdf
# http://stats.stackexchange.com/questions/41861/calculating-eta-squared-from-f-and-df
#
# RULES OF THUMB
# http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize
# ... d: 0.2 small, 0.5 medium, 0.8 large
# ... eta-squared: 0.01 small, 0.06 medium, 0.14 large


miscstat$lmer_effect_size_r_squared <- function(lmer_model) {
    m <- lmer_model
    lmfit <- lm(model.response(model.frame(m)) ~ fitted(m))
    summary(lmfit)$r.squared
}


miscstat$sigstars <- function(p, default = "") {
    ifelse(p < 0.001, "***",
           ifelse(p < 0.01, "**",
                  ifelse(p < 0.05, "*", default)))
}

#lmer_effect_size_1_poor <- function(lmer_model) {
#    coeffs <- coef(summary(lmer_model))
#    colnames(coeffs) <- c(
#        "estimate",  # was Estimate
#        "se",  # was "Std. Error"
#        "df",  # as before
#        "t",  # was "t value"
#        "p"  # was "Pr(>|t|)"
#    )
#    dt <- data.table(coeffs)
#    dt$term <- rownames(coeffs)
#    dt[, sig := sigstars(p)]
#    # dt[, sd_error := sigma(lmer_model)]
#    # dt[, abs_estimate_over_sd_error := abs(estimate / sd_error)]
#    dt[, wrong_approx_cohen_d_abs := abs(2 * t) / sqrt(df)]
#    # ... http://davidileitman.com/wp-content/uploads/2014/04/EffectSizeFormulas.pdf
#    # ... wrong but close?
#    return(dt)
#}

miscstat$sum_of_squares <- function(x) {
    # The sum of squared deviations from the mean
    mu <- mean(x)
    sum((x - mu)^2)

    # REMEMBER ALSO: var(x) = sum_of_squares(x) / (n - 1) [check: sample variance?]
}


miscstat$ss_total_for_lmer_model <- function(lmer_model) {
    if (class(lmer_model) != "merModLmerTest") {
        stop("Model is not of class merModLmerTest")
    }
    depvar <- lmer_model@frame[, 1]  # assumes depvar is always first column; think it is!
    miscstat$sum_of_squares(depvar)
}


miscstat$ss_total_for_lm_model <- function(lm_model) {
    if (class(lm_model) != "lm") {
        stop("Model is not of class lm")
    }
    depvar <- lm_model$model[, 1]  # assumes depvar is always first column; think it is!
    miscstat$sum_of_squares(depvar)
}


miscstat$cohen_size_eta_sq <- function(eta_sq, default = "-") {
    # Nonsense, but helpful nonsense
    # http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize
    # For ANOVA:
    ifelse(eta_sq >= 0.14, "large",
           ifelse(eta_sq >= 0.06, "medium",
                  ifelse(eta_sq >= 0.01, "small", default)))
}

miscstat$lmer_effect_size_eta_sq <- function(lmer_model) {
    if (class(lmer_model) != "merModLmerTest") {
        stop("Model is not of class merModLmerTest")
    }
    a <- anova(lmer_model)
    dt <- data.table(a)
    colnames(dt) <- c(
        "SS_effect",  # was "Sum Sq"
        "MS_effect",  # was "Mean Sq"
        "df_num",  # was "NumDF"
        "df_den",  # was "DenDF"
        "F",  # was "F.value"
        "p"  # was "Pr(>F)"
    )
    dt$term = rownames(a)
    dt[, sig := miscstat$sigstars(p)]
    # dt[, wrong_approx_cohen_d := 2 * sqrt(df_num * F / df_den)]
    # ... http://davidileitman.com/wp-content/uploads/2014/04/EffectSizeFormulas.pdf
    # ... wrong but close?

    # F = MS_effect / MS_error
    # so:
    # UNNECESSARY # dt[, MS_error := MS_effect / F]

    # MS_error = SS_error / df_error
    # so:
    # UNNECESSARY # dt[, SS_error := MS_error * df_den]

    # eta_squared = SS_effect / SS_total

    total_sum_of_squares <- miscstat$ss_total_for_lmer_model(lmer_model)
    dt[, SS_total := total_sum_of_squares]

    dt[, eta_sq := SS_effect / SS_total]
    dt[, interp_eta_sq := miscstat$cohen_size_eta_sq(eta_sq)][]

    return(dt)
}


miscstat$lm_effect_size_eta_sq <- function(lm_model) {
    if (class(lm_model) != "lm") {
        stop("Model is not of class lm")
    }
    a <- anova(lm_model)
    dt <- data.table(a)
    colnames(dt) <- c(
        "df_num",  # was "Df"
        "SS_effect",  # was "Sum Sq"
        "MS_effect",  # was "Mean Sq"
        "F",  # was "F.value"
        "p"  # was "Pr(>F)"
    )
    dt$term = rownames(a)
    dt[, sig := miscstat$sigstars(p)]
    total_sum_of_squares <- miscstat$ss_total_for_lm_model(lm_model)
    dt[, SS_total := total_sum_of_squares]
    dt[, eta_sq := SS_effect / SS_total]
    dt[, interp_eta_sq := miscstat$cohen_size_eta_sq(eta_sq)][]
    return(dt)
}


miscstat$pooled_variance_two_groups <- function(data1, data2) {
    # http://trendingsideways.com/index.php/cohens-d-formula/
    # https://en.wikipedia.org/wiki/Pooled_variance
    n1 <- length(data1)
    n2 <- length(data2)
    var1 <- var(data1)
    var2 <- var(data2)
    pooled_var <- ((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2)
}


miscstat$pooled_sd_two_groups <- function(data1, data2) {
    sqrt(miscstat$pooled_variance_two_groups(data1, data2))
}


miscstat$cohen_d_by_factor <- function(data_table, depvar, factor_name) {
    factor_col_num <- grep(factor_name, colnames(data_table))
    factor_values <- data_table[[factor_col_num]]  # as a vector
    if (nlevels(factor_values) != 2) {
        stop("Factor must have two levels")
    }
    level_1_value <- levels(factor_values)[1]
    level_2_value <- levels(factor_values)[2]

    depvar_col_num <- grep(depvar, colnames(data_table))
    depvar_values <- data_table[[depvar_col_num]]
    group_1_values <- depvar_values[factor_values == level_1_value]
    group_2_values <- depvar_values[factor_values == level_2_value]
    mean_1 <- mean(group_1_values)
    mean_2 <- mean(group_2_values)
    mean_diff_gp2_minus_gp1 <- mean_2 - mean_1
    sd_1 <- sd(group_1_values)
    sd_2 <- sd(group_2_values)
    pooled_sd <- miscstat$pooled_sd_two_groups(group_1_values, group_2_values)
    cohen_d <- mean_diff_gp2_minus_gp1 / pooled_sd
    return(list(
        data_table = data_table,
        depvar = depvar,
        factor_name = factor_name,
        factor_values = factor_values,
        level_1_value = level_1_value,
        level_2_value = level_2_value,
        group_1_values = group_1_values,
        group_2_values = group_2_values,
        mean_1 = mean_1,
        mean_2 = mean_2,
        mean_diff_gp2_minus_gp1 = mean_diff_gp2_minus_gp1,
        sd_1 = sd_1,
        sd_2 = sd_2,
        pooled_sd = pooled_sd,
        cohen_d = cohen_d
    ))
}


miscstat$is_lme4_model_singular <- function(model) {
    # A singular model may give WRONG RESULTS by giving zero variance for
    # random effects.

    # http://stats.stackexchange.com/questions/115090/using-glmer-why-is-my-random-effect-zero
    # https://rawgit.com/bbolker/mixedmodels-misc/master/glmmFAQ.html#singular-models-random-effect-variances-estimated-as-zero-or-correlations-estimated-as---1
    # https://arxiv.org/abs/1406.5823
    theta <- getME(model, "theta")
    # diagonal elements are identifiable because they are fitted with a lower bound of zero...
    diag.element <- getME(model, "lower") == 0
    any(theta[diag.element] < 1e-5)
}


miscstat$IGNOREME_MISCSTAT_EXAMPLE <- "

# =============================================================================
# WORKING/THINKING
# =============================================================================

testdata <- expand.grid(
    A=c(1, 2, 3),
    B=c(10, 11, 12),
    subj=seq(1,50)
)
testdata <- within(testdata, {
    y <- 13 + 0.5*A + 6*B + rnorm(sd=0.5, n=nrow(testdata))
    # will give intercept = 13 + 0.5*1 + 6*10 = 73.5 (at A1, B1)
    #   A2 effect = 0.5 (relative to A1)
    #   A3 effect = 1   (relative to A1)
    #   B2 effect = 6   (relative to B1)
    #   B3 effect = 12  (relative to B1)
    #   interaction terms = 0
    A <- as.factor(A)
    B <- as.factor(B)
    subj <- as.factor(subj)
})
testmodel <- lmer(y ~ A*B + (1 | subj), data=testdata)
print(sed_info(testmodel))

"

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("miscstat" %in% search()) detach("miscstat")
attach(miscstat)  # subsequent additions not found, so attach at the end
