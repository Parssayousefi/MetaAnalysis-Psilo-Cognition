#' Calculate I-squared values and variance distribution for three-level meta-analysis models
#'
#' This function calculates variance proportions and I-squared values for three-level meta-analysis
#' models fitted using the `rma.mv` function from the `metafor` package. It provides a breakdown of
#' variance attributable to different levels of the model and visualizes these statistics.
#'
#' @param x An `rma.mv` object representing a three-level meta-analysis model with two random effects.
#'
#' @return A list containing:
#'   - `ResultsTable`: A data frame with variance percentages and I-squared values for each level.
#'   - `TotalI2`: Total I-squared value combining levels 2 and 3.
#'   - `Plot`: A ggplot object showing the distribution of variances.
#'
#' @details
#' The function checks if the input model is a three-level `rma.mv` model with two variance components
#' and nested random effects. It calculates variance proportions using the variance components of the model
#' and the residual variance. The I-squared values are computed to show the percentage of total variance
#' due to heterogeneity at each level.
#'
#' @examples
#' \dontrun{
#' library(metafor)
#' data(dat.konstantopoulos2011)
#' model <- rma.mv(yi, vi, random = ~ 1 | district/school, data = dat.konstantopoulos2011)
#'
#' }
#'
#' @references
#' Harrer, M., Cuijpers, P., Furukawa, T.A., and Ebert, D.D. (2019). Doing Meta-Analysis in R: A Hands-on Guide.
#' Cheung, M.W.L. (2014). Modeling dependent effect sizes with three-level meta-analyses: a structural
#' equation modeling approach. Psychological Methods, 19(2), 211.
#'
#' @export


mlm.variance.distribution = function(x) {
    if (!inherits(x, "rma.mv")) {
        stop("Input must be an object of class 'rma.mv'.")
    }

    if (length(x$sigma2) != 2) {
        stop("Input model must be a three-level model with exactly two random effects components.")
    }

    if (!any(grepl("/", deparse(x$call$random)))) {
        stop("Model must include nested random effects specified as '~ 1 | cluster/effect-within-cluster'.")
    }

    # Calculating the residuals
    residuals = x$yi - fitted(x)

    # Variance components calculations
    total_var = sum(x$sigma2) + var(residuals)
    level1_var_perc = (var(residuals) / total_var) * 100
    level2_var_perc = (x$sigma2[2] / total_var) * 100
    level3_var_perc = (x$sigma2[1] / total_var) * 100
    total_i2 = 100 - level1_var_perc

    # Results data frame
    results_df = data.frame(
        Level = c("Level 1 (Sampling Error)", "Level 2", "Level 3"),
        Variance_Percentage = c(level1_var_perc, level2_var_perc, level3_var_perc),
        I2 = c(NA, level2_var_perc, level3_var_perc)
    )

    # Plotting
    plot = ggplot(results_df, aes(x = Level, y = Variance_Percentage, fill = Level)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(round(Variance_Percentage, 2), "%")), vjust = -0.5) +
        theme_minimal() +
        labs(title = "Variance Distribution in Multi-Level Meta-Analysis", y = "Percentage of Total Variance", x = NULL) +
        scale_fill_brewer(palette = "Pastel1")

    # Return structured output
    return(invisible(list(
        ResultsTable = results_df,
        TotalI2 = total_i2,
        Plot = plot
    )))
}
