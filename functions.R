#' Plot decomposition of time series data
#'
#' This function generates a plot of the decomposition of time series data into trend,
#' seasonal, and irregular components. Supporting either classical or STL decomposition.
#'
#' @param obj A decompose table object from the feasts library
#' @param var The series variable name (e.g., "trend", "seasonal", etc.)
#' @param outliers Logical; should outliers be shown in the plot? Default is FALSE
#' @return A plotly subplot of the decomposition components


plot_decomposition <- function(obj, var, outliers = FALSE) {
    d <- obj
    obj_attr <- attributes(obj)
    intervals <- unlist(obj_attr$interval)
    interval <- names(which(intervals == 1))
    index <- as.character(obj_attr$index)
    if (interval %in% c("week", "month", "quarter")) {
        d$date <- as.Date(d[[index]])
    } else {
        d$date <- d[[index]]
    }

    color <- "#0072B5"



    if (outliers) {
        if (obj_attr$method == "STL") {
            sdv <- sd(d$remainder)

            d$sd3 <- ifelse(d$remainder >= 3 * sdv | d$remainder <= -3 * sdv, d[[var]], NA)
            d$sd2 <- ifelse(d$remainder >= 2 * sdv & d$remainder < 3 * sdv | d$remainder <= -2 * sdv & d$remainder > -3 * sdv, d[[var]], NA)
        } else {
            sdv <- sd(d$random, na.rm = TRUE)

            d$sd3 <- ifelse(d$random >= 3 * sdv | d$random <= -3 * sdv, d[[var]], NA)
            d$ sd2 <- ifelse(d$random >= 2 * sdv & d$random < 3 * sdv | d$random <= -2 * sdv & d$random > -3 * sdv, d[[var]], NA)
        }
    }


    series <- d |>
        plotly::plot_ly(x = ~date, y = ~ get(var), type = "scatter", mode = "lines", line = list(color = color), name = "Actual", showlegend = FALSE) |>
        plotly::layout(yaxis = list(title = "Actial"))


    trend <- d |>
        plotly::plot_ly(x = ~date, y = ~trend, type = "scatter", mode = "lines", line = list(color = color), name = "Trend", showlegend = FALSE) |>
        plotly::layout(yaxis = list(title = "Trend"))


    if (obj_attr$method == "STL") {
        if (interval != "year") {
            seasonal <- d |>
                plotly::plot_ly(x = ~date, y = ~season_year, type = "scatter", mode = "lines", line = list(color = color), name = "Seasonal", showlegend = FALSE) |>
                plotly::layout(yaxis = list(title = "Seasonal"))
        } else {
            seasonal <- NULL
        }

        seasonal_adj <- d |>
            plotly::plot_ly(x = ~date, y = ~season_adjust, type = "scatter", mode = "lines", line = list(color = color), name = "Seasonal Adjusted", showlegend = FALSE) |>
            plotly::layout(yaxis = list(title = "Seasonal Adjusted"))

        irregular <- d |> plotly::plot_ly(
            x = ~date, y = ~remainder,
            type = "scatter", mode = "lines",
            line = list(color = color), name = "Irregular", showlegend = FALSE
        )
    } else {
        if (interval != "year") {
            seasonal <- d |>
                plotly::plot_ly(x = ~date, y = ~seasonal, type = "scatter", mode = "lines", line = list(color = color), name = "Seasonal", showlegend = FALSE) |>
                plotly::layout(yaxis = list(title = "Seasonal"))
        } else {
            seasonal <- NULL
        }

        seasonal_adj <- d |>
            plotly::plot_ly(x = ~date, y = ~season_adjust, type = "scatter", mode = "lines", line = list(color = color), name = "Seasonal Adjusted", showlegend = FALSE) |>
            plotly::layout(yaxis = list(title = "Seasonal Adjusted"))

        irregular <- d |>
            plotly::plot_ly(
                x = ~date, y = ~random,
                type = "scatter", mode = "lines",
                line = list(color = color), name = "Irregular", showlegend = FALSE
            ) |>
            plotly::layout(yaxis = list(title = "Irregular"))
    }


    if (outliers) {
        series <- series |>
            plotly::add_trace(x = ~date, y = ~sd2, marker = list(color = "orange")) |>
            plotly::add_trace(x = ~date, y = ~sd3, marker = list(color = "red"))
        irregular <- irregular |>
            plotly::add_segments(
                x = min(d$date),
                xend = max(d$date),
                y = 2 * sdv,
                yend = 2 * sdv,
                name = "2SD",
                line = list(color = "orange", dash = "dash")
            ) |>
            plotly::add_segments(
                x = min(d$date),
                xend = max(d$date),
                y = -2 * sdv,
                yend = -2 * sdv,
                name = "-2SD",
                line = list(color = "orange", dash = "dash")
            ) |>
            plotly::add_segments(
                x = min(d$date),
                xend = max(d$date),
                y = 3 * sdv,
                yend = 3 * sdv,
                name = "3SD",
                line = list(color = "red", dash = "dash")
            ) |>
            plotly::add_segments(
                x = min(d$date),
                xend = max(d$date),
                y = -3 * sdv,
                yend = -3 * sdv,
                name = "-3SD",
                line = list(color = "red", dash = "dash")
            ) |>
            plotly::layout(yaxis = list(title = "Irregular"))
    }
    if (is.null(seasonal)) {
        p <- plotly::subplot(series, trend, seasonal_adj, irregular,
            nrows = 4, titleY = TRUE, shareX = TRUE
        )
    } else {
        p <- plotly::subplot(series, trend, seasonal, seasonal_adj, irregular,
            nrows = 5, titleY = TRUE, shareX = TRUE
        )
    }
    capitalize_first <- function(word) {
        if (!is.character(word) || length(word) != 1) {
            stop("Input must be a single character string")
        }
        return(paste0(toupper(substr(word, 1, 1)), tolower(substr(word, 2, nchar(word)))))
    }

    p <- p |>
        plotly::layout(xaxis = list(title = paste("Decomposition Method: ",
            obj_attr$method,
            "; Frequency: ",
            capitalize_first(interval),
            sep = ""
        )))

    return(p)
}


#' Compute and plot ACF of a time series
#'
#' @param ts Time series data
#' @param var Variable name to be plotted
#' @param lag_max Maximum number of lags
#' @param frequency Frequency at which seasonal component is expected (default = NULL)
#'
#' @return A plotly object

plot_acf <- function(ts, var, lag_max, frequency, alpha = 0.05) {
    a <- ts |> feasts::ACF(!!rlang::sym(var), lag_max = lag_max)
    color <- "#0072B5"
    pi_upper <- qnorm(1 - alpha / 2) / sqrt(nrow(ts))
    pi_upper
    p <- plotly::plot_ly(type = "bar")

    if (!is.null(frequency)) {
        s <- seq(from = frequency, by = frequency, to = nrow(a))
        a$seasonal <- NA
        a$non_seasonal <- a$acf
        a$non_seasonal[s] <- NA
        a$seasonal[s] <- a$acf[s]

        p <- p |>
            plotly::add_trace(x = a$lag, y = a$non_seasonal, name = "Non-seasonal", marker = list(
                color = color,
                line = list(
                    color = "rgb(8,48,107)",
                    width = 1.5
                )
            )) |>
            plotly::add_trace(x = a$lag, y = a$seasonal, name = "Seasonal", marker = list(color = "red", line = list(
                color = "rgb(8,48,107)",
                width = 1.5
            )))
    } else {
        p <- p |> plotly::add_trace(x = a$lag, y = a$acf, name = "Lags", marker = list(
            color = color,
            line = list(
                color = "rgb(8,48,107)",
                width = 1.5
            )
        ))
    }

    p <- p |>
        plotly::layout("ACF Plot", yaxis = list(title = "ACF"), xaxis = list(title = "Lags")) |>
        plotly::add_segments(x = ~ min(a$lag), xend = ~ max(a$lag), y = pi_upper, yend = pi_upper, line = list(color = "black", dash = "dash"), name = "95% CI", showlegend = TRUE, legendgroup = "ci") |>
        plotly::add_segments(x = ~ min(a$lag), xend = ~ max(a$lag), y = -pi_upper, yend = -pi_upper, line = list(color = "black", dash = "dash"), name = "95% CI", showlegend = FALSE, legendgroup = "ci")


    return(p)
}



#' Plot a time series against its lagged value with regression line and metrics
#'
#' @param ts A data frame containing a single time series column.
#' @param var The name of the variable to plot.
#' @param lag The number of lags to consider.
#'
#' @return A `plotly` object showing the relationship between the original
#'         variable and its lagged value, along with a regression line and metrics.
#'

plot_lag <- function(ts, var, lag) {
    d <- ts |>
        dplyr::mutate(lag = dplyr::lag(x = !!rlang::sym(var), n = lag))

    # Create the regression formula
    formula <- as.formula(paste(var, "~ lag"))

    # Fit the linear model
    model <- lm(formula, data = d)

    # Extract model coefficients
    intercept <- coef(model)[1]
    slope <- coef(model)[2]

    # Format regression formula text
    reg_formula <- paste0(
        "y = ", round(intercept, 2),
        ifelse(slope < 0, " - ", " + "),
        abs(round(slope, 2)), paste("*lag", lag, sep = "")
    )

    # Get adjusted R-squared
    adj_r2 <- summary(model)$adj.r.squared
    adj_r2_label <- paste0("Adjusted R² = ", round(adj_r2, 3))

    # Add predicted values to data
    d$predicted <- predict(model, newdata = d)

    # Create plot
    p <- plot_ly(d,
        x = ~lag, y = ~ get(var), type = "scatter", mode = "markers",
        name = "Actual"
    ) %>%
        add_lines(
            x = ~lag, y = ~predicted, name = "Regression Fitted Line",
            line = list(color = "red", dash = "dash")
        ) %>%
        layout(
            title = paste(var, "vs Lag", lag, sep = " "),
            xaxis = list(title = paste("Lag", lag, sep = " ")),
            yaxis = list(title = var),
            annotations = list(
                list(
                    x = 0.05, y = 0.95, xref = "paper", yref = "paper",
                    text = reg_formula,
                    showarrow = FALSE,
                    font = list(size = 12)
                ),
                list(
                    x = 0.05, y = 0.88, xref = "paper", yref = "paper",
                    text = adj_r2_label,
                    showarrow = FALSE,
                    font = list(size = 12)
                )
            )
        )

    return(p)
}


#' Piecewise Linear Regression with Grid Search
#'
#' @param data A data frame or tsibble with time series data
#' @param time_col Name of the time column (as string)
#' @param value_col Name of the value column (as string)
#' @param max_knots Maximum number of knots to test (default: 3)
#' @param min_segment_length Minimum number of observations per segment (default: 30)
#' @param edge_buffer Percentage of data to exclude from edges (default: 0.05)
#' @param grid_resolution Number of candidate positions per knot (default: 20)
#' @param record_search Logical; if TRUE, plots each configuration for camcorder recording (default: FALSE)
#' @param plot_dir Directory to save plots if record_search is TRUE (default: NULL uses tempdir)
#'
#' @return List with optimal model, knot positions, BIC scores, and fitted values
piecewise_regression <- function(data,
                                 time_col = "date",
                                 value_col = "value",
                                 max_knots = 3,
                                 min_segment_length = 30,
                                 edge_buffer = 0.05,
                                 grid_resolution = 20,
                                 record_search = FALSE,
                                 plot_dir = NULL) {
    # Prepare data
    df <- data %>%
        arrange(!!sym(time_col)) %>%
        mutate(
            time_index = 1:n(),
            y = !!sym(value_col)
        )

    n <- nrow(df)

    # Define valid range for knots (exclude edges)
    min_idx <- ceiling(n * edge_buffer)
    max_idx <- floor(n * (1 - edge_buffer))

    # Function to fit piecewise linear model given knot positions
    fit_piecewise <- function(knots, data_df) {
        if (length(knots) == 0) {
            # No knots - simple linear regression
            model <- lm(y ~ time_index, data = data_df)
            return(list(
                model = model,
                rss = sum(residuals(model)^2),
                n_params = 2 # intercept + slope
            ))
        }

        # Sort knots
        knots <- sort(knots)

        # Create piecewise linear model using splines with continuity constraint
        # Build the design matrix manually for continuous piecewise linear
        X <- matrix(1, nrow = n, ncol = 1) # Intercept
        X <- cbind(X, data_df$time_index) # First slope

        # Add broken stick terms (continuous piecewise linear)
        for (k in knots) {
            X <- cbind(X, pmax(data_df$time_index - k, 0))
        }

        # Fit model
        model <- lm(data_df$y ~ X - 1) # -1 removes duplicate intercept

        n_params <- 2 + length(knots) # intercept + initial slope + slope changes

        return(list(
            model = model,
            rss = sum(residuals(model)^2),
            n_params = n_params
        ))
    }

    # Function to calculate BIC
    calc_bic <- function(rss, n, k) {
        n * log(rss / n) + k * log(n)
    }

    # Function to generate candidate knot positions
    generate_candidates <- function(n_knots, min_idx, max_idx, min_segment) {
        if (n_knots == 0) {
            return(list(integer(0)))
        }

        # Calculate minimum spacing required
        total_min_length <- (n_knots + 1) * min_segment
        available_length <- max_idx - min_idx + 1

        if (total_min_length > available_length) {
            warning(paste("Cannot fit", n_knots, "knots with min segment length", min_segment))
            return(list())
        }

        # Grid search approach
        candidates <- list()

        if (n_knots == 1) {
            # For 1 knot, test positions with proper spacing
            positions <- seq(min_idx + min_segment,
                max_idx - min_segment,
                length.out = min(grid_resolution, max_idx - min_idx - 2 * min_segment)
            )
            positions <- round(positions)
            candidates <- as.list(positions)
        } else if (n_knots == 2) {
            # For 2 knots, test grid of positions
            pos1_range <- seq(min_idx + min_segment,
                max_idx - 2 * min_segment,
                length.out = min(grid_resolution, available_length / 3)
            )
            pos1_range <- round(pos1_range)

            for (pos1 in pos1_range) {
                pos2_range <- seq(pos1 + min_segment,
                    max_idx - min_segment,
                    length.out = min(grid_resolution, (max_idx - pos1 - min_segment) / min_segment)
                )
                pos2_range <- round(pos2_range)

                for (pos2 in pos2_range) {
                    candidates[[length(candidates) + 1]] <- c(pos1, pos2)
                }
            }
        } else {
            # For 3+ knots, use a coarser grid
            # Divide the range into n_knots+1 segments and place knots at boundaries
            segment_length <- (max_idx - min_idx) / (n_knots + 1)

            # Create base positions evenly spaced
            base_positions <- round(seq(min_idx + segment_length,
                max_idx - segment_length,
                length.out = n_knots
            ))

            # Test variations around base positions
            search_window <- round(segment_length * 0.3)

            # For simplicity with 3+ knots, test fewer variations
            if (n_knots == 3) {
                for (offset1 in seq(-search_window, search_window, length.out = 5)) {
                    for (offset2 in seq(-search_window, search_window, length.out = 5)) {
                        for (offset3 in seq(-search_window, search_window, length.out = 5)) {
                            pos <- round(base_positions + c(offset1, offset2, offset3))
                            # Check minimum segment constraint
                            if (all(diff(c(min_idx, pos, max_idx)) >= min_segment)) {
                                candidates[[length(candidates) + 1]] <- pos
                            }
                        }
                    }
                }
            } else {
                # For 4+ knots, just use base positions
                candidates <- list(base_positions)
            }
        }

        return(candidates)
    }

    # Test different numbers of knots
    results <- list()

    # Track overall best for animation
    overall_best_bic <- Inf
    config_count <- 0  # Track total configurations tested

    for (k in 0:max_knots) {
        cat("Testing", k, "knot(s)...\n")

        # Generate candidate knot positions
        candidates <- generate_candidates(k, min_idx, max_idx, min_segment_length)

        if (length(candidates) == 0) {
            next
        }

        best_bic <- Inf
        best_knots <- NULL
        best_model <- NULL
        best_rss <- NULL

        # Test each candidate
        for (knots in candidates) {
            fit <- fit_piecewise(knots, df)
            bic <- calc_bic(fit$rss, n, fit$n_params)
            config_count <- config_count + 1

            # Plot if recording is enabled
            if (record_search) {
                # Get fitted values for current configuration
                fitted_vals <- fitted(fit$model)

                # Determine if this is the new best
                is_current_best <- bic < best_bic
                is_overall_best <- bic < overall_best_bic

                # Create the plot
                p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[time_col]], y = .data$y)) +
                    ggplot2::geom_point(alpha = 0.5, size = 1.5, color = "steelblue") +
                    ggplot2::geom_line(ggplot2::aes(y = fitted_vals),
                                      color = ifelse(is_overall_best, "darkgreen",
                                                   ifelse(is_current_best, "steelblue", "gray50")),
                                      linewidth = 1.2) +
                    ggplot2::labs(
                        title = sprintf("Grid Search: Testing %d Knot(s) | Config #%d",
                                      k, config_count),
                        subtitle = ifelse(is_overall_best, "NEW OVERALL BEST!",
                                        ifelse(is_current_best, "New best for this knot count", "")),
                        x = time_col,
                        y = value_col
                    ) +
                    ggplot2::theme_minimal(base_size = 14) +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(face = "bold", size = 16),
                        plot.subtitle = ggplot2::element_text(
                            color = ifelse(is_overall_best, "darkgreen", "steelblue"),
                            face = "bold",
                            size = 13
                        ),
                        plot.background = ggplot2::element_rect(fill = "white", color = NA),
                        panel.background = ggplot2::element_rect(fill = "white", color = NA)
                    )

                # Add vertical lines for knots
                if (length(knots) > 0) {
                    knot_times <- df[[time_col]][knots]
                    for (kt in knot_times) {
                        p <- p + ggplot2::geom_vline(xintercept = kt,
                                                    linetype = "dashed",
                                                    color = "red",
                                                    linewidth = 0.8,
                                                    alpha = 0.7)
                    }
                }

                # Add BIC score annotation
                p <- p + ggplot2::annotate("text",
                    x = -Inf, y = Inf,
                    label = sprintf("BIC: %.2f\nKnots: %d\nRSS: %.2f",
                                  bic, k, fit$rss),
                    hjust = -0.1, vjust = 1.2,
                    size = 5,
                    fontface = "bold",
                    color = ifelse(is_overall_best, "darkgreen", "gray20")
                )

                # Save plot manually to file
                if (!is.null(plot_dir)) {
                    frame_file <- file.path(plot_dir, sprintf("frame_%04d.png", config_count))
                    ggplot2::ggsave(
                        filename = frame_file,
                        plot = p,
                        width = 8,
                        height = 6,
                        dpi = 100,
                        device = "png"
                    )
                    cat("  Saved frame", config_count, "to", basename(frame_file), "\n")
                } else {
                    # Just print if no directory specified
                    print(p)
                }
            }

            if (bic < best_bic) {
                best_bic <- bic
                best_knots <- knots
                best_model <- fit$model
                best_rss <- fit$rss
            }

            # Update overall best
            if (bic < overall_best_bic) {
                overall_best_bic <- bic
            }
        }

        results[[k + 1]] <- list(
            n_knots = k,
            knots = best_knots,
            bic = best_bic,
            rss = best_rss,
            model = best_model,
            n_candidates = length(candidates)
        )

        cat(
            "  Best BIC:", round(best_bic, 2), "| RSS:", round(best_rss, 2),
            "| Tested", length(candidates), "configurations\n"
        )
    }

    # Find optimal number of knots
    bic_values <- sapply(results, function(x) x$bic)
    optimal_idx <- which.min(bic_values)
    optimal <- results[[optimal_idx]]

    cat("\nOptimal model: ", optimal$n_knots, "knot(s) with BIC =", round(optimal$bic, 2), "\n")

    # Get fitted values for optimal model
    df$fitted <- fitted(optimal$model)

    # Convert knot indices back to original time values
    if (length(optimal$knots) > 0) {
        knot_dates <- df[[time_col]][optimal$knots]
    } else {
        knot_dates <- NULL
    }

    return(list(
        optimal_knots = optimal$n_knots,
        knot_positions = optimal$knots,
        knot_dates = knot_dates,
        bic_scores = data.frame(
            n_knots = sapply(results, function(x) x$n_knots),
            bic = bic_values,
            rss = sapply(results, function(x) x$rss)
        ),
        model = optimal$model,
        data = df,
        all_results = results
    ))
}


#' Plot BIC Scores by Number of Knots using Plotly
#'
#' This function creates an interactive plotly visualization of BIC scores
#' across different numbers of knots, highlighting the optimal choice.
#'
#' @param result Output from the piecewise_regression function
#'
#' @return A plotly object showing BIC scores by number of knots
#'
plot_bic_scores <- function(result) {
    # Extract BIC scores data frame
    bic_data <- result$bic_scores
    optimal_knots <- result$optimal_knots

    # Get the optimal point data
    optimal_point <- bic_data[bic_data$n_knots == optimal_knots, ]

    # Create the plot
    p <- plotly::plot_ly() |>
        # Add line
        plotly::add_trace(
            data = bic_data,
            x = ~n_knots,
            y = ~bic,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "steelblue", width = 2),
            marker = list(size = 8, color = "steelblue"),
            name = "BIC Score",
            showlegend = FALSE
        ) |>
        # Add optimal point highlight
        plotly::add_trace(
            data = optimal_point,
            x = ~n_knots,
            y = ~bic,
            type = "scatter",
            mode = "markers",
            marker = list(size = 12, color = "red"),
            name = "Optimal",
            showlegend = TRUE
        ) |>
        # Add annotation for optimal point
        plotly::layout(
            title = list(
                text = "BIC Scores by Number of Knots<br><sub>Lower BIC = Better Model</sub>",
                font = list(size = 16)
            ),
            xaxis = list(
                title = "Number of Knots",
                dtick = 1
            ),
            yaxis = list(
                title = "BIC"
            ),
            annotations = list(
                list(
                    x = optimal_point$n_knots,
                    y = optimal_point$bic,
                    text = "Optimal",
                    xanchor = "left",
                    xshift = 10,
                    showarrow = TRUE,
                    arrowhead = 2,
                    arrowsize = 1,
                    arrowwidth = 2,
                    arrowcolor = "red",
                    ax = 40,
                    ay = 0,
                    font = list(
                        size = 12,
                        color = "red",
                        family = "Arial Black"
                    )
                )
            ),
            hovermode = "closest"
        )

    return(p)
}


#' Record Piecewise Regression Grid Search Animation
#'
#' This function is a wrapper around piecewise_regression that handles
#' the camcorder recording setup and creates an animated GIF of the grid search process.
#'
#' @param data A data frame or tsibble with time series data
#' @param time_col Name of the time column (as string)
#' @param value_col Name of the value column (as string)
#' @param max_knots Maximum number of knots to test (default: 3)
#' @param min_segment_length Minimum number of observations per segment (default: 30)
#' @param edge_buffer Percentage of data to exclude from edges (default: 0.05)
#' @param grid_resolution Number of candidate positions per knot (default: 20)
#' @param output_dir Directory to save animation frames and GIF (default: "grid_search_animation")
#' @param gif_name Name of output GIF file (default: "grid_search.gif")
#' @param width Plot width in pixels (default: 800)
#' @param height Plot height in pixels (default: 600)
#' @param fps Frames per second for GIF (default: 2)
#'
#' @return List with piecewise regression results and path to GIF
#'
#' @examples
#' \dontrun{
#' # Record the grid search animation
#' result <- record_piecewise_search(
#'   data = ts1,
#'   time_col = "index",
#'   value_col = "y",
#'   max_knots = 2,
#'   output_dir = "animation_frames"
#' )
#'
#' # View the result
#' browseURL(result$gif_path)
#' }
#'
record_piecewise_search <- function(data,
                                   time_col = "date",
                                   value_col = "value",
                                   max_knots = 3,
                                   min_segment_length = 30,
                                   edge_buffer = 0.05,
                                   grid_resolution = 20,
                                   output_dir = "grid_search_animation",
                                   gif_name = "grid_search.gif",
                                   width = 800,
                                   height = 600,
                                   fps = 2) {
    # Check if magick is installed
    if (!requireNamespace("magick", quietly = TRUE)) {
        stop("Package 'magick' is required. Install it with: install.packages('magick')")
    }

    # Check data size and provide warnings
    n <- nrow(data)
    min_idx <- ceiling(n * edge_buffer)
    max_idx <- floor(n * (1 - edge_buffer))
    available_length <- max_idx - min_idx + 1

    cat("\n=== Data Analysis ===\n")
    cat("Total observations:", n, "\n")
    cat("Available range for knots:", min_idx, "to", max_idx,
        "(", available_length, "observations )\n")
    cat("Min segment length:", min_segment_length, "\n")
    cat("Max segments needed for", max_knots, "knots:", max_knots + 1, "\n")
    cat("Min observations required:", (max_knots + 1) * min_segment_length, "\n")

    if ((max_knots + 1) * min_segment_length > available_length) {
        recommended_min_seg <- floor(available_length / (max_knots + 1))
        cat("\n⚠️  WARNING: Your dataset may be too small!\n")
        cat("Recommended min_segment_length:", max(3, recommended_min_seg), "\n")
        cat("Or reduce max_knots to:", floor(available_length / min_segment_length) - 1, "\n\n")
    }

    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Clean out any old PNG files from previous runs
    old_pngs <- list.files(output_dir, pattern = "^frame_.*\\.png$", full.names = TRUE)
    if (length(old_pngs) > 0) {
        cat("Removing", length(old_pngs), "old PNG files from previous run...\n")
        file.remove(old_pngs)
    }

    # Run piecewise regression with recording enabled
    cat("\n=== Running grid search ===\n")
    result <- piecewise_regression(
        data = data,
        time_col = time_col,
        value_col = value_col,
        max_knots = max_knots,
        min_segment_length = min_segment_length,
        edge_buffer = edge_buffer,
        grid_resolution = grid_resolution,
        record_search = TRUE,
        plot_dir = output_dir
    )

    # Check if any frames were captured
    png_files <- list.files(output_dir, pattern = "^frame_.*\\.png$", full.names = TRUE)
    png_files <- sort(png_files)  # Ensure correct order

    cat("\n=== Checking captured frames ===\n")
    cat("Output directory:", normalizePath(output_dir), "\n")
    cat("PNG files found:", length(png_files), "\n")

    if (length(png_files) == 0) {
        warning(
            "No animation frames were captured.\n",
            "  Current settings: min_segment_length = ", min_segment_length,
            ", max_knots = ", max_knots, "\n",
            "  Recommended: min_segment_length = ", max(3, floor(available_length / (max_knots + 1)))
        )
        return(list(
            regression_result = result,
            gif_path = NULL,
            frames_dir = output_dir,
            frames_captured = 0
        ))
    }

    # Create GIF using magick
    cat("\n=== Creating GIF with magick ===\n")
    gif_path <- file.path(output_dir, gif_name)

    # Read all frames
    frames <- magick::image_read(png_files)

    # Set delays (in 1/100ths of a second)
    delay_frames <- 100 / fps  # Normal frames
    delay_first <- 300  # 3 seconds for first frame
    delay_last <- 500   # 5 seconds for last frame

    # Create vector of delays
    n_frames <- length(frames)
    delays <- rep(delay_frames, n_frames)
    delays[1] <- delay_first
    delays[n_frames] <- delay_last

    # Animate with specified delays
    animated <- magick::image_animate(frames, delay = delays, optimize = TRUE)

    # Write GIF
    magick::image_write(animated, path = gif_path, format = "gif")

    cat("\n✅ Animation saved to:", gif_path, "\n")
    cat("📊 Total frames captured:", length(png_files), "\n\n")

    return(list(
        regression_result = result,
        gif_path = gif_path,
        frames_dir = output_dir,
        frames_captured = length(png_files)
    ))
}
