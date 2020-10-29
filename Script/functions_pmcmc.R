## Plot output
plot_pmcmc_output_deaths <- function(pmcmc_output,df,title_name){
  if(sum(df$deaths>0)>1) {
    if(tail(diff(which(df$deaths>0)),1) > 21) {
      df$deaths[tail(which(df$deaths>0),1)] <- 0
    }
  }
  
  # get the raw data correct
  data <- df[,c("date", "deaths")]
  names(data)[1] <- "date"
  data <- data[order(data$date),]
  data$date <- as.Date(data$date)
  
  # and remove the rows with no data up to the first date that a death was reported
  first_report <- which(data$deaths>0)[1]
  missing <- which(data$deaths == 0 | is.na(data$deaths))
  to_remove <- missing[missing<first_report]
  if(length(to_remove) > 0) {
    if(length(to_remove) == (nrow(data)-1)) {
      data <- data[-head(to_remove,-1),]
    } else {
      data <- data[-to_remove,]
    }
  }
  
  plot_title <- ggdraw() + 
    draw_label(
      title_name,
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  plot_row <- cowplot::plot_grid(rt_plot_immunity(pmcmc_output)$plot, plot(pmcmc_output, particle_fit = TRUE) + geom_smooth(aes(date, deaths), data, span = 0.3))
  plot_output <- plot_grid(plot_title, plot_row, ncol = 1, # rel_heights values control vertical title margins
                           rel_heights = c(0.1, 1)
  )
  
  return(plot_output)
}


get_immunity_ratios <- function(out) {
  
  mixing_matrix <- squire:::process_contact_matrix_scaled_age(
    out$pmcmc_results$inputs$model_params$contact_matrix_set[[1]],
    out$pmcmc_results$inputs$model_params$population
  )
  
  dur_ICase <- out$parameters$dur_ICase
  dur_IMild <- out$parameters$dur_IMild
  prob_hosp <- out$parameters$prob_hosp
  
  # assertions
  squire:::assert_single_pos(dur_ICase, zero_allowed = FALSE)
  squire:::assert_single_pos(dur_IMild, zero_allowed = FALSE)
  squire:::assert_numeric(prob_hosp)
  squire:::assert_numeric(mixing_matrix)
  squire:::assert_square_matrix(mixing_matrix)
  squire:::assert_same_length(mixing_matrix[,1], prob_hosp)
  
  if(sum(is.na(prob_hosp)) > 0) {
    stop("prob_hosp must not contain NAs")
  }
  
  if(sum(is.na(mixing_matrix)) > 0) {
    stop("mixing_matrix must not contain NAs")
  }
  
  index <- squire:::odin_index(out$model)
  pop <- out$parameters$population
  t_now <- which(as.Date(rownames(out$output)) == max(out$pmcmc_results$inputs$data$date))
  prop_susc <- lapply(seq_len(dim(out$output)[3]), function(x) {
    t(t(out$output[seq_len(t_now), index$S, x])/pop)
  } )
  
  relative_R0_by_age <- prob_hosp*dur_ICase + (1-prob_hosp)*dur_IMild
  
  adjusted_eigens <- lapply(prop_susc, function(x) {
    
    unlist(lapply(seq_len(nrow(x)), function(y) {
      if(any(is.na(x[y,]))) {
        return(NA)
      } else {
        Re(eigen(mixing_matrix*x[y,]*relative_R0_by_age)$values[1])
      }
    }))
    
  })
  
  betas <- lapply(out$replicate_parameters$R0, function(x) {
    squire:::beta_est(squire_model = out$pmcmc_results$inputs$squire_model, 
                      model_params = out$pmcmc_results$inputs$model_params, 
                      R0 = x)
  })
  
  ratios <- lapply(seq_along(betas), function(x) {
    (betas[[x]] * adjusted_eigens[[x]]) / out$replicate_parameters$R0[[x]]
  })
  
  return(ratios)
}


rt_plot_immunity <- function(out) {
  
  if (is.null(out$parameters$country)) {
    iso3c <- "NA" 
  } else {
    iso3c <- squire::get_population(out$parameters$country)$iso3c[1]
  }
  
  if("pmcmc_results" %in% names(out)) {
    wh <- "pmcmc_results"
  } else {
    wh <- "scan_results"
  }
  
  date <- max(as.Date(out$pmcmc_results$inputs$data$date))
  date_0 <- date
  
  # impact of immunity ratios
  ratios <- get_immunity_ratios(out)
  
  # create the Rt data frame
  rts <- lapply(seq_len(length(out$replicate_parameters$R0)), function(y) {
    
    tt <- squire:::intervention_dates_for_odin(dates = out$interventions$date_R0_change, 
                                               change = out$interventions$R0_change, 
                                               start_date = out$replicate_parameters$start_date[y],
                                               steps_per_day = 1/out$parameters$dt)
    
    if(wh == "scan_results") {
      Rt <- c(out$replicate_parameters$R0[y], 
              vapply(tt$change, out[[wh]]$inputs$Rt_func, numeric(1), 
                     R0 = out$replicate_parameters$R0[y], Meff = out$replicate_parameters$Meff[y])) 
    } else {
      Rt <- squire:::evaluate_Rt_pmcmc(
        R0_change = tt$change, 
        date_R0_change = tt$dates, 
        R0 = out$replicate_parameters$R0[y], 
        pars = as.list(out$replicate_parameters[y,]),
        Rt_args = out$pmcmc_results$inputs$Rt_args) 
    }
    
    df <- data.frame(
      "Rt" = Rt,
      "Reff" = Rt*tail(na.omit(ratios[[y]]),length(Rt)),
      "R0" = na.omit(Rt)[1]*tail(na.omit(ratios[[y]]),length(Rt)),
      "date" = tt$dates,
      "iso" = iso3c,
      rep = y,
      stringsAsFactors = FALSE)
    df$pos <- seq_len(nrow(df))
    return(df)
  } )
  
  rt <- do.call(rbind, rts)
  rt$date <- as.Date(rt$date)
  
  rt <- rt[,c(5,4,1,2,3,6,7)]
  
  new_rt_all <- rt %>%
    group_by(iso, rep) %>% 
    arrange(date) %>% 
    complete(date = seq.Date(min(rt$date), date_0, by = "days")) 
  
  column_names <- colnames(new_rt_all)[-c(1,2,3)]
  new_rt_all <- fill(new_rt_all, all_of(column_names), .direction = c("down"))
  new_rt_all <- fill(new_rt_all, all_of(column_names), .direction = c("up"))
  
  suppressMessages(sum_rt <- group_by(new_rt_all, iso, date) %>% 
                     summarise(Rt_min = quantile(Rt, 0.025),
                               Rt_q25 = quantile(Rt, 0.25),
                               Rt_q75 = quantile(Rt, 0.75),
                               Rt_max = quantile(Rt, 0.975),
                               Rt_median = median(Rt),
                               Rt = mean(Rt),
                               R0_min = quantile(R0, 0.025),
                               R0_q25 = quantile(R0, 0.25),
                               R0_q75 = quantile(R0, 0.75),
                               R0_max = quantile(R0, 0.975),
                               R0_median = median(R0),
                               R0 = mean(R0),
                               Reff_min = quantile(Reff, 0.025),
                               Reff_q25 = quantile(Reff, 0.25),
                               Reff_q75 = quantile(Reff, 0.75),
                               Reff_max = quantile(Reff, 0.975),
                               Reff_median = median(Reff),
                               Reff = mean(Reff)))
  
  min_date <- min(as.Date(out$replicate_parameters$start_date))
  
  country_plot <- function(vjust = -1.2) {
    ggplot(sum_rt %>% filter(
      date > min_date & date <= as.Date(as.character(date_0+as.numeric(lubridate::wday(date_0)))))) +
      geom_ribbon(mapping = aes(x=date, ymin=R0_min, ymax = R0_max, group = iso), fill = "#8cbbca") +
      geom_ribbon(mapping = aes(x = date, ymin = R0_q25, ymax = R0_q75, group = iso), fill = "#3f8da7") +
      geom_ribbon(mapping = aes(x=date, ymin=Reff_min, ymax = Reff_max, group = iso), fill = "#96c4aa") +
      geom_ribbon(mapping = aes(x = date, ymin = Reff_q25, ymax = Reff_q75, group = iso), fill = "#48996b") +
      geom_line(mapping = aes(x = date, y = Reff_median), color = "#48996b") +
      geom_hline(yintercept = 1, linetype = "dashed") +
      geom_hline(yintercept = sum_rt$R0_median[1], linetype = "dashed") +
      theme_bw() +
      theme(axis.text = element_text(size=12)) +
      xlab("") +
      ylab("Reff") +
      scale_x_date(breaks = "2 weeks",
                   limits = as.Date(c(as.character(min_date),
                                      as.character(date_0+as.numeric(lubridate::wday(date_0))))), 
                   date_labels = "%d %b",
                   expand = c(0,0)) + 
      theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = "black"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black")
      )
  }
  
  
  res <- list("plot" = suppressWarnings(country_plot()), "rts" = sum_rt)
  return(res)  
}
