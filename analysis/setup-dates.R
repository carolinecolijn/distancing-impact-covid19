# setup-dates.R  Temporary file to explain how Andy is setting up dates in
#  SIR-explore.Rmd. If we could use the same names that would be great, or at
#  least be aware of them (easy  enough to change in mine).
# Each model run is defined as a list object in
#  SIR-explore.Rmd, as run[[x]] for run x. I think STAN will take care of a lot
#  of the model runs that we might have done, but I think we still need this
#  approach for testing bigger assumptions.
# So this sets up the defaults dates and timing, which I then save for run 1 at
#  the end. [With bcdata from the old data file, everything started on
#  1st March, but the new data stream has data back to 22nd January, but we don't
#  want to fit all the way back to then, so we need more definitions of dates].
#
# So we have things like
#  run[[1]]$timing$start_data
# and it would be helpful (though not imperative) if the 02-seeiqr-fit.R
#  etc. code had variable names such as just
#  start_data

timing_default = list()

timing_default$start_data <- min(new_data$"Date")     # first date with data
timing_default$final_data <- max(new_data$"Date")     # last date with data
timing_default$start_fit_data <- ymd("2020-03-01")    # first data to use for fitting
timing_default$last_fit_data <- timing_default$final_data     # last data to use for fitting (change this for
                      #  retrospective runs)

timing_default$start_ode <- ymd("2020-01-31")  # date to start running ode
                                        # model (will be time = 0)
timing_default$end_ode <- timing_default$final_data + 30           # date to stop running ode model

# These were for what to return from the ode solver to then figure out the
# expected cases each day - may not be needed for STAN approach?
timing_default$inc_ode <- 0.1   # incremental time step (days) for ode model; need for
                      #  integrating the Weibull delay function)
timing_default$seq_ode <- seq(0,
                      as.numeric(timing_default$end_ode - timing_default$start_ode),
                      by = timing_default$inc_ode)

run[[1]]$timing <- timing_default
