#' Data summary function
#'
#' @description Convert 'nlmixr' model output into an 'xpose' database
#'
#' @param obj nlmixr fit object to be evaluated
#' @param model Model. Can be blank
#' @param software Software that generated the model fit
#' @param rounding Number of figures to round estimates to
#' @param runname Name of the model object being converted
#'
#' @return A summary data object used by \link{xpose_data_nlmixr}.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows filter mutate arrange_at case_when select one_of tibble
#' @importFrom tidyr complete
#' @importFrom rlang syms


summarise_nlmixr_model <- function(obj, model, software, rounding, runname) {
    sum <- dplyr::bind_rows(
                      sum_nlmixr_software(software),                    # Software name
    sum_nlmixr_version(software),                     # Software version
    sum_nlmixr_file(runname, software),               # Model file
    sum_nlmixr_run(obj, runname, software),           # Model run
    sum_nlmixr_directory(obj, software),              # Model directory
    sum_nlmixr_reference(model, software),            # Reference model
    sum_nlmixr_timestart(obj, model, software),            # Run start time
    sum_nlmixr_timestop(obj, model, software),             # Run stop time
    sum_nlmixr_probn(model, software),                # Problem no.
    sum_nlmixr_label(model, software),                # Model label
    sum_nlmixr_description(model, software),          # Model description
    sum_nlmixr_input_data(obj, model, software),           # Model input data used
    sum_nlmixr_nobs(model, software, obj),               # Simulation seed
    sum_nlmixr_subroutine(model, software),           # Des solver
    sum_nlmixr_runtime(model, software, obj, rounding),   # Estimation runtime
    sum_nlmixr_covtime(model, software, obj, rounding),   # Covariance matrix runtime
    sum_nlmixr_term(model, software),                 # Run termination message
    sum_nlmixr_warnings(model, software),             # Run warnings (e.g. boundary)
    sum_nlmixr_errors(model, software),               # Run errors (e.g termination error)
    sum_nlmixr_nsig(model, software),                 # Number of significant digits
    sum_nlmixr_condn(model, software, rounding),      # Condition number
    sum_nlmixr_nesample(model, software),             # Number of esample
    sum_nlmixr_esampleseed(model, software),          # esample seed number
    sum_nlmixr_ofv(model, software, obj, rounding),   # Objective function value
    sum_nlmixr_method(model, software, obj),          # Estimation method or sim
    sum_nlmixr_shk(model, software, 'eps', obj, rounding), # Epsilon shrinkage
    sum_nlmixr_shk(model, software, 'eta', obj, rounding)  # Eta shrinkage
  )


  . = NULL
  problem <- label <- NULL

  # Complete missing cases for consistency
  tmp <- sum %>%
    dplyr::filter(.$problem != 0)

  if (nrow(tmp) == 0) return(sum)


  tmp %>%
    tidyr::complete(!!!rlang::syms(c('problem', 'label')),
                    fill = list(subprob = 0, value = 'na')) %>%
    dplyr::bind_rows(dplyr::filter(sum, sum$problem == 0)) %>%
    dplyr::arrange_at(.vars = c('problem', 'label', 'subprob')) %>%
    dplyr::mutate(descr = dplyr::case_when(
      .$label == 'software' ~ 'Software',
      .$label == 'version' ~ 'Software version',
      .$label == 'file' ~ 'Run file',
      .$label == 'run' ~ 'Run number',
      .$label == 'dir' ~ 'Run directory',
      .$label == 'ref' ~ 'Reference model',
      .$label == 'probn' ~ 'Problem number',
      .$label == 'timestart' ~ 'Run start time',
      .$label == 'timestop' ~ 'Run stop time',
      .$label == 'descr' ~ 'Run description',
      .$label == 'label' ~ 'Run label',
      .$label == 'data' ~ 'Input data',
      .$label == 'nobs' ~ 'Number of observations',
      .$label == 'nind' ~ 'Number of individuals',
      .$label == 'nsim' ~ 'Number of simulations',
      .$label == 'simseed' ~ 'Simulation seed',
      .$label == 'subroutine' ~ 'ADVAN',
      .$label == 'runtime' ~ 'Estimation runtime',
      .$label == 'covtime' ~ 'Covariance step runtime',
      .$label == 'term' ~ 'Termination message',
      .$label == 'warnings' ~ 'Run warnings',
      .$label == 'errors' ~ 'Run errors',
      .$label == 'nsig' ~ 'Number of significant digits',
      .$label == 'condn' ~ 'Condition number',
      .$label == 'nesample' ~ 'Number of ESAMPLE',
      .$label == 'esampleseed' ~ 'ESAMPLE seed number',
      .$label == 'ofv' ~ 'Objective function value',
      .$label == 'method' ~ 'Estimation method',
      .$label == 'epsshk' ~ 'Epsilon shrinkage',
      .$label == 'etashk' ~ 'Eta shrinkage')) %>%
    dplyr::select(dplyr::one_of('problem', 'subprob', 'descr', 'label', 'value'))
}

# Default template for function output
sum_tpl <- function(label, value) {
  dplyr::tibble(problem = 0,
                subprob = 0,
                label   = label,
                value   = value)
}

# Software name
sum_nlmixr_software <- function(software) {
  sum_tpl('software', software)
}

# Software version
sum_nlmixr_version <- function(software) {
    sum_tpl('version', as.character(utils::packageVersion('nlmixr')))
}

# Model object name
sum_nlmixr_file <- function(runname, software) {
  if (software == 'nlmixr') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'file', value = runname)
  }
}

# Model run name
sum_nlmixr_run <- function(obj, runname, software) {
  if (software == 'nlmixr') {
    if(!is.null(obj$model.name)) {
      dplyr::tibble(problem = 1, subprob = 0, label = 'run', value = obj$model.name)
    } else {
      dplyr::tibble(problem = 1, subprob = 0, label = 'run', value = runname)
    }
  }
}

# Model file directory
sum_nlmixr_directory <- function(obj, software) {
  if (software == 'nlmixr') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'dir', value = getwd())
  }
}

# Reference model
sum_nlmixr_reference <- function(model, software) {
  if (software == 'nlmixr') {
    sum_tpl('ref', 'not implemented')
  }
}

# Run start time
sum_nlmixr_timestart <- function(obj, model, software) {
  if (software == 'nlmixr') {
    dplyr::tibble(problem = 0, subprob = 0, label = 'timestart', value = as.character(obj$start.time))
  }
}

# Run stop time
sum_nlmixr_timestop <- function(obj, model, software) {
  if (software == 'nlmixr') {
    dplyr::tibble(problem = 0, subprob = 0, label = 'timestop', value = as.character(obj$stop.time))
  }
}

# Problem no.
sum_nlmixr_probn <- function(model, software) {
  if (software == 'nlmixr') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'probn', value = '1')
  }
}

# Model Label
sum_nlmixr_label <- function(model, software) {
  if (software == 'nlmixr') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'label', value = 'na')
  }
}

# Model description
sum_nlmixr_description <- function(model, software) {
  if (software == 'nlmixr') {
    sum_tpl('descr', 'not implemented')
  }
}

# Input data
sum_nlmixr_input_data <- function(obj, model, software) {
  if (software == 'nlmixr') {
    if(!is.null(obj$data.name)) {
        dplyr::tibble(problem = 1, subprob = 0, label = 'data', value = obj$data.name)
    } else {
      sum_tpl('data', 'not available')
    }
  }
}

# Number of observations
sum_nlmixr_nobs <- function(model, software, obj) {
  if (software == 'nlmixr') {
      dplyr::tibble(problem = 1, subprob = 0, label = 'nobs', value = as.character(stats::nobs(obj)))
  }
}

# Number of individuals
sum_nlmixr_nind <- function(model, software, obj) {
  if (software == 'nlmixr') {
    if ("nlmixr_nlme" %in% class(obj)) {
      nind <- dim(obj$fitted)[1]
    }
    if (any("nlmixrFitData" == class(obj))) {
      nind <- length(unique(obj$ID))
    }
    dplyr::tibble(problem = 1, subprob = 0, label = 'nind', value = as.character(nind))
  }
}

# Simulation number
sum_nlmixr_nsim <- function(model, software) {
  if (software == 'nlmixr') {
    sum_tpl('nsim', 'not implemented')
  }
}

# Simulation seed
sum_nlmixr_simseed <- function(obj, model, software) {
  if (software == 'nlmixr') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'seed', value = as.character(obj$seed))
  }
}

# DES solver
sum_nlmixr_subroutine <- function(model, software) {
  if (software == 'nlmixr') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'subroutine', value = 'na')
  }
}

# Estimation runtime
sum_nlmixr_runtime <- function(model, software, obj, rounding) {
  if (software == 'nlmixr') {
    rt <- 'na'
    if (any("nlmixrFitData" == class(obj))) {
        rt <- sum(as.matrix(obj$time[, names(obj$time) != "covariance"]), na.rm = TRUE)
    }
    if (rt!='na') {
      dplyr::tibble(problem = 1, subprob = 0, label = 'runtime', value = as.character(round(rt, rounding)))
    } else {
      dplyr::tibble(problem = 1, subprob = 0, label = 'runtime', value = 'not available')
    }
  }
}

# Covariance matrix runtime
sum_nlmixr_covtime <- function(model, software, obj, rounding) {
  if (software == 'nlmixr') {

    rt <- obj$time$covariance
    if (!is.null(rt)) {
      dplyr::tibble(problem = 1, subprob = 0, label = 'covtime', value = as.character(round(rt, rounding)))
    } else {
      dplyr::tibble(problem = 1, subprob = 0, label = 'covtime', value = 'not available')
    }
  }
}

# Run termination
sum_nlmixr_term <- function(model, software) {
  if (software == 'nlmixr') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'term', value = 'na')
  }
}


# Run warnings (e.g. boundary)
sum_nlmixr_warnings <- function(model, software) {
  if (software == 'nlmixr') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'warnings', value = 'na')
  }
}

# Run errors (e.g termination error)
sum_nlmixr_errors <- function(model, software) {
  if (software == 'nlmixr') {
    sum_tpl('errors', 'na')
  }
}

# Number of significant digits
sum_nlmixr_nsig <- function(model, software) {
  if (software == 'nlmixr') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'nsig', value = 'na')
  }
}

# Condition number
sum_nlmixr_condn <- function(model, software, rounding) {
  if (software == 'nlmixr') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'condn', value = 'not implemented')
  }
}

# Number of ESAMPLE (i.e. NPDE)
sum_nlmixr_nesample <- function(model, software) {
  if (software == 'nlmixr') {
    sum_tpl('nesample', 'na')
  }
}

# ESAMPLE seed number
sum_nlmixr_esampleseed <- function(model, software) {
  if (software == 'nlmixr') {
    sum_tpl('esampleseed', 'na')
  }
}

# Objective function value
sum_nlmixr_ofv <- function(model, software, obj, rounding) {
  if (software == 'nlmixr') {
    ofv <- obj$objective
    if(!is.null(ofv)) {
      dplyr::tibble(problem = 1, subprob = 0, label = 'ofv', value = as.character(round(ofv, digits=rounding)))
    } else {
      dplyr::tibble(problem = 1, subprob = 0, label = 'ofv', value = 'not available')
    }
  }
}

# Estimation method or sim
sum_nlmixr_method <- function(model, software, obj) {
  if (software == 'nlmixr') {
      dplyr::tibble(problem = 1, subprob = 0, label = 'method', value = obj$method)
  }
}

# Epsilon/Eta shrinkage
sum_nlmixr_shk <- function(model, software, type, obj, rounding) {
  if (software == 'nlmixr') {
    if ("nlmixr_nlme" %in% class(obj)) {
      lab <- paste(type, 'shk', sep='')
      dplyr::tibble(problem = 1, subprob = 0, label = lab, value = 'not implemented')
    } else {
      shk <- 'na'
      lab <- paste(type, 'shk', sep='')
      if (any("nlmixrFitData" == class(obj))) {
        if(type=="eps") {
          shk <- paste(round((1 - stats::sd(obj$IWRES))*100, digits = rounding), "[1]", sep=" ")
        }
        if(type=="eta") {
          omega <- diag(obj$omega)
          d <- as.data.frame(obj[!duplicated(obj$ID),])

          ## add ETA if missing
          if(!all(names(omega) %in% names(d))) {
            d <- merge(d, obj$eta)
          }

          ## account for 1-eta systems
          if(length(names(omega)) == 1) {
            d <- data.frame(eta = d[,names(d) %in% names(omega)])
            names(d) <- names(omega)
          } else {
            d <- d[,names(d) %in% names(omega)]
          }

          eshr <- c()
          for (i in 1:length(omega)) {
            shr <- (1 - (stats::sd(d[,i]) / sqrt(omega[i])))*100
            eshr <- c(eshr, round(shr, 3))
          }
          shk <- paste(paste(round(eshr, digits = rounding), ' [', 1:length(eshr), ']', sep=''), collapse=', ')
        }
      }
      dplyr::tibble(problem = 1, subprob = 0, label = lab, value = shk)
    }
  }
}
