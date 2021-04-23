#' Import nlmixr output into R
#'
#' @description Convert 'nlmixr' model output into an 'xpose' database.
#'
#' @param obj nlmixr fit object to be evaluated.
#' @param pred Name of the population prediction variable to use for
#'     plotting. If unspecified, it will choose either "NPDE",
#'     "CWRES", and "RES" (in that order) if the column exists in the
#'     data.
#' @param wres Name of the weighted residual variable to use for
#'     plotting. If unspecified, it will choose either "NPDE",
#'     "CWRES", and "RES" (in that order) if the column exists in the
#'     data.
#' @param gg_theme A ggplot2 theme object.
#' @param xp_theme An xpose theme or vector of modifications to the
#'     xpose theme (eg. \code{c(point_color = 'red', line_linetype =
#'     'dashed')}).
#' @param quiet Logical, if \code{FALSE} messages are printed to the
#'     console.
#' @param skip Character vector be used to skip the import/generation
#'     of: 'data', 'files', 'summary' or any combination of the three.
#' @param ... Additional arguments to be passed to the
#'     \code{\link[readr]{read_delim}} functions.
#'
#' @return An \code{\link[xpose]{xpose_data}} object suitable for use in 'xpose'.
#'
#' @import nlmixr
#' @importFrom dplyr group_by mutate tibble case_when
#' @importFrom tibble as_tibble
#' @importFrom stringr str_detect
#' @importFrom xpose theme_readable theme_xp_default
#' @importFrom stats coef rnorm
#'
#' @examples
#' xpdb <- xpose_data_nlmixr(obj = theo_sd_fit)
#'
#' @export

xpose_data_nlmixr <- function(obj         = NULL,
                              pred        = NULL, #"CPRED",
                              wres        = NULL, #"CWRES",
                              gg_theme    = theme_readable(),
                              xp_theme    = theme_xp_default(),
                              quiet,
                              skip        = NULL,
                              ...) {


  . = NULL
  ID = NULL
  RES = NULL
  DV = NULL
  PRED = NULL
  msg = NULL

  get_wres <- function(res, dv, pred) {
    suppressWarnings(res / (sqrt(stats::cov(dv, pred))))
  }

  if (is.null(obj)) {
    stop('Argument `obj` required.', call. = FALSE)
  }


  if (missing(quiet)) quiet <- !interactive()

  objok <- FALSE

  if (any("nlmixrFitData" == class(obj))) {
      mtype <- obj$method
      software <- "nlmixr"
      if (is.null(wres)){
          if (any(names(obj) == "CWRES")) {
              wres <- "CWRES"
          } else  if (any(names(obj) == "NPDE")){
              wres <- "NPDE"
          } else if (any(names(obj) == "RES")) {
              wres <- "RES"
              obj <- nlmixr::addCwres(obj)
              if (any(names(obj) == "CWRES")){
                  wres <- "CWRES"
                  warning(sprintf("Added CWRES to fit (using %s%s)",
                                  crayon::blue("nlmixr::"), crayon::yellow("addCwres")))
              } else {
                  warning(sprintf("Using RES; Consider adding NPDE (%s%s) to fit",
                                  crayon::blue("nlmixr::"), crayon::yellow("addNpde")))
              }

          }
      }
      if (is.null(pred)){
          if (any(names(obj) == "EPRED") & wres == "NPDE"){
              pred <- "EPRED"
          } else if (any(names(obj) == "CPRED") & wres == "CWRES"){
              pred <- "CPRED"
          } else if (any(names(obj) == "PRED") & wres == "RES"){
              pred <- "PRED"
          } else if (any(names(obj) == "EPRED")){
              pred <- "EPRED"
          } else if (any(names(obj) == "CPRED")){
              pred <- "CPRED"
          } else if (any(names(obj) == "PRED")){
              pred <- "PRED"
          }
      }
      objok <- TRUE
  } else if (any("nlmixr_nlme" == class(obj))) {
    mtype <- obj$method
    software <- "nlmixr"
    if (mtype == "nlme"){
        wres <- "WRES"
        pred <- "PRED"
    }
    objok <- TRUE
  }



                                        #if ((objok == FALSE) | ("nlmixr_nlme" %in% class(obj))) {
  if ((objok == FALSE)) {
    stop('Model type currently not supported by xpose.', call. = FALSE)
  }

  runname <- deparse(substitute(obj))

  if ("nlmixr_nlme" %in% class(obj)) {
    data <- obj$call[[3]]

    data$PRED  <- obj$fitted[,1]
    data$IPRED <- obj$fitted[,2]
    data$RES   <- obj$residuals[,1]
    data$IRES  <- obj$residuals[,2]

    pars <- as.data.frame(stats::coef(obj))
    pars$ID <- row.names(as.data.frame(stats::coef(obj)))

    etas <- as.data.frame(obj$coefficients$random$ID)
    names(etas) <- paste("eta.", names(etas), sep="")
    etas$ID <- row.names(as.data.frame(obj$coefficients$random$ID))

    data$ID <- as.character(data$ID)
    data <- suppressMessages(dplyr::inner_join(data, pars))
    data <- suppressMessages(dplyr::inner_join(data, etas))

    data_a <- data %>%
      dplyr::group_by(ID) %>%
      dplyr::mutate(WRES = get_wres(res = RES, dv = DV, pred=PRED))

    data_a <- tibble::as_tibble(data_a)
  }

  if (any("nlmixrFitData" == class(obj))) {
    data <- as.data.frame(obj)
    data_a <- data %>%
      dplyr::group_by(ID)

    data_a <- tibble::as_tibble(data_a)
  }

  if(!(wres %in% names(data_a))) {
    stop(paste(wres, ' not found in nlmixr fit object.', sep=""), call. = FALSE)
  }

  if(!(pred %in% names(data_a))) {
    stop(paste(pred, ' not found in nlmixr fit object.', sep=""), call. = FALSE)
  }

  if (!is.null(obj$data.name) & any("nlmixrFitData" == class(obj))) {
      ## getData works for nlme/saem;  It also works if you remove the data or use nlmixr's read data set
      full.dat <- suppressWarnings({nlmixr::nlmixrData(nlme::getData(obj))})
      names(full.dat) <- toupper(names(full.dat))
      if (any(names(full.dat) == "EVID")){
          full.dat <- full.dat[full.dat$EVID == 0 | full.dat$EVID == 2, !(names(full.dat) %in% names(data_a))];
      } else if (any(names(full.dat) == "MDV")){
          full.dat <- full.dat[full.dat$MDV == 0, !(names(full.dat) %in% names(data_a))];
      } else {
          full.dat <- full.dat[, !(names(full.dat) %in% names(data_a))]
      }
      data_a <- data.frame(data_a, full.dat);
  }

  # check for ETAs
  # if(!any(stringr::str_detect(names(data_a), 'ETA\\d+|ET\\d+|eta.*'))) {
  #   data_a <- merge(data_a, obj$eta)
  # }
  if(!all(names(diag(obj$omega)) %in% names(data_a))) {
    data_a <- merge(data_a, obj$eta)
  }

  data <- NULL
  data_ind <- data_a %>%
    colnames() %>%
    dplyr::tibble(table = 'nlmixr',
                  col   = .,
                  type  = NA_character_,
                  label = NA_character_,     # Feature to be added in future release
                  units = NA_character_) %>% # Feature to be added in future release
    dplyr::mutate(type = dplyr::case_when(
      .$col == 'ID' ~ 'id',
      .$col == 'DV' ~ 'dv',
      .$col == 'TIME' ~ 'idv',
      .$col == 'OCC' ~ 'occ',
      .$col == 'DVID' ~ 'dvid',
      .$col == 'AMT' ~ 'amt',
      .$col == 'MDV' ~ 'mdv',
      .$col == 'EVID' ~ 'evid',
      .$col == 'IPRED' ~ 'ipred',
      .$col == pred ~ 'pred',
      .$col %in% c('RES', 'WRES', 'CWRES', 'IWRES', 'EWRES', 'NPDE','IRES','CRES') ~ 'res',
      .$col %in% c('WT','AGE','HT','BMI','LBM') ~ 'contcov',
      .$col %in% c('SEX','RACE') ~ 'catcov',
      .$col %in% c('CL','V','V1','V2','V3','Q','Q2','Q3','KA','K12','K21','K','K13','K31','K23','K32','K24','K42','K34','K43',
                   'cl','v','v1','v2','v3','q','q2','q3','ka','k12','k21','k','k13','k31','k23','k32','k24','k42','k34','k43',
                   'tcl','tv','tv1','tv2','tv3','tq','tq2','tq3','tka','tk12','tk21','tk','tk13','tk31','tk23','tk32','tk24','tk42','tk34','tk43') ~ 'param',
      stringr::str_detect(.$col, 'ETA\\d+|ET\\d+|eta.*') ~ 'eta'))

  data_ind$type[is.na(data_ind$type)] <- 'na'

  data <- list()
  data <- dplyr::tibble(problem = 1,
                        simtab = F,
                        index = list(data_ind),
                        data = list(data_a),
                        modified = F)

  # Generate model summary
  if ('summary' %in% skip) {
    msg('Skipping summary generation', quiet)
    summary <- NULL
  } else if (software == 'nlmixr') {
    summary <- summarise_nlmixr_model(obj, '', software, rounding = xp_theme$rounding, runname=runname)
  }

  # The weighted residuals are calculated by dividing the vector of each
  # individual's residuals (res_i) by the square root of the matrix of
  # covariances of that individual's data conditional on the population model:
  #
  #   WRES_i = RES_i / SQRT(COV(data_i | F_pop))
  #
  # This means that for each WRES calculated we include covariances between data
  # points of an individual.  If the correlations between some of these data
  # points are negative then the resulting WRES could also be negative, while
  # the RES could be positive.
  #
  # -Andy

  files <- NULL
  if(mtype=="SAEM") {
    tracedat <- tibble::as_tibble(as.data.frame(obj$par.hist))
    names(tracedat)[grep("iter", names(tracedat))] <-
      "ITERATION"

    files <- dplyr::tibble(name = deparse(substitute(obj)),
                           extension = 'ext',
                           problem = 1,
                           subprob = 0,
                           method = 'saem',
                           data = list(tracedat),
                           modified = FALSE)
  }

  # Label themes
  attr(gg_theme, 'theme') <- as.character(substitute(gg_theme))
  attr(xp_theme, 'theme') <- as.character(substitute(xp_theme))

  # Output xpose_data
  list(code = obj$uif, summary = summary, data = data,
       files = files, gg_theme = gg_theme, xp_theme = xp_theme,
       options = list(dir = NULL, quiet = quiet,
                      manual_import = NULL), software = 'nlmixr') %>%
    structure(class = c('xpose_data', 'uneval'))
}


