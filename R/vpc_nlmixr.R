#' Create a visual predictive check (VPC) from an 'nlmixr' model
#'
#' @description Create a visual predictive check (VPC) from an 'nlmixr' model.
#'
#' @param obj nlmixr fit object to be used as the basis for the VPC.
#' @param dat Data file to use as the basis for simulations.
#' @param nsim Number of new datasets to simulate (default is 400).
#' @param by Variable by which to stratify the VPC. Currently not used.
#' @param ... Additional arguments to be passed to the \code{\link[vpc]{vpc}} functions.
#'
#' @importFrom vpc vpc new_vpc_theme
#' @importFrom xpose theme_xp_default
#'
#' @examples
#' \dontrun{
#' vpc1 <- vpc_nlmixr(as.saem(myfit), dat, nsim=400, show=list(obs_dv=T),
#' lloq=0, n_bins = 16, vpc_theme = nlmixr_vpc_theme)
#' }
#'
#' @export


vpc_nlmixr = function(obj, dat, nsim = 400, by=NULL, ...) {

  EVID <- NULL

  multi2 = function (mu, vmat, n)
  {
    eta <- matrix(stats::rnorm(length(mu) * n), ncol = n, nrow = length(mu))
    Q <- chol(vmat, pivot = TRUE)
    pivot <- attr(Q, "pivot")
    oo <- order(pivot)
    para <- t(Q[, oo]) %*% eta
    sweep(para, 1, mu, "+")
  }

  rmvnorm = function(n, mu, vmat) multi2(mu, vmat, n)

  saem.cfg = attr(obj, "saem.cfg")
  dopred <- attr(obj, "dopred")
  red.mod = sum((obj$sig2 != 0) * 1:2)

  if (!is.null(by)) {
    if (by %in% names(dat)) {
      dat$grp = eval(parse(text=paste0("dat$",by)))
    } else {
      msg = paste0(by, " not found in data")
      stop(msg)
    }
  }
  else dat$grp = T

  xd = subset(dat, EVID==0)
  nsub = length(unique(xd$ID))
  ntim = dim(xd)[1]
  ord=rep(1:ntim, nsim)
  sim=rep(1:nsim, each=ntim)

  s = lapply(1:nsim, function(k) {
    mpost_rand = t(rmvnorm(nsub, obj$Plambda, obj$Gamma2_phi1))
    p = dopred(mpost_rand, saem.cfg$evt, saem.cfg$opt)
    if      (red.mod==1) res = rnorm(ntim,0,sqrt(obj$sig2[1]))
    else if (red.mod==2) res = p*obj$sig2[2]*rnorm(ntim,0,1)
    else if (red.mod==3) res = (obj$sig2[1]+p*obj$sig2[2])*rnorm(ntim,0,1)
    p+res
  })
  xs = do.call("cbind",s)

  df = cbind(xd[ord, c("ID", "TIME", "grp")], DV=as.vector(xs), SIM=sim)
  df


  vpc::vpc(sim = df, obs = dat, ...)
}

nlmixr_vpc_theme <- vpc::new_vpc_theme(list(obs_color = "#1F4E79",
                                            obs_size = 1,
                                            obs_median_color = "#1F4E79",
                                            obs_median_linetype = "solid",
                                            obs_median_size = 1,
                                            obs_alpha = .7,
                                            obs_shape = 1,
                                            obs_ci_color = "#1F4E79",
                                            obs_ci_linetype = "dashed",
                                            obs_ci_size = .5,
                                            sim_pi_fill = "#C00000",
                                            sim_pi_alpha = 0.15,
                                            sim_pi_color = "#1F4E79",
                                            sim_pi_linetype = 'dotted',
                                            sim_pi_size = 1,
                                            sim_median_fill = "#C00000",
                                            sim_median_alpha = 0.15,
                                            sim_median_color = "#1F4E79",
                                            sim_median_linetype = "dashed",
                                            sim_median_size = 1,
                                            bin_separators_color = "#000000" )  )
