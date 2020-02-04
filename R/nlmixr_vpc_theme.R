#' Default VPC theme for 'xpose.nlmixr'
#'
#' @description Default VPC theme for 'xpose.nlmixr'.
#'
#' @return A list with 'vpc' theme specifiers.
#'
#' @importFrom vpc new_vpc_theme
#' @importFrom xpose theme_xp_default
#'
#' @export

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
                                            bin_separators_color = "#000000" ) )

##'@export
nlmixr::vpc
