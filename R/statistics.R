#' Hotelling's statistics (for two independent (small) samples)
#'
#' The function \code{get_hotellings()} estimates the parameters for Hotelling's
#' two-sample \eqn{T^2} statistic for small samples.
#'
#' @param m1 A matrix with the data of the reference group.
#' @param m2 A matrix with the same same dimensions as matrix \eqn{m1},
#'  with the data of the test group.
#' @inheritParams mimcr
#'
#' @details The two-sample Hotelling's \eqn{T^2} test statistic is given by
#'
#' \deqn{T^2 = \left( \bar{\bm{x}}_1 - \bar{\bm{x}}_2 \right)^{\top}
#'   \left( \bm{S}_p \left( \frac{1}{n_1} + \frac{1}{n_2} \right) \right)^{-1}
#'   \left( \bar{\bm{x}}_1 - \bar{\bm{x}}_2 \right) .}{%
#'   T^2 = (x.bar_1 - x.bar_2)^{\top} (S_p (1 / n_1 + 1 / n_2))^{-1}
#'   (x.bar_1 - x.bar_2) .}
#'
#' For large samples, this test statistic will be approximately chi-square
#' distributed with \eqn{p} degrees of freedom. However, this approximation
#' does not take into account the variation due to estimating the
#' variance-covariance matrix. Therefore, Hotelling's \eqn{T^2} statistic
#' is transformed into an \eqn{F}-statistic using the expression
#'
#' \deqn{F = \frac{n_1 + n_2 - p - 1}{(n_1 + n_2 - 2) p} T^2 ,}{%
#'   F = (n_1 + n_2 - p - 1) / ((n_1 + n_2 - 2) p) T^2 ,}
#'
#' where \eqn{n_1} and \eqn{n_2} are the sample sizes of the two samples being
#' compared and \eqn{p} is the number of variables.
#'
#' Under the null hypothesis, \eqn{H_0: \bm{\mu}_1 = \bm{\mu}_2}{%
#' H_0: \mu_1 = \mu_2}, this \eqn{F}-statistic will be \eqn{F}-distributed
#' with \eqn{p} and \eqn{n_1 + n_2 - p} degrees of freedom. \eqn{H_0} is
#' rejected at significance level \eqn{\alpha} if the \eqn{F}-value exceeds the
#' critical value from the \eqn{F}-table evaluated at \eqn{\alpha}, i.e.
#' \eqn{F > F_{p, n_1 + n_2 - p - 1, \alpha}}. The null hypothesis is satisfied
#' if, and only if, the population means are identical for all variables. The
#' alternative is that at least one pair of these means is different.
#'
#' The following assumptions concerning the data are made:
#' \itemize{
#' \item The data from population \eqn{i} is a sample from a population with
#'   mean vector \eqn{\mu_i}. In other words, it is assumed that there are no
#'   sub-populations.
#' \item The data from both populations have common variance-covariance matrix
#'   \eqn{\Sigma}.
#' \item The subjects from both populations are independently sampled.
#' \item Both populations are normally distributed.
#' }
#'
#' @return A list with the following elements is returned:
#' \item{Parameters}{Parameters determined for the estimation of
#'   Hotelling's \eqn{T^2}}
#' \item{S.pool}{The pooled variance-covariance matrix}
#' \item{covs}{A list with the elements \code{S.b1} and \code{S.b2}, the
#'   variance-covariance matrices of the reference and the test group}
#' \item{means}{A list with the elements \code{mean.b1}, \code{mean.b2} and
#'   \code{mean.diff}, the average profile values (for each time point) of
#'   the reference and the test group and the corresponding difference of
#'   the averages}
#'
#' The \code{Parameters} element contains the following information:
#' \item{DM}{The Mahalanobis distance of the samples}
#' \item{df1}{Degrees of freedom (number of variables or time points)}
#' \item{df2}{Degrees of freedom (number of rows - number of variables - 1)}
#' \item{alpha}{The provided significance level for the calculation of the
#'   confidence region}
#' \item{K}{Scaling factor for \eqn{F} to account for the distribution of the
#'   \eqn{T^2} statistic}
#' \item{k}{Scaling factor for the squared Mahalanobis distance to obtain
#'   the \eqn{T^2} statistic}
#' \item{T2}{Hotelling's \eqn{T^2} statistic (\eqn{F}-distributed)}
#' \item{F}{The observed \eqn{F} value}
#' \item{F.crit}{Critical \eqn{F} value}
#' \item{p.F}{\eqn{p} value for the Hotelling's \eqn{T^2} test statistic}
#'
#' @references
#' Hotelling, H. The generalisation of Student's ratio. \emph{Ann Math Stat}.
#' 1931; \strong{2}(3): 360-378.
#'
#' Hotelling, H. Multivariate Quality Control. In C. Eisenhart, M.W. Hastay,
#' and W.A. Wallis, eds. \emph{Techniques of Statistical Analysis}. 1947;
#' New York: McGraw-Hill.
#'
#' @seealso \code{\link{mimcr}}, \code{\link{get_sim_lim}}.
#'
#' @importFrom stats cov
#' @importFrom stats pf
#' @importFrom stats qf
#'
#' @export

get_hotellings <- function(m1, m2, signif) {
  if (!is.matrix(m1)) {
    stop("The sample m1 must be provided as matrix.")
  }
  if (!is.matrix(m2)) {
    stop("The sample m2 must be provided as matrix.")
  }
  if (!isTRUE(all.equal(dim(m1), dim(m2)))) {
    stop("The parameters m1 and m2 must have the same dimensions.")
  }
  if (signif <= 0 | signif > 1) {
    stop("Please specify signif as (0, 1]")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculation of various parameters

  # Number of profile time points (equal to sum(diag(solve(t_S) %*% t_S))) and
  # number of observations of the reference and test group
  n_tp <- ncol(m1)
  n_b1 <- n_b2 <- nrow(m1)

  # Covariance matrices of the reference and test group and their pooled
  # covariance matrix
  S_b1 <- cov(m1)
  S_b2 <- cov(m2)
  t_S <- ((n_b1 - 1) * S_b1 + (n_b2 - 1) * S_b2) / (n_b1 + n_b2 - 2)

  # Average dissolution at a given time point of the reference and test group
  # and the corresponding difference vector
  mean_b1 <- apply(X = m1, MARGIN = 2, FUN = mean)
  mean_b2 <- apply(X = m2, MARGIN = 2, FUN = mean)
  mean_diff <- mean_b2 - mean_b1

  # Mahalanobis distance (DM)
  DM <- sqrt(t(mean_diff) %*% solve(t_S) %*% mean_diff)

  # Degrees of freedom
  df1 <- n_tp
  df2 <- n_b1 + n_b2 - n_tp - 1

  # Scaling factors for the calculation of the Hotelling's T2 statistic
  k <- (n_b2 * n_b1) / (n_b2 + n_b1)
  K <- k * df2 / ((n_b2 + n_b1 - 2) * df1)

  # Hotelling's T2 statistic (general) and observed F value
  T2_value <- k * DM^2
  F_obs <- K * DM^2

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculation of critical F values

  # (1 - alpha) * 100th percentile of the F distribution with given degrees of
  # freedom
  F_crit <- qf(p = (1 - signif), df1 = df1, df2 = df2)

  # Probability of seeing something as or even more extreme than F_obs
  p_F <- 1 - pf(F_obs, df1 = df1, df2 = df2)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compilation of results

  t_res <- c(DM, df1, df2, signif, K, k, T2_value, F_obs, F_crit, p_F)
  names(t_res) <- c("DM", "df1", "df2", "signif", "K", "k",
                    "T2", "F", "F.crit", "p.F")

  l_res <- list(Parameters = t_res,
                S.pool = t_S,
                covs = list(S.b1 = S_b1,
                           S.b2 = S_b2),
                means = list(mean.b1 = mean_b1,
                             mean.b2 = mean_b2,
                             mean.diff = mean_diff))

  return(l_res)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Similarity limit
#'
#' The function \code{get_sim_lim()} estimates similarity limit in terms of
#' the \sQuote{Multivariate Statistical Distance} (MSD).
#'
#' @param lhs A list of the estimates of Hotelling's two-sample \eqn{T^2}
#'   statistic for small samples as returned by the function
#'   \code{\link{get_hotellings}}.
#' @inheritParams mimcr
#'
#' @details Details about the estimation of similarity limits in terms of
#' the \sQuote{Multivariate Statistical Distance} (MSD) are explained in
#' the corresponding section below.
#'
#' @inheritSection mimcr Similarity limits in terms of MSD
#'
#' @inheritSection mimcr T2 test for equivalence
#'
#' @return A vector containing the following information is returned:
#' \item{DM}{The Mahalanobis distance of the samples}
#' \item{df1}{Degrees of freedom (number of variables or time points)}
#' \item{df2}{Degrees of freedom (number of rows - number of variables - 1)}
#' \item{alpha}{The provided significance level for the calculation of the
#'   confidence region}
#' \item{K}{Scaling factor for \eqn{F} to account for the distribution of the
#'   \eqn{T^2} statistic}
#' \item{k}{Scaling factor for the squared Mahalanobis distance to obtain
#'   the \eqn{T^2} statistic}
#' \item{T2}{Hotelling's \eqn{T^2} statistic (\eqn{F}-distributed)}
#' \item{F}{The observed \eqn{F} value}
#' \item{ncp.Hoffelder}{Non-centrality parameter for calculation of the \eqn{F}
#'   statistic (\eqn{T^2} test procedure)}
#' \item{F.crit}{Critical \eqn{F} value (Tsong's procedure)}
#' \item{F.crit.Hoffelder}{Critical \eqn{F} value (\eqn{T^2} test procedure)}
#' \item{p.F}{\eqn{p} value for the Hotelling's \eqn{T^2} test statistic}
#' \item{p.F.Hoffelder}{\eqn{p} value for the Hotelling's \eqn{T^2} statistic
#'   based on the non-central \eqn{F} distribution}
#' \item{MTAD}{The specified \dQuote{maximum tolerable average difference}
#'   (MTAD) of the profiles of two formulations at each individual time point
#'   (in \%)}
#' \item{Sim.Limit}{The critical Mahalanobis distance or similarity limit
#'   (Tsong's procedure)}
#'
#' @references
#' Tsong, Y., Hammerstrom, T., Sathe, P.M., and Shah, V.P. Statistical
#' Assessment of Mean Differences Between Two Dissolution Data Sets.
#' \emph{Drug Inf J}. 1996; \strong{30}: 1105-1112.
#'
#' Wellek S. Testing Statistical Hypotheses of Equivalence and Noninferiority,
#' 2nd Edition, Chapman & Hall /CRC Press LLC, 2010.
#'
#' Hoffelder, T. Highly Variable Dissolution Profiles. Comparison of
#' \eqn{T^2}-Test for Equivalence and \eqn{f_2} Based Methods. \emph{Pharm Ind}.
#' 2016; \strong{78}(4): 587-592.\cr
#' \href{https://www.ecv.de/suse_item.php?suseId=Z|pi|8430}{LINK}
#' Accessed 22. September 2020.
#'
#' Hoffelder, T. EMA (2018) Q\&A on Mahalanobis distance (MD) to assess drug
#' dissolution profiles - Statistical critique \& demonstration of adequacy
#' of T2EQ approach (based on MD); 4th EFSPI Workshop on Regulatory Statistics;
#' September 23-24, 2019.\cr
#' \href{https://www.efspi.org/Documents/Events/Events%202019/Reg%20Stats/
#' Presentations/6_4_Hoffelder.pdf}{LINK}
#' Accessed 22. September 2020.
#'
#' @seealso \code{\link{mimcr}}, \code{\link{get_hotellings}}.
#'
#' @importFrom stats pf
#' @importFrom stats qf
#'
#' @export

get_sim_lim <- function(mtad, lhs) {
  if (!inherits(lhs, "list")) {
    stop("The parameter lhs must be a list returned by get_hotellings.")
  } else {
    if (sum(names(lhs) %in% c("Parameters", "S.pool", "covs", "means")) != 4) {
      stop("The parameter lhs must be a list returned by get_hotellings.")
    }
  }
  if (mtad <= 0 | mtad > 50) {
    stop("Please specify mtad as (0, 50]")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Similarity limit and critical F values

  hs <- lhs[[1]]

  # Global similarity limit D_crit determined according to Tsong 1996.
  # Note that D_glob is a vector of p * mtad specified as the global (or local)
  # similarity limit (in percent), i.e. the maximum tolerable average mean_diff
  # at all time points p.
  D_glob <- rep(mtad, times = hs["df1"])
  D_crit <- sqrt(t(D_glob) %*% solve(lhs[[2]]) %*% D_glob)

  # Non-centrality parameter that is based on  the equivalence region
  ncp_Hoffelder <- hs["k"] * D_crit^2

  # alpha * 100th percentile of the F distribution with given degrees of freedom
  # and the
  F_crit_Hoffelder <-
    qf(p = hs["signif"], df1 = hs["df1"], df2 = hs["df2"], ncp = ncp_Hoffelder)

  # Probability of seeing something as or even more extreme than F_obs, given
  # the degrees of freedom and the non-centrality parameter defined by the
  # equivalence region
  p_F_Hoffelder <-
    pf(hs["F"], df1 = hs["df1"], df2 = hs["df2"], ncp = ncp_Hoffelder)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compilation of results

  t_res <- c(hs["DM"], hs["df1"], hs["df2"], hs["signif"], hs["K"],
             hs["k"], hs["T2"], hs["F"], ncp_Hoffelder, hs["F.crit"],
             F_crit_Hoffelder, hs["p.F"], p_F_Hoffelder, mtad, D_crit)
  names(t_res) <- c("DM", "df1", "df2", "alpha", "K", "k", "T2",
                    "F", "ncp.Hoffelder", "F.crit", "F.crit.Hoffelder",
                    "p.F", "p.F.Hoffelder", "MTAD", "Sim.Limit")
  return(t_res)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Dissimilarity factor f1 for dissolution data
#'
#' The function \code{f1()} calculates the dissimilarity factor \eqn{f_1}.
#'
#' @param useEMA A string variable indicating if the dissimilarity factor
#'   \eqn{f_1} should be calculated following the EMA guideline \dQuote{On
#'   the investigation of bioequivalence} (\code{"yes"}, the default) or not
#'   (\code{"no"}), i.e. the recommendations concerning the similarity factor
#'   \eqn{f_2}. A third option is \code{"ignore"}. If \code{useEMA} is
#'   \code{"yes"} or \code{"no"} the appropriate profile portion is determined
#'   on the basis of the values of the parameters \code{lorellim} and
#'   \code{uprellim}. If it is \code{"ignore"}, the complete profiles are used
#'   as specified by the parameter \code{tcol}.
#' @inheritParams bootstrap_f2
#'
#' @details Similarity of dissolution profiles is often assessed using the
#' similarity factor \eqn{f_2}, as recommended by the EMA guideline (European
#' Medicines Agency 2010) \dQuote{On the investigation of bioequivalence}. The
#' evaluation of the similarity factor is based on the following constraints:
#' \enumerate{
#'   \item A minimum of three time points (zero excluded).
#'   \item The time points should be the same for the two formulations.
#'   \item Twelve individual values for every time point for each formulation.
#'   \item Not more than one mean value of > 85\% dissolved for any of the
#'     formulations.
#'   \item The relative standard deviation or coefficient of variation of any
#'     product should be less than 20\% for the first time point and less than
#'     10\% from the second to the last time point.
#' }
#'
#' The \emph{dis}similarity factor, or difference factor, \eqn{f_1}, is the
#' counterpart of the similarity factor \eqn{f_2}. The difference factor
#' \eqn{f_1} is a measure of the relative error between two curves. Current FDA
#' guidelines suggest that two profiles can be considered similar if \eqn{f_1}
#' is less than \eqn{15} (\eqn{0 - 15}) and \eqn{f_2} is greater than \eqn{50}
#' (\eqn{50 - 100}), which is equivalent to an average difference of 10\% at
#' all sampling time points. The dissimilarity factor \eqn{f_1} is calculated
#' by aid of the equation
#'
#' \deqn{f_1 = 100 \times \frac{\sum_{t=1}^{n} \left( \left| \bar{R}(t) -
#'   \bar{T}(t) \right| \right)}{\sum_{t=1}^{n} (\bar{R}(t))} .}{%
#'   f_1 = 100 (sum(abs(R.bar(t) - T.bar(t))) / sum(R.bar(t))) .}
#'
#' In this equation
#' \describe{
#'   \item{\eqn{f_1}}{is the dissimilarity factor,}
#'   \item{\eqn{n}}{is the number of time points,}
#'   \item{\eqn{\bar{R}(t)}{R.bar(t)}}{is the mean percent reference drug
#'       dissolved at time \eqn{t} after initiation of the study, and}
#'   \item{\eqn{\bar{T}(t)}{T.bar(t)}}{is the mean percent test drug dissolved
#'       at time \eqn{t} after initiation of the study.}
#' }
#'
#' @return A list with the following elements is returned:
#' \item{f1}{A numeric value representing the similarity factor \eqn{f_1}}
#' \item{Profile.TP}{A named numeric vector of the profile time points.}
#'
#' @references
#' United States Food and Drug Administration (FDA). Guidance for industry:
#' dissolution testing of immediate release solid oral dosage forms. 1997.\cr
#' \href{https://www.fda.gov/media/70936/download}{LINK}
#' Accessed 05. Mai 2019.
#'
#' United States Food and Drug Administration (FDA). Guidance for industry:
#' waiver of \emph{in vivo} bioavailability and bioequivalence studies for
#' immediate release solid oral dosage forms based on a biopharmaceutics
#' classification system. 2000.\cr
#' \href{https://www.fda.gov/media/70963/download}{LINK}
#' Accessed 05. Mai 2019.
#'
#' United States Food and Drug Administration (FDA). Guidance for industry:
#' immediate release solid oral dosage form: scale-up and post-approval
#' changes, chemistry, manufacturing and controls, \emph{in vitro} dissolution
#' testing, and \emph{in vivo} bioequivalence documentation (SUPAC-IR). 1995.\cr
#' \href{https://www.fda.gov/media/70949/download}{LINK}
#' Accessed 05. Mai 2019.
#'
#' European Medicines Agency (EMA), Committee for Medicinal Products for
#' Human Use (CHMP). Guideline on the Investigation of Bioequivalence. 2010;
#' CPMP/EWP/QWP/1401/98 Rev. 1.\cr
#' \href{http://www.ema.europa.eu/docs/en_GB/document_library/
#' Scientific_guideline/2010/01/WC500070039.pdf}{LINK}
#' Accessed 05. Mai 2019.
#'
#' @seealso \code{\link{f2}}.
#'
#' @example man/examples/examples_f1.R
#'
#' @export

f1 <- function(data, tcol, grouping, useEMA = "yes",
               lorellim = 1, uprellim = 85) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.numeric(tcol) | length(tcol) < 3) {
    stop("The parameter tcol must be an integer vector of at least length 3.")
  }
  if (!isTRUE(all.equal(tcol, as.integer(tcol)))) {
    stop("The parameter tcol must be an integer vector.")
  }
  if (min(tcol) < 1 | max(tcol) > ncol(data)) {
    stop("Some columns specified by tcol were not found in data frame.")
  }
  if (sum(grepl("\\d", colnames(data[, tcol]))) < length(tcol)) {
    stop("Some names of columns specified by tcol ",
         "do not contain numeric information.")
  }
  if (sum(vapply(data[, tcol], is.numeric, logical(1))) != length(tcol)) {
    stop("Some columns specified by tcol are not numeric.")
  }
  if (!is.character(grouping)) {
    stop("The parameter grouping must be string.")
  }
  if (!(grouping %in% colnames(data))) {
    stop("The grouping variable was not found in the provided data frame.")
  }
  if (!is.factor(data[, grouping])) {
    stop("The grouping variable's column in data must be a factor.")
  }
  if (!(useEMA %in% c("yes", "no", "ignore"))) {
    stop("Please specify useEMA either as \"yes\" or \"no\" or \"ignore\".")
  }
  if (lorellim < 0 | lorellim > uprellim) {
    stop("The variable lorellim must be single number >= 0 and < uprellim.")
  }
  if (uprellim > 100 | uprellim < lorellim) {
    stop("The variable uprellim must be a single number <= 100 and > lorellim.")
  }

  # <-><-><-><->

  data <- droplevels(data)

  if (nlevels(data[, grouping]) != 2) {
    stop("The number of levels in column ", grouping, " differs from 2.")
  }

  # <-><-><-><->

  # Generation of logical vector representing the reference and test group
  b1 <- make_grouping(data = data, grouping = grouping)

  # Check if the two groups have the same number of observations. If  not so,
  # and if the parameter useEMA is either "no" or "ignore", adjust the data
  # frames in such a way that both groups to be compared will have the same
  # number of observations and that the number of observations per group
  # corresponds to the largest common value (lcv) between number of observations
  # per group.
  # Note: an alternative would be to use the least common multiple.
  # Note that together with the data adjustment the b1 vector must be reset.

  if (useEMA == "yes" & sum(b1) != sum(!b1)) {
    stop("The two groups to be compared must have the same number of ",
         "observations.")
  }
  if (useEMA %in% c("no", "ignore") & sum(b1) != sum(!b1)) {
    warning("The two groups to be compared do not have the same number of ",
            "observations. Thus, the number of rows is adjusted according ",
            "to the largest common value between the number of observations ",
            "per group.")

    data <- balance_observations(data = data, groups = b1,
                                 n_obs = max(sum(b1), sum(!b1)))
    b1 <- make_grouping(data = data, grouping = grouping)
  }

  # Note: the following requirements prescribed by the EMA:
  # A minimum of three time points (zero excluded).
  # The time points should be the same for the two formulations.
  # Twelve individual values for every time point for each formulation.
  # Not more than one mean value > 85% dissolved for any of the formulations.
  # The relative standard deviation or coefficient of variation of any product
  # should be
  #   less than 20% for the first time point and
  #   less than 10% from the second to the last time point.

  ok <- get_profile_portion(data = data, tcol = tcol, groups = b1,
                            useEMA = useEMA,
                            lorellim = lorellim, uprellim = uprellim)

  if (useEMA == "yes" & sum(ok) < 3) {
    stop("According to EMA the profiles must comprise a minimum of 3 time ",
         "points. The actual profiles comprise ", sum(ok), " points only.")
  }
  if (sum(ok) < 3) {
    warning("The profiles should comprise a minimum of 3 time points. ",
            "The actual profiles comprise ", sum(ok), " points only.")
  }

  # <-><-><-><->
  # Extraction of information on the time points
  time_points <- get_time_points(svec = colnames(data)[tcol])
  time_points <- time_points[ok]

  # <-><-><-><->
  # Calculation of f1

  t_f1 <- get_f1(data = data, ins = seq_along(b1), tcol = tcol[ok],
                 grouping = grouping)

  # <-><-><-><->
  # Compilation of results

  return(list(f1 = t_f1,
              Profile.TP = time_points))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Get the f1 dissimilarity factor
#'
#' The function \code{get_f1()} calculates the dissimilarity factor \eqn{f_1}
#' for the assessment of dissolution profiles.
#'
#' @param data A data frame with the dissolution profile data in wide format.
#' @param ins A vector of indices generated regarding the grouping.
#' @inheritParams bootstrap_f2
#'
#' @inherit f1 details references
#'
#' @return The function returns the \eqn{f_1} value.
#'
#' @seealso \code{\link{get_f2}}.
#'
#' @keywords internal

get_f1 <- function(data, ins, tcol, grouping) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.numeric(ins) | length(ins) < 3 | length(ins) > nrow(data)) {
    stop("The parameter ins must be an integer vector not longer than ",
         "nrow(data).")
  }
  if (!isTRUE(all.equal(ins, as.integer(ins)))) {
    stop("The parameter ins must be an integer vector not longer than ",
         "nrow(data).")
  }
  if (!is.numeric(tcol) | length(tcol) < 3) {
    stop("The parameter tcol must be an integer vector of at least length 3.")
  }
  if (!isTRUE(all.equal(tcol, as.integer(tcol)))) {
    stop("The parameter tcol must be an integer vector.")
  }
  if (min(tcol) < 1 | max(tcol) > ncol(data)) {
    stop("Some columns specified by tcol were not found in data frame.")
  }
  if (sum(grepl("\\d", colnames(data[, tcol]))) < length(tcol)) {
    stop("Some names of columns specified by tcol ",
         "do not contain numeric information.")
  }
  if (sum(vapply(data[, tcol], is.numeric, logical(1))) != length(tcol)) {
    stop("Some columns specified by tcol are not numeric.")
  }
  if (!is.character(grouping)) {
    stop("The parameter grouping must be string.")
  }
  if (!(grouping %in% colnames(data))) {
    stop("The grouping variable was not found in the provided data frame.")
  }
  if (!is.factor(data[, grouping])) {
    stop("The grouping variable's column in data must be a factor.")
  }

  # <-><-><-><->

  data <- droplevels(data)

  if (nlevels(data[, grouping]) != 2) {
    stop("The number of levels in column ", grouping, " differs from 2.")
  }

  # <-><-><-><->

  n <- length(tcol)
  b1 <- make_grouping(data = data[ins, ], grouping = grouping)

  R_i <- apply(data[ins, ][b1, tcol], MARGIN = 2, FUN = mean)
  T_i <- apply(data[ins, ][!b1, tcol], MARGIN = 2, FUN = mean)

  ddelta_hat <- sum(abs(R_i - T_i))
  f1 <- 100 * ddelta_hat / sum(R_i)

  return(f1)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Similarity factor f2 for dissolution data
#'
#' The function \code{f2()} calculates the similarity factor \eqn{f_2}.
#'
#' @param useEMA A string variable indicating if the similarity factor \eqn{f_2}
#'   should be calculated following the EMA guideline \dQuote{On the
#'   investigation of bioequivalence} (\code{"yes"}, the default) or not
#'   (\code{"no"}). A third option is \code{"ignore"}. If \code{useEMA} is
#'   \code{"yes"} or \code{"no"} the appropriate profile portion is determined
#'   on the basis of the values of the parameters \code{lorellim} and
#'   \code{uprellim}. If it is \code{"ignore"}, the complete profiles are used
#'   as specified by the parameter \code{tcol}.
#' @inheritParams bootstrap_f2
#'
#' @details Similarity of dissolution profiles is assessed using the similarity
#' factor \eqn{f_2} according to the EMA guideline (European Medicines Agency
#' 2010) \dQuote{On the investigation of bioequivalence}. The evaluation of the
#' similarity factor is based on the following constraints:
#' \enumerate{
#'   \item A minimum of three time points (zero excluded).
#'   \item The time points should be the same for the two formulations.
#'   \item Twelve individual values for every time point for each formulation.
#'   \item NOt more than one mean value of > 85\% dissolved for any of the
#'     formulations.
#'   \item The relative standard deviation or coefficient of variation of any
#'     product should be less than 20\% for the first time point and less than
#'     10\% from the second to the last time point.
#' }
#'
#' The similarity factor \eqn{f_2} is calculated by aid of the equation
#'
#' \deqn{f_2 = 50 \times \log \left(\frac{100}{\sqrt{1 + \frac{\sum_{t=1}^{n}
#'   \left(\bar{R}(t) - \bar{T}(t) \right)^2}{n}}} \right) .}{%
#'   f_2 = 50 log(100 / (sqrt(1 + (sum((R.bar(t) - T.bar(t))^2) / n)))) .}
#'
#' In this equation
#' \describe{
#'   \item{\eqn{f_2}}{is the similarity factor,}
#'   \item{\eqn{n}}{is the number of time points,}
#'   \item{\eqn{\bar{R}(t)}{R.bar(t)}}{is the mean percent reference drug
#'       dissolved at time \eqn{t} after initiation of the study, and}
#'   \item{\eqn{\bar{T}(t)}{T.bar(t)}}{is the mean percent test drug dissolved
#'       at time \eqn{t} after initiation of the study.}
#' }
#'
#' Dissolution profiles are regarded as similar if the \eqn{f_2} value is
#' between \eqn{50} and \eqn{100}. \cr
#'
#' When the \eqn{f_2} statistic is not suitable, then the similarity may be
#' compared using model-dependent or model-independent methods. One often
#' encountered problem is that the \%CV constraint cannot be fulfilled. One
#' possible model-independent method is the bootstrap \eqn{f_2} method proposed
#' by Shah 1998. By repeated random sampling the distribution of \eqn{f_2} is
#' simulated to obtain an unbiased estimate of the expected value of \eqn{f_2}
#' and the variability of the underlying distribution. Other alternatives are
#' multivariate methods such as the method proposed by Tsong 1996 or by
#' Hoffelder 2016.
#'
#' @return A list with the following elements is returned:
#' \item{f2}{A numeric value representing the similarity factor \eqn{f_2}}
#' \item{Profile.TP}{A named numeric vector of the profile time points.}
#'
#' @references
#' United States Food and Drug Administration (FDA). Guidance for industry:
#' dissolution testing of immediate release solid oral dosage forms. 1997.\cr
#' \href{https://www.fda.gov/media/70936/download}{LINK}
#' Accessed 05. Mai 2019.
#'
#' United States Food and Drug Administration (FDA). Guidance for industry:
#' waiver of \emph{in vivo} bioavailability and bioequivalence studies for
#' immediate release solid oral dosage forms based on a biopharmaceutics
#' classification system. 2000.\cr
#' \href{https://www.fda.gov/media/70963/download}{LINK}
#' Accessed 05. Mai 2019.
#'
#' United States Food and Drug Administration (FDA). Guidance for industry:
#' immediate release solid oral dosage form: scale-up and post-approval
#' changes, chemistry, manufacturing and controls, \emph{in vitro} dissolution
#' testing, and \emph{in vivo} bioequivalence documentation (SUPAC-IR). 1995.\cr
#' \href{https://www.fda.gov/media/70949/download}{LINK}
#' Accessed 05. Mai 2019.
#'
#' European Medicines Agency (EMA), Committee for Medicinal Products for
#' Human Use (CHMP). Guideline on the Investigation of Bioequivalence. 2010;
#' CPMP/EWP/QWP/1401/98 Rev. 1.\cr
#' \href{http://www.ema.europa.eu/docs/en_GB/document_library/
#' Scientific_guideline/2010/01/WC500070039.pdf}{LINK}
#' Accessed 05. Mai 2019.
#'
#' Shah, V. P., Tsong, Y., Sathe, P. & Liu, J. P. \emph{In vitro} dissolution
#' profile comparison - statistics and analysis of the similarity factor,
#' \eqn{f_2}. \emph{Pharm Res}. 1998; \strong{15}(6): 889-896.
#'
#' Tsong, Y., Hammerstrom, T., Sathe, P.M., and Shah, V.P. Statistical
#' Assessment of Mean Differences Between Two Dissolution Data Sets.
#' \emph{Drug Inf J}. 1996; \strong{30}: 1105-1112.
#'
#' Hoffelder, T. Highly Variable Dissolution Profiles. Comparison of
#' \eqn{T^2}-Test for Equivalence and \eqn{f_2} Based Methods. \emph{Pharm Ind}.
#' 2016; \strong{78}(4): 587-592.\cr
#' \href{https://www.ecv.de/suse_item.php?suseId=Z|pi|8430}{LINK}
#' Accessed 22. September 2020.
#'
#' @seealso \code{\link{f1}}.
#'
#' @example man/examples/examples_f2.R
#'
#' @export

f2 <- function(data, tcol, grouping, useEMA = "yes",
               lorellim = 1, uprellim = 85) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.numeric(tcol) | length(tcol) < 3) {
    stop("The parameter tcol must be an integer vector of at least length 3.")
  }
  if (!isTRUE(all.equal(tcol, as.integer(tcol)))) {
    stop("The parameter tcol must be an integer vector.")
  }
  if (min(tcol) < 1 | max(tcol) > ncol(data)) {
    stop("Some columns specified by tcol were not found in data frame.")
  }
  if (sum(grepl("\\d", colnames(data[, tcol]))) < length(tcol)) {
    stop("Some names of columns specified by tcol ",
         "do not contain numeric information.")
  }
  if (sum(vapply(data[, tcol], is.numeric, logical(1))) != length(tcol)) {
    stop("Some columns specified by tcol are not numeric.")
  }
  if (!is.character(grouping)) {
    stop("The parameter grouping must be string.")
  }
  if (!(grouping %in% colnames(data))) {
    stop("The grouping variable was not found in the provided data frame.")
  }
  if (!is.factor(data[, grouping])) {
    stop("The grouping variable's column in data must be a factor.")
  }
  if (!(useEMA %in% c("yes", "no", "ignore"))) {
    stop("Please specify useEMA either as \"yes\" or \"no\" or \"ignore\".")
  }
  if (lorellim < 0 | lorellim > uprellim) {
    stop("The variable lorellim must be single number >= 0 and < uprellim.")
  }
  if (uprellim > 100 | uprellim < lorellim) {
    stop("The variable uprellim must be a single number <= 100 and > lorellim.")
  }

  # <-><-><-><->

  data <- droplevels(data)

  if (nlevels(data[, grouping]) != 2) {
    stop("The number of levels in column ", grouping, " differs from 2.")
  }

  # <-><-><-><->

  # Generation of logical vector representing the reference and test group
  b1 <- make_grouping(data = data, grouping = grouping)

  # Check if the two groups have the same number of observations. If  not so,
  # and if the parameter useEMA is either "no" or "ignore", adjust the data
  # frames in such a way that both groups to be compared will have the same
  # number of observations and that the number of observations per group
  # corresponds to the largest common value (lcv) between number of observations
  # per group.
  # Note: an alternative would be to use the least common multiple.
  # Note that together with the data adjustment the b1 vector must be reset.

  if (useEMA == "yes" & sum(b1) != sum(!b1)) {
    stop("The two groups to be compared must have the same number of ",
         "observations.")
  }
  if (useEMA %in% c("no", "ignore") & sum(b1) != sum(!b1)) {
    warning("The two groups to be compared do not have the same number of ",
            "observations. Thus, the number of rows is adjusted according ",
            "to the largest common value between the number of observations ",
            "per group.")

    data <- balance_observations(data = data, groups = b1,
                                 n_obs = max(sum(b1), sum(!b1)))
    b1 <- make_grouping(data = data, grouping = grouping)
  }

  # Note: the following requirements prescribed by the EMA:
  # A minimum of three time points (zero excluded).
  # The time points should be the same for the two formulations.
  # Twelve individual values for every time point for each formulation.
  # Not more than one mean value > 85% dissolved for any of the formulations.
  # The relative standard deviation or coefficient of variation of any product
  # should be
  #   less than 20% for the first time point and
  #   less than 10% from the second to the last time point.

  ok <- get_profile_portion(data = data, tcol = tcol, groups = b1,
                            useEMA = useEMA,
                            lorellim = lorellim, uprellim = uprellim)

  if (useEMA == "yes" & sum(ok) < 3) {
    stop("According to EMA the profiles must comprise a minimum of 3 time ",
         "points. The actual profiles comprise ", sum(ok), " points only.")
  }
  if (sum(ok) < 3) {
    warning("The profiles should comprise a minimum of 3 time points. ",
            "The actual profiles comprise ", sum(ok), " points only.")
  }

  # <-><-><-><->
  # Extraction of information on the time points
  time_points <- get_time_points(svec = colnames(data)[tcol])
  time_points <- time_points[ok]

  # <-><-><-><->
  # Calculation of f2

  t_f2 <- get_f2(data = data, ins = seq_along(b1), tcol = tcol[ok],
                 grouping = grouping)

  # <-><-><-><->
  # Compilation of results

  return(list(f2 = t_f2,
              Profile.TP = time_points))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Get the f2 similarity factor
#'
#' The function \code{get_f2()} calculates the similarity factor \eqn{f_2} for
#' the assessment of dissolution profiles.
#'
#' @param data A data frame with the dissolution profile data in wide format.
#' @param ins A vector of indices generated regarding the grouping.
#' @inheritParams bootstrap_f2
#'
#' @inherit f2 details references
#'
#' @return The function returns the \eqn{f_2} value.
#'
#' @seealso \code{\link{get_f1}}.
#'
#' @keywords internal

get_f2 <- function(data, ins, tcol, grouping) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.numeric(ins) | length(ins) < 3 | length(ins) > nrow(data)) {
    stop("The parameter ins must be an integer vector not longer than ",
         "nrow(data).")
  }
  if (!isTRUE(all.equal(ins, as.integer(ins)))) {
    stop("The parameter ins must be an integer vector not longer than ",
         "nrow(data).")
  }
  if (!is.numeric(tcol) | length(tcol) < 3) {
    stop("The parameter tcol must be an integer vector of at least length 3.")
  }
  if (!isTRUE(all.equal(tcol, as.integer(tcol)))) {
    stop("The parameter tcol must be an integer vector.")
  }
  if (min(tcol) < 1 | max(tcol) > ncol(data)) {
    stop("Some columns specified by tcol were not found in data frame.")
  }
  if (sum(grepl("\\d", colnames(data[, tcol]))) < length(tcol)) {
    stop("Some names of columns specified by tcol ",
         "do not contain numeric information.")
  }
  if (sum(vapply(data[, tcol], is.numeric, logical(1))) != length(tcol)) {
    stop("Some columns specified by tcol are not numeric.")
  }
  if (!is.character(grouping)) {
    stop("The parameter grouping must be string.")
  }
  if (!(grouping %in% colnames(data))) {
    stop("The grouping variable was not found in the provided data frame.")
  }
  if (!is.factor(data[, grouping])) {
    stop("The grouping variable's column in data must be a factor.")
  }

  # <-><-><-><->

  data <- droplevels(data)

  if (nlevels(data[, grouping]) != 2) {
    stop("The number of levels in column ", grouping, " differs from 2.")
  }

  # <-><-><-><->

  n <- length(tcol)
  b1 <- make_grouping(data = data[ins, ], grouping = grouping)

  R_i <- apply(data[ins, ][b1, tcol], MARGIN = 2, FUN = mean)
  T_i <- apply(data[ins, ][!b1, tcol], MARGIN = 2, FUN = mean)

  ddelta_hat <- 1 + sum((R_i - T_i)^2) / n
  f2 <- 50 * log10(100 / sqrt(ddelta_hat))

  return(f2)
}

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
