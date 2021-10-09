#' Model-independent multivariate confidence region (MIMCR) procedure
#'
#' The function \code{mimcr()} assesses the equivalence of highly variable
#' dissolution profiles. It does so by applying different methods proposed in
#' the literature, i.e. it implements the non-parametric
#' \sQuote{Model-Independent Multivariate Confidence Region} (MIMCR) procedure,
#' including the \eqn{T^2} test for equivalence of dissolution data as proposed
#' by Hoffelder (2016).
#'
#' @param data A data frame with the dissolution profile data in wide format.
#'   Since the information on the time points of dissolution testing is
#'   extracted from the corresponding column names, they must contain
#'   extractable numeric information. For example, from the column names
#'   \code{(t_0, t_5, t_10)} the time points are determined to be
#'   \code{(0, 5, 10)}.
#' @param tcol A vector of indices specifying the columns in \code{data} that
#'   contain the \% release values. The length of \code{tcol} must be three or
#'   longer.
#' @param grouping A string variable specifying the column in \code{data} that
#'   contains the group names (i.e. a factorial variable, e.g., for the
#'   differentiation of batches or formulations of a drug product).
#' @param fit_n_obs A logical value specifying if the number of rows per level
#'   in the  column specified by \code{grouping} parameter should be adjusted
#'   to be equal given that they are not equal. The default is \code{FALSE}
#'   because each group should have the same number of observations. If
#'   \code{fit_n_obs} is \code{TRUE}, redundant observations from the level
#'   with more observations are dropped, i.e. only the observations \eqn{1}
#'   to the number of observations of the level with the fewer observations
#'   will be used for the comparison of the two groups.
#' @param mtad A numeric value specifying the \dQuote{maximum tolerable average
#'   difference} (MTAD) of the profiles of two formulations at each individual
#'   time point (in \%). The default value is \code{10}. It determines the the
#'   size of the similarity limit \eqn{\bm{d}_g}{d_g}.
#' @param signif A positive numeric value between 0 and 1 specifying the
#'   significance level for the calculation of the \sQuote{Confidence Region}
#'   (CR). The coverage of CR is \eqn{(1 - signif) 100}\%. The default value
#'   is \code{0.05}.
#' @param max_trial A positive integer specifying the maximum number of
#'   Newton-Raphson search rounds to be performed.
#' @param lorellim A numeric value which specifies the lower limit for the
#'   release in \%. Observations smaller than this value are ignored. The
#'   default is \code{1}.
#' @param uprellim A numeric value which specifies the upper limit for the
#'   release in \%. Observations greater than this value are ignored. The
#'   default is \code{85}.
#' @param tol A non-negative numeric specifying the accepted minimal
#'   difference between two consecutive search rounds.
#'
#' @details The function \code{mimcr()} assesses the equivalence of highly
#' variable dissolution profiles by aid of a \dQuote{Model-Independent
#' Multivariate Confidence Region} (MIMCR) procedure as proposed by Tsong et al.
#' (1996) and by aid of a \dQuote{T2 test for equivalence} as proposed by
#' Hoffelder (2016).
#'
#' For details see the sections \dQuote{Comparison of highly variable
#' dissolution profiles}, \dQuote{Similarity limits in terms of MSD} and
#' \dQuote{T2 test for equivalence} below.
#'
#' @section Comparison of highly variable dissolution profiles:
#' When comparing the dissolution data of a post-approval change product and a
#' reference approval product, the goal is to assess the similarity between the
#' mean dissolution values at the observed sample time points. A widely used
#' method is the \eqn{f_2} method that was introduced by Moore & Flanner (1996).
#' Similarity testing criteria based on \eqn{f_2} can be found in several FDA
#' guidelines and in the guideline of the European Medicines Agency (EMA)
#' \dQuote{On the investigation of bioequivalence} (EMA 2010).
#'
#' In situations where within-batch variation is greater than 15\%, FDA
#' guidelines recommend use of a multivariate confidence interval as an
#' alternative to the \eqn{f_2} method. This can be done using the following
#' stepwise procedure:
#' \enumerate{
#' \item Establish a similarity limit in terms of \sQuote{Multivariate
#'   Statistical Distance} (MSD) based on inter-batch differences in \% drug
#'   release from reference (standard approved) formulations, i.e. the so-
#'   called \sQuote{Equivalence Margin} (EM).
#' \item Calculate the MSD between test and reference mean dissolutions.
#' \item Estimate the 90\% confidence interval (CI) of the true MSD as
#'   determined in step 2.
#' \item Compare the upper limit of the 90\% CI with the similarity limit
#'   determined in step 1. The test formulation is declared to be similar to
#'   the reference formulation if the upper limit of the 90\% CI is less than
#'   or equal to the similarity limit.
#' }
#'
#' @section Similarity limits in terms of MSD:
#' For calculating the MSD, the procedure proposed by Tsong et al. (1996) can
#' be considered as well-accepted method that is actually recommended by the
#' FDA. According to this method, a multivariate statistical distance, called
#' Mahalanobis distance, is used to measure the difference between two
#' multivariate means. This distance measure is calculated as
#'
#' \deqn{D_M = \sqrt{ \left( \bm{x_T} - \bm{x_R} \right)^{\top}
#'   \bm{S}_{pooled}^{-1} \left( \bm{x_T} - \bm{x_R} \right)} ,}{%
#'   D_M = sqrt((x_T - x_R)^{\top} S_{pooled}^{-1} (x_T - x_R)) ,}
#'
#' where \eqn{\bm{S}_{pooled} = \frac{\left( \bm{S}_T + \bm{S}_R \right)}{2}}{%
#' S_{pooled} = (S_T + S_R) / 2} is the sample variance-covariance matrix
#' pooled across the batches, \eqn{\bm{x}_T}{x_T} and \eqn{\bm{x}_R}{x_R} are
#' the vectors of the sample means for the test (\eqn{T}) and reference
#' (\eqn{R}) profiles, and \eqn{\bm{S}_T}{S_T} and \eqn{\bm{S}_R}{x_R} are the
#' variance-covariance matrices of the test and reference profiles.
#'
#' In order to determine the similarity limits in terms of the
#' \sQuote{Multivariate Statistical Distance} (MSD), i.e. the Mahalanobis
#' distance between the two multivariate means of the dissolution profiles
#' of the formulations to be compared, Tsong et al. (1996) proposed to use
#' the equation
#'
#' \deqn{D_M^{max} = \sqrt{ \bm{d}_g^{\top} \bm{S}_{pooled}^{-1} \bm{d}_g} ,}{%
#'   D_M^{max} = sqrt(d_g^{\top} S_{pooled}^{-1} d_g) ,}
#'
#' where \eqn{\bm{d}_g}{d_g} is a \eqn{1 \times p}{1 x p} vector with all
#' \eqn{p} elements equal to an empirically defined limit \eqn{\bm{d}_g}{d_g},
#' e.g., \eqn{15}\%, for the maximum tolerable difference at all time points,
#' and \eqn{p} is the number of sampling points. By assuming that the data
#' follow a multivariate normal distribution, the 90\% confidence region
#' (\eqn{CR}) for the true difference between the mean vectors,
#' \eqn{\bm{\mu}_T - \bm{\mu}_R}{\mu_T - \mu_R}, can be computed for the
#' resultant vector \eqn{\bm{\mu}}{\mu} to satisfy the following condition:
#'
#' \deqn{\bm{CR} = K \left( \bm{\mu} - \left( \bm{x_T} - \bm{x_R} \right)
#'   \right)^{\top} \bm{S}_{pooled}^{-1} \left( \bm{\mu} - \left( \bm{x_T} -
#'   \bm{x_R} \right) \right) \leq F_{p, n_T + n_R - p - 1, 0.9} ,}{%
#'   CR = sqrt((\mu - (x_T - x_R))^{\top} S_{pooled}^{-1} (\mu - (x_T - x_R)))
#'   \leq F_{p, n_T + n_R - p - 1, 0.9} ,}
#'
#' where \eqn{K} is the scaling factor that is calculated as
#'
#' \deqn{K = \frac{n_T n_R}{n_T + n_R} \cdot \frac{n_T + n_R - p - 1}{
#'   \left( n_T + n_R - 2 \right) \cdot p} ,}{%
#'   (n_T n_R) / (n_T + n_R) * (n_T + n_R - p - 1) / ((n_T + n_R - 2) p) ,}
#'
#' and \eqn{F_{p, n_T + n_R - p - 1, 0.9}} is the \eqn{90^{th}} percentile of
#' the \eqn{F} distribution with degrees of freedom \eqn{p} and
#' \eqn{n_T + n_R - p - 1}. It is obvious that \eqn{(n_T + n_R)} must be greater
#' than \eqn{(p + 1)}. The formula for \eqn{CR} gives a \eqn{p}-variate 90\%
#' confidence region for the possible true differences. \cr
#'
#' @section T2 test for equivalence:
#' Based on the distance measure for profile comparison that was suggested by
#' Tsong et al. (1996), i.e. the Mahalanobis distance, Hoffelder (2016) proposed
#' a statistical equivalence procedure for that distance, the so-called
#' \eqn{T^2} test for equivalence (T2EQ). It is used to demonstrate that the
#' Mahalanobis distance between reference and test group dissolution profiles
#' is smaller than EM. Decision in favour of equivalence is taken if the \eqn{p}
#' value of this test statistic is smaller than the pre-specified significance
#' level, i.e. if \eqn{p < \alpha}. The \eqn{p} value is calculated by aid of
#' the formula
#'
#' \deqn{p = F_{p, n_T + n_R - p - 1, ncp, \alpha}
#'   \frac{n_T + n_R - p - 1}{(n_T + n_R - 2) p} T^2 ,}{%
#'   p = F_{p, n_T + n_R - p - 1, ncp, \alpha}
#'   (n_T + n_R - p - 1) / ((n_T + n_R - 2) p) T^2 ,}
#'
#' where \eqn{\alpha} is the significance level and \eqn{ncp} is the so-called
#' \sQuote{\emph{non-centrality parameter}} that is calculated by
#'
#' \deqn{\frac{n_T n_R}{n_T + n_R} \left( D_M^{max} \right)^2 .}{%
#'   (n_T n_R) / (n_T + n_R) (D_M^{max})^2 .}
#'
#' The test statistic being used is Hotelling's \eqn{T^2} that is given as
#'
#' \deqn{T^2 = \frac{n_T n_R}{n_T + n_R} \left( \bm{x_T} - \bm{x_R}
#'   \right)^{\top} \bm{S}_{pooled}^{-1} \left( \bm{x_T} - \bm{x_R} \right) .}{%
#'   (n_T n_R) / (n_T + n_R) * (x_T - x_R)^{\top} S_{pooled}^{-1} (x_T - x_R) .}
#'
#' As mentioned previously, \eqn{\bm{d}_g}{d_g} is a \eqn{1 \times p}{1 x p}
#' vector with all \eqn{p} elements equal to an empirically defined limit
#' \eqn{d_g}. Thus, the components of the vector \eqn{\bm{d}_g}{d_g} can be
#' interpreted as upper bound for a kind of \sQuote{\emph{average}} allowed
#' difference between test and reference profiles, the \sQuote{\emph{global
#' similarity limit}}. Since the EMA requires that \dQuote{similarity acceptance
#' limits should be pre-defined and justified and not be greater than a 10\%
#' difference}, it is recommended to use 10\%, not 15\% as proposed by Tsong
#' et al., for the maximum tolerable difference at all time points.
#'
#' @return An object of class \code{mimcr} is returned, containing the following
#' list elements:
#' \item{Similarity}{Conclusion concerning similarity}
#' \item{Parameters}{Parameters calculated during the assessment}
#' \item{NR.CI}{List with results from the Newton-Raphson (NR) search}
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
#' \item{Obs.L}{Observed lower limit (Tsong's procedure)}
#' \item{Obs.U}{Observed upper limit (Tsong's procedure)}
#'
#' The \code{NR.CI} element contains the following information:
#' \item{CI}{A matrix of the points on the \eqn{CR} bounds for each time point}
#' \item{converged}{A logical specifying if the NR algorithm converged or not}
#' \item{n.trial}{The number of trials until convergence}
#' \item{max.trial}{The maximal number of trials}
#' \item{Warning}{A warning message, if applicable, or otherwise NULL}
#' \item{Error}{An error message, if applicable, or otherwise NULL}
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
#'   Scientific_guideline/2010/01/WC500070039.pdf}{LINK}
#' Accessed 05. Mai 2019.
#'
#' Tsong, Y., Hammerstrom, T., Sathe, P.M., and Shah, V.P. Statistical
#' Assessment of Mean Differences Between Two Dissolution Data Sets.
#' \emph{Drug Inf J}. 1996; \strong{30}: 1105-1112.
#'
#' Tsong, Y., Hammerstrom, T., and Chen, J.J. Multipoint dissolution
#' specification and acceptance sampling rule based on profile modeling and
#' principal component analysis. \emph{J Biopharm Stat}. 1997; \strong{7}(3):
#' 423-439.
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
#' @seealso \code{\link{gep_by_nera}}, \code{\link{bootstrap_f2}},
#'   \code{\link{mztia}},
#'   \code{\link[T2EQ]{T2EQ.dissolution.profiles.hoffelder}}.
#'
#' @example man/examples/examples_mimcr.R
#'
#' @importFrom stats pf
#' @importFrom stats qf
#'
#' @export

mimcr <- function(data, tcol, grouping, fit_n_obs = FALSE, mtad = 10,
                  signif = 0.05, max_trial = 50, lorellim = 1, uprellim = 85,
                  tol = 1e-9) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.numeric(tcol) | length(tcol) < 2) {
    stop("The parameter tcol must be an integer vector of at least length 2.")
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
  if (!is.logical(fit_n_obs) | length(fit_n_obs) != 1) {
    stop("The parameter fit_n_obs must be a logical of length 1.")
  }
  if (mtad <= 0 | mtad > 50) {
    stop("Please specify mtad as (0, 50]")
  }
  if (signif <= 0 | signif > 1) {
    stop("Please specify signif as (0, 1]")
  }
  if (!is.numeric(max_trial) | length(max_trial) > 1) {
    stop("The parameter max_trial must be a positive integer of length 1.")
  }
  if (max_trial != as.integer(max_trial)) {
    stop("The parameter max_trial must be a positive integer of length 1.")
  }
  if (max_trial < 0) {
    stop("The parameter max_trial must be a positive integer of length 1.")
  }
  if (lorellim < 0 | lorellim > uprellim) {
    stop("The variable lorellim must be single number >= 0 and < uprellim.")
  }
  if (uprellim > 100 | uprellim < lorellim) {
    stop("The variable uprellim must be a single number <= 100 and > lorellim.")
  }
  if (!is.numeric(tol) | length(tol) > 1) {
    stop("The parameter tol must be a non-negative numeric value of length 1.")
  }
  if (tol < 0) {
    stop("The parameter tol must be a non-negative numeric value of length 1.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  data <- droplevels(data)

  if (nlevels(data[, grouping]) != 2) {
    stop("The number of levels in column ", grouping, " differs from 2.")
  }

  # Generation of logical vector representing the reference and test group
  b1 <- make_grouping(data = data, grouping = grouping)

  # Check if the two groups have the same number of observations. If  not so,
  # adjust the data frame in such a way that both groups to be compared will
  # have the same number of observations and that the number of observations
  # per group corresponds to the number of observations of the group with the
  # fewer observations (mno: minimal number of observations).

  if (fit_n_obs && sum(b1) != sum(!b1)) {
    warning("Rows from the group with redundant observations are dropped.")

    mno <- min(sum(b1), sum(!b1))

    slctn1 <- 1:mno + (which(b1)[1] - 1)
    slctn2 <- 1:mno + (which(!b1)[1] - 1)

    # Together with the data adjustment the b1 vector must be reset.
    data <- rbind(data[slctn1, ], data[slctn2, ])
    b1 <- make_grouping(data = data, grouping = grouping)
  }

  if (length(data[b1, grouping]) != length(data[!b1, grouping])) {
    stop("The treatments to be tested must have the same number of rows.")
  }

  # <-><-><->

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
                            useEMA = "no", lorellim = lorellim,
                            uprellim = uprellim)

  if (sum(ok) < 3) {
    warning("The profiles should comprise a minimum of 3 time points. ",
            "The actual profiles comprise ", sum(ok), " points only.")
  }

  # <-><-><->
  # Extraction of information on the time points
  time_points <- get_time_points(svec = colnames(data)[tcol])

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of MSD and similarity assessment

  # Hotelling's T2 statistics
  l_hs <- get_hotellings(m1 = as.matrix(data[b1, tcol[ok]]),
                         m2 = as.matrix(data[!b1, tcol[ok]]),
                         signif = signif)

  # Similarity limit and critical F values
  t_sl <- get_sim_lim(mtad, l_hs)

  # Similarity conclusion based on Hoffelder's p value
  if (t_sl["p.F.Hoffelder"] < signif) {
    conclusion_Hoffelder <- "Similar"
  } else {
    conclusion_Hoffelder <- "Dissimilar"
  }

  # Compilation of results
  t_res <- c(t_sl, NA, NA)
  names(t_res)[(length(t_res) - 1):length(t_res)] <- c("Obs.L", "Obs.U")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of points on the confidence region (CR) according
  # to Tsong 1996 and checking if these points are within the global
  # similarity limit D_glob

  tmp <- rep(1, times = t_sl["df1"] + 1)
  tmp <- try_get_model(
    gep_by_nera(n_p = as.numeric(t_sl["df1"]), K = as.numeric(t_sl["K"]),
                mean_diff = l_hs[["means"]][["mean.diff"]],
                S_pool = l_hs[["S.pool"]], F_crit = as.numeric(t_sl["F.crit"]),
                y = tmp, max_trial = max_trial, tol = tol))

  if (!is.null(tmp[["Error"]]) | !is.null(tmp[["Warning"]])) {
      CI_NR <- cbind(LCL = rep(NA, times = t_sl["df1"]),
                     UCL = rep(NA, times = t_sl["df1"]))
      rownames(CI_NR) <- colnames(data[, tcol[ok]])

      # Similarity conclusion based on Tsong's D_crit
      conclusion_Tsong <- "Dissimilar"
    } else {
      y_b1 <- tmp[["Model"]][["points"]]

      # The points at the ellipse's opposite side are obtained by subtraction.
      y_b2 <- l_hs[["means"]][["mean.diff"]] +
        (l_hs[["means"]][["mean.diff"]] - y_b1[1:t_sl["df1"]])

      # At this point, it has to be checked if the T2 statistic of the found
      # data points, compared with the mean difference, is equal to the critical
      # F value. If not so, the found points are not located on the confidence
      # region boundary.

      kdvd <-
        t_sl["K"] * t(y_b1[1:t_sl["df1"]] - l_hs[["means"]][["mean.diff"]]) %*%
        solve(l_hs[["S.pool"]]) %*%
        (y_b1[1:t_sl["df1"]] - l_hs[["means"]][["mean.diff"]])

      if (round(kdvd, tol) != round(t_sl["F.crit"], tol)) {
        CI_NR <- cbind(LCL = rep(NA, times = t_sl["df1"]),
                       UCL = rep(NA, times = t_sl["df1"]))
        rownames(CI_NR) <- colnames(data[, tcol[ok]])

        # Similarity conclusion based on Tsong's D_crit
        conclusion_Tsong <- "Dissimilar"

        warning("The points found by the Newton-Raphson search are not ",
                "located on the confidence region boundary.")
      } else {
        # If it has been confirmed that the found data points are located on
        # the confidence boundary, the corresponding Mahalanobis distances are
        # calculated. Then it is checked if the longer distance is smaller than
        # the global similarity limit D_crit.

        MD1 <- sqrt(t(y_b1[1:t_sl["df1"]]) %*% solve(l_hs[["S.pool"]]) %*%
                      y_b1[1:t_sl["df1"]])
        MD2 <- sqrt(t(y_b2[1:t_sl["df1"]]) %*% solve(l_hs[["S.pool"]]) %*%
                      y_b2[1:t_sl["df1"]])

        t_res[length(t_res) - 1] <- min(MD1, MD2)
        t_res[length(t_res)] <- max(MD1, MD2)

        # Similarity conclusion based on Tsong's D_crit
        if (t_res[length(t_res)] < t_sl["Sim.Limit"]) {
          conclusion_Tsong <- "Similar"
        } else {
          conclusion_Tsong <- "Dissimilar"
        }

        if (MD1 < MD2) {
          CI_NR <- cbind(LCL = y_b1[1:t_sl["df1"]], UCL = y_b2[1:t_sl["df1"]])
          rownames(CI_NR) <- colnames(data[, tcol[ok]])
        } else {
          CI_NR <- cbind(LCL = y_b2[1:t_sl["df1"]], UCL = y_b1[1:t_sl["df1"]])
          rownames(CI_NR) <- colnames(data[, tcol[ok]])
        }
      }
    }

  l_NR <- vector(mode = "list", length = 6)
  names(l_NR) <-
    c("CI", "converged", "n.trial", "max.trial", "Warning", "Error")

  l_NR[[1]] <- CI_NR
  l_NR[[2]] <- tmp[["Model"]]$converged
  l_NR[[3]] <- tmp[["Model"]]$n.trial
  l_NR[[4]] <- tmp[["Model"]]$max.trial
  if(!is.null(tmp[["Warning"]])) l_NR[[5]] <- tmp[["Warning"]]
  if(!is.null(tmp[["Error"]])) l_NR[[6]] <- tmp[["Error"]]

  conclusions <- c(conclusion_Tsong, conclusion_Hoffelder)
  names(conclusions) <- c("Tsong", "Hoffelder")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compilation of results

  structure(list(Similarity = conclusions,
                 Parameters = t_res,
                 NR.CI = l_NR),
            class = "mimcr")
}

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
