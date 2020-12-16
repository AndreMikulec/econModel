#' Convert an xts object Into a St Louis FED Weekly xts Object.
#'
#' @description
#' Given Dailies, return the five(5) day moving average (called a "weekly") with an Observation data on Fridays that are published on the following Thursdays.
#'
#' @param x xts object of Dailies.
#' @param Level String. Default is "Levels" Other choices are "ChangeInLevels" and "ChangeInLogLevels."
#' @return Modified xts object
#' @examples
#' \dontrun{
#' DGS2 <- quantmod::getSymbols("DGS2", src = "FRED", auto.assign = F)
#' DGS2 <- stLouisFEDdaily2Weekly(DGS2, Level = "ChangeInLevels")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stats na.omit
#' @importFrom zoo index `index<-` coredata as.zoo rollapply
#' @importFrom xts xts as.xts lag.xts xtsAttributes `xtsAttributes<-`
#' @importFrom timeDate timeNthNdayInMonth
#' @export
stLouisFEDdaily2Weekly <- function(x, Level) {
  tryCatchLog::tryCatchLog({

    # if not done elsewhere
    #correct for TZ
    oldtz <- Sys.getenv("TZ")
    if(oldtz!="UTC") {
      Sys.setenv(TZ="UTC")
    }

    if(missing(Level)) {
      Level <- "Levels"
    }
    if(!Level %in% c("Levels", "ChangeInLevels", "ChangeInLogLevels")) {
      stop("Level must be one of \"Levels\", \"ChangeInLevels\", or \"ChangeInLogLevels\"")
    }

    Result <- ORIG <- x

    Result <- stats::na.omit(Result)
    # changes
    if(Level == "ChangeInLogLevels") {
      Result <- log(Result)
    }
    if(Level %in%  c("ChangeInLevels","ChangeInLogLevels")) {
      Result <- (Result - xts::lag.xts(Result))/abs(xts::lag.xts(Result))
    }
    # 5 days(week) rolling means of changes
    Result  <- zoo::rollapply(zoo::as.zoo(Result), width = 5L, mean, fill = NA , partial = F, align = "right")

    # xts (with the original index class, format, and time zone)
    Result <- cbind(xts::xts(, zoo::index(ORIG)[0]), xts::as.xts(Result))
    # personal "xts attributes"
    xts::xtsAttributes(Result) <- c(list(), xts::xtsAttributes(Result), xts::xtsAttributes(ORIG))

    # FRI entries (Observation Dates)
    Index <- zoo::index(Result)[weekdays(zoo::index(Result)) == "Friday"]
    #
    Result <- Result[Index]

    # save seconds
    TimeDiff <- as.POSIXct(Index) - as.POSIXct(zoo::as.Date(Index))
    # next THU entries (Publication Dates) - a "better" Index
    BetterIndex <- zoo::as.Date(timeDate::timeNthNdayInMonth(as.character(zoo::as.Date(Index)), nday = 4, nth = 1))
    # put back TimeDiff
    BetterIndex <- as.POSIXct(BetterIndex) + TimeDiff
    # put back the class
    BetterIndex <- eval(parse(text = paste0("as.", class(index(ORIG))[1], "(BetterIndex)")))
    #
    zoo::index(Result) <- BetterIndex

    Sys.setenv(TZ=oldtz)

    Result

  }, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Stress Index
#'
#' @description
#' \preformatted{
#' Types of Risk
#'
#' "default risk":
#'   interest rate spread designed to measure
#'   the difference between yields on a “risky” asset
#'   and a “risk-free” asset
#'
#' "liquidity risk""
#'   inability to secure resources to acquire short term
#'   liabilities
#'
#' Stress Index
#' Uses a precedure (Principal Components) to create
#' the "stress index".
#'
#' Creating the Stress Index
#'
#' 1. First, each of the data series is de-meaned.
#'    The de-meaned series are then
#'    divided by their respective sample standard deviations (SDs)
#'    (Because each variable was standardized,
#'    the coefficient of a variable (SEE 2.) represents the
#'    "influence of a 1 SD change in that variable on the Stress Index.")
#' 2. Use the method of principal components:
#'    Extracting this factor (the first principal component).
#'    makes it able to create an index with a useful interpretation
#' 3. Calculate the  coefficients of the variables.
#'
#'    The factor loadings, also called component loadings in PCA,
#'    are the correlation coefficients
#'    between the variables (rows) and factors (columns).
#'
#'    The elements of the eigenvectors of . . . are the
#'    "coefficients" or "loadings" of the principal components.
#'
#' 4. We then scale these coefficients so that the SD of the index is 1.
#' 5. Finally, each data series is multiplied
#'    by its respective adjusted coefficient.
#' 6. Stress Index for time t is
#'    the sum of each series multiplied
#'    by its respective adjusted coefficient.
#'    (Higher values of the FSI indicate a
#'    greater degree of financial stress in the economy.)
#'
#' Stress Index proc details drawbacks
#'   A negative coefficient multiplied by a negative data value
#'   will result in a positive contribution to financial stress.
#'
#' Stress Index percent of the total variation
#'   in the many variables
#'   1 – SSE/SST
#' SST is the total sum of squares
#' SSE is the sum of squared errors
#' X(N,t) is the value of the Nth standardized variable in month t,
#' and a(N) is the set of coefficients chosen.

#' Updating the Stress Index
#'
#' When the sample changes (i.e., a new item of data is added),
#' the values of the Stress Index in the original sample can be changed.
#' This alteration can occur either through a
#' change in the coefficients of the variables in the index
#' or by a change in the actual values of the variables in the
#' original sample. The overall magnitude of the coefficients,
#' as well as their relative magnitudes, can change.
#' }
#' @param x xts object. Required. This is the input used for prediction.
#' @param ValDates List of (or vector of) pairs of vectors of begin and end date-time ranges.  Required (if not all of the ValidationData is to be used).
#' @return Index that is centered about zero(0) and scaled to the standard deviation of one(1).
#' @references
#' \cite{How to compute varimax-rotated principal components in R?, 2014
#' \url{https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r}
#' }
#' @references
#' \cite{Aaron Schlegel, Principal Component Analysis with R Example, Thu 19 January 2017
#' \url{https://aaronschlegel.me/principal-component-analysis-r-example.html}
#' }
#' @references
#' \cite{Introduction to Principal Components and FactorAnalysis, North Carolina State University
#' \url{ftp://statgen.ncsu.edu/pub/thorne/molevoclass/AtchleyOct19.pdf}
#' }
#' @references
#' \cite{Luke Hayden, Principal Component Analysis in R, August 9th, 2018
#'   \url{https://www.datacamp.com/community/tutorials/pca-analysis-r}
#' }
#' @references
#' \cite{author(s)?, National Economic Trends, Appendix, January 2010
#' \url{https://files.stlouisfed.org/files/htdocs/publications/net/NETJan2010Appendix.pdf}
#' }
#' @references
#' \cite{Kevin L. Kliesen and Douglas C. Smith, Measuring Financial Market Stress, Economic SYNOPSES, 2010 Number 2, Posted on January 15, 2010
#' \url{https://files.stlouisfed.org/files/htdocs/publications/es/10/ES1002.pdf}
#' }
#' @examples
#' \dontrun{
#' # E.g. some (and only some of the) interest rates
#' # of the St Louis Stress Index 2 (STLFSI2)
#' for(Symbol in c("DFF","DGS2","DGS10")) {
#'   quantmod::getSymbols(Symbol, src = "FRED")
#'   assign(Symbol, stLouisFEDdaily2Weekly(get(Symbol), Level = "ChangeInLevels"))
#' }
#'
#' Data <- stats::na.omit(cbind(DFF,DGS2,DGS10))
#' # dygraphs::dygraph(stressIndex(Data))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stats na.omit prcomp
#' @importFrom zoo index
#' @importFrom xts xts
#' @export
stressIndex <- function(x, ValDates,
                        ...
) {
tryCatchLog::tryCatchLog({

  x <- stats::na.omit(x)

  PrCmpObj <- stats::prcomp(x, center = TRUE, scale. = TRUE)
  Prediction <- predict(PrCmpObj, newdata = Data)[, "PC1"]
  PredictionScaled <- scale(Prediction, center = T, scale = T)

  colnames(PredictionScaled) <- "StressIndex"
  StressIndex <- xts::xts(PredictionScaled, zoo::index(Data))

  StressIndex

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Re-Createion of the St. Louis Federal Board of Governors Financial Stress Index Version 2
#'
#' @description
#' Uses a precedure (e.g. Principal Components) to create the "stress index" Financial Stress Index Version 2
#'
#' @references
#' \cite{St. Louis Fed Financial Stress Index (STLFSI2)
#' \url{https://fred.stlouisfed.org/series/STLFSI2}
#' }
#' @references
#' \cite{author(s)?, Table 1: Data Used to Construct STLFSI
#' \url{https://fredblog.stlouisfed.org/wp-content/uploads/2020/03/FSI2.pdf}
#' }
#' @references
#' \cite{Kevin Kliesen and Michael McCracken, The St. Louis Fed’s Financial Stress Index, Version 2.0, March 26, 2020
#' \url{https://fredblog.stlouisfed.org/2020/03/the-st-louis-feds-financial-stress-index-version-2-0/}
#' }
#' @references
#' \cite{List of Data Series Used to Construct the St. Louis Fed Financial Stress Index (STLFSI)
#'   \url{https://www.stlouisfed.org/~/media/Files/PDFs/STLFSI-Key.pdf?la=en}
#' }
#' @references
#' \cite{list of the components that are used to construct the STLFSI
#'   \url{https://www.stlouisfed.org/news-releases/st-louis-fed-financial-stress-index/stlfsi-key}
#' }
#' @references
#' \cite{St. Louis Fed Financial Stress Index (DISCONTINUED) (STLFSI)
#' \url{https://fred.stlouisfed.org/series/STLFSI}
#' }
#' @references
#' \cite{author(s)?, National Economic Trends, Appendix, January 2010
#' \url{https://files.stlouisfed.org/files/htdocs/publications/net/NETJan2010Appendix.pdf}
#' }
#' @references
#' \cite{author(s)?, The LIBOR-OIS Spread as a Summary Indicator, MonetaryTrends, November 2008
#' \url{https://files.stlouisfed.org/files/htdocs/publications/mt/20081101/cover.pdf}
#' }
#' @references
#' \cite{Kevin L. Kliesen and Douglas C. Smith, Measuring Financial Market Stress, Economic SYNOPSES, 2010 Number 2, Posted on January 15, 2010
#' \url{https://files.stlouisfed.org/files/htdocs/publications/es/10/ES1002.pdf}
#' }
#' @param TrainData xts object. Required.
#' @param TrainDates List of (or vector of) pairs of vectors of begin and end date-time ranges.  Required (if not all of the ValidationData is to be used).
#' @return Index.
#' @examples
#' \dontrun{
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
FSI2 <- function(x, TrainDates,
                 ...
) {
tryCatchLog::tryCatchLog({

  # STLFSI TrainData
  # We construct the STLFSI using 18 weekly data series . . .
  #
  # STLFSI2
  # 18 weekly data series, all of which are weekly averages of daily data series

  # Date Range:   1993-12-31 to 2020-11-27 (ObservationDate) - End of Week - FRI
  # Last Updated: 2020-12-03 9:00 AM CST   (PublishedDate)   - Following Week - THU Morn
  # https://fred.stlouisfed.org/data/STLFSI2.txt

  # STLFSI2
  # The key difference between versions 1.0 and 2.0 is that
  # 2.0 uses daily changes in interest rates and stock prices,
  # rather than the levels of interest rates and stock prices in the PCA calculation.
  #
  # Effective Federal Funds Rate (DFF)
  # Daily, 7-Day (since 1954)
  # https://fred.stlouisfed.org/series/DFF

  # Following are all Levels (unless noted otherwise)

  # 2-Year Treasury Constant Maturity Rate (DGS2) -  STLFSI2 - Change in Levels
  # Daily (since 1976)
  # https://fred.stlouisfed.org/series/DGS2


  # 10-Year Treasury Constant Maturity Rate (DGS10) -  STLFSI2 - Change in Levels
  # Daily (since 1952)
  # https://fred.stlouisfed.org/series/DGS10


  # 30-Year Treasury Constant Maturity Rate (DGS30) -  STLFSI2 - Change in Levels
  # Daily (since 1977)
  # https://fred.stlouisfed.org/series/DGS30


  # Moodys Seasoned Baa Corporate Bond Yield (DBAA) -  STLFSI2 - Change in Levels
  # Daily (since 1986)
  # https://fred.stlouisfed.org/series/DBAA
  # Bank of America - Previously BofAML
  # Bank of America Merrill Lynch is now Bank of America.
  # Google query of November 2020


  # Merrill Lynch High-Yield Corporate Master II Index -  STLFSI2 - Change in Levels
  # Merrill Lynch High Yield Master II
  # https://en.wikipedia.org/wiki/Merrill_Lynch_High_Yield_Master_II
  # NOT EXACTLY
  # ICE BofA US High Yield Index Effective Yield (BAMLH0A0HYM2EY)
  # Daily (since early 1997)
  # https://fred.stlouisfed.org/series/BAMLH0A0HYM2EY


  # Merrill Lynch Asset-Backed Master BBB-rated - STLFSI2 - Change in Levels
  # NOT EXACTLY
  # ICE BofA BBB US Corporate Index Effective Yield
  # Daily (since early 1997)
  # This data represents the effective yield of the ICE BofA BBB US Corporate Index,
  # a subset of the ICE BofA US Corporate Master Index
  # https://fred.stlouisfed.org/series/BAMLC0A4CBBBEY

  # Yield Curve: 10-Year minus 3-Month Treasury                                - STLFSI2
  # 10-Year Treasury Constant Maturity Minus 3-Month Treasury Constant Maturity (T10Y3M)
  # Daily (since 1982)
  # https://fred.stlouisfed.org/series/T10Y3M


  # Corporate Baa-rated bond minus 10-Year Treasury                           - STLFSI2
  # NOT EXACTLY
  # Moodys Seasoned Baa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity (BAA10Y)
  # Daily (since 1986)
  # Series is calculated as the spread between
  # Moodys Seasoned Baa Corporate Bond© (https://fred.stlouisfed.org/series/DBAA)
  # and 10-Year Treasury Constant Maturity (BC_10YEAR).
  # https://fred.stlouisfed.org/series/BAA10Y


  # Merrill Lynch High-Yield Corporate Master II minus 10-Year Treasury - STLFSI2
  # NOT EXACTLY
  # ICE BofA US High Yield Index Effective Yield (BAMLH0A0HYM2EY)
  # Daily (since early 1997)
  # https://fred.stlouisfed.org/series/BAMLH0A0HYM2EY
  # LESS
  # EXACTLY
  # 10-Year Treasury Constant Maturity Rate (DGS10)
  # Daily (since 1952)
  # https://fred.stlouisfed.org/series/DGS10

  # 3-month London Interbank Offering Rate - Overnight Index Swap (LIBOR-OIS) spread
  # overnight indexed swap (OIS)                                           - STLFSI2
  # (in the United States, this is the
  # effective federal funds rate)
  # The LIBOR-OIS Spread as a Summary Indicator
  # MonetaryTrends
  # November 2008
  # https://files.stlouisfed.org/files/htdocs/publications/mt/20081101/cover.pdf
  # NOT EXACTLY
  # 3-Month London Interbank Offered Rate (LIBOR), based on U.S. Dollar (USD3MTD156N)
  # Data (since 1986)
  # The data series is lagged by one week due to an agreement with the source.
  # https://fred.stlouisfed.org/series/USD3MTD156N
  # LESS
  # ???
  # The OIS, meanwhile, represents a given country’s central bank rate throughout a certain period;
  # in the US, that is the Fed funds rate—the key interest rate controlled
  # by the Federal Reserve, commonly called "the Fed".
  # https://www.investopedia.com/articles/active-trading/061114/what-ois-libor-spread-and-what-it.asp
  # NOT EXACTLY OR MAYBE EXACTLY
  # Effective Federal Funds Rate (DFF)
  # Daily, 7-Day (since 1954)
  # https://fred.stlouisfed.org/series/DFF


  # 3-Month Treasury-Eurodollar (TED) spread                           - STLFSI2
  # TED Spread (TEDRATE)
  # Daily (since 1986)
  # Series is calculated as the spread between 3-Month LIBOR
  # based on US dollars (https://fred.stlouisfed.org/series/USD3MTD156N) and
  # 3-Month Treasury Bill (https://fred.stlouisfed.org/series/DTB3).
  # The series is lagged by one week because the LIBOR series is lagged by one week due to an agreement with the source.
  # https://fred.stlouisfed.org/series/TEDRATE


  # 3-Month Commercial Paper minus 3-Month Treasury
  # 90-Day AA Financial Commercial Paper Interest Rate (RIFSPPFAAD90NB)
  # Daily (since 1997)
  # https://fred.stlouisfed.org/series/RIFSPPFAAD90NB
  # FROM DESCRIPTION OF
  # 3-Month Commercial Paper Minus Federal Funds Rate (CPFF)
  # Daily (since 1997)
  # https://fred.stlouisfed.org/series/CPFF
  # LESS
  # 3-Month Treasury Constant Maturity Rate (DGS3MO)
  # Daily (since 1981)
  # https://fred.stlouisfed.org/series/DGS3MO


  # J.P. Morgan Emerging Markets Bond Index Plus - STLFSI2 - Change in Log Levels
  # Emerging Markets Bond Index Plus
  # total returns for traded external debt instruments
  # (external meaning foreign currency denominated fixed income) in the emerging markets.
  # The regular EMBI index covers U.S.dollar-denominated Brady bonds, loans and Eurobonds.
  # An external debt version, the EMBI+ is the JPMorgan EMBI Global Index
  # https://en.wikipedia.org/wiki/JPMorgan_EMBI#Emerging_Markets_Bond_Index_Plus
  # NOT EXACTLY (CAN NOT FIND EMBI+)
  # iShares J.P. Morgan USD Emerging Markets Bond ETF (EMB)
  # NasdaqGS - NasdaqGS Real Time Price. Currency in USD
  # https://finance.yahoo.com/quote/EMB/
  # NOT EXACTLY AND ALTER DATA IS "NOT EASILY AVAILABLE"
  # quantmod::getSymbols("EMB", from = "1960-01-01")
  # 2007-12-19
  # MORE INFO
  # Introducing the J.P. Morgan Emerging
  # Markets Bond Index Global (EMBI Global)
  # capitalization-weighted indeOur current source for these classifications
  # (presently, any country with a per capita income less
  #   than US$9,635) is the World Bank publication, Global Development Finance.
  # https://faculty.darden.virginia.edu/liw/emf/embi.pdf
  # MORE INFO
  # The Top 5 International Bond Funds for 2020
  # By MELISSA HORTON
  # Updated Mar 25, 2020
  # 5. GMO Emerging Country Debt Fund (GMCDX)
  # objective of achieving total return in excess of its benchmark,
  # which is the J.P. Morgan Emerging Markets Bond Index Global Diversified
  # https://www.investopedia.com/articles/investing/010416/top-5-international-bond-funds-2016.asp
  # REPLACEMENT
  # quantmod::getSymbols("GMCDX", from = "1900-01-01")
  # (since 1994)


  # Chicago Board Options Exchange Market Volatility Index (VIX)      - STLFSI2
  # quantmod::getSymbols("VIX", from = "1900-01-01")
  # (only since 2014)
  # CBOE Volatility Index: VIX (VIXCLS)
  # Daily, Close
  # https://fred.stlouisfed.org/series/VIXCLS
  # (since 1990)


  # Merrill Lynch Bond Market Volatility Index (1-Month)             - STLFSI2
  # NOT EXACTLY
  # Investors seeking yield should use MOVE and VIX
  # The MOVE (Merrill Lynch Option Volatility Estimate) Index
  # measures the implied volatility of U.S. Treasury markets and, along with the VIX,
  # is a useful indicator for investors in assessing the psyche of the market
  # . . .
  # MOVE
  # gauging options contracts on one-month Treasury issues.
  # . . .
  # https://financialpost.com/investing/investors-seeking-yield-should-use-move-and-vix
  # ROLL MY OWN - 22 DAYS
  # 1-Month Treasury Constant Maturity Rate
  # Daily
  # https://fred.stlouisfed.org/data/DGS1MO.txt
  # (since 2001 ) too young
  # 4-Week Treasury Bill: Secondary Market Rate (TB4WK)
  # Daily
  # https://fred.stlouisfed.org/series/TB4WK
  # (since 2001) too young
  # Commercial Paper
  # https://www.sciencedirect.com/topics/economics-econometrics-and-finance/commercial-paper
  # 30-Day AA Financial Commercial Paper Interest Rate (RIFSPPFAAD30NB)
  # https://fred.stlouisfed.org/series/RIFSPPFAAD30NB
  # (since 1997)


  # 10-Year Nominal Treasury Yield minus 10-Year Treasury Inflation Protected Security Yield (Breakeven Inflation Rate)
  # 10-Year Breakeven Inflation Rate (T10YIE)                       - STLFSI2
  # Daily(since 2003) too young
  # https://fred.stlouisfed.org/series/T10YIE
  # 10-Year Treasury Inflation-Indexed Security, Constant Maturity (DFII10)
  # Daily (since 2003) too young
  # https://fred.stlouisfed.org/series/DFII10
  # . . .
  # A yield curve is a line that plots yields (interest rates) of bonds
  # having equal credit quality but differing maturity dates
  # https://www.investopedia.com/terms/y/yieldcurve.asp
  # . . .
  # Treasury Real Yield Curve Rates. These rates are commonly referred to as "Real Constant Maturity Treasury" rates,
  # Real yields on Treasury Inflation Protected Securities (TIPS) at "constant maturity" are
  # interpolated by the U.S. Treasury from Treasurys daily real yield curve.
  # SO I WANT
  # Daily Treasury Yield Curve Rates (getYieldCurve example 1)
  # LESS
  # Daily Treasury Real Yield Curve Rates (getYieldCurve example 4)
  #
  # NOTE example 5 is direct.
  # . . .
  # (since 1990)
  # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield
  #
  # What Is the Nominal Interest Rate? (Need Inflation Adjustment)
  # Nominal interest rate refers to the interest rate before taking inflation into account.
  # https://www.investopedia.com/terms/n/nominalinterestrate.asp

  # S&P Financial Index                                 - STLFSI2 - Change in Log Levels
  # Vanguard Financials Index Fund ETF Shares (VFH)
  # NYSEArca - NYSEArca Delayed Price. Currency in USD
  # http://finance.yahoo.com/q?s=vfh&.yficrumb=CgCsdHP9jeG
  # https://finance.yahoo.com/quote/vfh?ltr=1
  # quantmod::getSymbols("VFH", from = "1900-01-01")
  # 2004-01-30 (too young)
  # ^SP500-40	S&P 500 Financials (Sector) - symbol
  # https://finance.yahoo.com/lookup/index?s=sector?
  # Getting data of sub-sector indexes of an S&P 500 index sector using QuantMod in R
  # 2016
  # https://quant.stackexchange.com/questions/29576/getting-data-of-sub-sector-indexes-of-an-sp-500-index-sector-using-quantmod-in
  # quantmod::getSymbols("XLF", from = "1900-01-01")
  # 1998-12-22

  # STLFSI of 07/15/2010 the Vanguard Financial Exchange-Traded Fund series
  # has been replaced with the S&P 500 Financials Index.

  # STLFSI TrainDates
  # . . . over the
  # sample period December 31, 1993, to December 11, 2009
  #
  # STLFSI and STLFSI2 start December 31, 1993
  # STLFSI ends March 13, 2020
  # # The St. Louis Fed’s Financial Stress Index, Version 2.0
  # https://fredblog.stlouisfed.org/2020/03/the-st-louis-feds-financial-stress-index-version-2-0/

  # BUT THEY ARE DOING
  # When the sample changes (i.e., a new week of data is added),
  # the values of the FSI in the original sample can be changed.
  #

  # STLFSI proc
  # Principal components analysis is used to construct the STLFSI
  #
  # STLFSI index
  # by extracting this factor (the first principal component)
  # we are able to create an index
  #

  # STLFSI index reports
  # St. Louis Fed Financial Stress Index (DISCONTINUED) (STLFSI)
  # Weekly, Ending Friday
  # https://fred.stlouisfed.org/series/STLFSI
  #
  # St. Louis Fed Financial Stress Index (STLFSI2)
  # Weekly, Ending Friday
  # https://fred.stlouisfed.org/series/STLFSI2


  # STLFSI index intepretation
  # How to Interpret the Index:
  # The average value of the index, which begins in late 1993, is designed to be zero.
  # Thus, zero is viewed as representing normal financial market conditions.
  # Values below zero suggest below-average financial market stress,
  # while values above zero suggest above-average financial market stress.


  #   STLFSI proc details
  #   1. First, each of the data series is de-meaned.
  #      The de-meaned series are then
  #      divided by their respective sample standard deviations (SDs)
  #      (Because each variable was standardized,
  #      the coefficient of a variable (SEE 2.) represents the
  #      "influence of a 1 SD change in that variable on the FSI.")
  #   2. we use the method of principal components to calculate the
  #      coefficients of the variables
  #   3. We then scale these coefficients so that the SD of the index is 1.
  #   4. Finally, each data series is multiplied by its respective adjusted coefficient
  #   5. FSI for time t is
  #      the sum of each series multiplied by its respective adjusted coefficient.
  #      (Higher values of the FSI indicate a
  #      greater degree of financial stress in the economy.)
  #
  #   STLFSI proc details drawbacks
  #     A negative coefficient multiplied by a negative data value
  #     will result in a positive contribution to financial stress.
  #
  #   STLFSI percent of the total variation
  #     in the 18 variables (DEFINITION)
  #     1 – SSE/SST
  #

  stressIndex(x , TrainDates,
              ...
  )

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
