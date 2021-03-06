#include <Rcpp.h>
using namespace Rcpp;

/* This is taken from envir.c in the R 2.15.1 source
 https://github.com/SurajGupta/r-source/blob/master/src/main/envir.c
 */
#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))
//' Unlock an R Package Enviroment and Assign an Object to the R Package Environment
//'
//' Of unlockEnvironment, unlock a non-base package environment.  Note: R package base is unlocked (by design)
//'
//' @param env Environment. Default none. Required.
//' @return Of unlockEnvironment, Logical.  TRUE if this package environment has been successfully unlocked. FALSE otherwise.
//' @author Willem Ligtenberg is the author of unlockEnvironment
//' @references
//' \cite{unlockEnvironment function
//' \url{https://github.com/openanalytics/Rango/blob/adc99e077b71c8c6826cabb7ff1266050898718a/Rango/src/unlockEnvironment.cpp}
//' }
//' @references
//' \cite{envir.c in the R 2.15.1 source (still in R 3.3.1)
//' \url{https://github.com/SurajGupta/r-source/blob/a28e609e72ed7c47f6ddfbb86c85279a0750f0b7/src/main/envir.c}
//' }
//' @export
// [[Rcpp::export]]
RObject unlockEnvironment(Environment env){
  if (TYPEOF(env) == NILSXP)
    ::Rf_error("use of NULL environment is defunct");
  if (TYPEOF(env) != ENVSXP)
    ::Rf_error("not an environment");

  UNLOCK_FRAME(env);
  // Return TRUE if unlocked; FALSE otherwise
  SEXP result = PROTECT( Rf_allocVector(LGLSXP, 1) );
  LOGICAL(result)[0] = FRAME_IS_LOCKED(env) == 0;
  UNPROTECT(1);
  return result;
}

