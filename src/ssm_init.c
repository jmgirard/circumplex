#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _ssm_angle_mean(SEXP);
extern SEXP _ssm_angle_median(SEXP);
extern SEXP _ssm_compare_pi(SEXP);
extern SEXP _ssm_group_parameters(SEXP, SEXP);
extern SEXP _ssm_group_scores(SEXP, SEXP);
extern SEXP _ssm_measure_scores(SEXP, SEXP);
extern SEXP _ssm_ssm_parameters(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_ssm_angle_mean",       (DL_FUNC) &_ssm_angle_mean,       1},
    {"_ssm_angle_median",     (DL_FUNC) &_ssm_angle_median,     1},
    {"_ssm_compare_pi",       (DL_FUNC) &_ssm_compare_pi,       1},
    {"_ssm_group_parameters", (DL_FUNC) &_ssm_group_parameters, 2},
    {"_ssm_group_scores",     (DL_FUNC) &_ssm_group_scores,     2},
    {"_ssm_measure_scores",   (DL_FUNC) &_ssm_measure_scores,   2},
    {"_ssm_ssm_parameters",   (DL_FUNC) &_ssm_ssm_parameters,   2},
    {NULL, NULL, 0}
};

void R_init_ssm(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}