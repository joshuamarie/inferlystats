#' @rdname define-model
#' @export
define_model.data.frame = function(.x, to_analyze, ...) {
    processed = process_model_id(to_analyze, data = .x)

    model_id = if (inherits(to_analyze, "formula")) {
        out = list(formula = processed$formula)
        class(out) = "formula"
        out
    } else {
        to_analyze
    }

    out = list(
        model_id  = model_id,
        test_spec = NULL,
        options = vctrs::vec_c(...),
        processed = processed
    )
    class(out) = "model_to_analyze"
    out
}

#' @rdname define-model
#' @export
define_model.infer_model = function(.x, data = NULL, ...) {
    processed = if (!is.null(data)) {
        process_model_id(.x, data = data)
    } else {
        process_model_id_global(.x)
    }

    model_id = if (inherits(.x, "formula")) {
        out = list(formula = processed$formula)
        class(out) = "formula"
        out
    } else {
        .x
    }

    out = list(
        model_id = model_id,
        processed = processed,
        test_spec = NULL,
        options = vctrs::vec_c(...)
    )
    class(out) = "model_to_analyze"
    out
}

#' Model ID constructors for use inside `define_model()`
#'
#' These functions declare the structure of the analysis — which variables to
#' use and how they relate to each other. They are always passed as the
#' `to_analyze` argument to [define_model()], or called standalone for the
#' global (no data frame) path.
#'
#' Each constructor produces an `infer_model` object carrying the variable
#' declarations as unevaluated expressions. Variables are resolved lazily —
#' either against a data frame inside [define_model()], or from the calling
#' environment when no data frame is supplied.
#'
#' @param x <[`tidy-select`][dplyr::dplyr_tidy_select]> The grouping or
#'   predictor variable. For `rel()`, this is the variable whose levels define
#'   the groups being compared.
#' @param resp <[`tidy-select`][dplyr::dplyr_tidy_select]> The response
#'   variable. For `rel()`, this is the outcome being measured across groups.
#' @param ind <[`tidy-select`][dplyr::dplyr_tidy_select]> The independent
#'   (row) variable in the contingency table.
#' @param dep <[`tidy-select`][dplyr::dplyr_tidy_select]> The dependent
#'   (column) variable in the contingency table.
#' @param x <[`tidy-select`][dplyr::dplyr_tidy_select]> The variable whose
#'   proportion is being modeled. For the global path, a count of successes.
#' @param size For `prop_model()` on the global path only: the total number
#'   of trials.
#' @param var <[`tidy-select`][dplyr::dplyr_tidy_select]> The single variable
#'   to analyze.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> For `pairwise()`
#'   and `selected_vars()`: two or more variables to include.
#' @param .dir Direction of pairing for `pairwise()`. One of `"leq"`
#'   (default), `"lt"`, `"gt"`, `"geq"`, `"eq"`, `"neq"`, or `"all"`.
#'
#' @return An `infer_model` object of the corresponding subclass —
#'   `rel`, `cont_tab`, `prop_model`, `pairwise`, `selected_vars`, or
#'   `one_sample`.
#'
#' @section `rel()`:
#' Declares a two-variable relationship between a grouping variable `x` and a
#' response variable `resp`. Used for tests that compare a response across
#' groups, such as and `COR_TEST()`.
#'
#' @section `cont_tab()`:
#' Declares a contingency table from two categorical variables `ind` and
#' `dep`. Used for tests of independence or association, such as
#' [CHI2_TEST()].
#'
#' @section `prop_model()`:
#' Declares a proportion model from a binary variable or raw success/trial
#' counts. Used for `BINOM_TEST()` and `PROP_TEST()`.
#'
#' @section `pairwise()`:
#' Declares all pairwise combinations of two or more variables. The direction
#' of pairing is controlled by `.dir`. Used for pairwise [TTEST()] and similar
#' tests.
#'
#' @section `selected_vars()`:
#' Declares an arbitrary set of variables with no assumed structure. Used when
#' the test itself determines how the variables are used.
#'
#' @section `one_sample()`:
#' Declares a single variable for one-sample inference — testing or estimating
#' a population parameter from a single group.
#'
#' @seealso [define_model()], [run_test()], [prepare_test()]
#'
#' @examples
#' # rel() — grouping variable x response
#' sleep |> define_model(rel(group, extra))
#' define_model(rel(sleep$group, sleep$extra))
#'
#' # cont_tab() — contingency table
#' mtcars |> define_model(cont_tab(vs, am))
#'
#' # prop_model() — proportion from data or counts
#' mtcars |> define_model(prop_model(vs))
#' define_model(prop_model(x = 10, size = 19))
#'
#' # pairwise() — all pairwise combinations
#' sleep |> define_model(pairwise(extra, group))
#'
#' # selected_vars() — arbitrary variable set
#' mtcars |> define_model(selected_vars(mpg, wt, hp))
#'
#' # one_sample() — single variable
#' sleep |> define_model(one_sample(extra))
#'
#' @name model-id
NULL

#' @rdname model-id
#' @export
rel = function(x, resp, ...) {
    xs = rlang::enquo(x)
    resps = rlang::enquo(resp)

    out = list(
        args = list(x = xs, resp = resps),
        data = NULL
    )
    class(out) = write_class("rel")
    out
}

#' @rdname model-id
#' @export
compare_by = function(x, grp, ...) {
    xs = rlang::enquo(x)
    grps = rlang::enquo(grp)

    out = list(
        args = list(x = xs, grp = grps),
        data = NULL
    )
    class(out) = write_class("compare_by")
    out
}

#' @rdname model-id
#' @export
cont_tab = function(ind, dep, ...) {
    inds = rlang::enquo(ind)
    deps = rlang::enquo(dep)

    out = list(
        args = list(ind = inds, dep = deps),
        data = NULL,
        formula = NULL
    )
    class(out) = write_class("cont_tab")
    out
}

#' @rdname model-id
#' @export
prop_model = function(x, ...) {
    xs = rlang::enquo(x)

    out = list(
        args = list(x = xs),
        data = NULL,
        formula = NULL
    )
    class(out) = write_class("prop_model")
    out
}

#' @rdname model-id
#' @export
pairwise = function(..., .dir = "leq") {
    dots = rlang::enquos(...)

    out = list(
        args = list(dots = rlang::expr(c(!!!dots))),
        data = NULL,
        formula = NULL,
        .dir = .dir
    )
    class(out) = write_class("pairwise")
    out
}

#' @rdname model-id
#' @export
selected_vars = function(...) {
    dots = rlang::enquos(...)

    out = list(
        args = list(dots = rlang::expr(c(!!!dots))),
        data = NULL,
        formula = NULL
    )
    class(out) = write_class("selected_vars")
    out
}

#' @rdname model-id
#' @export
one_sample = function(var, ...) {
    vs = rlang::enquo(var)

    out = list(
        args = list(var = vs),
        data = NULL,
        formula = NULL
    )
    class(out) = write_class("one_sample")
    out
}

#' @keywords internal
#' @export
model_id = function(clss, arg_list, data = NULL, formula = NULL) {
    out = list(
        args = arg_list,
        data = data,
        formula = formula
    )
    class(out) = write_class(clss)
    out
}
