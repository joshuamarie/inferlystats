#' Validate and extract data for a model ID
#'
#' `process_model_id()` resolves the variables declared in a model ID object
#' against a data frame, validates that all referenced columns exist, and
#' returns a named list of processed data ready for use by test
#' implementations.
#'
#' `process_model_id_global()` is the counterpart for the no-data path — when
#' variables are resolved from the calling environment rather than a data
#' frame.
#'
#' @param model_id A model ID object from [rel()], [cont_tab()],
#'   [prop_model()], [pairwise()], [selected_vars()], or [one_sample()].
#' @param data A data frame against which variables in `model_id` are
#'   resolved.
#'
#' @return A named list containing:
#' - `$data` — a subsetted data frame containing only the relevant columns,
#'   or a contingency table for `cont_tab()`
#' - `$vars` — a named character vector or list of resolved column names
#' - `$mat` — a contingency table, only present for `cont_tab()`
#' - `$pairs` — a list of variable pairs, only present for `pairwise()`
#' - `$formula` — the formula, only present for formula models
#'
#' @section Error behaviour:
#' Errors immediately if any referenced column is missing from `data`, with
#' an informative message listing both the missing columns and the available
#' ones.
#'
#' @seealso [define_model()], [rel()], [cont_tab()], [prop_model()],
#'   [pairwise()], [selected_vars()], [one_sample()]
#'
#' @examples
#' m = rel(group, extra)
#' process_model_id(m, sleep)
#'
#' m = cont_tab(vs, am)
#' process_model_id(m, mtcars)
#'
#' @name process-model-id
#' @export
process_model_id = function(model_id, data) {
    # ---- Process current model ----
    UseMethod("process_model_id")
}

#' @rdname process-model-id
#' @export
process_model_id.formula = function(model_id, data) {
    # ---- Formula ----
    list(
        data = data,
        vars = NULL,
        formula = model_id
    )
}

#' @rdname process-model-id
#' @export
process_model_id.rel = function(model_id, data) {
    # ---- "Relation" ----
    x_names = names(tidyselect::eval_select(model_id$args$x, data))
    resp_names = names(tidyselect::eval_select(model_id$args$resp, data))

    all_names = c(x_names, resp_names)
    .check_cols(data, all_names)

    list(
        data = data[, all_names, drop = FALSE],
        vars = list(x = x_names, resp = resp_names)
    )
}

#' @rdname process-model-id
#' @export
process_model_id.compare_by = function(model_id, data) {
    # ---- "Comparison" ----
    x_names = names(tidyselect::eval_select(model_id$args$x, data))
    grp_names = names(tidyselect::eval_select(model_id$args$grp, data))

    all_names = c(x_names, grp_names)
    .check_cols(data, all_names)

    list(
        data = data[, all_names, drop = FALSE],
        vars = list(x = x_names, grp = grp_names)
    )
}

#' @rdname process-model-id
#' @export
process_model_id.cont_tab = function(model_id, data) {
    # ---- Cont. Table ----
    ind_name = rlang::as_name(model_id$args$ind)
    dep_name = rlang::as_name(model_id$args$dep)

    .check_cols(data, c(ind_name, dep_name))

    list(
        data = data[, c(ind_name, dep_name), drop = FALSE],
        mat = table(
            data[[ind_name]],
            data[[dep_name]],
            dnn = c(ind_name, dep_name)
        ),
        vars = c(ind = ind_name, dep = dep_name)
    )
}

#' @rdname process-model-id
#' @export
process_model_id.prop_model = function(model_id, data) {
    # ---- Prop. Model ----
    var_name = rlang::as_name(model_id$args$x)

    .check_cols(data, var_name)

    list(
        data = data[, var_name, drop = FALSE],
        vars = c(x = var_name)
    )
}

#' @rdname process-model-id
#' @export
process_model_id.pairwise = function(model_id, data) {
    # ---- Pairwise ----
    sel = tidyselect::eval_select(model_id$args$dots, data)
    var_names = names(sel)
    .dir = model_id$.dir

    pairs = all_pairs(var_names, direction = .dir)

    list(
        data = vctrs::vec_slice(data, seq_len(nrow(data)))[var_names],
        vars = var_names,
        .dir = .dir,
        pairs = pairs
    )
}

#' @rdname process-model-id
#' @export
process_model_id.selected_vars = function(model_id, data) {
    # ---- Independent Vars ----
    sel = tidyselect::eval_select(model_id$args$dots, data)
    var_names = names(sel)

    list(
        data = data[, var_names, drop = FALSE],
        vars = var_names
    )
}

#' @rdname process-model-id
#' @export
process_model_id.one_sample = function(model_id, data) {
    # ---- Only 1 Var ----
    var_name = rlang::as_name(model_id$args$var)

    .check_cols(data, var_name)

    list(
        data = data[, var_name, drop = FALSE],
        vars = c(var = var_name)
    )
}

#' @rdname process-model-id
#' @export
process_model_id.infer_model = function(model_id, data) {
    cli::cli_abort(c(
        "No {.fn process_model_id} method for model type {.cls {class(model_id)[[1]]}}.",
        "i" = "This model type is not yet supported in {.fn define_model}."
    ))
}

#' Validate and extract data for a model ID from the calling environment
#'
#' `process_model_id_global()` is the counterpart to [process_model_id()] for
#' the no-data path. Instead of resolving variables against a data frame, it
#' evaluates them from the calling environment via lexical scoping.
#'
#' This is used when [define_model()] is called without a data frame — for
#' instance when passing raw vectors or pre-summarized values directly as
#' model ID arguments.
#'
#' @param model_id A model ID object from [rel()], [cont_tab()],
#'   [prop_model()], [pairwise()], [selected_vars()], or [one_sample()].
#'
#' @return A named list with the same structure as [process_model_id()]:
#' - `$data` — a data frame or contingency table constructed from the
#'   evaluated variables
#' - `$vars` — a named character vector or list of resolved variable names
#' - `$mat` — a contingency table, only present for `cont_tab()`
#' - `$formula` — the formula, only present for formula models
#'
#' @section Error behaviour:
#' Errors if the model type is not supported on the global path, with an
#' informative message suggesting the user supply a data frame via
#' [define_model()].
#'
#' @seealso [process_model_id()], [define_model()]
#'
#' @examples
#' m = rel(sleep$group, sleep$extra)
#' process_model_id_global(m)
#'
#' @name process-model-id-global
#' @export
process_model_id_global = function(model_id) {
    # ---- Process model globally ----
    UseMethod("process_model_id_global")
}

#' @rdname process-model-id-global
#' @export
process_model_id_global.rel = function(model_id) {
    # ---- "Relation" ----
    x = rlang::eval_tidy(model_id$args$x)
    resp = rlang::eval_tidy(model_id$args$resp)

    data = data.frame(x = x, resp = resp)
    names(data) = c(
        rlang::as_label(model_id$args$x),
        rlang::as_label(model_id$args$resp)
    )

    list(
        data = data,
        vars = list(x = names(data)[[1]], resp = names(data)[[2]])
    )
}

#' @rdname process-model-id-global
#' @export
process_model_id_global.formula = function(model_id) {
    # ---- Formula ----
    data = environment(model_id)

    list(
        data = data,
        vars = NULL,
        formula = model_id
    )
}

#' @rdname process-model-id-global
#' @export
process_model_id_global.cont_tab = function(model_id) {
    # ---- Cont. Table ----
    ind = rlang::eval_tidy(model_id$args$ind)
    dep = rlang::eval_tidy(model_id$args$dep)

    ind_label = rlang::as_label(model_id$args$ind)
    dep_label = rlang::as_label(model_id$args$dep)

    data = data.frame(ind, dep)
    names(data) = c(ind_label, dep_label)

    list(
        data = data,
        mat = table(ind, dep, dnn = c(ind_label, dep_label)),
        vars = c(x = names(data)[[1]], resp = names(data)[[2]])
    )
}

#' @rdname process-model-id-global
#' @export
process_model_id_global.infer_model = function(model_id) {
    cli::cli_abort(c(
        "No {.fn process_model_id_global} method for model type {.cls {class(model_id)[[1]]}}.",
        "i" = "This model type is not yet supported without a data frame."
    ))
}

.check_cols = function(data, cols) {
    missing = cols[!cols %in% names(data)]
    if (length(missing) > 0L) {
        cli::cli_abort(c(
            "Column{?s} not found in data: {.val {missing}}.",
            "i" = "Available columns: {.val {names(data)}}"
        ))
    }
    invisible(NULL)
}

#' @keywords internal
#' @export
print.model_to_analyze = function(x, ...) {
    model_id  = x$model_id
    processed = x$processed
    model_cls = class(model_id)[[1]]

    cli::cli_text(cli::style_bold("Model to Analyze"))
    cli::cat_line(cli::rule(line = "-"))

    cli::cli_text("{.field Type}  : {.cls {model_cls}}")

    switch(
        model_cls,
        rel = {
            cli::cli_text("{.field x}     : {.val {processed$vars$x}}")
            cli::cli_text("{.field resp}  : {.val {processed$vars$resp}}")
        },
        cont_tab = {
            cli::cli_text("{.field ind}   : {.val {processed$vars[['ind']]}}")
            cli::cli_text("{.field dep}   : {.val {processed$vars[['dep']]}}")
        },
        prop_model = {
            cli::cli_text("{.field x}     : {.val {processed$vars[['x']]}}")
        },
        pairwise = {
            cli::cli_text("{.field vars}  : {.val {processed$vars}}")
            cli::cli_text("{.field pairs} : {length(processed$pairs)} pair{?s}")
        },
        selected_vars = {
            cli::cli_text("{.field vars}  : {.val {processed$vars}}")
        },
        one_sample = {
            cli::cli_text("{.field var}   : {.val {processed$vars[['var']]}}")
        },
        formula = {
            cli::cli_text("{.field formula}: {.code {deparse(processed$formula)}}")
        },
        {
            nms = setdiff(names(processed), "data")
            for (nm in nms)
                cli::cli_text("{.field {nm}} : {.val {processed[[nm]]}}")
        }
    )

    if (!is.null(processed$data)) {
        dims = dim(processed$data)
        if (!is.null(dims))
            cli::cli_text("{.field data}  : {dims[[1]]} row{?s} x {dims[[2]]} col{?s}")
    }

    invisible(x)
}

#' @keywords internal
#' @export
print.infer_model = function(x, ...) {
    model_cls = class(x)[[1]]

    cli::cli_text(cli::style_bold("Model ID"))
    cli::cat_line(cli::rule(line = "-"))
    cli::cli_text("{.field Type} : {.cls {model_cls}}")

    switch(
        model_cls,
        rel = {
            cli::cli_text("{.field x}      : {.code {rlang::as_label(x$args$x)}}")
            cli::cli_text("{.field resp}   : {.code {rlang::as_label(x$args$resp)}}")
        },
        cont_tab = {
            cli::cli_text("{.field ind}    : {.code {rlang::as_label(x$args$ind)}}")
            cli::cli_text("{.field dep}    : {.code {rlang::as_label(x$args$dep)}}")
        },
        prop_model = {
            cli::cli_text("{.field x}      : {.code {rlang::as_label(x$args$x)}}")
        },
        pairwise = {
            vars = vapply(rlang::call_args(x$args$dots), rlang::as_label, character(1))
            cli::cli_text("{.field vars}   : {.code {vars}}")
            cli::cli_text("{.field dir}    : {.val {x$.dir}}")
        },
        selected_vars = {
            vars = vapply(rlang::call_args(x$args$dots), rlang::as_label, character(1))
            cli::cli_text("{.field vars}   : {.code {vars}}")
        },
        one_sample = {
            cli::cli_text("{.field var}    : {.code {rlang::as_label(x$args$var)}}")
        },
        {
            # fallback for user-defined model types
            nms = setdiff(names(x$args), "dots")
            for (nm in nms)
                cli::cli_text("{.field {nm}} : {.code {rlang::as_label(x$args[[nm]])}}")
        }
    )

    invisible(x)
}
