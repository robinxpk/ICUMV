add_to_be_removed = function(df_to_add, df_base, cause, comment = NULL, id_colname = "combined_id"){
    # Function used to add the combined_id of patient that are supposed to be removed onto the removed_patient data.frame
    return(
        rbind(
            df_base,
            df_to_add |> 
                dplyr::select(all_of(id_colname)) |> 
                dplyr::mutate(
                    cause = cause,
                    comment = comment
                )
        )
    )
}

rename_cols = function(df, rename_list){
    # Rename the columns of the data frame `df` using the rename_list. 
    # rename_list is expected to have the shape:
    # rename_list = list(
    #     "old_name" = "new_name",
    #     ...
    # )
    names(df)[names(df) %in% names(rename_list)] <- rename_list[names(df)[names(df) %in% names(rename_list)]]
    return(df)
}

describe_variables = function(df, df_name = "df_name", output_file = "test.csv"){
    info <- tibble::tibble(
        df_name = df_name,
        variable = names(df),
        encoding = sapply(df, function(x) class(x)[1]),
        label = sapply(
            df, 
            function(x){
                lbl = attr(x, "label") |> unname() |> unlist()
                if (is.null(lbl)) "" else lbl
            }
        )
    )
    write.csv2(info, file = output_file)
}

encode_cols = function(df, encoding_triplets){

    for (triplet in encoding_triplets){
        entries = df[, triplet$col] |> unlist()

        if (!is.null(triplet$lookup_list)) entries = lookup_entries(entries, triplet$lookup_list)

        if (triplet$type == "factor") df[, triplet$col] = as.factor(entries)
        else if (triplet$type == "logical") df[, triplet$col]  = as.logical(entries)
    }

    return(df)
}

lookup_entries = function(entries, lookup_list){
    sapply(
        lookup_list[as.character(entries)], 
        function(x) if (!is.null(x)) x else NA
    ) |> unlist() |> unname()
}
