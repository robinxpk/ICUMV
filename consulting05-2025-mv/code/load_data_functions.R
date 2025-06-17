add_to_be_removed = function(df_to_add, df_base, cause, comment = NULL){
    # Function used to add the combined_id of patient that are supposed to be removed onto the removed_patient data.frame
    return(
        rbind(
            df_base,
            df_to_add |> 
                dplyr::select(CombinedID) |> 
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
