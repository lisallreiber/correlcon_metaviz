# functions for MA


# Funktion Correlation with N
corr_pipe = . %>% as.matrix() %>%
      Hmisc::rcorr(.) %>% {
            left_join(left_join(.$r %>% reshape2::melt() %>% rename(r = value),
                                .$n %>% reshape2::melt() %>% rename(n = value),
                                by = c("Var1", "Var2")),
                      .$P %>% reshape2::melt() %>% rename(p = value),
                      by = c("Var1", "Var2"))
      } %>%
      arrange(desc(r)) %>%
      filter(Var1 != Var2)


# grouped_cor_test --------------------------------------------------------------
# Function to calculate grouped t-test for correlations

grouped_cor_test = function(data, group, ...) {

      cor_vars <- enquos(...)

      if (sum(!is.na(group)) == 0) {

         print(paste0("Overall Correlations"))

         data %>%
            drop_na(!!!cor_vars) %>%
            copy_labels(data) %>%
            dplyr::summarise(
               COR = cor.test(!!!cor_vars, use = "complete.obs")[[4]],
               CI_low = (cor.test(!!!cor_vars, use = "complete.obs")[[9]][1]),
               CI_high = (cor.test(!!!cor_vars, use = "complete.obs")[[9]][2]),
               df = cor.test(!!!cor_vars, use = "complete.obs")[[2]]) %>%
            mutate(n = df + length(cor_vars)) %>%
            adorn_rounding(3) %>%
            as_tibble()

      } else if (sum(!is.na(group)) > 0) {

         index_group <- which(colnames(data) %in% group)

         print(paste0("Correlations grouped by: ", group))

         data %>% drop_na(!!!cor_vars) %>% copy_labels(data) %>%
            group_by_at(index_group) %>%
            dplyr::summarize(
                  COR = cor.test(!!!cor_vars, use = "complete.obs")[[4]],
                  CI_low = (cor.test(!!!cor_vars, use = "complete.obs")[[9]][1]),
                  CI_high = (cor.test(!!!cor_vars, use = "complete.obs")[[9]][2]),
                  df = cor.test(!!!cor_vars, use = "complete.obs")[[2]]) %>%
            mutate(n = df + length(cor_vars)) %>%
            adorn_rounding(3) %>%
            as_tibble()
      }

}

# 1.2_Identify_ParentPointers-----

select_pointers <- function(input_data, pointer_id_name, pointer, source, additional_vars = NULL) {

   # this function takes one of the source datasets that contains child and parent pointer id's
   # 1. selects those two ids
   # 2. keep all unique child-parent combinations
   # 3. keeps only fully identified pairs (drops rows with missing parental pointers)
   # 4. generates information about the source of the parental pointer and the kind (mother or father)
   # 5. generates pointer_nr which identifies how many parent pointers were identified in this dataset per child (e.g. if the data contains two mother pointers (social and genetic) for one child)

   selected_pointers <- input_data %>%
      # here we throw away the information about the survey year
      # use the pointer_id specified above to get match mothers
      tidylog::select(child_id, pointer_id_name, {{additional_vars}}) %>%
      rename_at(vars(contains(paste0(pointer, "_"))), list(~str_replace(., paste0(pointer, "_"), "pointer_"))) %>%
      tidylog::distinct(child_id, pointer_id, .keep_all = T) %>%
      drop_na(pointer_id) %>%
      tidylog::mutate(pointer = pointer,
                      source = source)


   return(selected_pointers)

}


match_child_pointer <- function(child_data, pointer, source, pointer_data ) {

   # this function takes the children base data and merges all available pointers from the source dataset
   # 1. it filters the base child data based on the source used, and the pointer that is supposed to be matches (mother or father)
   # 2. merges information stored in specified source_pointer dataset
   # !!NOTE: depends on the pointer_parent datasets created in the line before

   child_data %>%
      # here we filter for the specific source and pointer we want to join data from, so that missings are only due to missings in the source dataset
      filter(source == {{source}}, pointer == {{pointer}}) %>%
      tidylog::left_join(pointer_data %>%
                            # we create an indicator for the number of different pointers identified in the source for every child
                            group_by(child_id, pointer) %>%
                            mutate(pointer_nr = row_number()),
                         by = c("child_id", "pointer", "source", "pointer_nr"))

}

summary_by_child <- function(summary_data, kind = c("specific", "all"), names_from = "pointer", values_from = "n_avail", names_prefix = "n_") {

   # this function takes a summarizes dataset on childrne and their matched parental pointers and transforms it into a wider format so that the number of identified mothers and fathers can be compared per child

   if (kind == "specific") {

      summary_by_child <- summary_data %>% select(child_id, pointer, {{values_from}}, source) %>%
         pivot_wider(names_from = {{names_from}}, values_from = {{values_from}}, names_prefix = names_prefix)

   } else if (kind == "all") {

      summary_by_child <- summary_data %>%
         select(-n_avail) %>%
         pivot_wider(names_from = "source", values_from = "flag_avail", names_prefix = "avail_") %>%
         group_by(pointer)
   }

   return(summary_by_child)

}

# this function goes to the large child_parent_sources_pointers dataset filtered by match_type and source and returns the found matches
find_matches <- function(input_data, filter_match = NA, first_run = FALSE) {

   if (first_run == TRUE) {
      found_matches <- input_data %>%
         tidylog::left_join(child_parent_sources_pointers %>% filter(select_match_source == filter_match,
                                                                     child_match_rel == "child"),
                            by = c("child_id", "pointer", "pointer_nr"))

   } else if (first_run == FALSE) {

      found_matches <- input_data %>%
         group_by(child_id, pointer) %>%
         add_tally(is.na(pointer_id)) %>%
         ungroup() %>%
         filter(is.na(pointer_id) & n == 3) %>%
         select(child_id, pointer, pointer_nr) %>%
         tidylog::left_join(child_parent_sources_pointers %>% filter(select_match_source == filter_match,
                                                                     child_match_rel == "child"),
                            by = c("child_id", "pointer", "pointer_nr"))
   }
   return(found_matches)
}

find_social_matches <- function(input_data, filter_match = NA, first_run = FALSE) {

   if (first_run == TRUE) {
      found_matches <- input_data %>%
         tidylog::left_join(child_parent_sources_pointers %>% filter(select_match_source == filter_match),
                            by = c("child_id", "pointer", "pointer_nr"))

   } else if (first_run == FALSE) {

      found_matches <- input_data %>%
         filter(is.na(pointer_id)) %>%
         select(child_id, pointer, pointer_nr) %>%
         tidylog::left_join(child_parent_sources_pointers %>% filter(select_match_source == filter_match), by = c("child_id", "pointer", "pointer_nr"))
   }
   return(found_matches)
}
