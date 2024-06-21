
## Use this to add modules.


# actdata validations -----------------------------------------------------

available_equations <- actdata::equations |> 
  dplyr::filter(equation_type == "impressionabo") |> 
  dplyr::select(key, group)

available_dictionaries <- purrr::map(actdata::get_dicts(), \(x) x@groups)
names(available_dictionaries) <- purrr::map_chr(actdata::get_dicts(), \(x) x@key)
