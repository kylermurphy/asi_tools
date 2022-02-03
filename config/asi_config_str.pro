function asi_config_str

  return, {init:0, $ ; determine if asi_tools has been intialized 
           data_dir:'', $ ; data directory for 
           themis_url:'https://data.phys.ucalgary.ca/sort_by_project/THEMIS/asi/', $
           rego_url:'https://data.phys.ucalgary.ca/sort_by_project/GO-Canada/REGO', $
           rgb_url:'https://data.phys.ucalgary.ca/sort_by_project/TREx/RGB', $
           blue_url:'Place Holder'}

end