# Auto-code constituent_type

# get constituent_types google sheet
constituent_types <- googledrive::drive_ls() %>% 
  filter(name == ".constituent_types") %>% 
  googlesheets4::read_sheet()

