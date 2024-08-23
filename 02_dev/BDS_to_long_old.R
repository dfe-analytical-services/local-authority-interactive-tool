#### ACTIONS TO BDS FORMAT

bdsdata <- read.csv(
  here::here("01_data/01_raw/BDS_Wide.csv"),
  skip = 2
) # Reads in BDS

bdslong <- bdsdata |>
  tidyr::pivot_longer(
    cols = Barking.and.Dagenham:England_2, names_to = "LA and Regions",
    values_to = "Values"
  ) |>
  dplyr::filter(
    Years %notin% c(
      "-",
      "",
      "Trend",
      "Change",
      "Rank",
      "Target",
      "Distance",
      "Quartile",
      "Deviation National",
      "% YoY change",
      "%3yr change",
      "2010 Target",
      "Target 2011",
      "NA - Distance",
      is.na(0)
    )
  ) # removes these lines with matching entries in year column

# Creating Vectors:
columns_to_delete <- c(
  "Quartile.1.(A)",
  "Quartile.2.(B)",
  "Quartile.3.(C.)",
  "Quartile.4.(D)",
  "X181",
  "line",
  "Data.item"
) # vector to store columns to delete
# Underscore_delete <-c("_1", "_2", "_3", "_4", "_5", "_6", "_7" ,"_8" ,"_9" ,"_10")


bdslong <- bdslong |>
  dplyr::select(
    !columns_to_delete,
    -dplyr::starts_with("X")
  ) |> # delete lines in vector from column
  dplyr::mutate(
    "LA and Regions" = stringr::str_replace_all(`LA and Regions`, "\\.", " ")
  ) |> # replaces dots in LA and Regions with spaces
  dplyr::mutate(
    "LA and Regions" = str_replace_all(`LA and Regions`, "  ", "\\. ")
  ) |> # sorts out St.Helens issue!
  dplyr::mutate(
    Short.Desc = (str_remove(Short.Desc, "(_[0-9]{1,2})"))
  ) # removes underscore and number from Short code

LACharacteristics <- readxl::read_xlsx(
  here::here("01_data/02_prod/LA code list.xlsx")
) # Reads in LA code list

bdslong <- bdslong |>
  tidyr::left_join(
    LACharacteristics,
    by = c("LA and Regions" = "LA.Name")
  ) # Joins data frames on LA Names

bdslong$CombinedCode <- paste(
  bdslong$LA.Number,
  bdslong$Short.Desc,
  bdslong$Years,
  sep = "_"
) # concatenates short code, year and LA num


# bdslong_NA <- bdslong |>
#   dplyr::filter(is.na(Region))#creates an unwritten data frame to check BDSlong for NA mismatches from join


# Create link to live file and pull data into BDS Long - do this next....

# Ofsted <- readxl::read_xlsx(
#   createLink(
#     link = paste0(
#       "https://educationgovuk.sharepoint.com/sites/sdg/c/WorkplaceDocuments/",
#       "BETA_Interventions/Data/Latest%20Ofsted%20Overall%20Judgement.xlsx"
#     ),
#     skip=!overwrite,
#     overwrite=FALSE,
#     methods=getOption(
#       "createLink/args/methods",
#       c("windows-ntfs-symlink", "windows-shortcut")
#     ), ...
#   )
# )


# Ofsted <- file.path(
#   paste0("//educationgovuk.sharepoint.com/sites/sdg/c/WorkplaceDocuments/",
#          "BETA_Interventions/Data/Latest%20Ofsted%20Overall%20Judgement.xlsx"
#   )
# )# Create file path
# OfstedJudgements<-readxl::read_xlsx(
#   url(Ofsted,sheets("Latest Ofsted"),startRow=16)
# )
# get(Ofsted, authenticate("name", "password","ntlm"),
#     write_disk("tempfile.xlsx", overwrite = TRUE))
# df <- readxl::read_xlsx(
#   "tempfile.xlsx",
#   sheet = "Latest Ofsted",
#   skip = 16
# )
# bdslong$OfstedJudgement

# copyFromSharePoint <- function(fileName) {
#   cmd <- paste0(
#     "curl --max-time 7200 --connect-timeout 7200 --ntlm --user ",
#     "--download-file educationgovuk.sharepoint.com/sites/sdg/c/",
#     "WorkplaceDocuments/BETA_Interventions/Data/",
#     fileName,
#     "OneDrive - Department for Education/MyRCode/LAIT modernisation project",
#     fileName
#     )
#
#   system(cmd)
# }

# copyFromSharePoint("Latest%20Ofsted%20Overall%20Judgement.xlsx")



# write.csv(bdslong, "BDS_Long.csv") # Write first pivot to help check outputs


# Regionfilter

bdslongregion <- bdslong |>
  dplyr::filter(`LA and Regions` %in%
    c(
      "North.East",
      "North.West",
      "Yorkshire.and.the.Humber",
      "East.Midlands", "West.Midlands",
      "East.of.England",
      "London",
      "Inner.London",
      "Outer.London",
      "South.East",
      "South.West"
    ))

# write.csv(bdslongregion, "BDS_Regions_Only_long.csv")

# EnglandFilter

bdsNational <- bdslong |>
  dplyr::filter(`LA and Regions` == "England")

# write.csv(bdsNational, "National_Only_long.csv")

# LAFilter

bdsLAOnly <- bdslong |>
  dplyr::filter(`LA and Regions` %notin%
    c(
      "England",
      "North.East",
      "North.West",
      "Yorkshire.and.the.Humber",
      "East.Midlands",
      "West.Midlands",
      "East.of.England",
      "London",
      "Inner.London",
      "Outer.London",
      "South.East",
      "South.West",
      "England.2"
    ))

# write.csv(bdsLAOnly, "LA_Only_long.csv")

# England2Filter

bdsNational2 <- bdslong |>
  dplyr::filter(`LA and Regions` == "England.2")

write.csv(bdsNational2, "National2_Only_long.csv")
