
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(lubridate)
library(rmarkdown)
library(plotly)
library(RColorBrewer)
library(ggthemes)
library(tidyr)
library(insee)
library(graffiti)

options(warn=-1)

Print = function(x){
  obj_name = deparse(substitute(x))
  cat(file = stderr(), obj_name, ":", x, "\n")
}

idbank_list = get_idbank_list2()
# idbank_list = insee:::idbank_list
dataset_list = insee:::dataset_list_internal
# dataset_list = insee::get_dataset_list()

list_order_stopped_series = c('FALSE', 'TRUE')

id = insee:::idbank_list_internal %>% 
  mutate(title_en_upper = toupper(title_en)) %>% 
  mutate(SERIE_ARRETEE = SERIE_ARRETEE) %>% 
  mutate(SERIE_ARRETEE2 = case_when(str_detect(title_en_upper, 'STOPPED SERIES') ~ 'TRUE',
                                    is.na(SERIE_ARRETEE) ~ 'FALSE')) %>% 
  mutate(SERIE_ARRETEE2 = factor(SERIE_ARRETEE2, levels = list_order_stopped_series)) %>% 
  arrange(SERIE_ARRETEE2)

id_en = id %>%
  select(nomflow, idbank, cleFlow, title_en, dplyr::starts_with("dim")) %>% 
  dplyr::rename(title = title_en)

id_fr = id %>%
  select(nomflow, idbank, cleFlow, title_fr, dplyr::starts_with("dim")) %>% 
  dplyr::rename(title = title_fr)

idbank_list_all = id$idbank
idbank_list_all_label_en = paste(id$idbank, ":", id$title_en)
idbank_list_all_label_fr = paste(id$idbank, ":", id$title_fr)

dataset_list_selectize =
  dataset_list %>%
  filter(id %in% unique(idbank_list$nomflow))

dataset_list_id = dataset_list_selectize$id

dataset_list_selectize_fr = paste(dataset_list_selectize$id, ":", dataset_list_selectize$Name.fr)
dataset_list_selectize_en = paste(dataset_list_selectize$id, ":", dataset_list_selectize$Name.en)

label_table = data.frame(
  id = NA,
  label_fr = NA,
  label_en = NA,
  stringsAsFactors = FALSE)

label_table[nrow(label_table)+1,] = c("dshb_title", "graffiti", "graffiti")
label_table[nrow(label_table)+1,] = c("select_title_placeholder", "S\U00E9lectionner un titre", "Select title")
label_table[nrow(label_table)+1,] = c("menu_item1", "A la Une", "Headlines")
label_table[nrow(label_table)+1,] = c("menu_item2", "Catalogue des graphiques", "Plot store")
label_table[nrow(label_table)+1,] = c("menu_item3", "Graphique \U00E0 la demande", "Plot yourself")
label_table[nrow(label_table)+1,] = c("catalog",
                                      "Catalogue des graphiques - Cliquer sur une ligne pour faire apparaître le graphique",
                                      "Catalogue of plots - Click on one row to display the plot")

label_table[nrow(label_table)+1,] = c("slides_title", "Cr\u00E9er une pr\U00E9sentation", "Make your slides")
label_table[nrow(label_table)+1,] = c("slides_placeholder", "Ajouter le graphique à l'\u00E9cran", "Add the current plot")
label_table[nrow(label_table)+1,] = c("slides_download",
                                      "T\u00E9l\u00E9charger la pr\u00E9sentation",
                                      "Download slides")

label_table[nrow(label_table)+1,] = c("idbank_list",
                                      "Catalogue des s\u00E9ries - Cliquer sur une ou plusieurs s\U00E9ries pour afficher le graphique",
                                      "Catalogue of series - Click on one or several series to display the plot")
label_table[nrow(label_table)+1,] = c("dataset_list", "Choix du jeu de donn\u00E9es", "Choose a dataset")

label_table[nrow(label_table)+1,] = c("new_plot_title",
                                      "Nouveau graphique!",
                                      "Get a new plot!")

label_table[nrow(label_table)+1,] = c("plot_catalogue",
                                      "Graphique",
                                      "Plot")

label_table[nrow(label_table)+1,] = c("interactive_plot_title",
                                      "Graphique int\U00E9ractif",
                                      "Interactive Plot")

label_table[nrow(label_table)+1,] = c("downloadData_title",
                                      "Donn\u00E9es du graphique",
                                      "Download plot's data")

label_table[nrow(label_table)+1,] = c("slider_period",
                                      "Choix de la p\u00E9riode temporelle",
                                      "Choose the time period")

label_table[nrow(label_table)+1,] = c("deselect_idbank_in_list_title",
                                      "D\u00E9s\u00E9lectionner toutes les s\u00E9ries",
                                      "Deselect all series from the catalogue")

label_table[nrow(label_table)+1,] = c("one_plot_title",
                                      "Toutes les courbes sur un seul graphique",
                                      "All curves on one plot")

label_table[nrow(label_table)+1,] = c("data_table",
                                      "Donn\u00E9es",
                                      "Data")

label_table[nrow(label_table)+1,] = c("growth_button_title",
                                      "Choisir : ",
                                      "Choose : ")

label_table[nrow(label_table)+1,] = c("growth_button_raw",
                                      "Donn\u00E9es brutes",
                                      "Raw data")

label_table[nrow(label_table)+1,] = c("growth_button_yoy",
                                      "Taux de croissance annuel",
                                      "Yearly growth rate")

label_table[nrow(label_table)+1,] = c("growth_button_pop",
                                      "Taux de croissance trimestriel ou mensuel",
                                      "Quarterly or monthly growth rate")

label_table[nrow(label_table)+1,] = c("growth_button_qoq",
                                      "Taux de croissance trimestriel",
                                      "Quarterly growth rate")

label_table[nrow(label_table)+1,] = c("growth_button_mom",
                                      "Taux de croissance mensuel",
                                      "Monthly growth rate")

label_table[nrow(label_table)+1,] = c("growth_button_sos",
                                      "Taux de croissance semestriel",
                                      "Semesterly growth rate")

label_table[nrow(label_table)+1,] = c("growth_button_bob",
                                      "Taux de croissance bimensuel",
                                      "Bimonthly growth rate")

label_table[nrow(label_table)+1,] = c("warning_insee_data1",
                                      "Seules les donn\u00E9es disponibles\n ",
                                      "Only the data avaiable on insee.fr should be")

label_table[nrow(label_table)+1,] = c("warning_insee_data2",
                                      "sur insee.fr font foi!",
                                      "deemed as official statistics!")

label_table[nrow(label_table)+1,] = c("export_data",
                                      "Exporter les donn\u00E9es",
                                      "Export data")

label_table[nrow(label_table)+1,] = c("enhance_plot",
                                      "Am\u00E9liorer le graphique",
                                      "Export your plot")

label_table[nrow(label_table)+1,] = c("AppDescriptionText",
                                      "Ma super app",
                                      paste0("Graffiti is an experimental R-Shiny Application \n",
                                             "it helps users to have access to Insee macroeconomic data.",
                                             "Either tap into the plots catalogue, or make your own plot!"))




get_label = function(id, df = label_table, lang = "en"){

  row_df = which(df[,"id"] == id)

  if(length(row_df) > 0){
    if(lang == "en"){
      label = df[row_df, "label_en"]
    }else{
      label = df[row_df, "label_fr"]
    }
  }else{
    label = "label missing"
  }

  return(label)
}

plot_table = data.frame(
  id = NA,
  title_fr = NA,
  title_en = NA,
  dataset = NA,
  stringsAsFactors = FALSE)

plot_table[nrow(plot_table)+1,] = c("inflation_contributions",
                                    "Inflation et contributions",
                                    "Inflation and contributions",
                                    'IPC-2015')

plot_table[nrow(plot_table)+1,] = c("inflation_contributions_energy",
                                    "Inflation, contributions et \u00E9nergie",
                                    "Inflation, contributions and energy",
                                    'IPC-2015')

plot_table[nrow(plot_table)+1,] = c("enquete_indus",
                                      "Enqu\U00EAtes dans l'industrie",
                                      "Surveys in industry",
                                    NA)

plot_table[nrow(plot_table)+1,] = c("pib_growth_qoq",
                                     "PIB - taux de croissance trimestriel",
                                     "GDP growth rate quarter-on-quarter",
                                    NA)

plot_table[nrow(plot_table)+1,] = c("icp_growth_yoy",
                                    "Inflation au niveau coicop 2, glissement annuel",
                                    "Inflation at coicop level 2, yearly growth rate",
                                    "IPC-2015")

plot_table[nrow(plot_table)+1,] = c("death_birth",
                                    "D\u00E9c\U00E8s et naissances en France depuis 1948",
                                    "Deaths and Births in France since 1948",
                                    "DECES-MORTALITE")

plot_table[nrow(plot_table)+1,] = c("immat",
                                    "Immatriculations de voitures particuli\U00E8res neuves",
                                    "New passenger cars registrations",
                                    "TRANSPORTS")

plot_table[nrow(plot_table)+1,] = c("ippmp",
                                    "Indices des prix des mati\U00E8res premi\U00E8res import\u00E9es en France",
                                    "Imported raw materials prices index in France",
                                    '"IPPMP-NF"')

plot_table[nrow(plot_table)+1,] = c("gov_debt",
                                    "Dette des administrations publiques, crit\u00E9res de Maastricht",
                                    "Debt of the general government, Maastricht definition",
                                    'DETTE-TRIM-APU-2014')

plot_table[nrow(plot_table)+1,] = c("supermarket",
                                    "Chiffres d'affaires des enseignes de grande distribution",
                                    "Sales in supermarkets",
                                    "ICA-2015-EMAGSA")

plot_table[nrow(plot_table)+1,] = c("survey_household",
                                    "Enqu\U00EAtes de conjoncture au pr\u00E8s des m\u00E9nages",
                                    "Households' survey about economic outlook",
                                    "ENQ-CONJ-MENAGES")

# plot_table[nrow(plot_table)+1,] = c("pop_map",
#                                     "R\u00E9partition de la population sur le territoire - densit\u00E9 par kilom\u00E8tre carr\u00E9",
#                                     "Distribution of the population within French territory",
#                                     "TCRED-ESTIMATIONS-POPULATION")

# plot_table[nrow(plot_table)+1,] = c("elect_map",
#                                     "Taux de participation \U00E0 la derni\u00E8re \u00E9lection pr\u00E9sidentielle - premier tour",
#                                     "Rate of participation to the last presidential election - first round",
#                                     "TCRED-CONDITIONSDEVIE-TXP-CDE")

plot_table[nrow(plot_table)+1,] = c("pib_growth_contrib_aoa",
                                    "PIB - contribution des composantes \U00E0 l'\u00E9volution annuelle en %",
                                    "GDP - growth contriutions year-over-year, percentage",
                                    NA)

plot_table[nrow(plot_table)+1,] = c("pib_current_price",
                                    "PIB courant - d\u00E9composition, milliards d'euros",
                                    "GDP current prices, billions euros",
                                    "CNT-2014-PIB-EQB-RF"
                                    )

plot_table[nrow(plot_table)+1,] = c("rdb_pvr_achat",
                                    "Revenu disponible brut des m\u00E9nages - pouvoir d'achat",
                                    "Gross disposable income of households - purchasing power",
                                    "CNA-2014-RDB")

plot_table[nrow(plot_table)+1,] = c("housing_construction",
                                    "Construction de logements - nombre de logements commenc\u00E9s - cumul sur 12 mois",
                                    "Housing construction - started dwellings - 12-month aggregate",
                                    "CONSTRUCTION-LOGEMENTS")

plot_table[nrow(plot_table)+1,] = c("conso_produit_val",
                                    "Consommation effective des m\u00E9nages - prix courant",
                                    "Actual household consumption - current price",
                                    "CNA-2014-CONSO-MEN")

plot_table[nrow(plot_table)+1,] = c("air_traffic",
                                    'Vols nationaux et internationaux',
                                    'Internal and international flights',
                                    'TRANSPORTS')



plot_table = plot_table[which(!is.na(plot_table[,"id"])),]

link_app_plot = system.file("shiny/graffiti-app/plot", package = "graffiti")

list_file = file.path(link_app_plot, list.files(link_app_plot, pattern= ".R$"))
for(plot_file in list_file){
  source(plot_file)
}

slides_rmd_file = system.file("shiny/graffiti-app/slides.Rmd", package = "graffiti")

# slides_rmd_file = system.file("./inst/shiny/graffiti-app/slides.Rmd", package = "graffiti")
#
# last_release =
#   get_last_release() %>%
#   mutate(pub_date = lubridate::dmy(substr(pubDate, 6, 16))) %>%
#   mutate(dataset = substr(title, 1, stringr::str_locate(title, "]"))) %>%
#   mutate(dataset = gsub("\\[|\\]", "", dataset))
#

# convertMenuItem <- function(mi,tabName) {
#   mi$children[[1]]$attribs['data-toggle']="tab"
#   mi$children[[1]]$attribs['data-value'] = tabName
#   mi
# }

