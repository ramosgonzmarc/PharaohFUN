#.libPaths("/home/marcos/R/x86_64-pc-linux-gnu-library/4.1")

## app.R ##
library(shinydashboard)
library(bslib)
library(gridlayout)
#library(shinyWidgets)
#library(semantic.dashboard)
library(DT)
library(ape)
#library(seqinr)
library(msaR)
# Functions


## PFAM link
## https://www.ebi.ac.uk/interpro/entry/pfam/PF02728
pfam.link <- function(pfam.term)
{
  pfam.red.term <- strsplit(pfam.term, split = "[.]")[[1]][1]
  link <- paste0("https://www.ebi.ac.uk/interpro/entry/pfam/", pfam.red.term)
  complete.link <- paste(c("<a href=\"",
                           link,
                           "\" target=\"_blank\">",
                           pfam.term, "</a>"),
                         collapse = "")
  return(complete.link)
}


# Create data frame

column1 <-c("Phaeodactylum tricornutum","Nannochloropsis gaditana", "Saccharina japonica",
                "Guillardia theta", "Cryptophyceae CCMP2293") 
column2 <- c("Cyanidioschyzon merolae",
                "Galdieria sulphuraria", "Gracilariopsis chorda", "Porphyra umbilicalis")
column3 <- c("Cyanophora paradoxa")
column4 <- c("Ostreococcus tauri", "Bathycoccus prasinos","Micromonas pusilla")
column5 <- c("Ulva mutabilis", "Coccomyxa subellipsoidea",
                "Chromochloris zofingiensis", "Scenedesmus obliquus", "Raphidocelis subcapitata",
                "Chlamydomonas reinhardtii", "Volvox carteri", "Dunaliella salina",
                "Haematococcus lacustris")
column6 <- c("Mesostigma viride", "Chlorokybus atmophyticus",
                "Klebsormidium nitens", "Chara braunii", "Spirogloea muscicola",
                "Mesotaenium endlicherianum")
column7 <- c("Marchantia polymorpha", "Sphagnum magellanicum",
                "Physcomitrium patens", "Ceratodon purpureus", "Anthoceros agrestis")
column8 <- c("Selaginella moellendorffii", "Ceratopteris richardii", "Salvinia cucullata",
                "Azolla filiculoides")
column9 <- c("Thuja plicata", "Cycas panzhihuaensis","Aegilops tauschii", "Triticum aestivum", "Oryza sativa", 
                "Sorghum bicolor", "Zea mays", "Solanum lycopersicum", "Arabidopsis thaliana")

organisms_values <- c("pt","ng", "saccha", "guilla", "crypto", "cymero", 
                      "galsul", "gracichor", "pu", "cyano","ot", "bp", "mi",
                      "um", "cocco", "cz", "sceobli", "rs", "cr", "vc", "ds", "haema",
                      "mv", "ca", "kn", "chara", "sp", "me", 
                      "mp", "smag", "pp", "cp", "aa", "sm", "cri", "sc", "af",
                      "tp", "cyc", "aegi", "ta", "os", "sb","zm", "sl", "at")

names(organisms_values) <- c(column1, column2, column3, column4, column5, column6,
                             column7, column8, column9)

column_names_upper <- c(toupper(letters[1:26]))
column_names_lower <- c(tolower(letters[1:26]))
column_names = c(column_names_upper, column_names_lower)
df <- data.frame(replicate(length(column_names),sample(0:1,1000,rep=TRUE)))

# assign column names
colnames(df) = column_names

ui <- dashboardPage(

  dashboardHeader( #disable = TRUE
    title = "PharaohFUN",
                  dropdownMenu(badgeStatus = "warning", icon = icon("question"), headerText = "", type = "notifications",
                               notificationItem(
                                 text = "A tutorial is available at",
                                 icon("question"),
                                 status = "success"
                               )),
                  dropdownMenu(headerText = "", icon = icon("address-book"), type = "notifications",
                               notificationItem(
                                 text = "Contact us at",
                                 icon = icon("question")
                               ))
                  
                               
                  ),
  dashboardSidebar(
    ## Sidebar content
    dashboardSidebar( 
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("house")),
        menuItem("Gene ID from organisms list", icon = icon("dna"), tabName = "gene_search"),
        menuItem("Sequence from organisms list", icon = icon("a"), tabName = "seq_search"),
        menuItem("Existing Orthogroup ID", icon = icon("dashboard"), tabName = "og_id_search"),
        menuItem("Batch mode", icon = icon("layer-group"), tabName = "batch_search"),
        menuItem("Sequence from new organism", icon = icon("leaf", lib = "glyphicon"), tabName = "shoot_search"),
        menuItem("Source code", icon = icon("code"), 
                 href = "https://github.com/fran-romero-campero/AlgaeFUN")
      )
    )
  ),
  dashboardBody(
    ## Body content
    
    
      tabItems(
        # First tab content
        tabItem(tabName = "home",
                #tags$head(tags$style(HTML("a {color: white}"))),
                h2("PharaohFUN: Phylogenomic Analysis for Plant Protein History and Function Elucidation"),
                br(),
                fluidRow(
                valueBox(a("Gene ID", href="#shiny-tab-gene_search", "data-toggle" = "tab", "style" = "color:white"), 
                         "Gene ID from one of the listed organisms",
                         icon = icon("dna"), width = 4),
                
                valueBox(a("Sequence", href="#shiny-tab-seq_search", "data-toggle" = "tab", "style" = "color:white"), 
                         "Protein sequence from one of the listed organisms",
                         icon = icon("a"), width = 4, color = "lime"),
                
                valueBox(a("Orthogroup ID", href="#shiny-tab-og_id_search", "data-toggle" = "tab", "style" = "color:white"), 
                         "Orthogroup ID (from STRING result)",
                         icon = icon("dashboard"), width = 4, color = "red")
                ),
                
                fluidRow(
                  
                  column(2),
                  valueBox(a("Batch mode", href="#shiny-tab-batch_search", "data-toggle" = "tab", "style" = "color:white"), 
                           "Sequences set from one of the listed organisms",
                           icon = icon("layer-group"), width = 4, color = "orange"),
                  
                  valueBox(a("New organism", href="#shiny-tab-shoot_search", "data-toggle" = "tab", "style" = "color:white"), 
                           "Protein sequence from any organism",
                           icon = icon("leaf", lib = "glyphicon"), width = 4, color = "purple"),
                  br(),
                  br(),
                  fluidRow(br()),
                  
                  box(status = "primary", width = 12, title = "",
                      
                      fluidRow(),
                      img(
                        src = "species_gg46.png",
                        alt = "streptophytes",
                        width = 900,
                        height = 550, style="display: block; margin-left: auto; margin-right: auto;"
                      )
                      )
                  
                )
        ),
       
        # Second tab content
        tabItem(tabName = "gene_search",
                h2(""),
                fluidRow(valueBox("Gene ID-based search", 
                         subtitle = "Single gene, available organism",
                         icon = icon("dna"), width = 6)),
                br(),
                box(
                  title = span(tags$b("Organism selection"), style = "color:#34c5d1; font-size: 20px; "), status = "info", width = "500",
                  "Please select the desired organisms from the following list for performing the analysis.
                  Please take care of selecting the organism whose gene ID is being inputted. Organisms in 
                  green belong to Viridiplantae, while other colors indicate groups outside this clade.", br(), br(),
                  column(3,
                         checkboxGroupInput(
                           "tsar_check_1",
                           p("Cryptophytes and TSAR", class= "h4",
                             tags$img(
                               src = "phaeodactylum.png",
                               alt = "streptophytes",
                               width = 25,
                               height = 25, style = "margin-left: 10px;"
                             )
                           ),
                           choices = column1,
                           inline = F,
                           selected = NULL),
                         checkboxGroupInput(
                           "rhodo_check_1",
                           p("Rhodophytes", class= "h4",
                             tags$img(
                               src = "porphyra.png",
                               alt = "rhodophyta",
                               width = 25,
                               height = 25, style = "margin-left: 10px;"
                             )
                           ),
                           choices = column2,
                           inline = F,
                           selected = NULL),
                         checkboxGroupInput(
                           "glauco_check_1",
                           p("Glaucophytes", class= "h4",
                             tags$img(
                               src = "cyanophora.png",
                               alt = "glaucophytes",
                               width = 18,
                               height = 30, style = "margin-left: 10px;"
                             )
                           ),
                           choices = column3,
                           inline = F,
                           selected = NULL)
                         
                  ),
                  column(3,
                         checkboxGroupInput(
                           "mami_check_1",
                           p("Mamiellophyceae", class= "h4",
                             tags$img(
                               src = "bathycoccus.png",
                               alt = "mamiellales",
                               width = 20,
                               height = 22, style = "margin-left: 10px;"
                             )
                           ),
                           choices = column4,
                           inline = F,
                           selected = NULL),
                         
                         checkboxGroupInput(
                           "chloro_check_1",
                           p("Other Chlorophytes", class= "h4",
                             tags$img(
                               src = "scenedesmus.png",
                               alt = "chlorophytes",
                               width = 50,
                               height = 25, style = "margin-left: 10px;"
                             )
                           ),
                           choices = column5,
                           inline = F,
                           selected = NULL)
                  ),
                  column(3,
                         checkboxGroupInput(
                           "strepto_check_1",
                           p("Streptophyte algae", class= "h4",
                             tags$img(
                               src = "klebsormidium.png",
                               alt = "streptophytes",
                               width = 25,
                               height = 25, style = "margin-left: 10px;"
                             )
                           ),
                           choices = column6,
                           inline = F,
                           selected = NULL),
                         
                         checkboxGroupInput(
                           "bryo_check_1",
                           p("Bryophytes", class= "h4",
                             tags$img(
                               src = "marchantia.png",
                               alt = "bryophyta",
                               width = 32,
                               height = 25, style = "margin-left: 10px;"
                             )
                           ),
                           choices = column7,
                           inline = F,
                           selected = NULL)
                  ),
                  
                  column(3,
                         checkboxGroupInput(
                           "lyco_check_1",
                           p("Lycophytes and Ferns", class= "h4",
                             tags$img(
                               src = "selaginella.png",
                               alt = "lycophyta",
                               width = 50,
                               height = 25, style = "margin-left: 10px;"
                             )
                           ),
                           
                           choices = column8,
                           inline = F,
                           selected = NULL),
                         
                         checkboxGroupInput(
                           "sperma_check_1",
                           p("Spermatophyta", class= "h4",
                             tags$img(
                               src = "arabidopsis.png",
                               alt = "spermatophyta",
                               width = 25,
                               height = 25, style = "margin-left: 10px;"
                             )
                           ),
                           choices = column9,
                           inline = F,
                           selected = NULL)
                  ),
                  
                  fluidRow(br()),
                  fluidRow(br()),
                  
                  span(tags$b("Tree building method"), style = "color:#34c5d1; font-size: 20px; "),
                  div(br()),
                  
                  div("The first step in any study using PharaohFUN is the construction of the 
                    gene tree corresponding to the selected orthogroup. For this, four standard methods 
                    are offered, select one before continuing. Default is performed with FastTree using Orthofinder 2
                    pipeline, which renders trees by aproximate maximum-likelihood and then reconciliate
                    gene trees with the species tree shown in the Home tab. For the other three methods,
                    no reconciliation is performed but support for each branch is shown as bootstrap 
                    values."),
                  
                  
                  
                  
                  shinyWidgets::awesomeRadio(
                    inputId = "build_trees_1",
                    label = "", 
                    choices = c("FastTree", "Neighbour Joining", "UPGMA", "Parsimony"),
                    selected = "FastTree",
                    inline = TRUE, 
                    status = "info"
                  ),
                  
                  fluidRow(br()),
                  
                  span(tags$b("Insert gene ID"), style = "color:#34c5d1; font-size: 20px; "),
                  div(br()),
                  
                  div("Below you can write the ID associated to the protein whose evolutionary history you
                             wish to analyze. Then, select a model: Global or Viridiplantae (default
                             is Viridiplantae). Note that the groups TSAR
                             and Cryptophytes, Rhodophytes and Glaucophytes do not belong to Viridiplantae, so they
                             will be ignored in case this is the selected model."),
                
                  fluidRow(
                    column(5, textInput("geneInt1", label = "", 
                                        width = "100%", placeholder = "AT2G46830")),
                   
                    column(1, div( style = "margin-top: 20px;", 
                                   shinyWidgets::actionBttn("run_button1", "Run", size = "sm", icon = icon("magnifying-glass"),
                                                            style = "float", color = "primary"))),
                  column(3,div(style = "margin-top: 24px;",
                               shinyWidgets::materialSwitch(inputId = "switch1", label = "Global", 
                                                            value = T, status = "info", inline = TRUE),
                               span("Viridiplantae")))
                )
                
                ),
                
                br(),
                fluidRow(
                  box(status = "info", width = 12, 
                      title = span(tags$b("Results"), style = "color:#34c5d1; font-size: 20px; "),
                      "Results for the query gene and species are displayed below, arranged in 
                      different tabs. The execution of each analysis is initiated from the start 
                      button within each of the tabs, with specific instructions for each analysis.",
                      div(br()),
                      tabsetPanel(type = "tabs",
                                  tabPanel("Genes Tree",
                                           fluidRow(br()),
                                           shinyjs::useShinyjs(),
                                           shinyjs::hidden(div(id='loading.tree1',h3('Please be patient, building tree ...'))),
                                           uiOutput(outputId = "error_tree1"),
                                           splitLayout(cellWidths = c("50%", "50%"),
                                                       tags$div(id = "boxouttext1"), tags$div(id = "boxouttext2")),
                                           fluidRow(tags$br()),
                                           fluidRow(tags$div(id = "boxout1")),
                                           splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                       tags$div(id = "download_tree1"),
                                                       tags$div(id = "download_newick1"),
                                                       tags$div(id = "download_tree_seqs1"))
                                           ),
                                  tabPanel("Expansion/Contraction",
                                            fluidRow(tags$br()),
                                            shinyWidgets::actionBttn("cafe_start1", "Show Evolutionary History",
                                                                    size = "sm", icon = icon("magnifying-glass"),
                                                                    style = "float", color = "primary"),
                                           fluidRow(tags$br()),
                                          
                                           shinyjs::hidden(div(id='loading.cafe1', h3('Please be patient, reconstructing expansion/contraction events ...'))),
                                           tags$div(id = "error_cafe1"),
                                           tags$div(id = "box_mrca1"),
                                           tags$br(),
                                           tags$div(id = "box_cafe1"),
                                           fluidRow(tags$br()),
                                           splitLayout(cellWidths = c("50%", "50%"),
                                           tags$div(id = "cafe_down_button1"),
                                           tags$div(id = "download_ui_for_cafe_plot1"))
                                           ),
                                  tabPanel("PFAM Domains", 
                                           fluidRow(tags$br()),
                                           shinyWidgets::actionBttn("pfam_start1", "Show Gene Selection for Pfam",
                                                                    size = "sm", icon = icon("magnifying-glass"),
                                                                    style = "float", color = "primary"),
                                           fluidRow(tags$br()),
                                           tags$div(id = "selected_pfams1"),
                                           fluidRow(tags$br()),
                                           tags$div(id = "pfam_selectionI1"),
                                           fluidRow(tags$br()),
                                           shinyjs::hidden(div(id='loading.pfam.pf1',h3('Please be patient, identifying domains ...'))),
                                           uiOutput(outputId = "error_pfam1"),
                                           tags$div(id = "box_pfam1"),
                                           tags$br(),
                                           tags$div(id = "box_pfplot1"),
                                           fluidRow(tags$br()),
                                           splitLayout(cellWidths = c("50%", "50%"), 
                                                      tags$div(id = "pfam_down_button1"),
                                                      tags$div(id = "download_ui_for_pfam_table1")
                                                      )
                                           ),
                                  tabPanel("Multiple Sequence Alignment",
                                           fluidRow(tags$br()),
                                           shinyWidgets::actionBttn("msa_start1", "Show Gene Selection for Pfam",
                                                                    size = "sm", icon = icon("magnifying-glass"),
                                                                    style = "float", color = "primary"),
                                           fluidRow(tags$br()),
                                           tags$div(id = "selected_msa1"),
                                           fluidRow(tags$br()),
                                           tags$div(id = "msa_selectionI1"),
                                           fluidRow(tags$br()),
                                           shinyjs::hidden(div(id='loading.msa1',h3('Please be patient, aligning sequences ...'))),
                                           uiOutput(outputId = "error_msa1"),
                                           tags$div(id = "box_msa1"),
                                           fluidRow(tags$br()),
                                           # splitLayout(cellWidths = c("50%", "50%"), 
                                           #             tags$div(id = "pfam_down_button1"),
                                           #             tags$div(id = "download_ui_for_pfam_table1"))
                                           ),
                                  tabPanel("GO Terms", "First tab content"),
                                  tabPanel("KEGG Orthology", "Tab content 2"),
                                  tabPanel("STRING Interactions", "First tab content"),
                                  tabPanel("Literature Annotation", "Tab content 2")
                      ))
                )
        ),
                
                 
        # Third tab content
        tabItem(tabName = "seq_search",
                h2(""),
                fluidRow(valueBox("Sequence-based search", 
                                  subtitle = "Single gene, available organism",
                                  icon = icon("a"), width = 6, color = "lime")),
                br(),
              box(
                title = span(tags$b("Organism selection"), style = "color:#25d04a; font-size: 20px; "), status = "success", width = "500",
                "Please select the desired organisms from the following list for performing the analysis.
                  Please take care of selecting the organism whose gene ID is being inputted.", br(), br(),
                column(3,
                       checkboxGroupInput(
                         "tsar_check_2",
                         p("Cryptophytes and TSAR", class= "h4",
                           tags$img(
                             src = "phaeodactylum.png",
                             alt = "streptophytes",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column1,
                         inline = F,
                         selected = NULL),
                       checkboxGroupInput(
                         "rhodo_check_2",
                         p("Rhodophytes", class= "h4",
                           tags$img(
                             src = "porphyra.png",
                             alt = "rhodophyta",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column2,
                         inline = F,
                         selected = NULL),
                       checkboxGroupInput(
                         "glauco_check_2",
                         p("Glaucophytes", class= "h4",
                           tags$img(
                             src = "cyanophora.png",
                             alt = "glaucophytes",
                             width = 18,
                             height = 30, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column3,
                         inline = F,
                         selected = NULL)
                       
                ),
                column(3,
                       checkboxGroupInput(
                         "mami_check_2",
                         p("Mamiellophyceae", class= "h4",
                           tags$img(
                             src = "bathycoccus.png",
                             alt = "mamiellales",
                             width = 20,
                             height = 22, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column4,
                         inline = F,
                         selected = NULL),
                       
                       checkboxGroupInput(
                         "chloro_check_2",
                         p("Other Chlorophytes", class= "h4",
                           tags$img(
                             src = "scenedesmus.png",
                             alt = "chlorophytes",
                             width = 50,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column5,
                         inline = F,
                         selected = NULL)
                ),
                column(3,
                       checkboxGroupInput(
                         "strepto_check_2",
                         p("Streptophyte algae", class= "h4",
                           tags$img(
                             src = "klebsormidium.png",
                             alt = "streptophytes",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column6,
                         inline = F,
                         selected = NULL),
                       
                       checkboxGroupInput(
                         "bryo_check_2",
                         p("Bryophytes", class= "h4",
                           tags$img(
                             src = "marchantia.png",
                             alt = "bryophyta",
                             width = 32,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column7,
                         inline = F,
                         selected = NULL)
                ),

                column(3,
                       checkboxGroupInput(
                         "lyco_check_2",
                         p("Lycophytes and Ferns", class= "h4",
                           tags$img(
                             src = "selaginella.png",
                             alt = "lycophyta",
                             width = 50,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         
                         choices = column8,
                         inline = F,
                         selected = NULL),
                       
                       checkboxGroupInput(
                         "sperma_check_2",
                         p("Spermatophyta", class= "h4",
                           tags$img(
                             src = "arabidopsis.png",
                             alt = "spermatophyta",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column9,
                         inline = F,
                         selected = NULL)
                ),
                
                fluidRow(br()),
                
                span(tags$b("Insert a protein chain"), style = "color:#25d04a; font-size: 20px; "),
                br(),
                br(),
                div("Below you can paste the sequence of the protein whose evolutionary history you
                             wish to analyze. After pasting it, select a model: Global or Viridiplantae (default
                             is Viridiplantae). Note that the groups TSAR
                             and Cryptophytes, Rhodophytes and Glaucophytes do not belong to Viridiplantae, so they
                             will be ignored in case this is the selected model."),
                fluidRow(
                  column(8, 
                  textAreaInput(inputId = "geneInt2", 
                                #label= span(tags$b("Insert a protein chain"), style = "color:#25d04a; font-size: 20px; "),
                                  "", 
                                width="200%", height = "200px", 
                                value= "", resize = "vertical",
                                placeholder = 
                                "METNSSGEDLVIKTRKPYTITKQRERWTEEEHNRFIEALRLYGRAWQKIEEHVATKTAVQ...")),
                  
                  column(1, div( style = "margin-top: 188px;", 
                                 shinyWidgets::actionBttn("run_button2", "Run", size = "sm", icon = icon("magnifying-glass"),
                                                          style = "float", color = "success"))),
                  column(3,div(style = "margin-top: 192px;",
                               shinyWidgets::materialSwitch(inputId = "switch2", label = "Global", 
                                                            value = T, status = "success", inline = TRUE),
                               span("Viridiplantae")))
                  )
                ),
              
              br(),
              fluidRow(
                box(status = "success", width = 12, 
                    title = span(tags$b("Results"), style = "color:#25d04a; font-size: 20px; "),
                    tabsetPanel(type = "tabs",
                                tabPanel("Genes Tree", "First tab content"),
                                tabPanel("Expansion/Contraction", "Tab content 2"),
                                tabPanel("PFAM Domains", "First tab content"),
                                tabPanel("Multiple Sequence Alignment", "Tab content 2"),
                                tabPanel("GO Terms", "First tab content"),
                                tabPanel("KEGG Orthology", "Tab content 2"),
                                tabPanel("STRING Interactions", "First tab content"),
                                tabPanel("Literature Annotation", "Tab content 2")
                    ))
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "og_id_search",
              h2(""),
              fluidRow(valueBox("Orthogroup ID-based search", 
                                subtitle = "Single orthogroup, available organism",
                                icon = icon("dashboard"), width = 6, color = "red")),
              br(),
              box(
                title = span(tags$b("Organism selection"), style = "color:#d5251d; font-size: 20px; "), status = "danger", width = "500",
                "Please select the desired organisms from the following list for performing the analysis.
                  Please take care of selecting organisms that are present in the selected orthogroup.", br(), br(),
                column(3,
                       checkboxGroupInput(
                         "tsar_check_3",
                         p("Cryptophytes and TSAR", class= "h4",
                           tags$img(
                             src = "phaeodactylum.png",
                             alt = "streptophytes",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column1,
                         inline = F,
                         selected = NULL),
                       checkboxGroupInput(
                         "rhodo_check_3",
                         p("Rhodophytes", class= "h4",
                           tags$img(
                             src = "porphyra.png",
                             alt = "rhodophyta",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column2,
                         inline = F,
                         selected = NULL),
                       checkboxGroupInput(
                         "glauco_check_3",
                         p("Glaucophytes", class= "h4",
                           tags$img(
                             src = "cyanophora.png",
                             alt = "glaucophytes",
                             width = 18,
                             height = 30, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column3,
                         inline = F,
                         selected = NULL)
                       
                ),
                column(3,
                       checkboxGroupInput(
                         "mami_check_3",
                         p("Mamiellophyceae", class= "h4",
                           tags$img(
                             src = "bathycoccus.png",
                             alt = "mamiellales",
                             width = 20,
                             height = 22, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column4,
                         inline = F,
                         selected = NULL),
                       
                       checkboxGroupInput(
                         "chloro_check_3",
                         p("Other Chlorophytes", class= "h4",
                           tags$img(
                             src = "scenedesmus.png",
                             alt = "chlorophytes",
                             width = 50,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column5,
                         inline = F,
                         selected = NULL)
                ),
                column(3,
                       checkboxGroupInput(
                         "strepto_check_3",
                         p("Streptophyte algae", class= "h4",
                           tags$img(
                             src = "klebsormidium.png",
                             alt = "streptophytes",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column6,
                         inline = F,
                         selected = NULL),
                       
                       checkboxGroupInput(
                         "bryo_check_3",
                         p("Bryophytes", class= "h4",
                           tags$img(
                             src = "marchantia.png",
                             alt = "bryophyta",
                             width = 32,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column7,
                         inline = F,
                         selected = NULL)
                ),
                
                column(3,
                       checkboxGroupInput(
                         "lyco_check_3",
                         p("Lycophytes and Ferns", class= "h4",
                           tags$img(
                             src = "selaginella.png",
                             alt = "lycophyta",
                             width = 50,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         
                         choices = column8,
                         inline = F,
                         selected = NULL),
                       
                       checkboxGroupInput(
                         "sperma_check_3",
                         p("Spermatophyta", class= "h4",
                           tags$img(
                             src = "arabidopsis.png",
                             alt = "spermatophyta",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column9,
                         inline = F,
                         selected = NULL)
                ),
                
                
                fluidRow(
                  column(5, textInput("geneInt3", label = h4("Insert Orthogroup ID"), width = "100%", placeholder = "OG0001709")),
                  #column(1, div( style = "margin-top: 20px;", actionButton("run", "Run", icon = icon("magnifying-glass")))))
                  column(1, div( style = "margin-top: 45px;", 
                                 shinyWidgets::actionBttn("run_button3", "Run", size = "sm", icon = icon("magnifying-glass"),
                                                          style = "float", color = "danger"))),
                  column(3,div(style = "margin-top: 49px;",
                               shinyWidgets::materialSwitch(inputId = "switch3", label = "Global", 
                                                            value = T, status = "danger", inline = TRUE),
                               span("Viridiplantae")))
                  
                  
                  
                )),
              
              br(),
              fluidRow(
                box(status = "danger", width = 12,
                    title = span(tags$b("Results"), style = "color:#d5251d; font-size: 20px; "),
                    tabsetPanel(type = "tabs",
                                tabPanel("Genes Tree", "First tab content"),
                                tabPanel("Expansion/Contraction", "Tab content 2"),
                                tabPanel("PFAM Domains", "First tab content"),
                                tabPanel("Multiple Sequence Alignment", "Tab content 2"),
                                tabPanel("GO Terms", "First tab content"),
                                tabPanel("KEGG Orthology", "Tab content 2"),
                                tabPanel("STRING Interactions", "First tab content"),
                                tabPanel("Literature Annotation", "Tab content 2")
                    ))
              )
      ),
      
      # Fifth tab content
      tabItem(tabName = "batch_search",
              h2(""),
              fluidRow(valueBox("Batch mode search", 
                                subtitle = "Set of genes, available organism",
                                icon = icon("layer-group"), width = 6, color = "orange")),
              br(),
              box(
                title = span(tags$b("Organism selection"), style = "color:#e37326; font-size: 20px; "), status = "warning", width = "500",
                "Please select the desired organisms from the following list for performing the analysis.
                  Please take care of selecting the organism whose sequences are being used as input.", br(), br(),
                column(3,
                       checkboxGroupInput(
                         "tsar_check_4",
                         p("Cryptophytes and TSAR", class= "h4",
                           tags$img(
                             src = "phaeodactylum.png",
                             alt = "streptophytes",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column1,
                         inline = F,
                         selected = NULL),
                       checkboxGroupInput(
                         "rhodo_check_4",
                         p("Rhodophytes", class= "h4",
                           tags$img(
                             src = "porphyra.png",
                             alt = "rhodophyta",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column2,
                         inline = F,
                         selected = NULL),
                       checkboxGroupInput(
                         "glauco_check_4",
                         p("Glaucophytes", class= "h4",
                           tags$img(
                             src = "cyanophora.png",
                             alt = "glaucophytes",
                             width = 18,
                             height = 30, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column3,
                         inline = F,
                         selected = NULL)
                       
                ),
                column(3,
                       checkboxGroupInput(
                         "mami_check_4",
                         p("Mamiellophyceae", class= "h4",
                           tags$img(
                             src = "bathycoccus.png",
                             alt = "mamiellales",
                             width = 20,
                             height = 22, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column4,
                         inline = F,
                         selected = NULL),
                       
                       checkboxGroupInput(
                         "chloro_check_4",
                         p("Other Chlorophytes", class= "h4",
                           tags$img(
                             src = "scenedesmus.png",
                             alt = "chlorophytes",
                             width = 50,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column5,
                         inline = F,
                         selected = NULL)
                ),
                column(3,
                       checkboxGroupInput(
                         "strepto_check_4",
                         p("Streptophyte algae", class= "h4",
                           tags$img(
                             src = "klebsormidium.png",
                             alt = "streptophytes",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column6,
                         inline = F,
                         selected = NULL),
                       
                       checkboxGroupInput(
                         "bryo_check_4",
                         p("Bryophytes", class= "h4",
                           tags$img(
                             src = "marchantia.png",
                             alt = "bryophyta",
                             width = 32,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column7,
                         inline = F,
                         selected = NULL)
                ),
                
                column(3,
                       checkboxGroupInput(
                         "lyco_check_4",
                         p("Lycophytes and Ferns", class= "h4",
                           tags$img(
                             src = "selaginella.png",
                             alt = "lycophyta",
                             width = 50,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         
                         choices = column8,
                         inline = F,
                         selected = NULL),
                       
                       checkboxGroupInput(
                         "sperma_check_4",
                         p("Spermatophyta", class= "h4",
                           tags$img(
                             src = "arabidopsis.png",
                             alt = "spermatophyta",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column9,
                         inline = F,
                         selected = NULL)
                ),
                
                
                fluidRow(
                  column(5, fileInput(inputId = "geneInt4",label = h4("Choose Sequence File to Upload")),),
                  #column(1, div( style = "margin-top: 20px;", actionButton("run", "Run", icon = icon("magnifying-glass")))))
                  column(1, div( style = "margin-top: 45px;", 
                                 shinyWidgets::actionBttn("run_button4", "Run", size = "sm", icon = icon("magnifying-glass"),
                                                          style = "float", color = "warning"))),
                  column(3,div(style = "margin-top: 49px;",
                               shinyWidgets::materialSwitch(inputId = "switch4", label = "Global", 
                                                            value = T, status = "warning", inline = TRUE),
                               span("Viridiplantae")))
                  
                  
                  
                )),
              
              br(),
              fluidRow(
                box(status = "warning", width = 12, 
                    title = span(tags$b("Results"), style = "color:#e37326; font-size: 20px; ")
                    ))
              
      ),
      
      # Sixth tab content
      tabItem(tabName = "shoot_search",
              h2(""),
              fluidRow(valueBox("New organism sequence search", 
                                subtitle = "Single gene, custom organism",
                                icon = icon("leaf", lib = "glyphicon"), width = 6, color = "purple")),
              br(),
              box(
                title = span(tags$b("Organism selection"), style = "color:#5e3587; font-size: 20px; "), status = "primary", width = "500",
                "Please select the desired organisms from the following list for performing the analysis.
                  It is recommended to select organisms that span the evolutionary placement of the custom
                organism to allow for better resolution of the created tree.", br(), br(),
                column(3,
                       checkboxGroupInput(
                         "tsar_check_5",
                         p("Cryptophytes and TSAR", class= "h4",
                           tags$img(
                             src = "phaeodactylum.png",
                             alt = "streptophytes",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column1,
                         inline = F,
                         selected = NULL),
                       checkboxGroupInput(
                         "rhodo_check_5",
                         p("Rhodophytes", class= "h4",
                           tags$img(
                             src = "porphyra.png",
                             alt = "rhodophyta",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column2,
                         inline = F,
                         selected = NULL),
                       checkboxGroupInput(
                         "glauco_check_5",
                         p("Glaucophytes", class= "h4",
                           tags$img(
                             src = "cyanophora.png",
                             alt = "glaucophytes",
                             width = 18,
                             height = 30, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column3,
                         inline = F,
                         selected = NULL)
                       
                ),
                column(3,
                       checkboxGroupInput(
                         "mami_check_5",
                         p("Mamiellophyceae", class= "h4",
                           tags$img(
                             src = "bathycoccus.png",
                             alt = "mamiellales",
                             width = 20,
                             height = 22, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column4,
                         inline = F,
                         selected = NULL),
                       
                       checkboxGroupInput(
                         "chloro_check_5",
                         p("Other Chlorophytes", class= "h4",
                           tags$img(
                             src = "scenedesmus.png",
                             alt = "chlorophytes",
                             width = 50,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column5,
                         inline = F,
                         selected = NULL)
                ),
                column(3,
                       checkboxGroupInput(
                         "strepto_check_5",
                         p("Streptophyte algae", class= "h4",
                           tags$img(
                             src = "klebsormidium.png",
                             alt = "streptophytes",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column6,
                         inline = F,
                         selected = NULL),
                       
                       checkboxGroupInput(
                         "bryo_check_5",
                         p("Bryophytes", class= "h4",
                           tags$img(
                             src = "marchantia.png",
                             alt = "bryophyta",
                             width = 32,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column7,
                         inline = F,
                         selected = NULL)
                ),
                
                column(3,
                       checkboxGroupInput(
                         "lyco_check_5",
                         p("Lycophytes and Ferns", class= "h4",
                           tags$img(
                             src = "selaginella.png",
                             alt = "lycophyta",
                             width = 50,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         
                         choices = column8,
                         inline = F,
                         selected = NULL),
                       
                       checkboxGroupInput(
                         "sperma_check_5",
                         p("Spermatophyta", class= "h4",
                           tags$img(
                             src = "arabidopsis.png",
                             alt = "spermatophyta",
                             width = 25,
                             height = 25, style = "margin-left: 10px;"
                           )
                         ),
                         choices = column9,
                         inline = F,
                         selected = NULL)
                ),
                
                
                fluidRow(
                  column(5, textInput("geneInt5", label = h5("Insert gene ID"), width = "100%", placeholder = "AT2G46830")),
                  #column(1, div( style = "margin-top: 20px;", actionButton("run", "Run", icon = icon("magnifying-glass")))))
                  column(1, div( style = "margin-top: 40px;", 
                                 shinyWidgets::actionBttn("run_button5", "Run", size = "sm", icon = icon("magnifying-glass"),
                                                          style = "float", color = "royal"))),
                  column(3,div(style = "margin-top: 43px;",
                               shinyWidgets::materialSwitch(inputId = "switch5", label = "Global", 
                                                            value = T, status = "primary", inline = TRUE),
                               span("Viridiplantae")))
                  
                  
                  
                )),
              
              br(),
              fluidRow(
                box(status = "primary", width = 12,
                    title = span(tags$b("Results"), style = "color:#5e3587; font-size: 20px; "),
                    tabsetPanel(type = "tabs",
                                tabPanel("Genes Tree", "First tab content"),
                                tabPanel("Expansion/Contraction", "Tab content 2"),
                                tabPanel("PFAM Domains", "First tab content"),
                                tabPanel("Multiple Sequence Alignment", "Tab content 2"),
                                tabPanel("GO Terms", "First tab content"),
                                tabPanel("KEGG Orthology", "Tab content 2"),
                                tabPanel("STRING Interactions", "First tab content"),
                                tabPanel("Literature Annotation", "Tab content 2")
                    ))
              )
      )
      
      
      
    )
  )
)

server <- function(input, output) {
  
  # Set global variables for tracking changes in output
  UI_exist_pfam1 <<- F
  UI_exist_tree1 <<- F
  UI_exist_cafe1 <<- F
  UI_exist_error_cafe1 <<- F
  UI_exist_msa1 <<- F
  
  # Activate a global wrapper for reactivity when the action button of the
  # gene search panel is activated. Every result of this panel will be 
  # calculated inside this environment 
    
  # Clear previous error outputs
    
  # observeEvent(input$run_button1, {
  #   
  #                output$error_pfam1 <- NULL
  #                output$error_tree1 <- NULL
  #                
  #              })
  
  # To avoid autoupdating some inputs, define variables with its values
  model.selected1 <- reactive({
    model.selected <- !input$switch1
    return(model.selected)
  })%>% bindEvent(input$run_button1)
  
  build_trees1 <- reactive({
    build_trees <- as.character(input$build_trees_1)
    return(build_trees)
  }) %>% bindEvent(input$run_button1)
  
  # Load organisms selection based on the model selected
  selected_organisms1 <- reactive({
    selected_organisms <- c(input$mami_check_1,input$chloro_check_1, input$strepto_check_1,
                            input$bryo_check_1, input$lyco_check_1, input$sperma_check_1)
    if(model.selected1()){selected_organisms <- c(input$tsar_check_1, input$rhodo_check_1, 
                                                input$glauco_check_1,selected_organisms)}
    return(selected_organisms)
    
  }) %>% bindEvent(input$run_button1)
  
  selected_values_org1 <- reactive(organisms_values[selected_organisms1()]) %>% bindEvent(input$run_button1)
 
  
  og.name1 <- reactive({
    gene.name.tree <- input$geneInt1
    shinyjs::showElement(id = 'loading.tree1')
    
    # Load table with orthogroups information depending on selected model
    ortho.table.search <- ifelse(model.selected1(), "Global_Gene_Trees/Orthogroups.tsv",
                          "Green_Gene_Trees/Orthogroups.tsv")
    ortho.table <- read.csv(ortho.table.search,
                           header = T, sep = "\t", as.is = T,
                           fill = T, blank.lines.skip = F)

    
    # Find orthogroup of target gene
    found <- F
    file.name <- NULL
    i <- 1
    while (!(found) && (i <= nrow(ortho.table)))
    {
      object <- as.character(ortho.table[i,])
      gene.number <- grep(pattern = gene.name.tree, object)
      if (length(gene.number) != 0)
      {
        found <- T
        file.name <- ortho.table[i,1]
      }
      else
      {
        i <- i+1
      }
    }
    
    # Error message if gene ID does not belong to any OG
    if (is.null(file.name))
    {
      shinyjs::hideElement(id = 'loading.tree1')
      
      if (UI_exist_tree1)
      {
        removeUI(
          selector = "div:has(>> #treeTips1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>>> #presentorg1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #tree_image1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadTree1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadNewick1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadTreeSeqs1)",
          multiple = TRUE,
          immediate = TRUE
        )
      }
      
      UI_exist_tree1 <<- F
      output$error_tree1 <- renderUI({renderText({print("No results for this 
      query due to not supported gene name or lack of orthologs in the selected organisms.")})})
      validate(need(!is.null(file.name), " "))
    }
    
    return(file.name)
    
  }) %>% bindEvent(input$run_button1)
  
  tree1 <- reactive({
    file.name <- og.name1()
    # Load gene tree file depending on the input
    
    tree.name <- ifelse(model.selected1(),
                        paste("Global_Gene_Trees",paste(file.name, "tree.txt", sep = "_"), sep="/"),
                        paste("Green_Gene_Trees",paste(file.name, "tree.txt", sep = "_"), sep="/"))
    
    # Error if tree file not found
    if (!(file.exists(tree.name)))
    {
      shinyjs::hideElement(id = 'loading.tree1')
      
      if (UI_exist_tree1)
      {
        removeUI(
          selector = "div:has(>> #treeTips1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>>> #presentorg1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #tree_image1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadTree1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadNewick1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadTreeSeqs1)",
          multiple = TRUE,
          immediate = TRUE
        )
      }
      
      UI_exist_tree1 <<- F
      output$error_tree1 <- renderUI({renderText({print("Unable to construct tree associated to
                                                        to an orthogroup with less than 4 genes.")})})
      validate(need(file.exists(tree.name), " "))
    }
    
    # Generate tree depending on the tree building method selected
    {
    if (build_trees1() == "FastTree")
    {
      tree <- read.tree(tree.name)
      return(tree)
    }
    
    else
    {
      library(phangorn)
      
      # Read MSA
      multiple_file <- ifelse(model.selected1(),
                              paste("Global_MultipleSequenceAlignments", paste0(file.name, ".fa"), sep="/"),
                              paste("Green_MultipleSequenceAlignments", paste0(file.name, ".fa"), sep="/"))
      
      
      mytree <- read.phyDat(file = multiple_file,
                            format="fasta", type = "AA")
      
      # To keep the same workflow, the reduced tree will be calculated, then the applied subsetting
      # will only reduce them if the selected method has been fasttree
      
      
      # Selection of genes from the selected organism
      
      # Create empty vectors
      
      tips_to_keep.mp1 = tips_to_keep.at1 = tips_to_keep.ot1 = tips_to_keep.cp1 <- c()
      tips_to_keep.cr1 = tips_to_keep.cz1 = tips_to_keep.kn1 = tips_to_keep.me1 <- c()
      tips_to_keep.mi1 = tips_to_keep.pp1 = tips_to_keep.sl1 = tips_to_keep.sm1 <- c()
      tips_to_keep.sp1 = tips_to_keep.ta1 = tips_to_keep.vc1 = tips_to_keep.bp1 <- c()
      tips_to_keep.cri1 = tips_to_keep.ds1 = tips_to_keep.os1 = tips_to_keep.smag1 <- c()
      tips_to_keep.tp1 = tips_to_keep.aa1 = tips_to_keep.um1 = tips_to_keep.rs1 <- c()
      tips_to_keep.cyc1 = tips_to_keep.pu1 = tips_to_keep.pt1 = tips_to_keep.ng1 <- c()
      tips_to_keep.cyano1 = tips_to_keep.ca1 = tips_to_keep.mv1 = tips_to_keep.af1 <- c()
      tips_to_keep.sc1 = tips_to_keep.aegi1 = tips_to_keep.sb1 = tips_to_keep.chara1 <- c()
      tips_to_keep.guilla1 = tips_to_keep.crypto1 = tips_to_keep.cymero1 = tips_to_keep.galsul1 <- c()
      tips_to_keep.gracichor1 = tips_to_keep.sceobli1 = tips_to_keep.cocco1 = tips_to_keep.saccha1 <- c()
      tips_to_keep.haema1 = tips_to_keep.zm1 <- c()
      
      
      organisms.list <- c(selected_values_org1())
      
      
      # Fill vectors if organisms are in list and change names
      {
        if ("mp" %in% organisms.list)
        {
          tips_to_keep.mp1 <- grep(pattern = "marchantia", names(mytree)) 
        }
        
        if ("ot" %in% organisms.list)
        {
          tips_to_keep.ot1 <- grep(pattern = "ostreoco",names(mytree)) 
        }
        
        if ("at" %in% organisms.list)
        {
          tips_to_keep.at1 <- grep(pattern = "arabidopsis",names(mytree)) 
        }
        
        if ("cp" %in% organisms.list)
        {
          tips_to_keep.cp1 <- grep(pattern = "ceratodon",names(mytree)) 
        }
        
        if ("cr" %in% organisms.list)
        {
          tips_to_keep.cr1 <- grep(pattern = "chlamy",names(mytree))
        }
        
        if ("cz" %in% organisms.list)
        {
          tips_to_keep.cz1 <- grep(pattern = "chromochloris",names(mytree)) 
        }
        
        if ("kn" %in% organisms.list)
        {
          tips_to_keep.kn1 <- grep(pattern = "klebsormidium",names(mytree)) 
        }
        
        if ("me" %in% organisms.list)
        {
          tips_to_keep.me1 <- grep(pattern = "mesotaenium",names(mytree)) 
        }
        
        if ("mi" %in% organisms.list)
        {
          tips_to_keep.mi1 <- grep(pattern = "micromonas",names(mytree)) 
        }
        
        if ("pp" %in% organisms.list)
        {
          tips_to_keep.pp1 <- grep(pattern = "physcomitrium",names(mytree)) 
        }
        
        if ("sl" %in% organisms.list)
        {
          tips_to_keep.sl1 <- grep(pattern = "solanum",names(mytree)) 
        }
        
        if ("sm" %in% organisms.list)
        {
          tips_to_keep.sm1 <- grep(pattern = "selaginella",names(mytree))
        }
        
        if ("sp" %in% organisms.list)
        {
          tips_to_keep.sp1 <- grep(pattern = "spirogloea",names(mytree)) 
        }
        
        if ("ta" %in% organisms.list)
        {
          tips_to_keep.ta1 <- grep(pattern = "triticum",names(mytree)) 
        }
        
        if ("vc" %in% organisms.list)
        {
          tips_to_keep.vc1 <- grep(pattern = "volvox",names(mytree))
        }
        
        if ("bp" %in% organisms.list)
        {
          tips_to_keep.bp1 <- grep(pattern = "bathycoccus",names(mytree))
        }
        
        if ("cri" %in% organisms.list)
        {
          tips_to_keep.cri1 <- grep(pattern = "ceratopteris",names(mytree))
        }
        
        if ("ds" %in% organisms.list)
        {
          tips_to_keep.ds1 <- grep(pattern = "dunaliella",names(mytree))
        }
        
        if ("os" %in% organisms.list)
        {
          tips_to_keep.os1 <- grep(pattern = "oryza",names(mytree))
        }
        
        if ("smag" %in% organisms.list)
        {
          tips_to_keep.smag1 <- grep(pattern = "sphagnum",names(mytree))
        }
        
        if ("tp" %in% organisms.list)
        {
          tips_to_keep.tp1 <- grep(pattern = "thuja",names(mytree))
        }
        
        if ("aa" %in% organisms.list)
        {
          tips_to_keep.aa1 <- grep(pattern = "anthoceros",names(mytree))
        }
        
        if ("um" %in% organisms.list)
        {
          tips_to_keep.um1 <- grep(pattern = "ulva",names(mytree))
        }
        
        if ("rs" %in% organisms.list)
        {
          tips_to_keep.rs1 <- grep(pattern = "raphidocelis",names(mytree))
        }
        
        if ("cyc" %in% organisms.list)
        {
          tips_to_keep.cyc1 <- grep(pattern = "cycas",names(mytree))
        }
        
        if ("pu" %in% organisms.list)
        {
          tips_to_keep.pu1 <- grep(pattern = "porphyra",names(mytree))
        }
        
        if ("pt" %in% organisms.list)
        {
          tips_to_keep.pt1 <- grep(pattern = "phaeodactylum",names(mytree))
        }
        
        if ("ng" %in% organisms.list)
        {
          tips_to_keep.ng1 <- grep(pattern = "gaditana",names(mytree))
        }
        
        if ("cyano" %in% organisms.list)
        {
          tips_to_keep.cyano1 <- grep(pattern = "cyanophora",names(mytree))
        }
        
        if ("ca" %in% organisms.list)
        {
          tips_to_keep.ca1 <- grep(pattern = "chlorokybus",names(mytree))
        }
        
        if ("mv" %in% organisms.list)
        {
          tips_to_keep.mv1 <- grep(pattern = "mesostigma",names(mytree))
        }
        
        if ("af" %in% organisms.list)
        {
          tips_to_keep.af1 <- grep(pattern = "azolla",names(mytree))
        }
        
        if ("sc" %in% organisms.list)
        {
          tips_to_keep.sc1 <- grep(pattern = "salvinia",names(mytree))
        }
        
        if ("aegi" %in% organisms.list)
        {
          tips_to_keep.aegi1 <- grep(pattern = "aegilops",names(mytree))
        }
        
        if ("sb" %in% organisms.list)
        {
          tips_to_keep.sb1 <- grep(pattern = "sorghum",names(mytree))
        }
        
        if ("chara" %in% organisms.list)
        {
          tips_to_keep.chara1 <- grep(pattern = "chara",names(mytree))
        }
        
        if ("guilla" %in% organisms.list)
        {
          tips_to_keep.guilla1 <- grep(pattern = "guillardia",names(mytree))
        }
        
        if ("crypto" %in% organisms.list)
        {
          tips_to_keep.crypto1 <- grep(pattern = "cryptophyceae",names(mytree))
        }
        
        if ("cymero" %in% organisms.list)
        {
          tips_to_keep.cymero1 <- grep(pattern = "cyanidioschyzon",names(mytree))
        }
        
        if ("galsul" %in% organisms.list)
        {
          tips_to_keep.galsul1 <- grep(pattern = "galdieria",names(mytree))
        }
        
        if ("gracichor" %in% organisms.list)
        {
          tips_to_keep.gracichor1 <- grep(pattern = "gracilariopsis",names(mytree))
        }
        
        if ("sceobli" %in% organisms.list)
        {
          tips_to_keep.sceobli1 <- grep(pattern = "scenedesmus",names(mytree))
        }
        
        if ("cocco" %in% organisms.list)
        {
          tips_to_keep.cocco1 <- grep(pattern = "coccomyxa",names(mytree))
        }
        
        if ("saccha" %in% organisms.list)
        {
          tips_to_keep.saccha1 <- grep(pattern = "saccharina",names(mytree))
        }
        
        if ("haema" %in% organisms.list)
        {
          tips_to_keep.haema1 <- grep(pattern = "haematococcus",names(mytree))
        }
        
        if ("zm" %in% organisms.list)
        {
          tips_to_keep.zm1 <- grep(pattern = "mays",names(mytree))
        }
      }
      
      
      # Concatenate indexes to keep and subset MSA
      tips_to_keep.global <- c(tips_to_keep.mp1, tips_to_keep.ot1, tips_to_keep.at1, tips_to_keep.cp1,
                               tips_to_keep.cr1, tips_to_keep.cz1, tips_to_keep.kn1, tips_to_keep.me1,
                               tips_to_keep.mi1, tips_to_keep.pp1, tips_to_keep.sl1, tips_to_keep.sm1,
                               tips_to_keep.sp1, tips_to_keep.ta1, tips_to_keep.vc1, tips_to_keep.bp1,
                               tips_to_keep.cri1, tips_to_keep.ds1, tips_to_keep.os1, tips_to_keep.smag1,
                               tips_to_keep.tp1, tips_to_keep.aa1, tips_to_keep.um1, tips_to_keep.rs1,
                               tips_to_keep.cyc1, tips_to_keep.pu1, tips_to_keep.pt1, tips_to_keep.ng1,
                               tips_to_keep.cyano1, tips_to_keep.ca1, tips_to_keep.mv1, tips_to_keep.af1,
                               tips_to_keep.sc1, tips_to_keep.aegi1, tips_to_keep.sb1, tips_to_keep.chara1,
                               tips_to_keep.guilla1, tips_to_keep.crypto1, tips_to_keep.cymero1, tips_to_keep.galsul1,
                               tips_to_keep.gracichor1, tips_to_keep.sceobli1, tips_to_keep.cocco1, tips_to_keep.saccha1,
                               tips_to_keep.haema1,tips_to_keep.zm1)
      
      
      my_subset_tree <- subset(mytree, tips_to_keep.global)
      
      {
      if (build_trees1() == "Neighbour Joining")
      {
        # Create dist matrix for NJ and build tree
        dm <- dist.ml(my_subset_tree)
        treeNJ  <- NJ(dm)
        
        # Bootstrap for NJ
        fun_nj <- function(x) NJ(dist.ml(x))
        bs_nj <- bootstrap.phyDat(my_subset_tree, fun_nj, bs=100)
        
        # Save bootstrap values to the tree
        tree <- plotBS(treeNJ, bs_nj, type = "n")
        # Rooting tree
        tree <- midpoint(tree)
        return(tree)
      }
      
      else if (build_trees1() == "UPGMA")
      {
        dm <- dist.ml(my_subset_tree)
        treeUPGMA  <- upgma(dm)
        
        # Bootstrap for UPGMA
        fun_upgma <- function(x) upgma(dist.ml(x))
        bs_upgma <- bootstrap.phyDat(my_subset_tree, fun_upgma, bs=100)
        
        # Save bootstrap values to the tree
        tree <- addConfidences(treeUPGMA, bs_upgma)
        return(tree)
      }
      
      else if (build_trees1() == "Parsimony")
      {
        # Use ratchet for stimating tree (BS incorporated)
        tree  <- pratchet(my_subset_tree, trace = 0, minit=100)
        
        # Include branch lengths
        tree  <- acctran(tree, my_subset_tree)
        
        # Prune away internal edges of length tol (default = 1e-08) so our trees may contain multifurcations
        tree  <- di2multi(tree)
        
        # Some trees may difer in edges of lengths 0
        if(inherits(tree, "multiPhylo")){
          tree <- unique(tree)
        }
        return(tree)
      }
      }
      
    }
    }
  }) %>% bindEvent(input$run_button1)
  
  ortho_seq1 <- reactive({
    file.name <- og.name1()
  
    # Load orthogroup sequences file
    ortho.seq.name <- ifelse(model.selected1(),
                        paste("Global_Orthogroup_Sequences",paste(file.name, "fa", sep = "."), sep="/"),
                        paste("Green_Orthogroup_Sequences",paste(file.name, "fa", sep = "."), sep="/"))
    
    ortho_seq <- seqinr::read.fasta(ortho.seq.name, seqtype = "AA")
    return(ortho_seq)
  })
  
  
  # Tips to keep of each species with proper notation
  tips_to_keep.mp1 <- reactive({
    
    tree <- tree1()
    # Selection of organisms
    organisms.list <- c(selected_values_org1())
    
    # Selection of genes from the selected organism
    tips_to_keep.mp <- c()
    if ("mp" %in% organisms.list)
    {
      tips_to_keep.mp <- grep(pattern = "marchantia",tree$tip.label)
    }
    return(tips_to_keep.mp)
  })
  
  tips_to_keep.ot1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.ot <- c()
    if ("ot" %in% organisms.list)
    {
      tips_to_keep.ot <- grep(pattern = "ostreoco",tree$tip.label)
    }
    return(tips_to_keep.ot)
  })
  
  tips_to_keep.at1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.at <- c()
    if ("at" %in% organisms.list)
    {
      tips_to_keep.at <- grep(pattern = "arabidopsis",tree$tip.label)
    }
    return(tips_to_keep.at)
  })
  
  tips_to_keep.cp1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.cp <- c()
    if ("cp" %in% organisms.list)
    {
      tips_to_keep.cp <- grep(pattern = "ceratodon",tree$tip.label)
    }
    
    return(tips_to_keep.cp)
  })
  
  tips_to_keep.cr1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.cr <- c()
    if ("cr" %in% organisms.list)
    {
      tips_to_keep.cr <- grep(pattern = "chlamy",tree$tip.label)
    }
    return(tips_to_keep.cr)
  })
  
  tips_to_keep.cz1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.cz <- c()
    if ("cz" %in% organisms.list)
    {
      tips_to_keep.cz <- grep(pattern = "chromochloris",tree$tip.label)
    }
    return(tips_to_keep.cz)
  })
  
  tips_to_keep.kn1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.kn <- c()
    if ("kn" %in% organisms.list)
    {
      tips_to_keep.kn <- grep(pattern = "klebsormidium",tree$tip.label)
    }
    return(tips_to_keep.kn)
  })
  
  tips_to_keep.me1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.me <- c()
    if ("me" %in% organisms.list)
    {
      tips_to_keep.me <- grep(pattern = "mesotaenium",tree$tip.label)
    }
    return(tips_to_keep.me)
  })
  
  tips_to_keep.mi1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.mi <- c()
    if ("mi" %in% organisms.list)
    {
      tips_to_keep.mi <- grep(pattern = "micromonas",tree$tip.label)
    }
    return(tips_to_keep.mi)
  })
  
  tips_to_keep.pp1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.pp <- c()
    if ("pp" %in% organisms.list)
    {
      tips_to_keep.pp <- grep(pattern = "physcomitrium",tree$tip.label)
    }
    return(tips_to_keep.pp)
  })
  
  tips_to_keep.sl1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.sl <- c()
    if ("sl" %in% organisms.list)
    {
      tips_to_keep.sl <- grep(pattern = "solanum",tree$tip.label)
    }
    return(tips_to_keep.sl)
  })
  
  tips_to_keep.sm1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.sm <- c()
    if ("sm" %in% organisms.list)
    {
      tips_to_keep.sm <- grep(pattern = "selaginella",tree$tip.label)
    }
    return(tips_to_keep.sm)
  })
  
  tips_to_keep.sp1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.sp <- c()
    if ("sp" %in% organisms.list)
    {
      tips_to_keep.sp <- grep(pattern = "spirogloea",tree$tip.label)
    }
    return(tips_to_keep.sp)
  })
  
  tips_to_keep.ta1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.ta <- c()
    if ("ta" %in% organisms.list)
    {
      tips_to_keep.ta <- grep(pattern = "triticum",tree$tip.label)
    }
    return(tips_to_keep.ta)
  })
  
  tips_to_keep.vc1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.vc <- c()
    if ("vc" %in% organisms.list)
    {
      tips_to_keep.vc <- grep(pattern = "volvox",tree$tip.label)
    }
    return(tips_to_keep.vc)
  })
  
  tips_to_keep.bp1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.bp <- c()
    if ("bp" %in% organisms.list)
    {
      tips_to_keep.bp <- grep(pattern = "bathycoccus",tree$tip.label)
    }
    return(tips_to_keep.bp)
  })
  
  tips_to_keep.cri1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.cri <- c()
    if ("cri" %in% organisms.list)
    {
      tips_to_keep.cri <- grep(pattern = "ceratopteris",tree$tip.label)
    }
    return(tips_to_keep.cri)
  })
  
  tips_to_keep.ds1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.ds <- c()
    if ("ds" %in% organisms.list)
    {
      tips_to_keep.ds <- grep(pattern = "dunaliella",tree$tip.label)
    }
    return(tips_to_keep.ds)
  })
  
  tips_to_keep.os1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.os <- c()
    if ("os" %in% organisms.list)
    {
      tips_to_keep.os <- grep(pattern = "oryza",tree$tip.label)
    }
    return(tips_to_keep.os)
  })
  
  tips_to_keep.smag1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.smag <- c()
    if ("smag" %in% organisms.list)
    {
      tips_to_keep.smag <- grep(pattern = "sphagnum",tree$tip.label)
    }
    return(tips_to_keep.smag)
  })
  
  tips_to_keep.tp1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.tp <- c()
    if ("tp" %in% organisms.list)
    {
      tips_to_keep.tp <- grep(pattern = "thuja",tree$tip.label)
    }
    return(tips_to_keep.tp)
  })
  
  tips_to_keep.aa1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.aa <- c()
    if ("aa" %in% organisms.list)
    {
      tips_to_keep.aa <- grep(pattern = "anthoceros",tree$tip.label)
    }
    return(tips_to_keep.aa)
  })
  
  tips_to_keep.um1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.um <- c()
    if ("um" %in% organisms.list)
    {
      tips_to_keep.um <- grep(pattern = "ulva",tree$tip.label)
    }
    return(tips_to_keep.um)
  })
  
  tips_to_keep.rs1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.rs <- c()
    if ("rs" %in% organisms.list)
    {
      tips_to_keep.rs <- grep(pattern = "raphidocelis",tree$tip.label)
    }
    return(tips_to_keep.rs)
  })
  
  tips_to_keep.cyc1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.cyc <- c()
    if ("cyc" %in% organisms.list)
    {
      tips_to_keep.cyc <- grep(pattern = "cycas",tree$tip.label)
    }
    return(tips_to_keep.cyc)
  })
  
  tips_to_keep.pu1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.pu <- c()
    if ("pu" %in% organisms.list)
    {
      tips_to_keep.pu <- grep(pattern = "porphyra",tree$tip.label)
    }
    return(tips_to_keep.pu)
  })
  
  tips_to_keep.pt1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.pt <- c()
    if ("pt" %in% organisms.list)
    {
      tips_to_keep.pt <- grep(pattern = "phaeodactylum",tree$tip.label)
    }
    return(tips_to_keep.pt)
  })
  
  tips_to_keep.ng1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.ng <- c()
    if ("ng" %in% organisms.list)
    {
      tips_to_keep.ng <- grep(pattern = "gaditana",tree$tip.label)
    }
    return(tips_to_keep.ng)
  })
  
  tips_to_keep.cyano1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.cyano <- c()
    if ("cyano" %in% organisms.list)
    {
      tips_to_keep.cyano <- grep(pattern = "cyanophora",tree$tip.label)
    }
    return(tips_to_keep.cyano)
  })
  
  tips_to_keep.ca1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.ca <- c()
    if ("ca" %in% organisms.list)
    {
      tips_to_keep.ca <- grep(pattern = "chlorokybus",tree$tip.label)
    }
    return(tips_to_keep.ca)
  })
  
  tips_to_keep.mv1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.mv <- c()
    if ("mv" %in% organisms.list)
    {
      tips_to_keep.mv <- grep(pattern = "mesostigma",tree$tip.label)
    }
    return(tips_to_keep.mv)
  })
  
  tips_to_keep.af1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.af <- c()
    if ("af" %in% organisms.list)
    {
      tips_to_keep.af <- grep(pattern = "azolla",tree$tip.label)
    }
    return(tips_to_keep.af)
  })
  
  tips_to_keep.sc1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.sc <- c()
    if ("sc" %in% organisms.list)
    {
      tips_to_keep.sc <- grep(pattern = "salvinia",tree$tip.label)
    }
    return(tips_to_keep.sc)
  })
  
  tips_to_keep.aegi1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.aegi <- c()
    if ("aegi" %in% organisms.list)
    {
      tips_to_keep.aegi <- grep(pattern = "aegilops",tree$tip.label)
    }
    return(tips_to_keep.aegi)
  })
  
  tips_to_keep.sb1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.sb <- c()
    if ("sb" %in% organisms.list)
    {
      tips_to_keep.sb <- grep(pattern = "sorghum",tree$tip.label)
    }
    return(tips_to_keep.sb)
  })
  
  tips_to_keep.chara1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.chara <- c()
    if ("chara" %in% organisms.list)
    {
      tips_to_keep.chara <- grep(pattern = "chara",tree$tip.label)
    }
    return(tips_to_keep.chara)
  })
  
  tips_to_keep.guilla1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.guilla <- c()
    if ("guilla" %in% organisms.list)
    {
      tips_to_keep.guilla <- grep(pattern = "guillardia",tree$tip.label)
    }
    return(tips_to_keep.guilla)
  })
  
  tips_to_keep.crypto1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.crypto <- c()
    if ("crypto" %in% organisms.list)
    {
      tips_to_keep.crypto <- grep(pattern = "cryptophyceae",tree$tip.label)
    }
    return(tips_to_keep.crypto)
  })
  
  tips_to_keep.cymero1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.cymero <- c()
    if ("cymero" %in% organisms.list)
    {
      tips_to_keep.cymero <- grep(pattern = "cyanidioschyzon",tree$tip.label)
    }
    return(tips_to_keep.cymero)
  })
  
  tips_to_keep.galsul1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.galsul <- c()
    if ("galsul" %in% organisms.list)
    {
      tips_to_keep.galsul <- grep(pattern = "galdieria",tree$tip.label)
    }
    return(tips_to_keep.galsul)
  })
  
  tips_to_keep.gracichor1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.gracichor <- c()
    if ("gracichor" %in% organisms.list)
    {
      tips_to_keep.gracichor <- grep(pattern = "gracilariopsis",tree$tip.label)
    }
    return(tips_to_keep.gracichor)
  })
  
  tips_to_keep.sceobli1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.sceobli <- c()
    if ("sceobli" %in% organisms.list)
    {
      tips_to_keep.sceobli <- grep(pattern = "scenedesmus",tree$tip.label)
    }
    return(tips_to_keep.sceobli)
  })
  
  tips_to_keep.cocco1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.cocco <- c()
    if ("cocco" %in% organisms.list)
    {
      tips_to_keep.cocco <- grep(pattern = "coccomyxa",tree$tip.label)
    }
    return(tips_to_keep.cocco)
  })
  
  tips_to_keep.saccha1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.saccha <- c()
    if ("saccha" %in% organisms.list)
    {
      tips_to_keep.saccha <- grep(pattern = "saccharina",tree$tip.label)
    }
    return(tips_to_keep.saccha)
  })
  
  tips_to_keep.haema1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.haema <- c()
    if ("haema" %in% organisms.list)
    {
      tips_to_keep.haema <- grep(pattern = "haematococcus",tree$tip.label)
    }
    return(tips_to_keep.haema)
  })
  
  tips_to_keep.zm1 <- reactive({
    
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    tips_to_keep.zm <- c()
    if ("zm" %in% organisms.list)
    {
      tips_to_keep.zm <- grep(pattern = "mays",tree$tip.label)
    }
    return(tips_to_keep.zm)
  }) %>% bindEvent(input$run_button1)
  
  
  # Create complete gene tree with the proper name for each gene
  # For this, we split the species name apart from the gene name
  tree_adj1 <- reactive({
    tree <- tree1()
    organisms.list <- c(selected_values_org1())
    
    if ("mp" %in% organisms.list)
    {
      tips_to_keep.mp <- grep(pattern = "marchantia",tree$tip.label) 
      if (length(tips_to_keep.mp) != 0)
      {
        mp.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.mp]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.mp] <- mp.v
      }
    }
    
    if ("ot" %in% organisms.list)
    {
      tips_to_keep.ot <- grep(pattern = "ostreoco",tree$tip.label) 
      if (length(tips_to_keep.ot) != 0)
      {
        ost.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.ot]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.ot] <- ost.v
      }
    }
    
    if ("at" %in% organisms.list)
    {
      tips_to_keep.at <- grep(pattern = "arabidopsis",tree$tip.label) 
      if (length(tips_to_keep.at) != 0)
      {
        arabi.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.at]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.at] <- arabi.v
      }
    }
    
    if ("cp" %in% organisms.list)
    {
      tips_to_keep.cp <- grep(pattern = "ceratodon",tree$tip.label) 
      if (length(tips_to_keep.cp) != 0)
      {
        cer.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.cp]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.cp] <- cer.v
      }
    }
    
    if ("cr" %in% organisms.list)
    {
      tips_to_keep.cr <- grep(pattern = "chlamy",tree$tip.label)
      if (length(tips_to_keep.cr) != 0)
      {
        chlamy.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.cr]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.cr] <- chlamy.v
      }
    }
    
    if ("cz" %in% organisms.list)
    {
      tips_to_keep.cz <- grep(pattern = "chromochloris",tree$tip.label) 
      if (length(tips_to_keep.cz) != 0)
      {
        chromo.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.cz]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.cz] <- chromo.v
      }
    }
    
    if ("kn" %in% organisms.list)
    {
      tips_to_keep.kn <- grep(pattern = "klebsormidium",tree$tip.label) 
      if (length(tips_to_keep.kn) != 0)
      {
        klebs.v1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.kn]), "_"), function(x) x[[3]])
        klebs.v2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.kn]), "_"), function(x) x[[4]])
        klebs.v <- paste(klebs.v1, klebs.v2, sep = "_")
        tree$tip.label[tips_to_keep.kn] <- klebs.v
      }
    }
    
    if ("me" %in% organisms.list)
    {
      tips_to_keep.me <- grep(pattern = "mesotaenium",tree$tip.label) 
      if (length(tips_to_keep.me) != 0)
      {
        meso.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.me]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.me] <- meso.v
      }
    }
    
    if ("mi" %in% organisms.list)
    {
      tips_to_keep.mi <- grep(pattern = "micromonas",tree$tip.label) 
      if (length(tips_to_keep.mi) != 0)
      {
        micro.v1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.mi]), "_"), function(x) x[[3]])
        micro.v2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.mi]), "_"), function(x) x[[4]])
        micro.v <- paste(micro.v1, micro.v2, sep = "_")
        tree$tip.label[tips_to_keep.mi] <- micro.v
      }
    }
    
    if ("pp" %in% organisms.list)
    {
      tips_to_keep.pp <- grep(pattern = "physcomitrium",tree$tip.label) 
      if (length(tips_to_keep.pp) != 0)
      {
        phys.v1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.pp]), "_"), function(x) x[[3]])
        phys.v2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.pp]), "_"), function(x) x[[4]])
        phys.v <- paste(phys.v1, phys.v2, sep = "_")
        tree$tip.label[tips_to_keep.pp] <- phys.v
      }
    }
    
    if ("sl" %in% organisms.list)
    {
      tips_to_keep.sl <- grep(pattern = "solanum",tree$tip.label) 
      if (length(tips_to_keep.sl) != 0)
      {
        sola.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.sl]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.sl] <- sola.v
      }
    }
    
    if ("sm" %in% organisms.list)
    {
      tips_to_keep.sm <- grep(pattern = "selaginella",tree$tip.label) 
      if (length(tips_to_keep.sm) != 0)
      {
        sel.v1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.sm]), "_"), function(x) x[[3]])
        sel.v2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.sm]), "_"), function(x) x[[4]])
        sel.v <- paste(sel.v1, sel.v2, sep = "_")
        tree$tip.label[tips_to_keep.sm] <- sel.v
      }
    }
    
    if ("sp" %in% organisms.list)
    {
      tips_to_keep.sp <- grep(pattern = "spirogloea",tree$tip.label) 
      if (length(tips_to_keep.sp) != 0)
      {
        spiro.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.sp]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.sp] <- spiro.v
      }
    }
    
    if ("ta" %in% organisms.list)
    {
      tips_to_keep.ta <- grep(pattern = "triticum",tree$tip.label) 
      if (length(tips_to_keep.ta) != 0)
      {
        tri.v1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.ta]), "_"), function(x) x[[3]])
        tri.v2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.ta]), "_"), function(x) x[[4]])
        tri.v3 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.ta]), "_"), function(x) x[[5]])
        tri.v <- paste(tri.v1, tri.v2, tri.v3, sep = "_")
        tree$tip.label[tips_to_keep.ta] <- tri.v
      }
    }
    
    if ("vc" %in% organisms.list)
    {
      tips_to_keep.vc <- grep(pattern = "volvox",tree$tip.label)
      if (length(tips_to_keep.vc) != 0)
      {
        vc.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.vc]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.vc] <- vc.v
      }
    }
    
    if ("bp" %in% organisms.list)
    {
      tips_to_keep.bp <- grep(pattern = "bathycoccus",tree$tip.label)
      if (length(tips_to_keep.bp) != 0)
      {
        bp.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.bp]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.bp] <- bp.v
      }
    }
    
    if ("cri" %in% organisms.list)
    {
      tips_to_keep.cri <- grep(pattern = "ceratopteris",tree$tip.label)
      if (length(tips_to_keep.cri) != 0)
      {
        cri.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.cri]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.cri] <- cri.v
      }
    }
    
    if ("ds" %in% organisms.list)
    {
      tips_to_keep.ds <- grep(pattern = "dunaliella",tree$tip.label)
      if (length(tips_to_keep.ds) != 0)
      {
        ds.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.ds]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.ds] <- ds.v
      }
    }
    
    if ("os" %in% organisms.list)
    {
      tips_to_keep.os <- grep(pattern = "oryza",tree$tip.label)
      if (length(tips_to_keep.os) != 0)
      {
        os.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.os]), "va_"), function(x) x[[2]])
        tree$tip.label[tips_to_keep.os] <- os.v
      }
    }
    
    if ("smag" %in% organisms.list)
    {
      tips_to_keep.smag <- grep(pattern = "sphagnum",tree$tip.label)
      if (length(tips_to_keep.smag) != 0)
      {
        smag.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.smag]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.smag] <- smag.v
      }
    }
    
    if ("tp" %in% organisms.list)
    {
      tips_to_keep.tp <- grep(pattern = "thuja",tree$tip.label)
      if (length(tips_to_keep.tp) != 0)
      {
        tp.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.tp]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.tp] <- tp.v
      }
    }
    
    if ("aa" %in% organisms.list)
    {
      tips_to_keep.aa <- grep(pattern = "anthoceros",tree$tip.label)
      if (length(tips_to_keep.aa) != 0)
      {
        aa.v1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.aa]), "_"), function(x) x[[3]])
        aa.v2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.aa]), "_"), function(x) x[[4]])
        aa.v <- paste(aa.v1, aa.v2, sep="_")
        tree$tip.label[tips_to_keep.aa] <- aa.v
      }
    }
    
    if ("um" %in% organisms.list)
    {
      tips_to_keep.um <- grep(pattern = "ulva",tree$tip.label)
      if (length(tips_to_keep.um) != 0)
      {
        um.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.um]), "_"), function(x) x[[3]])
        um.vec2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.um]), "_"), function(x) x[[4]])
        um.v <- paste(um.vec1, um.vec2, sep = "_")
        tree$tip.label[tips_to_keep.um] <- um.v
      }
    }
    
    if ("rs" %in% organisms.list)
    {
      tips_to_keep.rs <- grep(pattern = "raphidocelis",tree$tip.label)
      if (length(tips_to_keep.rs) != 0)
      {
        rs.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.rs]), "_"), function(x) x[[3]])
        rs.vec2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.rs]), "_"), function(x) x[[4]])
        rs.v <- paste(rs.vec1, rs.vec2, sep = "_")
        tree$tip.label[tips_to_keep.rs] <- rs.v
      }
    }
    
    if ("cyc" %in% organisms.list)
    {
      tips_to_keep.cyc <- grep(pattern = "cycas",tree$tip.label)
      if (length(tips_to_keep.cyc) != 0)
      {
        cyc.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.cyc]), "_"), function(x) x[[3]])
        cyc.vec2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.cyc]), "_"), function(x) x[[4]])
        cyc.v <- paste(cyc.vec1, cyc.vec2, sep = "_")
        tree$tip.label[tips_to_keep.cyc] <- cyc.v
      }
    }
    
    if ("pu" %in% organisms.list)
    {
      tips_to_keep.pu <- grep(pattern = "porphyra",tree$tip.label)
      if (length(tips_to_keep.pu) != 0)
      {
        pu.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.pu]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.pu] <- pu.vec1
      }
    }
    
    if ("pt" %in% organisms.list)
    {
      tips_to_keep.pt <- grep(pattern = "phaeodactylum",tree$tip.label)
      if (length(tips_to_keep.pt) != 0)
      {
        pt.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.pt]), "_"), function(x) x[[3]])
        pt.vec2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.pt]), "_"), function(x) x[[4]])
        pt.v <- paste(pt.vec1, pt.vec2, sep = "_")
        tree$tip.label[tips_to_keep.pt] <- pt.v
      }
    }
    
    if ("ng" %in% organisms.list)
    {
      tips_to_keep.ng <- grep(pattern = "gaditana",tree$tip.label)
      if (length(tips_to_keep.ng) != 0)
      {
        ng.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.ng]), "_"), function(x) x[[3]])
        ng.vec2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.ng]), "_"), function(x) x[[4]])
        ng.v <- paste(ng.vec1, ng.vec2, sep = "_")
        tree$tip.label[tips_to_keep.ng] <- ng.v
      }
    }
    
    if ("cyano" %in% organisms.list)
    {
      tips_to_keep.cyano <- grep(pattern = "cyanophora",tree$tip.label)
      if (length(tips_to_keep.cyano) != 0)
      {
        cyano.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.cyano]), "_"), function(x) x[[3]])
        cyano.vec2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.cyano]), "_"), function(x) x[[4]])
        cyano.v <- paste(cyano.vec1, cyano.vec2, sep = "_")
        tree$tip.label[tips_to_keep.cyano] <- cyano.v
      }
    }
    
    if ("ca" %in% organisms.list)
    {
      tips_to_keep.ca <- grep(pattern = "chlorokybus",tree$tip.label)
      if (length(tips_to_keep.ca) != 0)
      {
        ca.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.ca]), "_"), function(x) x[[3]])
        ca.vec2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.ca]), "_"), function(x) x[[4]])
        ca.v <- paste(ca.vec1, ca.vec2, sep = "_")
        tree$tip.label[tips_to_keep.ca] <- ca.v
      }
    }
    
    if ("mv" %in% organisms.list)
    {
      tips_to_keep.mv <- grep(pattern = "mesostigma",tree$tip.label)
      if (length(tips_to_keep.mv) != 0)
      {
        mv.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.mv]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.mv] <- mv.vec1
      }
    }
    
    if ("af" %in% organisms.list)
    {
      tips_to_keep.af <- grep(pattern = "azolla",tree$tip.label)
      if (length(tips_to_keep.af) != 0)
      {
        af.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.af]), "_"), function(x) x[[3]])
        af.vec2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.af]), "_"), function(x) x[[4]])
        af.v <- paste(af.vec1, af.vec2, sep = "_")
        tree$tip.label[tips_to_keep.af] <- af.v
      }
    }
    
    if ("sc" %in% organisms.list)
    {
      tips_to_keep.sc <- grep(pattern = "salvinia",tree$tip.label)
      if (length(tips_to_keep.sc) != 0)
      {
        sc.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.sc]), "_"), function(x) x[[3]])
        sc.vec2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.sc]), "_"), function(x) x[[4]])
        sc.vec3 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.sc]), "_"), function(x) x[[5]])
        sc.v <- paste(sc.vec1, sc.vec2, sc.vec3, sep = "_")
        tree$tip.label[tips_to_keep.sc] <- sc.v
      }
    }
    
    if ("aegi" %in% organisms.list)
    {
      tips_to_keep.aegi <- grep(pattern = "aegilops",tree$tip.label)
      if (length(tips_to_keep.aegi) != 0)
      {
        aegi.v <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.aegi]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.aegi] <- aegi.v
      }
    }
    
    if ("sb" %in% organisms.list)
    {
      tips_to_keep.sb <- grep(pattern = "sorghum",tree$tip.label)
      if (length(tips_to_keep.sb) != 0)
      {
        sb.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.sb]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.sb] <- sb.vec1
      }
    }
    
    if ("chara" %in% organisms.list)
    {
      tips_to_keep.chara <- grep(pattern = "chara",tree$tip.label)
      if (length(tips_to_keep.chara) != 0)
      {
        chara.v1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.chara]), "_"), function(x) x[[3]])
        chara.v2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.chara]), "_"), function(x) x[[4]])
        chara.v <- paste(chara.v1, chara.v2, sep = "_")
        tree$tip.label[tips_to_keep.chara] <- chara.v
      }
    }
    
    if ("guilla" %in% organisms.list)
    {
      tips_to_keep.guilla <- grep(pattern = "guillardia",tree$tip.label)
      if (length(tips_to_keep.guilla) != 0)
      {
        guilla.v1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.guilla]), "_"), function(x) x[[3]])
        guilla.v2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.guilla]), "_"), function(x) x[[4]])
        guilla.v <- paste(guilla.v1, guilla.v2, sep = "_")
        tree$tip.label[tips_to_keep.guilla] <- guilla.v
      }
    }
    
    if ("crypto" %in% organisms.list)
    {
      tips_to_keep.crypto <- grep(pattern = "cryptophyceae",tree$tip.label)
      if (length(tips_to_keep.crypto) != 0)
      {
        crypto.v1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.crypto]), "_"), function(x) x[[3]])
        crypto.v2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.crypto]), "_"), function(x) x[[4]])
        crypto.v3 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.crypto]), "_"), function(x) x[[5]])
        crypto.v <- paste(crypto.v1, crypto.v2, crypto.v3, sep = "_")
        tree$tip.label[tips_to_keep.crypto] <- crypto.v
      }
    }
    
    if ("cymero" %in% organisms.list)
    {
      tips_to_keep.cymero <- grep(pattern = "cyanidioschyzon",tree$tip.label)
      if (length(tips_to_keep.cymero) != 0)
      {
        cymero.v1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.cymero]), "_"), function(x) x[[3]])
        cymero.v2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.cymero]), "_"), function(x) x[[4]])
        cymero.v <- paste(cymero.v1, cymero.v2, sep = "_")
        tree$tip.label[tips_to_keep.cymero] <- cymero.v
      }
    }
    
    if ("galsul" %in% organisms.list)
    {
      tips_to_keep.galsul <- grep(pattern = "galdieria",tree$tip.label)
      if (length(tips_to_keep.galsul) != 0)
      {
        galsul.v1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.galsul]), "_"), function(x) x[[3]])
        galsul.v2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.galsul]), "_"), function(x) x[[4]])
        galsul.v <- paste(galsul.v1, galsul.v2, sep = "_")
        tree$tip.label[tips_to_keep.galsul] <- galsul.v
      }
    }
    
    if ("gracichor" %in% organisms.list)
    {
      tips_to_keep.gracichor <- grep(pattern = "gracilariopsis",tree$tip.label)
      if (length(tips_to_keep.gracichor) != 0)
      {
        gracichor.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.gracichor]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.gracichor] <- gracichor.vec1
      }
    }
    
    if ("sceobli" %in% organisms.list)
    {
      tips_to_keep.sceobli <- grep(pattern = "scenedesmus",tree$tip.label)
      if (length(tips_to_keep.sceobli) != 0)
      {
        sceobli.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.sceobli]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.sceobli] <- sceobli.vec1
      }
    }
    
    if ("cocco" %in% organisms.list)
    {
      tips_to_keep.cocco <- grep(pattern = "coccomyxa",tree$tip.label)
      if (length(tips_to_keep.cocco) != 0)
      {
        cocco.v1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.cocco]), "_"), function(x) x[[3]])
        cocco.v2 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.cocco]), "_"), function(x) x[[4]])
        cocco.v <- paste(cocco.v1, cocco.v2, sep = "_")
        tree$tip.label[tips_to_keep.cocco] <- cocco.v
      }
    }
    
    if ("saccha" %in% organisms.list)
    {
      tips_to_keep.saccha <- grep(pattern = "saccharina",tree$tip.label)
      if (length(tips_to_keep.saccha) != 0)
      {
        saccha.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.saccha]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.saccha] <- saccha.vec1
      }
    }
    
    if ("haema" %in% organisms.list)
    {
      tips_to_keep.haema <- grep(pattern = "haematococcus",tree$tip.label)
      if (length(tips_to_keep.haema) != 0)
      {
        haema.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.haema]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.haema] <- haema.vec1
      }
    }
    
    if ("zm" %in% organisms.list)
    {
      tips_to_keep.zm <- grep(pattern = "mays",tree$tip.label)
      if (length(tips_to_keep.zm) != 0)
      {
        zm.vec1 <- sapply(strsplit(as.character(tree$tip.label[tips_to_keep.zm]), "_"), function(x) x[[3]])
        tree$tip.label[tips_to_keep.zm] <- zm.vec1
      }
    }
    
    return(tree)
  }) %>% bindEvent(input$run_button1)
  
  # Generate reduced tree when the corresponding button is activated
  tree_reduced1 <- reactive({
    
    tree <- tree_adj1()
    # Define tips to keep (selected organisms) and generate the reduced tree
    tips_to_keep.global <- c(tips_to_keep.mp1(), tips_to_keep.ot1(), tips_to_keep.at1(), tips_to_keep.cp1(),
                             tips_to_keep.cr1(), tips_to_keep.cz1(), tips_to_keep.kn1(), tips_to_keep.me1(),
                             tips_to_keep.mi1(), tips_to_keep.pp1(), tips_to_keep.sl1(), tips_to_keep.sm1(),
                             tips_to_keep.sp1(), tips_to_keep.ta1(), tips_to_keep.vc1(), tips_to_keep.bp1(),
                             tips_to_keep.cri1(), tips_to_keep.ds1(), tips_to_keep.os1(), tips_to_keep.smag1(),
                             tips_to_keep.tp1(), tips_to_keep.aa1(), tips_to_keep.um1(), tips_to_keep.rs1(),
                             tips_to_keep.cyc1(), tips_to_keep.pu1(), tips_to_keep.pt1(), tips_to_keep.ng1(),
                             tips_to_keep.cyano1(), tips_to_keep.ca1(), tips_to_keep.mv1(), tips_to_keep.af1(),
                             tips_to_keep.sc1(), tips_to_keep.aegi1(), tips_to_keep.sb1(), tips_to_keep.chara1(),
                             tips_to_keep.guilla1(), tips_to_keep.crypto1(), tips_to_keep.cymero1(), tips_to_keep.galsul1(),
                             tips_to_keep.gracichor1(), tips_to_keep.sceobli1(), tips_to_keep.cocco1(), tips_to_keep.saccha1(),
                             tips_to_keep.haema1(),tips_to_keep.zm1())
    
    # Error message if trying to build tree with less than two tips
    if (length(tips_to_keep.global) < 2)
    {
      shinyjs::hideElement(id = 'loading.tree1')
      
      if (UI_exist_tree1)
      {
        removeUI(
          selector = "div:has(>> #treeTips1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>>> #presentorg1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #tree_image1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadTree1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadNewick1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadTreeSeqs1)",
          multiple = TRUE,
          immediate = TRUE
        )
      }
      
      UI_exist_tree1 <<- F
      output$error_tree1 <- renderUI({renderText({print("Unable to construct 
      tree with a single tip, please select more organisms.")})})
      validate(need(length(tips_to_keep.global) > 1, " "))
    }
    
      
  # Error message if query gene does not belong to selected organisms
    if (!(input$geneInt1 %in% tree$tip.label))
    {
      shinyjs::hideElement(id = 'loading.tree1')
      
      if (UI_exist_tree1)
      {
        removeUI(
          selector = "div:has(>> #treeTips1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>>> #presentorg1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #tree_image1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadTree1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadNewick1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadTreeSeqs1)",
          multiple = TRUE,
          immediate = TRUE
        )
      }
      
      UI_exist_tree1 <<- F
      output$error_tree1 <- renderUI({renderText({print("Please select the species corresponding
      to the query gene. Read the instructions for clarification.")})})
      validate(need(input$geneInt1 %in% tree$tip.label, " "))
    }
    
    
    tips_to_drop <- setdiff(1:length(tree$tip.label), tips_to_keep.global)
    tree_reduced <- drop.tip(tree, tips_to_drop)
    
    return(tree_reduced)
  }) %>% bindEvent(input$run_button1)
  
  ### Select orthogroup sequences based on the reduced tree
  ortho_reduced1 <- reactive({
    
    tree_reduced <- tree_reduced1()
    ortho_seq <- ortho_seq1()
    ortho_reduced <- ortho_seq[tree_reduced$tip.label]
    return(ortho_reduced)
  }) %>% bindEvent(input$run_button1)
  
  ### Select orthogroup sequences based on the reduced tree
  organims_reduced1 <- reactive({
    
    tree_reduced <- tree_reduced1()
    
    len.mp <- length(tips_to_keep.mp1())
    len.ot <- length(tips_to_keep.ot1())
    len.at <- length(tips_to_keep.at1())
    len.cp <- length(tips_to_keep.cp1())
    len.cr <- length(tips_to_keep.cr1())
    len.cz <- length(tips_to_keep.cz1())
    len.kn <- length(tips_to_keep.kn1())
    len.me <- length(tips_to_keep.me1())
    len.mi <- length(tips_to_keep.mi1())
    len.pp <- length(tips_to_keep.pp1())
    len.sl <- length(tips_to_keep.sl1())
    len.sm <- length(tips_to_keep.sm1())
    len.sp <- length(tips_to_keep.sp1())
    len.ta <- length(tips_to_keep.ta1())
    len.vc <- length(tips_to_keep.vc1())
    len.bp <- length(tips_to_keep.bp1())
    len.cri <- length(tips_to_keep.cri1())
    len.ds <- length(tips_to_keep.ds1())
    len.os <- length(tips_to_keep.os1())
    len.smag <- length(tips_to_keep.smag1())
    len.tp <- length(tips_to_keep.tp1())
    len.aa <- length(tips_to_keep.aa1())
    len.um <- length(tips_to_keep.um1())
    len.rs <- length(tips_to_keep.rs1())
    len.cyc <- length(tips_to_keep.cyc1())
    len.pu <- length(tips_to_keep.pu1())
    len.pt <- length(tips_to_keep.pt1())
    len.ng <- length(tips_to_keep.ng1())
    len.cyano <- length(tips_to_keep.cyano1())
    len.ca <- length(tips_to_keep.ca1())
    len.mv <- length(tips_to_keep.mv1())
    len.af <- length(tips_to_keep.af1())
    len.sc <- length(tips_to_keep.sc1())
    len.aegi <- length(tips_to_keep.aegi1())
    len.sb <- length(tips_to_keep.sb1())
    len.chara <- length(tips_to_keep.chara1())
    len.guilla <- length(tips_to_keep.guilla1())
    len.crypto <- length(tips_to_keep.crypto1())
    len.cymero <- length(tips_to_keep.cymero1())
    len.galsul <- length(tips_to_keep.galsul1())
    len.gracichor <- length(tips_to_keep.gracichor1())
    len.sceobli <- length(tips_to_keep.sceobli1())
    len.cocco <- length(tips_to_keep.cocco1())
    len.saccha <- length(tips_to_keep.saccha1())
    len.haema <- length(tips_to_keep.haema1())
    len.zea <- length(tips_to_keep.zm1())
                             
    organims_reduced <- c(rep("Marchantia", len.mp), rep("Ostreococcus", len.ot),
                          rep("Arabidopsis", len.at), rep("Ceratodon", len.cp),
                          rep("Chlamydomonas", len.cr), rep("Chromochloris", len.cz),
                          rep("Klebsormidium", len.kn), rep("Mesotaenium", len.me),
                          rep("Micromonas", len.mi), rep("Physcomitrium", len.pp),
                          rep("Solanum", len.sl), rep("Selaginella", len.sm),
                          rep("Spirogloea", len.sp), rep("Triticum", len.ta),
                          rep("Volvox", len.vc), rep("Bathycoccus", len.bp),
                          rep("Ceratopteris", len.cri), rep("Dunaliella", len.ds),
                          rep("Oryza", len.os), rep("Sphagnum", len.smag),
                          rep("Thuja", len.tp), rep("Anthoceros", len.aa),
                          rep("Ulva", len.um), rep("Raphidocelis", len.rs),
                          rep("Cycas", len.cyc), rep("Porphyra", len.pu),
                          rep("Phaeodactylum", len.pt), rep("Nannochloropsis", len.ng),
                          rep("Cyanophora", len.cyano), rep("Chlorokybus", len.ca),
                          rep("Mesostigma", len.mv), rep("Azolla", len.af),
                          rep("Salvinia", len.sc), rep("Aegilops", len.aegi),
                          rep("Sorghum", len.sb), rep("Chara", len.chara),
                          rep("Guillardia", len.guilla), rep("Cryptophyceae", len.crypto),
                          rep("Cyanidioschyzon", len.cymero), rep("Galdieria", len.galsul),
                          rep("Gracilariopsis", len.gracichor), rep("Scenedesmus", len.sceobli),
                          rep("Coccomyxa", len.cocco), rep("Saccharina", len.saccha),
                          rep("Haematococcus", len.haema), rep("Zea", len.zea))
    
    return(organims_reduced)
  }) %>% bindEvent(input$run_button1)
  
  tree_plot1 <- reactive({
    
    # Define previous variables
    tree_reduced <- tree_reduced1()
    gene.name.tree <- input$geneInt1
    tree <- tree_adj1()
    
    tips_to_keep.mp <- tips_to_keep.mp1()
    tips_to_keep.ot <- tips_to_keep.ot1()
    tips_to_keep.at <- tips_to_keep.at1()
    tips_to_keep.cp <- tips_to_keep.cp1()
    tips_to_keep.cr <- tips_to_keep.cr1()
    tips_to_keep.cz <- tips_to_keep.cz1()
    tips_to_keep.kn <- tips_to_keep.kn1()
    tips_to_keep.me <- tips_to_keep.me1()
    tips_to_keep.mi <- tips_to_keep.mi1()
    tips_to_keep.pp <- tips_to_keep.pp1()
    tips_to_keep.sl <- tips_to_keep.sl1()
    tips_to_keep.sm <- tips_to_keep.sm1()
    tips_to_keep.sp <- tips_to_keep.sp1()
    tips_to_keep.ta <- tips_to_keep.ta1()
    tips_to_keep.vc <- tips_to_keep.vc1()
    tips_to_keep.bp <- tips_to_keep.bp1()
    tips_to_keep.cri <- tips_to_keep.cri1()
    tips_to_keep.ds <- tips_to_keep.ds1()
    tips_to_keep.os <- tips_to_keep.os1()
    tips_to_keep.smag <- tips_to_keep.smag1()
    tips_to_keep.tp <- tips_to_keep.tp1()
    tips_to_keep.aa <- tips_to_keep.aa1()
    tips_to_keep.um <- tips_to_keep.um1()
    tips_to_keep.rs <- tips_to_keep.rs1()
    tips_to_keep.cyc <- tips_to_keep.cyc1()
    tips_to_keep.pu <- tips_to_keep.pu1()
    tips_to_keep.pt <- tips_to_keep.pt1()
    tips_to_keep.ng <- tips_to_keep.ng1()
    tips_to_keep.cyano <- tips_to_keep.cyano1()
    tips_to_keep.ca <- tips_to_keep.ca1()
    tips_to_keep.mv <- tips_to_keep.mv1()
    tips_to_keep.af <- tips_to_keep.af1()
    tips_to_keep.sc <- tips_to_keep.sc1()
    tips_to_keep.aegi <- tips_to_keep.aegi1()
    tips_to_keep.sb <- tips_to_keep.sb1()
    tips_to_keep.chara <- tips_to_keep.chara1()
    tips_to_keep.guilla <- tips_to_keep.guilla1()
    tips_to_keep.crypto <- tips_to_keep.crypto1()
    tips_to_keep.cymero <- tips_to_keep.cymero1()
    tips_to_keep.galsul <- tips_to_keep.galsul1()
    tips_to_keep.gracichor <- tips_to_keep.gracichor1()
    tips_to_keep.sceobli <- tips_to_keep.sceobli1()
    tips_to_keep.cocco <- tips_to_keep.cocco1()
    tips_to_keep.saccha <- tips_to_keep.saccha1()
    tips_to_keep.haema <- tips_to_keep.haema1()
    tips_to_keep.zm <- tips_to_keep.zm1()
    
    if (length(tree_reduced$tip.label) < 2)
    {
      cat("")
    }
    else 
    {
      # Highlight the target gene
      high.gene <<- tree_reduced$tip.label[grep(pattern = gene.name.tree, tree_reduced$tip.label)]
      
      
      # Color asignment per species
      col.factor <- c()
      org.factor <- c()
      
      library(glue)
      library(ggtree)
      library(ggplot2)
      
      for (i in 1:length(tree_reduced$tip.label))
      {
        if (tree_reduced$tip.label[i] %in% high.gene)
        {
          col.factor <- c(col.factor,"#CD0000")
          org.factor <- c(org.factor,"Gen of interest")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mp])
        {
          col.factor <- c(col.factor,"#006400")
          org.factor <- c(org.factor,"Marchantia")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ot])
        {
          col.factor <- c(col.factor,"#00008B")
          org.factor <- c(org.factor,"Ostreococcus")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.at])
        {
          col.factor <- c(col.factor,"#CD661D")
          org.factor <- c(org.factor,"Arabidopsis")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cp])
        {
          col.factor <- c(col.factor,"#458B74")
          org.factor <- c(org.factor,"Ceratodon")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cr])
        {
          col.factor <- c(col.factor,"#8B7355")
          org.factor <- c(org.factor,"Chlamydomonas")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cz])
        {
          col.factor <- c(col.factor,"#458B00")
          org.factor <- c(org.factor,"Chromochloris")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.kn])
        {
          col.factor <- c(col.factor,"#CD1076")
          org.factor <- c(org.factor,"Klebsormidium")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.me])
        {
          col.factor <- c(col.factor,"#8B8878")
          org.factor <- c(org.factor,"Mesotaenium")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mi])
        {
          col.factor <- c(col.factor,"#666666")
          org.factor <- c(org.factor,"Micromonas")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pp])
        {
          col.factor <- c(col.factor,"#B8860B")
          org.factor <- c(org.factor,"Physcomitrium")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sl])
        {
          col.factor <- c(col.factor,"#8B008B")
          org.factor <- c(org.factor,"Solanum")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sm])
        {
          col.factor <- c(col.factor,"#6E8B3D")
          org.factor <- c(org.factor,"Selaginella")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sp])
        {
          col.factor <- c(col.factor,"#79CDCD")
          org.factor <- c(org.factor,"Spirogloea")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ta])
        {
          col.factor <- c(col.factor,"#CDCD00")
          org.factor <- c(org.factor,"Triticum")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.vc])
        {
          col.factor <- c(col.factor,"#16317d")
          org.factor <- c(org.factor,"Volvox")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.bp])
        {
          col.factor <- c(col.factor,"#007e2f")
          org.factor <- c(org.factor,"Bathycoccus")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cri])
        {
          col.factor <- c(col.factor,"#ffcd12")
          org.factor <- c(org.factor,"Ceratopteris")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ds])
        {
          col.factor <- c(col.factor,"#b86092")
          org.factor <- c(org.factor,"Dunaliella")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.os])
        {
          col.factor <- c(col.factor,"#721b3e")
          org.factor <- c(org.factor,"Oryza")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.smag])
        {
          col.factor <- c(col.factor,"#00b7a7")
          org.factor <- c(org.factor,"Sphagnum")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.tp])
        {
          col.factor <- c(col.factor,"#67000d")
          org.factor <- c(org.factor,"Thuja")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.aa])
        {
          col.factor <- c(col.factor,"#5b2c6f")
          org.factor <- c(org.factor,"Anthoceros")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.um])
        {
          col.factor <- c(col.factor,"#15e71b")
          org.factor <- c(org.factor,"Ulva")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.rs])
        {
          col.factor <- c(col.factor,"#e67e22")
          org.factor <- c(org.factor,"Raphidocelis")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cyc])
        {
          col.factor <- c(col.factor,"#873600")
          org.factor <- c(org.factor,"Cycas")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pu])
        {
          col.factor <- c(col.factor,"#dc1c0f")
          org.factor <- c(org.factor,"Porphyra")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pt])
        {
          col.factor <- c(col.factor,"#a04000")
          org.factor <- c(org.factor,"Phaeodactylum")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ng])
        {
          col.factor <- c(col.factor,"#935116")
          org.factor <- c(org.factor,"Nannochloropsis")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cyano])
        {
          col.factor <- c(col.factor,"#2874a6")
          org.factor <- c(org.factor,"Cyanophora")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ca])
        {
          col.factor <- c(col.factor,"#0b5345")
          org.factor <- c(org.factor,"Chlorokybus")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mv])
        {
          col.factor <- c(col.factor,"#283747")
          org.factor <- c(org.factor,"Mesostigma")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.af])
        {
          col.factor <- c(col.factor,"#145a32")
          org.factor <- c(org.factor,"Azolla")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sc])
        {
          col.factor <- c(col.factor,"#3339e6")
          org.factor <- c(org.factor,"Salvinia")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.aegi])
        {
          col.factor <- c(col.factor,"#e6338f")
          org.factor <- c(org.factor,"Aegilops")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sb])
        {
          col.factor <- c(col.factor,"#cd016a")
          org.factor <- c(org.factor,"Sorghum")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.chara])
        {
          col.factor <- c(col.factor,"#117a65")
          org.factor <- c(org.factor,"Chara")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.guilla])
        {
          col.factor <- c(col.factor,"#424949")
          org.factor <- c(org.factor,"Guillardia")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.crypto])
        {
          col.factor <- c(col.factor,"#515a5a")
          org.factor <- c(org.factor,"Cryptophyceae")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cymero])
        {
          col.factor <- c(col.factor,"#641e16")
          org.factor <- c(org.factor,"Cyanidioschyzon")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.galsul])
        {
          col.factor <- c(col.factor,"#633974")
          org.factor <- c(org.factor,"Galdieria")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.gracichor])
        {
          col.factor <- c(col.factor,"#a93226")
          org.factor <- c(org.factor,"Gracilariopsis")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sceobli])
        {
          col.factor <- c(col.factor,"#148f77")
          org.factor <- c(org.factor,"Scenedesmus")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cocco])
        {
          col.factor <- c(col.factor,"#9c640c")
          org.factor <- c(org.factor,"Coccomyxa")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.saccha])
        {
          col.factor <- c(col.factor,"#6e2c00")
          org.factor <- c(org.factor,"Saccharina")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.haema])
        {
          col.factor <- c(col.factor,"#196f3d")
          org.factor <- c(org.factor,"Haematococcus")
        }
        else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.zm])
        {
          col.factor <- c(col.factor,"#666909")
          org.factor <- c(org.factor,"Zea")
        }
        
      }
      
      
      
      #Matrix with labels and colors and transform to dplyr format
      data.tree <- data.frame(node = 1:length(tree_reduced$tip.label), label = tree_reduced$tip.label,
                              col = col.factor, org = org.factor)
      
      d2 <- dplyr::mutate(data.tree, lab = data.tree$label,
                          color = data.tree$col,
                          organism = data.tree$org,
                          name = glue("<i style='color:{color}'> {lab} </i>"))
      { 
      if (build_trees1() == "FastTree")
      {
      tree_plot <- ggtree(tree_reduced) %<+% d2 + geom_tiplab() + theme(legend.position =) +
        xlim(0, max(tree_reduced$edge.length)*3) + geom_tiplab(aes(label = label, color = organism)) +
        scale_color_manual(values = unique(d2$col), breaks = unique(d2$org)) +
        geom_highlight(mapping=aes(subset = label %in% high.gene,
                                   node = node,
                                   fill = as.factor(node)), extend = 0.8) + 
        labs(fill = "Node of interest")
      
      }
      else
      {
        tree_plot <- ggtree(tree_reduced) %<+% d2 + geom_tiplab() + theme(legend.position =) +
          xlim(0, max(tree_reduced$edge.length)*3) + geom_tiplab(aes(label = label, color = organism)) +
          geom_nodelab() +
          scale_color_manual(values = unique(d2$col), breaks = unique(d2$org)) +
          geom_highlight(mapping=aes(subset = label %in% high.gene,
                                     node = node,
                                     fill = as.factor(node)), extend = 0.8) + 
          labs(fill = "Node of interest")
      }
      }
      shinyjs::hideElement(id = 'loading.tree1')
      return(tree_plot)
    }}) %>% bindEvent(input$run_button1)
  

  # Outputs
  observeEvent(isTruthy(tree_plot1()), {
    output$error_tree1 <- NULL
  })
  
  # Create boxes
  observeEvent(isTruthy(tree_plot1()), {
    
    if (UI_exist_tree1)
    {
      removeUI(
        selector = "div:has(>> #treeTips1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>>> #presentorg1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #tree_image1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadTree1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadNewick1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadTreeSeqs1)",
        multiple = TRUE,
        immediate = TRUE
      )
    }
    
    
    insertUI("#boxouttext1", "afterEnd", ui = {
      box(
        title = "Organisms", status = "info", solidHeader = TRUE, width = 12,
        collapsible = TRUE,
        verbatimTextOutput("treeTips1")
      )
    }) 
    
    insertUI("#boxouttext2", "afterEnd", ui = {
      box(
        title = "Tree method", status = "info", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        fluidRow(column(1), imageOutput("presentorg1"))
      )
    }) 
    
    insertUI("#boxout1", "afterEnd", ui = {
      box(width = 12,
          title = "Image", status = "info", solidHeader = TRUE,
          collapsible = TRUE, 
          plotOutput("tree_image1", height = 500)
      )
    })
      
      insertUI("#download_tree1", "afterEnd", ui = {
        tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadTree1", 
                                                                           "Download Tree Plot",
                                                                           size = "sm", color = "primary"))
      })
      
      insertUI("#download_newick1", "afterEnd", ui = {
        tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadNewick1", 
                                                                           "Download NEWICK Tree",
                                                                           size = "sm", color = "primary"))
      })
      
      insertUI("#download_tree_seqs1", "afterEnd", ui = {
        tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadTreeSeqs1", 
                                                                           "Download Protein Sequences",
                                                                           size = "sm", color = "primary"))
      })
 
    UI_exist_tree1 <<- TRUE
    shinyjs::hideElement(id = 'loading.tree1')
  })
  
  # Fill boxes with output
  output$treeTips1 <- renderPrint({
    print(tree_reduced1()$tip.label)
  }, width = 400) # %>% bindEvent(input$run_button1)

  # Render pie chart
  output$presentorg1 <- renderImage({

    {library(ggplot2)
    library(dplyr)

    data <- data.frame(table(organims_reduced1()))
    colnames(data) <- c("group", "value")
  
  # Compute the position of labels
    data <- data %>%
      arrange(desc(group)) %>%
      mutate(prop = value / sum(data$value) *100) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop )

  # Create plot
    a <- ggplot(data, aes(x="", y=prop, fill=group)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() +
      theme(legend.position="none") +
      geom_text(aes(y = ypos, label = group), color = "white", size=6) +
      scale_fill_brewer(palette="Set1")}

    png("organims_reduced1.png", height = 450, width = 450)
    plot(a)
    dev.off()

    list(src = "organims_reduced1.png",
         contentType="image/png", width=400,height=400)
  }, deleteFile = T)

  # Render tree image
  output$tree_image1 <- renderImage({
    png("tree1.png", height = 500, width = 1000)
    plot(tree_plot1())
    dev.off()

    list(src = "tree1.png",
         contentType="image/png", width=1000,height=500)
  }, deleteFile = T)

 # Download results
  output$downloadTree1 <- downloadHandler(
    filename= function() {
      paste("tree", ".png", sep="")
    },
    content= function(file) {
      image_height <- 300 + 11*length(tree_reduced1()$tip.label)
      image_width <- 200 + 400*max(tree_reduced1()$edge.length)
      png(file, height = image_height, width = image_width)
      plot(tree_plot1())
      dev.off()
    })

 # Create and download tree in newick format
 output$downloadNewick1 <- downloadHandler(
    filename= function() {
      paste("tree_newick", ".txt", sep="")
    },
    content= function(file) {
      write.tree(tree_reduced1(), file)
    })

 #  # Create and download sequences for genes in tree
  output$downloadTreeSeqs1 <- downloadHandler(
    filename= function() {
      paste("tree_seqs", ".fa", sep="")
    },
    content= function(file) {
      seqinr::write.fasta(sequences = seqinr::getSequence(ortho_reduced1()),
                  names = seqinr::getName(ortho_reduced1()), file.out = file)
    })
  
  
  
  ###########################PFAM##################################
  
  observeEvent(input$run_button1, {
    removeUI(
      selector = "div:has(>> #selected_pfamsI1)",
      multiple = TRUE,
      immediate = TRUE
    )
  })
  
  observeEvent(input$pfam_start1, {
    insertUI("#selected_pfams1", "afterEnd", ui = {
      
      shinyWidgets::pickerInput("selected_pfamsI1","Select the desired genes from the tree",
                                choices=isolate({tree_reduced1()$tip.label}), options = list(`actions-box` = TRUE),
                                multiple = T, selected = isolate({input$geneInt1}))
    })
  })
  
  observeEvent(input$run_button1, {
    removeUI("#pfam_selection1")
  })
  
  observeEvent(input$pfam_start1, {
    insertUI("#pfam_selectionI1", "afterEnd", ui = {
      
      shinyWidgets::actionBttn("pfam_selection1", "Show Pfam Domains", size = "sm",
                               style = "float", color = "primary")
    })
  })
  
  
  observeEvent(input$run_button1, {
    if (UI_exist_pfam1)
    {
      removeUI(
        selector = "div:has(>> #output_pfam_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #pfam_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #pfam_download1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadPFAMTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      UI_exist_pfam1 <<- F
    }
  })
  
  
  total_table_pfam1 <- reactive({
    shinyjs::showElement(id = 'loading.pfam.pf1')
    ortho_reduced <- ortho_reduced1()
    sel_genes <- as.vector(input$selected_pfamsI1)
    
    if (length(sel_genes) < 1)
    {
      shinyjs::hideElement(id = 'loading.pfam.pf1')
      removeUI(
        selector = "div:has(>> #output_pfam_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #pfam_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #pfam_download1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadPFAMTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      UI_exist_pfam1 <<- F
      output$error_pfam1 <- renderUI({renderText({print("Please select at least one gene.")})})
      validate(need(length(sel_genes) > 0, "Please select at least one gene."))
    }
    
    output$error_pfam1 <- NULL
    #library(bio3d)
    library(RCurl)
    library(drawProteins)
    library(ggplot2)
    
    # Get the sequences as a vector of strings
    
    
    # Create data frame with proper columns
    total_table_pfam <- data.frame(type=NA,
                                   description=NA,
                                   begin=NA, end=NA,
                                   length=NA,
                                   accession=NA, entryName=NA,
                                   taxid=NA, order=NA)
    
    
    # Fill data frame with the information about domains obtained with hmmer
    for (i in 1:length(sel_genes))
    {
      ortho_comp <- ortho_reduced[[sel_genes[i]]]
      ortho_str <- seqinr::getSequence(ortho_comp, as.string = T)
      ortho_cha <- unlist(ortho_str)
      
      
      
      url <- paste("https://www.ebi.ac.uk/Tools/hmmer/search/", "hmmscan", sep = "")
      curl.opts <- list(httpheader = "Expect:", httpheader = "Accept:text/xml", verbose = T, followlocation = TRUE)
      curl_env <- getCurlHandle()
      
      
      RCurl::postForm(url, hmmdb = "pfam", seqdb = NULL,  seq = ortho_cha ,  style = "POST", .opts = curl.opts,  .contentEncodeFun = RCurl::curlPercentEncode,  .checkParams = TRUE, curl=curl_env)
      
      curl_info <- getCurlInfo(curl_env, which = getCurlInfoConstants())
      
      
      
      if (curl_info$response.code == 200)
      {
        url_vec <- strsplit(curl_info$effective.url, split = "/")
        url_vec[[1]][1] <- "https:"
        url_vec[[1]][6] <- "download"
        url_vec[[1]][8] <- "score?format=tsv"
        url_tsv <- paste0(url_vec[[1]], collapse = "/")
        tsv_res <- getURL(url_tsv)
        nap.time <- 0
        while (strsplit(tsv_res, "\t")[[1]][1] != "Family id")
        {
          nap.time <- nap.time + 5
          tsv_res <- getURL(url_tsv)
          Sys.sleep(nap.time)
          if (nap.time > 25){
            shinyjs::hideElement(id = 'loading.pfam.pf1')
            break
          }
        }
        
        validate(need(nap.time < 25,"Connection time too high."))
        res_pfam <- read.csv(textConnection(tsv_res), header = T, sep="\t")
        pfam_table <- data.frame(type=c("CHAIN", rep("DOMAIN", nrow(res_pfam))),
                                 description=c("Protein chain",res_pfam$Family.Accession),
                                 begin=c(1, res_pfam$Env..Star), end=c(nchar(ortho_cha),res_pfam$Env..End),
                                 length=c(nchar(ortho_cha)-1, res_pfam$Env..End-res_pfam$Env..Start),
                                 accession=sel_genes[i], entryName=sel_genes[i],
                                 taxid=c("Chain", res_pfam$Description), order=i)
        
        total_table_pfam <- rbind(total_table_pfam, pfam_table)
        
      }
      else
      {
        pfam_table <- data.frame(type="CHAIN",
                                 description="Protein chain",
                                 begin=1, end=nchar(ortho_cha),
                                 length=nchar(ortho_cha)-1,
                                 accession=sel_genes[i], entryName=sel_genes[i],
                                 taxid="Chain", order=i)
        total_table_pfam <- rbind(total_table_pfam, pfam_table)
      }
    }
    
    total_table_pfam <- total_table_pfam[-1,]
    
    
    return(total_table_pfam)
    
  }) %>% bindEvent(input$pfam_selection1)
  
  pfplot1 <- reactive({
    
    total_table_pfam <- total_table_pfam1()
    # Now we can plot domains information as chains
    pfplot <- draw_canvas(total_table_pfam)
    pfplot <- draw_chains(pfplot, total_table_pfam)
    pfplot <- draw_domains(pfplot, total_table_pfam, label_domains = F)
    pfplot <- pfplot + theme_bw(base_size = 20) + # white background
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank()) +
      theme(axis.ticks = element_blank(),
            axis.text.y = element_blank()) +
      theme(panel.border = element_blank())
    #pfplot <- pfplot + labs(title = "Pfam domains")
    pfplot <- pfplot + theme(legend.position="top") + labs(fill="")
    
  }) %>% bindEvent(input$pfam_selection1)
  
  
  # Outputs
  
  observeEvent(isTruthy(pfplot1()), {
    
    if (UI_exist_pfam1)
    {
     removeUI(
           selector = "div:has(>> #output_pfam_table1)",
           multiple = TRUE,
           immediate = TRUE
         )
      
      removeUI(
        selector = "div:has(>> #pfam_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #pfam_download1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadPFAMTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
    
    }
    
      insertUI("#box_pfam1", "afterEnd", ui = {
        
        box(
          title = "PFAM Table", status = "info", solidHeader = TRUE, width = 12,
          collapsible = TRUE,
          dataTableOutput(outputId = "output_pfam_table1"))
    }) 
      
      insertUI("#box_pfplot1", "afterEnd", ui = {
        total_table_pfam <- total_table_pfam1()
        box_pfplot_height <- 150 + 700*length(total_table_pfam$order[nrow(total_table_pfam)])
        box(
          title = "PFAM Table", status = "info", solidHeader = TRUE, width = 12, #height = box_pfplot_height,
          collapsible = TRUE,
          imageOutput("pfam_plot1"))
      }) 
      
      insertUI("#pfam_down_button1", "afterEnd", ui = {
        tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "pfam_download1", "Download PFAM figures",
                                              size = "sm", color = "primary"))
        })
      
      insertUI("#download_ui_for_pfam_table1", "afterEnd", ui = {
        tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadPFAMTable1", "Download PFAM Table",
                                            size = "sm", color = "primary"))
      })
      
    UI_exist_pfam1 <<- TRUE
    shinyjs::hideElement(id = 'loading.pfam.pf1')
  })
  
  output$output_pfam_table1 <- renderDataTable({
    total_table_pfam <- total_table_pfam1()
    out_pf_table <- subset(total_table_pfam[,c(1:6,8)], total_table_pfam$type != "CHAIN")
    out_pf_table$description <- sapply(out_pf_table$description, function(x) pfam.link(x))
    colnames(out_pf_table) <- c(colnames(total_table_pfam)[1:6],"biological description")
    return(out_pf_table)
  }, escape=FALSE, options = list(pageLength = 5))
  
  output$pfam_plot1 <- renderImage({
    total_table_pfam <- total_table_pfam1()
    pfam_height <- 50 + 700*length(total_table_pfam$order[nrow(total_table_pfam)])
    pfam_width <- 1000
    pfplot <- pfplot1()
    png("pharaoh_folder/pfam.png",  width = pfam_width, height = pfam_height)
    plot(pfplot)
    dev.off()
    list(src = "pharaoh_folder/pfam.png",
         contentType="image/png")
  }, deleteFile = T
  )
  
  # Download results

  output$pfam_download1 <- downloadHandler(
    filename= function() {
      paste("pfam", ".png", sep="")
    },
    content= function(file) {
      total_table_pfam <- total_table_pfam1()
      pfam_height <- 50 + 700*length(total_table_pfam$order[nrow(total_table_pfam)])
      pfam_width <- 1150
      pfplot <- pfplot1()

      png(file, height = pfam_height, width = pfam_width)
      plot(pfplot)
      dev.off()
    })

  output$downloadPFAMTable1<- downloadHandler(
    filename= function() {
      paste("pfam_table", ".tsv", sep="")
    },
    content= function(file) {
      total_table_pfam <- total_table_pfam1()
      out_pf_table <- subset(total_table_pfam[,c(1:6,8)], total_table_pfam$type != "CHAIN")
      colnames(out_pf_table) <- c(colnames(total_table_pfam)[1:6],"biological description")
      write.table(x = out_pf_table, quote = F,sep = "\t",
                  file=file,row.names=FALSE,col.names=TRUE)
    })
  
  
####################### CAFE #################################
  
  # Remove previous outputs when updated by a new search
  observeEvent(input$run_button1, {
    if (UI_exist_cafe1)
    {
      removeUI(
        selector = "div:has(>> #cafe_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )

      removeUI(
        selector = "div:has(>> #cafe_mrca1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #cafe_download1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadCAFEPlot1)",
        multiple = TRUE,
        immediate = TRUE
      )

      UI_exist_cafe1 <<- F
    }
    
    if (UI_exist_error_cafe1)
    {
      removeUI(
        selector = "div:has(>> #cafe_error_message1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      UI_exist_error_cafe1 <<- F
    }
  })

  ### CAFE parser and tree generator
  cafe_tree1 <- reactive({

    shinyjs::showElement(id = 'loading.cafe1')

    library(ape)

    # Import OG name
    og.cafe <- og.name1()

    # Define path to CAFE trees file
    cafe_comp_tree_file <- ifelse(model.selected1(), "pharaoh_folder/global_cafe.tre",
                                  "pharaoh_folder/green_cafe.tre")

    # Extract CAFE tree for current OG
    cafe.tree.set <- ape::read.nexus(cafe_comp_tree_file)
    cafe.tree <- cafe.tree.set[[og.cafe]]
    
    if (length(cafe.tree) < 1)
    {
      shinyjs::hideElement(id = 'loading.cafe1')
      if (UI_exist_cafe1)
      {
        removeUI(
          selector = "div:has(>> #cafe_plot1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #cafe_mrca1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #cafe_download1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadCAFEPlot1)",
          multiple = TRUE,
          immediate = TRUE
        )
      }
      
      UI_exist_cafe1 <<- F
      
      if(UI_exist_error_cafe1)
      {
        removeUI(
          selector = "div:has(>> #cafe_error_message1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
      }
        insertUI("#error_cafe1", "afterEnd", ui = {
          box(width = 12,
              title = "Image", status = "info", solidHeader = TRUE,
              collapsible = TRUE,
              textOutput("cafe_error_message1"))
        })
        
        output$cafe_error_message1 <- renderText({print("No expansions/contraction detected for this orthogroup,
                                                        or infeasible computation due to large size and variance across
                                                        species.")})
        UI_exist_error_cafe1 <<- T
      
      validate(need(length(cafe.tree) > 0 , ""))
    }
    
    return(cafe.tree)
  }) %>% bindEvent(input$cafe_start1)

  evo_plot1 <- reactive({
    
    og.cafe <- og.name1()
    cafe.tree <- cafe_tree1()
    
    # Show an error if the orthogroup is not significantly expanded/collapsed in any branch

    model.node.number <- ifelse(model.selected1(), 46, 36)
    total.model.node.number <- ifelse(model.selected1(), 91, 71)

    node.count <- sapply(strsplit(cafe.tree$node.label, split = ">"), function(x) x[[2]])
    node.count.clean <- gsub("[_]", "", node.count)

    tip.count <- sapply(strsplit(cafe.tree$tip.label, split = ">"), function(x) x[[2]])
    tip.count.clean <- gsub("[_]", "", tip.count)

    # Identify parental node for significant changes to determine if a change
    # corresponds to an expansion or to a contraction only if significant changes
    # are detected

    # Nodes with significant changes are labelled with a *
    tip.sig <- grep("[*]", tip.count.clean)
    node.sig <- grep("[*]", node.count.clean)

    #Create a table with edges to identify parental nodes
    edge_table <- as.data.frame(cafe.tree$edge)
    rownames(edge_table) <- paste("edge", 1:nrow(edge_table), sep = "")
    colnames(edge_table) <- c("parent", "child")

    {
      if (length(tip.sig) + length(node.sig) == 0)
      {
        change_vector <- rep("No significant changes", length(node.count.clean) + length(tip.count.clean))
      }

      else
      {
        # For tips
        exp_cont_tip <- sapply(tip.sig, function(x)
          if(as.numeric(gsub("[*]", "", node.count.clean[edge_table$parent[match(x, edge_table$child)]-model.node.number])) >
             as.numeric(gsub("[*]", "", tip.count.clean[x]))) "Significant Contraction"
          else "Significant Expansion"
        )

        # For nodes
        exp_cont_nodes <- sapply(node.sig, function(x)
          if(as.numeric(gsub("[*]", "", node.count.clean[edge_table$parent[match(x+model.node.number, edge_table$child)]-model.node.number])) >
             as.numeric(gsub("[*]", "", node.count.clean[x]))) "Significant Contraction"
          else "Significant Expansion"
        )

        # Create a sorted vector with change categories
        change_vector <- rep("No significant changes", length(node.count.clean) + length(tip.count.clean))
        change_vector[tip.sig] <- exp_cont_tip
        change_vector[node.sig + model.node.number] <- exp_cont_nodes

      }
    }

    # Merge tips and nodes reconstruction
    cafe.count <- c(tip.count.clean, node.count.clean)

    # Create phylogenomic tree with internal nodes names

    mrca.tree <- read.tree(ifelse(model.selected1(), "pharaoh_folder/species_tree_global.txt",
                                  "pharaoh_folder/species_tree_green.txt"))

    node.names <- read.csv(ifelse(model.selected1(), "pharaoh_folder/tax_labels_global.tsv",
                                  "pharaoh_folder/tax_labels_green.tsv"), header = F, sep="\t")

    mrca.tree$node.label <- node.names$V2

    # Create a timeline for a given OG

    tree.name <- ifelse(model.selected1(),
                        paste("Global_Gene_Trees",paste(og.cafe, "tree.txt", sep = "_"), sep="/"),
                        paste("Green_Gene_Trees",paste(og.cafe, "tree.txt", sep = "_"), sep="/"))
    tree.ancestor <- read.tree(tree.name)
    tips.orgs1 <- sapply(strsplit(as.character(tree.ancestor$tip.label), "_"), function(x) x[[1]])
    tips.orgs2 <- sapply(strsplit(as.character(tree.ancestor$tip.label), "_"), function(x) x[[2]])
    tips.orgs <- paste(tips.orgs1, tips.orgs2, sep = "_")

    mrca.id <- getMRCA(mrca.tree,unique(tips.orgs))
    evo.paths <- c()
    for (i in 1:length(unique(tips.orgs)))
    {
      evo.paths <- c(evo.paths, nodepath(mrca.tree, mrca.id, which(unique(tips.orgs)[i] == mrca.tree$tip.label)))
    }

    evo.paths <- unique(evo.paths)
    evo.paths.id <- sapply(evo.paths, function(x) if (x <= model.node.number) mrca.tree$tip.label[x] else mrca.tree$node.label[x-model.node.number])


    # Associate gray and 0 to reconstruction for nodes not in allowed paths
    change_vector[setdiff(1:total.model.node.number, evo.paths)] <- "OG not present"
    cafe.count[setdiff(1:total.model.node.number, evo.paths)] <- 0


    color_cafe <- sapply(change_vector, function(x) if (x == "No significant changes") "black"
                         else if (x == "Significant Expansion") "red" else if (x == "Significant Contraction") "blue"
                         else "gray", USE.NAMES = F)

    # Create tree representation
    cafe.table.tips <- data.frame(node = 1:length(mrca.tree$tip.label), label = mrca.tree$tip.label,
                                  col = color_cafe[1:length(mrca.tree$tip.label)], reconst = change_vector[1:length(mrca.tree$tip.label)],
                                  dup_number = cafe.count[1:length(mrca.tree$tip.label)])

    cafe.table.nodes <- data.frame(node = (model.node.number+1):(model.node.number+length(mrca.tree$node.label)), label = mrca.tree$node.label,
                                   col = color_cafe[(model.node.number+1):(model.node.number+length(mrca.tree$node.label))],
                                   reconst = change_vector[(model.node.number+1):(model.node.number+length(mrca.tree$node.label))],
                                   dup_number = cafe.count[(model.node.number+1):(model.node.number+length(mrca.tree$node.label))])

    cafe.table.node.comp <- rbind(cafe.table.tips, cafe.table.nodes)

    d <- dplyr::mutate(cafe.table.node.comp)

    library(ggtree)
    library(ggplot2)

    evo_plot <- ggtree(mrca.tree, layout = "ellipse") %<+% d + aes(colour = I(d$col)) +
      geom_tiplab(aes(label=gsub("_", " ", tools::toTitleCase(d$label))), offset = 30) +
      theme(legend.position = "none") +
      xlim(0, max(mrca.tree$edge.length)*1.5) +
      geom_nodepoint(aes(color=d$col, size = as.numeric(gsub("[*]", "", d$dup_number))*4/max(as.numeric(gsub("[*]", "", d$dup_number)))),
                     alpha = .75) +
      scale_color_manual(values = unique(d$col), breaks = unique(d$col)) +
      geom_tippoint(aes(color=d$col, size = as.numeric(gsub("[*]", "", d$dup_number))*4/max(as.numeric(gsub("[*]", "", d$dup_number)))),
                    alpha = .75)

    return(evo_plot)

  }) %>% bindEvent(input$cafe_start1)

  evo.paths.id1 <- reactive({

    # Create phylogenomic tree with internal nodes names
    og.cafe <- og.name1()
    model.node.number <- ifelse(model.selected1(), 46, 36)

    mrca.tree <- read.tree(ifelse(model.selected1(), "pharaoh_folder/species_tree_global.txt",
                                  "pharaoh_folder/species_tree_green.txt"))

    node.names <- read.csv(ifelse(model.selected1(), "pharaoh_folder/tax_labels_global.tsv",
                                  "pharaoh_folder/tax_labels_green.tsv"), header = F, sep="\t")

    mrca.tree$node.label <- node.names$V2

    # Create timeline
    tree.name <- ifelse(model.selected1(),
                        paste("Global_Gene_Trees",paste(og.cafe, "tree.txt", sep = "_"), sep="/"),
                        paste("Green_Gene_Trees",paste(og.cafe, "tree.txt", sep = "_"), sep="/"))

    tree.ancestor <- read.tree(tree.name)
    tips.orgs1 <- sapply(strsplit(as.character(tree.ancestor$tip.label), "_"), function(x) x[[1]])
    tips.orgs2 <- sapply(strsplit(as.character(tree.ancestor$tip.label), "_"), function(x) x[[2]])
    tips.orgs <- paste(tips.orgs1, tips.orgs2, sep = "_")

    mrca.id <- getMRCA(mrca.tree,unique(tips.orgs))
    evo.paths <- c()
    for (i in 1:length(unique(tips.orgs)))
    {
      evo.paths <- c(evo.paths, nodepath(mrca.tree, mrca.id, which(unique(tips.orgs)[i] == mrca.tree$tip.label)))
    }

    evo.paths <- unique(evo.paths)
    evo.paths.id <- sapply(evo.paths, function(x) if (x <= model.node.number) mrca.tree$tip.label[x] else mrca.tree$node.label[x-model.node.number])
    return(evo.paths.id)

  }) %>% bindEvent(input$cafe_start1)

  # Outputs

  # Remove previous boxes if they exist and create new ones
  observeEvent(isTruthy(evo_plot1()), {

    if (UI_exist_cafe1)
    {
      removeUI(
        selector = "div:has(>> #cafe_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )

      removeUI(
        selector = "div:has(>> #cafe_mrca1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #cafe_download1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadCAFEPlot1)",
        multiple = TRUE,
        immediate = TRUE
      )

    }
    
    if (UI_exist_error_cafe1)
    {
      removeUI(
        selector = "div:has(>> #cafe_error_message1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
    }
    
    insertUI("#box_cafe1", "afterEnd", ui = {
      box(width = 12,
          title = "Image", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          imageOutput("cafe_plot1", height = 500, width = 1000))
    })

    insertUI("#box_mrca1", "afterEnd", ui = {
      box(width = 8,
          title = "Image", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          textOutput("cafe_mrca1")
      )
    })
    
    insertUI("#cafe_down_button1", "afterEnd", ui = {
      tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "cafe_download1", "Download NEWICK tree",
                                                                         size = "sm", color = "primary"))
    })
    
    insertUI("#download_ui_for_cafe_plot1", "afterEnd", ui = {
      tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadCAFEPlot1", "Download Ancestral State Plot",
                                                                         size = "sm", color = "primary"))
    })

    UI_exist_cafe1 <<- TRUE
    shinyjs::hideElement(id = 'loading.cafe1')
  })

 # Fill outputs

  output$cafe_plot1 <- renderImage({
        png("evo_plot1.png", height = 500, width = 1000)
        plot(evo_plot1())
        dev.off()

        list(src = "evo_plot1.png",
             contentType="image/png", width=1000,height=500)
      }, deleteFile = T)

  output$cafe_mrca1 <- renderText({
        print(paste0("Most recent common ancestor for this orthogroup is the
                   ancestor of the clade: ", evo.paths.id1()[1]))
      })
  
  # Download tab's results
  
  output$cafe_download1 <- downloadHandler(
    filename= function() {
      paste("ancestral_newick", ".txt", sep="")
    },
    content= function(file) {
      cafe_tree <- cafe_tree1()
      
      write.tree(cafe_tree, file)
    })
  
  output$downloadCAFEPlot1<- downloadHandler(
    filename= function() {
      paste("ancestral_plot", ".png", sep="")
    },
    content= function(file) {
      evo_plot <- evo_plot1()
      
      png(file)
      plot(evo_plot)
      dev.off()
    })
  
  ####################### MSA #################################
  
  observeEvent(input$run_button1, {
      removeUI(
        selector = "div:has(>> #selected_msaI1)",
        multiple = TRUE,
        immediate = TRUE
      )
    
    removeUI("#msa_selection1")
    
    if (UI_exist_msa1)
    {
      removeUI(
        selector = "div:has(>> #msa_print1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      UI_exist_msa1 <<- F
    }
    
  })
  
  observeEvent(input$msa_start1, {
    insertUI("#selected_msa1", "afterEnd", ui = {
      
      shinyWidgets::pickerInput("selected_msaI1","Select the desired genes from the tree to align",
                                choices=isolate({tree_reduced1()$tip.label}), options = list(`actions-box` = TRUE),
                                multiple = T, selected = isolate({input$geneInt1}))
    })
    
    insertUI("#msa_selectionI1", "afterEnd", ui = {
      
      shinyWidgets::actionBttn("msa_selection1", "Align Sequences", size = "sm",
                               style = "float", color = "primary")
    })
    
  })
  
  alignseqs1 <- reactive({
    
    library(msa)
    shinyjs::showElement(id = 'loading.msa1')
    
    selected_genes <- as.vector(input$selected_msaI1)
    file.name <- og.name1()
    
    if (length(selected_genes) < 2)
    {
      shinyjs::hideElement(id = 'loading.msa1')
      removeUI(
        selector = "div:has(>> #msa_print1)",
        multiple = TRUE,
        immediate = TRUE
      )
      UI_exist_msa1 <<- F
      output$error_msa1 <- renderUI({renderText({print("Please select at least two genes.")})})
      validate(need(length(selected_genes) > 1, "Please select at least two genes."))
    }
    
    output$error_msa1 <- NULL
    
    # Define path to orthogroup sequences file
    ortho.seq.name <- ifelse(model.selected1(),
                        paste("Global_Orthogroup_Sequences", paste(file.name, "fa", sep = "."), sep="/"),
                        paste("Green_Orthogroup_Sequences", paste(file.name, "fa", sep = "."), sep="/"))
    
    
    # Read orthogroup sequences file and select the genes for alignment
    mySequences1 <- Biostrings::readAAStringSet(ortho.seq.name)
    mysubseqs <- mySequences1[selected_genes]
    
    alignseqs <- msa(mysubseqs, verbose = F, method = "ClustalOmega")
    
    
    detach("package:msa", unload=TRUE)
    
    return(alignseqs)
    
    }) %>% bindEvent(input$msa_selection1)
  
  # Create boxes for outputs
  observeEvent(isTruthy(alignseqs1()), {
    
    if (UI_exist_msa1)
    {
      removeUI(
        selector = "div:has(>> #msa_print1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
    }
    
    insertUI("#box_msa1", "afterEnd", ui = {
      box(width = 12,
          title = "Image", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          #verbatimTextOutput("msa_print1")
          msaROutput("msa_print1", width = "60%")
      )
    })
    
    UI_exist_msa1 <<- TRUE
    shinyjs::hideElement(id = 'loading.msa1')
  })
  

  # Outputs
  output$msa_print1 <- renderMsaR({
    alignseqs <- alignseqs1()
    # class(alignseqs) <- "MsaAAMultipleAlignment"
    # cat(msa::print(alignseqs, showConsensus=T, show="complete", 
    #           halfNrow=ceiling(length(as.vector(input$selected_msaI1))/2)))
    hola3 <- msa::msaConvert(alignseqs, "ape::AAbin")
    msaR(hola3, menu=T, overviewbox = F,  colorscheme = "clustal")
  })
  

# End of Gene ID-based search results
   
  
  
# End of the whole server function
     }




shinyApp(ui, server)