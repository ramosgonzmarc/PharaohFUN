# Author: Marcos Ramos Gonzalez

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
library(shinyjs)
library(phylowidget)

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

## Remove gaps from MAFFT Alignment
remove_gaps <- function(multseq)
{
  matrix1 <- do.call(rbind, multseq)
  bool <- apply(matrix1, MARGIN = 2, function(x) sum(x == "-") == nrow(matrix1))
  indexes <- which(bool)
  if(length(indexes) != 0)
  {
    multseq <- lapply(multseq, function(x) x[-indexes])
  }
  return(multseq)
}

## Gene Ontology term link
# http://amigo.geneontology.org/amigo/term/GO:0015979
go.link <- function(go.term)
{
  link <- paste0("http://amigo.geneontology.org/amigo/term/", go.term)
  complete.link <- paste(c("<a href=\"",
                           link,
                           "\" target=\"_blank\">",
                           go.term, "</a>"),
                         collapse = "")
  return(complete.link)
}

## KO term link
# https://www.genome.jp/dbget-bin/www_bget?ko:K12133
ko.link <- function(ko.term)
{
  link <- paste0("https://www.genome.jp/dbget-bin/www_bget?ko:", ko.term)
  complete.link <- paste(c("<a href=\"",
                           link,
                           "\" target=\"_blank\">",
                           ko.term, "</a>"),
                         collapse = "")
  return(complete.link)
}

## KEGG term link
# https://www.genome.jp/dbget-bin/www_bget?ko04712
kegg.link <- function(kegg.term)
{
  link <- paste0("https://www.genome.jp/dbget-bin/www_bget?", kegg.term)
  complete.link <- paste(c("<a href=\"",
                           link,
                           "\" target=\"_blank\">",
                           kegg.term, "</a>"),
                         collapse = "")
  return(complete.link)
}

# Coloring STRING Table
color_string <- function(int_type)
{
  color <- ifelse(int_type == "Interolog", "#CA931B",  "#196F3D")
  comp_chain <- paste0("<i style='color:{", color, "}'> {", int_type, "} </i>")
  return(comp_chain)

}

# JS Functions
# STRING network retrieval
jsCode <- "
    shinyjs.loadStringData = function(gene) {
        getSTRING('https://string-db.org', {
            'identifiers': gene,
            'add_color_nodes':25,
            'network_flavor':'confidence'})
    }"

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

ui <- dashboardPage(
  
  dashboardHeader( #disable = TRUE
    title = "PharaohFUN",
    tags$li(a(href = 'https://www.ibvf.us-csic.es/', tags$img(src = 'logoibvf.png',
                                                              title = "IBVF", height = "20px"), style = "margin-top: 0px;"),
            class = "dropdown"),
    tags$li(a(href = 'https://www.us.es/', tags$img(src = 'Logo_US.png',
                                                    title = "US", height = "20px"), style = "margin-top: 0px;"),
            class = "dropdown"),
    tags$li(a(href = 'https://www.csic.es/es', tags$img(src = '2560px-Logotipo_del_CSIC.svg.png',
                                                        title = "CSIC", height = "20px"), style = "margin-top: 0px;"),
            class = "dropdown")
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
                 href = "https://github.com/fran-romero-campero/AlgaeFUN"),
        menuItem("Contact and Tutorial", icon = icon("envelope"), tabName = "contact_tutorial")
      )
    )
  ),
  dashboardBody(
    ## Body content
    
    tabItems(
      
      # First tab content
      tabItem(tabName = "home",
              #tags$head(tags$style(HTML("a {color: white}"))),
              #h1("PharaohFUN: Phylogenomic Analysis for Plant Protein History and Function Elucidation"),
              #h1(span(HTML(tab2), style = 'color:green; font-weight: bold;')),
              tags$br(),
              
              fluidRow(column(9,
                              tags$div(span("PharaohFUN: Phylogenomic Analysis for Plant Protein History and Function Elucidation",
                                            style = 'color: #1f618d; font-weight: 540; font-size: 42px; font-family: "Alatsi"", Verdana, sans-serif;')),
                              style = "margin-top: 45px; margin-left: 35px;"
                              
              ),
              column(2, 
                     img(
                       src = "pharaohlogo-removebg-preview.png",
                       alt = "logo",
                       width = 220,
                       height = 220, style="display: block; margin-left: 0px``; margin-right: auto;"
                     )
              ),
              column(1)
              ),
              # Put a margin on the right
              fluidRow(
                column(11,
                       tags$br(),
                       tags$br(),
                       tags$div(align="justify", tags$b("PharaohFUN"), "allows researchers to explore orthologous proteins across different evolutionarily 
                       distant species given a target protein. The tool focuses on photosynthetic eukaryotes, whether brown, red, green microalgae
                       or land plants. It offers flexibility in terms of the available search modalities, allowing the use of 
                             up to 5 different ones:", style = 'font-size: 18px; margin-left: 20px;'
                                
                       ),
                       fluidRow(tags$br()),
                       fluidRow(tags$br()),
                       fluidRow(
                         column(1),
                         column(12,
                                valueBox(a("Gene ID", href="#shiny-tab-gene_search", "data-toggle" = "tab", "style" = "color:white"), 
                                         "Gene ID from one of the listed organisms",
                                         icon = icon("dna"), width = 4),
                                
                                valueBox(a("Sequence", href="#shiny-tab-seq_search", "data-toggle" = "tab", "style" = "color:white"), 
                                         "Protein sequence from one of the listed organisms",
                                         icon = icon("a"), width = 4, color = "lime"),
                                
                                valueBox(a("Orthogroup ID", href="#shiny-tab-og_id_search", "data-toggle" = "tab", "style" = "color:white"), 
                                         "Orthogroup ID (from STRING result)",
                                         icon = icon("dashboard"), width = 4, color = "red")
                         )
                       ),
                       fluidRow(
                         
                         column(2),
                         valueBox(a("Batch mode", href="#shiny-tab-batch_search", "data-toggle" = "tab", "style" = "color:white"), 
                                  "Sequences set from one of the listed organisms",
                                  icon = icon("layer-group"), width = 4, color = "orange"),
                         
                         valueBox(a("New organism", href="#shiny-tab-shoot_search", "data-toggle" = "tab", "style" = "color:white"), 
                                  "Protein sequence from any organism",
                                  icon = icon("leaf", lib = "glyphicon"), width = 4, color = "purple")
                       ),
                       
                       fluidRow(tags$br()),
                       tags$div(align="justify", style = 'font-size: 18px; margin-left: 30px;',
                                tags$ol(
                                  tags$li(tags$b("Gene ID-based search"), ". When the identifier of a gene in one of the species available 
                                       in the database is known, it allows the direct search for a gene. Only one gene
                                       per query corresponding to a species supported by the tool."),
                                  tags$li(tags$b("Sequence-based search"),". If an identifier is not known or differs from the nomenclature
                                       used by the tool, this modality maps an amino acid sequence onto the proteome of the 
                                       species, identifying the protein and eliminating possible ambiguities. Only one sequence
                                       per query corresponding to a species supported by the tool."),
                                  tags$li(tags$b("Orthogroup ID-based search"),". When the identifier of an orthogroup is known (typically 
                                       from STRING results of previous searches), it is possible to explore it directly
                                       without resorting to searching for individual genes. Only one orthogroup per query."),
                                  tags$li(tags$b("Batch mode search"),". It allows searching for multiple (up to 300) IDs or sequences 
                                       automatically, creating a folder with the sorted results. All genes must correspond
                                       to a single species supported by the tool."),
                                  tags$li(tags$b("New organism search"),". Unlike the previous cases, if you want to perform a study of 
                                       a protein that does not come from a species supported by the tool, this search method 
                                       allows the inclusion of the sequence in the gene tree and in subsequent analyses. Only
                                       one sequence per query from any organism.")
                                )
                       ),
                       
                       fluidRow(br()),
                       fluidRow(br()),
                       fluidRow(
                         column(7, 
                                tags$div(align="justify", style = 'font-size: 18px; margin-left: 20px;',  br(), 
                                         "The exploration of orthology is based on the construction of orthogroups, sets of genes that descend from a 
                         single gene in the common ancestor of the species under study. In this way, it is possible to trace the evolutionary 
                         history of these genes and to analyze the changes that the orthogroup has undergone from its appearance to 
                         its current situation in the extant species.", br(), br(), "Regarding the species supported by the tool, two different models are offered. 
                         The default model focuses on 36 species of the ", tags$b("Viridiplantae"), " clade, with representatives of the 
                         Chlorophyta and Streptophyta clades chosen to span the evolutionary set of these groups.",
                                         br(), br(),"Furthermore, an additional ", tags$b("Global"), " model includes species from the whole Archaeplastida clade, 
                         as well as examples of Stramenopiles and Cryptophytes. Note that although both models contain the green lineage,
                         their results may vary because the common ancestors of the groups are very distant in time,
                         so for a more accurate analysis it is recommended to use the Viridiplantae model, while for a more 
                         generalist one the Global model should yield more information.", br(), br(), "After the construction of 
                         the tree, PharaohFUN allows the exploration of the proteins encoded by the orthogroup genes, implementing 
                         modules for interactive tree viewing, PFAM module determination, multiple sequence alignment, Gene Ontology 
                         terms annotation, KEGG pathways annotation, exploration of physical interactions between proteins 
                         and scientific literature annotation.")
                         ),
                         column(4,
                                tags$div(style = "margin-left: 20px;",
                                         img(
                                           src = "species_gg46-removebg-preview.png",
                                           alt = "species",
                                           width = 400,
                                           height = 600, style="display: block; margin-left: 20px; margin-right: auto;"
                                         )
                                )
                         ),
                         column(1)
                         
                       ),
                       fluidRow(br()),
                ),
                
                # Right Margin
                column(1)
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
                  choices = c("Maximum Likelihood", "Neighbour Joining", "UPGMA", "Parsimony"),
                  selected = "Maximum Likelihood",
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
                      button within each of the tabs, with specific instructions for each analysis. First, go to Gene Tree tab,
                      which shows a gene tree containing the genes corresponding to the same orthogroup as the query in the
                      selected species. The rest of the tabs allow for a deeper exploration of these genes. Do not start any 
                      analysis before creating gene tree.",
                    div(br()),
                    tabsetPanel(type = "tabs",
                                tabPanel("Gene Tree",
                                         fluidRow(br()),
                                         div("This tab shows three different results. First of all, a complete list of the genes
                                               that are assigned to the same orthogroup as the query. Secondly, the proportion of
                                               genes of each species. Finally, a gene tree show the evolutionary relationships 
                                               between those genes. All results can be downloaded using the buttons at the bottom
                                               of the page."),
                                         fluidRow(br()),
                                         shinyjs::useShinyjs(),
                                         shinyjs::hidden(div(id='loading.tree1',h3('Please be patient, building tree ...'))),
                                         uiOutput(outputId = "error_tree1"),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "box_tree_text1"), tags$div(id = "box_tree_pie1")),
                                         fluidRow(tags$br()),
                                         fluidRow(tags$div(id = "box_tree_plot1")),
                                         splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     tags$div(id = "download_tree1"),
                                                     tags$div(id = "download_newick1"),
                                                     tags$div(id = "download_tree_seqs1"))
                                ),
                                tabPanel("Collapsable tree",
                                         fluidRow(tags$br()),
                                         div("This tab allows an interactive visualization of the previous tree. 
                                               In particular, in situations where gene duplications have given rise 
                                               to several clades and you want to reduce the tree not in relation to 
                                               the species that appear, but to these clades, this visualization allows 
                                               the collapse of subtrees and the simple exploration of the areas of interest. 
                                               Press Show Collapsable Tree to show the tree."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("phylo_start1", "Show Collapsable Tree",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "primary"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_phylo1"),
                                         fluidRow(tags$br())
                                ),
                                tabPanel("Expansion/Contraction",
                                         fluidRow(tags$br()),
                                         div("Click the Show Evolutionary History button to calculate the species tree showing 
                                               the reconstruction of ancestral states of the orthogroup, i.e. the number of genes 
                                               calculated for each common ancestor of each species. In case there has been no significant 
                                               variation in the size of the orthogroup or the variability is so high between species that
                                               the sizes cannot be reliably calculated, the tree will not be plotted. For orthogroups that
                                               have undergone significant expansions or contractions along any branch, these are marked 
                                               in red and blue on the tree, respectively. In turn, the loss or non-existence of orthogroup
                                               genes in the different clades is represented in gray."),
                                         fluidRow(br()),
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
                                         div("Next, click on the Show Gene Selection for Pfam button, select the desired proteins
                                               from the tree and click on the Show Pfam Domains button to determine their PFAM domains.
                                               A table with the domains of each protein and their positions will be displayed, as well 
                                               as a plot showing the same information. Links to each domain's data are provided.
                                               Warning: this process may take a long time in the case of selecting many proteins."),
                                         fluidRow(br()),
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
                                         fluidRow(br()),
                                         div("Press the Show Gene Selection for MSA button to enable the selection of genes 
                                           from the tree to be aligned. Two alignment methods are supported: ClustalOmega uses this 
                                           algorithm to perform a de novo multiple sequence alignment of the selected proteins, while 
                                           the second option filters the precalculated MSA of the entire orthogroup (computed using the 
                                           software MAFFT) to retain 
                                           only the selected sequences, removing columns containing only gaps in the chosen subset. Thus,
                                           this second option retains the evolutionary framework to which the entire orthogroup is ascribed, 
                                           while the first aligns only the chosen sequences. The resulting alignment can be explored interactively,
                                           including searching for patterns USING Selection -> Find Motifs and different cholor schemes can be applied 
                                           based on different properties of amino acids. A consensus sequence is shown to summarize the alignment. For 
                                           a graphical representation with the different amino acids colored according to their chemical nature, click 
                                           on the Download Colored MSA button. The aligned sequences can also be download as a standard FASTA file.
                                           Warning: This process (specially the colored download) may take a long time in the case of selecting many proteins."),
                                         fluidRow(tags$br()),
                                         shinyWidgets::actionBttn("msa_start1", "Show Gene Selection for MSA",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "primary"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_msa1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "msa_method1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "msa_selectionI1"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.msa1',h3('Please be patient, aligning sequences ...'))),
                                         uiOutput(outputId = "error_msa1"),
                                         tags$div(id = "box_msa1"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "msa_down_fasta1"),
                                                     tags$div(id = "msa_down_plot1"))
                                ),
                                tabPanel("GO Terms", 
                                         fluidRow(tags$br()),
                                         div("Click on the button to select the genes of interest from the tree and select 
                                               the ontology type to display the GO terms associated with those genes. After
                                               pressing the Show GO terms button, the results are displayed in tabular form 
                                               and are accompanied by a GO association plot (each node is a GO and an edge 
                                               is drawn between two nodes if a gene from the set share both terms) and a treeplot
                                               showing the hierarchy of the identified terms and their summary. Links to each 
                                               individual GO term are avalaible from table and all results can be downloaded in
                                               standard PNG and TSV files."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("go_start1", "Show Gene Selection for GO Annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "primary"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_gos1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_gos_mode1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "gos_selection1"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.go1',h3('Please be patient, preparing results ...'))),
                                         uiOutput(outputId = "error_gos1"),
                                         tags$div(id = "box_gos_table1"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "box_gos_plot1"), tags$div(id = "box_gos_treeplot1")),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     tags$div(id = "download_ui_for_gos_table1"),
                                                     tags$div(id = "gos_down_button1"),
                                                     tags$div(id = "tree_gos_down_button1"))
                                         
                                         
                                ),
                                tabPanel("KEGG Orthology",
                                         fluidRow(tags$br()),
                                         div("Click on the button to select the genes of interest from the tree and press
                                               the Show KEGG information button to show the results. These include a table 
                                               showing the KEGG Orthology IDs, indicating how many and which of the selected 
                                               proteins correspond to each ID. In addition, the application performs an enrichment 
                                               in KEGG pathways from the identified KOs and allows for the plotting of these 
                                               pathways with the genes mapped onto them, using a selector to choose the pathway
                                               in case several enriched ones exist. Warning: this process may take a long time 
                                               in the case of selecting many proteins. Attempts to plot large pathways will
                                               produce an error, only more specific ones are supported i.e. Carbon metabolism 
                                               will return an error while Biotin metabolism will not."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("kegg_start1", "Show Gene Selection for KEGG Annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "primary"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_kos1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "kos_selection1"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.ko1',h3('Please be patient, exploring pathways ...'))),
                                         uiOutput(outputId = "error_kos1"),
                                         tags$div(id = "box_kos_table1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_kegg_table1"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"), 
                                                     tags$div(id = "download_ui_for_kos_table1"),
                                                     tags$div(id = "download_ui_for_kegg_table1")),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_paths1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "paths_button1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_path_image1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "path_download_ui1")
                                         
                                ),
                                tabPanel("STRING Interactions",
                                         fluidRow(tags$br()),
                                         div("This tab presents the results of interactions of the different proteins 
                                               for the species supported by the STRING database. After clicking the button,
                                               the proteins of the species included in the database are displayed. Once the
                                               desired selection has been made, press the Show String Interactions
                                               button and a table will be displayed
                                               indicating in each row an interaction and the type of evidence for its existence,
                                               either experimental (direct) or ortholog-based (interolog). In addition, a count
                                               of the orthogroups to which the target proteins belong is computed, presented in
                                               tabular form and by means of a pie chart, to assess preferential interaction of
                                               the orthogroup under study with other specific orthogroups. Finally, for
                                               specific proteins, an
                                               interaction network can be generated with the proteins that have the most reliable
                                               interactions with the selected protein. In case more than one protein is selected,
                                               all of them will appear in the network, which will contain their interactions to 
                                               determine pathways between them. The network is shown as an image and a button
                                               that creates the network is provided on the STRING page, where an interactive 
                                               network with additional protein structure information and SMART domains appears.
                                               Species currently supported by STRING: Aegilops, Arabidopsis, Bathycoccus, 
                                               Chara, Chlamydomonas, Coccomyxa, Cyanidioschyzon, Galdieria, Gracilariopsis,
                                               Guillardia, Klebsormidium, Micromonas, Oryza, Ostreococcus, Phaeodactylum,
                                               Physcomitrium, Raphidocelis, Scenedesmus, Selaginella, Solanum, Sorghum,
                                               Triticum and Volvox. Warning: the selection of many proteins in the first
                                               step can lead to particularly high waiting times."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("string_start1", "Show Gene Selection for STRING annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "primary"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_string1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "string_selection1"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.string1',h3('Please be patient, retrieving interactions ...'))),
                                         uiOutput(outputId = "error_string1"),
                                         tags$div(id = "box_st_table1"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "box_count_table1"), tags$div(id = "box_count_plot1")),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     tags$div(id = "download_ui_for_st_table1"),
                                                     tags$div(id = "download_ui_for_count_table1"),
                                                     tags$div(id = "count_down_button1")),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_network1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "network_button1"),
                                         fluidRow(tags$br()),
                                         uiOutput(outputId = "error_network1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_output_network1")
                                ),
                                tabPanel("Literature Annotation", 
                                         fluidRow(tags$br()),
                                         div("In the text box, type the search term to query your bibliographic information,
                                               i.e., CCA1. Then select one of the 4 search modes: Normal returns the entities 
                                               containing the term, Exact returns those that are identical, Alias returns all 
                                               aliases associated with the term and Substring returns all those containing a 
                                               given string. Associations found in the literature are returned in tabular form,
                                               with links to the papers from which the information was extracted. This
                                               functionality is based on PlantConnectome, to extend this analysis, it is recommended
                                               to go to https://connectome.plant.tools/."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("lit_start1", "Show Search Box for Literature Annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "primary"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "query_lit1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_lit1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "lit_selection1"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.lit1',h3('Please be patient, browsing literature ...'))),
                                         uiOutput(outputId = "error_lit1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_lit_table1"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "download_ui_for_lit_table1")
                                         
                                )
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
                  Please take care of selecting the organism whose gene ID is being inputted. Organisms in 
                  green belong to Viridiplantae, while other colors indicate groups outside this clade.", br(), br(),
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
                fluidRow(br()),
                
                span(tags$b("Tree building method"), style = "color:#25d04a; font-size: 20px; "),
                div(br()),
                
                div("The first step in any study using PharaohFUN is the construction of the 
                    gene tree corresponding to the selected orthogroup. For this, four standard methods 
                    are offered, select one before continuing. Default is performed with FastTree using Orthofinder 2
                    pipeline, which renders trees by aproximate maximum-likelihood and then reconciliate
                    gene trees with the species tree shown in the Home tab. For the other three methods,
                    no reconciliation is performed but support for each branch is shown as bootstrap 
                    values."),
                
                shinyWidgets::awesomeRadio(
                  inputId = "build_trees_2",
                  label = "", 
                  choices = c("Maximum Likelihood", "Neighbour Joining", "UPGMA", "Parsimony"),
                  selected = "Maximum Likelihood",
                  inline = TRUE, 
                  status = "success"
                ),
                
                fluidRow(br()),
                
                span(tags$b("Insert a protein chain"), style = "color:#25d04a; font-size: 20px; "),
                br(),
                br(),
                div("Below you can paste the sequence of the protein whose evolutionary history you
                             wish to analyze. Then use the selection bar to choose the organism which the pasted sequence
                             belongs to. After pasting it, select a model: Global or Viridiplantae (default
                             is Viridiplantae). Note that the groups TSAR
                             and Cryptophytes, Rhodophytes and Glaucophytes do not belong to Viridiplantae, so they
                             will be ignored in case this is the selected model."),
                fluidRow(
                  
                  column(8, 
                         textAreaInput(inputId = "geneInt2", 
                                       "", 
                                       width="200%", height = "200px", 
                                       value= "", resize = "vertical",
                                       placeholder = 
                                         "METNSSGEDLVIKTRKPYTITKQRERWTEEEHNRFIEALRLYGRAWQKIEEHVATKTAVQ...")),
                  column(4,
                         
                         fluidRow(
                           
                           column(9,
                                  div(style = "margin-top: 25px;",
                                      shinyWidgets::pickerInput("organism_input_2","Choose organism",
                                                                choices=names(organisms_values),
                                                                multiple = F, selected=names(organisms_values)[11])
                                  )
                                  
                                  
                           )),
                         
                         fluidRow(
                           column(3, div( style = "margin-top: 85px;", 
                                          shinyWidgets::actionBttn("run_button2", "Run", size = "sm", icon = icon("magnifying-glass"),
                                                                   style = "float", color = "success"))),
                           
                           column(9,div(style = "margin-top: 89px;",
                                        shinyWidgets::materialSwitch(inputId = "switch2", label = "Global", 
                                                                     value = T, status = "success", inline = TRUE),
                                        span("Viridiplantae"))
                                  
                           )),
                         
                  )
                  
                )),
              
              br(),
              fluidRow(
                box(status = "success", width = 12, 
                    title = span(tags$b("Results"), style = "color:#25d04a; font-size: 20px; "),
                    "Results for the query gene and species are displayed below, arranged in 
                      different tabs. The execution of each analysis is initiated from the start 
                      button within each of the tabs, with specific instructions for each analysis. First, go to Gene Tree tab,
                      which shows best matches for query sequence and a tree containing the genes corresponding to the same 
                      orthogroup as the query in the
                      selected species. The rest of the tabs allow for a deeper exploration of these genes. Do not start any 
                      analysis before creating gene tree.",
                    div(br()),
                    tabsetPanel(type = "tabs",
                                tabPanel("Gene Tree", 
                                         fluidRow(br()),
                                         div("This tab shows four different results. First of all, a table with up to 5 best 
                                         matches for the query sequence in the chosen proteome, with decreasing confidence. First row
                                         corresponds to best match, which is used to perform subsequent analysis. If you are interested
                                         in another one, please copy de ID and paste it in Gene ID-based search tab. Secondly, a complete list of the genes
                                         that are assigned to the same orthogroup as the query. Next, the proportion of  genes of each species. 
                                         Finally, a gene tree show the evolutionary relationships between those genes. All results can be downloaded 
                                         using the buttons at the bottom of the page."),
                                         fluidRow(br()),
                                         
                                         shinyjs::useShinyjs(),
                                         shinyjs::hidden(div(id='loading.tree2',h3('Please be patient, building tree ...'))),
                                         uiOutput(outputId = "error_tree2"),
                                         fluidRow(tags$div(id = "box_tree_seq_table2")),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "box_tree_text2"), tags$div(id = "box_tree_pie2")),
                                         fluidRow(tags$br()),
                                         fluidRow(tags$div(id = "box_tree_plot2")),
                                         splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     tags$div(id = "download_tree2"),
                                                     tags$div(id = "download_newick2"),
                                                     tags$div(id = "download_tree_seqs2"))
                                ),
                                tabPanel("Collapsable tree",
                                         fluidRow(tags$br()),
                                         div("This tab allows for an interactive visualization of the previous tree. 
                                               In particular, in situations where gene duplications have given rise 
                                               to several clades and you want to reduce the tree not in relation to 
                                               the species that appear, but to these clades, this visualization allows 
                                               the collapse of subtrees and the simple exploration of the areas of interest. 
                                               Press Show Collapsable Tree to show the tree."),
                                         fluidRow(tags$br()),
                                         shinyWidgets::actionBttn("phylo_start2", "Show Collapsable Tree",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "success"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_phylo2"),
                                         fluidRow(tags$br())
                                ),
                                tabPanel("Expansion/Contraction",
                                         fluidRow(tags$br()),
                                         div("Click the Show Evolutionary History button to calculate the species tree showing 
                                               the reconstruction of ancestral states of the orthogroup, i.e. the number of genes 
                                               calculated for each common ancestor of each species. In case there has been no significant 
                                               variation in the size of the orthogroup or the variability is so high between species that
                                               the sizes cannot be reliably calculated, the tree will not be plotted. For orthogroups that
                                               have undergone significant expansions or contractions along any branch, these are marked 
                                               in red and blue on the tree, respectively. In turn, the loss or non-existence of orthogroup
                                               genes in the different clades is represented in gray."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("cafe_start2", "Show Evolutionary History",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "success"),
                                         fluidRow(tags$br()),
                                         
                                         shinyjs::hidden(div(id='loading.cafe2', h3('Please be patient, reconstructing expansion/contraction events ...'))),
                                         tags$div(id = "error_cafe2"),
                                         tags$div(id = "box_mrca2"),
                                         tags$br(),
                                         tags$div(id = "box_cafe2"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "cafe_down_button2"),
                                                     tags$div(id = "download_ui_for_cafe_plot2"))
                                ),
                                tabPanel("PFAM Domains", 
                                         fluidRow(tags$br()),
                                         div("Next, click on the Show Gene Selection for Pfam button, select the desired proteins
                                               from the tree and click on the Show Pfam Domains button to determine their PFAM domains.
                                               A table with the domains of each protein and their positions will be displayed, as well 
                                               as a plot showing the same information. Links to each domain's data are provided.
                                               Warning: this process may take a long time in the case of selecting many proteins."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("pfam_start2", "Show Gene Selection for Pfam",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "success"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_pfams2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "pfam_selectionI2"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.pfam.pf2',h3('Please be patient, identifying domains ...'))),
                                         uiOutput(outputId = "error_pfam2"),
                                         tags$div(id = "box_pfam2"),
                                         tags$br(),
                                         tags$div(id = "box_pfplot2"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"), 
                                                     tags$div(id = "pfam_down_button2"),
                                                     tags$div(id = "download_ui_for_pfam_table2")
                                         )
                                ),
                                tabPanel("Multiple Sequence Alignment",
                                         fluidRow(tags$br()),
                                         div("Press the Show Gene Selection for MSA button to enable the selection of genes 
                                           from the tree to be aligned. Two alignment methods are supported: ClustalOmega uses this 
                                           algorithm to perform a de novo multiple sequence alignment of the selected proteins, while 
                                           the second option filters the precalculated MSA of the entire orthogroup (computed using the 
                                           software MAFFT) to retain 
                                           only the selected sequences, removing columns containing only gaps in the chosen subset. Thus,
                                           this second option retains the evolutionary framework to which the entire orthogroup is ascribed, 
                                           while the first aligns only the chosen sequences. The resulting alignment can be explored interactively,
                                           including searching for patterns USING Selection -> Find Motifs and different cholor schemes can be applied 
                                           based on different properties of amino acids. A consensus sequence is shown to summarize the alignment. For 
                                           a graphical representation with the different amino acids colored according to their chemical nature, click 
                                           on the Download Colored MSA button. The aligned sequences can also be download as a standard FASTA file.
                                           Warning: This process (specially the colored download) may take a long time in the case of selecting many proteins."),
                                         fluidRow(tags$br()),
                                         shinyWidgets::actionBttn("msa_start2", "Show Gene Selection for MSA",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "success"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_msa2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "msa_method2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "msa_selectionI2"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.msa2',h3('Please be patient, aligning sequences ...'))),
                                         uiOutput(outputId = "error_msa2"),
                                         tags$div(id = "box_msa2"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "msa_down_fasta2"),
                                                     tags$div(id = "msa_down_plot2"))
                                ),
                                tabPanel("GO Terms", 
                                         fluidRow(tags$br()),
                                         div("Click on the button to select the genes of interest from the tree and select 
                                               the ontology type to display the GO terms associated with those genes. After
                                               pressing the Show GO terms button, the results are displayed in tabular form 
                                               and are accompanied by a GO association plot (each node is a GO and an edge 
                                               is drawn between two nodes if a gene from the set share both terms) and a treeplot
                                               showing the hierarchy of the identified terms and their summary. Links to each 
                                               individual GO term are avalaible from table and all results can be downloaded in
                                               standard PNG and TSV files."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("go_start2", "Show Gene Selection for GO Annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "success"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_gos2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_gos_mode2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "gos_selection2"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.go2',h3('Please be patient, preparing results ...'))),
                                         uiOutput(outputId = "error_gos2"),
                                         tags$div(id = "box_gos_table2"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "box_gos_plot2"), tags$div(id = "box_gos_treeplot2")),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     tags$div(id = "download_ui_for_gos_table2"),
                                                     tags$div(id = "gos_down_button2"),
                                                     tags$div(id = "tree_gos_down_button2"))
                                         
                                         
                                ),
                                tabPanel("KEGG Orthology",
                                         fluidRow(tags$br()),
                                         div("Click on the button to select the genes of interest from the tree and press
                                               the Show KEGG information button to show the results. These include a table 
                                               showing the KEGG Orthology IDs, indicating how many and which of the selected 
                                               proteins correspond to each ID. In addition, the application performs an enrichment 
                                               in KEGG pathways from the identified KOs and allows for the plotting of these 
                                               pathways with the genes mapped onto them, using a selector to choose the pathway
                                               in case several enriched ones exist. Warning: this process may take a long time 
                                               in the case of selecting many proteins. Attempts to plot large pathways will
                                               produce an error, only more specific ones are supported i.e. Carbon metabolism 
                                               will return an error while Biotin metabolism will not."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("kegg_start2", "Show Gene Selection for KEGG Annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "success"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_kos2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "kos_selection2"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.ko2',h3('Please be patient, exploring pathways ...'))),
                                         uiOutput(outputId = "error_kos2"),
                                         tags$div(id = "box_kos_table2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_kegg_table2"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"), 
                                                     tags$div(id = "download_ui_for_kos_table2"),
                                                     tags$div(id = "download_ui_for_kegg_table2")),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_paths2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "paths_button2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_path_image2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "path_download_ui2")
                                         
                                ),
                                tabPanel("STRING Interactions",
                                         fluidRow(tags$br()),
                                         div("This tab presents the results of interactions of the different proteins 
                                               for the species supported by the STRING database. After clicking the button,
                                               the proteins of the species included in the database are displayed. Once the
                                               desired selection has been made, press the Show String Interactions
                                               button and a table will be displayed
                                               indicating in each row an interaction and the type of evidence for its existence,
                                               either experimental (direct) or ortholog-based (interolog). In addition, a count
                                               of the orthogroups to which the target proteins belong is computed, presented in
                                               tabular form and by means of a pie chart, to assess preferential interaction of
                                               the orthogroup under study with other specific orthogroups. Finally, for
                                               specific proteins, an
                                               interaction network can be generated with the proteins that have the most reliable
                                               interactions with the selected protein. In case more than one protein is selected,
                                               all of them will appear in the network, which will contain their interactions to 
                                               determine pathways between them. The network is shown as an image and a button
                                               that creates the network is provided on the STRING page, where an interactive 
                                               network with additional protein structure information and SMART domains appears.
                                               Species currently supported by STRING: Aegilops, Arabidopsis, Bathycoccus, 
                                               Chara, Chlamydomonas, Coccomyxa, Cyanidioschyzon, Galdieria, Gracilariopsis,
                                               Guillardia, Klebsormidium, Micromonas, Oryza, Ostreococcus, Phaeodactylum,
                                               Physcomitrium, Raphidocelis, Scenedesmus, Selaginella, Solanum, Sorghum,
                                               Triticum and Volvox. Warning: the selection of many proteins in the first
                                               step can lead to particularly high waiting times."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("string_start2", "Show Gene Selection for STRING annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "success"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_string2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "string_selection2"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.string2',h3('Please be patient, retrieving interactions ...'))),
                                         uiOutput(outputId = "error_string2"),
                                         tags$div(id = "box_st_table2"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "box_count_table2"), tags$div(id = "box_count_plot2")),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     tags$div(id = "download_ui_for_st_table2"),
                                                     tags$div(id = "download_ui_for_count_table2"),
                                                     tags$div(id = "count_down_button2")),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_network2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "network_button2"),
                                         fluidRow(tags$br()),
                                         uiOutput(outputId = "error_network2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_output_network2")
                                         
                                ),
                                tabPanel("Literature Annotation", 
                                         fluidRow(tags$br()),
                                         div("In the text box, type the search term to query your bibliographic information,
                                               i.e., CCA1. Then select one of the 4 search modes: Normal returns the entities 
                                               containing the term, Exact returns those that are identical, Alias returns all 
                                               aliases associated with the term and Substring returns all those containing a 
                                               given string. Associations found in the literature are returned in tabular form,
                                               with links to the papers from which the information was extracted. This
                                               functionality is based on PlantConnectome, to extend this analysis, it is recommended
                                               to go to https://connectome.plant.tools/."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("lit_start2", "Show Search Box for Literature Annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "success"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "query_lit2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_lit2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "lit_selection2"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.lit2',h3('Please be patient, browsing literature ...'))),
                                         uiOutput(outputId = "error_lit2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_lit_table2"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "download_ui_for_lit_table2")
                                         
                                )
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
                  Please take care of selecting organisms that are present in the selected orthogroup. Organisms in 
                  green belong to Viridiplantae, while other colors indicate groups outside this clade.", br(), br(),
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
                
                fluidRow(br()),
                fluidRow(br()),
                
                span(tags$b("Tree building method"), style = "color:#d5251d; font-size: 20px; "),
                div(br()),
                
                div("The first step in any study using PharaohFUN is the construction of the 
                    gene tree corresponding to the selected orthogroup. For this, four standard methods 
                    are offered, select one before continuing. Default is performed with FastTree using Orthofinder 2
                    pipeline, which renders trees by aproximate maximum-likelihood and then reconciliate
                    gene trees with the species tree shown in the Home tab. For the other three methods,
                    no reconciliation is performed but support for each branch is shown as bootstrap 
                    values."),
                
                shinyWidgets::awesomeRadio(
                  inputId = "build_trees_3",
                  label = "", 
                  choices = c("Maximum Likelihood", "Neighbour Joining", "UPGMA", "Parsimony"),
                  selected = "Maximum Likelihood",
                  inline = TRUE, 
                  status = "danger"
                ),
                
                fluidRow(br()),
                
                span(tags$b("Insert Orthogroup ID"), style = "color:#d5251d; font-size: 20px; "),
                div(br()),
                
                div("Below you can write the ID corresponding to the desired orthogroup. It is specially intended
                             to study STRING's results from previous queries. Then, select a model: Global 
                             or Viridiplantae (default is Viridiplantae). Note that the groups TSAR
                             and Cryptophytes, Rhodophytes and Glaucophytes do not belong to Viridiplantae, so they
                             will be ignored in case this is the selected model."),
                
                
                fluidRow(
                  column(5, textInput("geneInt3", label = "", width = "100%", placeholder = "OG0001709")),
                  
                  column(1, div( style = "margin-top: 20px;", 
                                 shinyWidgets::actionBttn("run_button3", "Run", size = "sm", icon = icon("magnifying-glass"),
                                                          style = "float", color = "danger"))),
                  column(3,div(style = "margin-top: 24px;",
                               shinyWidgets::materialSwitch(inputId = "switch3", label = "Global", 
                                                            value = T, status = "danger", inline = TRUE),
                               span("Viridiplantae")))
                  
                  
                  
                )),
              
              br(),
              fluidRow(
                box(status = "danger", width = 12,
                    title = span(tags$b("Results"), style = "color:#d5251d; font-size: 20px; "),
                    "Results for the query orthogroup and species are displayed below, arranged in 
                      different tabs. The execution of each analysis is initiated from the start 
                      button within each of the tabs, with specific instructions for each analysis. First, go to Gene Tree tab,
                      which shows a tree containing the genes corresponding to the orthogroup in the selected species. 
                      The rest of the tabs allow for a deeper exploration of these genes. Do not start any analysis before 
                    creating gene tree.",
                    div(br()),
                    tabsetPanel(type = "tabs",
                                tabPanel("Gene Tree",
                                         fluidRow(br()),
                                         div("This tab shows three different results. First of all, a complete list of the genes
                                               that are assigned to the query orthogroup. Secondly, the proportion of
                                               genes of each species. Finally, a gene tree show the evolutionary relationships 
                                               between those genes. All results can be downloaded using the buttons at the bottom
                                               of the page."),
                                         fluidRow(br()),
                                         shinyjs::useShinyjs(),
                                         shinyjs::hidden(div(id='loading.tree3',h3('Please be patient, building tree ...'))),
                                         uiOutput(outputId = "error_tree3"),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "box_tree_text3"), tags$div(id = "box_tree_pie3")),
                                         fluidRow(tags$br()),
                                         fluidRow(tags$div(id = "box_tree_plot3")),
                                         splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     tags$div(id = "download_tree3"),
                                                     tags$div(id = "download_newick3"),
                                                     tags$div(id = "download_tree_seqs3"))
                                ),
                                tabPanel("Collapsable tree",
                                         fluidRow(tags$br()),
                                         div("This tab allows an interactive visualization of the previous tree. 
                                               In particular, in situations where gene duplications have given rise 
                                               to several clades and you want to reduce the tree not in relation to 
                                               the species that appear, but to these clades, this visualization allows 
                                               the collapse of subtrees and the simple exploration of the areas of interest. 
                                               Press Show Collapsable Tree to show the tree."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("phylo_start3", "Show Collapsable Tree",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "danger"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_phylo3"),
                                         fluidRow(tags$br())
                                ),
                                tabPanel("Expansion/Contraction",
                                         fluidRow(tags$br()),
                                         div("Click the Show Evolutionary History button to calculate the species tree showing 
                                               the reconstruction of ancestral states of the orthogroup, i.e. the number of genes 
                                               calculated for each common ancestor of each species. In case there has been no significant 
                                               variation in the size of the orthogroup or the variability is so high between species that
                                               the sizes cannot be reliably calculated, the tree will not be plotted. For orthogroups that
                                               have undergone significant expansions or contractions along any branch, these are marked 
                                               in red and blue on the tree, respectively. In turn, the loss or non-existence of orthogroup
                                               genes in the different clades is represented in gray."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("cafe_start3", "Show Evolutionary History",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "danger"),
                                         fluidRow(tags$br()),
                                         
                                         shinyjs::hidden(div(id='loading.cafe3', h3('Please be patient, reconstructing expansion/contraction events ...'))),
                                         tags$div(id = "error_cafe3"),
                                         tags$div(id = "box_mrca3"),
                                         tags$br(),
                                         tags$div(id = "box_cafe3"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "cafe_down_button3"),
                                                     tags$div(id = "download_ui_for_cafe_plot3"))
                                ),
                                tabPanel("PFAM Domains", 
                                         fluidRow(tags$br()),
                                         div("Next, click on the Show Gene Selection for Pfam button, select the desired proteins
                                               from the tree and click on the Show Pfam Domains button to determine their PFAM domains.
                                               A table with the domains of each protein and their positions will be displayed, as well 
                                               as a plot showing the same information. Links to each domain's data are provided.
                                               Warning: this process may take a long time in the case of selecting many proteins."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("pfam_start3", "Show Gene Selection for Pfam",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "danger"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_pfams3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "pfam_selectionI3"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.pfam.pf3',h3('Please be patient, identifying domains ...'))),
                                         uiOutput(outputId = "error_pfam3"),
                                         tags$div(id = "box_pfam3"),
                                         tags$br(),
                                         tags$div(id = "box_pfplot3"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"), 
                                                     tags$div(id = "pfam_down_button3"),
                                                     tags$div(id = "download_ui_for_pfam_table3")
                                         )
                                ),
                                tabPanel("Multiple Sequence Alignment",
                                         fluidRow(tags$br()),
                                         div("Press the Show Gene Selection for MSA button to enable the selection of genes 
                                           from the tree to be aligned. Two alignment methods are supported: ClustalOmega uses this 
                                           algorithm to perform a de novo multiple sequence alignment of the selected proteins, while 
                                           the second option filters the precalculated MSA of the entire orthogroup (computed using the 
                                           software MAFFT) to retain 
                                           only the selected sequences, removing columns containing only gaps in the chosen subset. Thus,
                                           this second option retains the evolutionary framework to which the entire orthogroup is ascribed, 
                                           while the first aligns only the chosen sequences. The resulting alignment can be explored interactively,
                                           including searching for patterns USING Selection -> Find Motifs and different cholor schemes can be applied 
                                           based on different properties of amino acids. A consensus sequence is shown to summarize the alignment. For 
                                           a graphical representation with the different amino acids colored according to their chemical nature, click 
                                           on the Download Colored MSA button. The aligned sequences can also be download as a standard FASTA file.
                                           Warning: This process (specially the colored download) may take a long time in the case of selecting many proteins."),
                                         fluidRow(tags$br()),
                                         shinyWidgets::actionBttn("msa_start3", "Show Gene Selection for MSA",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "danger"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_msa3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "msa_method3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "msa_selectionI3"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.msa3',h3('Please be patient, aligning sequences ...'))),
                                         uiOutput(outputId = "error_msa3"),
                                         tags$div(id = "box_msa3"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "msa_down_fasta3"),
                                                     tags$div(id = "msa_down_plot3"))
                                ),
                                tabPanel("GO Terms", 
                                         fluidRow(tags$br()),
                                         div("Click on the button to select the genes of interest from the tree and select 
                                               the ontology type to display the GO terms associated with those genes. After
                                               pressing the Show GO terms button, the results are displayed in tabular form 
                                               and are accompanied by a GO association plot (each node is a GO and an edge 
                                               is drawn between two nodes if a gene from the set share both terms) and a treeplot
                                               showing the hierarchy of the identified terms and their summary. Links to each 
                                               individual GO term are avalaible from table and all results can be downloaded in
                                               standard PNG and TSV files."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("go_start3", "Show Gene Selection for GO Annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "danger"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_gos3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_gos_mode3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "gos_selection3"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.go3',h3('Please be patient, preparing results ...'))),
                                         uiOutput(outputId = "error_gos3"),
                                         tags$div(id = "box_gos_table3"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "box_gos_plot3"), tags$div(id = "box_gos_treeplot3")),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     tags$div(id = "download_ui_for_gos_table3"),
                                                     tags$div(id = "gos_down_button3"),
                                                     tags$div(id = "tree_gos_down_button3"))
                                         
                                         
                                ),
                                tabPanel("KEGG Orthology",
                                         fluidRow(tags$br()),
                                         div("Click on the button to select the genes of interest from the tree and press
                                               the Show KEGG information button to show the results. These include a table 
                                               showing the KEGG Orthology IDs, indicating how many and which of the selected 
                                               proteins correspond to each ID. In addition, the application performs an enrichment 
                                               in KEGG pathways from the identified KOs and allows for the plotting of these 
                                               pathways with the genes mapped onto them, using a selector to choose the pathway
                                               in case several enriched ones exist. Warning: this process may take a long time 
                                               in the case of selecting many proteins. Attempts to plot large pathways will
                                               produce an error, only more specific ones are supported i.e. Carbon metabolism 
                                               will return an error while Biotin metabolism will not."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("kegg_start3", "Show Gene Selection for KEGG Annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "danger"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_kos3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "kos_selection3"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.ko3',h3('Please be patient, exploring pathways ...'))),
                                         uiOutput(outputId = "error_kos3"),
                                         tags$div(id = "box_kos_table3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_kegg_table3"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"), 
                                                     tags$div(id = "download_ui_for_kos_table3"),
                                                     tags$div(id = "download_ui_for_kegg_table3")),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_paths3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "paths_button3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_path_image3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "path_download_ui3")
                                         
                                ),
                                tabPanel("STRING Interactions",
                                         fluidRow(tags$br()),
                                         div("This tab presents the results of interactions of the different proteins 
                                               for the species supported by the STRING database. After clicking the button,
                                               the proteins of the species included in the database are displayed. Once the
                                               desired selection has been made, press the Show String Interactions
                                               button and a table will be displayed
                                               indicating in each row an interaction and the type of evidence for its existence,
                                               either experimental (direct) or ortholog-based (interolog). In addition, a count
                                               of the orthogroups to which the target proteins belong is computed, presented in
                                               tabular form and by means of a pie chart, to assess preferential interaction of
                                               the orthogroup under study with other specific orthogroups. Finally, for
                                               specific proteins, an
                                               interaction network can be generated with the proteins that have the most reliable
                                               interactions with the selected protein. In case more than one protein is selected,
                                               all of them will appear in the network, which will contain their interactions to 
                                               determine pathways between them. The network is shown as an image and a button
                                               that creates the network is provided on the STRING page, where an interactive 
                                               network with additional protein structure information and SMART domains appears.
                                               Species currently supported by STRING: Aegilops, Arabidopsis, Bathycoccus, 
                                               Chara, Chlamydomonas, Coccomyxa, Cyanidioschyzon, Galdieria, Gracilariopsis,
                                               Guillardia, Klebsormidium, Micromonas, Oryza, Ostreococcus, Phaeodactylum,
                                               Physcomitrium, Raphidocelis, Scenedesmus, Selaginella, Solanum, Sorghum,
                                               Triticum and Volvox. Warning: the selection of many proteins in the first
                                               step can lead to particularly high waiting times."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("string_start3", "Show Gene Selection for STRING annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "danger"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_string3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "string_selection3"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.string3',h3('Please be patient, retrieving interactions ...'))),
                                         uiOutput(outputId = "error_string3"),
                                         tags$div(id = "box_st_table3"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "box_count_table3"), tags$div(id = "box_count_plot3")),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     tags$div(id = "download_ui_for_st_table3"),
                                                     tags$div(id = "download_ui_for_count_table3"),
                                                     tags$div(id = "count_down_button3")),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_network3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "network_button3"),
                                         fluidRow(tags$br()),
                                         uiOutput(outputId = "error_network3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_output_network3")
                                         
                                ),
                                tabPanel("Literature Annotation", 
                                         fluidRow(tags$br()),
                                         div("In the text box, type the search term to query your bibliographic information,
                                               i.e., CCA1. Then select one of the 4 search modes: Normal returns the entities 
                                               containing the term, Exact returns those that are identical, Alias returns all 
                                               aliases associated with the term and Substring returns all those containing a 
                                               given string. Associations found in the literature are returned in tabular form,
                                               with links to the papers from which the information was extracted. This
                                               functionality is based on PlantConnectome, to extend this analysis, it is recommended
                                               to go to https://connectome.plant.tools/."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("lit_start3", "Show Search Box for Literature Annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "danger"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "query_lit3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_lit3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "lit_selection3"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.lit3',h3('Please be patient, browsing literature ...'))),
                                         uiOutput(outputId = "error_lit3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_lit_table3"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "download_ui_for_lit_table3")
                                         
                                )
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
                  Please take care of selecting the organism whose sequences or IDs are being used as input. The 
                selected organisms will be the only ones that appear in heatmap and gene tables. Organisms in 
                  green belong to Viridiplantae, while other colors indicate groups outside this clade.", br(), br(),
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
                
                fluidRow(br()),
                fluidRow(br()),
                
                span(tags$b("Choose Sequence File to Upload"), style = "color:#e37326; font-size: 20px; "),
                div(br()),
                
                div("Below you can upload a file with a list of gene IDs (with a single ID per line) or a FASTA file
                    containing custom identifiers for each sequence. These IDs or sequences must correspond to one
                    of the available organisms, and the correct species must be selected prior to analysis on the 
                    selection bar to the right of the Run button. Then, select a model: Global 
                             or Viridiplantae (default is Viridiplantae). Note that the groups TSAR
                             and Cryptophytes, Rhodophytes and Glaucophytes do not belong to Viridiplantae, so they
                             will be ignored in case this is the selected model. Finally, choose if the upload file
                    contains IDs or Sequences and click Run to begin computations."),
                fluidRow(br()),
                
                
                fluidRow(
                  column(4, fileInput(inputId = "geneInt4",label = "")),
                  column(1, div( style = "margin-top: 20px;", 
                                 shinyWidgets::actionBttn("run_button4", "Run", size = "sm", icon = icon("magnifying-glass"),
                                                          style = "float", color = "warning"))),
                  column(3, div(style = "margin-top: 0px;",
                                
                                shinyWidgets::pickerInput("organism_input_4","",
                                                          choices=names(organisms_values),
                                                          multiple = F, selected=names(organisms_values)[11]))
                         
                         
                  ),
                  column(2,div(style = "margin-top: 24px;",
                               shinyWidgets::materialSwitch(inputId = "switch4", label = "Global", 
                                                            value = T, status = "warning", inline = TRUE),
                               span("Viridiplantae"))),
                  
                  column(2,div(style = "margin-top: 24px;",
                               shinyWidgets::materialSwitch(inputId = "search_mode4", label = "IDs", 
                                                            value = T, status = "warning", inline = TRUE),
                               span("Sequences")))
                  
                  
                  
                )),
              
              br(),
              
              fluidRow(
                box(status = "warning", width = 12, 
                    title = span(tags$b("Results"), style = "color:#e37326; font-size: 20px; "),
                    "Results for the query sequences or IDs and species are displayed below. Using the download button, user can
                    access a compressed file with results arranged in different subfolders. First, a table in TSV format indicates
                    the mapping of each sequence against the proteome and its associated metrics (in case sequence sarch mode is 
                    selected). Each of the subfolders corresponds to a query sequence/ID and contains the gene tree of its 
                    orthogroup (without filtering by species), the MSA of the complete orthogroup, the expansion/contraction 
                    results, the KO and GO annotations for the genes from the selected species and a table with the genes
                    forming the orthogroup in the selected species. Additionally, a visual summary of the number of genes 
                    within each orthogroup relative to each of the sequences/IDs is presented in the form of a heatmap, 
                    which allows to determine if in any of the selected species some orthogroups have undergone expansions 
                    or if there are orthogroups that are not present in all species.",
                    div(br()),
                    fluidRow(br()),
                    shinyjs::useShinyjs(),
                    shinyjs::hidden(div(id='loading.batch4',h3('Please be patient, preparing your folders ...'))),
                    uiOutput(outputId = "error_batch4"),
                    tags$div(id = "download_batch4"),
                    fluidRow(tags$br()),
                    tags$div(id = "box_heatmap4")
                    
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
                organism to allow for better resolution of the created tree. Organisms in 
                  green belong to Viridiplantae, while other colors indicate groups outside this clade.", br(), br(),
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
                
                fluidRow(br()),
                fluidRow(br()),
                
                span(tags$b("Insert a protein chain"), style = "color:#5e3587; font-size: 20px; "),
                br(),
                br(),
                div("Below you can paste the sequence of the protein whose evolutionary history you
                             wish to analyze. After pasting it, select a model: Global or Viridiplantae (default
                             is Viridiplantae). Note that the groups TSAR
                             and Cryptophytes, Rhodophytes and Glaucophytes do not belong to Viridiplantae, so they
                             will be ignored in case this is the selected model."),
                
                
                fluidRow(
                  column(8, textAreaInput(inputId = "geneInt5", 
                                          "", 
                                          width="200%", height = "200px", 
                                          value= "", resize = "vertical",
                                          placeholder = 
                                            "METNSSGEDLVIKTRKPYTITKQRERWTEEEHNRFIEALRLYGRAWQKIEEHVATKTAVQ...")),
                  #column(1, div( style = "margin-top: 20px;", actionButton("run", "Run", icon = icon("magnifying-glass")))))
                  column(1, div( style = "margin-top: 185px;", 
                                 shinyWidgets::actionBttn("run_button5", "Run", size = "sm", icon = icon("magnifying-glass"),
                                                          style = "float", color = "royal"))),
                  column(3,div(style = "margin-top: 189px;",
                               shinyWidgets::materialSwitch(inputId = "switch5", label = "Global", 
                                                            value = T, status = "primary", inline = TRUE),
                               span("Viridiplantae")))
                  
                  
                  
                )),
              
              br(),
              fluidRow(
                box(status = "primary", width = 12,
                    title = span(tags$b("Results"), style = "color:#5e3587; font-size: 20px; "),
                    "Results for the query sequence and species are displayed below, arranged in 
                      different tabs. The execution of each analysis is initiated from the start 
                      button within each of the tabs, with specific instructions for each analysis. First, go to Gene Tree tab,
                      which shows a tree containing the genes corresponding to the same orthogroup as the query in the
                      selected species. The rest of the tabs allow for a deeper exploration of these genes. Do not start any 
                      analysis before creating gene tree.",
                    div(br()),
                    tabsetPanel(type = "tabs",
                                tabPanel("Gene Tree",
                                         fluidRow(br()),
                                         div("This tab shows three different results. First of all, a complete list of the genes
                                               that are assigned to the same orthogroup as the query. Secondly, the proportion of
                                               genes of each species. Finally, a gene tree show the evolutionary relationships 
                                               between those genes. Query sequence placement is highlighted in red. All results 
                                               can be downloaded using the buttons at the bottom
                                               of the page."),
                                         fluidRow(br()),
                                         shinyjs::useShinyjs(),
                                         shinyjs::hidden(div(id='loading.tree5',h3('Please be patient, building tree ...'))),
                                         uiOutput(outputId = "error_tree5"),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "box_tree_text5"), tags$div(id = "box_tree_pie5")),
                                         fluidRow(tags$br()),
                                         fluidRow(tags$div(id = "box_tree_plot5")),
                                         splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     tags$div(id = "download_tree5"),
                                                     tags$div(id = "download_newick5"),
                                                     tags$div(id = "download_tree_seqs5"))
                                ),
                                tabPanel("Collapsable tree",
                                         fluidRow(tags$br()),
                                         div("This tab allows an interactive visualization of the previous tree. 
                                               In particular, in situations where gene duplications have given rise 
                                               to several clades and you want to reduce the tree not in relation to 
                                               the species that appear, but to these clades, this visualization allows 
                                               the collapse of subtrees and the simple exploration of the areas of interest. 
                                               Press Show Collapsable Tree to show the tree."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("phylo_start5", "Show Collapsable Tree",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "royal"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_phylo5"),
                                         fluidRow(tags$br())
                                ),
                                tabPanel("Expansion/Contraction",
                                         fluidRow(tags$br()),
                                         div("Click the Show Evolutionary History button to calculate the species tree showing 
                                               the reconstruction of ancestral states of the orthogroup, i.e. the number of genes 
                                               calculated for each common ancestor of each species. In case there has been no significant 
                                               variation in the size of the orthogroup or the variability is so high between species that
                                               the sizes cannot be reliably calculated, the tree will not be plotted. For orthogroups that
                                               have undergone significant expansions or contractions along any branch, these are marked 
                                               in red and blue on the tree, respectively. In turn, the loss or non-existence of orthogroup
                                               genes in the different clades is represented in gray."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("cafe_start5", "Show Evolutionary History",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "royal"),
                                         fluidRow(tags$br()),
                                         
                                         shinyjs::hidden(div(id='loading.cafe5', h3('Please be patient, reconstructing expansion/contraction events ...'))),
                                         tags$div(id = "error_cafe5"),
                                         tags$div(id = "box_mrca5"),
                                         tags$br(),
                                         tags$div(id = "box_cafe5"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "cafe_down_button5"),
                                                     tags$div(id = "download_ui_for_cafe_plot5"))
                                ),
                                tabPanel("PFAM Domains", 
                                         fluidRow(tags$br()),
                                         div("Next, click on the Show Gene Selection for Pfam button, select the desired proteins
                                               from the tree and click on the Show Pfam Domains button to determine their PFAM domains.
                                               A table with the domains of each protein and their positions will be displayed, as well 
                                               as a plot showing the same information. Links to each domain's data are provided.
                                               Warning: this process may take a long time in the case of selecting many proteins."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("pfam_start5", "Show Gene Selection for Pfam",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "royal"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_pfams5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "pfam_selectionI5"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.pfam.pf5',h3('Please be patient, identifying domains ...'))),
                                         uiOutput(outputId = "error_pfam5"),
                                         tags$div(id = "box_pfam5"),
                                         tags$br(),
                                         tags$div(id = "box_pfplot5"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"), 
                                                     tags$div(id = "pfam_down_button5"),
                                                     tags$div(id = "download_ui_for_pfam_table5")
                                         )
                                ),
                                tabPanel("Multiple Sequence Alignment",
                                         fluidRow(tags$br()),
                                         div("Press the Show Gene Selection for MSA button to enable the selection of genes 
                                           from the tree to be aligned. Two alignment methods are supported: ClustalOmega uses this 
                                           algorithm to perform a de novo multiple sequence alignment of the selected proteins, while 
                                           the second option filters the precalculated MSA of the entire orthogroup (computed using the 
                                           software MAFFT) to retain 
                                           only the selected sequences, removing columns containing only gaps in the chosen subset. Thus,
                                           this second option retains the evolutionary framework to which the entire orthogroup is ascribed, 
                                           while the first aligns only the chosen sequences. The resulting alignment can be explored interactively,
                                           including searching for patterns USING Selection -> Find Motifs and different cholor schemes can be applied 
                                           based on different properties of amino acids. A consensus sequence is shown to summarize the alignment. For 
                                           a graphical representation with the different amino acids colored according to their chemical nature, click 
                                           on the Download Colored MSA button. The aligned sequences can also be download as a standard FASTA file.
                                           Warning: This process (specially the colored download) may take a long time in the case of selecting many proteins."),
                                         fluidRow(tags$br()),
                                         shinyWidgets::actionBttn("msa_start5", "Show Gene Selection for MSA",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "royal"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_msa5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "msa_method5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "msa_selectionI5"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.msa5',h3('Please be patient, aligning sequences ...'))),
                                         uiOutput(outputId = "error_msa5"),
                                         tags$div(id = "box_msa5"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "msa_down_fasta5"),
                                                     tags$div(id = "msa_down_plot5"))
                                ),
                                tabPanel("GO Terms", 
                                         fluidRow(tags$br()),
                                         div("Click on the button to select the genes of interest from the tree and select 
                                               the ontology type to display the GO terms associated with those genes. After
                                               pressing the Show GO terms button, the results are displayed in tabular form 
                                               and are accompanied by a GO association plot (each node is a GO and an edge 
                                               is drawn between two nodes if a gene from the set share both terms) and a treeplot
                                               showing the hierarchy of the identified terms and their summary. Links to each 
                                               individual GO term are avalaible from table and all results can be downloaded in
                                               standard PNG and TSV files."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("go_start5", "Show Gene Selection for GO Annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "royal"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_gos5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_gos_mode5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "gos_selection5"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.go5',h3('Please be patient, preparing results ...'))),
                                         uiOutput(outputId = "error_gos5"),
                                         tags$div(id = "box_gos_table5"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "box_gos_plot5"), tags$div(id = "box_gos_treeplot5")),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     tags$div(id = "download_ui_for_gos_table5"),
                                                     tags$div(id = "gos_down_button5"),
                                                     tags$div(id = "tree_gos_down_button5"))
                                         
                                         
                                ),
                                tabPanel("KEGG Orthology",
                                         fluidRow(tags$br()),
                                         div("Click on the button to select the genes of interest from the tree and press
                                               the Show KEGG information button to show the results. These include a table 
                                               showing the KEGG Orthology IDs, indicating how many and which of the selected 
                                               proteins correspond to each ID. In addition, the application performs an enrichment 
                                               in KEGG pathways from the identified KOs and allows for the plotting of these 
                                               pathways with the genes mapped onto them, using a selector to choose the pathway
                                               in case several enriched ones exist. Warning: this process may take a long time 
                                               in the case of selecting many proteins. Attempts to plot large pathways will
                                               produce an error, only more specific ones are supported i.e. Carbon metabolism 
                                               will return an error while Biotin metabolism will not."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("kegg_start5", "Show Gene Selection for KEGG Annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "royal"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_kos5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "kos_selection5"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.ko5',h3('Please be patient, exploring pathways ...'))),
                                         uiOutput(outputId = "error_kos5"),
                                         tags$div(id = "box_kos_table5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_kegg_table5"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"), 
                                                     tags$div(id = "download_ui_for_kos_table5"),
                                                     tags$div(id = "download_ui_for_kegg_table5")),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_paths5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "paths_button5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_path_image5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "path_download_ui5")
                                         
                                ),
                                tabPanel("STRING Interactions",
                                         fluidRow(tags$br()),
                                         div("This tab presents the results of interactions of the different proteins 
                                               for the species supported by the STRING database. After clicking the button,
                                               the proteins of the species included in the database are displayed. Once the
                                               desired selection has been made, press the Show String Interactions
                                               button and a table will be displayed
                                               indicating in each row an interaction and the type of evidence for its existence,
                                               either experimental (direct) or ortholog-based (interolog). In addition, a count
                                               of the orthogroups to which the target proteins belong is computed, presented in
                                               tabular form and by means of a pie chart, to assess preferential interaction of
                                               the orthogroup under study with other specific orthogroups. Finally, for
                                               specific proteins, an
                                               interaction network can be generated with the proteins that have the most reliable
                                               interactions with the selected protein. In case more than one protein is selected,
                                               all of them will appear in the network, which will contain their interactions to 
                                               determine pathways between them. The network is shown as an image and a button
                                               that creates the network is provided on the STRING page, where an interactive 
                                               network with additional protein structure information and SMART domains appears.
                                               Species currently supported by STRING: Aegilops, Arabidopsis, Bathycoccus, 
                                               Chara, Chlamydomonas, Coccomyxa, Cyanidioschyzon, Galdieria, Gracilariopsis,
                                               Guillardia, Klebsormidium, Micromonas, Oryza, Ostreococcus, Phaeodactylum,
                                               Physcomitrium, Raphidocelis, Scenedesmus, Selaginella, Solanum, Sorghum,
                                               Triticum and Volvox. Warning: the selection of many proteins in the first
                                               step can lead to particularly high waiting times."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("string_start5", "Show Gene Selection for STRING annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "royal"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_string5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "string_selection5"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.string5',h3('Please be patient, retrieving interactions ...'))),
                                         uiOutput(outputId = "error_string5"),
                                         tags$div(id = "box_st_table5"),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     tags$div(id = "box_count_table5"), tags$div(id = "box_count_plot5")),
                                         fluidRow(tags$br()),
                                         splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     tags$div(id = "download_ui_for_st_table5"),
                                                     tags$div(id = "download_ui_for_count_table5"),
                                                     tags$div(id = "count_down_button5")),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_network5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "network_button5"),
                                         fluidRow(tags$br()),
                                         uiOutput(outputId = "error_network5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_output_network5")
                                         
                                ),
                                tabPanel("Literature Annotation", 
                                         fluidRow(tags$br()),
                                         div("In the text box, type the search term to query your bibliographic information,
                                               i.e., CCA1. Then select one of the 4 search modes: Normal returns the entities 
                                               containing the term, Exact returns those that are identical, Alias returns all 
                                               aliases associated with the term and Substring returns all those containing a 
                                               given string. Associations found in the literature are returned in tabular form,
                                               with links to the papers from which the information was extracted. This
                                               functionality is based on PlantConnectome, to extend this analysis, it is recommended
                                               to go to https://connectome.plant.tools/."),
                                         fluidRow(br()),
                                         shinyWidgets::actionBttn("lit_start5", "Show Search Box for Literature Annotation",
                                                                  size = "sm", icon = icon("magnifying-glass"),
                                                                  style = "float", color = "royal"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "query_lit5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "selected_lit5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "lit_selection5"),
                                         fluidRow(tags$br()),
                                         shinyjs::hidden(div(id='loading.lit5',h3('Please be patient, browsing literature ...'))),
                                         uiOutput(outputId = "error_lit5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "box_lit_table5"),
                                         fluidRow(tags$br()),
                                         tags$div(id = "download_ui_for_lit_table5")
                                         
                                )
                    ))
              )
      ),
      
      # Last tab
      tabItem(tabName = "contact_tutorial", 
              
              h3(""),
              
              tags$div(style = 'font-size: 18px; margin-left: 20px;',"We are strongly committed to open access software and open science. PharaohFUN's source code is available
                       at GitHub following the lateral panel link and is released under a GNU General Public License v3.0. If you 
                       experience any problem using PharaohFUN, please create an issue in GitHub and we will address it."),
              tags$br(),
              
              fluidRow(
                column(8, 
                       tags$div(style = 'font-size: 18px; margin-left: 20px;', 
                                "All organisms images where acquired from PhyloPic. Next, we present a list with the authors of each one:",
                                tags$ul(
                                  tags$li("Phaeodactylum by Jonathan Wells."),
                                  tags$li("Porphyra Guillaume Dera."),
                                  tags$li("Cyanophora Guillaume Dera."),
                                  tags$li("Ostreococcus by Guillaume Dera."),
                                  tags$li("Scenedesmus Sergio A. Muñoz-Gómez."),
                                  tags$li("Klebsormidium by Matt Crook."),
                                  tags$li("Marchantia by Guillaume Dera."),
                                  tags$li("Selaginella by Mason McNair."),
                                  tags$li("Arabidopsis by Mason McNair."),
                                  tags$li("Chlamydomonas by Sergio A. Muñoz-Gómez."),
                                  tags$li("Haematococcus by Matthew Crook."),
                                  tags$li("Volvox by Matthew Crook."),
                                  tags$li("Zygnema by Matthew Crook"),
                                  tags$li("Marchantia by T. Michael Keesey."),
                                  tags$li("Araucaria by T. Michael Keesey."),
                                  tags$li("Gnetum by T. Michael Keesey."),
                                  
                                ),
                       )
                ),
                column(4, tags$div(align="center",width=60,
                                   HTML('<script type="text/javascript" src="//rf.revolvermaps.com/0/0/6.js?i=5yb18atzqxr&amp;m=7&amp;c=e63100&amp;cr1=ffffff&amp;f=arial&amp;l=0&amp;bv=90&amp;lx=-420&amp;ly=420&amp;hi=20&amp;he=7&amp;hc=a8ddff&amp;rs=80" async="async"></script>'))
                       
                )
              ),
              tags$div(style = 'font-size: 18px; margin-left: 20px;', 
                       "Some of these images are licensed under an ", tags$a(href="https://creativecommons.org/licenses/by-nc-sa/3.0/", 
                                                                             "Attribution-NonCommercial-ShareAlike 3.0 Unported"), "or ",
                       
                       tags$a(href="https://creativecommons.org/licenses/by-sa/3.0/", "Attribution-ShareAlike 3.0 Unported"), " license.")
      )
      # Close tabs, body and UI code   
      
    )
    
  )
)

server <- function(input, output) {
  
  ############## GENE ID-BASED SEARCH ##############
  
  # Set global variables for tracking changes in output
  UI_exist_pfam1 <<- F
  UI_exist_tree1 <<- F
  UI_exist_phylo1 <<- F
  UI_exist_cafe1 <<- F
  UI_exist_error_cafe1 <<- F
  UI_exist_msa1 <<- F
  UI_exist_go1 <<- F
  UI_exist_kegg1 <<- F
  UI_exist_pathview1 <<- F
  UI_exist_lit1 <<-  F
  UI_exist_string1 <<-  F
  UI_exist_network1 <<-  F
    
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
    if (build_trees1() == "Maximum Likelihood")
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
      if (build_trees1() == "Maximum Likelihood")
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
    
    
    insertUI("#box_tree_text1", "afterEnd", ui = {
      box(
        title = "Genes in Orthogroup", status = "info", solidHeader = TRUE, width = 12,
        collapsible = TRUE,
        verbatimTextOutput("treeTips1")
      )
    }) 
    
    insertUI("#box_tree_pie1", "afterEnd", ui = {
      box(
        title = "Present Organisms", status = "info", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        fluidRow(column(1), imageOutput("presentorg1"))
      )
    }) 
    
    insertUI("#box_tree_plot1", "afterEnd", ui = {
      box(width = 12,
          title = "Gene Tree", status = "info", solidHeader = TRUE,
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
      #scale_fill_brewer(palette="Set1")
      scale_fill_manual(values = rep(RColorBrewer::brewer.pal(n = 9, name = "Set1"), 
                                   floor(nrow(data)/9)+1))}

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
  
  
  ####################### PHYLOWIDGET ############################
  # Remove previous outputs when updated by a new search
  observeEvent(input$run_button1, {
    if (UI_exist_phylo1)
    {
      removeUI(
        selector = "div:has(>>> #phylo_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      UI_exist_phylo1 <<- F
    }
    
  })
  
  
  phylo_tree1 <- reactive({
    
    library(ape)
    tree_phylo <- tree_reduced1()
    
    # Normalize tree depth
    root_id <- length(tree_phylo$tip.label)+1
    norm_factor <- max(dist.nodes(tree_phylo)[root_id,])
    tree_phylo$edge.length <- tree_phylo$edge.length/norm_factor
    
    return(tree_phylo)
    
  }) %>% bindEvent(input$phylo_start1)
  
  observeEvent(isTruthy(phylo_tree1()),{
    
    if(UI_exist_phylo1)
    {
      removeUI(
        selector = "div:has(>>> #phylo_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      UI_exist_phylo1 <<- F
    }
    
    
    insertUI("#box_phylo1", "afterEnd", ui = {
      phylo_tree <- phylo_tree1()
      phylo_height <- length(phylo_tree$tip.label) *14 + 220
      box(width = 12,
          title = "Interactive Tree", status = "info", solidHeader = TRUE, height = phylo_height + 100,
          collapsible = TRUE,
          tags$div(id = "phylo_pocket1", style = paste0("width: 1300px; height: ",  phylo_height + 50, "px"),
                   phylowidgetOutput("phylo_plot1", height = paste0(phylo_height,"px"), width = "98%"))
      )
      })
    
    UI_exist_phylo1 <<- T
    
  })
  
  output$phylo_plot1 <- renderPhylowidget({
    
    phylo_tree <- phylo_tree1()
    phylowidget(phylo_tree)
  })
  
  #########################  PFAM  ###############################
  
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
          if (nap.time > 11){
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
          title = "Domains Localization", status = "info", solidHeader = TRUE, width = 12, height = box_pfplot_height+10,
          collapsible = TRUE,
          plotOutput("pfam_plot1", height = box_pfplot_height)
          )
        
      }) 
      
      insertUI("#pfam_down_button1", "afterEnd", ui = {
        tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "pfam_download1", "Download PFAM figure",
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
              title = "Ancestral State Reconstruction", status = "info", solidHeader = TRUE,
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
          title = "Ancestral State Reconstruction", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          imageOutput("cafe_plot1", height = 500, width = 1000))
    })

    insertUI("#box_mrca1", "afterEnd", ui = {
      box(width = 8,
          title = "Most Recent Common Ancestor", status = "info", solidHeader = TRUE,
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
    
    removeUI(
      selector = "div:has(>> #msa_methodI1)",
      multiple = TRUE,
      immediate = TRUE
    )
    
    removeUI("#msa_selection1")
    
    if (UI_exist_msa1)
    {
      removeUI(
        selector = "div:has(>>> #msa_print1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #msa_download_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #msa_download_fa1)",
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
    
    insertUI("#msa_method1", "afterEnd", ui = {
      shinyWidgets::pickerInput(inputId = "msa_methodI1", label = "Choose alignment method", 
                                choices = c("ClustalOmega", "MAFFT"), selected = "ClustalOmega")
      
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
    selected_method <- as.character(input$msa_methodI1)
    file.name <- og.name1()
    
    if (length(selected_genes) < 2)
    {
      shinyjs::hideElement(id = 'loading.msa1')
      if (UI_exist_msa1)
      {
      removeUI(
        selector = "div:has(>>> #msa_print1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #msa_download_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #msa_download_fa1)",
        multiple = TRUE,
        immediate = TRUE
      )
      }
      UI_exist_msa1 <<- F
      output$error_msa1 <- renderUI({renderText({print("Please select at least two genes.")})})
      validate(need(length(selected_genes) > 1, "Please select at least two genes."))
    }
    
    output$error_msa1 <- NULL
    
    # If de novo alignment is selected
    {
    if(selected_method == "ClustalOmega")
    {
    # Define path to orthogroup sequences file
    ortho.seq.name <- ifelse(model.selected1(),
                        paste("Global_Orthogroup_Sequences", paste(file.name, "fa", sep = "."), sep="/"),
                        paste("Green_Orthogroup_Sequences", paste(file.name, "fa", sep = "."), sep="/"))
    
    
    # Read orthogroup sequences file and select the genes for alignment
    mySequences1 <- Biostrings::readAAStringSet(ortho.seq.name)
    mysubseqs <- mySequences1[selected_genes]
    
    alignseqs <- msa(mysubseqs, verbose = F, method = "ClustalOmega")
    }
    
    # If MAFFT alignment is selected
    else
    {
      ortho.seq.name <- ifelse(model.selected1(),
                               paste("Global_MultipleSequenceAlignments", paste(file.name, "fa", sep = "."), sep="/"),
                               paste("Green_MultipleSequenceAlignments", paste(file.name, "fa", sep = "."), sep="/"))
      mySequences1 <- seqinr::read.fasta(ortho.seq.name, seqtype = "AA")
      mysubseqs <- mySequences1[selected_genes]
      mysubnames <- seqinr::getName(mySequences1)
      
      # Identify indexes associated with reduced names
      indexes_msa <- sapply(selected_genes, function(x) grep(mysubnames, pattern = x))
      
      # Retrieve those sequences from alignment keeping gaps
      mysubseqs <- mySequences1[indexes_msa]
      names(mysubseqs) <- names(indexes_msa)
      
      # Remove columns with gaps andremove empty spaces in last positions
      seqs_mysubseqs <- seqinr::getSequence(mysubseqs)
      last <- seqs_mysubseqs[[length(seqs_mysubseqs)]]
      last <- last[which(last != " ")]
      seqs_mysubseqs[[length(seqs_mysubseqs)]] <- last
      seqs_mysubseqs <- remove_gaps(seqs_mysubseqs)
      names(seqs_mysubseqs) <- names(mysubseqs)
      
      mysubseqs2 <- unlist(lapply(seqs_mysubseqs, function(x) paste(x, collapse="")))
      
      alignseqs <- Biostrings::AAMultipleAlignment(mysubseqs2, use.names = T)
      
    }
    }
    
    detach("package:msa", unload=TRUE)
    
    return(alignseqs)
    
    }) %>% bindEvent(input$msa_selection1)
  
  # Create boxes for outputs
  observeEvent(isTruthy(alignseqs1()), {
    
    if (UI_exist_msa1)
    {
      removeUI(
        selector = "div:has(>>> #msa_print1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #msa_download_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #msa_download_fa1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
    }
    
    insertUI("#box_msa1", "afterEnd", ui = {
      selected_msa <- isolate({input$selected_msaI1})
      msa_height <- ifelse(length(selected_msa) > 14, 550, 400 + 5*length(selected_msa))
      box(width = 12,
          title = "MSA Explorer", status = "info", solidHeader = TRUE, height = msa_height,
          collapsible = TRUE,
          tags$div(id = "msa_pocket1", style = "width: 1300px; height: 400px",
                   msaROutput("msa_print1"))
          
      )
    })
    
    insertUI("#msa_down_plot1", "afterEnd", ui = {
      tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "msa_download_plot1", "Download Colored MSA",
                                                                         size = "sm", color = "primary"))
    })
    
    insertUI("#msa_down_fasta1", "afterEnd", ui = {
      tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "msa_download_fa1", "Download MSA FASTA",
                                                                         size = "sm", color = "primary"))
    })
    
    UI_exist_msa1 <<- TRUE
    #shinyjs::hideElement(id = 'loading.msa1')
  })
  

  # Fill Output
  output$msa_print1 <- renderMsaR({
    alignseqs <- alignseqs1()
    msaout <- msa::msaConvert(alignseqs, "ape::AAbin")
    msaR(msaout, menu=T, overviewbox = F,  colorscheme = "clustal")
  })
  
  # Prepare variables for pdf construction
  observeEvent(isTruthy(alignseqs1()), {
  alignseqs <- alignseqs1()
  
  library(ggmsa)
  class(alignseqs) <- "AAMultipleAlignment"
  
  for(i in 1:(ncol(alignseqs)%/%100 +1)){
    assign(paste("msap", i, sep = ""), ggmsa(alignseqs, 1+(100*(i-1)), i*100, seq_name = TRUE, char_width = 0.5) +
             geom_seqlogo(color = "Chemistry_AA"), envir = as.environment(1), pos=1)
  }
  shinyjs::hideElement(id = 'loading.msa1')
  })
  
  # Download tab's results
  # Download colored MSA in pdf
  output$msa_download_plot1 <- downloadHandler(
    filename= function() {
      paste("msa", ".pdf", sep="")
    },
    content= function(file) {
      selected_msa <- input$selected_msaI1
      alignseqs <- alignseqs1()
      pdf(file, height = 2+length(selected_msa)*0.25, width = 16)
      {
        for(i in 1:(ncol(alignseqs)%/%100 +1)){
          print(mget(paste0("msap", i), envir = as.environment(1)))
        }
        dev.off()
      }
    })
  
  # Download MSA in FASTA format
  output$msa_download_fa1<- downloadHandler(
    filename= function() {
      paste("msa", ".fa", sep="")
    },
    content= function(file) {
      alignseqs <- alignseqs1()
      writeXStringSet(as(unmasked(alignseqs), "XStringSet"), file)
    })
  
  
  ####################### GO #################################
  
  observeEvent(input$run_button1, {
    removeUI(
      selector = "div:has(>> #selected_gosI1)",
      multiple = TRUE,
      immediate = TRUE
    )
    
    removeUI(
      selector = "div:has(>> #selected_gos_modeI1)",
      multiple = TRUE,
      immediate = TRUE
    )
    
    removeUI("#gos_selectionI1") ####### No sería sin I
    
    if (UI_exist_go1)
    {
      removeUI(
        selector = "div:has(>> #output_gos_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #gos_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #gos_treeplot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadGOSTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #gos_download1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #tree_gos_download1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      UI_exist_go1 <<- F
    }
    
  })
  
  observeEvent(input$go_start1, {
    insertUI("#selected_gos1", "afterEnd", ui = {
      
      shinyWidgets::pickerInput("selected_gosI1","Select the desired genes from the tree",
                                choices=isolate({tree_reduced1()$tip.label}), options = list(`actions-box` = TRUE),
                                multiple = T, selected = isolate({input$geneInt1}))
      
    })
    
    insertUI("#selected_gos_mode1", "afterEnd", ui = {
      
      selectInput(inputId = "selected_gos_modeI1",
                  choices=c("Biological Processes" = "bp",
                            "Molecular Functions" = "mf",
                            "Cellular Components" = "cc"),
                  label = "Select the gene ontology to use",
                  multiple = F, selected = c("bp"))
      
    })
    
    insertUI("#gos_selection1", "afterEnd", ui = {
      
      shinyWidgets::actionBttn("gos_selectionI1", "Show GO terms", size = "sm",
                               style = "float", color = "primary")
    })
    
  })
  
  total_table_gos1 <- reactive({
    
    shinyjs::showElement(id = 'loading.go1')
    gos_anot <- read.csv("pharaoh_folder/final_anot_table.tsv", sep="\t", header = T)
    sel.genes.go <- as.vector(input$selected_gosI1)
    selected_gos_mode <- as.character(isolate({input$selected_gos_modeI1}))
    
    total_table_gos <- subset(gos_anot, gos_anot$name %in% sel.genes.go)
    
    # Show an error if no terms are identified in the input
      if (nrow(total_table_gos) == 0) 
      {
        if (UI_exist_go1)
        {
          removeUI(
            selector = "div:has(>> #output_gos_table1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          removeUI(
            selector = "div:has(>> #gos_plot1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          removeUI(
            selector = "div:has(>> #gos_treeplot1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          removeUI(
            selector = "div:has(>> #downloadGOSTable1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          removeUI(
            selector = "div:has(>> #gos_download1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          removeUI(
            selector = "div:has(>> #tree_gos_download1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          UI_exist_go1 <<- F
        }
        
        shinyjs::hideElement(id = 'loading.go1')
        output$error_gos1 <- renderUI({
          renderPrint({cat("0 GO terms identified.")})
        })
        
        validate(need(nrow(total_table_gos) != 0, " "))
      }
    
    
    gos_sel <- paste("gos", selected_gos_mode, sep="_")
    terms_sel <- paste("terms", selected_gos_mode, sep="_")
    total_table_gos <- total_table_gos[,c("organism", "id", "name", gos_sel, terms_sel)]
    
    # Once removed the two GO categories not selected, remove rows with blank cells
    total_table_gos_clean <- subset(total_table_gos, gos_sel != "")
    
    # Show an error if no terms are identified after the previous operation
    if (nrow(total_table_gos_clean) == 0) 
    {
      shinyjs::hideElement(id = 'loading.go1')
      if (UI_exist_go1)
      {
        removeUI(
          selector = "div:has(>> #output_gos_table1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #gos_plot1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #gos_treeplot1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadGOSTable1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #gos_download1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #tree_gos_download1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        UI_exist_go1 <<- F
      }
      
      output$error_gos1 <- renderUI({
        renderPrint({cat("0 GO terms identified.")})
      })
      
      validate(need(nrow(total_table_gos_clean) != 0, " "))
    }
    
    output$error_gos1 <- NULL
    
    return(total_table_gos_clean)
    
  }) %>% bindEvent(input$gos_selectionI1)
  
  enr_table1 <- reactive({
    
    total_table_gos <- total_table_gos1()
    selected_gos_mode <- as.character(isolate({input$selected_gos_modeI1}))
    
    # Create the plot
    
    # Create a list of GO terms vector of each gene
    gos_sel <- paste("gos", selected_gos_mode, sep="_")
    gos_list <- apply(total_table_gos,MARGIN=1,FUN = function(x) trimws(strsplit(as.character(x[gos_sel]), split = "[|]")[[1]]))
    names(gos_list) <- total_table_gos$name
    
    # Count GOs for each gene and create a matrix of gene-GO pairs
    count_gos_in_genes <- sapply(gos_list, FUN = function(x) length(x))
    comp_data <- data.frame(gene = rep(names(count_gos_in_genes), count_gos_in_genes), gos = as.character(unlist(gos_list)))
    
    # Collapse genes that share a same GO
    gene.v <- c()
    for (i in 1:length(unique(comp_data$gos)))
    {
      new.table <- subset(comp_data, gos == unique(comp_data$gos)[i])
      new.cha <- as.character(new.table$gene)
      gene.v <- c(gene.v, paste(new.cha, collapse = "/"))
    }
    
    names(gene.v) <- unique(comp_data$gos)
    
    # Load libraries and create gene chains, count and GO IDs fields (same order)
    library(GO.db)
    library("multienrichjam")
    library(clusterProfiler)
    library(enrichplot)
    library(ggplot2)
    
    count_go <- table(comp_data$gos)
    geneids <- gene.v[names(count_go)]
    count_terms <- mapply(function(x) {Term(x)}, names(count_go), USE.NAMES = F)
    
    # Create pseudo-enrichment table
    enr_table <- data.frame(ID=names(count_go), Description=count_terms, GeneRatio="90/100", BgRatio="90/10000", 
                            pvalue=0.000005, p.adjust=0.000005, qvalue=0.000005, geneID=geneids, Count = as.vector(count_go))
    
    return(enr_table)
    
  }) %>% bindEvent(input$gos_selectionI1)
  
  ema_gos_plot1 <- reactive({
    
    enr_table <- enr_table1()
    
    # Transform to enrichResult object
    enr <- enrichDF2enrichResult(enrichDF = enr_table, keyColname = "ID",
                                 geneColname = "geneID", pvalueColname = "p.adjust",
                                 descriptionColname = "Description", pvalueCutoff = 0.05)
    
    # Create plot
    ema_gos_plot <- emapplot(pairwise_termsim(enr), showCategory = 15) + theme(legend.position='none')
    return(ema_gos_plot)
  })
  
  tree_gos_plot1 <- reactive({
    
    enr_table <- enr_table1()
    
    # Transform to enrichResult object
    enr <- enrichDF2enrichResult(enrichDF = enr_table, keyColname = "ID",
                                 geneColname = "geneID", pvalueColname = "p.adjust",
                                 descriptionColname = "Description", pvalueCutoff = 0.05)
    {
      if (nrow(enr_table) > 4)
      {
        tree_gos_plot <- treeplot(pairwise_termsim(enr),showCategory = 15, cluster.params = list(label_words_n = 3)) +
          theme(legend.position='none')
      }
      else if (nrow(enr_table) > 2)
      {
        tree_gos_plot <- treeplot(pairwise_termsim(enr),showCategory = 15, cluster.params = list(n = 2, label_words_n = 3)) + 
          theme(legend.position='none')
      }
      else
      {
        text <- paste("\n  Unable to create treeplot with less than 3 GO terms \n")
        tree_gos_plot <- ggplot() + 
          annotate("text", x = 4, y = 25, size=8, label = text) + 
          theme_void()
      }
    }
    
    shinyjs::hideElement(id = 'loading.go1')
    return(tree_gos_plot)
  })
  
  # Create boxes for outputs
  observeEvent(isTruthy(tree_gos_plot1()), {
    
    if (UI_exist_go1)
    {
      removeUI(
        selector = "div:has(>> #output_gos_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #gos_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #gos_treeplot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadGOSTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #gos_download1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #tree_gos_download1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
    }
    
    insertUI("#box_gos_table1", "afterEnd", ui = {
      box(width = 12,
          title = "GO Terms Table", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          dataTableOutput("output_gos_table1")
      )
    })
    
    insertUI("#box_gos_plot1", "afterEnd", ui = {
      box(width = 12,
          title = "GO Terms Plot", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("gos_plot1", height = 610)
      )
    })
    
    insertUI("#box_gos_treeplot1", "afterEnd", ui = {
      box(width = 12,
          title = "GO Terms Treeplot", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("gos_treeplot1", height = 610)
      )
    })
    
    
    insertUI("#download_ui_for_gos_table1", "afterEnd", ui = {
      tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadGOSTable1", "Download GO Table",
                                                                         size = "sm", color = "primary"))
    })
    
    insertUI("#gos_down_button1", "afterEnd", ui = {
      tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "gos_download1", "Download GO Plot",
                                                                         size = "sm", color = "primary"))
    })
    
    insertUI("#tree_gos_down_button1", "afterEnd", ui = {
      tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "tree_gos_download1", "Download GO Treeplot",
                                                                         size = "sm", color = "primary"))
    })
    
    UI_exist_go1 <<- TRUE
  })
  
  # Fill outputs
  # Table
  output$output_gos_table1 <- renderDataTable({
    total_table_gos <- total_table_gos1()
    selected_gos_mode <- as.character(isolate({input$selected_gos_modeI1}))
    gos_sel <- paste("gos", selected_gos_mode, sep="_")
    gos_list <- apply(total_table_gos,MARGIN=1,
                      FUN = function(x) trimws(strsplit(as.character(x[gos_sel]), split = "[|]")[[1]]))
    gos_links <- lapply(gos_list, function(x) sapply(x, go.link))
    gos_formatted <- unlist(lapply(gos_links, function(x) paste0(x, collapse = " | ")))
    total_table_gos[,gos_sel] <- gos_formatted
    total_table_gos
  },escape=FALSE, rownames=F, options =list(pageLength = 5))
  
  # First plot
  output$gos_plot1 <- renderImage({
    ema_gos_plot <- ema_gos_plot1()
    
    png("pharaoh_folder/gosplot.png", width = 590, height = 590, res = 90)
    plot(ema_gos_plot)
    dev.off()
    list(src = "pharaoh_folder/gosplot.png",
         contentType="image/png")
    
  }, deleteFile=T
  )
  
  # Second plot
  output$gos_treeplot1 <- renderImage({
    tree_gos_plot <- tree_gos_plot1()
    
    png("pharaoh_folder/treeplot.png", width = 620, height = 590, res = 80)
    plot(tree_gos_plot)
    dev.off()
    list(src = "pharaoh_folder/treeplot.png",
         contentType="image/png")
    
  }, deleteFile=T
  )
  
  # Download tab's results
  # Download GO table
  output$downloadGOSTable1 <- downloadHandler(
    filename= function() {
      paste("GOS_table", ".tsv", sep="")
    },
    content= function(file) {
      total_table_gos <- total_table_gos1()
      
      write.table(x = total_table_gos,quote = F,sep = "\t",
                  file=file,row.names=FALSE,col.names=TRUE)
    })
  
  # Download emaplot
  output$gos_download1 <- downloadHandler(
    filename= function() {
      paste("gos_plot", ".png", sep="")
    },
    content= function(file) {
      ema_gos_plot <- ema_gos_plot1()
      
      png(file, height = 1200, width = 1500, res=140)
      plot(ema_gos_plot)
      dev.off()
    })
  
  # Download treeplot
  output$tree_gos_download1 <- downloadHandler(
    filename= function() {
      paste("gos_treeplot", ".png", sep="")
    },
    content= function(file) {
      tree_gos_plot <- tree_gos_plot1()
      
      png(file, height = 1200, width = 1500, res=140)
      plot(tree_gos_plot)
      dev.off()
    })
  

###################### KEGG ###########################
  
  observeEvent(input$run_button1, {
    removeUI(
      selector = "div:has(>> #selected_kosI1)",
      multiple = TRUE,
      immediate = TRUE
    )
    
    removeUI("#kos_selectionI1") # MIRAR SI EN GO ESTA CON O SIN I
    
    if (UI_exist_kegg1)
    {
      removeUI(
        selector = "div:has(>> #output_kos_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #output_kegg_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadKOSTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadKEGGTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #selected_pathsI1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI("#paths_buttonI1")
      
      UI_exist_kegg1 <<- F
    }
    
    if (UI_exist_pathview1)
    {
      removeUI(
        selector = "div:has(>> #path_image1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadKEGGpathway1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      UI_exist_pathview1 <<- F
    }
    
    
  })
  
  observeEvent(input$kegg_start1, {
    insertUI("#selected_kos1", "afterEnd", ui = {
      
      shinyWidgets::pickerInput("selected_kosI1","Select the desired genes from the tree",
                                choices=isolate({tree_reduced1()$tip.label}), options = list(`actions-box` = TRUE),
                                multiple = T, selected = isolate({input$geneInt1}))
      
      
    })
    
    
    insertUI("#kos_selection1", "afterEnd", ui = {
      
      shinyWidgets::actionBttn("kos_selectionI1", "Show KEGG pathways", size = "sm",
                               style = "float", color = "primary")
    })
    
  })
  
  tab_kegg1 <- reactive({
    
    shinyjs::showElement(id = 'loading.ko1')
    # Create KOs set
    kos_anot <- read.csv("pharaoh_folder/ko_table_funtree.tsv", sep="\t", header = T)
    sel.genes.ko <- as.vector(isolate({input$selected_kosI1}))
    
    tab_kegg <- subset(kos_anot, gene %in% sel.genes.ko)
    set_kegg <- tab_kegg$ko[tab_kegg$ko != ""]
    
    # Show an error if no terms are identified in the input
    {
      if (length(set_kegg) == 0) 
      {
        shinyjs::hideElement(id = 'loading.ko1')
        output$error_kos1 <- renderUI({
          renderPrint({cat("0 KO terms identified. Please select more genes. If this 
        message persists, it should be interpreted as a lack of KO annotation for this orthogroup")})
        })
        
        if (UI_exist_kegg1)
        {
          removeUI(
            selector = "div:has(>> #output_kos_table1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          removeUI(
            selector = "div:has(>> #output_kegg_table1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          removeUI(
            selector = "div:has(>> #downloadKOSTable1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          removeUI(
            selector = "div:has(>> #downloadKEGGTable1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          removeUI(
            selector = "div:has(>> #selected_pathsI1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          removeUI("#paths_buttonI1")
          
          UI_exist_kegg1 <<- F
        }
        
        if (UI_exist_pathview1)
        {
          removeUI(
            selector = "div:has(>> #path_image1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          removeUI(
            selector = "div:has(>> #downloadKEGGpathway1)",
            multiple = TRUE,
            immediate = TRUE
          )
          
          UI_exist_pathview1 <<- F
        }
        
        validate("No KO terms detected")
      }
    }
    
    output$error_kos1 <- NULL
    
    return(tab_kegg)
    
  }) %>% bindEvent(input$kos_selectionI1)
  
  total_table_kegg1 <- reactive({
    
    tab_kegg <- tab_kegg1()
    set_kegg <- tab_kegg$ko[tab_kegg$ko != ""]
    
    # Load libraries
    library(clusterProfiler)
    library(enrichplot)
    
    # Enrich with pvalue cutoff = 1 to show all paths
    kos_enrich <- enrichKEGG(gene         = set_kegg,
                             organism     = 'ko',
                             pvalueCutoff = 1)
    
    total_table_kegg <- as.data.frame(kos_enrich)
    
    # Filter out pathways that are not present in plants
    kegg_plants <- read.csv("pharaoh_folder/pathways_plant.ids", sep = "\t", header = T)$x
    total_table_kegg <- subset(total_table_kegg, ID %in% kegg_plants)
    
    return(total_table_kegg)
    
  }) %>% bindEvent(input$kos_selectionI1)
  
  total_table_kos1 <- reactive({
    
    tab_kegg <- tab_kegg1()
    
    library(KEGGREST)
    
    # Collapse genes that share KOs
    
    tab_kegg_for_ko <- subset(tab_kegg, ko != "")
    gene.v.ko <- c()
    for (i in 1:length(unique(tab_kegg_for_ko$ko)))
    {
      new.table <- subset(tab_kegg_for_ko, ko == unique(tab_kegg_for_ko$ko)[i])
      new.cha <- as.character(new.table$gene)
      gene.v.ko <- c(gene.v.ko, paste(new.cha, collapse = "/"))
    }
    
    names(gene.v.ko) <- unique(tab_kegg_for_ko$ko)
    
    # Create gene chains, count and KO IDs fields (same order)
    count_ko <- table(tab_kegg_for_ko$ko)
    geneids.ko <- gene.v.ko[names(count_ko)]
    count_terms.ko <- mapply(function(x) {keggFind("ko", x)}, names(count_ko), USE.NAMES = F)
    total_table_kos <- data.frame(ko=names(count_ko), name=count_terms.ko, count=as.numeric(count_ko),
                                  genes=geneids.ko)
    
    return(total_table_kos)
    
  }) %>% bindEvent(input$kos_selectionI1)
  
  # Create boxes for outputs
  observeEvent(isTruthy(total_table_kos1()), {
    
    if (UI_exist_kegg1)
    {
      removeUI(
        selector = "div:has(>> #output_kos_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #output_kegg_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadKOSTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadKEGGTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #selected_pathsI1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI("#paths_buttonI1")
      
    }
    
    insertUI("#box_kos_table1", "afterEnd", ui = {
      box(width = 12,
          title = "KO Terms Table", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          dataTableOutput("output_kos_table1")
      )
    })
    
    
    insertUI("#box_kegg_table1", "afterEnd", ui = {
      box(width = 12,
          title = "KEGG Pathways Table", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          dataTableOutput("output_kegg_table1")
      )
    })
    
    insertUI("#download_ui_for_kos_table1", "afterEnd", ui = {
      tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadKOSTable1", "Download KO Table",
                                                                         size = "sm", color = "primary"))
    })
    
    insertUI("#download_ui_for_kegg_table1", "afterEnd", ui = {
      tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadKEGGTable1", "Download Pathways Table",
                                                                         size = "sm", color = "primary"))
    })
    
    UI_exist_kegg1 <<- TRUE
    
    # Remove previous results for pathview
    if (UI_exist_pathview1)
    {
      removeUI(
        selector = "div:has(>> #path_image1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadKEGGpathway1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      UI_exist_pathview1 <<- F
    }
  })
  
  # Fill outputs
  # Render KO table
  output$output_kos_table1 <- renderDataTable({
    total_table_kos <- total_table_kos1()
    total_table_kos$ko <- sapply(total_table_kos$ko, ko.link)
    total_table_kos
  },escape=FALSE, rownames= F, options =list(pageLength = 5))
  
  # Render KEGG table
  output$output_kegg_table1 <- renderDataTable({
    total_table_kegg <- total_table_kegg1()
    total_table_kegg$ID <- sapply(total_table_kegg$ID, kegg.link)
    total_table_kegg[,c("ID", "Description", "geneID")]
  },escape=FALSE, rownames= F, options =list(pageLength = 5))
  
  # Download tab's outputs
  # Download KO table
  output$downloadKOSTable1 <- downloadHandler(
    filename= function() {
      paste("KO_table", ".tsv", sep="")
    },
    content= function(file) {
      total_table_kos <- total_table_kos1()
      write.table(x = total_table_kos,quote = F,sep = "\t",
                  file=file,row.names=FALSE,col.names=TRUE)
    })
  
  # Download KEGG table
  output$downloadKEGGTable1 <- downloadHandler(
    filename= function() {
      paste("KEGG_table", ".tsv", sep="")
    },
    content= function(file) {
      total_table_kegg <- total_table_kegg1()
      write.table(x = total_table_kegg[,c("ID", "Description", "geneID")],quote = F,sep = "\t",
                  file=file,row.names=FALSE,col.names=TRUE)
    })
  
  # Create pathway selector and button
  observeEvent(input$kos_selectionI1,{
    
    total_table_kos <- total_table_kos1()
    total_table_kegg <- total_table_kegg1()
    
    if(nrow(total_table_kegg) != 0)
    {
      paths.options <- sapply(strsplit(total_table_kegg$ID, split = "ko"), function(x) x[[2]])
      
      
      insertUI("#selected_paths1", "afterEnd", ui = {
        shinyWidgets::pickerInput(inputId = "selected_pathsI1", label = "Select the pathway to plot", 
                                  choices = paths.options, selected = paths.options[1], multiple = F)
        
      })
        
      
      insertUI("#paths_button1", "afterEnd", ui = {
        
        shinyWidgets::actionBttn("paths_buttonI1", "Plot Pathway", size = "sm",
                                 style = "float", color = "primary")
      })
      
      shinyjs::hideElement(id = 'loading.ko1')
    }
  })
  
  observeEvent(input$paths_buttonI1,{
    
    if (UI_exist_pathview1)
    {
      removeUI(
        selector = "div:has(>> #path_image1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadKEGGpathway1)",
        multiple = TRUE,
        immediate = TRUE
      )
    }
    
    UI_exist_pathview1 <<- F
    
  })
  
  # Create Image to Render and save path name
  pathway.current.id1 <- reactive({
    pathway.current.id <- input$selected_pathsI1
    total_table_kos <- total_table_kos1()
    
    kos_unique <- unique(total_table_kos$ko)
    gene.pathway <- rep(0, length(kos_unique))
    names(gene.pathway) <-  kos_unique
    gene.pathway[kos_unique] <-1
    
    library(pathview)
    pathview(gene.data = sort(gene.pathway,decreasing = TRUE),kegg.dir = "pharaoh_folder",
             pathway.id = pathway.current.id,
             species = "ko",
             limit = list(gene=max(abs(gene.pathway)), cpd=1),
             gene.idtype ="kegg")
    
    return(pathway.current.id)
    
  }) %>% bindEvent(input$paths_buttonI1)
  
  # Create output box and download button
  observeEvent(isTruthy(pathway.current.id1()),{
    
    insertUI("#box_path_image1", "afterEnd", ui = {
      box(width = 12,
          title = "KEGG Pathway Plot", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(column(1), imageOutput("path_image1", width = "100%", height = "700px"))
      )
    })
    
    insertUI("#path_download_ui1", "afterEnd", ui = {
      tags$div(shinyWidgets::downloadBttn(outputId= "downloadKEGGpathway1", "Download KEGG Pathway Plot",
                                          size = "sm", color = "primary"))
    })
    
    UI_exist_pathview1 <<- T
    
  })
  
  # Fill path image output
  output$path_image1 <- renderImage({
    
    pathway.current.id <- pathway.current.id1()
    list(src = paste(c(paste0(c("ko",pathway.current.id), collapse=""),"pathview","png"), collapse="."),
         contentType="image/png",width=900,height=700)
  },deleteFile = F)
  
  # Download and remove path image output
  output$downloadKEGGpathway1 <- downloadHandler(
    filename= function() {
      paste("path_plot", ".png", sep="")
    },
    content= function(file) {
      pathway.current.id <- pathway.current.id1()
      file.copy(paste(c(paste0(c("ko",pathway.current.id), collapse=""),"pathview","png"), collapse="."), file)
      file.remove(paste(c(paste0(c("ko",pathway.current.id), collapse=""),"pathview","png"), collapse="."))
    })
  
############################# LITERATURE ANNOTATION ##########################################
  
  observeEvent(input$run_button1, {
    removeUI(
      selector = "div:has(>> #selected_litI1)",
      multiple = TRUE,
      immediate = TRUE
    )
    
    removeUI(
      selector = "div:has(> #query_litI1)",
      multiple = TRUE,
      immediate = TRUE
    )
    
    removeUI("#lit_selectionI1")
    
    
    if (UI_exist_lit1)
    {
      removeUI(
        selector = "div:has(>> #output_lit_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadLITTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      UI_exist_lit1 <<- F
    }
    
  })
  
  observeEvent(input$lit_start1, {
    
    insertUI("#query_lit1", "afterEnd", ui = {
      
    textInput(inputId = "query_litI1",value = "", label = "Enter search term", placeholder = "CCA1")
      
    })
    
    
    insertUI("#selected_lit1", "afterEnd", ui = {
      
      shinyWidgets::pickerInput("selected_litI1","Select the search mode",
                                choices=c("Normal","Exact", "Substring", "Alias"), 
                                #options = list(`actions-box` = TRUE),
                                multiple = F, selected = "Normal")
      
      
    })
    
    
    insertUI("#lit_selection1", "afterEnd", ui = {
      
      shinyWidgets::actionBttn("lit_selectionI1", "Get biological information and papers", size = "sm",
                               style = "float", color = "primary")
    })
    
  })
  
  pc_result1 <- reactive({
    
    shinyjs::showElement(id = 'loading.lit1')
    pc_search <- as.character(input$query_litI1)
    pc_search <- gsub(" ", "%20", pc_search) # To make spaces interpretable
    pc_modality <- tolower(as.character(input$selected_litI1))
    
    # Get PlantConnectome URL for query
    pc_url <- paste(c("https://connectome.plant.tools", pc_modality, pc_search), collapse = "/")
    

    library(RCurl)
    
    pc_res <- getURL(pc_url)
    
    if (!length(grep("No hits", pc_res)) == 0)
    {
       shinyjs::hideElement(id = 'loading.lit1')
        
      if (UI_exist_lit1)
      {
        removeUI(
          selector = "div:has(>> #output_lit_table1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        removeUI(
          selector = "div:has(>> #downloadLITTable1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        UI_exist_lit1 <<- F
      }
      
      output$error_lit1 <- renderUI({
        renderPrint({cat("No results found for this query")})
        })
      
      validate(" ")
      
    }
    
    output$error_lit1 <- NULL
        
    # Isolate data frame from complete HTML file
    pc_split <- strsplit(as.character(pc_res), split = "<tbody")[[1]][2]
    pc_split <- strsplit(as.character(pc_split), split = "</tbody>")[[1]][1]
        
    pc_clean <- gsub("</tr>", "", pc_split)
    pc_clean <- gsub("[\r\n\t]", "", pc_clean)
        
    pc_vector <- strsplit(pc_clean, split = "<tr>")[[1]][-1]
    pc_vector2 <- sapply(pc_vector, FUN=function(x) strsplit(x, split = "<td> | </td>"))
    pc_table <- sapply(pc_vector2, FUN=function(x) as.character(unlist(x)))
        
    colnames(pc_table) <- NULL
    pc_result <- data.frame(t(pc_table[c(2,4,6,8),]))
    colnames(pc_result) <- c("Source", "Interaction Type", "Target", "Pubmed ID")

    return(pc_result)
    
  }) %>% bindEvent(input$lit_selectionI1)
  
  pc_result_show1 <- reactive({
    
    pc_result <- pc_result1()
    
    # Add links to papers
    urls_connect <- sapply(pc_result$`Pubmed ID`, FUN = function(x) paste0(c("<a href=\"",
                                                                             "https://pubmed.ncbi.nlm.nih.gov/",x,"/",
                                                                             "\" target=\"_blank\">", x,
                                                                             "</a>"),
                                                                           collapse=""))
    pc_result_show <- pc_result
    pc_result_show$`Pubmed ID` <- urls_connect
    
    #shinyjs::hideElement(id = 'loading.lit1')
    
    return(pc_result_show)
    
  })
  
  # Create boxes for outputs
  observeEvent(isTruthy(pc_result_show1()), {
    
    if (UI_exist_lit1)
    {
      removeUI(
        selector = "div:has(>> #output_lit_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadLITTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
    }
    
    insertUI("#box_lit_table1", "afterEnd", ui = {
      box(width = 12,
          title = "Literature Table", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          dataTableOutput("output_lit_table1")
      )
    })
    
    insertUI("#download_ui_for_lit_table1", "afterEnd", ui = {
      tags$div(style = "margin-left: 400px;", shinyWidgets::downloadBttn(outputId= "downloadLITTable1", "Download Literature Table",
                                                                         size = "sm", color = "primary"))
    })
    
    shinyjs::hideElement(id = 'loading.lit1')
    UI_exist_lit1 <<- TRUE
    
  })
  
  # Fill outputs
  # Render table
  output$output_lit_table1 <- renderDataTable({
    pc_result_show1()
  },escape=FALSE, rownames= F, options =list(pageLength = 10))
  
  # Download results
  output$downloadLITTable1 <- downloadHandler(
    filename= function() {
      paste("literature_table", ".tsv", sep="")
    },
    content= function(file) {
      pc_result <- pc_result1()
      write.table(x = pc_result,quote = F,sep = "\t",
                  file=file,row.names=FALSE,col.names=TRUE)
    })
  
  ######################## STRING ###########################
  
  observeEvent(input$run_button1, {
    removeUI(
      selector = "div:has(>> #selected_stringI1)",
      multiple = TRUE,
      immediate = TRUE
    )
    
    removeUI("#string_selectionI1")
    
    shinyjs::hideElement("error_string1")
    
    
    if (UI_exist_string1)
    {
      removeUI(
        selector = "div:has(>> #output_st_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #output_count_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>>> #count_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadSTRINGTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadCOUNTTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #count_download1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #selected_networkI1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI("#network_buttonI1")
      
      UI_exist_string1 <<- F
    }
    
    if (UI_exist_network1)
    {
      removeUI(
        selector = "div:has(>>>> #network_image1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      UI_exist_network1 <<- F
    }
    
  })
  
  # First, create a table that links reduced gene names to its species
  string_sel_table1 <- reactive({
    
    # Define previous variables
    tree_reduced <- tree_reduced1()
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
    
    # Table construction
    org.factor <- c()
    
    for (i in 1:length(tree_reduced$tip.label))
    {
      
      if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mp])
      {
        org.factor <- c(org.factor,"Marchantia")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ot])
      {
        org.factor <- c(org.factor,"Ostreococcus")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.at])
      {
        org.factor <- c(org.factor,"Arabidopsis")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cp])
      {
        org.factor <- c(org.factor,"Ceratodon")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cr])
      {
        org.factor <- c(org.factor,"Chlamydomonas")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cz])
      {
        org.factor <- c(org.factor,"Chromochloris")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.kn])
      {
        org.factor <- c(org.factor,"Klebsormidium")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.me])
      {
        org.factor <- c(org.factor,"Mesotaenium")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mi])
      {
        org.factor <- c(org.factor,"Micromonas")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pp])
      {
        org.factor <- c(org.factor,"Physcomitrium")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sl])
      {
        org.factor <- c(org.factor,"Solanum")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sm])
      {
        org.factor <- c(org.factor,"Selaginella")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sp])
      {
        org.factor <- c(org.factor,"Spirogloea")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ta])
      {
        org.factor <- c(org.factor,"Triticum")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.vc])
      {
        org.factor <- c(org.factor,"Volvox")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.bp])
      {
        org.factor <- c(org.factor,"Bathycoccus")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cri])
      {
        org.factor <- c(org.factor,"Ceratopteris")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ds])
      {
        org.factor <- c(org.factor,"Dunaliella")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.os])
      {
        org.factor <- c(org.factor,"Oryza")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.smag])
      {
        org.factor <- c(org.factor,"Sphagnum")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.tp])
      {
        org.factor <- c(org.factor,"Thuja")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.aa])
      {
        org.factor <- c(org.factor,"Anthoceros")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.um])
      {
        org.factor <- c(org.factor,"Ulva")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.rs])
      {
        org.factor <- c(org.factor,"Raphidocelis")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cyc])
      {
        org.factor <- c(org.factor,"Cycas")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pu])
      {
        org.factor <- c(org.factor,"Porphyra")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pt])
      {
        org.factor <- c(org.factor,"Phaeodactylum")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ng])
      {
        org.factor <- c(org.factor,"Nannochloropsis")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cyano])
      {
        org.factor <- c(org.factor,"Cyanophora")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ca])
      {
        org.factor <- c(org.factor,"Chlorokybus")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mv])
      {
        org.factor <- c(org.factor,"Mesostigma")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.af])
      {
        org.factor <- c(org.factor,"Azolla")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sc])
      {
        org.factor <- c(org.factor,"Salvinia")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.aegi])
      {
        org.factor <- c(org.factor,"Aegilops")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sb])
      {
        org.factor <- c(org.factor,"Sorghum")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.chara])
      {
        org.factor <- c(org.factor,"Chara")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.guilla])
      {
        org.factor <- c(org.factor,"Guillardia")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.crypto])
      {
        org.factor <- c(org.factor,"Cryptophyceae")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cymero])
      {
        org.factor <- c(org.factor,"Cyanidioschyzon")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.galsul])
      {
        org.factor <- c(org.factor,"Galdieria")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.gracichor])
      {
        org.factor <- c(org.factor,"Gracilariopsis")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sceobli])
      {
        org.factor <- c(org.factor,"Scenedesmus")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cocco])
      {
        org.factor <- c(org.factor,"Coccomyxa")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.saccha])
      {
        org.factor <- c(org.factor,"Saccharina")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.haema])
      {
        org.factor <- c(org.factor,"Haematococcus")
      }
      else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.zm])
      {
        org.factor <- c(org.factor,"Zea")
      }
      
    }
    
    #Matrix with labels and colors and transform to dplyr format
    string.sel.table <- data.frame(node = 1:length(tree_reduced$tip.label), label = tree_reduced$tip.label,
                                   org = org.factor)
    
    return(string.sel.table)
    
  }) %>% bindEvent(input$string_start1)
  
  # Now, selection is allowed for genes of species with STRING support
  observeEvent(input$string_start1, {
    
    string_sel_table <- string_sel_table1()
    allow_string_species <- c("Aegilops", "Arabidopsis", "Bathycoccus", "Chara", "Chlamydomonas",
                              "Coccomyxa", "Cyanidioschyzon", "Galdieria", "Gracilariopsis",
                              "Guillardia", "Klebsormidium", "Micromonas","Oryza",
                              "Ostreococcus", "Phaeodactylum","Physcomitrium", "Raphidocelis",
                              "Scenedesmus", "Selaginella", "Solanum", "Sorghum", "Triticum",
                              "Volvox")
    
    st_genes <- subset(string_sel_table, string_sel_table$org %in% allow_string_species)$label
    
    if (length(st_genes) == 0)
    {
      shinyjs::showElement("error_string1")
      output$error_string1 <- renderUI({renderText({print("No results for this
      analysis due to lack of genes of STRING-supported species in the selection.")})})
      validate(" ")
    }
    
    output$error_string1 <- NULL
    
    insertUI("#selected_string1", "afterEnd", ui = {
      
      shinyWidgets::pickerInput("selected_stringI1","Select the desired genes from the tree",
                                choices=st_genes, options = list(`actions-box` = TRUE),
                                multiple = T)
      
    })
    
    
    insertUI("#string_selection1", "afterEnd", ui = {
      
      shinyWidgets::actionBttn("string_selectionI1", "Show STRING Interactions", size = "sm",
                               style = "float", color = "primary")
    })
    
  })
  
  phys_table1 <- reactive({
    
    shinyjs::showElement(id = 'loading.string1')
    query_genes <- input$selected_stringI1
    
    # Load complete STRING annotation table
    library(data.table)
    
    # Load iteratively from split files using data.table format
    data_phys <- fread("pharaoh_folder/string_physical/string_physical_1.tsv")
    
    for (x in list.files("pharaoh_folder/string_physical/")[-1])
    {
      data_phys <- rbind(data_phys, 
                         fread(paste0("pharaoh_folder/string_physical/", x)))
    }
    
    
    
    # Subset by query genes using data.table for speed
    #string_res <- subset(data_phys, data_phys$prot_query %in% query_genes)
    string_res <- data_phys[prot_query %in% query_genes,]
    string_res <- as.data.frame(string_res)
    
    # Assign OG ID to each target
    ortho_data_file <- ifelse(model.selected1(), "Global_Gene_Trees/Orthogroups.tsv",
                              "Green_Gene_Trees/Orthogroups.tsv")
    
    ortho_data <- as.data.frame(fread(ortho_data_file))
    
    ortho_char <- apply(ortho_data, MARGIN = 1, function(x) paste(x, collapse = ","))
    ortho.numbers <- sapply(string_res$prot_interaction, function(x) grep(x, ortho_char), USE.NAMES = F)
    
    # If a pattern is found in several names, i.e., is a subpattern of several genes,
    # search for the exact match
    if(class(ortho.numbers) == "list")
    {
      # If a gene isn't associated to an OG
      index.none <- which(sapply(ortho.numbers, function(x) length(x) == 0))
      
      if (length(index.none) != 0)
      {
        # We create another row for OG table to associate those genes
        ortho_data <- rbind(ortho_data, "No OG")
        ortho.numbers[index.none] <- nrow(ortho_data)
      }
      
      # If a gene has more than one match due to subpatterns
      index.wrong <- which(sapply(ortho.numbers, function(x) length(x) > 1))
      {
        if (length(index.wrong) == 0)
        {
          ortho.numbers <- unlist(ortho.numbers, use.names = F)
        }
        else
        {
          for (i in index.wrong)
          {
            for (j in ortho.numbers[[i]])
            {
              ortho_char_split <- strsplit(ortho_char[j],split = ",")
              ortho_char_split_clean <- sapply(ortho_char_split, function(x) gsub(" ", "", x))
              if (string_res$prot_interaction[i] %in% ortho_char_split_clean)
              {
                ortho.numbers[i] <- j
                ortho.numbers <- unlist(ortho.numbers, use.names = F)
              }
            }
          }
        }
      }
    }
    
    
    
    # Create the final table
    ortho.string.names <- ortho_data$Orthogroup[ortho.numbers]
    phys_table <- data.frame(string_res, orthogroup = ortho.string.names)
    
    return(phys_table)
  }) %>% bindEvent(input$string_selectionI1)
  
  # Create count table to identify enriched OGs in STRING result
  string_counts1 <- reactive({
    
    phys_table <- phys_table1()
    string_counts <- sort(table(phys_table$orthogroup), decreasing = T)
    
    return(string_counts)
    
  }) %>% bindEvent(input$string_selectionI1)
  
  string_count_plot1 <- reactive({
    
    library(ggplot2)
    library(dplyr)
    
    data_count <- as.data.frame(string_counts1())
    colnames(data_count) <- c("orthogroup", "value")
    
    # Compute the position of labels
    data_count <- data_count %>%
      arrange(desc(orthogroup)) %>%
      mutate(prop = value / sum(data_count$value) *100) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop )
    
    # Create plot
    count_plot <- ggplot(data_count, aes(x="", y=prop, fill=orthogroup)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() +
      theme(legend.position="none") +
      #geom_text(aes(y = ypos, label = orthogroup), color = "white", size=6) +
      scale_fill_manual(values = rep(RColorBrewer::brewer.pal(n = 9, name = "Set1"), 
                                     floor(nrow(data_count)/9)+1))
    
    return(count_plot)
    
  }) %>% bindEvent(input$string_selectionI1)
  
  # Create boxes for outputs
  observeEvent(isTruthy(string_count_plot1()), {
    
    if (UI_exist_string1)
    {
      removeUI(
        selector = "div:has(>> #output_st_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #output_count_table1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>>> #count_plot1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadSTRINGTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #downloadCOUNTTable1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #count_download1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI(
        selector = "div:has(>> #selected_networkI1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      removeUI("#network_buttonI1")
      
    }
    
    insertUI("#box_st_table1", "afterEnd", ui = {
      box(width = 12,
          title = "STRING Interactions Table", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          dataTableOutput("output_st_table1")
      )
    })
    
    insertUI("#box_count_table1", "afterEnd", ui = {
      box(width = 12,
          title = "Interacting Orthogroups Table", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          dataTableOutput("output_count_table1")
      )
    })
    
    insertUI("#box_count_plot1", "afterEnd", ui = {
      box(width = 12,
          title = "Interacting Orthogroups Plot", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(column(1), imageOutput("count_plot1"))
      )
    })
    
    
    insertUI("#download_ui_for_st_table1", "afterEnd", ui = {
      tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadSTRINGTable1", "Download STRING Table",
                                                                         size = "sm", color = "primary"))
    })
    
    insertUI("#download_ui_for_count_table1", "afterEnd", ui = {
      tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadCOUNTTable1", "Download OG Count Table",
                                                                         size = "sm", color = "primary"))
    })
    
    insertUI("#count_down_button1", "afterEnd", ui = {
      tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "count_download1", "Download OG Count Plot",
                                                                         size = "sm", color = "primary"))
    })
    
    UI_exist_string1 <<- TRUE
    
    # Remove previous results for STRING network
    if (UI_exist_network1)
    {
      removeUI(
        selector = "div:has(>>>> #network_image1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
      UI_exist_network1 <<- F
    }
    
  })
  
  # Fill outputs
  # Render STRING table

  output$output_st_table1 <- renderDataTable({
    phys_table <- phys_table1()
    #phys_table$type <- sapply(phys_table$type, color_string)
    datatable(phys_table, escape=FALSE, rownames= F, options =list(pageLength = 10)) %>%
      formatStyle(
        'type',
        color = styleEqual(
          c("Direct interaction", "Interolog"), c('green', '#CA931B')
        )
      )
  }) 
  
  
  # Render OG count table
  output$output_count_table1 <- renderDataTable({
    string_counts <- as.data.frame(string_counts1())
    colnames(string_counts) <- c("orthogroup", "count")
    string_counts
  },escape=FALSE, rownames= F, options =list(pageLength = 7))
  
  # Render OG count pie chart
  output$count_plot1 <- renderImage({
    
    string_count_plot <- string_count_plot1()
    
    png("string_count_plot1.png", height = 450, width = 450)
    plot(string_count_plot)
    dev.off()
    
    list(src = "string_count_plot1.png",
         contentType="image/png", width=400,height=400)
  }, deleteFile = T)
  
  
  # Download tab's outputs
  # Download STRING table
  output$downloadSTRINGTable1 <- downloadHandler(
    filename= function() {
      paste("string_table", ".tsv", sep="")
    },
    content= function(file) {
      phys_table <- phys_table1()
      write.table(x = phys_table,quote = F,sep = "\t",
                  file=file,row.names=FALSE,col.names=TRUE)
    })
  
  # Download count table
  output$downloadCOUNTTable1 <- downloadHandler(
    filename= function() {
      paste("string_count_table", ".tsv", sep="")
    },
    content= function(file) {
      string_counts <- string_counts1()
      write.table(x = string_counts,quote = F,sep = "\t",
                  file=file,row.names=FALSE,col.names=TRUE)
    })
  
  # Download count plot
  output$count_download1 <- downloadHandler(
    filename= function() {
      paste("string_count", ".png", sep="")
    },
    content= function(file) {
      string_count_plot <- string_count_plot1()
      
      png(file, height = 450, width = 450)
      plot(string_count_plot)
      dev.off()
    })
  
  # Create gene selector and button for STRING network representation
  observeEvent(input$string_selectionI1,{
    
    phys_table <- phys_table1()
    network_genes <- unique(phys_table$prot_query)
    
    # Error message if no genes are allowed for selection should have been reported
    # earlier
    
    insertUI("#selected_network1", "afterEnd", ui = {
      
      shinyWidgets::pickerInput(inputId = "selected_networkI1", label = "Select the gene whose network you want to plot", 
                                choices = network_genes, selected = network_genes[1],
                                options = list(`actions-box` = TRUE), multiple = T)
      
    })
    
    
    insertUI("#network_button1", "afterEnd", ui = {
      
      shinyWidgets::actionBttn("network_buttonI1", "Plot STRING Network", size = "sm",
                               style = "float", color = "primary")
    })
    
    shinyjs::hideElement(id = 'loading.string1')
    
  })

  
  mapped_string1 <- reactive({
    
    # Load genes (PharaohFUN IDs) and convert to STRING IDs
    library(data.table)
    
    network_genes <- input$selected_networkI1
    map_table <- fread("pharaoh_folder/string_map.tsv")
    
    # Fast subset using data.table
    map_network <- map_table[pharaohfun_id %in% network_genes,]
    
    # Error if no genes from selection have an associated high fidelity STRING ID
    if (nrow(map_network) == 0)
    {
      
      if (UI_exist_network1)
      {
        removeUI(
          selector = "div:has(>>>> #network_image1)",
          multiple = TRUE,
          immediate = TRUE
        )
        
        UI_exist_network1 <<- F
      }
      
      output$error_network1 <- renderUI({renderText({print("It's not possible to map any
                               genes from selection to STRING IDs, please select different ones.")})})
      validate( " ")
      
    }
    
    output$error_network1 <- NULL
    
    # Get only STRING IDS and paste in a format interpretable by JS function
    string_ids <- as.data.frame(map_network)$string_id
    
    
    mapped_string <- paste0(string_ids, collapse = "%0d")
    return(mapped_string)
    
  }) %>% bindEvent(input$network_buttonI1)
  
  # Create boxes
  observeEvent(isTruthy(mapped_string1()),{
    
    mapped_string <- mapped_string1()
    
    if (UI_exist_network1)
    {
      removeUI(
        selector = "div:has(>>>> #network_image1)",
        multiple = TRUE,
        immediate = TRUE
      )
      
    }
    
    url_interactive <- paste0("https://string-db.org/cgi/network?identifiers=", 
                              mapped_string, 
                              "&add_color_nodes=25&network_flavor=confidence&show_query_node_labels=1")
    
    insertUI("#box_output_network1", "afterEnd", ui = {
      box(width = 12,
          title = "Image", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(column(1), 
                   column(8, htmlOutput("network_image1")), 
                   column(3, div( style = "margin-top: 300px;", 
                            shinyWidgets::actionBttn("network_link1", "Interactive Network", size = "md", 
                                                     icon = icon("circle-nodes"), style = "float", color = "primary", 
                                                        onclick=paste0("window.open('", url_interactive,"','_blank')")))
                          
                          ))
          
      )
    })
    
    UI_exist_network1 <<- T
    
  })
  
  # Fill network box
  
  output$network_image1 <- renderText({
    
    mapped_string <- mapped_string1()
    src_map <- paste0("https://string-db.org/api/image/network?identifiers=", mapped_string, 
                      "&add_color_nodes=25&network_flavor=confidence")
    
    c('<img src="',src_map,'"width="675" height="625">')
    })
  

# End of Gene ID-based search results
  
  
  
  ############### SEQUENCE-BASED SEARCH ####################
  
  UI_exist_pfam2 <<- F
  UI_exist_tree2 <<- F
  UI_exist_phylo2 <<- F
  UI_exist_cafe2 <<- F
  UI_exist_error_cafe2 <<- F
  UI_exist_msa2 <<- F
  UI_exist_go2 <<- F
  UI_exist_kegg2 <<- F
  UI_exist_pathview2 <<- F
  UI_exist_lit2 <<-  F
  UI_exist_string2 <<-  F
  UI_exist_network2 <<-  F
  
  # Define variables for input values, update only when Run button is clicked
  model.selected2 <- reactive({
    model.selected <- !input$switch2
    return(model.selected)
  })%>% bindEvent(input$run_button2)
  
  build_trees2 <- reactive({
    build_trees <- as.character(input$build_trees_2)
    return(build_trees)
  }) %>% bindEvent(input$run_button2)
  
  selected_organisms2 <- reactive({
    selected_organisms <- c(input$mami_check_2,input$chloro_check_2, input$strepto_check_2,
                            input$bryo_check_2, input$lyco_check_2, input$sperma_check_2)
    if(model.selected2()){selected_organisms <- c(input$tsar_check_2, input$rhodo_check_2, 
                                                  input$glauco_check_2,selected_organisms)}
    return(selected_organisms)
    
  }) %>% bindEvent(input$run_button2)
  
  selected_values_org2 <- reactive(organisms_values[selected_organisms2()]) %>% bindEvent(input$run_button2)

  
   seq_org2 <- reactive({
     
     seq_org <- input$organism_input_2
     
     return(seq_org)
     
   }) %>% bindEvent(input$run_button2)
    
   # Start results computation
   # First, search for the most similar protein in the proteome of the species
   # chosen by the user to identify the corresponding ID. After this, workflow is
   # the same as on the Gene ID-based search
   
   diamond_table2 <- reactive({
     
     shinyjs::showElement(id = 'loading.tree2')
     
     seq_org <- seq_org2()
     model.selected <- model.selected2()
     
     # Check if selected model contains selected proteome
     if (!model.selected && seq_org %in% c(column1, column2, column3))
     {
       shinyjs::hideElement(id = 'loading.tree2')
       
       if (UI_exist_tree2)
       {
         removeUI(
           selector = "div:has(>> #tree_seq_table2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #treeTips2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>>> #presentorg2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_image2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTree2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadNewick2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTreeSeqs2)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       
       UI_exist_tree2 <<- F
       output$error_tree2 <- renderUI({renderText({print("Global Model must be selected for the analysis
                                      of the chosen proteome.")})})
       validate(" ")
     }
     
     
     # Clean chain input
     library(seqinr)
     library(stringr)
     
     seq_comp <- as.character(input$geneInt2)
     seq_comp_clean <- toupper(gsub("[\r\n\t]", "", seq_comp))
     seq_comp_clean2 <- str_replace_all(seq_comp_clean, fixed(" "), "")
     vec_comp <- stringr::str_split_1(seq_comp_clean2, pattern = "")
     
     # Create fasta por comparison
     random_name <- "new_diamond"
     write.fasta(vec_comp, names = "query_prot", paste0("pharaoh_folder/", random_name, ".fa"))
     
     # Access species proteome
     org_search <- gsub(" ", "_", tolower(as.character(seq_org)))
     org_fasta <- paste("pharaohfun_proteomes", paste0(org_search, ".fa", sep=""), sep = "/")
     
     library(rdiamond)
     
     # Run diamond
     diamond_res <- rdiamond::diamond_protein_to_protein(
       query   = paste0("pharaoh_folder/", random_name, ".fa"),
       subject = org_fasta,
       sensitivity_mode = "sensitive",
       output_path = tempdir(),
       #db_import  = FALSE,
       ##diamond_exec_path = "/usr/bin", 
       max_target_seqs = 5)
     
     # Convert to data.frame
     diamond_table <- data.frame(ID=diamond_res$subject_id, Identity_perc=diamond_res$perc_identity,
                                 Num_identical_matches=diamond_res$num_ident_matches, 
                                 Length = diamond_res$alig_length, Mismatches = diamond_res$mismatches,
                                 Num_gaps=diamond_res$n_gaps, E_value=diamond_res$evalue)
     
     return(diamond_table)
     
     
   }) %>% bindEvent(input$run_button2)
  
   
   og.name2 <- reactive({
     
     diamond_table <- diamond_table2()
     gene.name.tree <- as.character(diamond_table$ID[1])
       
       
     # Load table with orthogroups information depending on selected model
     ortho.table.search <- ifelse(model.selected2(), "Global_Gene_Trees/Orthogroups.tsv",
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
       shinyjs::hideElement(id = 'loading.tree2')
       
       if (UI_exist_tree2)
       {
         removeUI(
           selector = "div:has(>> #tree_seq_table2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #treeTips2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>>> #presentorg2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_image2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTree2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadNewick2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTreeSeqs2)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       
       UI_exist_tree2 <<- F
       output$error_tree2 <- renderUI({renderText({print("No results for this 
      query due to not supported gene name or lack of orthologs in the selected organisms.")})})
       validate(need(!is.null(file.name), " "))
     }
     
     return(file.name)
     
   }) %>% bindEvent(input$run_button2)
   
   
   tree2 <- reactive({
     file.name <- og.name2()
     # Load gene tree file depending on the input
     
     tree.name <- ifelse(model.selected2(),
                         paste("Global_Gene_Trees",paste(file.name, "tree.txt", sep = "_"), sep="/"),
                         paste("Green_Gene_Trees",paste(file.name, "tree.txt", sep = "_"), sep="/"))
     
     # Error if tree file not found
     if (!(file.exists(tree.name)))
     {
       shinyjs::hideElement(id = 'loading.tree2')
       
       if (UI_exist_tree2)
       {
         removeUI(
           selector = "div:has(>> #tree_seq_table2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #treeTips2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>>> #presentorg2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_image2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTree2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadNewick2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTreeSeqs2)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       
       UI_exist_tree2 <<- F
       output$error_tree2 <- renderUI({renderText({print("Unable to construct tree associated to
                                                        to an orthogroup with less than 4 genes.")})})
       validate(need(file.exists(tree.name), " "))
     }
     
     # Generate tree depending on the tree building method selected
     {
       if (build_trees2() == "Maximum Likelihood")
       {
         tree <- read.tree(tree.name)
         return(tree)
       }
       
       else
       {
         library(phangorn)
         
         # Read MSA
         multiple_file <- ifelse(model.selected2(),
                                 paste("Global_MultipleSequenceAlignments", paste0(file.name, ".fa"), sep="/"),
                                 paste("Green_MultipleSequenceAlignments", paste0(file.name, ".fa"), sep="/"))
         
         
         mytree <- read.phyDat(file = multiple_file,
                               format="fasta", type = "AA")
         
         # To keep the same workflow, the reduced tree will be calculated, then the applied subsetting
         # will only reduce them if the selected method has been fasttree
         
         
         # Selection of genes from the selected organism
         
         # Create empty vectors
         
         tips_to_keep.mp2 = tips_to_keep.at2 = tips_to_keep.ot2 = tips_to_keep.cp2 <- c()
         tips_to_keep.cr2 = tips_to_keep.cz2 = tips_to_keep.kn2 = tips_to_keep.me2 <- c()
         tips_to_keep.mi2 = tips_to_keep.pp2 = tips_to_keep.sl2 = tips_to_keep.sm2 <- c()
         tips_to_keep.sp2 = tips_to_keep.ta2 = tips_to_keep.vc2 = tips_to_keep.bp2 <- c()
         tips_to_keep.cri2 = tips_to_keep.ds2 = tips_to_keep.os2 = tips_to_keep.smag2 <- c()
         tips_to_keep.tp2 = tips_to_keep.aa2 = tips_to_keep.um2 = tips_to_keep.rs2 <- c()
         tips_to_keep.cyc2 = tips_to_keep.pu2 = tips_to_keep.pt2 = tips_to_keep.ng2 <- c()
         tips_to_keep.cyano2 = tips_to_keep.ca2 = tips_to_keep.mv2 = tips_to_keep.af2 <- c()
         tips_to_keep.sc2 = tips_to_keep.aegi2 = tips_to_keep.sb2 = tips_to_keep.chara2 <- c()
         tips_to_keep.guilla2 = tips_to_keep.crypto2 = tips_to_keep.cymero2 = tips_to_keep.galsul2 <- c()
         tips_to_keep.gracichor2 = tips_to_keep.sceobli2 = tips_to_keep.cocco2 = tips_to_keep.saccha2 <- c()
         tips_to_keep.haema2 = tips_to_keep.zm2 <- c()
         
         
         organisms.list <- c(selected_values_org2())
         
         
         # Fill vectors if organisms are in list and change names
         {
           if ("mp" %in% organisms.list)
           {
             tips_to_keep.mp2 <- grep(pattern = "marchantia", names(mytree)) 
           }
           
           if ("ot" %in% organisms.list)
           {
             tips_to_keep.ot2 <- grep(pattern = "ostreoco",names(mytree)) 
           }
           
           if ("at" %in% organisms.list)
           {
             tips_to_keep.at2 <- grep(pattern = "arabidopsis",names(mytree)) 
           }
           
           if ("cp" %in% organisms.list)
           {
             tips_to_keep.cp2 <- grep(pattern = "ceratodon",names(mytree)) 
           }
           
           if ("cr" %in% organisms.list)
           {
             tips_to_keep.cr2 <- grep(pattern = "chlamy",names(mytree))
           }
           
           if ("cz" %in% organisms.list)
           {
             tips_to_keep.cz2 <- grep(pattern = "chromochloris",names(mytree)) 
           }
           
           if ("kn" %in% organisms.list)
           {
             tips_to_keep.kn2 <- grep(pattern = "klebsormidium",names(mytree)) 
           }
           
           if ("me" %in% organisms.list)
           {
             tips_to_keep.me2 <- grep(pattern = "mesotaenium",names(mytree)) 
           }
           
           if ("mi" %in% organisms.list)
           {
             tips_to_keep.mi2 <- grep(pattern = "micromonas",names(mytree)) 
           }
           
           if ("pp" %in% organisms.list)
           {
             tips_to_keep.pp2 <- grep(pattern = "physcomitrium",names(mytree)) 
           }
           
           if ("sl" %in% organisms.list)
           {
             tips_to_keep.sl2 <- grep(pattern = "solanum",names(mytree)) 
           }
           
           if ("sm" %in% organisms.list)
           {
             tips_to_keep.sm2 <- grep(pattern = "selaginella",names(mytree))
           }
           
           if ("sp" %in% organisms.list)
           {
             tips_to_keep.sp2 <- grep(pattern = "spirogloea",names(mytree)) 
           }
           
           if ("ta" %in% organisms.list)
           {
             tips_to_keep.ta2 <- grep(pattern = "triticum",names(mytree)) 
           }
           
           if ("vc" %in% organisms.list)
           {
             tips_to_keep.vc2 <- grep(pattern = "volvox",names(mytree))
           }
           
           if ("bp" %in% organisms.list)
           {
             tips_to_keep.bp2 <- grep(pattern = "bathycoccus",names(mytree))
           }
           
           if ("cri" %in% organisms.list)
           {
             tips_to_keep.cri2 <- grep(pattern = "ceratopteris",names(mytree))
           }
           
           if ("ds" %in% organisms.list)
           {
             tips_to_keep.ds2 <- grep(pattern = "dunaliella",names(mytree))
           }
           
           if ("os" %in% organisms.list)
           {
             tips_to_keep.os2 <- grep(pattern = "oryza",names(mytree))
           }
           
           if ("smag" %in% organisms.list)
           {
             tips_to_keep.smag2 <- grep(pattern = "sphagnum",names(mytree))
           }
           
           if ("tp" %in% organisms.list)
           {
             tips_to_keep.tp2 <- grep(pattern = "thuja",names(mytree))
           }
           
           if ("aa" %in% organisms.list)
           {
             tips_to_keep.aa2 <- grep(pattern = "anthoceros",names(mytree))
           }
           
           if ("um" %in% organisms.list)
           {
             tips_to_keep.um2 <- grep(pattern = "ulva",names(mytree))
           }
           
           if ("rs" %in% organisms.list)
           {
             tips_to_keep.rs2 <- grep(pattern = "raphidocelis",names(mytree))
           }
           
           if ("cyc" %in% organisms.list)
           {
             tips_to_keep.cyc2 <- grep(pattern = "cycas",names(mytree))
           }
           
           if ("pu" %in% organisms.list)
           {
             tips_to_keep.pu2 <- grep(pattern = "porphyra",names(mytree))
           }
           
           if ("pt" %in% organisms.list)
           {
             tips_to_keep.pt2 <- grep(pattern = "phaeodactylum",names(mytree))
           }
           
           if ("ng" %in% organisms.list)
           {
             tips_to_keep.ng2 <- grep(pattern = "gaditana",names(mytree))
           }
           
           if ("cyano" %in% organisms.list)
           {
             tips_to_keep.cyano2 <- grep(pattern = "cyanophora",names(mytree))
           }
           
           if ("ca" %in% organisms.list)
           {
             tips_to_keep.ca2 <- grep(pattern = "chlorokybus",names(mytree))
           }
           
           if ("mv" %in% organisms.list)
           {
             tips_to_keep.mv2 <- grep(pattern = "mesostigma",names(mytree))
           }
           
           if ("af" %in% organisms.list)
           {
             tips_to_keep.af2 <- grep(pattern = "azolla",names(mytree))
           }
           
           if ("sc" %in% organisms.list)
           {
             tips_to_keep.sc2 <- grep(pattern = "salvinia",names(mytree))
           }
           
           if ("aegi" %in% organisms.list)
           {
             tips_to_keep.aegi2 <- grep(pattern = "aegilops",names(mytree))
           }
           
           if ("sb" %in% organisms.list)
           {
             tips_to_keep.sb2 <- grep(pattern = "sorghum",names(mytree))
           }
           
           if ("chara" %in% organisms.list)
           {
             tips_to_keep.chara2 <- grep(pattern = "chara",names(mytree))
           }
           
           if ("guilla" %in% organisms.list)
           {
             tips_to_keep.guilla2 <- grep(pattern = "guillardia",names(mytree))
           }
           
           if ("crypto" %in% organisms.list)
           {
             tips_to_keep.crypto2 <- grep(pattern = "cryptophyceae",names(mytree))
           }
           
           if ("cymero" %in% organisms.list)
           {
             tips_to_keep.cymero2 <- grep(pattern = "cyanidioschyzon",names(mytree))
           }
           
           if ("galsul" %in% organisms.list)
           {
             tips_to_keep.galsul2 <- grep(pattern = "galdieria",names(mytree))
           }
           
           if ("gracichor" %in% organisms.list)
           {
             tips_to_keep.gracichor2 <- grep(pattern = "gracilariopsis",names(mytree))
           }
           
           if ("sceobli" %in% organisms.list)
           {
             tips_to_keep.sceobli2 <- grep(pattern = "scenedesmus",names(mytree))
           }
           
           if ("cocco" %in% organisms.list)
           {
             tips_to_keep.cocco2 <- grep(pattern = "coccomyxa",names(mytree))
           }
           
           if ("saccha" %in% organisms.list)
           {
             tips_to_keep.saccha2 <- grep(pattern = "saccharina",names(mytree))
           }
           
           if ("haema" %in% organisms.list)
           {
             tips_to_keep.haema2 <- grep(pattern = "haematococcus",names(mytree))
           }
           
           if ("zm" %in% organisms.list)
           {
             tips_to_keep.zm2 <- grep(pattern = "mays",names(mytree))
           }
         }
         
         
         # Concatenate indexes to keep and subset MSA
         tips_to_keep.global <- c(tips_to_keep.mp2, tips_to_keep.ot2, tips_to_keep.at2, tips_to_keep.cp2,
                                  tips_to_keep.cr2, tips_to_keep.cz2, tips_to_keep.kn2, tips_to_keep.me2,
                                  tips_to_keep.mi2, tips_to_keep.pp2, tips_to_keep.sl2, tips_to_keep.sm2,
                                  tips_to_keep.sp2, tips_to_keep.ta2, tips_to_keep.vc2, tips_to_keep.bp2,
                                  tips_to_keep.cri2, tips_to_keep.ds2, tips_to_keep.os2, tips_to_keep.smag2,
                                  tips_to_keep.tp2, tips_to_keep.aa2, tips_to_keep.um2, tips_to_keep.rs2,
                                  tips_to_keep.cyc2, tips_to_keep.pu2, tips_to_keep.pt2, tips_to_keep.ng2,
                                  tips_to_keep.cyano2, tips_to_keep.ca2, tips_to_keep.mv2, tips_to_keep.af2,
                                  tips_to_keep.sc2, tips_to_keep.aegi2, tips_to_keep.sb2, tips_to_keep.chara2,
                                  tips_to_keep.guilla2, tips_to_keep.crypto2, tips_to_keep.cymero2, tips_to_keep.galsul2,
                                  tips_to_keep.gracichor2, tips_to_keep.sceobli2, tips_to_keep.cocco2, tips_to_keep.saccha2,
                                  tips_to_keep.haema2,tips_to_keep.zm2)
         
         
         my_subset_tree <- subset(mytree, tips_to_keep.global)
         
         {
           if (build_trees2() == "Neighbour Joining")
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
           
           else if (build_trees2() == "UPGMA")
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
           
           else if (build_trees2() == "Parsimony")
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
   }) %>% bindEvent(input$run_button2)
   
   ortho_seq2 <- reactive({
     file.name <- og.name2()
     
     # Load orthogroup sequences file
     ortho.seq.name <- ifelse(model.selected2(),
                              paste("Global_Orthogroup_Sequences",paste(file.name, "fa", sep = "."), sep="/"),
                              paste("Green_Orthogroup_Sequences",paste(file.name, "fa", sep = "."), sep="/"))
     
     ortho_seq <- seqinr::read.fasta(ortho.seq.name, seqtype = "AA")
     return(ortho_seq)
   })
   
   
   # Tips to keep of each species with proper notation
   tips_to_keep.mp2 <- reactive({
     
     tree <- tree2()
     # Selection of organisms
     organisms.list <- c(selected_values_org2())
     
     # Selection of genes from the selected organism
     tips_to_keep.mp <- c()
     if ("mp" %in% organisms.list)
     {
       tips_to_keep.mp <- grep(pattern = "marchantia",tree$tip.label)
     }
     return(tips_to_keep.mp)
   })
   
   tips_to_keep.ot2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.ot <- c()
     if ("ot" %in% organisms.list)
     {
       tips_to_keep.ot <- grep(pattern = "ostreoco",tree$tip.label)
     }
     return(tips_to_keep.ot)
   })
   
   tips_to_keep.at2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.at <- c()
     if ("at" %in% organisms.list)
     {
       tips_to_keep.at <- grep(pattern = "arabidopsis",tree$tip.label)
     }
     return(tips_to_keep.at)
   })
   
   tips_to_keep.cp2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.cp <- c()
     if ("cp" %in% organisms.list)
     {
       tips_to_keep.cp <- grep(pattern = "ceratodon",tree$tip.label)
     }
     
     return(tips_to_keep.cp)
   })
   
   tips_to_keep.cr2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.cr <- c()
     if ("cr" %in% organisms.list)
     {
       tips_to_keep.cr <- grep(pattern = "chlamy",tree$tip.label)
     }
     return(tips_to_keep.cr)
   })
   
   tips_to_keep.cz2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.cz <- c()
     if ("cz" %in% organisms.list)
     {
       tips_to_keep.cz <- grep(pattern = "chromochloris",tree$tip.label)
     }
     return(tips_to_keep.cz)
   })
   
   tips_to_keep.kn2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.kn <- c()
     if ("kn" %in% organisms.list)
     {
       tips_to_keep.kn <- grep(pattern = "klebsormidium",tree$tip.label)
     }
     return(tips_to_keep.kn)
   })
   
   tips_to_keep.me2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.me <- c()
     if ("me" %in% organisms.list)
     {
       tips_to_keep.me <- grep(pattern = "mesotaenium",tree$tip.label)
     }
     return(tips_to_keep.me)
   })
   
   tips_to_keep.mi2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.mi <- c()
     if ("mi" %in% organisms.list)
     {
       tips_to_keep.mi <- grep(pattern = "micromonas",tree$tip.label)
     }
     return(tips_to_keep.mi)
   })
   
   tips_to_keep.pp2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.pp <- c()
     if ("pp" %in% organisms.list)
     {
       tips_to_keep.pp <- grep(pattern = "physcomitrium",tree$tip.label)
     }
     return(tips_to_keep.pp)
   })
   
   tips_to_keep.sl2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.sl <- c()
     if ("sl" %in% organisms.list)
     {
       tips_to_keep.sl <- grep(pattern = "solanum",tree$tip.label)
     }
     return(tips_to_keep.sl)
   })
   
   tips_to_keep.sm2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.sm <- c()
     if ("sm" %in% organisms.list)
     {
       tips_to_keep.sm <- grep(pattern = "selaginella",tree$tip.label)
     }
     return(tips_to_keep.sm)
   })
   
   tips_to_keep.sp2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.sp <- c()
     if ("sp" %in% organisms.list)
     {
       tips_to_keep.sp <- grep(pattern = "spirogloea",tree$tip.label)
     }
     return(tips_to_keep.sp)
   })
   
   tips_to_keep.ta2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.ta <- c()
     if ("ta" %in% organisms.list)
     {
       tips_to_keep.ta <- grep(pattern = "triticum",tree$tip.label)
     }
     return(tips_to_keep.ta)
   })
   
   tips_to_keep.vc2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.vc <- c()
     if ("vc" %in% organisms.list)
     {
       tips_to_keep.vc <- grep(pattern = "volvox",tree$tip.label)
     }
     return(tips_to_keep.vc)
   })
   
   tips_to_keep.bp2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.bp <- c()
     if ("bp" %in% organisms.list)
     {
       tips_to_keep.bp <- grep(pattern = "bathycoccus",tree$tip.label)
     }
     return(tips_to_keep.bp)
   })
   
   tips_to_keep.cri2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.cri <- c()
     if ("cri" %in% organisms.list)
     {
       tips_to_keep.cri <- grep(pattern = "ceratopteris",tree$tip.label)
     }
     return(tips_to_keep.cri)
   })
   
   tips_to_keep.ds2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.ds <- c()
     if ("ds" %in% organisms.list)
     {
       tips_to_keep.ds <- grep(pattern = "dunaliella",tree$tip.label)
     }
     return(tips_to_keep.ds)
   })
   
   tips_to_keep.os2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.os <- c()
     if ("os" %in% organisms.list)
     {
       tips_to_keep.os <- grep(pattern = "oryza",tree$tip.label)
     }
     return(tips_to_keep.os)
   })
   
   tips_to_keep.smag2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.smag <- c()
     if ("smag" %in% organisms.list)
     {
       tips_to_keep.smag <- grep(pattern = "sphagnum",tree$tip.label)
     }
     return(tips_to_keep.smag)
   })
   
   tips_to_keep.tp2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.tp <- c()
     if ("tp" %in% organisms.list)
     {
       tips_to_keep.tp <- grep(pattern = "thuja",tree$tip.label)
     }
     return(tips_to_keep.tp)
   })
   
   tips_to_keep.aa2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.aa <- c()
     if ("aa" %in% organisms.list)
     {
       tips_to_keep.aa <- grep(pattern = "anthoceros",tree$tip.label)
     }
     return(tips_to_keep.aa)
   })
   
   tips_to_keep.um2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.um <- c()
     if ("um" %in% organisms.list)
     {
       tips_to_keep.um <- grep(pattern = "ulva",tree$tip.label)
     }
     return(tips_to_keep.um)
   })
   
   tips_to_keep.rs2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.rs <- c()
     if ("rs" %in% organisms.list)
     {
       tips_to_keep.rs <- grep(pattern = "raphidocelis",tree$tip.label)
     }
     return(tips_to_keep.rs)
   })
   
   tips_to_keep.cyc2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.cyc <- c()
     if ("cyc" %in% organisms.list)
     {
       tips_to_keep.cyc <- grep(pattern = "cycas",tree$tip.label)
     }
     return(tips_to_keep.cyc)
   })
   
   tips_to_keep.pu2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.pu <- c()
     if ("pu" %in% organisms.list)
     {
       tips_to_keep.pu <- grep(pattern = "porphyra",tree$tip.label)
     }
     return(tips_to_keep.pu)
   })
   
   tips_to_keep.pt2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.pt <- c()
     if ("pt" %in% organisms.list)
     {
       tips_to_keep.pt <- grep(pattern = "phaeodactylum",tree$tip.label)
     }
     return(tips_to_keep.pt)
   })
   
   tips_to_keep.ng2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.ng <- c()
     if ("ng" %in% organisms.list)
     {
       tips_to_keep.ng <- grep(pattern = "gaditana",tree$tip.label)
     }
     return(tips_to_keep.ng)
   })
   
   tips_to_keep.cyano2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.cyano <- c()
     if ("cyano" %in% organisms.list)
     {
       tips_to_keep.cyano <- grep(pattern = "cyanophora",tree$tip.label)
     }
     return(tips_to_keep.cyano)
   })
   
   tips_to_keep.ca2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.ca <- c()
     if ("ca" %in% organisms.list)
     {
       tips_to_keep.ca <- grep(pattern = "chlorokybus",tree$tip.label)
     }
     return(tips_to_keep.ca)
   })
   
   tips_to_keep.mv2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.mv <- c()
     if ("mv" %in% organisms.list)
     {
       tips_to_keep.mv <- grep(pattern = "mesostigma",tree$tip.label)
     }
     return(tips_to_keep.mv)
   })
   
   tips_to_keep.af2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.af <- c()
     if ("af" %in% organisms.list)
     {
       tips_to_keep.af <- grep(pattern = "azolla",tree$tip.label)
     }
     return(tips_to_keep.af)
   })
   
   tips_to_keep.sc2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.sc <- c()
     if ("sc" %in% organisms.list)
     {
       tips_to_keep.sc <- grep(pattern = "salvinia",tree$tip.label)
     }
     return(tips_to_keep.sc)
   })
   
   tips_to_keep.aegi2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.aegi <- c()
     if ("aegi" %in% organisms.list)
     {
       tips_to_keep.aegi <- grep(pattern = "aegilops",tree$tip.label)
     }
     return(tips_to_keep.aegi)
   })
   
   tips_to_keep.sb2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.sb <- c()
     if ("sb" %in% organisms.list)
     {
       tips_to_keep.sb <- grep(pattern = "sorghum",tree$tip.label)
     }
     return(tips_to_keep.sb)
   })
   
   tips_to_keep.chara2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.chara <- c()
     if ("chara" %in% organisms.list)
     {
       tips_to_keep.chara <- grep(pattern = "chara",tree$tip.label)
     }
     return(tips_to_keep.chara)
   })
   
   tips_to_keep.guilla2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.guilla <- c()
     if ("guilla" %in% organisms.list)
     {
       tips_to_keep.guilla <- grep(pattern = "guillardia",tree$tip.label)
     }
     return(tips_to_keep.guilla)
   })
   
   tips_to_keep.crypto2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.crypto <- c()
     if ("crypto" %in% organisms.list)
     {
       tips_to_keep.crypto <- grep(pattern = "cryptophyceae",tree$tip.label)
     }
     return(tips_to_keep.crypto)
   })
   
   tips_to_keep.cymero2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.cymero <- c()
     if ("cymero" %in% organisms.list)
     {
       tips_to_keep.cymero <- grep(pattern = "cyanidioschyzon",tree$tip.label)
     }
     return(tips_to_keep.cymero)
   })
   
   tips_to_keep.galsul2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.galsul <- c()
     if ("galsul" %in% organisms.list)
     {
       tips_to_keep.galsul <- grep(pattern = "galdieria",tree$tip.label)
     }
     return(tips_to_keep.galsul)
   })
   
   tips_to_keep.gracichor2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.gracichor <- c()
     if ("gracichor" %in% organisms.list)
     {
       tips_to_keep.gracichor <- grep(pattern = "gracilariopsis",tree$tip.label)
     }
     return(tips_to_keep.gracichor)
   })
   
   tips_to_keep.sceobli2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.sceobli <- c()
     if ("sceobli" %in% organisms.list)
     {
       tips_to_keep.sceobli <- grep(pattern = "scenedesmus",tree$tip.label)
     }
     return(tips_to_keep.sceobli)
   })
   
   tips_to_keep.cocco2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.cocco <- c()
     if ("cocco" %in% organisms.list)
     {
       tips_to_keep.cocco <- grep(pattern = "coccomyxa",tree$tip.label)
     }
     return(tips_to_keep.cocco)
   })
   
   tips_to_keep.saccha2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.saccha <- c()
     if ("saccha" %in% organisms.list)
     {
       tips_to_keep.saccha <- grep(pattern = "saccharina",tree$tip.label)
     }
     return(tips_to_keep.saccha)
   })
   
   tips_to_keep.haema2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.haema <- c()
     if ("haema" %in% organisms.list)
     {
       tips_to_keep.haema <- grep(pattern = "haematococcus",tree$tip.label)
     }
     return(tips_to_keep.haema)
   })
   
   tips_to_keep.zm2 <- reactive({
     
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     tips_to_keep.zm <- c()
     if ("zm" %in% organisms.list)
     {
       tips_to_keep.zm <- grep(pattern = "mays",tree$tip.label)
     }
     return(tips_to_keep.zm)
   }) %>% bindEvent(input$run_button2)
   
   
   # Create complete gene tree with the proper name for each gene
   # For this, we split the species name apart from the gene name
   tree_adj2 <- reactive({
     tree <- tree2()
     organisms.list <- c(selected_values_org2())
     
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
   }) %>% bindEvent(input$run_button2)
   
   # Generate reduced tree when the corresponding button is activated
   tree_reduced2 <- reactive({
     
     tree <- tree_adj2()
     # Define tips to keep (selected organisms) and generate the reduced tree
     tips_to_keep.global <- c(tips_to_keep.mp2(), tips_to_keep.ot2(), tips_to_keep.at2(), tips_to_keep.cp2(),
                              tips_to_keep.cr2(), tips_to_keep.cz2(), tips_to_keep.kn2(), tips_to_keep.me2(),
                              tips_to_keep.mi2(), tips_to_keep.pp2(), tips_to_keep.sl2(), tips_to_keep.sm2(),
                              tips_to_keep.sp2(), tips_to_keep.ta2(), tips_to_keep.vc2(), tips_to_keep.bp2(),
                              tips_to_keep.cri2(), tips_to_keep.ds2(), tips_to_keep.os2(), tips_to_keep.smag2(),
                              tips_to_keep.tp2(), tips_to_keep.aa2(), tips_to_keep.um2(), tips_to_keep.rs2(),
                              tips_to_keep.cyc2(), tips_to_keep.pu2(), tips_to_keep.pt2(), tips_to_keep.ng2(),
                              tips_to_keep.cyano2(), tips_to_keep.ca2(), tips_to_keep.mv2(), tips_to_keep.af2(),
                              tips_to_keep.sc2(), tips_to_keep.aegi2(), tips_to_keep.sb2(), tips_to_keep.chara2(),
                              tips_to_keep.guilla2(), tips_to_keep.crypto2(), tips_to_keep.cymero2(), tips_to_keep.galsul2(),
                              tips_to_keep.gracichor2(), tips_to_keep.sceobli2(), tips_to_keep.cocco2(), tips_to_keep.saccha2(),
                              tips_to_keep.haema2(),tips_to_keep.zm2())
     
     # Error message if trying to build tree with less than two tips
     if (length(tips_to_keep.global) < 2)
     {
       shinyjs::hideElement(id = 'loading.tree2')
       
       if (UI_exist_tree2)
       {
         removeUI(
           selector = "div:has(>> #tree_seq_table2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #treeTips2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>>> #presentorg2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_image2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTree2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadNewick2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTreeSeqs2)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       
       UI_exist_tree2 <<- F
       output$error_tree2 <- renderUI({renderText({print("Unable to construct 
      tree with a single tip, please select more organisms.")})})
       validate(need(length(tips_to_keep.global) > 1, " "))
     }
     
     
     # Error message if query gene does not belong to selected organisms due to
     # not having selected chosen proteome in organisms list
     diamond_table <- diamond_table2()
     
     if (!(as.character(diamond_table$ID[1]) %in% tree$tip.label))
     {
       shinyjs::hideElement(id = 'loading.tree2')
       
       if (UI_exist_tree2)
       {
         removeUI(
           selector = "div:has(>> #tree_seq_table2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #treeTips2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>>> #presentorg2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_image2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTree2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadNewick2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTreeSeqs2)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       
       UI_exist_tree2 <<- F
       output$error_tree2 <- renderUI({renderText({print("Please select the species corresponding
      to the query gene. Read the instructions for clarification.")})})
       validate(need(as.character(diamond_table$ID[1]) %in% tree$tip.label, " "))
     }
     
     
     tips_to_drop <- setdiff(1:length(tree$tip.label), tips_to_keep.global)
     tree_reduced <- drop.tip(tree, tips_to_drop)
     
     return(tree_reduced)
   }) %>% bindEvent(input$run_button2)
   
   ### Select orthogroup sequences based on the reduced tree
   ortho_reduced2 <- reactive({
     
     tree_reduced <- tree_reduced2()
     ortho_seq <- ortho_seq2()
     ortho_reduced <- ortho_seq[tree_reduced$tip.label]
     return(ortho_reduced)
   }) %>% bindEvent(input$run_button2)
   
   ### Select orthogroup sequences based on the reduced tree
   organims_reduced2 <- reactive({
     
     tree_reduced <- tree_reduced2()
     
     len.mp <- length(tips_to_keep.mp2())
     len.ot <- length(tips_to_keep.ot2())
     len.at <- length(tips_to_keep.at2())
     len.cp <- length(tips_to_keep.cp2())
     len.cr <- length(tips_to_keep.cr2())
     len.cz <- length(tips_to_keep.cz2())
     len.kn <- length(tips_to_keep.kn2())
     len.me <- length(tips_to_keep.me2())
     len.mi <- length(tips_to_keep.mi2())
     len.pp <- length(tips_to_keep.pp2())
     len.sl <- length(tips_to_keep.sl2())
     len.sm <- length(tips_to_keep.sm2())
     len.sp <- length(tips_to_keep.sp2())
     len.ta <- length(tips_to_keep.ta2())
     len.vc <- length(tips_to_keep.vc2())
     len.bp <- length(tips_to_keep.bp2())
     len.cri <- length(tips_to_keep.cri2())
     len.ds <- length(tips_to_keep.ds2())
     len.os <- length(tips_to_keep.os2())
     len.smag <- length(tips_to_keep.smag2())
     len.tp <- length(tips_to_keep.tp2())
     len.aa <- length(tips_to_keep.aa2())
     len.um <- length(tips_to_keep.um2())
     len.rs <- length(tips_to_keep.rs2())
     len.cyc <- length(tips_to_keep.cyc2())
     len.pu <- length(tips_to_keep.pu2())
     len.pt <- length(tips_to_keep.pt2())
     len.ng <- length(tips_to_keep.ng2())
     len.cyano <- length(tips_to_keep.cyano2())
     len.ca <- length(tips_to_keep.ca2())
     len.mv <- length(tips_to_keep.mv2())
     len.af <- length(tips_to_keep.af2())
     len.sc <- length(tips_to_keep.sc2())
     len.aegi <- length(tips_to_keep.aegi2())
     len.sb <- length(tips_to_keep.sb2())
     len.chara <- length(tips_to_keep.chara2())
     len.guilla <- length(tips_to_keep.guilla2())
     len.crypto <- length(tips_to_keep.crypto2())
     len.cymero <- length(tips_to_keep.cymero2())
     len.galsul <- length(tips_to_keep.galsul2())
     len.gracichor <- length(tips_to_keep.gracichor2())
     len.sceobli <- length(tips_to_keep.sceobli2())
     len.cocco <- length(tips_to_keep.cocco2())
     len.saccha <- length(tips_to_keep.saccha2())
     len.haema <- length(tips_to_keep.haema2())
     len.zea <- length(tips_to_keep.zm2())
     
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
   }) %>% bindEvent(input$run_button2)

   # Create plot of subset tree
   tree_plot2 <- reactive({
     
     # Define previous variables
     tree_reduced <- tree_reduced2()
     diamond_table <- diamond_table2()
     gene.name.tree <- as.character(diamond_table$ID[1])
     tree <- tree_adj2()
     
     tips_to_keep.mp <- tips_to_keep.mp2()
     tips_to_keep.ot <- tips_to_keep.ot2()
     tips_to_keep.at <- tips_to_keep.at2()
     tips_to_keep.cp <- tips_to_keep.cp2()
     tips_to_keep.cr <- tips_to_keep.cr2()
     tips_to_keep.cz <- tips_to_keep.cz2()
     tips_to_keep.kn <- tips_to_keep.kn2()
     tips_to_keep.me <- tips_to_keep.me2()
     tips_to_keep.mi <- tips_to_keep.mi2()
     tips_to_keep.pp <- tips_to_keep.pp2()
     tips_to_keep.sl <- tips_to_keep.sl2()
     tips_to_keep.sm <- tips_to_keep.sm2()
     tips_to_keep.sp <- tips_to_keep.sp2()
     tips_to_keep.ta <- tips_to_keep.ta2()
     tips_to_keep.vc <- tips_to_keep.vc2()
     tips_to_keep.bp <- tips_to_keep.bp2()
     tips_to_keep.cri <- tips_to_keep.cri2()
     tips_to_keep.ds <- tips_to_keep.ds2()
     tips_to_keep.os <- tips_to_keep.os2()
     tips_to_keep.smag <- tips_to_keep.smag2()
     tips_to_keep.tp <- tips_to_keep.tp2()
     tips_to_keep.aa <- tips_to_keep.aa2()
     tips_to_keep.um <- tips_to_keep.um2()
     tips_to_keep.rs <- tips_to_keep.rs2()
     tips_to_keep.cyc <- tips_to_keep.cyc2()
     tips_to_keep.pu <- tips_to_keep.pu2()
     tips_to_keep.pt <- tips_to_keep.pt2()
     tips_to_keep.ng <- tips_to_keep.ng2()
     tips_to_keep.cyano <- tips_to_keep.cyano2()
     tips_to_keep.ca <- tips_to_keep.ca2()
     tips_to_keep.mv <- tips_to_keep.mv2()
     tips_to_keep.af <- tips_to_keep.af2()
     tips_to_keep.sc <- tips_to_keep.sc2()
     tips_to_keep.aegi <- tips_to_keep.aegi2()
     tips_to_keep.sb <- tips_to_keep.sb2()
     tips_to_keep.chara <- tips_to_keep.chara2()
     tips_to_keep.guilla <- tips_to_keep.guilla2()
     tips_to_keep.crypto <- tips_to_keep.crypto2()
     tips_to_keep.cymero <- tips_to_keep.cymero2()
     tips_to_keep.galsul <- tips_to_keep.galsul2()
     tips_to_keep.gracichor <- tips_to_keep.gracichor2()
     tips_to_keep.sceobli <- tips_to_keep.sceobli2()
     tips_to_keep.cocco <- tips_to_keep.cocco2()
     tips_to_keep.saccha <- tips_to_keep.saccha2()
     tips_to_keep.haema <- tips_to_keep.haema2()
     tips_to_keep.zm <- tips_to_keep.zm2()
     
     if (length(tree_reduced$tip.label) < 2)
     {
       cat("")
     }
     else 
     {
       # Highlight the target gene
       high.gene2 <<- tree_reduced$tip.label[grep(pattern = gene.name.tree, tree_reduced$tip.label)]
       
       
       # Color asignment per species
       col.factor <- c()
       org.factor <- c()
       
       library(glue)
       library(ggtree)
       library(ggplot2)
       
       for (i in 1:length(tree_reduced$tip.label))
       {
         if (tree_reduced$tip.label[i] %in% high.gene2)
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
         if (build_trees2() == "Maximum Likelihood")
         {
           tree_plot <- ggtree(tree_reduced) %<+% d2 + geom_tiplab() + theme(legend.position =) +
             xlim(0, max(tree_reduced$edge.length)*3) + geom_tiplab(aes(label = label, color = organism)) +
             scale_color_manual(values = unique(d2$col), breaks = unique(d2$org)) +
             geom_highlight(mapping=aes(subset = label %in% high.gene2,
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
             geom_highlight(mapping=aes(subset = label %in% high.gene2,
                                        node = node,
                                        fill = as.factor(node)), extend = 0.8) + 
             labs(fill = "Node of interest")
         }
       }
       shinyjs::hideElement(id = 'loading.tree2')
       return(tree_plot)
     }}) %>% bindEvent(input$run_button2)
   
   
   # Outputs
   observeEvent(isTruthy(tree_plot2()), {
     output$error_tree2 <- NULL
   })
   
   # Create boxes
   observeEvent(isTruthy(tree_plot2()), {
     
     if (UI_exist_tree2)
     {
       removeUI(
         selector = "div:has(>> #tree_seq_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #treeTips2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>>> #presentorg2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #tree_image2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadTree2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadNewick2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadTreeSeqs2)",
         multiple = TRUE,
         immediate = TRUE
       )
     }
     
     insertUI("#box_tree_seq_table2", "afterEnd", ui = {
       box(
         title = "Sequence Hits", status = "success", solidHeader = TRUE, width = 12,
         collapsible = TRUE,
         dataTableOutput(outputId = "tree_seq_table2")
         )
     }) 
     
     insertUI("#box_tree_text2", "afterEnd", ui = {
       box(
         title = "Genes in Orthogroup", status = "success", solidHeader = TRUE, width = 12,
         collapsible = TRUE,
         verbatimTextOutput("treeTips2")
       )
     }) 
     
     insertUI("#box_tree_pie2", "afterEnd", ui = {
       box(
         title = "Present Organisms", status = "success", solidHeader = TRUE,
         collapsible = TRUE, width = 12,
         fluidRow(column(1), imageOutput("presentorg2"))
       )
     }) 
     
     insertUI("#box_tree_plot2", "afterEnd", ui = {
       box(width = 12,
           title = "Gene Tree", status = "success", solidHeader = TRUE,
           collapsible = TRUE, 
           plotOutput("tree_image2", height = 500)
       )
     })
     
     insertUI("#download_tree2", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadTree2", 
                                                                          "Download Tree Plot",
                                                                          size = "sm", color = "success"))
     })
     
     insertUI("#download_newick2", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadNewick2", 
                                                                          "Download NEWICK Tree",
                                                                          size = "sm", color = "success"))
     })
     
     insertUI("#download_tree_seqs2", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadTreeSeqs2", 
                                                                          "Download Protein Sequences",
                                                                          size = "sm", color = "success"))
     })
     
     UI_exist_tree2 <<- TRUE
     shinyjs::hideElement(id = 'loading.tree2')
   })
   
   # Fill boxes with output
   # DIAMOND Table
   output$tree_seq_table2 <- renderDataTable({
     diamond_table <- diamond_table2()
     return(diamond_table)
   }, escape=FALSE, rownames= F, options = list(pageLength = 5))
   
   # Gene IDS
   output$treeTips2 <- renderPrint({
     print(tree_reduced2()$tip.label)
   }, width = 400) # %>% bindEvent(input$run_button2)
   
   # Render pie chart
   output$presentorg2 <- renderImage({
     
     {library(ggplot2)
       library(dplyr)
       
       data <- data.frame(table(organims_reduced2()))
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
         #scale_fill_brewer(palette="Set1")
         scale_fill_manual(values = rep(RColorBrewer::brewer.pal(n = 9, name = "Set1"), 
                                        floor(nrow(data)/9)+1))}
     
     png("organims_reduced2.png", height = 450, width = 450)
     plot(a)
     dev.off()
     
     list(src = "organims_reduced2.png",
          contentType="image/png", width=400,height=400)
   }, deleteFile = T)
   
   # Render tree image
   output$tree_image2 <- renderImage({
     png("tree2.png", height = 500, width = 1000)
     plot(tree_plot2())
     dev.off()
     
     list(src = "tree2.png",
          contentType="image/png", width=1000,height=500)
   }, deleteFile = T)
   
   # Download results
   output$downloadTree2 <- downloadHandler(
     filename= function() {
       paste("tree", ".png", sep="")
     },
     content= function(file) {
       image_height <- 300 + 11*length(tree_reduced2()$tip.label)
       image_width <- 200 + 400*max(tree_reduced2()$edge.length)
       png(file, height = image_height, width = image_width)
       plot(tree_plot2())
       dev.off()
     })
   
   # Create and download tree in newick format
   output$downloadNewick2 <- downloadHandler(
     filename= function() {
       paste("tree_newick", ".txt", sep="")
     },
     content= function(file) {
       write.tree(tree_reduced2(), file)
     })
   
   #  # Create and download sequences for genes in tree
   output$downloadTreeSeqs2 <- downloadHandler(
     filename= function() {
       paste("tree_seqs", ".fa", sep="")
     },
     content= function(file) {
       seqinr::write.fasta(sequences = seqinr::getSequence(ortho_reduced2()),
                           names = seqinr::getName(ortho_reduced2()), file.out = file)
     })

   
   ####################### PHYLOWIDGET ############################
   # Remove previous outputs when updated by a new search
   observeEvent(input$run_button2, {
     if (UI_exist_phylo2)
     {
       removeUI(
         selector = "div:has(>>> #phylo_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_phylo2 <<- F
     }
     
   })
   
   phylo_tree2 <- reactive({
     
     library(ape)
     tree_phylo <- tree_reduced2()
     
     # Normalize tree depth
     root_id <- length(tree_phylo$tip.label)+1
     norm_factor <- max(dist.nodes(tree_phylo)[root_id,])
     tree_phylo$edge.length <- tree_phylo$edge.length/norm_factor
     
     return(tree_phylo)
     
   }) %>% bindEvent(input$phylo_start2)
   
   observeEvent(isTruthy(phylo_tree2()),{
     
     if(UI_exist_phylo2)
     {
       removeUI(
         selector = "div:has(>>> #phylo_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_phylo2 <<- F
     }
     
     
     insertUI("#box_phylo2", "afterEnd", ui = {
       phylo_tree <- phylo_tree2()
       phylo_height <- length(phylo_tree$tip.label) *14 + 220
       box(width = 12,
           title = "Interactive Tree", status = "success", solidHeader = TRUE, height = phylo_height + 100,
           collapsible = TRUE,
           tags$div(id = "phylo_pocket2", style = paste0("width: 1300px; height: ",  phylo_height + 50, "px"),
                    phylowidgetOutput("phylo_plot2", height = paste0(phylo_height,"px"), width = "98%"))
       )
     })
     
     UI_exist_phylo2 <<- T
     
   })
   
   output$phylo_plot2 <- renderPhylowidget({
     
     phylo_tree <- phylo_tree2()
     phylowidget(phylo_tree)
   })
   
   #########################  PFAM  ###############################
   
   observeEvent(input$run_button2, {
     removeUI(
       selector = "div:has(>> #selected_pfamsI2)",
       multiple = TRUE,
       immediate = TRUE
     )
   })
   
   observeEvent(input$pfam_start2, {
     insertUI("#selected_pfams2", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_pfamsI2","Select the desired genes from the tree",
                                 choices=isolate({tree_reduced2()$tip.label}), options = list(`actions-box` = TRUE),
                                 multiple = T, selected = isolate({diamond_table2()$ID[1]}))
     })
   })
   
   observeEvent(input$run_button2, {
     removeUI("#pfam_selection2")
   })
   
   observeEvent(input$pfam_start2, {
     insertUI("#pfam_selectionI2", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("pfam_selection2", "Show Pfam Domains", size = "sm",
                                style = "float", color = "success")
     })
   })
   
   
   observeEvent(input$run_button2, {
     if (UI_exist_pfam2)
     {
       removeUI(
         selector = "div:has(>> #output_pfam_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_download2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadPFAMTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_pfam2 <<- F
     }
   })
   
   
   total_table_pfam2 <- reactive({
     shinyjs::showElement(id = 'loading.pfam.pf2')
     ortho_reduced <- ortho_reduced2()
     sel_genes <- as.vector(input$selected_pfamsI2)
     
     if (length(sel_genes) < 1)
     {
       shinyjs::hideElement(id = 'loading.pfam.pf2')
       removeUI(
         selector = "div:has(>> #output_pfam_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_download2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadPFAMTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       UI_exist_pfam2 <<- F
       output$error_pfam2 <- renderUI({renderText({print("Please select at least one gene.")})})
       validate(need(length(sel_genes) > 0, "Please select at least one gene."))
     }
     
     output$error_pfam2 <- NULL
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
             shinyjs::hideElement(id = 'loading.pfam.pf2')
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
     
   }) %>% bindEvent(input$pfam_selection2)
   
   pfplot2 <- reactive({
     
     total_table_pfam <- total_table_pfam2()
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
     
   }) %>% bindEvent(input$pfam_selection2)
   
   
   # Outputs
   
   observeEvent(isTruthy(pfplot2()), {
     
     if (UI_exist_pfam2)
     {
       removeUI(
         selector = "div:has(>> #output_pfam_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_download2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadPFAMTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_pfam2", "afterEnd", ui = {
       
       box(
         title = "PFAM Table", status = "success", solidHeader = TRUE, width = 12,
         collapsible = TRUE,
         dataTableOutput(outputId = "output_pfam_table2"))
     }) 
     
     insertUI("#box_pfplot2", "afterEnd", ui = {
       total_table_pfam <- total_table_pfam2()
       box_pfplot_height <- 150 + 700*length(total_table_pfam$order[nrow(total_table_pfam)])
       box(
         title = "Domains Localization", status = "success", solidHeader = TRUE, width = 12, #height = box_pfplot_height,
         collapsible = TRUE,
         plotOutput("pfam_plot2", height = box_pfplot_height))
     }) 
     
     insertUI("#pfam_down_button2", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "pfam_download2", "Download PFAM figure",
                                                                          size = "sm", color = "success"))
     })
     
     insertUI("#download_ui_for_pfam_table2", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadPFAMTable2", "Download PFAM Table",
                                                                          size = "sm", color = "success"))
     })
     
     UI_exist_pfam2 <<- TRUE
     shinyjs::hideElement(id = 'loading.pfam.pf2')
   })
   
   output$output_pfam_table2 <- renderDataTable({
     total_table_pfam <- total_table_pfam2()
     out_pf_table <- subset(total_table_pfam[,c(1:6,8)], total_table_pfam$type != "CHAIN")
     out_pf_table$description <- sapply(out_pf_table$description, function(x) pfam.link(x))
     colnames(out_pf_table) <- c(colnames(total_table_pfam)[1:6],"biological description")
     return(out_pf_table)
   }, escape=FALSE, options = list(pageLength = 5))
   
   output$pfam_plot2 <- renderImage({
     total_table_pfam <- total_table_pfam2()
     pfam_height <- 50 + 700*length(total_table_pfam$order[nrow(total_table_pfam)])
     pfam_width <- 1000
     pfplot <- pfplot2()
     png("pharaoh_folder/pfam.png",  width = pfam_width, height = pfam_height)
     plot(pfplot)
     dev.off()
     list(src = "pharaoh_folder/pfam.png",
          contentType="image/png")
   }, deleteFile = T
   )
   
   # Download results
   
   output$pfam_download2 <- downloadHandler(
     filename= function() {
       paste("pfam", ".png", sep="")
     },
     content= function(file) {
       total_table_pfam <- total_table_pfam2()
       pfam_height <- 50 + 700*length(total_table_pfam$order[nrow(total_table_pfam)])
       pfam_width <- 1150
       pfplot <- pfplot2()
       
       png(file, height = pfam_height, width = pfam_width)
       plot(pfplot)
       dev.off()
     })
   
   output$downloadPFAMTable2<- downloadHandler(
     filename= function() {
       paste("pfam_table", ".tsv", sep="")
     },
     content= function(file) {
       total_table_pfam <- total_table_pfam2()
       out_pf_table <- subset(total_table_pfam[,c(1:6,8)], total_table_pfam$type != "CHAIN")
       colnames(out_pf_table) <- c(colnames(total_table_pfam)[1:6],"biological description")
       write.table(x = out_pf_table, quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
  
   ####################### CAFE #################################
   
   # Remove previous outputs when updated by a new search
   observeEvent(input$run_button2, {
     if (UI_exist_cafe2)
     {
       removeUI(
         selector = "div:has(>> #cafe_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #cafe_mrca2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #cafe_download2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadCAFEPlot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_cafe2 <<- F
     }
     
     if (UI_exist_error_cafe2)
     {
       removeUI(
         selector = "div:has(>> #cafe_error_message2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_error_cafe2 <<- F
     }
   })
   
   ### CAFE parser and tree generator
   cafe_tree2 <- reactive({
     
     shinyjs::showElement(id = 'loading.cafe2')
     
     library(ape)
     
     # Import OG name
     og.cafe <- og.name2()
     
     # Define path to CAFE trees file
     cafe_comp_tree_file <- ifelse(model.selected2(), "pharaoh_folder/global_cafe.tre",
                                   "pharaoh_folder/green_cafe.tre")
     
     # Extract CAFE tree for current OG
     cafe.tree.set <- ape::read.nexus(cafe_comp_tree_file)
     cafe.tree <- cafe.tree.set[[og.cafe]]
     
     if (length(cafe.tree) < 1)
     {
       shinyjs::hideElement(id = 'loading.cafe2')
       if (UI_exist_cafe2)
       {
         removeUI(
           selector = "div:has(>> #cafe_plot2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #cafe_mrca2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #cafe_download2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadCAFEPlot2)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       
       UI_exist_cafe2 <<- F
       
       if(UI_exist_error_cafe2)
       {
         removeUI(
           selector = "div:has(>> #cafe_error_message2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
       }
       insertUI("#error_cafe2", "afterEnd", ui = {
         box(width = 12,
             title = "Ancestral State Reconstruction", status = "success", solidHeader = TRUE,
             collapsible = TRUE,
             textOutput("cafe_error_message2"))
       })
       
       output$cafe_error_message2 <- renderText({print("No expansions/contraction detected for this orthogroup,
                                                        or infeasible computation due to large size and variance across
                                                        species.")})
       UI_exist_error_cafe2 <<- T
       
       validate(need(length(cafe.tree) > 0 , ""))
     }
     
     return(cafe.tree)
   }) %>% bindEvent(input$cafe_start2)
   
   evo_plot2 <- reactive({
     
     og.cafe <- og.name2()
     cafe.tree <- cafe_tree2()
     
     # Show an error if the orthogroup is not significantly expanded/collapsed in any branch
     
     model.node.number <- ifelse(model.selected2(), 46, 36)
     total.model.node.number <- ifelse(model.selected2(), 91, 71)
     
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
     
     mrca.tree <- read.tree(ifelse(model.selected2(), "pharaoh_folder/species_tree_global.txt",
                                   "pharaoh_folder/species_tree_green.txt"))
     
     node.names <- read.csv(ifelse(model.selected2(), "pharaoh_folder/tax_labels_global.tsv",
                                   "pharaoh_folder/tax_labels_green.tsv"), header = F, sep="\t")
     
     mrca.tree$node.label <- node.names$V2
     
     # Create a timeline for a given OG
     
     tree.name <- ifelse(model.selected2(),
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
     
   }) %>% bindEvent(input$cafe_start2)
   
   evo.paths.id2 <- reactive({
     
     # Create phylogenomic tree with internal nodes names
     og.cafe <- og.name2()
     model.node.number <- ifelse(model.selected2(), 46, 36)
     
     mrca.tree <- read.tree(ifelse(model.selected2(), "pharaoh_folder/species_tree_global.txt",
                                   "pharaoh_folder/species_tree_green.txt"))
     
     node.names <- read.csv(ifelse(model.selected2(), "pharaoh_folder/tax_labels_global.tsv",
                                   "pharaoh_folder/tax_labels_green.tsv"), header = F, sep="\t")
     
     mrca.tree$node.label <- node.names$V2
     
     # Create timeline
     tree.name <- ifelse(model.selected2(),
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
     
   }) %>% bindEvent(input$cafe_start2)
   
   # Outputs
   
   # Remove previous boxes if they exist and create new ones
   observeEvent(isTruthy(evo_plot2()), {
     
     if (UI_exist_cafe2)
     {
       removeUI(
         selector = "div:has(>> #cafe_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #cafe_mrca2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #cafe_download2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadCAFEPlot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     if (UI_exist_error_cafe2)
     {
       removeUI(
         selector = "div:has(>> #cafe_error_message2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_cafe2", "afterEnd", ui = {
       box(width = 12,
           title = "Ancestral State Reconstruction", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           imageOutput("cafe_plot2", height = 500, width = 1000))
     })
     
     insertUI("#box_mrca2", "afterEnd", ui = {
       box(width = 8,
           title = "Most Recent Common Ancestor", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           textOutput("cafe_mrca2")
       )
     })
     
     insertUI("#cafe_down_button2", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "cafe_download2", "Download NEWICK tree",
                                                                          size = "sm", color = "success"))
     })
     
     insertUI("#download_ui_for_cafe_plot2", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadCAFEPlot2", "Download Ancestral State Plot",
                                                                          size = "sm", color = "success"))
     })
     
     UI_exist_cafe2 <<- TRUE
     shinyjs::hideElement(id = 'loading.cafe2')
   })
   
   # Fill outputs
   
   output$cafe_plot2 <- renderImage({
     png("evo_plot2.png", height = 500, width = 1000)
     plot(evo_plot2())
     dev.off()
     
     list(src = "evo_plot2.png",
          contentType="image/png", width=1000,height=500)
   }, deleteFile = T)
   
   output$cafe_mrca2 <- renderText({
     print(paste0("Most recent common ancestor for this orthogroup is the
                   ancestor of the clade: ", evo.paths.id2()[1]))
   })
   
   # Download tab's results
   
   output$cafe_download2 <- downloadHandler(
     filename= function() {
       paste("ancestral_newick", ".txt", sep="")
     },
     content= function(file) {
       cafe_tree <- cafe_tree2()
       
       write.tree(cafe_tree, file)
     })
   
   output$downloadCAFEPlot2<- downloadHandler(
     filename= function() {
       paste("ancestral_plot", ".png", sep="")
     },
     content= function(file) {
       evo_plot <- evo_plot2()
       
       png(file)
       plot(evo_plot)
       dev.off()
     }) 
   

   ####################### MSA #################################
   
   observeEvent(input$run_button2, {
     removeUI(
       selector = "div:has(>> #selected_msaI2)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI(
       selector = "div:has(>> #msa_methodI2)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#msa_selection2")
     
     if (UI_exist_msa2)
     {
       removeUI(
         selector = "div:has(>>> #msa_print2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_fa2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_msa2 <<- F
     }
     
   })
   
   observeEvent(input$msa_start2, {
     insertUI("#selected_msa2", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_msaI2","Select the desired genes from the tree to align",
                                 choices=isolate({tree_reduced2()$tip.label}), options = list(`actions-box` = TRUE),
                                 multiple = T, selected = isolate({diamond_table2()$ID[1]}))
       
     })
     
     insertUI("#msa_method2", "afterEnd", ui = {
       shinyWidgets::pickerInput(inputId = "msa_methodI2", label = "Choose alignment method", 
                                 choices = c("ClustalOmega", "MAFFT"), selected = "ClustalOmega")
       
     })
     
     insertUI("#msa_selectionI2", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("msa_selection2", "Align Sequences", size = "sm",
                                style = "float", color = "success")
     })
     
   })
   
   alignseqs2 <- reactive({
     
     library(msa)
     shinyjs::showElement(id = 'loading.msa2')
     
     selected_genes <- as.vector(input$selected_msaI2)
     selected_method <- as.character(input$msa_methodI2)
     file.name <- og.name2()
     
     if (length(selected_genes) < 2)
     {
       shinyjs::hideElement(id = 'loading.msa2')
       
       if (UI_exist_msa2)
       {
       removeUI(
         selector = "div:has(>>> #msa_print2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_fa2)",
         multiple = TRUE,
         immediate = TRUE
       )
       }
       UI_exist_msa2 <<- F
       output$error_msa2 <- renderUI({renderText({print("Please select at least two genes.")})})
       validate(need(length(selected_genes) > 1, "Please select at least two genes."))
     }
     
     output$error_msa2 <- NULL
     
     # If de novo alignment is selected
     {
       if(selected_method == "ClustalOmega")
       {
         # Define path to orthogroup sequences file
         ortho.seq.name <- ifelse(model.selected2(),
                                  paste("Global_Orthogroup_Sequences", paste(file.name, "fa", sep = "."), sep="/"),
                                  paste("Green_Orthogroup_Sequences", paste(file.name, "fa", sep = "."), sep="/"))
         
         
         # Read orthogroup sequences file and select the genes for alignment
         mySequences1 <- Biostrings::readAAStringSet(ortho.seq.name)
         mysubseqs <- mySequences1[selected_genes]
         
         alignseqs <- msa(mysubseqs, verbose = F, method = "ClustalOmega")
       }
       
       # If MAFFT alignment is selected
       else
       {
         ortho.seq.name <- ifelse(model.selected2(),
                                  paste("Global_MultipleSequenceAlignments", paste(file.name, "fa", sep = "."), sep="/"),
                                  paste("Green_MultipleSequenceAlignments", paste(file.name, "fa", sep = "."), sep="/"))
         mySequences1 <- seqinr::read.fasta(ortho.seq.name, seqtype = "AA")
         mysubseqs <- mySequences1[selected_genes]
         mysubnames <- seqinr::getName(mySequences1)
         
         # Identify indexes associated with reduced names
         indexes_msa <- sapply(selected_genes, function(x) grep(mysubnames, pattern = x))
         
         # Retrieve those sequences from alignment keeping gaps
         mysubseqs <- mySequences1[indexes_msa]
         names(mysubseqs) <- names(indexes_msa)
         
         # Remove columns with gaps and remove empty spaces in last positions
         seqs_mysubseqs <- seqinr::getSequence(mysubseqs)
         last <- seqs_mysubseqs[[length(seqs_mysubseqs)]]
         last <- last[which(last != " ")]
         seqs_mysubseqs[[length(seqs_mysubseqs)]] <- last
         seqs_mysubseqs <- remove_gaps(seqs_mysubseqs)
         names(seqs_mysubseqs) <- names(mysubseqs)
         
         mysubseqs2 <- unlist(lapply(seqs_mysubseqs, function(x) paste(x, collapse="")))
         
         alignseqs <- Biostrings::AAMultipleAlignment(mysubseqs2, use.names = T)
         
       }
     }
     
     detach("package:msa", unload=TRUE)
     
     return(alignseqs)
     
   }) %>% bindEvent(input$msa_selection2)
   
   # Create boxes for outputs
   observeEvent(isTruthy(alignseqs2()), {
     
     if (UI_exist_msa2)
     {
       removeUI(
         selector = "div:has(>>> #msa_print2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_fa2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_msa2", "afterEnd", ui = {
       selected_msa <- isolate({input$selected_msaI2})
       msa_height <- ifelse(length(selected_msa) > 14, 550, 400 + 5*length(selected_msa))
       box(width = 12,
           title = "MSA Explorer", status = "success", solidHeader = TRUE, height = msa_height,
           collapsible = TRUE,
           tags$div(id = "msa_pocket2", style = "width: 1300px; height: 400px",
                    msaROutput("msa_print2"))
           
       )
     })
     
     insertUI("#msa_down_plot2", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "msa_download_plot2", "Download Colored MSA",
                                                                          size = "sm", color = "success"))
     })
     
     insertUI("#msa_down_fasta2", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "msa_download_fa2", "Download MSA FASTA",
                                                                          size = "sm", color = "success"))
     })
     
     UI_exist_msa2 <<- TRUE
   })
   
   
   # Fill Output
   output$msa_print2 <- renderMsaR({
     alignseqs <- alignseqs2()
     msaout <- msa::msaConvert(alignseqs, "ape::AAbin")
     msaR(msaout, menu=T, overviewbox = F,  colorscheme = "clustal")
   })
   
   # Prepare variables for pdf construction
   observeEvent(isTruthy(alignseqs2()), {
     alignseqs <- alignseqs2()
     
     library(ggmsa)
     class(alignseqs) <- "AAMultipleAlignment"
     
     for(i in 1:(ncol(alignseqs)%/%100 +1)){
       assign(paste("msapseq", i, sep = ""), ggmsa(alignseqs, 1+(100*(i-1)), i*100, seq_name = TRUE, char_width = 0.5) +
                geom_seqlogo(color = "Chemistry_AA"), envir = as.environment(1), pos=1)
     }
     shinyjs::hideElement(id = 'loading.msa2')
   })
   
   # Download tab's results
   # Download colored MSA in pdf
   output$msa_download_plot2 <- downloadHandler(
     filename= function() {
       paste("msa", ".pdf", sep="")
     },
     content= function(file) {
       selected_msa <- input$selected_msaI2
       alignseqs <- alignseqs2()
       pdf(file, height = 2+length(selected_msa)*0.25, width = 16)
       {
         for(i in 1:(ncol(alignseqs)%/%100 +1)){
           print(mget(paste0("msapseq", i), envir = as.environment(1)))
         }
         dev.off()
       }
     })
   
   # Download MSA in FASTA format
   output$msa_download_fa2<- downloadHandler(
     filename= function() {
       paste("msa", ".fa", sep="")
     },
     content= function(file) {
       alignseqs <- alignseqs2()
       writeXStringSet(as(unmasked(alignseqs), "XStringSet"), file)
     })
   
   ####################### GO #################################
   
   observeEvent(input$run_button2, {
     removeUI(
       selector = "div:has(>> #selected_gosI2)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI(
       selector = "div:has(>> #selected_gos_modeI2)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#gos_selectionI2")
     
     if (UI_exist_go2)
     {
       removeUI(
         selector = "div:has(>> #output_gos_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_treeplot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadGOSTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_download2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #tree_gos_download2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_go2 <<- F
     }
     
   })
   
   observeEvent(input$go_start2, {
     insertUI("#selected_gos2", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_gosI2","Select the desired genes from the tree",
                                 choices=isolate({tree_reduced2()$tip.label}), options = list(`actions-box` = TRUE),
                                 multiple = T, selected = isolate({diamond_table2()$ID[1]}))
       
     })
     
     insertUI("#selected_gos_mode2", "afterEnd", ui = {
       
       selectInput(inputId = "selected_gos_modeI2",
                   choices=c("Biological Processes" = "bp",
                             "Molecular Functions" = "mf",
                             "Cellular Components" = "cc"),
                   label = "Select the gene ontology to use",
                   multiple = F, selected = c("bp"))
       
     })
     
     insertUI("#gos_selection2", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("gos_selectionI2", "Show GO terms", size = "sm",
                                style = "float", color = "success")
     })
     
   })
   
   total_table_gos2 <- reactive({
     
     shinyjs::showElement(id = 'loading.go2')
     gos_anot <- read.csv("pharaoh_folder/final_anot_table.tsv", sep="\t", header = T)
     sel.genes.go <- as.vector(input$selected_gosI2)
     selected_gos_mode <- as.character(isolate({input$selected_gos_modeI2}))
     
     total_table_gos <- subset(gos_anot, gos_anot$name %in% sel.genes.go)
     
     # Show an error if no terms are identified in the input
     if (nrow(total_table_gos) == 0) 
     {
       shinyjs::hideElement(id = 'loading.go2')
       if (UI_exist_go2)
       {
         removeUI(
           selector = "div:has(>> #output_gos_table2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_plot2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_treeplot2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadGOSTable2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_download2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_gos_download2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_go2 <<- F
       }
       output$error_gos2 <- renderUI({
         renderPrint({cat("0 GO terms identified.")})
       })
       
       validate(need(nrow(total_table_gos) != 0, " "))
     }
     
     
     gos_sel <- paste("gos", selected_gos_mode, sep="_")
     terms_sel <- paste("terms", selected_gos_mode, sep="_")
     total_table_gos <- total_table_gos[,c("organism", "id", "name", gos_sel, terms_sel)]
     
     # Once removed the two GO categories not selected, remove rows with blank cells
     total_table_gos_clean <- subset(total_table_gos, gos_sel != "")
     
     # Show an error if no terms are identified after the previous operation
     if (nrow(total_table_gos_clean) == 0) 
     {
       shinyjs::hideElement(id = 'loading.go2')
       if (UI_exist_go2)
       {
         removeUI(
           selector = "div:has(>> #output_gos_table2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_plot2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_treeplot2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadGOSTable2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_download2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_gos_download2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_go2 <<- F
       }
       
       output$error_gos2 <- renderUI({
         renderPrint({cat("0 GO terms identified.")})
       })
       
       validate(need(nrow(total_table_gos_clean) != 0, " "))
     }
     
     output$error_gos2 <- NULL
     
     return(total_table_gos_clean)
     
   }) %>% bindEvent(input$gos_selectionI2)
   
   enr_table2 <- reactive({
     
     total_table_gos <- total_table_gos2()
     selected_gos_mode <- as.character(isolate({input$selected_gos_modeI2}))
     
     # Create the plot
     
     # Create a list of GO terms vector of each gene
     gos_sel <- paste("gos", selected_gos_mode, sep="_")
     gos_list <- apply(total_table_gos,MARGIN=1,FUN = function(x) trimws(strsplit(as.character(x[gos_sel]), split = "[|]")[[1]]))
     names(gos_list) <- total_table_gos$name
     
     # Count GOs for each gene and create a matrix of gene-GO pairs
     count_gos_in_genes <- sapply(gos_list, FUN = function(x) length(x))
     comp_data <- data.frame(gene = rep(names(count_gos_in_genes), count_gos_in_genes), gos = as.character(unlist(gos_list)))
     
     # Collapse genes that share a same GO
     gene.v <- c()
     for (i in 1:length(unique(comp_data$gos)))
     {
       new.table <- subset(comp_data, gos == unique(comp_data$gos)[i])
       new.cha <- as.character(new.table$gene)
       gene.v <- c(gene.v, paste(new.cha, collapse = "/"))
     }
     
     names(gene.v) <- unique(comp_data$gos)
     
     # Load libraries and create gene chains, count and GO IDs fields (same order)
     library(GO.db)
     library("multienrichjam")
     library(clusterProfiler)
     library(enrichplot)
     library(ggplot2)
     
     count_go <- table(comp_data$gos)
     geneids <- gene.v[names(count_go)]
     count_terms <- mapply(function(x) {Term(x)}, names(count_go), USE.NAMES = F)
     
     # Create pseudo-enrichment table
     enr_table <- data.frame(ID=names(count_go), Description=count_terms, GeneRatio="90/100", BgRatio="90/10000", 
                             pvalue=0.000005, p.adjust=0.000005, qvalue=0.000005, geneID=geneids, Count = as.vector(count_go))
     
     return(enr_table)
     
   }) %>% bindEvent(input$gos_selectionI2)
   
   ema_gos_plot2 <- reactive({
     
     enr_table <- enr_table2()
     
     # Transform to enrichResult object
     enr <- enrichDF2enrichResult(enrichDF = enr_table, keyColname = "ID",
                                  geneColname = "geneID", pvalueColname = "p.adjust",
                                  descriptionColname = "Description", pvalueCutoff = 0.05)
     
     # Create plot
     ema_gos_plot <- emapplot(pairwise_termsim(enr), showCategory = 15) + theme(legend.position='none')
     return(ema_gos_plot)
   })
   
   tree_gos_plot2 <- reactive({
     
     enr_table <- enr_table2()
     
     # Transform to enrichResult object
     enr <- enrichDF2enrichResult(enrichDF = enr_table, keyColname = "ID",
                                  geneColname = "geneID", pvalueColname = "p.adjust",
                                  descriptionColname = "Description", pvalueCutoff = 0.05)
     {
       if (nrow(enr_table) > 4)
       {
         tree_gos_plot <- treeplot(pairwise_termsim(enr),showCategory = 15, cluster.params = list(label_words_n = 3)) +
           theme(legend.position='none')
       }
       else if (nrow(enr_table) > 2)
       {
         tree_gos_plot <- treeplot(pairwise_termsim(enr),showCategory = 15, cluster.params = list(n = 2, label_words_n = 3)) + 
           theme(legend.position='none')
       }
       else
       {
         text <- paste("\n  Unable to create treeplot with less than 3 GO terms \n")
         tree_gos_plot <- ggplot() + 
           annotate("text", x = 4, y = 25, size=8, label = text) + 
           theme_void()
       }
     }
     
     shinyjs::hideElement(id = 'loading.go2')
     return(tree_gos_plot)
   })
   
   # Create boxes for outputs
   observeEvent(isTruthy(tree_gos_plot2()), {
     
     if (UI_exist_go2)
     {
       removeUI(
         selector = "div:has(>> #output_gos_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_treeplot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadGOSTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_download2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #tree_gos_download2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_gos_table2", "afterEnd", ui = {
       box(width = 12,
           title = "GO Terms Table", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_gos_table2")
       )
     })
     
     insertUI("#box_gos_plot2", "afterEnd", ui = {
       box(width = 12,
           title = "GO Terms Plot", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           imageOutput("gos_plot2")
       )
     })
     
     insertUI("#box_gos_treeplot2", "afterEnd", ui = {
       box(width = 12,
           title = "GO Terms Treeplot", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           imageOutput("gos_treeplot2")
       )
     })
     
     insertUI("#download_ui_for_gos_table2", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadGOSTable2", "Download GO Table",
                                                                          size = "sm", color = "success"))
     })
     
     insertUI("#gos_down_button2", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "gos_download2", "Download GO Plot",
                                                                          size = "sm", color = "success"))
     })
     
     insertUI("#tree_gos_down_button2", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "tree_gos_download2", "Download GO Treeplot",
                                                                          size = "sm", color = "success"))
     })
     
     UI_exist_go2 <<- TRUE
   })
   
   # Fill outputs
   # Table
   output$output_gos_table2 <- renderDataTable({
     total_table_gos <- total_table_gos2()
     selected_gos_mode <- as.character(isolate({input$selected_gos_modeI2}))
     gos_sel <- paste("gos", selected_gos_mode, sep="_")
     gos_list <- apply(total_table_gos,MARGIN=1,
                       FUN = function(x) trimws(strsplit(as.character(x[gos_sel]), split = "[|]")[[1]]))
     gos_links <- lapply(gos_list, function(x) sapply(x, go.link))
     gos_formatted <- unlist(lapply(gos_links, function(x) paste0(x, collapse = " | ")))
     total_table_gos[,gos_sel] <- gos_formatted
     total_table_gos
   },escape=FALSE, rownames=F, options =list(pageLength = 5))
   
   # First plot
   output$gos_plot2 <- renderImage({
     ema_gos_plot <- ema_gos_plot2()
     
     png("pharaoh_folder/gosplot.png", width = 590, height = 590, res = 90)
     plot(ema_gos_plot)
     dev.off()
     list(src = "pharaoh_folder/gosplot.png",
          contentType="image/png")
     
   }, deleteFile=T
   )
   
   # Second plot
   output$gos_treeplot2 <- renderImage({
     tree_gos_plot <- tree_gos_plot2()
     
     png("pharaoh_folder/treeplot.png", width = 620, height = 590, res = 80)
     plot(tree_gos_plot)
     dev.off()
     list(src = "pharaoh_folder/treeplot.png",
          contentType="image/png")
     
   }, deleteFile=T
   )
   
   # Download tab's results
   # Download GO table
   output$downloadGOSTable2 <- downloadHandler(
     filename= function() {
       paste("GOS_table", ".tsv", sep="")
     },
     content= function(file) {
       total_table_gos <- total_table_gos2()
       
       write.table(x = total_table_gos,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Download emaplot
   output$gos_download2 <- downloadHandler(
     filename= function() {
       paste("gos_plot", ".png", sep="")
     },
     content= function(file) {
       ema_gos_plot <- ema_gos_plot2()
       
       png(file, height = 1200, width = 1500, res=140)
       plot(ema_gos_plot)
       dev.off()
     })
   
   # Download treeplot
   output$tree_gos_download2 <- downloadHandler(
     filename= function() {
       paste("gos_treeplot", ".png", sep="")
     },
     content= function(file) {
       tree_gos_plot <- tree_gos_plot2()
       
       png(file, height = 1200, width = 1500, res=140)
       plot(tree_gos_plot)
       dev.off()
     })
   
   
   
   ###################### KEGG ###########################
   
   observeEvent(input$run_button2, {
     removeUI(
       selector = "div:has(>> #selected_kosI2)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#kos_selectionI2")
     
     if (UI_exist_kegg2)
     {
       removeUI(
         selector = "div:has(>> #output_kos_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #output_kegg_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKOSTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #selected_pathsI2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI("#paths_buttonI2")
       
       UI_exist_kegg2 <<- F
     }
     
     if (UI_exist_pathview2)
     {
       removeUI(
         selector = "div:has(>> #path_image2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGpathway2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_pathview2 <<- F
     }
     
     
   })
   
   observeEvent(input$kegg_start2, {
     insertUI("#selected_kos2", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_kosI2","Select the desired genes from the tree",
                                 choices=isolate({tree_reduced2()$tip.label}), options = list(`actions-box` = TRUE),
                                 multiple = T, selected = isolate({diamond_table2()$ID[1]}))
       
       
     })
     
     
     insertUI("#kos_selection2", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("kos_selectionI2", "Show KEGG pathways", size = "sm",
                                style = "float", color = "success")
     })
     
   })
   
   tab_kegg2 <- reactive({
     
     shinyjs::showElement(id = 'loading.ko2')
     # Create KOs set
     kos_anot <- read.csv("pharaoh_folder/ko_table_funtree.tsv", sep="\t", header = T)
     sel.genes.ko <- as.vector(isolate({input$selected_kosI2}))
     
     tab_kegg <- subset(kos_anot, gene %in% sel.genes.ko)
     set_kegg <- tab_kegg$ko[tab_kegg$ko != ""]
     
     # Show an error if no terms are identified in the input
     {
       if (length(set_kegg) == 0) 
       {
         shinyjs::hideElement(id = 'loading.ko2')
         output$error_kos2 <- renderUI({
           renderPrint({cat("0 KO terms identified. Please select more genes. If this 
        message persists, it should be interpreted as a lack of KO annotation for this orthogroup")})
         })
         
         if (UI_exist_kegg2)
         {
           removeUI(
             selector = "div:has(>> #output_kos_table2)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #output_kegg_table2)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #downloadKOSTable2)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #downloadKEGGTable2)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #selected_pathsI2)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI("#paths_buttonI2")
           
           UI_exist_kegg2 <<- F
         }
         
         if (UI_exist_pathview2)
         {
           removeUI(
             selector = "div:has(>> #path_image2)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #downloadKEGGpathway2)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           UI_exist_pathview2 <<- F
         }
         
         validate("No KO terms detected")
       }
     }
     
     output$error_kos2 <- NULL
     
     return(tab_kegg)
     
   }) %>% bindEvent(input$kos_selectionI2)
   
   total_table_kegg2 <- reactive({
     
     tab_kegg <- tab_kegg2()
     set_kegg <- tab_kegg$ko[tab_kegg$ko != ""]
     
     # Load libraries
     library(clusterProfiler)
     library(enrichplot)
     
     # Enrich with pvalue cutoff = 1 to show all paths
     kos_enrich <- enrichKEGG(gene         = set_kegg,
                              organism     = 'ko',
                              pvalueCutoff = 1)
     
     total_table_kegg <- as.data.frame(kos_enrich)
     
     # Filter out pathways that are not present in plants
     kegg_plants <- read.csv("pharaoh_folder/pathways_plant.ids", sep = "\t", header = T)$x
     total_table_kegg <- subset(total_table_kegg, ID %in% kegg_plants)
     
     return(total_table_kegg)
     
   }) %>% bindEvent(input$kos_selectionI2)
   
   total_table_kos2 <- reactive({
     
     tab_kegg <- tab_kegg2()
     
     library(KEGGREST)
     
     # Collapse genes that share KOs
     
     tab_kegg_for_ko <- subset(tab_kegg, ko != "")
     gene.v.ko <- c()
     for (i in 1:length(unique(tab_kegg_for_ko$ko)))
     {
       new.table <- subset(tab_kegg_for_ko, ko == unique(tab_kegg_for_ko$ko)[i])
       new.cha <- as.character(new.table$gene)
       gene.v.ko <- c(gene.v.ko, paste(new.cha, collapse = "/"))
     }
     
     names(gene.v.ko) <- unique(tab_kegg_for_ko$ko)
     
     # Create gene chains, count and KO IDs fields (same order)
     count_ko <- table(tab_kegg_for_ko$ko)
     geneids.ko <- gene.v.ko[names(count_ko)]
     count_terms.ko <- mapply(function(x) {keggFind("ko", x)}, names(count_ko), USE.NAMES = F)
     total_table_kos <- data.frame(ko=names(count_ko), name=count_terms.ko, count=as.numeric(count_ko),
                                   genes=geneids.ko)
     
     return(total_table_kos)
     
   }) %>% bindEvent(input$kos_selectionI2)
   
   # Create boxes for outputs
   observeEvent(isTruthy(total_table_kos2()), {
     
     if (UI_exist_kegg2)
     {
       removeUI(
         selector = "div:has(>> #output_kos_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #output_kegg_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKOSTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #selected_pathsI2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI("#paths_buttonI2")
       
     }
     
     insertUI("#box_kos_table2", "afterEnd", ui = {
       box(width = 12,
           title = "KO Terms Table", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_kos_table2")
       )
     })
     
     
     insertUI("#box_kegg_table2", "afterEnd", ui = {
       box(width = 12,
           title = "KEGG Pathways Table", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_kegg_table2")
       )
     })
     
     insertUI("#download_ui_for_kos_table2", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadKOSTable2", "Download KO Table",
                                                                          size = "sm", color = "success"))
     })
     
     insertUI("#download_ui_for_kegg_table2", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadKEGGTable2", "Download Pathways Table",
                                                                          size = "sm", color = "success"))
     })
     
     UI_exist_kegg2 <<- TRUE
     
     # Remove previous results for pathview
     if (UI_exist_pathview2)
     {
       removeUI(
         selector = "div:has(>> #path_image2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGpathway2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_pathview2 <<- F
     }
   })
   
   # Fill outputs
   # Render KO table
   output$output_kos_table2 <- renderDataTable({
     total_table_kos <- total_table_kos2()
     total_table_kos$ko <- sapply(total_table_kos$ko, ko.link)
     total_table_kos
   },escape=FALSE, rownames= F, options =list(pageLength = 5))
   
   # Render KEGG table
   output$output_kegg_table2 <- renderDataTable({
     total_table_kegg <- total_table_kegg2()
     total_table_kegg$ID <- sapply(total_table_kegg$ID, kegg.link)
     total_table_kegg[,c("ID", "Description", "geneID")]
   },escape=FALSE, rownames= F, options =list(pageLength = 5))
   
   # Download tab's outputs
   # Download KO table
   output$downloadKOSTable2 <- downloadHandler(
     filename= function() {
       paste("KO_table", ".tsv", sep="")
     },
     content= function(file) {
       total_table_kos <- total_table_kos2()
       write.table(x = total_table_kos,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Download KEGG table
   output$downloadKEGGTable2 <- downloadHandler(
     filename= function() {
       paste("KEGG_table", ".tsv", sep="")
     },
     content= function(file) {
       total_table_kegg <- total_table_kegg2()
       write.table(x = total_table_kegg[,c("ID", "Description", "geneID")],quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Create pathway selector and button
   observeEvent(input$kos_selectionI2,{
     
     total_table_kos <- total_table_kos2()
     total_table_kegg <- total_table_kegg2()
     
     if(nrow(total_table_kegg) != 0)
     {
       paths.options <- sapply(strsplit(total_table_kegg$ID, split = "ko"), function(x) x[[2]])
       
       
       insertUI("#selected_paths2", "afterEnd", ui = {
         shinyWidgets::pickerInput(inputId = "selected_pathsI2", label = "Select the pathway to plot", 
                                   choices = paths.options, selected = paths.options[1], multiple = F)
         
       })
       
       
       insertUI("#paths_button2", "afterEnd", ui = {
         
         shinyWidgets::actionBttn("paths_buttonI2", "Plot Pathway", size = "sm",
                                  style = "float", color = "success")
       })
       
       shinyjs::hideElement(id = 'loading.ko2')
     }
   })
   
   observeEvent(input$paths_buttonI2,{
     
     if (UI_exist_pathview2)
     {
       removeUI(
         selector = "div:has(>> #path_image2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGpathway2)",
         multiple = TRUE,
         immediate = TRUE
       )
     }
     
     UI_exist_pathview2 <<- F
     
   })
   
   # Create Image to Render and save path name
   pathway.current.id2 <- reactive({
     pathway.current.id <- input$selected_pathsI2
     total_table_kos <- total_table_kos2()
     
     kos_unique <- unique(total_table_kos$ko)
     gene.pathway <- rep(0, length(kos_unique))
     names(gene.pathway) <-  kos_unique
     gene.pathway[kos_unique] <-1
     
     library(pathview)
     pathview(gene.data = sort(gene.pathway,decreasing = TRUE),kegg.dir = "pharaoh_folder",
              pathway.id = pathway.current.id,
              species = "ko",
              limit = list(gene=max(abs(gene.pathway)), cpd=1),
              gene.idtype ="kegg")
     
     return(pathway.current.id)
     
   }) %>% bindEvent(input$paths_buttonI2)
   
   # Create output box and download button
   observeEvent(isTruthy(pathway.current.id2()),{
     
     insertUI("#box_path_image2", "afterEnd", ui = {
       box(width = 12,
           title = "KEGG Pathway Plot", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           fluidRow(column(1), imageOutput("path_image2", width = "100%", height = "700px"))
       )
     })
     
     insertUI("#path_download_ui2", "afterEnd", ui = {
       tags$div(shinyWidgets::downloadBttn(outputId= "downloadKEGGpathway2", "Download KEGG Pathway Plot",
                                           size = "sm", color = "success"))
     })
     
     UI_exist_pathview2 <<- T
     
   })
   
   # Fill path image output
   output$path_image2 <- renderImage({
     
     pathway.current.id <- pathway.current.id2()
     list(src = paste(c(paste0(c("ko",pathway.current.id), collapse=""),"pathview","png"), collapse="."),
          contentType="image/png",width=900,height=700)
   },deleteFile = F)
   
   # Download and remove path image output
   output$downloadKEGGpathway2 <- downloadHandler(
     filename= function() {
       paste("path_plot", ".png", sep="")
     },
     content= function(file) {
       pathway.current.id <- pathway.current.id2()
       file.copy(paste(c(paste0(c("ko",pathway.current.id), collapse=""),"pathview","png"), collapse="."), file)
       file.remove(paste(c(paste0(c("ko",pathway.current.id), collapse=""),"pathview","png"), collapse="."))
     })   
   
   
   
   ############################# LITERATURE ANNOTATION ##########################################
   
   observeEvent(input$run_button2, {
     removeUI(
       selector = "div:has(>> #selected_litI2)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI(
       selector = "div:has(> #query_litI2)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#lit_selectionI2")
     
     
     if (UI_exist_lit2)
     {
       removeUI(
         selector = "div:has(>> #output_lit_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadLITTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_lit2 <<- F
     }
     
   })
   
   observeEvent(input$lit_start2, {
     
     insertUI("#query_lit2", "afterEnd", ui = {
       
       textInput(inputId = "query_litI2",value = "", label = "Enter search term", placeholder = "CCA1")
       
     })
     
     
     insertUI("#selected_lit2", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_litI2","Select the search mode",
                                 choices=c("Normal","Exact", "Substring", "Alias"),
                                 multiple = F, selected = "Normal")
       
       
     })
     
     
     insertUI("#lit_selection2", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("lit_selectionI2", "Get biological information and papers", size = "sm",
                                style = "float", color = "success")
     })
     
   })
   
   pc_result2 <- reactive({
     
     shinyjs::showElement(id = 'loading.lit2')
     pc_search <- as.character(input$query_litI2)
     pc_search <- gsub(" ", "%20", pc_search) 
     pc_modality <- tolower(as.character(input$selected_litI2))
     
     # Get PlantConnectome URL for query
     pc_url <- paste(c("https://connectome.plant.tools", pc_modality, pc_search), collapse = "/")
     
     library(RCurl)
     
     pc_res <- getURL(pc_url)
     
     if (!length(grep("No hits", pc_res)) == 0)
     {
       shinyjs::hideElement(id = 'loading.lit2')
       
       if (UI_exist_lit2)
       {
         removeUI(
           selector = "div:has(>> #output_lit_table2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadLITTable2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_lit2 <<- F
       }
       
       output$error_lit2 <- renderUI({
         renderPrint({cat("No results found for this query")})
       })
       
       validate(" ")
       
     }
     
     output$error_lit2 <- NULL
     
     # Isolate data frame from complete HTML file
     pc_split <- strsplit(as.character(pc_res), split = "<tbody")[[1]][2]
     pc_split <- strsplit(as.character(pc_split), split = "</tbody>")[[1]][1]
     
     pc_clean <- gsub("</tr>", "", pc_split)
     pc_clean <- gsub("[\r\n\t]", "", pc_clean)
     
     pc_vector <- strsplit(pc_clean, split = "<tr>")[[1]][-1]
     pc_vector2 <- sapply(pc_vector, FUN=function(x) strsplit(x, split = "<td> | </td>"))
     pc_table <- sapply(pc_vector2, FUN=function(x) as.character(unlist(x)))
     
     colnames(pc_table) <- NULL
     pc_result <- data.frame(t(pc_table[c(2,4,6,8),]))
     colnames(pc_result) <- c("Source", "Interaction Type", "Target", "Pubmed ID")
     
     return(pc_result)
     
   }) %>% bindEvent(input$lit_selectionI2)
   
   pc_result_show2 <- reactive({
     
     pc_result <- pc_result2()
     
     # Add links to papers
     urls_connect <- sapply(pc_result$`Pubmed ID`, FUN = function(x) paste0(c("<a href=\"",
                                                                              "https://pubmed.ncbi.nlm.nih.gov/",x,"/",
                                                                              "\" target=\"_blank\">", x,
                                                                              "</a>"),
                                                                            collapse=""))
     pc_result_show <- pc_result
     pc_result_show$`Pubmed ID` <- urls_connect
     
     return(pc_result_show)
     
   })
   
   # Create boxes for outputs
   observeEvent(isTruthy(pc_result_show2()), {
     
     if (UI_exist_lit2)
     {
       removeUI(
         selector = "div:has(>> #output_lit_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadLITTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_lit_table2", "afterEnd", ui = {
       box(width = 12,
           title = "Literature Table", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_lit_table2")
       )
     })
     
     insertUI("#download_ui_for_lit_table2", "afterEnd", ui = {
       tags$div(style = "margin-left: 400px;", shinyWidgets::downloadBttn(outputId= "downloadLITTable2", "Download Literature Table",
                                                                          size = "sm", color = "success"))
     })
     
     shinyjs::hideElement(id = 'loading.lit2')
     
     UI_exist_lit2 <<- TRUE
     
   })
   
   # Fill outputs
   # Render table
   output$output_lit_table2 <- renderDataTable({
     pc_result_show2()
   },escape=FALSE, rownames= F, options =list(pageLength = 10))
   
   # Download results
   output$downloadLITTable2 <- downloadHandler(
     filename= function() {
       paste("literature_table", ".tsv", sep="")
     },
     content= function(file) {
       pc_result <- pc_result2()
       write.table(x = pc_result,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })   
   
   
   ######################## STRING ###########################
   
   observeEvent(input$run_button2, {
     removeUI(
       selector = "div:has(>> #selected_stringI2)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#string_selectionI2")
     
     shinyjs::hideElement("error_string2")
     
     
     if (UI_exist_string2)
     {
       removeUI(
         selector = "div:has(>> #output_st_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #output_count_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>>> #count_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadSTRINGTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadCOUNTTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #count_download2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #selected_networkI2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI("#network_buttonI2")
       
       UI_exist_string2 <<- F
     }
     
     if (UI_exist_network2)
     {
       removeUI(
         selector = "div:has(>>>> #network_image2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_network2 <<- F
     }
     
   })
   
   # First, create a table that links reduced gene names to its species
   string_sel_table2 <- reactive({
     
     # Define previous variables
     tree_reduced <- tree_reduced2()
     tree <- tree_adj2()
     
     tips_to_keep.mp <- tips_to_keep.mp2()
     tips_to_keep.ot <- tips_to_keep.ot2()
     tips_to_keep.at <- tips_to_keep.at2()
     tips_to_keep.cp <- tips_to_keep.cp2()
     tips_to_keep.cr <- tips_to_keep.cr2()
     tips_to_keep.cz <- tips_to_keep.cz2()
     tips_to_keep.kn <- tips_to_keep.kn2()
     tips_to_keep.me <- tips_to_keep.me2()
     tips_to_keep.mi <- tips_to_keep.mi2()
     tips_to_keep.pp <- tips_to_keep.pp2()
     tips_to_keep.sl <- tips_to_keep.sl2()
     tips_to_keep.sm <- tips_to_keep.sm2()
     tips_to_keep.sp <- tips_to_keep.sp2()
     tips_to_keep.ta <- tips_to_keep.ta2()
     tips_to_keep.vc <- tips_to_keep.vc2()
     tips_to_keep.bp <- tips_to_keep.bp2()
     tips_to_keep.cri <- tips_to_keep.cri2()
     tips_to_keep.ds <- tips_to_keep.ds2()
     tips_to_keep.os <- tips_to_keep.os2()
     tips_to_keep.smag <- tips_to_keep.smag2()
     tips_to_keep.tp <- tips_to_keep.tp2()
     tips_to_keep.aa <- tips_to_keep.aa2()
     tips_to_keep.um <- tips_to_keep.um2()
     tips_to_keep.rs <- tips_to_keep.rs2()
     tips_to_keep.cyc <- tips_to_keep.cyc2()
     tips_to_keep.pu <- tips_to_keep.pu2()
     tips_to_keep.pt <- tips_to_keep.pt2()
     tips_to_keep.ng <- tips_to_keep.ng2()
     tips_to_keep.cyano <- tips_to_keep.cyano2()
     tips_to_keep.ca <- tips_to_keep.ca2()
     tips_to_keep.mv <- tips_to_keep.mv2()
     tips_to_keep.af <- tips_to_keep.af2()
     tips_to_keep.sc <- tips_to_keep.sc2()
     tips_to_keep.aegi <- tips_to_keep.aegi2()
     tips_to_keep.sb <- tips_to_keep.sb2()
     tips_to_keep.chara <- tips_to_keep.chara2()
     tips_to_keep.guilla <- tips_to_keep.guilla2()
     tips_to_keep.crypto <- tips_to_keep.crypto2()
     tips_to_keep.cymero <- tips_to_keep.cymero2()
     tips_to_keep.galsul <- tips_to_keep.galsul2()
     tips_to_keep.gracichor <- tips_to_keep.gracichor2()
     tips_to_keep.sceobli <- tips_to_keep.sceobli2()
     tips_to_keep.cocco <- tips_to_keep.cocco2()
     tips_to_keep.saccha <- tips_to_keep.saccha2()
     tips_to_keep.haema <- tips_to_keep.haema2()
     tips_to_keep.zm <- tips_to_keep.zm2()
     
     # Table construction
     org.factor <- c()
     
     for (i in 1:length(tree_reduced$tip.label))
     {
       
       if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mp])
       {
         org.factor <- c(org.factor,"Marchantia")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ot])
       {
         org.factor <- c(org.factor,"Ostreococcus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.at])
       {
         org.factor <- c(org.factor,"Arabidopsis")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cp])
       {
         org.factor <- c(org.factor,"Ceratodon")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cr])
       {
         org.factor <- c(org.factor,"Chlamydomonas")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cz])
       {
         org.factor <- c(org.factor,"Chromochloris")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.kn])
       {
         org.factor <- c(org.factor,"Klebsormidium")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.me])
       {
         org.factor <- c(org.factor,"Mesotaenium")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mi])
       {
         org.factor <- c(org.factor,"Micromonas")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pp])
       {
         org.factor <- c(org.factor,"Physcomitrium")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sl])
       {
         org.factor <- c(org.factor,"Solanum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sm])
       {
         org.factor <- c(org.factor,"Selaginella")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sp])
       {
         org.factor <- c(org.factor,"Spirogloea")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ta])
       {
         org.factor <- c(org.factor,"Triticum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.vc])
       {
         org.factor <- c(org.factor,"Volvox")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.bp])
       {
         org.factor <- c(org.factor,"Bathycoccus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cri])
       {
         org.factor <- c(org.factor,"Ceratopteris")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ds])
       {
         org.factor <- c(org.factor,"Dunaliella")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.os])
       {
         org.factor <- c(org.factor,"Oryza")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.smag])
       {
         org.factor <- c(org.factor,"Sphagnum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.tp])
       {
         org.factor <- c(org.factor,"Thuja")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.aa])
       {
         org.factor <- c(org.factor,"Anthoceros")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.um])
       {
         org.factor <- c(org.factor,"Ulva")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.rs])
       {
         org.factor <- c(org.factor,"Raphidocelis")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cyc])
       {
         org.factor <- c(org.factor,"Cycas")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pu])
       {
         org.factor <- c(org.factor,"Porphyra")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pt])
       {
         org.factor <- c(org.factor,"Phaeodactylum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ng])
       {
         org.factor <- c(org.factor,"Nannochloropsis")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cyano])
       {
         org.factor <- c(org.factor,"Cyanophora")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ca])
       {
         org.factor <- c(org.factor,"Chlorokybus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mv])
       {
         org.factor <- c(org.factor,"Mesostigma")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.af])
       {
         org.factor <- c(org.factor,"Azolla")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sc])
       {
         org.factor <- c(org.factor,"Salvinia")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.aegi])
       {
         org.factor <- c(org.factor,"Aegilops")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sb])
       {
         org.factor <- c(org.factor,"Sorghum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.chara])
       {
         org.factor <- c(org.factor,"Chara")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.guilla])
       {
         org.factor <- c(org.factor,"Guillardia")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.crypto])
       {
         org.factor <- c(org.factor,"Cryptophyceae")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cymero])
       {
         org.factor <- c(org.factor,"Cyanidioschyzon")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.galsul])
       {
         org.factor <- c(org.factor,"Galdieria")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.gracichor])
       {
         org.factor <- c(org.factor,"Gracilariopsis")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sceobli])
       {
         org.factor <- c(org.factor,"Scenedesmus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cocco])
       {
         org.factor <- c(org.factor,"Coccomyxa")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.saccha])
       {
         org.factor <- c(org.factor,"Saccharina")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.haema])
       {
         org.factor <- c(org.factor,"Haematococcus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.zm])
       {
         org.factor <- c(org.factor,"Zea")
       }
       
     }
     
     #Matrix with labels and colors and transform to dplyr format
     string.sel.table <- data.frame(node = 1:length(tree_reduced$tip.label), label = tree_reduced$tip.label,
                                    org = org.factor)
     
     return(string.sel.table)
     
   }) %>% bindEvent(input$string_start2)
   
   # Now, selection is allowed for genes of species with STRING support
   observeEvent(input$string_start2, {
     
     string_sel_table <- string_sel_table2()
     allow_string_species <- c("Aegilops", "Arabidopsis", "Bathycoccus", "Chara", "Chlamydomonas",
                               "Coccomyxa", "Cyanidioschyzon", "Galdieria", "Gracilariopsis",
                               "Guillardia", "Klebsormidium", "Micromonas","Oryza",
                               "Ostreococcus", "Phaeodactylum","Physcomitrium", "Raphidocelis",
                               "Scenedesmus", "Selaginella", "Solanum", "Sorghum", "Triticum",
                               "Volvox")
     
     st_genes <- subset(string_sel_table, string_sel_table$org %in% allow_string_species)$label
     
     if (length(st_genes) == 0)
     {
       shinyjs::showElement("error_string2")
       output$error_string2 <- renderUI({renderText({print("No results for this
      analysis due to lack of genes of STRING-supported species in the selection.")})})
       validate(" ")
     }
     
     output$error_string2 <- NULL
     
     insertUI("#selected_string2", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_stringI2","Select the desired genes from the tree",
                                 choices=st_genes, options = list(`actions-box` = TRUE),
                                 multiple = T)
       
     })
     
     
     insertUI("#string_selection2", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("string_selectionI2", "Show STRING Interactions", size = "sm",
                                style = "float", color = "success")
     })
     
   })
   
   phys_table2 <- reactive({
     
     shinyjs::showElement(id = 'loading.string2')
     query_genes <- input$selected_stringI2
     
     # Load complete STRING annotation table
     library(data.table)
     
     # Load iteratively from split files using data.table format
     data_phys <- fread("pharaoh_folder/string_physical/string_physical_1.tsv")
     
     for (x in list.files("pharaoh_folder/string_physical/")[-1])
     {
       data_phys <- rbind(data_phys, 
                          fread(paste0("pharaoh_folder/string_physical/", x)))
     }
     
     
     
     # Subset by query genes using data.table for speed
     #string_res <- subset(data_phys, data_phys$prot_query %in% query_genes)
     string_res <- data_phys[prot_query %in% query_genes,]
     string_res <- as.data.frame(string_res)
     
     # Assign OG ID to each target
     ortho_data_file <- ifelse(model.selected2(), "Global_Gene_Trees/Orthogroups.tsv",
                               "Green_Gene_Trees/Orthogroups.tsv")
     
     ortho_data <- as.data.frame(fread(ortho_data_file))
     
     ortho_char <- apply(ortho_data, MARGIN = 1, function(x) paste(x, collapse = ","))
     ortho.numbers <- sapply(string_res$prot_interaction, function(x) grep(x, ortho_char), USE.NAMES = F)
     
     # If a pattern is found in several names, i.e., is a subpattern of several genes,
     # search for the exact match
     if(class(ortho.numbers) == "list")
     {
       # If a gene isn't associated to an OG
       index.none <- which(sapply(ortho.numbers, function(x) length(x) == 0))
       
       if (length(index.none) != 0)
       {
         # We create another row for OG table to associate those genes
         ortho_data <- rbind(ortho_data, "No OG")
         ortho.numbers[index.none] <- nrow(ortho_data)
       }
       
       # If a gene has more than one match due to subpatterns
       index.wrong <- which(sapply(ortho.numbers, function(x) length(x) > 1))
       {
         if (length(index.wrong) == 0)
         {
           ortho.numbers <- unlist(ortho.numbers, use.names = F)
         }
         else
         {
           for (i in index.wrong)
           {
             for (j in ortho.numbers[[i]])
             {
               ortho_char_split <- strsplit(ortho_char[j],split = ",")
               ortho_char_split_clean <- sapply(ortho_char_split, function(x) gsub(" ", "", x))
               if (string_res$prot_interaction[i] %in% ortho_char_split_clean)
               {
                 ortho.numbers[i] <- j
                 ortho.numbers <- unlist(ortho.numbers, use.names = F)
               }
             }
           }
         }
       }
     }
     
     
     
     # Create the final table
     ortho.string.names <- ortho_data$Orthogroup[ortho.numbers]
     phys_table <- data.frame(string_res, orthogroup = ortho.string.names)
     
     return(phys_table)
   }) %>% bindEvent(input$string_selectionI2)
   
   # Create count table to identify enriched OGs in STRING result
   string_counts2 <- reactive({
     
     phys_table <- phys_table2()
     string_counts <- sort(table(phys_table$orthogroup), decreasing = T)
     
     return(string_counts)
     
   }) %>% bindEvent(input$string_selectionI2)
   
   string_count_plot2 <- reactive({
     
     library(ggplot2)
     library(dplyr)
     
     data_count <- as.data.frame(string_counts2())
     colnames(data_count) <- c("orthogroup", "value")
     
     # Compute the position of labels
     data_count <- data_count %>%
       arrange(desc(orthogroup)) %>%
       mutate(prop = value / sum(data_count$value) *100) %>%
       mutate(ypos = cumsum(prop)- 0.5*prop )
     
     # Create plot
     count_plot <- ggplot(data_count, aes(x="", y=prop, fill=orthogroup)) +
       geom_bar(stat="identity", width=1, color="white") +
       coord_polar("y", start=0) +
       theme_void() +
       theme(legend.position="none") +
       #geom_text(aes(y = ypos, label = orthogroup), color = "white", size=6) +
       scale_fill_manual(values = rep(RColorBrewer::brewer.pal(n = 9, name = "Set1"), 
                                      floor(nrow(data_count)/9)+1))
     
     return(count_plot)
     
   }) %>% bindEvent(input$string_selectionI2)
   
   # Create boxes for outputs
   observeEvent(isTruthy(string_count_plot2()), {
     
     if (UI_exist_string2)
     {
       removeUI(
         selector = "div:has(>> #output_st_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #output_count_table2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>>> #count_plot2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadSTRINGTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadCOUNTTable2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #count_download2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #selected_networkI2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI("#network_buttonI2")
       
     }
     
     insertUI("#box_st_table2", "afterEnd", ui = {
       box(width = 12,
           title = "STRING Interactions Table", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_st_table2")
       )
     })
     
     insertUI("#box_count_table2", "afterEnd", ui = {
       box(width = 12,
           title = "Interacting Orthogroups Table", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_count_table2")
       )
     })
     
     insertUI("#box_count_plot2", "afterEnd", ui = {
       box(width = 12,
           title = "Interacting Orthogroups Plot", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           fluidRow(column(1), imageOutput("count_plot2"))
       )
     })
     
     
     insertUI("#download_ui_for_st_table2", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadSTRINGTable2", "Download STRING Table",
                                                                          size = "sm", color = "success"))
     })
     
     insertUI("#download_ui_for_count_table2", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadCOUNTTable2", "Download OG Count Table",
                                                                          size = "sm", color = "success"))
     })
     
     insertUI("#count_down_button2", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "count_download2", "Download OG Count Plot",
                                                                          size = "sm", color = "success"))
     })
     
     UI_exist_string2 <<- TRUE
     
     # Remove previous results for STRING network
     if (UI_exist_network2)
     {
       removeUI(
         selector = "div:has(>>>> #network_image2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_network2 <<- F
     }
     
   })
   
   # Fill outputs
   # Render STRING table
   # output$output_st_table1 <- renderDataTable({
   #   phys_table <- phys_table1()
   #   #phys_table$type <- sapply(phys_table$type, color_string)
   #   datatable(phys_table) %>%
   #     formatStyle(
   #       'type',
   #       color = styleEqual(
   #         c("Direct interaction", "Interolog"), c('green', '#CA931B')
   #       )
   #       )
   # },escape=FALSE, rownames= F, options =list(pageLength = 10)) 
   
   
   output$output_st_table2 <- renderDataTable({
     phys_table <- phys_table2()
     #phys_table$type <- sapply(phys_table$type, color_string)
     datatable(phys_table, escape=FALSE, rownames= F, options =list(pageLength = 10)) %>%
       formatStyle(
         'type',
         color = styleEqual(
           c("Direct interaction", "Interolog"), c('green', '#CA931B')
         )
       )
   }) 
   
   
   # Render OG count table
   output$output_count_table2 <- renderDataTable({
     string_counts <- as.data.frame(string_counts2())
     colnames(string_counts) <- c("orthogroup", "count")
     string_counts
   },escape=FALSE, rownames= F, options =list(pageLength = 7))
   
   # Render OG count pie chart
   output$count_plot2 <- renderImage({
     
     string_count_plot <- string_count_plot2()
     
     png("string_count_plot2.png", height = 450, width = 450)
     plot(string_count_plot)
     dev.off()
     
     list(src = "string_count_plot2.png",
          contentType="image/png", width=400,height=400)
   }, deleteFile = T)
   
   
   # Download tab's outputs
   # Download STRING table
   output$downloadSTRINGTable2 <- downloadHandler(
     filename= function() {
       paste("string_table", ".tsv", sep="")
     },
     content= function(file) {
       phys_table <- phys_table2()
       write.table(x = phys_table,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Download count table
   output$downloadCOUNTTable2 <- downloadHandler(
     filename= function() {
       paste("string_count_table", ".tsv", sep="")
     },
     content= function(file) {
       string_counts <- string_counts2()
       write.table(x = string_counts,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Download count plot
   output$count_download2 <- downloadHandler(
     filename= function() {
       paste("string_count", ".png", sep="")
     },
     content= function(file) {
       string_count_plot <- string_count_plot2()
       
       png(file, height = 450, width = 450)
       plot(string_count_plot)
       dev.off()
     })
   
   # Create gene selector and button for STRING network representation
   observeEvent(input$string_selectionI2,{
     
     phys_table <- phys_table2()
     network_genes <- unique(phys_table$prot_query)
     
     # Error message if no genes are allowed for selection should have  been reported
     # earlier
     
     insertUI("#selected_network2", "afterEnd", ui = {
       
       shinyWidgets::pickerInput(inputId = "selected_networkI2", label = "Select the gene whose network you want to plot", 
                                 choices = network_genes, selected = network_genes[1],
                                 options = list(`actions-box` = TRUE), multiple = T)
       
     })
     
     
     insertUI("#network_button2", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("network_buttonI2", "Plot STRING Network", size = "sm",
                                style = "float", color = "success")
     })
     
     shinyjs::hideElement(id = 'loading.string2')
     
   })
   
   mapped_string2 <- reactive({
     
     # Load genes (PharaohFUN IDs) and convert to STRING IDs
     library(data.table)
     
     network_genes <- input$selected_networkI2
     map_table <- fread("pharaoh_folder/string_map.tsv")
     
     # Fast subset using data.table
     map_network <- map_table[pharaohfun_id %in% network_genes,]
     
     # Error if no genes from selection have an associated high fidelity STRING ID
     if (nrow(map_network) == 0)
     {
       
       if (UI_exist_network2)
       {
         removeUI(
           selector = "div:has(>>>> #network_image2)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_network2 <<- F
       }
       
       output$error_network2 <- renderUI({renderText({print("It's not possible to map any
                               genes from selection to STRING IDs, please select different ones.")})})
       validate( " ")
       
     }
     
     output$error_network2 <- NULL
     
     # Get only STRING IDS and paste in a format interpretable by JS function
     string_ids <- as.data.frame(map_network)$string_id
     
     
     mapped_string <- paste0(string_ids, collapse = "%0d")
     return(mapped_string)
     
   }) %>% bindEvent(input$network_buttonI2)
   
   # Create boxes
   observeEvent(isTruthy(mapped_string2()),{
     
     mapped_string <- mapped_string2()
     
     if (UI_exist_network2)
     {
       removeUI(
         selector = "div:has(>>>> #network_image2)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     url_interactive <- paste0("https://string-db.org/cgi/network?identifiers=", 
                               mapped_string, 
                               "&add_color_nodes=25&network_flavor=confidence&show_query_node_labels=1")
     
     insertUI("#box_output_network2", "afterEnd", ui = {
       box(width = 12,
           title = "Image", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           fluidRow(column(1), 
                    column(8, htmlOutput("network_image2")), 
                    column(3, div( style = "margin-top: 300px;", 
                                   shinyWidgets::actionBttn("network_link2", "Interactive Network", size = "md", 
                                                            icon = icon("circle-nodes"), style = "float", color = "success", 
                                                            onclick=paste0("window.open('", url_interactive,"','_blank')")))
                           
                    ))
           
       )
     })
     
     UI_exist_network2 <<- T
     
   })
   
   # Fill network box
   
   output$network_image2 <- renderText({
     
     mapped_string <- mapped_string2()
     src_map <- paste0("https://string-db.org/api/image/network?identifiers=", mapped_string, 
                       "&add_color_nodes=25&network_flavor=confidence")
     
     c('<img src="',src_map,'"width="675" height="625">')
   })
   
   
# End of Sequence-based search results 
   
   
     ############## ORTHOGROUP-BASED SEARCH ##############
   
   # Set global variables for tracking changes in output
   UI_exist_pfam3 <<- F
   UI_exist_tree3 <<- F
   UI_exist_phylo3 <<- F
   UI_exist_cafe3 <<- F
   UI_exist_error_cafe3 <<- F
   UI_exist_msa3 <<- F
   UI_exist_go3 <<- F
   UI_exist_kegg3 <<- F
   UI_exist_pathview3 <<- F
   UI_exist_lit3 <<-  F
   UI_exist_string3 <<-  F
   UI_exist_network3 <<-  F
   
   model.selected3 <- reactive({
     model.selected <- !input$switch3
     return(model.selected)
   })%>% bindEvent(input$run_button3)
   
   build_trees3 <- reactive({
     build_trees <- as.character(input$build_trees_3)
     return(build_trees)
   }) %>% bindEvent(input$run_button3)
   
   # Load organisms selection based on the model selected
   selected_organisms3 <- reactive({
     selected_organisms <- c(input$mami_check_3,input$chloro_check_3, input$strepto_check_3,
                             input$bryo_check_3, input$lyco_check_3, input$sperma_check_3)
     if(model.selected3()){selected_organisms <- c(input$tsar_check_3, input$rhodo_check_3, 
                                                   input$glauco_check_3,selected_organisms)}
     return(selected_organisms)
     
   }) %>% bindEvent(input$run_button3)
   
   selected_values_org3 <- reactive(organisms_values[selected_organisms3()]) %>% bindEvent(input$run_button3)
   
   
   og.name3 <- reactive({
     shinyjs::showElement(id = 'loading.tree3')
     
     gene.name.tree <- input$geneInt3
     
     # Load table with orthogroups information depending on selected model
     ortho.table.search <- ifelse(model.selected3(), "Global_Gene_Trees/Orthogroups.tsv",
                                  "Green_Gene_Trees/Orthogroups.tsv")
     ortho.table <- read.csv(ortho.table.search,
                             header = T, sep = "\t", as.is = T,
                             fill = T, blank.lines.skip = F)
     
     
     # Error if OG does not correspond to any of OG IDs available
     if (!(gene.name.tree %in% ortho.table$Orthogroup))
     {
       shinyjs::hideElement(id = 'loading.tree3')
       
       if (UI_exist_tree3)
       {
         removeUI(
           selector = "div:has(>> #treeTips3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>>> #presentorg3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_image3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTree3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadNewick3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTreeSeqs3)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       
       UI_exist_tree3 <<- F
       output$error_tree3 <- renderUI({renderText({print("No results for this 
      query due to not supported OG ID, check that it is correct.")})})
       validate(need(gene.name.tree %in% ortho.table$Orthogroup, " "))
       
     }
     
     return(gene.name.tree)
     
   }) %>% bindEvent(input$run_button3)
   
   tree3 <- reactive({
     file.name <- og.name3()
     # Load gene tree file depending on the input
     
     tree.name <- ifelse(model.selected3(),
                         paste("Global_Gene_Trees",paste(file.name, "tree.txt", sep = "_"), sep="/"),
                         paste("Green_Gene_Trees",paste(file.name, "tree.txt", sep = "_"), sep="/"))
     
     # Error if tree file not found
     if (!(file.exists(tree.name)))
     {
       shinyjs::hideElement(id = 'loading.tree3')
       
       if (UI_exist_tree3)
       {
         removeUI(
           selector = "div:has(>> #treeTips3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>>> #presentorg3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_image3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTree3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadNewick3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTreeSeqs3)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       
       UI_exist_tree3 <<- F
       output$error_tree3 <- renderUI({renderText({print("Unable to construct tree associated to
                                                        to an orthogroup with less than 4 genes.")})})
       validate(need(file.exists(tree.name), " "))
     }
     
     # Generate tree depending on the tree building method selected
     {
       if (build_trees3() == "Maximum Likelihood")
       {
         tree <- read.tree(tree.name)
         return(tree)
       }
       
       else
       {
         library(phangorn)
         
         # Read MSA
         multiple_file <- ifelse(model.selected3(),
                                 paste("Global_MultipleSequenceAlignments", paste0(file.name, ".fa"), sep="/"),
                                 paste("Green_MultipleSequenceAlignments", paste0(file.name, ".fa"), sep="/"))
         
         
         mytree <- read.phyDat(file = multiple_file,
                               format="fasta", type = "AA")
         
         # To keep the same workflow, the reduced tree will be calculated, then the applied subsetting
         # will only reduce them if the selected method has been fasttree
         
         
         # Selection of genes from the selected organism
         
         # Create empty vectors
         
         tips_to_keep.mp3 = tips_to_keep.at3 = tips_to_keep.ot3 = tips_to_keep.cp3 <- c()
         tips_to_keep.cr3 = tips_to_keep.cz3 = tips_to_keep.kn3 = tips_to_keep.me3 <- c()
         tips_to_keep.mi3 = tips_to_keep.pp3 = tips_to_keep.sl3 = tips_to_keep.sm3 <- c()
         tips_to_keep.sp3 = tips_to_keep.ta3 = tips_to_keep.vc3 = tips_to_keep.bp3 <- c()
         tips_to_keep.cri3 = tips_to_keep.ds3 = tips_to_keep.os3 = tips_to_keep.smag3 <- c()
         tips_to_keep.tp3 = tips_to_keep.aa3 = tips_to_keep.um3 = tips_to_keep.rs3 <- c()
         tips_to_keep.cyc3 = tips_to_keep.pu3 = tips_to_keep.pt3 = tips_to_keep.ng3 <- c()
         tips_to_keep.cyano3 = tips_to_keep.ca3 = tips_to_keep.mv3 = tips_to_keep.af3 <- c()
         tips_to_keep.sc3 = tips_to_keep.aegi3 = tips_to_keep.sb3 = tips_to_keep.chara3 <- c()
         tips_to_keep.guilla3 = tips_to_keep.crypto3 = tips_to_keep.cymero3 = tips_to_keep.galsul3 <- c()
         tips_to_keep.gracichor3 = tips_to_keep.sceobli3 = tips_to_keep.cocco3 = tips_to_keep.saccha3 <- c()
         tips_to_keep.haema3 = tips_to_keep.zm3 <- c()
         
         
         organisms.list <- c(selected_values_org3())
         
         
         # Fill vectors if organisms are in list and change names
         {
           if ("mp" %in% organisms.list)
           {
             tips_to_keep.mp3 <- grep(pattern = "marchantia", names(mytree)) 
           }
           
           if ("ot" %in% organisms.list)
           {
             tips_to_keep.ot3 <- grep(pattern = "ostreoco",names(mytree)) 
           }
           
           if ("at" %in% organisms.list)
           {
             tips_to_keep.at3 <- grep(pattern = "arabidopsis",names(mytree)) 
           }
           
           if ("cp" %in% organisms.list)
           {
             tips_to_keep.cp3 <- grep(pattern = "ceratodon",names(mytree)) 
           }
           
           if ("cr" %in% organisms.list)
           {
             tips_to_keep.cr3 <- grep(pattern = "chlamy",names(mytree))
           }
           
           if ("cz" %in% organisms.list)
           {
             tips_to_keep.cz3 <- grep(pattern = "chromochloris",names(mytree)) 
           }
           
           if ("kn" %in% organisms.list)
           {
             tips_to_keep.kn3 <- grep(pattern = "klebsormidium",names(mytree)) 
           }
           
           if ("me" %in% organisms.list)
           {
             tips_to_keep.me3 <- grep(pattern = "mesotaenium",names(mytree)) 
           }
           
           if ("mi" %in% organisms.list)
           {
             tips_to_keep.mi3 <- grep(pattern = "micromonas",names(mytree)) 
           }
           
           if ("pp" %in% organisms.list)
           {
             tips_to_keep.pp3 <- grep(pattern = "physcomitrium",names(mytree)) 
           }
           
           if ("sl" %in% organisms.list)
           {
             tips_to_keep.sl3 <- grep(pattern = "solanum",names(mytree)) 
           }
           
           if ("sm" %in% organisms.list)
           {
             tips_to_keep.sm3 <- grep(pattern = "selaginella",names(mytree))
           }
           
           if ("sp" %in% organisms.list)
           {
             tips_to_keep.sp3 <- grep(pattern = "spirogloea",names(mytree)) 
           }
           
           if ("ta" %in% organisms.list)
           {
             tips_to_keep.ta3 <- grep(pattern = "triticum",names(mytree)) 
           }
           
           if ("vc" %in% organisms.list)
           {
             tips_to_keep.vc3 <- grep(pattern = "volvox",names(mytree))
           }
           
           if ("bp" %in% organisms.list)
           {
             tips_to_keep.bp3 <- grep(pattern = "bathycoccus",names(mytree))
           }
           
           if ("cri" %in% organisms.list)
           {
             tips_to_keep.cri3 <- grep(pattern = "ceratopteris",names(mytree))
           }
           
           if ("ds" %in% organisms.list)
           {
             tips_to_keep.ds3 <- grep(pattern = "dunaliella",names(mytree))
           }
           
           if ("os" %in% organisms.list)
           {
             tips_to_keep.os3 <- grep(pattern = "oryza",names(mytree))
           }
           
           if ("smag" %in% organisms.list)
           {
             tips_to_keep.smag3 <- grep(pattern = "sphagnum",names(mytree))
           }
           
           if ("tp" %in% organisms.list)
           {
             tips_to_keep.tp3 <- grep(pattern = "thuja",names(mytree))
           }
           
           if ("aa" %in% organisms.list)
           {
             tips_to_keep.aa3 <- grep(pattern = "anthoceros",names(mytree))
           }
           
           if ("um" %in% organisms.list)
           {
             tips_to_keep.um3 <- grep(pattern = "ulva",names(mytree))
           }
           
           if ("rs" %in% organisms.list)
           {
             tips_to_keep.rs3 <- grep(pattern = "raphidocelis",names(mytree))
           }
           
           if ("cyc" %in% organisms.list)
           {
             tips_to_keep.cyc3 <- grep(pattern = "cycas",names(mytree))
           }
           
           if ("pu" %in% organisms.list)
           {
             tips_to_keep.pu3 <- grep(pattern = "porphyra",names(mytree))
           }
           
           if ("pt" %in% organisms.list)
           {
             tips_to_keep.pt3 <- grep(pattern = "phaeodactylum",names(mytree))
           }
           
           if ("ng" %in% organisms.list)
           {
             tips_to_keep.ng3 <- grep(pattern = "gaditana",names(mytree))
           }
           
           if ("cyano" %in% organisms.list)
           {
             tips_to_keep.cyano3 <- grep(pattern = "cyanophora",names(mytree))
           }
           
           if ("ca" %in% organisms.list)
           {
             tips_to_keep.ca3 <- grep(pattern = "chlorokybus",names(mytree))
           }
           
           if ("mv" %in% organisms.list)
           {
             tips_to_keep.mv3 <- grep(pattern = "mesostigma",names(mytree))
           }
           
           if ("af" %in% organisms.list)
           {
             tips_to_keep.af3 <- grep(pattern = "azolla",names(mytree))
           }
           
           if ("sc" %in% organisms.list)
           {
             tips_to_keep.sc3 <- grep(pattern = "salvinia",names(mytree))
           }
           
           if ("aegi" %in% organisms.list)
           {
             tips_to_keep.aegi3 <- grep(pattern = "aegilops",names(mytree))
           }
           
           if ("sb" %in% organisms.list)
           {
             tips_to_keep.sb3 <- grep(pattern = "sorghum",names(mytree))
           }
           
           if ("chara" %in% organisms.list)
           {
             tips_to_keep.chara3 <- grep(pattern = "chara",names(mytree))
           }
           
           if ("guilla" %in% organisms.list)
           {
             tips_to_keep.guilla3 <- grep(pattern = "guillardia",names(mytree))
           }
           
           if ("crypto" %in% organisms.list)
           {
             tips_to_keep.crypto3 <- grep(pattern = "cryptophyceae",names(mytree))
           }
           
           if ("cymero" %in% organisms.list)
           {
             tips_to_keep.cymero3 <- grep(pattern = "cyanidioschyzon",names(mytree))
           }
           
           if ("galsul" %in% organisms.list)
           {
             tips_to_keep.galsul3 <- grep(pattern = "galdieria",names(mytree))
           }
           
           if ("gracichor" %in% organisms.list)
           {
             tips_to_keep.gracichor3 <- grep(pattern = "gracilariopsis",names(mytree))
           }
           
           if ("sceobli" %in% organisms.list)
           {
             tips_to_keep.sceobli3 <- grep(pattern = "scenedesmus",names(mytree))
           }
           
           if ("cocco" %in% organisms.list)
           {
             tips_to_keep.cocco3 <- grep(pattern = "coccomyxa",names(mytree))
           }
           
           if ("saccha" %in% organisms.list)
           {
             tips_to_keep.saccha3 <- grep(pattern = "saccharina",names(mytree))
           }
           
           if ("haema" %in% organisms.list)
           {
             tips_to_keep.haema3 <- grep(pattern = "haematococcus",names(mytree))
           }
           
           if ("zm" %in% organisms.list)
           {
             tips_to_keep.zm3 <- grep(pattern = "mays",names(mytree))
           }
         }
         
         
         # Concatenate indexes to keep and subset MSA
         tips_to_keep.global <- c(tips_to_keep.mp3, tips_to_keep.ot3, tips_to_keep.at3, tips_to_keep.cp3,
                                  tips_to_keep.cr3, tips_to_keep.cz3, tips_to_keep.kn3, tips_to_keep.me3,
                                  tips_to_keep.mi3, tips_to_keep.pp3, tips_to_keep.sl3, tips_to_keep.sm3,
                                  tips_to_keep.sp3, tips_to_keep.ta3, tips_to_keep.vc3, tips_to_keep.bp3,
                                  tips_to_keep.cri3, tips_to_keep.ds3, tips_to_keep.os3, tips_to_keep.smag3,
                                  tips_to_keep.tp3, tips_to_keep.aa3, tips_to_keep.um3, tips_to_keep.rs3,
                                  tips_to_keep.cyc3, tips_to_keep.pu3, tips_to_keep.pt3, tips_to_keep.ng3,
                                  tips_to_keep.cyano3, tips_to_keep.ca3, tips_to_keep.mv3, tips_to_keep.af3,
                                  tips_to_keep.sc3, tips_to_keep.aegi3, tips_to_keep.sb3, tips_to_keep.chara3,
                                  tips_to_keep.guilla3, tips_to_keep.crypto3, tips_to_keep.cymero3, tips_to_keep.galsul3,
                                  tips_to_keep.gracichor3, tips_to_keep.sceobli3, tips_to_keep.cocco3, tips_to_keep.saccha3,
                                  tips_to_keep.haema3,tips_to_keep.zm3)
         
         
         my_subset_tree <- subset(mytree, tips_to_keep.global)
         
         {
           if (build_trees3() == "Neighbour Joining")
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
           
           else if (build_trees3() == "UPGMA")
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
           
           else if (build_trees3() == "Parsimony")
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
   }) %>% bindEvent(input$run_button3)
   
   ortho_seq3 <- reactive({
     file.name <- og.name3()
     
     # Load orthogroup sequences file
     ortho.seq.name <- ifelse(model.selected3(),
                              paste("Global_Orthogroup_Sequences",paste(file.name, "fa", sep = "."), sep="/"),
                              paste("Green_Orthogroup_Sequences",paste(file.name, "fa", sep = "."), sep="/"))
     
     ortho_seq <- seqinr::read.fasta(ortho.seq.name, seqtype = "AA")
     return(ortho_seq)
   })
   
   
   # Tips to keep of each species with proper notation
   tips_to_keep.mp3 <- reactive({
     
     tree <- tree3()
     # Selection of organisms
     organisms.list <- c(selected_values_org3())
     
     # Selection of genes from the selected organism
     tips_to_keep.mp <- c()
     if ("mp" %in% organisms.list)
     {
       tips_to_keep.mp <- grep(pattern = "marchantia",tree$tip.label)
     }
     return(tips_to_keep.mp)
   })
   
   tips_to_keep.ot3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.ot <- c()
     if ("ot" %in% organisms.list)
     {
       tips_to_keep.ot <- grep(pattern = "ostreoco",tree$tip.label)
     }
     return(tips_to_keep.ot)
   })
   
   tips_to_keep.at3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.at <- c()
     if ("at" %in% organisms.list)
     {
       tips_to_keep.at <- grep(pattern = "arabidopsis",tree$tip.label)
     }
     return(tips_to_keep.at)
   })
   
   tips_to_keep.cp3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.cp <- c()
     if ("cp" %in% organisms.list)
     {
       tips_to_keep.cp <- grep(pattern = "ceratodon",tree$tip.label)
     }
     
     return(tips_to_keep.cp)
   })
   
   tips_to_keep.cr3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.cr <- c()
     if ("cr" %in% organisms.list)
     {
       tips_to_keep.cr <- grep(pattern = "chlamy",tree$tip.label)
     }
     return(tips_to_keep.cr)
   })
   
   tips_to_keep.cz3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.cz <- c()
     if ("cz" %in% organisms.list)
     {
       tips_to_keep.cz <- grep(pattern = "chromochloris",tree$tip.label)
     }
     return(tips_to_keep.cz)
   })
   
   tips_to_keep.kn3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.kn <- c()
     if ("kn" %in% organisms.list)
     {
       tips_to_keep.kn <- grep(pattern = "klebsormidium",tree$tip.label)
     }
     return(tips_to_keep.kn)
   })
   
   tips_to_keep.me3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.me <- c()
     if ("me" %in% organisms.list)
     {
       tips_to_keep.me <- grep(pattern = "mesotaenium",tree$tip.label)
     }
     return(tips_to_keep.me)
   })
   
   tips_to_keep.mi3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.mi <- c()
     if ("mi" %in% organisms.list)
     {
       tips_to_keep.mi <- grep(pattern = "micromonas",tree$tip.label)
     }
     return(tips_to_keep.mi)
   })
   
   tips_to_keep.pp3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.pp <- c()
     if ("pp" %in% organisms.list)
     {
       tips_to_keep.pp <- grep(pattern = "physcomitrium",tree$tip.label)
     }
     return(tips_to_keep.pp)
   })
   
   tips_to_keep.sl3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.sl <- c()
     if ("sl" %in% organisms.list)
     {
       tips_to_keep.sl <- grep(pattern = "solanum",tree$tip.label)
     }
     return(tips_to_keep.sl)
   })
   
   tips_to_keep.sm3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.sm <- c()
     if ("sm" %in% organisms.list)
     {
       tips_to_keep.sm <- grep(pattern = "selaginella",tree$tip.label)
     }
     return(tips_to_keep.sm)
   })
   
   tips_to_keep.sp3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.sp <- c()
     if ("sp" %in% organisms.list)
     {
       tips_to_keep.sp <- grep(pattern = "spirogloea",tree$tip.label)
     }
     return(tips_to_keep.sp)
   })
   
   tips_to_keep.ta3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.ta <- c()
     if ("ta" %in% organisms.list)
     {
       tips_to_keep.ta <- grep(pattern = "triticum",tree$tip.label)
     }
     return(tips_to_keep.ta)
   })
   
   tips_to_keep.vc3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.vc <- c()
     if ("vc" %in% organisms.list)
     {
       tips_to_keep.vc <- grep(pattern = "volvox",tree$tip.label)
     }
     return(tips_to_keep.vc)
   })
   
   tips_to_keep.bp3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.bp <- c()
     if ("bp" %in% organisms.list)
     {
       tips_to_keep.bp <- grep(pattern = "bathycoccus",tree$tip.label)
     }
     return(tips_to_keep.bp)
   })
   
   tips_to_keep.cri3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.cri <- c()
     if ("cri" %in% organisms.list)
     {
       tips_to_keep.cri <- grep(pattern = "ceratopteris",tree$tip.label)
     }
     return(tips_to_keep.cri)
   })
   
   tips_to_keep.ds3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.ds <- c()
     if ("ds" %in% organisms.list)
     {
       tips_to_keep.ds <- grep(pattern = "dunaliella",tree$tip.label)
     }
     return(tips_to_keep.ds)
   })
   
   tips_to_keep.os3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.os <- c()
     if ("os" %in% organisms.list)
     {
       tips_to_keep.os <- grep(pattern = "oryza",tree$tip.label)
     }
     return(tips_to_keep.os)
   })
   
   tips_to_keep.smag3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.smag <- c()
     if ("smag" %in% organisms.list)
     {
       tips_to_keep.smag <- grep(pattern = "sphagnum",tree$tip.label)
     }
     return(tips_to_keep.smag)
   })
   
   tips_to_keep.tp3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.tp <- c()
     if ("tp" %in% organisms.list)
     {
       tips_to_keep.tp <- grep(pattern = "thuja",tree$tip.label)
     }
     return(tips_to_keep.tp)
   })
   
   tips_to_keep.aa3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.aa <- c()
     if ("aa" %in% organisms.list)
     {
       tips_to_keep.aa <- grep(pattern = "anthoceros",tree$tip.label)
     }
     return(tips_to_keep.aa)
   })
   
   tips_to_keep.um3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.um <- c()
     if ("um" %in% organisms.list)
     {
       tips_to_keep.um <- grep(pattern = "ulva",tree$tip.label)
     }
     return(tips_to_keep.um)
   })
   
   tips_to_keep.rs3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.rs <- c()
     if ("rs" %in% organisms.list)
     {
       tips_to_keep.rs <- grep(pattern = "raphidocelis",tree$tip.label)
     }
     return(tips_to_keep.rs)
   })
   
   tips_to_keep.cyc3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.cyc <- c()
     if ("cyc" %in% organisms.list)
     {
       tips_to_keep.cyc <- grep(pattern = "cycas",tree$tip.label)
     }
     return(tips_to_keep.cyc)
   })
   
   tips_to_keep.pu3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.pu <- c()
     if ("pu" %in% organisms.list)
     {
       tips_to_keep.pu <- grep(pattern = "porphyra",tree$tip.label)
     }
     return(tips_to_keep.pu)
   })
   
   tips_to_keep.pt3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.pt <- c()
     if ("pt" %in% organisms.list)
     {
       tips_to_keep.pt <- grep(pattern = "phaeodactylum",tree$tip.label)
     }
     return(tips_to_keep.pt)
   })
   
   tips_to_keep.ng3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.ng <- c()
     if ("ng" %in% organisms.list)
     {
       tips_to_keep.ng <- grep(pattern = "gaditana",tree$tip.label)
     }
     return(tips_to_keep.ng)
   })
   
   tips_to_keep.cyano3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.cyano <- c()
     if ("cyano" %in% organisms.list)
     {
       tips_to_keep.cyano <- grep(pattern = "cyanophora",tree$tip.label)
     }
     return(tips_to_keep.cyano)
   })
   
   tips_to_keep.ca3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.ca <- c()
     if ("ca" %in% organisms.list)
     {
       tips_to_keep.ca <- grep(pattern = "chlorokybus",tree$tip.label)
     }
     return(tips_to_keep.ca)
   })
   
   tips_to_keep.mv3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.mv <- c()
     if ("mv" %in% organisms.list)
     {
       tips_to_keep.mv <- grep(pattern = "mesostigma",tree$tip.label)
     }
     return(tips_to_keep.mv)
   })
   
   tips_to_keep.af3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.af <- c()
     if ("af" %in% organisms.list)
     {
       tips_to_keep.af <- grep(pattern = "azolla",tree$tip.label)
     }
     return(tips_to_keep.af)
   })
   
   tips_to_keep.sc3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.sc <- c()
     if ("sc" %in% organisms.list)
     {
       tips_to_keep.sc <- grep(pattern = "salvinia",tree$tip.label)
     }
     return(tips_to_keep.sc)
   })
   
   tips_to_keep.aegi3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.aegi <- c()
     if ("aegi" %in% organisms.list)
     {
       tips_to_keep.aegi <- grep(pattern = "aegilops",tree$tip.label)
     }
     return(tips_to_keep.aegi)
   })
   
   tips_to_keep.sb3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.sb <- c()
     if ("sb" %in% organisms.list)
     {
       tips_to_keep.sb <- grep(pattern = "sorghum",tree$tip.label)
     }
     return(tips_to_keep.sb)
   })
   
   tips_to_keep.chara3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.chara <- c()
     if ("chara" %in% organisms.list)
     {
       tips_to_keep.chara <- grep(pattern = "chara",tree$tip.label)
     }
     return(tips_to_keep.chara)
   })
   
   tips_to_keep.guilla3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.guilla <- c()
     if ("guilla" %in% organisms.list)
     {
       tips_to_keep.guilla <- grep(pattern = "guillardia",tree$tip.label)
     }
     return(tips_to_keep.guilla)
   })
   
   tips_to_keep.crypto3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.crypto <- c()
     if ("crypto" %in% organisms.list)
     {
       tips_to_keep.crypto <- grep(pattern = "cryptophyceae",tree$tip.label)
     }
     return(tips_to_keep.crypto)
   })
   
   tips_to_keep.cymero3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.cymero <- c()
     if ("cymero" %in% organisms.list)
     {
       tips_to_keep.cymero <- grep(pattern = "cyanidioschyzon",tree$tip.label)
     }
     return(tips_to_keep.cymero)
   })
   
   tips_to_keep.galsul3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.galsul <- c()
     if ("galsul" %in% organisms.list)
     {
       tips_to_keep.galsul <- grep(pattern = "galdieria",tree$tip.label)
     }
     return(tips_to_keep.galsul)
   })
   
   tips_to_keep.gracichor3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.gracichor <- c()
     if ("gracichor" %in% organisms.list)
     {
       tips_to_keep.gracichor <- grep(pattern = "gracilariopsis",tree$tip.label)
     }
     return(tips_to_keep.gracichor)
   })
   
   tips_to_keep.sceobli3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.sceobli <- c()
     if ("sceobli" %in% organisms.list)
     {
       tips_to_keep.sceobli <- grep(pattern = "scenedesmus",tree$tip.label)
     }
     return(tips_to_keep.sceobli)
   })
   
   tips_to_keep.cocco3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.cocco <- c()
     if ("cocco" %in% organisms.list)
     {
       tips_to_keep.cocco <- grep(pattern = "coccomyxa",tree$tip.label)
     }
     return(tips_to_keep.cocco)
   })
   
   tips_to_keep.saccha3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.saccha <- c()
     if ("saccha" %in% organisms.list)
     {
       tips_to_keep.saccha <- grep(pattern = "saccharina",tree$tip.label)
     }
     return(tips_to_keep.saccha)
   })
   
   tips_to_keep.haema3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.haema <- c()
     if ("haema" %in% organisms.list)
     {
       tips_to_keep.haema <- grep(pattern = "haematococcus",tree$tip.label)
     }
     return(tips_to_keep.haema)
   })
   
   tips_to_keep.zm3 <- reactive({
     
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     tips_to_keep.zm <- c()
     if ("zm" %in% organisms.list)
     {
       tips_to_keep.zm <- grep(pattern = "mays",tree$tip.label)
     }
     return(tips_to_keep.zm)
   }) %>% bindEvent(input$run_button3)
   
   
   # Create complete gene tree with the proper name for each gene
   # For this, we split the species name apart from the gene name
   tree_adj3 <- reactive({
     tree <- tree3()
     organisms.list <- c(selected_values_org3())
     
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
   }) %>% bindEvent(input$run_button3)
   
   # Generate reduced tree when the corresponding button is activated
   tree_reduced3 <- reactive({
     
     tree <- tree_adj3()
     # Define tips to keep (selected organisms) and generate the reduced tree
     tips_to_keep.global <- c(tips_to_keep.mp3(), tips_to_keep.ot3(), tips_to_keep.at3(), tips_to_keep.cp3(),
                              tips_to_keep.cr3(), tips_to_keep.cz3(), tips_to_keep.kn3(), tips_to_keep.me3(),
                              tips_to_keep.mi3(), tips_to_keep.pp3(), tips_to_keep.sl3(), tips_to_keep.sm3(),
                              tips_to_keep.sp3(), tips_to_keep.ta3(), tips_to_keep.vc3(), tips_to_keep.bp3(),
                              tips_to_keep.cri3(), tips_to_keep.ds3(), tips_to_keep.os3(), tips_to_keep.smag3(),
                              tips_to_keep.tp3(), tips_to_keep.aa3(), tips_to_keep.um3(), tips_to_keep.rs3(),
                              tips_to_keep.cyc3(), tips_to_keep.pu3(), tips_to_keep.pt3(), tips_to_keep.ng3(),
                              tips_to_keep.cyano3(), tips_to_keep.ca3(), tips_to_keep.mv3(), tips_to_keep.af3(),
                              tips_to_keep.sc3(), tips_to_keep.aegi3(), tips_to_keep.sb3(), tips_to_keep.chara3(),
                              tips_to_keep.guilla3(), tips_to_keep.crypto3(), tips_to_keep.cymero3(), tips_to_keep.galsul3(),
                              tips_to_keep.gracichor3(), tips_to_keep.sceobli3(), tips_to_keep.cocco3(), tips_to_keep.saccha3(),
                              tips_to_keep.haema3(),tips_to_keep.zm3())
     
     # Error message if trying to build tree with less than two tips
     if (length(tips_to_keep.global) < 2)
     {
       shinyjs::hideElement(id = 'loading.tree3')
       
       if (UI_exist_tree3)
       {
         removeUI(
           selector = "div:has(>> #treeTips3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>>> #presentorg3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_image3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTree3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadNewick3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTreeSeqs3)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       
       UI_exist_tree3 <<- F
       output$error_tree3 <- renderUI({renderText({print("Unable to construct 
      tree with a single tip, please select more organisms.")})})
       validate(need(length(tips_to_keep.global) > 1, " "))
     }
     
     
     # Error message if query gene does not belong to selected organisms
     
     tips_to_drop <- setdiff(1:length(tree$tip.label), tips_to_keep.global)
     tree_reduced <- drop.tip(tree, tips_to_drop)
     
     return(tree_reduced)
   }) %>% bindEvent(input$run_button3)
   
   ### Select orthogroup sequences based on the reduced tree
   ortho_reduced3 <- reactive({
     
     tree_reduced <- tree_reduced3()
     ortho_seq <- ortho_seq3()
     ortho_reduced <- ortho_seq[tree_reduced$tip.label]
     return(ortho_reduced)
   }) %>% bindEvent(input$run_button3)
   
   ### Select orthogroup sequences based on the reduced tree
   organims_reduced3 <- reactive({
     
     tree_reduced <- tree_reduced3()
     
     len.mp <- length(tips_to_keep.mp3())
     len.ot <- length(tips_to_keep.ot3())
     len.at <- length(tips_to_keep.at3())
     len.cp <- length(tips_to_keep.cp3())
     len.cr <- length(tips_to_keep.cr3())
     len.cz <- length(tips_to_keep.cz3())
     len.kn <- length(tips_to_keep.kn3())
     len.me <- length(tips_to_keep.me3())
     len.mi <- length(tips_to_keep.mi3())
     len.pp <- length(tips_to_keep.pp3())
     len.sl <- length(tips_to_keep.sl3())
     len.sm <- length(tips_to_keep.sm3())
     len.sp <- length(tips_to_keep.sp3())
     len.ta <- length(tips_to_keep.ta3())
     len.vc <- length(tips_to_keep.vc3())
     len.bp <- length(tips_to_keep.bp3())
     len.cri <- length(tips_to_keep.cri3())
     len.ds <- length(tips_to_keep.ds3())
     len.os <- length(tips_to_keep.os3())
     len.smag <- length(tips_to_keep.smag3())
     len.tp <- length(tips_to_keep.tp3())
     len.aa <- length(tips_to_keep.aa3())
     len.um <- length(tips_to_keep.um3())
     len.rs <- length(tips_to_keep.rs3())
     len.cyc <- length(tips_to_keep.cyc3())
     len.pu <- length(tips_to_keep.pu3())
     len.pt <- length(tips_to_keep.pt3())
     len.ng <- length(tips_to_keep.ng3())
     len.cyano <- length(tips_to_keep.cyano3())
     len.ca <- length(tips_to_keep.ca3())
     len.mv <- length(tips_to_keep.mv3())
     len.af <- length(tips_to_keep.af3())
     len.sc <- length(tips_to_keep.sc3())
     len.aegi <- length(tips_to_keep.aegi3())
     len.sb <- length(tips_to_keep.sb3())
     len.chara <- length(tips_to_keep.chara3())
     len.guilla <- length(tips_to_keep.guilla3())
     len.crypto <- length(tips_to_keep.crypto3())
     len.cymero <- length(tips_to_keep.cymero3())
     len.galsul <- length(tips_to_keep.galsul3())
     len.gracichor <- length(tips_to_keep.gracichor3())
     len.sceobli <- length(tips_to_keep.sceobli3())
     len.cocco <- length(tips_to_keep.cocco3())
     len.saccha <- length(tips_to_keep.saccha3())
     len.haema <- length(tips_to_keep.haema3())
     len.zea <- length(tips_to_keep.zm3())
     
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
   }) %>% bindEvent(input$run_button3)
   
   tree_plot3 <- reactive({
     
     # Define previous variables
     tree_reduced <- tree_reduced3()
     gene.name.tree <- input$geneInt3
     tree <- tree_adj3()
     
     tips_to_keep.mp <- tips_to_keep.mp3()
     tips_to_keep.ot <- tips_to_keep.ot3()
     tips_to_keep.at <- tips_to_keep.at3()
     tips_to_keep.cp <- tips_to_keep.cp3()
     tips_to_keep.cr <- tips_to_keep.cr3()
     tips_to_keep.cz <- tips_to_keep.cz3()
     tips_to_keep.kn <- tips_to_keep.kn3()
     tips_to_keep.me <- tips_to_keep.me3()
     tips_to_keep.mi <- tips_to_keep.mi3()
     tips_to_keep.pp <- tips_to_keep.pp3()
     tips_to_keep.sl <- tips_to_keep.sl3()
     tips_to_keep.sm <- tips_to_keep.sm3()
     tips_to_keep.sp <- tips_to_keep.sp3()
     tips_to_keep.ta <- tips_to_keep.ta3()
     tips_to_keep.vc <- tips_to_keep.vc3()
     tips_to_keep.bp <- tips_to_keep.bp3()
     tips_to_keep.cri <- tips_to_keep.cri3()
     tips_to_keep.ds <- tips_to_keep.ds3()
     tips_to_keep.os <- tips_to_keep.os3()
     tips_to_keep.smag <- tips_to_keep.smag3()
     tips_to_keep.tp <- tips_to_keep.tp3()
     tips_to_keep.aa <- tips_to_keep.aa3()
     tips_to_keep.um <- tips_to_keep.um3()
     tips_to_keep.rs <- tips_to_keep.rs3()
     tips_to_keep.cyc <- tips_to_keep.cyc3()
     tips_to_keep.pu <- tips_to_keep.pu3()
     tips_to_keep.pt <- tips_to_keep.pt3()
     tips_to_keep.ng <- tips_to_keep.ng3()
     tips_to_keep.cyano <- tips_to_keep.cyano3()
     tips_to_keep.ca <- tips_to_keep.ca3()
     tips_to_keep.mv <- tips_to_keep.mv3()
     tips_to_keep.af <- tips_to_keep.af3()
     tips_to_keep.sc <- tips_to_keep.sc3()
     tips_to_keep.aegi <- tips_to_keep.aegi3()
     tips_to_keep.sb <- tips_to_keep.sb3()
     tips_to_keep.chara <- tips_to_keep.chara3()
     tips_to_keep.guilla <- tips_to_keep.guilla3()
     tips_to_keep.crypto <- tips_to_keep.crypto3()
     tips_to_keep.cymero <- tips_to_keep.cymero3()
     tips_to_keep.galsul <- tips_to_keep.galsul3()
     tips_to_keep.gracichor <- tips_to_keep.gracichor3()
     tips_to_keep.sceobli <- tips_to_keep.sceobli3()
     tips_to_keep.cocco <- tips_to_keep.cocco3()
     tips_to_keep.saccha <- tips_to_keep.saccha3()
     tips_to_keep.haema <- tips_to_keep.haema3()
     tips_to_keep.zm <- tips_to_keep.zm3()
     
     if (length(tree_reduced$tip.label) < 2)
     {
       cat("")
     }
     else 
     {
       
       # Color asignment per species
       col.factor <- c()
       org.factor <- c()
       
       library(glue)
       library(ggtree)
       library(ggplot2)
       
       for (i in 1:length(tree_reduced$tip.label))
       {
         if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mp])
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
         if (build_trees3() == "Maximum Likelihood")
         {
           tree_plot <- ggtree(tree_reduced) %<+% d2 + geom_tiplab() + theme(legend.position =) +
             xlim(0, max(tree_reduced$edge.length)*3) + geom_tiplab(aes(label = label, color = organism)) +
             scale_color_manual(values = unique(d2$col), breaks = unique(d2$org)) +
             labs(fill = "Node of interest")
           
         }
         else
         {
           tree_plot <- ggtree(tree_reduced) %<+% d2 + geom_tiplab() + theme(legend.position =) +
             xlim(0, max(tree_reduced$edge.length)*3) + geom_tiplab(aes(label = label, color = organism)) +
             geom_nodelab() +
             scale_color_manual(values = unique(d2$col), breaks = unique(d2$org)) +
             labs(fill = "Node of interest")
         }
       }
       shinyjs::hideElement(id = 'loading.tree3')
       return(tree_plot)
     }}) %>% bindEvent(input$run_button3)
   
   
   # Outputs
   observeEvent(isTruthy(tree_plot3()), {
     output$error_tree3 <- NULL
   })
   
   # Create boxes
   observeEvent(isTruthy(tree_plot3()), {
     
     if (UI_exist_tree3)
     {
       removeUI(
         selector = "div:has(>> #treeTips3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>>> #presentorg3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #tree_image3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadTree3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadNewick3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadTreeSeqs3)",
         multiple = TRUE,
         immediate = TRUE
       )
     }
     
     
     insertUI("#box_tree_text3", "afterEnd", ui = {
       box(
         title = "Genes in Orthogroup", status = "danger", solidHeader = TRUE, width = 12,
         collapsible = TRUE,
         verbatimTextOutput("treeTips3")
       )
     }) 
     
     insertUI("#box_tree_pie3", "afterEnd", ui = {
       box(
         title = "Present Organisms", status = "danger", solidHeader = TRUE,
         collapsible = TRUE, width = 12,
         fluidRow(column(1), imageOutput("presentorg3"))
       )
     }) 
     
     insertUI("#box_tree_plot3", "afterEnd", ui = {
       box(width = 12,
           title = "Gene Tree", status = "danger", solidHeader = TRUE,
           collapsible = TRUE, 
           plotOutput("tree_image3", height = 500)
       )
     })
     
     insertUI("#download_tree3", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadTree3", 
                                                                          "Download Tree Plot",
                                                                          size = "sm", color = "danger"))
     })
     
     insertUI("#download_newick3", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadNewick3", 
                                                                          "Download NEWICK Tree",
                                                                          size = "sm", color = "danger"))
     })
     
     insertUI("#download_tree_seqs3", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadTreeSeqs3", 
                                                                          "Download Protein Sequences",
                                                                          size = "sm", color = "danger"))
     })
     
     UI_exist_tree3 <<- TRUE
     shinyjs::hideElement(id = 'loading.tree3')
   })
   
   # Fill boxes with output
   output$treeTips3 <- renderPrint({
     print(tree_reduced3()$tip.label)
   }, width = 400) # %>% bindEvent(input$run_button3)
   
   # Render pie chart
   output$presentorg3 <- renderImage({
     
     {library(ggplot2)
       library(dplyr)
       
       data <- data.frame(table(organims_reduced3()))
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
         scale_fill_manual(values = rep(RColorBrewer::brewer.pal(n = 9, name = "Set1"), 
                                        floor(nrow(data)/9)+1))}
     
     png("organims_reduced3.png", height = 450, width = 450)
     plot(a)
     dev.off()
     
     list(src = "organims_reduced3.png",
          contentType="image/png", width=400,height=400)
   }, deleteFile = T)
   
   # Render tree image
   output$tree_image3 <- renderImage({
     png("tree3.png", height = 500, width = 1000)
     plot(tree_plot3())
     dev.off()
     
     list(src = "tree3.png",
          contentType="image/png", width=1000,height=500)
   }, deleteFile = T)
   
   # Download results
   output$downloadTree3 <- downloadHandler(
     filename= function() {
       paste("tree", ".png", sep="")
     },
     content= function(file) {
       image_height <- 300 + 11*length(tree_reduced3()$tip.label)
       image_width <- 200 + 400*max(tree_reduced3()$edge.length)
       png(file, height = image_height, width = image_width)
       plot(tree_plot3())
       dev.off()
     })
   
   # Create and download tree in newick format
   output$downloadNewick3 <- downloadHandler(
     filename= function() {
       paste("tree_newick", ".txt", sep="")
     },
     content= function(file) {
       write.tree(tree_reduced3(), file)
     })
   
   #  # Create and download sequences for genes in tree
   output$downloadTreeSeqs3 <- downloadHandler(
     filename= function() {
       paste("tree_seqs", ".fa", sep="")
     },
     content= function(file) {
       seqinr::write.fasta(sequences = seqinr::getSequence(ortho_reduced3()),
                           names = seqinr::getName(ortho_reduced3()), file.out = file)
     })
   
   
   ####################### PHYLOWIDGET ############################
   # Remove previous outputs when updated by a new search
   observeEvent(input$run_button3, {
     if (UI_exist_phylo3)
     {
       removeUI(
         selector = "div:has(>>> #phylo_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_phylo3 <<- F
     }
     
   })
   
   phylo_tree3 <- reactive({
     
     library(ape)
     tree_phylo <- tree_reduced3()
     
     # Normalize tree depth
     root_id <- length(tree_phylo$tip.label)+1
     norm_factor <- max(dist.nodes(tree_phylo)[root_id,])
     tree_phylo$edge.length <- tree_phylo$edge.length/norm_factor
     
     return(tree_phylo)
     
   }) %>% bindEvent(input$phylo_start3)
   
   observeEvent(isTruthy(phylo_tree3()),{
     
     if(UI_exist_phylo3)
     {
       removeUI(
         selector = "div:has(>>> #phylo_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_phylo3 <<- F
     }
     
     
     insertUI("#box_phylo3", "afterEnd", ui = {
       phylo_tree <- phylo_tree3()
       phylo_height <- length(phylo_tree$tip.label) *14 + 220
       box(width = 12,
           title = "Interactive Tree", status = "danger", solidHeader = TRUE, height = phylo_height + 100,
           collapsible = TRUE,
           tags$div(id = "phylo_pocket3", style = paste0("width: 1300px; height: ",  phylo_height + 50, "px"),
                    phylowidgetOutput("phylo_plot3", height = paste0(phylo_height,"px"), width = "98%"))
       )
     })
     
     UI_exist_phylo3 <<- T
     
   })
   
   output$phylo_plot3 <- renderPhylowidget({
     
     phylo_tree <- phylo_tree3()
     phylowidget(phylo_tree)
   })
   
   #########################  PFAM  ###############################
   
   observeEvent(input$run_button3, {
     removeUI(
       selector = "div:has(>> #selected_pfamsI3)",
       multiple = TRUE,
       immediate = TRUE
     )
   })
   
   observeEvent(input$pfam_start3, {
     insertUI("#selected_pfams3", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_pfamsI3","Select the desired genes from the tree",
                                 choices=isolate({tree_reduced3()$tip.label}), options = list(`actions-box` = TRUE),
                                 multiple = T, selected = isolate({tree_reduced3()$tip.label[1]}))
     })
   })
   
   observeEvent(input$run_button3, {
     removeUI("#pfam_selection3")
   })
   
   observeEvent(input$pfam_start3, {
     insertUI("#pfam_selectionI3", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("pfam_selection3", "Show Pfam Domains", size = "sm",
                                style = "float", color = "danger")
     })
   })
   
   
   observeEvent(input$run_button3, {
     if (UI_exist_pfam3)
     {
       removeUI(
         selector = "div:has(>> #output_pfam_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_download3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadPFAMTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_pfam3 <<- F
     }
   })
   
   
   total_table_pfam3 <- reactive({
     shinyjs::showElement(id = 'loading.pfam.pf3')
     ortho_reduced <- ortho_reduced3()
     sel_genes <- as.vector(input$selected_pfamsI3)
     
     if (length(sel_genes) < 1)
     {
       shinyjs::hideElement(id = 'loading.pfam.pf3')
       removeUI(
         selector = "div:has(>> #output_pfam_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_download3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadPFAMTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       UI_exist_pfam3 <<- F
       output$error_pfam3 <- renderUI({renderText({print("Please select at least one gene.")})})
       validate(need(length(sel_genes) > 0, "Please select at least one gene."))
     }
     
     output$error_pfam3 <- NULL

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
             shinyjs::hideElement(id = 'loading.pfam.pf3')
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
     
   }) %>% bindEvent(input$pfam_selection3)
   
   pfplot3 <- reactive({
     
     total_table_pfam <- total_table_pfam3()
     
     pfplot <- draw_canvas(total_table_pfam)
     pfplot <- draw_chains(pfplot, total_table_pfam)
     pfplot <- draw_domains(pfplot, total_table_pfam, label_domains = F)
     pfplot <- pfplot + theme_bw(base_size = 20) + # white background
       theme(panel.grid.minor=element_blank(),
             panel.grid.major=element_blank()) +
       theme(axis.ticks = element_blank(),
             axis.text.y = element_blank()) +
       theme(panel.border = element_blank())
     pfplot <- pfplot + theme(legend.position="top") + labs(fill="")
     
   }) %>% bindEvent(input$pfam_selection3)
   
   
   # Outputs
   
   observeEvent(isTruthy(pfplot3()), {
     
     if (UI_exist_pfam3)
     {
       removeUI(
         selector = "div:has(>> #output_pfam_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_download3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadPFAMTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_pfam3", "afterEnd", ui = {
       
       box(
         title = "PFAM Table", status = "danger", solidHeader = TRUE, width = 12,
         collapsible = TRUE,
         dataTableOutput(outputId = "output_pfam_table3"))
     }) 
     
     insertUI("#box_pfplot3", "afterEnd", ui = {
       total_table_pfam <- total_table_pfam3()
       box_pfplot_height <- 150 + 700*length(total_table_pfam$order[nrow(total_table_pfam)])
       box(
         title = "Domains Localization", status = "danger", solidHeader = TRUE, width = 12, #height = box_pfplot_height,
         collapsible = TRUE,
         plotOutput("pfam_plot3", height = box_pfplot_height))
     }) 
     
     insertUI("#pfam_down_button3", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "pfam_download3", "Download PFAM figure",
                                                                          size = "sm", color = "danger"))
     })
     
     insertUI("#download_ui_for_pfam_table3", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadPFAMTable3", "Download PFAM Table",
                                                                          size = "sm", color = "danger"))
     })
     
     UI_exist_pfam3 <<- TRUE
     shinyjs::hideElement(id = 'loading.pfam.pf3')
   })
   
   output$output_pfam_table3 <- renderDataTable({
     total_table_pfam <- total_table_pfam3()
     out_pf_table <- subset(total_table_pfam[,c(1:6,8)], total_table_pfam$type != "CHAIN")
     out_pf_table$description <- sapply(out_pf_table$description, function(x) pfam.link(x))
     colnames(out_pf_table) <- c(colnames(total_table_pfam)[1:6],"biological description")
     return(out_pf_table)
   }, escape=FALSE, options = list(pageLength = 5))
   
   output$pfam_plot3 <- renderImage({
     total_table_pfam <- total_table_pfam3()
     pfam_height <- 50 + 700*length(total_table_pfam$order[nrow(total_table_pfam)])
     pfam_width <- 1000
     pfplot <- pfplot3()
     png("pharaoh_folder/pfam.png",  width = pfam_width, height = pfam_height)
     plot(pfplot)
     dev.off()
     list(src = "pharaoh_folder/pfam.png",
          contentType="image/png")
   }, deleteFile = T
   )
   
   # Download results
   
   output$pfam_download3 <- downloadHandler(
     filename= function() {
       paste("pfam", ".png", sep="")
     },
     content= function(file) {
       total_table_pfam <- total_table_pfam3()
       pfam_height <- 50 + 700*length(total_table_pfam$order[nrow(total_table_pfam)])
       pfam_width <- 1150
       pfplot <- pfplot3()
       
       png(file, height = pfam_height, width = pfam_width)
       plot(pfplot)
       dev.off()
     })
   
   output$downloadPFAMTable3 <- downloadHandler(
     filename= function() {
       paste("pfam_table", ".tsv", sep="")
     },
     content= function(file) {
       total_table_pfam <- total_table_pfam3()
       out_pf_table <- subset(total_table_pfam[,c(1:6,8)], total_table_pfam$type != "CHAIN")
       colnames(out_pf_table) <- c(colnames(total_table_pfam)[1:6],"biological description")
       write.table(x = out_pf_table, quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   ####################### CAFE #################################
   
   observeEvent(input$run_button3, {
     if (UI_exist_cafe3)
     {
       removeUI(
         selector = "div:has(>> #cafe_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #cafe_mrca3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #cafe_download3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadCAFEPlot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_cafe3 <<- F
     }
     
     if (UI_exist_error_cafe3)
     {
       removeUI(
         selector = "div:has(>> #cafe_error_message3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_error_cafe3 <<- F
     }
   })
   
   ### CAFE parser and tree generator
   cafe_tree3 <- reactive({
     
     shinyjs::showElement(id = 'loading.cafe3')
     
     library(ape)
     
     # Import OG name
     og.cafe <- og.name3()
     
     # Define path to CAFE trees file
     cafe_comp_tree_file <- ifelse(model.selected3(), "pharaoh_folder/global_cafe.tre",
                                   "pharaoh_folder/green_cafe.tre")
     
     # Extract CAFE tree for current OG
     cafe.tree.set <- ape::read.nexus(cafe_comp_tree_file)
     cafe.tree <- cafe.tree.set[[og.cafe]]
     
     if (length(cafe.tree) < 1)
     {
       shinyjs::hideElement(id = 'loading.cafe3')
       if (UI_exist_cafe3)
       {
         removeUI(
           selector = "div:has(>> #cafe_plot3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #cafe_mrca3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #cafe_download3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadCAFEPlot3)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       
       UI_exist_cafe3 <<- F
       
       if(UI_exist_error_cafe3)
       {
         removeUI(
           selector = "div:has(>> #cafe_error_message3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
       }
       insertUI("#error_cafe3", "afterEnd", ui = {
         box(width = 12,
             title = "Ancestral State Reconstruction", status = "danger", solidHeader = TRUE,
             collapsible = TRUE,
             textOutput("cafe_error_message3"))
       })
       
       output$cafe_error_message3 <- renderText({print("No expansions/contraction detected for this orthogroup,
                                                        or infeasible computation due to large size and variance across
                                                        species.")})
       UI_exist_error_cafe3 <<- T
       
       validate(need(length(cafe.tree) > 0 , ""))
     }
     
     return(cafe.tree)
   }) %>% bindEvent(input$cafe_start3)
   
   evo_plot3 <- reactive({
     
     og.cafe <- og.name3()
     cafe.tree <- cafe_tree3()
     
     # Show an error if the orthogroup is not significantly expanded/collapsed in any branch
     
     model.node.number <- ifelse(model.selected3(), 46, 36)
     total.model.node.number <- ifelse(model.selected3(), 91, 71)
     
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
     
     mrca.tree <- read.tree(ifelse(model.selected3(), "pharaoh_folder/species_tree_global.txt",
                                   "pharaoh_folder/species_tree_green.txt"))
     
     node.names <- read.csv(ifelse(model.selected3(), "pharaoh_folder/tax_labels_global.tsv",
                                   "pharaoh_folder/tax_labels_green.tsv"), header = F, sep="\t")
     
     mrca.tree$node.label <- node.names$V2
     
     # Create a timeline for a given OG
     
     tree.name <- ifelse(model.selected3(),
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
     
   }) %>% bindEvent(input$cafe_start3)
   
   evo.paths.id3 <- reactive({
     
     # Create phylogenomic tree with internal nodes names
     og.cafe <- og.name3()
     model.node.number <- ifelse(model.selected3(), 46, 36)
     
     mrca.tree <- read.tree(ifelse(model.selected3(), "pharaoh_folder/species_tree_global.txt",
                                   "pharaoh_folder/species_tree_green.txt"))
     
     node.names <- read.csv(ifelse(model.selected3(), "pharaoh_folder/tax_labels_global.tsv",
                                   "pharaoh_folder/tax_labels_green.tsv"), header = F, sep="\t")
     
     mrca.tree$node.label <- node.names$V2
     
     # Create timeline
     tree.name <- ifelse(model.selected3(),
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
     
   }) %>% bindEvent(input$cafe_start3)
   
   # Outputs
   
   # Remove previous boxes if they exist and create new ones
   observeEvent(isTruthy(evo_plot3()), {
     
     if (UI_exist_cafe3)
     {
       removeUI(
         selector = "div:has(>> #cafe_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #cafe_mrca3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #cafe_download3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadCAFEPlot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     if (UI_exist_error_cafe3)
     {
       removeUI(
         selector = "div:has(>> #cafe_error_message3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_cafe3", "afterEnd", ui = {
       box(width = 12,
           title = "Ancestral State Reconstruction", status = "danger", solidHeader = TRUE,
           collapsible = TRUE,
           imageOutput("cafe_plot3", height = 500, width = 1000))
     })
     
     insertUI("#box_mrca3", "afterEnd", ui = {
       box(width = 8,
           title = "Most Recent Common Ancestor", status = "danger", solidHeader = TRUE,
           collapsible = TRUE,
           textOutput("cafe_mrca3")
       )
     })
     
     insertUI("#cafe_down_button3", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "cafe_download3", "Download NEWICK tree",
                                                                          size = "sm", color = "danger"))
     })
     
     insertUI("#download_ui_for_cafe_plot3", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadCAFEPlot3", "Download Ancestral State Plot",
                                                                          size = "sm", color = "danger"))
     })
     
     UI_exist_cafe3 <<- TRUE
     shinyjs::hideElement(id = 'loading.cafe3')
   })
   
   # Fill outputs
   
   output$cafe_plot3 <- renderImage({
     png("evo_plot3.png", height = 500, width = 1000)
     plot(evo_plot3())
     dev.off()
     
     list(src = "evo_plot3.png",
          contentType="image/png", width=1000,height=500)
   }, deleteFile = T)
   
   output$cafe_mrca3 <- renderText({
     print(paste0("Most recent common ancestor for this orthogroup is the
                   ancestor of the clade: ", evo.paths.id3()[1]))
   })
   
   # Download tab's results
   
   output$cafe_download3 <- downloadHandler(
     filename= function() {
       paste("ancestral_newick", ".txt", sep="")
     },
     content= function(file) {
       cafe_tree <- cafe_tree3()
       
       write.tree(cafe_tree, file)
     })
   
   output$downloadCAFEPlot3<- downloadHandler(
     filename= function() {
       paste("ancestral_plot", ".png", sep="")
     },
     content= function(file) {
       evo_plot <- evo_plot3()
       
       png(file)
       plot(evo_plot)
       dev.off()
     }) 
   
   ####################### MSA #################################
   
   observeEvent(input$run_button3, {
     removeUI(
       selector = "div:has(>> #selected_msaI3)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI(
       selector = "div:has(>> #msa_methodI3)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#msa_selection3")
     
     if (UI_exist_msa3)
     {
       removeUI(
         selector = "div:has(>>> #msa_print3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_fa3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_msa3 <<- F
     }
     
   })
   
   observeEvent(input$msa_start3, {
     insertUI("#selected_msa3", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_msaI3","Select the desired genes from the tree to align",
                                 choices=isolate({tree_reduced3()$tip.label}), options = list(`actions-box` = TRUE),
                                 multiple = T, selected = isolate({tree_reduced3()$tip.label[1]}))
       
     })
     
     insertUI("#msa_method3", "afterEnd", ui = {
       shinyWidgets::pickerInput(inputId = "msa_methodI3", label = "Choose alignment method", 
                                 choices = c("ClustalOmega", "MAFFT"), selected = "ClustalOmega")
       
     })
     
     insertUI("#msa_selectionI3", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("msa_selection3", "Align Sequences", size = "sm",
                                style = "float", color = "danger")
     })
     
   })
   
   alignseqs3 <- reactive({
     
     library(msa)
     shinyjs::showElement(id = 'loading.msa3')
     
     selected_genes <- as.vector(input$selected_msaI3)
     selected_method <- as.character(input$msa_methodI3)
     file.name <- og.name3()
     
     if (length(selected_genes) < 2)
     {
       shinyjs::hideElement(id = 'loading.msa3')
       
       if (UI_exist_msa3)
       {
         removeUI(
           selector = "div:has(>>> #msa_print3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #msa_download_plot3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #msa_download_fa3)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       UI_exist_msa3 <<- F
       output$error_msa3 <- renderUI({renderText({print("Please select at least two genes.")})})
       validate(need(length(selected_genes) > 1, "Please select at least two genes."))
     }
     
     output$error_msa3 <- NULL
     
     # If de novo alignment is selected
     {
       if(selected_method == "ClustalOmega")
       {
         # Define path to orthogroup sequences file
         ortho.seq.name <- ifelse(model.selected3(),
                                  paste("Global_Orthogroup_Sequences", paste(file.name, "fa", sep = "."), sep="/"),
                                  paste("Green_Orthogroup_Sequences", paste(file.name, "fa", sep = "."), sep="/"))
         
         
         # Read orthogroup sequences file and select the genes for alignment
         mySequences1 <- Biostrings::readAAStringSet(ortho.seq.name)
         mysubseqs <- mySequences1[selected_genes]
         
         alignseqs <- msa(mysubseqs, verbose = F, method = "ClustalOmega")
       }
       
       # If MAFFT alignment is selected
       else
       {
         ortho.seq.name <- ifelse(model.selected3(),
                                  paste("Global_MultipleSequenceAlignments", paste(file.name, "fa", sep = "."), sep="/"),
                                  paste("Green_MultipleSequenceAlignments", paste(file.name, "fa", sep = "."), sep="/"))
         mySequences1 <- seqinr::read.fasta(ortho.seq.name, seqtype = "AA")
         mysubseqs <- mySequences1[selected_genes]
         mysubnames <- seqinr::getName(mySequences1)
         
         # Identify indexes associated with reduced names
         indexes_msa <- sapply(selected_genes, function(x) grep(mysubnames, pattern = x))
         
         # Retrieve those sequences from alignment keeping gaps
         mysubseqs <- mySequences1[indexes_msa]
         names(mysubseqs) <- names(indexes_msa)
         
         # Remove columns with gaps and remove empty spaces in last positions
         seqs_mysubseqs <- seqinr::getSequence(mysubseqs)
         last <- seqs_mysubseqs[[length(seqs_mysubseqs)]]
         last <- last[which(last != " ")]
         seqs_mysubseqs[[length(seqs_mysubseqs)]] <- last
         seqs_mysubseqs <- remove_gaps(seqs_mysubseqs)
         names(seqs_mysubseqs) <- names(mysubseqs)
         
         mysubseqs2 <- unlist(lapply(seqs_mysubseqs, function(x) paste(x, collapse="")))
         
         alignseqs <- Biostrings::AAMultipleAlignment(mysubseqs2, use.names = T)
         
       }
     }
     
     detach("package:msa", unload=TRUE)
     
     return(alignseqs)
     
   }) %>% bindEvent(input$msa_selection3)
   
   # Create boxes for outputs
   observeEvent(isTruthy(alignseqs3()), {
     
     if (UI_exist_msa3)
     {
       removeUI(
         selector = "div:has(>>> #msa_print3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_fa3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_msa3", "afterEnd", ui = {
       selected_msa <- isolate({input$selected_msaI3})
       msa_height <- ifelse(length(selected_msa) > 14, 550, 400 + 5*length(selected_msa))
       box(width = 12,
           title = "MSA Explorer", status = "danger", solidHeader = TRUE, height = msa_height,
           collapsible = TRUE,
           tags$div(id = "msa_pocket3", style = "width: 1300px; height: 400px",
                    msaROutput("msa_print3"))
           
       )
     })
     
     insertUI("#msa_down_plot3", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "msa_download_plot3", "Download Colored MSA",
                                                                          size = "sm", color = "danger"))
     })
     
     insertUI("#msa_down_fasta3", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "msa_download_fa3", "Download MSA FASTA",
                                                                          size = "sm", color = "danger"))
     })
     
     UI_exist_msa3 <<- TRUE
   })
   
   
   # Fill Output
   output$msa_print3 <- renderMsaR({
     alignseqs <- alignseqs3()
     msaout <- msa::msaConvert(alignseqs, "ape::AAbin")
     msaR(msaout, menu=T, overviewbox = F,  colorscheme = "clustal")
   })
   
   # Prepare variables for pdf construction
   observeEvent(isTruthy(alignseqs3()), {
     alignseqs <- alignseqs3()
     
     library(ggmsa)
     class(alignseqs) <- "AAMultipleAlignment"
     
     for(i in 1:(ncol(alignseqs)%/%100 +1)){
       assign(paste("msaportho", i, sep = ""), ggmsa(alignseqs, 1+(100*(i-1)), i*100, seq_name = TRUE, char_width = 0.5) +
                geom_seqlogo(color = "Chemistry_AA"), envir = as.environment(1), pos=1)
     }
     shinyjs::hideElement(id = 'loading.msa3')
   })
   
   # Download tab's results
   # Download colored MSA in pdf
   output$msa_download_plot3 <- downloadHandler(
     filename= function() {
       paste("msa", ".pdf", sep="")
     },
     content= function(file) {
       selected_msa <- input$selected_msaI3
       alignseqs <- alignseqs3()
       pdf(file, height = 2+length(selected_msa)*0.25, width = 16)
       {
         for(i in 1:(ncol(alignseqs)%/%100 +1)){
           print(mget(paste0("msaportho", i), envir = as.environment(1)))
         }
         dev.off()
       }
     })
   
   # Download MSA in FASTA format
   output$msa_download_fa3<- downloadHandler(
     filename= function() {
       paste("msa", ".fa", sep="")
     },
     content= function(file) {
       alignseqs <- alignseqs3()
       writeXStringSet(as(unmasked(alignseqs), "XStringSet"), file)
     })
   
   
   ####################### GO #################################
   
   observeEvent(input$run_button3, {
     removeUI(
       selector = "div:has(>> #selected_gosI3)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI(
       selector = "div:has(>> #selected_gos_modeI3)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#gos_selectionI3")
     
     if (UI_exist_go3)
     {
       removeUI(
         selector = "div:has(>> #output_gos_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_treeplot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadGOSTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_download3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #tree_gos_download3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_go3 <<- F
     }
     
   })
   
   observeEvent(input$go_start3, {
     insertUI("#selected_gos3", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_gosI3","Select the desired genes from the tree",
                                 choices=isolate({tree_reduced3()$tip.label}), options = list(`actions-box` = TRUE),
                                 multiple = T, selected = isolate({tree_reduced3()$tip.label[1]}))
       
     })
     
     insertUI("#selected_gos_mode3", "afterEnd", ui = {
       
       selectInput(inputId = "selected_gos_modeI3",
                   choices=c("Biological Processes" = "bp",
                             "Molecular Functions" = "mf",
                             "Cellular Components" = "cc"),
                   label = "Select the gene ontology to use",
                   multiple = F, selected = c("bp"))
       
     })
     
     insertUI("#gos_selection3", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("gos_selectionI3", "Show GO terms", size = "sm",
                                style = "float", color = "danger")
     })
     
   })
   
   total_table_gos3 <- reactive({
     
     shinyjs::showElement(id = 'loading.go3')
     gos_anot <- read.csv("pharaoh_folder/final_anot_table.tsv", sep="\t", header = T)
     sel.genes.go <- as.vector(input$selected_gosI3)
     selected_gos_mode <- as.character(isolate({input$selected_gos_modeI3}))
     
     total_table_gos <- subset(gos_anot, gos_anot$name %in% sel.genes.go)
     
     # Show an error if no terms are identified in the input
     if (nrow(total_table_gos) == 0) 
     {
       shinyjs::hideElement(id = 'loading.go3')
       if (UI_exist_go3)
       {
         removeUI(
           selector = "div:has(>> #output_gos_table3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_plot3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_treeplot3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadGOSTable3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_download3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_gos_download3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_go3 <<- F
       }
       output$error_gos3 <- renderUI({
         renderPrint({cat("0 GO terms identified.")})
       })
       
       validate(need(nrow(total_table_gos) != 0, " "))
     }
     
     
     gos_sel <- paste("gos", selected_gos_mode, sep="_")
     terms_sel <- paste("terms", selected_gos_mode, sep="_")
     total_table_gos <- total_table_gos[,c("organism", "id", "name", gos_sel, terms_sel)]
     
     # Once removed the two GO categories not selected, remove rows with blank cells
     total_table_gos_clean <- subset(total_table_gos, gos_sel != "")
     
     # Show an error if no terms are identified after the previous operation
     if (nrow(total_table_gos_clean) == 0) 
     {
       shinyjs::hideElement(id = 'loading.go3')
       if (UI_exist_go3)
       {
         removeUI(
           selector = "div:has(>> #output_gos_table3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_plot3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_treeplot3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadGOSTable3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_download3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_gos_download3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_go3 <<- F
       }
       
       output$error_gos3 <- renderUI({
         renderPrint({cat("0 GO terms identified.")})
       })
       
       validate(need(nrow(total_table_gos_clean) != 0, " "))
     }
     
     output$error_gos3 <- NULL
     
     return(total_table_gos_clean)
     
   }) %>% bindEvent(input$gos_selectionI3)
   
   enr_table3 <- reactive({
     
     total_table_gos <- total_table_gos3()
     selected_gos_mode <- as.character(isolate({input$selected_gos_modeI3}))
     
     # Create the plot
     
     # Create a list of GO terms vector of each gene
     gos_sel <- paste("gos", selected_gos_mode, sep="_")
     gos_list <- apply(total_table_gos,MARGIN=1,FUN = function(x) trimws(strsplit(as.character(x[gos_sel]), split = "[|]")[[1]]))
     names(gos_list) <- total_table_gos$name
     
     # Count GOs for each gene and create a matrix of gene-GO pairs
     count_gos_in_genes <- sapply(gos_list, FUN = function(x) length(x))
     comp_data <- data.frame(gene = rep(names(count_gos_in_genes), count_gos_in_genes), gos = as.character(unlist(gos_list)))
     
     # Collapse genes that share a same GO
     gene.v <- c()
     for (i in 1:length(unique(comp_data$gos)))
     {
       new.table <- subset(comp_data, gos == unique(comp_data$gos)[i])
       new.cha <- as.character(new.table$gene)
       gene.v <- c(gene.v, paste(new.cha, collapse = "/"))
     }
     
     names(gene.v) <- unique(comp_data$gos)
     
     # Load libraries and create gene chains, count and GO IDs fields (same order)
     library(GO.db)
     library("multienrichjam")
     library(clusterProfiler)
     library(enrichplot)
     library(ggplot2)
     
     count_go <- table(comp_data$gos)
     geneids <- gene.v[names(count_go)]
     count_terms <- mapply(function(x) {Term(x)}, names(count_go), USE.NAMES = F)
     
     # Create pseudo-enrichment table
     enr_table <- data.frame(ID=names(count_go), Description=count_terms, GeneRatio="90/100", BgRatio="90/10000", 
                             pvalue=0.000005, p.adjust=0.000005, qvalue=0.000005, geneID=geneids, Count = as.vector(count_go))
     
     return(enr_table)
     
   }) %>% bindEvent(input$gos_selectionI3)
   
   ema_gos_plot3 <- reactive({
     
     enr_table <- enr_table3()
     
     # Transform to enrichResult object
     enr <- enrichDF2enrichResult(enrichDF = enr_table, keyColname = "ID",
                                  geneColname = "geneID", pvalueColname = "p.adjust",
                                  descriptionColname = "Description", pvalueCutoff = 0.05)
     
     # Create plot
     ema_gos_plot <- emapplot(pairwise_termsim(enr), showCategory = 15) + theme(legend.position='none')
     return(ema_gos_plot)
   })
   
   tree_gos_plot3 <- reactive({
     
     enr_table <- enr_table3()
     
     # Transform to enrichResult object
     enr <- enrichDF2enrichResult(enrichDF = enr_table, keyColname = "ID",
                                  geneColname = "geneID", pvalueColname = "p.adjust",
                                  descriptionColname = "Description", pvalueCutoff = 0.05)
     {
       if (nrow(enr_table) > 4)
       {
         tree_gos_plot <- treeplot(pairwise_termsim(enr),showCategory = 15, cluster.params = list(label_words_n = 3)) +
           theme(legend.position='none')
       }
       else if (nrow(enr_table) > 2)
       {
         tree_gos_plot <- treeplot(pairwise_termsim(enr),showCategory = 15, cluster.params = list(n = 2, label_words_n = 3)) + 
           theme(legend.position='none')
       }
       else
       {
         text <- paste("\n  Unable to create treeplot with less than 3 GO terms \n")
         tree_gos_plot <- ggplot() + 
           annotate("text", x = 4, y = 25, size=8, label = text) + 
           theme_void()
       }
     }
     
     shinyjs::hideElement(id = 'loading.go3')
     return(tree_gos_plot)
   })
   
   # Create boxes for outputs
   observeEvent(isTruthy(tree_gos_plot3()), {
     
     if (UI_exist_go3)
     {
       removeUI(
         selector = "div:has(>> #output_gos_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_treeplot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadGOSTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_download3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #tree_gos_download3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_gos_table3", "afterEnd", ui = {
       box(width = 12,
           title = "GO Terms Table", status = "danger", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_gos_table3")
       )
     })
     
     insertUI("#box_gos_plot3", "afterEnd", ui = {
       box(width = 12,
           title = "GO Terms Plot", status = "danger", solidHeader = TRUE,
           collapsible = TRUE,
           imageOutput("gos_plot3")
       )
     })
     
     insertUI("#box_gos_treeplot3", "afterEnd", ui = {
       box(width = 12,
           title = "GO Terms Treeplot", status = "danger", solidHeader = TRUE,
           collapsible = TRUE,
           imageOutput("gos_treeplot3")
       )
     })
     
     insertUI("#download_ui_for_gos_table3", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadGOSTable3", "Download GO Table",
                                                                          size = "sm", color = "danger"))
     })
     
     insertUI("#gos_down_button3", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "gos_download3", "Download GO Plot",
                                                                          size = "sm", color = "danger"))
     })
     
     insertUI("#tree_gos_down_button3", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "tree_gos_download3", "Download GO Treeplot",
                                                                          size = "sm", color = "danger"))
     })
     
     UI_exist_go3 <<- TRUE
   })
   
   # Fill outputs
   # Table
   output$output_gos_table3 <- renderDataTable({
     total_table_gos <- total_table_gos3()
     selected_gos_mode <- as.character(isolate({input$selected_gos_modeI3}))
     gos_sel <- paste("gos", selected_gos_mode, sep="_")
     gos_list <- apply(total_table_gos,MARGIN=1,
                       FUN = function(x) trimws(strsplit(as.character(x[gos_sel]), split = "[|]")[[1]]))
     gos_links <- lapply(gos_list, function(x) sapply(x, go.link))
     gos_formatted <- unlist(lapply(gos_links, function(x) paste0(x, collapse = " | ")))
     total_table_gos[,gos_sel] <- gos_formatted
     total_table_gos
   },escape=FALSE, rownames=F, options =list(pageLength = 5))
   
   # First plot
   output$gos_plot3 <- renderImage({
     ema_gos_plot <- ema_gos_plot3()
     
     png("pharaoh_folder/gosplot.png", width = 590, height = 590, res = 90)
     plot(ema_gos_plot)
     dev.off()
     list(src = "pharaoh_folder/gosplot.png",
          contentType="image/png")
     
   }, deleteFile=T
   )
   
   # Second plot
   output$gos_treeplot3 <- renderImage({
     tree_gos_plot <- tree_gos_plot3()
     
     png("pharaoh_folder/treeplot.png", width = 620, height = 590, res = 80)
     plot(tree_gos_plot)
     dev.off()
     list(src = "pharaoh_folder/treeplot.png",
          contentType="image/png")
     
   }, deleteFile=T
   )
   
   # Download tab's results
   # Download GO table
   output$downloadGOSTable3 <- downloadHandler(
     filename= function() {
       paste("GOS_table", ".tsv", sep="")
     },
     content= function(file) {
       total_table_gos <- total_table_gos3()
       
       write.table(x = total_table_gos,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Download emaplot
   output$gos_download3 <- downloadHandler(
     filename= function() {
       paste("gos_plot", ".png", sep="")
     },
     content= function(file) {
       ema_gos_plot <- ema_gos_plot3()
       
       png(file,height = 1200, width = 1500, res=140)
       plot(ema_gos_plot)
       dev.off()
     })
   
   # Download treeplot
   output$tree_gos_download3 <- downloadHandler(
     filename= function() {
       paste("gos_treeplot", ".png", sep="")
     },
     content= function(file) {
       tree_gos_plot <- tree_gos_plot3()
       
       png(file, height = 1200, width = 1500, res=140)
       plot(tree_gos_plot)
       dev.off()
     })
  
   
   ###################### KEGG ###########################
   
   observeEvent(input$run_button3, {
     removeUI(
       selector = "div:has(>> #selected_kosI3)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#kos_selectionI3")
     
     if (UI_exist_kegg3)
     {
       removeUI(
         selector = "div:has(>> #output_kos_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #output_kegg_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKOSTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #selected_pathsI3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI("#paths_buttonI3")
       
       UI_exist_kegg3 <<- F
     }
     
     if (UI_exist_pathview3)
     {
       removeUI(
         selector = "div:has(>> #path_image3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGpathway3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_pathview3 <<- F
     }
     
     
   })
   
   observeEvent(input$kegg_start3, {
     insertUI("#selected_kos3", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_kosI3","Select the desired genes from the tree",
                                 choices=isolate({tree_reduced3()$tip.label}), options = list(`actions-box` = TRUE),
                                 multiple = T, selected = isolate({tree_reduced3()$tip.label[1]}))
       
       
     })
     
     
     insertUI("#kos_selection3", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("kos_selectionI3", "Show KEGG pathways", size = "sm",
                                style = "float", color = "danger")
     })
     
   })
   
   tab_kegg3 <- reactive({
     
     shinyjs::showElement(id = 'loading.ko3')
     # Create KOs set
     kos_anot <- read.csv("pharaoh_folder/ko_table_funtree.tsv", sep="\t", header = T)
     sel.genes.ko <- as.vector(isolate({input$selected_kosI3}))
     
     tab_kegg <- subset(kos_anot, gene %in% sel.genes.ko)
     set_kegg <- tab_kegg$ko[tab_kegg$ko != ""]
     
     # Show an error if no terms are identified in the input
     {
       if (length(set_kegg) == 0) 
       {
         shinyjs::hideElement(id = 'loading.ko3')
         output$error_kos3 <- renderUI({
           renderPrint({cat("0 KO terms identified. Please select more genes. If this 
        message persists, it should be interpreted as a lack of KO annotation for this orthogroup")})
         })
         
         if (UI_exist_kegg3)
         {
           removeUI(
             selector = "div:has(>> #output_kos_table3)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #output_kegg_table3)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #downloadKOSTable3)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #downloadKEGGTable3)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #selected_pathsI3)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI("#paths_buttonI3")
           
           UI_exist_kegg3 <<- F
         }
         
         if (UI_exist_pathview3)
         {
           removeUI(
             selector = "div:has(>> #path_image3)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #downloadKEGGpathway3)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           UI_exist_pathview3 <<- F
         }
         
         validate("No KO terms detected")
       }
     }
     
     output$error_kos3 <- NULL
     
     return(tab_kegg)
     
   }) %>% bindEvent(input$kos_selectionI3)
   
   total_table_kegg3 <- reactive({
     
     tab_kegg <- tab_kegg3()
     set_kegg <- tab_kegg$ko[tab_kegg$ko != ""]
     
     # Load libraries
     library(clusterProfiler)
     library(enrichplot)
     
     # Enrich with pvalue cutoff = 1 to show all paths
     kos_enrich <- enrichKEGG(gene         = set_kegg,
                              organism     = 'ko',
                              pvalueCutoff = 1)
     
     total_table_kegg <- as.data.frame(kos_enrich)
     
     # Filter out pathways that are not present in plants
     kegg_plants <- read.csv("pharaoh_folder/pathways_plant.ids", sep = "\t", header = T)$x
     total_table_kegg <- subset(total_table_kegg, ID %in% kegg_plants)
     
     return(total_table_kegg)
     
   }) %>% bindEvent(input$kos_selectionI3)
   
   total_table_kos3 <- reactive({
     
     tab_kegg <- tab_kegg3()
     
     library(KEGGREST)
     
     # Collapse genes that share KOs
     
     tab_kegg_for_ko <- subset(tab_kegg, ko != "")
     gene.v.ko <- c()
     for (i in 1:length(unique(tab_kegg_for_ko$ko)))
     {
       new.table <- subset(tab_kegg_for_ko, ko == unique(tab_kegg_for_ko$ko)[i])
       new.cha <- as.character(new.table$gene)
       gene.v.ko <- c(gene.v.ko, paste(new.cha, collapse = "/"))
     }
     
     names(gene.v.ko) <- unique(tab_kegg_for_ko$ko)
     
     # Create gene chains, count and KO IDs fields (same order)
     count_ko <- table(tab_kegg_for_ko$ko)
     geneids.ko <- gene.v.ko[names(count_ko)]
     count_terms.ko <- mapply(function(x) {keggFind("ko", x)}, names(count_ko), USE.NAMES = F)
     total_table_kos <- data.frame(ko=names(count_ko), name=count_terms.ko, count=as.numeric(count_ko),
                                   genes=geneids.ko)
     
     return(total_table_kos)
     
   }) %>% bindEvent(input$kos_selectionI3)
   
   # Create boxes for outputs
   observeEvent(isTruthy(total_table_kos3()), {
     
     if (UI_exist_kegg3)
     {
       removeUI(
         selector = "div:has(>> #output_kos_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #output_kegg_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKOSTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #selected_pathsI3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI("#paths_buttonI3")
       
     }
     
     insertUI("#box_kos_table3", "afterEnd", ui = {
       box(width = 12,
           title = "KO Terms Table", status = "danger", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_kos_table3")
       )
     })
     
     
     insertUI("#box_kegg_table3", "afterEnd", ui = {
       box(width = 12,
           title = "KEGG Pathways Table", status = "danger", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_kegg_table3")
       )
     })
     
     insertUI("#download_ui_for_kos_table3", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadKOSTable3", "Download KO Table",
                                                                          size = "sm", color = "danger"))
     })
     
     insertUI("#download_ui_for_kegg_table3", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadKEGGTable3", "Download Pathways Table",
                                                                          size = "sm", color = "danger"))
     })
     
     UI_exist_kegg3 <<- TRUE
     
     # Remove previous results for pathview
     if (UI_exist_pathview3)
     {
       removeUI(
         selector = "div:has(>> #path_image3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGpathway3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_pathview3 <<- F
     }
   })
   
   # Fill outputs
   # Render KO table
   output$output_kos_table3 <- renderDataTable({
     total_table_kos <- total_table_kos3()
     total_table_kos$ko <- sapply(total_table_kos$ko, ko.link)
     total_table_kos
   },escape=FALSE, rownames= F, options =list(pageLength = 5))
   
   # Render KEGG table
   output$output_kegg_table3 <- renderDataTable({
     total_table_kegg <- total_table_kegg3()
     total_table_kegg$ID <- sapply(total_table_kegg$ID, kegg.link)
     total_table_kegg[,c("ID", "Description", "geneID")]
   },escape=FALSE, rownames= F, options =list(pageLength = 5))
   
   # Download tab's outputs
   # Download KO table
   output$downloadKOSTable3 <- downloadHandler(
     filename= function() {
       paste("KO_table", ".tsv", sep="")
     },
     content= function(file) {
       total_table_kos <- total_table_kos3()
       write.table(x = total_table_kos,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Download KEGG table
   output$downloadKEGGTable3 <- downloadHandler(
     filename= function() {
       paste("KEGG_table", ".tsv", sep="")
     },
     content= function(file) {
       total_table_kegg <- total_table_kegg3()
       write.table(x = total_table_kegg[,c("ID", "Description", "geneID")],quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Create pathway selector and button
   observeEvent(input$kos_selectionI3,{
     
     total_table_kos <- total_table_kos3()
     total_table_kegg <- total_table_kegg3()
     
     if(nrow(total_table_kegg) != 0)
     {
       paths.options <- sapply(strsplit(total_table_kegg$ID, split = "ko"), function(x) x[[2]])
       
       
       insertUI("#selected_paths3", "afterEnd", ui = {
         shinyWidgets::pickerInput(inputId = "selected_pathsI3", label = "Select the pathway to plot", 
                                   choices = paths.options, selected = paths.options[1], multiple = F)
         
       })
       
       
       insertUI("#paths_button3", "afterEnd", ui = {
         
         shinyWidgets::actionBttn("paths_buttonI3", "Plot Pathway", size = "sm",
                                  style = "float", color = "danger")
       })
       
       shinyjs::hideElement(id = 'loading.ko3')
     }
   })
   
   observeEvent(input$paths_buttonI3,{
     
     if (UI_exist_pathview3)
     {
       removeUI(
         selector = "div:has(>> #path_image3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGpathway3)",
         multiple = TRUE,
         immediate = TRUE
       )
     }
     
     UI_exist_pathview3 <<- F
     
   })
   
   # Create Image to Render and save path name
   pathway.current.id3 <- reactive({
     pathway.current.id <- input$selected_pathsI3
     total_table_kos <- total_table_kos3()
     
     kos_unique <- unique(total_table_kos$ko)
     gene.pathway <- rep(0, length(kos_unique))
     names(gene.pathway) <-  kos_unique
     gene.pathway[kos_unique] <-1
     
     library(pathview)
     pathview(gene.data = sort(gene.pathway,decreasing = TRUE),kegg.dir = "pharaoh_folder",
              pathway.id = pathway.current.id,
              species = "ko",
              limit = list(gene=max(abs(gene.pathway)), cpd=1),
              gene.idtype ="kegg")
     
     return(pathway.current.id)
     
   }) %>% bindEvent(input$paths_buttonI3)
   
   # Create output box and download button
   observeEvent(isTruthy(pathway.current.id3()),{
     
     insertUI("#box_path_image3", "afterEnd", ui = {
       box(width = 12,
           title = "KEGG Pathway Plot", status = "danger", solidHeader = TRUE,
           collapsible = TRUE,
           fluidRow(column(1), imageOutput("path_image3", width = "100%", height = "700px"))
       )
     })
     
     insertUI("#path_download_ui3", "afterEnd", ui = {
       tags$div(shinyWidgets::downloadBttn(outputId= "downloadKEGGpathway3", "Download KEGG Pathway Plot",
                                           size = "sm", color = "danger"))
     })
     
     UI_exist_pathview3 <<- T
     
   })
   
   # Fill path image output
   output$path_image3 <- renderImage({
     
     pathway.current.id <- pathway.current.id3()
     list(src = paste(c(paste0(c("ko",pathway.current.id), collapse=""),"pathview","png"), collapse="."),
          contentType="image/png",width=900,height=700)
   },deleteFile = F)
   
   # Download and remove path image output
   output$downloadKEGGpathway3 <- downloadHandler(
     filename= function() {
       paste("path_plot", ".png", sep="")
     },
     content= function(file) {
       pathway.current.id <- pathway.current.id3()
       file.copy(paste(c(paste0(c("ko",pathway.current.id), collapse=""),"pathview","png"), collapse="."), file)
       file.remove(paste(c(paste0(c("ko",pathway.current.id), collapse=""),"pathview","png"), collapse="."))
     })   
   
   
   
   ############################# LITERATURE ANNOTATION ##########################################
   
   observeEvent(input$run_button3, {
     removeUI(
       selector = "div:has(>> #selected_litI3)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI(
       selector = "div:has(> #query_litI3)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#lit_selectionI3")
     
     
     if (UI_exist_lit3)
     {
       removeUI(
         selector = "div:has(>> #output_lit_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadLITTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_lit3 <<- F
     }
     
   })
   
   observeEvent(input$lit_start3, {
     
     insertUI("#query_lit3", "afterEnd", ui = {
       
       textInput(inputId = "query_litI3",value = "", label = "Enter search term", placeholder = "CCA1")
       
     })
     
     
     insertUI("#selected_lit3", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_litI3","Select the search mode",
                                 choices=c("Normal","Exact", "Substring", "Alias"),
                                 multiple = F, selected = "Normal")
       
       
     })
     
     
     insertUI("#lit_selection3", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("lit_selectionI3", "Get biological information and papers", size = "sm",
                                style = "float", color = "danger")
     })
     
   })
   
   pc_result3 <- reactive({
     
     shinyjs::showElement(id = 'loading.lit3')
     pc_search <- as.character(input$query_litI3)
     pc_search <- gsub(" ", "%20", pc_search) # To make spaces interpretable
     pc_modality <- tolower(as.character(input$selected_litI3))
     
     # Get PlantConnectome URL for query
     pc_url <- paste(c("https://connectome.plant.tools", pc_modality, pc_search), collapse = "/")
     

     library(RCurl)
     
     pc_res <- getURL(pc_url)
     
     if (!length(grep("No hits", pc_res)) == 0)
     {
       shinyjs::hideElement(id = 'loading.lit3')
       
       if (UI_exist_lit3)
       {
         removeUI(
           selector = "div:has(>> #output_lit_table3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadLITTable3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_lit3 <<- F
       }
       
       output$error_lit3 <- renderUI({
         renderPrint({cat("No results found for this query")})
       })
       
       validate(" ")
       
     }
     
     output$error_lit3 <- NULL
     
     # Isolate data frame from complete HTML file
     pc_split <- strsplit(as.character(pc_res), split = "<tbody")[[1]][2]
     pc_split <- strsplit(as.character(pc_split), split = "</tbody>")[[1]][1]
     
     pc_clean <- gsub("</tr>", "", pc_split)
     pc_clean <- gsub("[\r\n\t]", "", pc_clean)
     
     pc_vector <- strsplit(pc_clean, split = "<tr>")[[1]][-1]
     pc_vector2 <- sapply(pc_vector, FUN=function(x) strsplit(x, split = "<td> | </td>"))
     pc_table <- sapply(pc_vector2, FUN=function(x) as.character(unlist(x)))
     
     colnames(pc_table) <- NULL
     pc_result <- data.frame(t(pc_table[c(2,4,6,8),]))
     colnames(pc_result) <- c("Source", "Interaction Type", "Target", "Pubmed ID")
     
     return(pc_result)
     
   }) %>% bindEvent(input$lit_selectionI3)
   
   pc_result_show3 <- reactive({
     
     pc_result <- pc_result3()
     
     # Add links to papers
     urls_connect <- sapply(pc_result$`Pubmed ID`, FUN = function(x) paste0(c("<a href=\"",
                                                                              "https://pubmed.ncbi.nlm.nih.gov/",x,"/",
                                                                              "\" target=\"_blank\">", x,
                                                                              "</a>"),
                                                                            collapse=""))
     pc_result_show <- pc_result
     pc_result_show$`Pubmed ID` <- urls_connect
     
     return(pc_result_show)
     
   })
   
   # Create boxes for outputs
   observeEvent(isTruthy(pc_result_show3()), {
     
     if (UI_exist_lit3)
     {
       removeUI(
         selector = "div:has(>> #output_lit_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadLITTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_lit_table3", "afterEnd", ui = {
       box(width = 12,
           title = "Literature Table", status = "danger", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_lit_table3")
       )
     })
     
     insertUI("#download_ui_for_lit_table3", "afterEnd", ui = {
       tags$div(style = "margin-left: 400px;", shinyWidgets::downloadBttn(outputId= "downloadLITTable3", "Download Literature Table",
                                                                          size = "sm", color = "danger"))
     })
     
     shinyjs::hideElement(id = 'loading.lit3')
     UI_exist_lit3 <<- TRUE
     
   })
   
   # Fill outputs
   # Render table
   output$output_lit_table3 <- renderDataTable({
     pc_result_show3()
   },escape=FALSE, rownames= F, options =list(pageLength = 10))
   
   # Download results
   output$downloadLITTable3 <- downloadHandler(
     filename= function() {
       paste("literature_table", ".tsv", sep="")
     },
     content= function(file) {
       pc_result <- pc_result3()
       write.table(x = pc_result,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })   
   
   
   ######################## STRING ###########################
   
   observeEvent(input$run_button3, {
     removeUI(
       selector = "div:has(>> #selected_stringI3)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#string_selectionI3")
     
     shinyjs::hideElement("error_string3")
     
     
     if (UI_exist_string3)
     {
       removeUI(
         selector = "div:has(>> #output_st_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #output_count_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>>> #count_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadSTRINGTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadCOUNTTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #count_download3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #selected_networkI3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI("#network_buttonI3")
       
       UI_exist_string3 <<- F
     }
     
     if (UI_exist_network3)
     {
       removeUI(
         selector = "div:has(>>>> #network_image3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_network3 <<- F
     }
     
   })
   
   # First, create a table that links reduced gene names to its species
   string_sel_table3 <- reactive({
     
     # Define previous variables
     tree_reduced <- tree_reduced3()
     tree <- tree_adj3()
     
     tips_to_keep.mp <- tips_to_keep.mp3()
     tips_to_keep.ot <- tips_to_keep.ot3()
     tips_to_keep.at <- tips_to_keep.at3()
     tips_to_keep.cp <- tips_to_keep.cp3()
     tips_to_keep.cr <- tips_to_keep.cr3()
     tips_to_keep.cz <- tips_to_keep.cz3()
     tips_to_keep.kn <- tips_to_keep.kn3()
     tips_to_keep.me <- tips_to_keep.me3()
     tips_to_keep.mi <- tips_to_keep.mi3()
     tips_to_keep.pp <- tips_to_keep.pp3()
     tips_to_keep.sl <- tips_to_keep.sl3()
     tips_to_keep.sm <- tips_to_keep.sm3()
     tips_to_keep.sp <- tips_to_keep.sp3()
     tips_to_keep.ta <- tips_to_keep.ta3()
     tips_to_keep.vc <- tips_to_keep.vc3()
     tips_to_keep.bp <- tips_to_keep.bp3()
     tips_to_keep.cri <- tips_to_keep.cri3()
     tips_to_keep.ds <- tips_to_keep.ds3()
     tips_to_keep.os <- tips_to_keep.os3()
     tips_to_keep.smag <- tips_to_keep.smag3()
     tips_to_keep.tp <- tips_to_keep.tp3()
     tips_to_keep.aa <- tips_to_keep.aa3()
     tips_to_keep.um <- tips_to_keep.um3()
     tips_to_keep.rs <- tips_to_keep.rs3()
     tips_to_keep.cyc <- tips_to_keep.cyc3()
     tips_to_keep.pu <- tips_to_keep.pu3()
     tips_to_keep.pt <- tips_to_keep.pt3()
     tips_to_keep.ng <- tips_to_keep.ng3()
     tips_to_keep.cyano <- tips_to_keep.cyano3()
     tips_to_keep.ca <- tips_to_keep.ca3()
     tips_to_keep.mv <- tips_to_keep.mv3()
     tips_to_keep.af <- tips_to_keep.af3()
     tips_to_keep.sc <- tips_to_keep.sc3()
     tips_to_keep.aegi <- tips_to_keep.aegi3()
     tips_to_keep.sb <- tips_to_keep.sb3()
     tips_to_keep.chara <- tips_to_keep.chara3()
     tips_to_keep.guilla <- tips_to_keep.guilla3()
     tips_to_keep.crypto <- tips_to_keep.crypto3()
     tips_to_keep.cymero <- tips_to_keep.cymero3()
     tips_to_keep.galsul <- tips_to_keep.galsul3()
     tips_to_keep.gracichor <- tips_to_keep.gracichor3()
     tips_to_keep.sceobli <- tips_to_keep.sceobli3()
     tips_to_keep.cocco <- tips_to_keep.cocco3()
     tips_to_keep.saccha <- tips_to_keep.saccha3()
     tips_to_keep.haema <- tips_to_keep.haema3()
     tips_to_keep.zm <- tips_to_keep.zm3()
     
     # Table construction
     org.factor <- c()
     
     for (i in 1:length(tree_reduced$tip.label))
     {
       
       if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mp])
       {
         org.factor <- c(org.factor,"Marchantia")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ot])
       {
         org.factor <- c(org.factor,"Ostreococcus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.at])
       {
         org.factor <- c(org.factor,"Arabidopsis")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cp])
       {
         org.factor <- c(org.factor,"Ceratodon")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cr])
       {
         org.factor <- c(org.factor,"Chlamydomonas")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cz])
       {
         org.factor <- c(org.factor,"Chromochloris")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.kn])
       {
         org.factor <- c(org.factor,"Klebsormidium")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.me])
       {
         org.factor <- c(org.factor,"Mesotaenium")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mi])
       {
         org.factor <- c(org.factor,"Micromonas")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pp])
       {
         org.factor <- c(org.factor,"Physcomitrium")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sl])
       {
         org.factor <- c(org.factor,"Solanum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sm])
       {
         org.factor <- c(org.factor,"Selaginella")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sp])
       {
         org.factor <- c(org.factor,"Spirogloea")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ta])
       {
         org.factor <- c(org.factor,"Triticum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.vc])
       {
         org.factor <- c(org.factor,"Volvox")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.bp])
       {
         org.factor <- c(org.factor,"Bathycoccus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cri])
       {
         org.factor <- c(org.factor,"Ceratopteris")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ds])
       {
         org.factor <- c(org.factor,"Dunaliella")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.os])
       {
         org.factor <- c(org.factor,"Oryza")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.smag])
       {
         org.factor <- c(org.factor,"Sphagnum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.tp])
       {
         org.factor <- c(org.factor,"Thuja")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.aa])
       {
         org.factor <- c(org.factor,"Anthoceros")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.um])
       {
         org.factor <- c(org.factor,"Ulva")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.rs])
       {
         org.factor <- c(org.factor,"Raphidocelis")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cyc])
       {
         org.factor <- c(org.factor,"Cycas")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pu])
       {
         org.factor <- c(org.factor,"Porphyra")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pt])
       {
         org.factor <- c(org.factor,"Phaeodactylum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ng])
       {
         org.factor <- c(org.factor,"Nannochloropsis")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cyano])
       {
         org.factor <- c(org.factor,"Cyanophora")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ca])
       {
         org.factor <- c(org.factor,"Chlorokybus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mv])
       {
         org.factor <- c(org.factor,"Mesostigma")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.af])
       {
         org.factor <- c(org.factor,"Azolla")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sc])
       {
         org.factor <- c(org.factor,"Salvinia")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.aegi])
       {
         org.factor <- c(org.factor,"Aegilops")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sb])
       {
         org.factor <- c(org.factor,"Sorghum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.chara])
       {
         org.factor <- c(org.factor,"Chara")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.guilla])
       {
         org.factor <- c(org.factor,"Guillardia")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.crypto])
       {
         org.factor <- c(org.factor,"Cryptophyceae")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cymero])
       {
         org.factor <- c(org.factor,"Cyanidioschyzon")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.galsul])
       {
         org.factor <- c(org.factor,"Galdieria")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.gracichor])
       {
         org.factor <- c(org.factor,"Gracilariopsis")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sceobli])
       {
         org.factor <- c(org.factor,"Scenedesmus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cocco])
       {
         org.factor <- c(org.factor,"Coccomyxa")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.saccha])
       {
         org.factor <- c(org.factor,"Saccharina")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.haema])
       {
         org.factor <- c(org.factor,"Haematococcus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.zm])
       {
         org.factor <- c(org.factor,"Zea")
       }
       
     }
     
     #Matrix with labels and colors and transform to dplyr format
     string.sel.table <- data.frame(node = 1:length(tree_reduced$tip.label), label = tree_reduced$tip.label,
                                    org = org.factor)
     
     return(string.sel.table)
     
   }) %>% bindEvent(input$string_start3)
   
   # Now, selection is allowed for genes of species with STRING support
   observeEvent(input$string_start3, {
     
     string_sel_table <- string_sel_table3()
     allow_string_species <- c("Aegilops", "Arabidopsis", "Bathycoccus", "Chara", "Chlamydomonas",
                               "Coccomyxa", "Cyanidioschyzon", "Galdieria", "Gracilariopsis",
                               "Guillardia", "Klebsormidium", "Micromonas","Oryza",
                               "Ostreococcus", "Phaeodactylum","Physcomitrium", "Raphidocelis",
                               "Scenedesmus", "Selaginella", "Solanum", "Sorghum", "Triticum",
                               "Volvox")
     
     st_genes <- subset(string_sel_table, string_sel_table$org %in% allow_string_species)$label
     
     if (length(st_genes) == 0)
     {
       shinyjs::showElement("error_string3")
       output$error_string3 <- renderUI({renderText({print("No results for this
      analysis due to lack of genes of STRING-supported species in the selection.")})})
       validate(" ")
     }
     
     output$error_string3 <- NULL
     
     insertUI("#selected_string3", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_stringI3","Select the desired genes from the tree",
                                 choices=st_genes, options = list(`actions-box` = TRUE),
                                 multiple = T)
       
     })
     
     
     insertUI("#string_selection3", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("string_selectionI3", "Show STRING Interactions", size = "sm",
                                style = "float", color = "danger")
     })
     
   })
   
   phys_table3 <- reactive({
     
     shinyjs::showElement(id = 'loading.string3')
     query_genes <- input$selected_stringI3
     
     # Load complete STRING annotation table
     library(data.table)
     
     # Load iteratively from split files using data.table format
     data_phys <- fread("pharaoh_folder/string_physical/string_physical_1.tsv")
     
     for (x in list.files("pharaoh_folder/string_physical/")[-1])
     {
       data_phys <- rbind(data_phys, 
                          fread(paste0("pharaoh_folder/string_physical/", x)))
     }
     
     
     
     # Subset by query genes using data.table for speed
     #string_res <- subset(data_phys, data_phys$prot_query %in% query_genes)
     string_res <- data_phys[prot_query %in% query_genes,]
     string_res <- as.data.frame(string_res)
     
     # Assign OG ID to each target
     ortho_data_file <- ifelse(model.selected3(), "Global_Gene_Trees/Orthogroups.tsv",
                               "Green_Gene_Trees/Orthogroups.tsv")
     
     ortho_data <- as.data.frame(fread(ortho_data_file))
     
     ortho_char <- apply(ortho_data, MARGIN = 1, function(x) paste(x, collapse = ","))
     ortho.numbers <- sapply(string_res$prot_interaction, function(x) grep(x, ortho_char), USE.NAMES = F)
     
     # If a pattern is found in several names, i.e., is a subpattern of several genes,
     # search for the exact match
     if(class(ortho.numbers) == "list")
     {
       # If a gene isn't associated to an OG
       index.none <- which(sapply(ortho.numbers, function(x) length(x) == 0))
       
       if (length(index.none) != 0)
       {
         # We create another row for OG table to associate those genes
         ortho_data <- rbind(ortho_data, "No OG")
         ortho.numbers[index.none] <- nrow(ortho_data)
       }
       
       # If a gene has more than one match due to subpatterns
       index.wrong <- which(sapply(ortho.numbers, function(x) length(x) > 1))
       {
         if (length(index.wrong) == 0)
         {
           ortho.numbers <- unlist(ortho.numbers, use.names = F)
         }
         else
         {
           for (i in index.wrong)
           {
             for (j in ortho.numbers[[i]])
             {
               ortho_char_split <- strsplit(ortho_char[j],split = ",")
               ortho_char_split_clean <- sapply(ortho_char_split, function(x) gsub(" ", "", x))
               if (string_res$prot_interaction[i] %in% ortho_char_split_clean)
               {
                 ortho.numbers[i] <- j
                 ortho.numbers <- unlist(ortho.numbers, use.names = F)
               }
             }
           }
         }
       }
     }
     
     
     
     # Create the final table
     ortho.string.names <- ortho_data$Orthogroup[ortho.numbers]
     phys_table <- data.frame(string_res, orthogroup = ortho.string.names)
     
     return(phys_table)
   }) %>% bindEvent(input$string_selectionI3)
   
   # Create count table to identify enriched OGs in STRING result
   string_counts3 <- reactive({
     
     phys_table <- phys_table3()
     string_counts <- sort(table(phys_table$orthogroup), decreasing = T)
     
     return(string_counts)
     
   }) %>% bindEvent(input$string_selectionI3)
   
   string_count_plot3 <- reactive({
     
     library(ggplot2)
     library(dplyr)
     
     data_count <- as.data.frame(string_counts3())
     colnames(data_count) <- c("orthogroup", "value")
     
     # Compute the position of labels
     data_count <- data_count %>%
       arrange(desc(orthogroup)) %>%
       mutate(prop = value / sum(data_count$value) *100) %>%
       mutate(ypos = cumsum(prop)- 0.5*prop )
     
     # Create plot
     count_plot <- ggplot(data_count, aes(x="", y=prop, fill=orthogroup)) +
       geom_bar(stat="identity", width=1, color="white") +
       coord_polar("y", start=0) +
       theme_void() +
       theme(legend.position="none") +
       #geom_text(aes(y = ypos, label = orthogroup), color = "white", size=6) +
       scale_fill_manual(values = rep(RColorBrewer::brewer.pal(n = 9, name = "Set1"), 
                                      floor(nrow(data_count)/9)+1))
     
     return(count_plot)
     
   }) %>% bindEvent(input$string_selectionI3)
   
   # Create boxes for outputs
   observeEvent(isTruthy(string_count_plot3()), {
     
     if (UI_exist_string3)
     {
       removeUI(
         selector = "div:has(>> #output_st_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #output_count_table3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>>> #count_plot3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadSTRINGTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadCOUNTTable3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #count_download3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #selected_networkI3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI("#network_buttonI3")
       
     }
     
     insertUI("#box_st_table3", "afterEnd", ui = {
       box(width = 12,
           title = "STRING Interactions Table", status = "danger", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_st_table3")
       )
     })
     
     insertUI("#box_count_table3", "afterEnd", ui = {
       box(width = 12,
           title = "Interacting Orthogroups Table", status = "danger", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_count_table3")
       )
     })
     
     insertUI("#box_count_plot3", "afterEnd", ui = {
       box(width = 12,
           title = "Interacting Orthogroups Plot", status = "danger", solidHeader = TRUE,
           collapsible = TRUE,
           fluidRow(column(1), imageOutput("count_plot3"))
       )
     })
     
     
     insertUI("#download_ui_for_st_table3", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadSTRINGTable3", "Download STRING Table",
                                                                          size = "sm", color = "danger"))
     })
     
     insertUI("#download_ui_for_count_table3", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadCOUNTTable3", "Download OG Count Table",
                                                                          size = "sm", color = "danger"))
     })
     
     insertUI("#count_down_button3", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "count_download3", "Download OG Count Plot",
                                                                          size = "sm", color = "danger"))
     })
     
     UI_exist_string3 <<- TRUE
     
     # Remove previous results for STRING network
     if (UI_exist_network3)
     {
       removeUI(
         selector = "div:has(>>>> #network_image3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_network3 <<- F
     }
     
   })
   
   # Fill outputs
   # Render STRING table
   output$output_st_table3 <- renderDataTable({
     phys_table <- phys_table3()
     
     datatable(phys_table, escape=FALSE, rownames= F, options =list(pageLength = 10)) %>%
       formatStyle(
         'type',
         color = styleEqual(
           c("Direct interaction", "Interolog"), c('green', '#CA931B')
         )
       )
   }) 
   
   
   # Render OG count table
   output$output_count_table3 <- renderDataTable({
     string_counts <- as.data.frame(string_counts3())
     colnames(string_counts) <- c("orthogroup", "count")
     string_counts
   },escape=FALSE, rownames= F, options =list(pageLength = 7))
   
   # Render OG count pie chart
   output$count_plot3 <- renderImage({
     
     string_count_plot <- string_count_plot3()
     
     png("string_count_plot3.png", height = 450, width = 450)
     plot(string_count_plot)
     dev.off()
     
     list(src = "string_count_plot3.png",
          contentType="image/png", width=400,height=400)
   }, deleteFile = T)
   
   
   # Download tab's outputs
   # Download STRING table
   output$downloadSTRINGTable3 <- downloadHandler(
     filename= function() {
       paste("string_table", ".tsv", sep="")
     },
     content= function(file) {
       phys_table <- phys_table3()
       write.table(x = phys_table,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Download count table
   output$downloadCOUNTTable3 <- downloadHandler(
     filename= function() {
       paste("string_count_table", ".tsv", sep="")
     },
     content= function(file) {
       string_counts <- string_counts3()
       write.table(x = string_counts,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Download count plot
   output$count_download3 <- downloadHandler(
     filename= function() {
       paste("string_count", ".png", sep="")
     },
     content= function(file) {
       string_count_plot <- string_count_plot3()
       
       png(file, height = 450, width = 450)
       plot(string_count_plot)
       dev.off()
     })
   
   # Create gene selector and button for STRING network representation
   observeEvent(input$string_selectionI3,{
     
     phys_table <- phys_table3()
     network_genes <- unique(phys_table$prot_query)
     
     # Error message if no genes are allowed for selection should have  been reported
     # earlier
     
     insertUI("#selected_network3", "afterEnd", ui = {
       
       shinyWidgets::pickerInput(inputId = "selected_networkI3", label = "Select the gene whose network you want to plot", 
                                 choices = network_genes, selected = network_genes[1],
                                 options = list(`actions-box` = TRUE), multiple = T)
       
     })
     
     
     insertUI("#network_button3", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("network_buttonI3", "Plot STRING Network", size = "sm",
                                style = "float", color = "danger")
     })
     
     shinyjs::hideElement(id = 'loading.string3')
     
   })
   
   mapped_string3 <- reactive({
     
     # Load genes (PharaohFUN IDs) and convert to STRING IDs
     library(data.table)
     
     network_genes <- input$selected_networkI3
     map_table <- fread("pharaoh_folder/string_map.tsv")
     
     # Fast subset using data.table
     map_network <- map_table[pharaohfun_id %in% network_genes,]
     
     # Error if no genes from selection have an associated high fidelity STRING ID
     if (nrow(map_network) == 0)
     {
       
       if (UI_exist_network3)
       {
         removeUI(
           selector = "div:has(>>>> #network_image3)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_network3 <<- F
       }
       
       output$error_network3 <- renderUI({renderText({print("It's not possible to map any
                               genes from selection to STRING IDs, please select different ones.")})})
       validate( " ")
       
     }
     
     output$error_network3 <- NULL
     
     # Get only STRING IDS and paste in a format interpretable by JS function
     string_ids <- as.data.frame(map_network)$string_id
     
     
     mapped_string <- paste0(string_ids, collapse = "%0d")
     return(mapped_string)
     
   }) %>% bindEvent(input$network_buttonI3)
   
   # Create boxes
   observeEvent(isTruthy(mapped_string3()),{
     
     mapped_string <- mapped_string3()
     
     if (UI_exist_network3)
     {
       removeUI(
         selector = "div:has(>>>> #network_image3)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     url_interactive <- paste0("https://string-db.org/cgi/network?identifiers=", 
                               mapped_string, 
                               "&add_color_nodes=25&network_flavor=confidence&show_query_node_labels=1")
     
     insertUI("#box_output_network3", "afterEnd", ui = {
       box(width = 12,
           title = "Image", status = "success", solidHeader = TRUE,
           collapsible = TRUE,
           fluidRow(column(1), 
                    column(8, htmlOutput("network_image3")), 
                    column(3, div( style = "margin-top: 300px;", 
                                   shinyWidgets::actionBttn("network_link3", "Interactive Network", size = "md", 
                                                            icon = icon("circle-nodes"), style = "float", color = "danger", 
                                                            onclick=paste0("window.open('", url_interactive,"','_blank')")))
                           
                    ))
           
       )
     })
     
     UI_exist_network3 <<- T
     
   })
   
   # Fill network box
   
   output$network_image3 <- renderText({
     
     mapped_string <- mapped_string3()
     src_map <- paste0("https://string-db.org/api/image/network?identifiers=", mapped_string, 
                       "&add_color_nodes=25&network_flavor=confidence")
     
     c('<img src="',src_map,'"width="675" height="625">')
   })
   
# End of OG-based search results
   
##################### BATCH SEARCH ##########################
   
   # Set global variables for tracking changes in output
   UI_exist_batch4 <<- F
   
   # To avoid autoupdating some inputs, define variables with its values
   model.selected4 <- reactive({
     model.selected <- !input$switch4
     return(model.selected)
   })%>% bindEvent(input$run_button4)
   
   search_mode4 <- reactive({
     search_mode <- input$search_mode4
     return(search_mode)
   }) %>% bindEvent(input$run_button4)
   
   seq_org4 <- reactive({
     seq_org <- input$organism_input_4
     return(seq_org)
     
   }) %>% bindEvent(input$run_button4)
   
   # Load organisms selection based on the model selected
   selected_organisms4 <- reactive({
     selected_organisms <- c(input$mami_check_4,input$chloro_check_4, input$strepto_check_4,
                             input$bryo_check_4, input$lyco_check_4, input$sperma_check_4)
     if(model.selected4()){selected_organisms <- c(input$tsar_check_4, input$rhodo_check_4, 
                                                   input$glauco_check_4,selected_organisms)}
     return(selected_organisms)
     
   }) %>% bindEvent(input$run_button4)
   
   selected_values_org4 <- reactive(organisms_values[selected_organisms4()]) %>% bindEvent(input$run_button4)
   
   # Generate random name
   random.file4 <- reactive({
     
     random.file <- paste0(c("batch_", sample(LETTERS, 7, replace = T), sample.int(9, size=5, replace = T)), collapse = "")
     return(random.file)
   }) %>% bindEvent(input$run_button4)
   
   
   # Reactive for creating results folder when Run button is clicked and storing a value
   table_heat4 <- reactive({
     
     shinyjs::showElement(id = 'loading.batch4')
     library(data.table)
     library(stringr)
     
     random.file <- random.file4()
     search_mode4 <- search_mode4()
     # Access species proteome
     
     org_batch <- as.character(seq_org4())
     org_search <- gsub(" ", "_", tolower(as.character(org_batch)))
     org_fasta <- paste("pharaohfun_proteomes", paste0(org_search, ".fa", sep=""), sep = "/")
     
     # Load data based on input mode
     # If input is entered as sequences
     {
       if (search_mode4)
       {
         # Load and clean fasta
         seqs_val <- seqinr::read.fasta(file = input$geneInt4$datapath, seqtype = "AA", forceDNAtolower = F, as.string = T) 
         names_seqs_val <- seqinr::getName(seqs_val)
         seqs_seqs_val <- seqinr::getSequence(seqs_val, as.string = T)
         seqs_seqs_val_clean <- lapply(seqs_seqs_val, function(x) as.character(toupper(gsub("[\r\n\t]", "", x))))
         seqs_seqs_val_clean2 <- lapply(seqs_seqs_val_clean, function(x) str_replace_all(x, fixed(" "), ""))
         vec_seq_val <- lapply(seqs_seqs_val_clean2, function(x) str_split_1(x, pattern = ""))
         
         # Create clean fasta file for comparison with random name to allow several users at once
         seqinr::write.fasta(vec_seq_val, names = names_seqs_val, paste0(random.file, "_new_diamond.fa"))
         
         # Run diamond
         library(rdiamond)
         
         diamond_res <- rdiamond::diamond_protein_to_protein(
           query   = paste0(random.file, "_new_diamond.fa"),
           subject = org_fasta,
           sensitivity_mode = "sensitive",
           output_path = tempdir(),
           #db_import  = FALSE,
           #diamond_exec_path = "/usr/bin", 
           max_target_seqs = 1)
         
         diamond_table <- data.frame(diamond_res)
         file.remove(paste0(random.file, "_new_diamond.fa"))
         
       }
       
       # If ID search mode is selected
       else
       {
         ids_vals <- fread(file = input$geneInt4$datapath, header = F)
         diamond_table <- data.frame(query_id = ids_vals$V1, subject_id = ids_vals$V1)
       }
     }
     
     # Error if an ID does not correspond to the selected proteome, it only
     # happens in ID search mode, but this chunk is placed here to preserve
     # a common framework for both search modes
     
     # Access species proteome
     prot_comp_fasta <- seqinr::read.fasta(file = org_fasta, seqtype = "AA", forceDNAtolower = F, as.string = T) 
     prot_ids <- seqinr::getName(prot_comp_fasta)
     
     if (!all(diamond_table$subject_id %in% prot_ids))
     {
       # Error message and remove output
       if (UI_exist_batch4)
       {
         
         removeUI(
           selector = "div:has(>> #heat_image4)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadBatch4)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_batch4 <<- F
         
       }
       
       shinyjs::hideElement(id = 'loading.batch4')
       output$error_batch4 <- renderUI({
         renderPrint({cat("No results available due to wrong ID format. Please use
                          supported IDs or introduce the corresponding sequences
                          using a FASTA format with your own portein names.")})
       })
       
       validate(" ")
     }
   
     # Once table is created and IDs checked, create a new results folder 
     # (remove previous one if it exists with the same random name)
     
     if (file.exists(paste0(random.file, ".zip")))
     {
       file.remove(paste0(random.file, ".zip"))
     }
     
     dir.create(random.file)
     
     # For each subject id in table, find OG name
     ortho.file <- ifelse(model.selected4(), "Global_Gene_Trees/Orthogroups.tsv",
                                             "Green_Gene_Trees/Orthogroups.tsv")
     
     ortho.line <- fread(ortho.file, select = c("Orthogroup",org_search))
     
     gene.numbers <- sapply(diamond_table$subject_id, function(x) grep(x, ortho.line[[org_search]]), USE.NAMES = F)
     
     # If a pattern is found in several names, i.e., is a subpattern of several genes,
     # search for the exact match
     if(class(gene.numbers) == "list")
     {
       # Check if there are genes with more than one match due to subpatterns
       index.wrong <- which(sapply(gene.numbers, function(x) length(x) > 1))
       {
         if (length(index.wrong != 0))
         {
           for (i in index.wrong)
           {
             for (j in gene.numbers[[i]])
             {
               ortho_char_split <- strsplit(ortho.line[[org_search]][j],split = ",")
               ortho_char_split_clean <- sapply(ortho_char_split, function(x) gsub(" ", "", x))
               if (diamond_table$subject_id[i] %in% ortho_char_split_clean)
               {
                 gene.numbers[i] <- j
               }
             }
           }
         }
       }
     }
     
     # Write corresponding OG to results table, and indicate it if it is not clustered in any OG
     ogs.batch <- sapply(gene.numbers, function(x) ortho.line$Orthogroup[x], USE.NAMES = F)
     ogs.names <- sapply(ogs.batch, function(x) if (length(x) == 0) "Gene is not clustered in an OG" else x)
     
     diamond_table <- cbind(diamond_table, "OG" = ogs.names)
     fwrite(diamond_table, file = paste0(random.file, "/results.tsv"),
            sep = "\t")
     
     # Create subfolders for each of the input proteins
     apply(diamond_table, MARGIN = 1,
           FUN = function(x) if (x["OG"] != "Gene is not clustered in an OG") 
             dir.create(paste0(random.file, "/",
                               x["query_id"])))
     
     # Copy results to its folders
     folder_prefix <- ifelse(model.selected4(), "Global_", "Green_")
     
     apply(diamond_table, MARGIN = 1,
           FUN = function(x) if (x["OG"] != "Gene is not clustered in an OG") 
           {
             # Copy trees to its folders
             
             file.copy(from=paste0(folder_prefix, "Gene_Trees/", 
                                   x["OG"], "_tree.txt"), 
                       to=paste0(random.file, "/", x["query_id"], "/", x["query_id"], "_tree.txt"), 
                       overwrite = TRUE, recursive = FALSE, 
                       copy.mode = TRUE)
             
             # Copy MSAs to its folders
             file.copy(from=paste0(folder_prefix, "MultipleSequenceAlignments/",
                                   x["OG"], ".fa"), 
                       to=paste0(random.file, "/", x["query_id"], "/", x["query_id"], "_MSA.fa"), 
                       overwrite = TRUE, recursive = FALSE, 
                       copy.mode = TRUE)
             
           }
     )
     
     # Functional annotation
     
     # Retrieve genes for each OG
     ortho_for_go <- apply(diamond_table, MARGIN = 1,
                           FUN = function(x) if (x["OG"] != "Gene is not clustered in an OG") 
                             as.character(subset(ortho.line, (ortho.line$Orthogroup == x["OG"]))))
     
     
     ortho_for_go2 <- lapply(ortho_for_go, function(x) paste0(x[-1][x[-1] != ""], collapse = ","))
     ortho_for_go_clean <- sapply(ortho_for_go2, function(x) strsplit(str_replace_all(x, fixed(" "), ""), split = ","))
     ortho_for_go_def <- ortho_for_go_clean[lapply(ortho_for_go_clean,length)>0]
     
     # Associate each vector of genes with its corresponding query id
     names(ortho_for_go_def) <- diamond_table$query_id[apply(diamond_table, MARGIN = 1,
                                                             FUN = function(x) (x["OG"] != "Gene is not clustered in an OG"))]
     
     
     # Subset the GO annotation and write TSVs
     annotation.go <- fread("pharaoh_folder/final_anot_table.tsv")
     
     mapply(function(x,y)
     {fwrite(annotation.go[annotation.go$id %in% x, ], 
             file = paste0(random.file, "/", y, "/", y, "_GO.tsv"), sep = "\t")}, 
     x=ortho_for_go_def, y=names(ortho_for_go_def))
     
     # Subset the KO annotation and write TSVs (using same set as in GO)
     annotation.ko <- fread("pharaoh_folder/ko_table_funtree.tsv")
     
     mapply(function(x,y)
     {fwrite(annotation.ko[annotation.ko$gene %in% x, ], 
             file = paste0(random.file, "/", y, "/", y, "_KO.tsv"), sep = "\t")}, 
     x=ortho_for_go_def, y=names(ortho_for_go_def))
     
     # CAFE files
     library(ape)
     
     # Read an extract CAFE trees
     cafe_file <- ifelse(model.selected4(), "pharaoh_folder/global_cafe.tre",
                         "pharaoh_folder/green_cafe.tre")
     
     cafe_comp <- read.nexus(cafe_file)
     
     # Keep the ones corresponding to present OGs
     cafe_correspondence <- subset(diamond_table, diamond_table$OG %in% names(cafe_comp))[,c("query_id", "OG")]
     
     apply(cafe_correspondence, MARGIN = 1,
           FUN = function(x)
           {
             # Write trees to its folders
             write.tree(cafe_comp[[x["OG"]]], file = paste0(random.file, "/", x["query_id"], "/", x["query_id"], "_ancestral.txt"))
           }
     )
     
     # Create gene tables with reduced species for each OG
     selected_organisms <- selected_organisms4()
     org_sel <- as.character(sapply(selected_organisms, function(x) gsub(" ", "_", tolower(x))))
     org_first <- sapply(strsplit(org_sel, split = "_"), function(x) x[[1]])
    
     
     table_ogs <- fread(ortho.file)
     org_index <- sapply(org_first, function(x) grep(x, colnames(table_ogs)), USE.NAMES = F)
     org_index <- as.numeric(c(1, org_index))
     table_ogs_red <- table_ogs[,c(org_index), with=F]
    
     apply(diamond_table, MARGIN = 1,
           FUN = function(x) if (x["OG"] != "Gene is not clustered in an OG") 
           {
             # Copy trees to its folders
             new_line <- as.data.frame(table_ogs_red[Orthogroup == x["OG"]])
             fwrite(new_line, file = paste0(random.file, "/", x["query_id"], "/", x["query_id"], "_gene_table.tsv"),
                    sep = "\t", col.names = T, row.names = F)
           })
     
     # Compress folder and remove uncompressed one
     zip(zipfile = paste0(random.file, ".zip"), files = random.file)
     unlink(random.file, recursive = TRUE)
     
     # Return table with query_id and Orthogroups for heatmap
     table_heat <- subset(diamond_table, OG != "Gene is not clustered in an OG")
     table_heat <- table_heat[, c("query_id", "OG")]
     return(table_heat)
     
   }) %>% bindEvent(input$run_button4)
   
   # Create heatmap for OGs in each species
   heat_plot4 <- reactive({
     
     library(ggplot2)
     library(dplyr)
     library(data.table)
     
     random.file <- random.file4()
     table_heat <- table_heat4()
     selected_organisms <- selected_organisms4()
     org_sel <- as.character(sapply(selected_organisms, function(x) gsub(" ", "_", tolower(x))))
     org_first <- sapply(strsplit(org_sel, split = "_"), function(x) x[[1]])
     
     # Load gene count for each OG and filter out species that are not selected by the user
     genecount_file <- ifelse(model.selected4(), "pharaoh_folder/global_genecount.tsv",
                              "pharaoh_folder/green_genecount.tsv")
     genecount_data_raw <- fread(genecount_file)
     org_index <- sapply(org_first, function(x) grep(x, colnames(genecount_data_raw)), USE.NAMES = F)
     org_index <- as.numeric(c(1, org_index))
     genecount_data <- genecount_data_raw[,c(org_index), with=F]
     
     # Create index to retain order and dups in the subset
     genecount_index <- sapply(table_heat$OG, function(x) which(x == genecount_data$Orthogroup))
     genecount_filt <- genecount_data[genecount_index,]
     
     genecount_filt$Orthogroup <- table_heat$query_id
     colnames(genecount_filt) <- c("query", colnames(genecount_filt)[-1])
     
     # Convert from wide to long format for plotting
     data_plot <- melt(genecount_filt,
                       id.vars = c("query"),
                       measure.vars = patterns("*_"),
                       variable.name = "species",
                       value.name = "value")
     
     # Create heatmap
     heat_plot <- ggplot(data_plot, aes(species, query, fill= value)) + 
       geom_tile(color = "white",
                 linewidth = 1.5,
                 linetype = 1) +
       scale_fill_gradientn(
         values = c(0, quantile(data_plot$value, 0.1), quantile(data_plot$value, 0.2),
                    quantile(data_plot$value, 0.3), quantile(data_plot$value, 0.4),
                    quantile(data_plot$value, 0.7)),
         guide = "colourbar",
         aesthetics = "fill",
         colors = c("white", "#fcd57a", "#fcc139", "#f48131", "#ed1411", "#ab0808")
       ) +
       theme(axis.text.x = element_text(angle = 65, hjust = 1, size=11),
             axis.text.y = element_text(size=12)) 
     
     return(heat_plot)
     
   }) 
   
   # Create boxes
   
   observeEvent(isTruthy(heat_plot4()), {
     
     if (UI_exist_batch4)
     {
      
       removeUI(
         selector = "div:has(>> #heat_image4)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadBatch4)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     table_heat <- table_heat4()
     
     insertUI("#box_heatmap4", "afterEnd", ui = {
       box(width = 12, height = nrow(table_heat)*30+20,
           title = "Orthogroups Heatmap", status = "warning", solidHeader = TRUE,
           collapsible = TRUE, 
           plotOutput("heat_image4", height = nrow(table_heat)*30-20)
       )
     })
     
     insertUI("#download_batch4", "afterEnd", ui = {
       tags$div(style = "margin-left: 300px;", shinyWidgets::downloadBttn(outputId= "downloadBatch4", 
                                                                          "Download Results Folder",
                                                                          size = "sm", color = "warning"))
     })
     
     UI_exist_batch4 <<- TRUE
     shinyjs::hideElement(id = 'loading.batch4')
   })
   
   # Fill boxes with output
   
   # Render heatmap
   output$heat_image4 <- renderImage({
     
     table_heat <- table_heat4()
     heat_plot <- heat_plot4()
     
     png("heatmap.png", height = nrow(table_heat)*30-20, width = 1000)
     plot(heat_plot)
     dev.off()
     
     list(src = "heatmap.png",
          contentType="image/png", width=1000,height=nrow(table_heat)*30-20)
   }, deleteFile = T)
   
   # Download results
   output$downloadBatch4 <- downloadHandler(
     filename= function() {
       paste0("batch_results", ".zip")
     },
     content= function(file) {
       random.file <- random.file4()
       file.copy(paste0(random.file, ".zip"), file)
       file.remove(paste0(random.file, ".zip"))
     })
   

# End of Batch Mode Search
   
   
######################## SHOOT-BASED SEARCH #################
   
   # Set global variables for tracking changes in output
   UI_exist_pfam5 <<- F
   UI_exist_tree5 <<- F
   UI_exist_phylo5 <<- F
   UI_exist_cafe5 <<- F
   UI_exist_error_cafe5 <<- F
   UI_exist_msa5 <<- F
   UI_exist_go5 <<- F
   UI_exist_kegg5 <<- F
   UI_exist_pathview5 <<- F
   UI_exist_lit5 <<-  F
   UI_exist_string5 <<-  F
   UI_exist_network5 <<-  F

   # To avoid autoupdating some inputs, define variables with its values
   model.selected5 <- reactive({
     model.selected <- !input$switch5
     return(model.selected)
   })%>% bindEvent(input$run_button5)

   # Load organisms selection based on the model selected
   selected_organisms5 <- reactive({
     selected_organisms <- c(input$mami_check_5,input$chloro_check_5, input$strepto_check_5,
                             input$bryo_check_5, input$lyco_check_5, input$sperma_check_5)
     if(model.selected5()){selected_organisms <- c(input$tsar_check_5, input$rhodo_check_5,
                                                   input$glauco_check_5,selected_organisms)}
     return(selected_organisms)

   }) %>% bindEvent(input$run_button5)

   selected_values_org5 <- reactive(organisms_values[selected_organisms5()]) %>% bindEvent(input$run_button5)
   
   random.file5 <- reactive({

     random.file <- paste0(c("neworg_", sample(LETTERS, 7, replace = T), sample.int(9, size=5, replace = T)), collapse = "")
     return(random.file)
   }) %>% bindEvent(input$run_button5)
   
   # First, clean sequence 
   shoot_sequence5 <- reactive({
     
     library(stringr)
     shinyjs::showElement(id = 'loading.tree5')
     seq_comp <- as.character(input$geneInt5)
     seq_comp_clean <- toupper(gsub("[\r\n\t]", "", seq_comp))
     seq_comp_clean2 <- str_replace_all(seq_comp_clean, fixed(" "), "")
     vec_comp <- str_split_1(seq_comp_clean2, pattern = "")
     return(vec_comp)
     
   }) %>% bindEvent(input$run_button5)

   
   tree5 <- reactive({

     library(seqinr)
     library(ape)
     vec_comp <- shoot_sequence5()
     random_name <- random.file5()
     
     # Create a FASTA file with the query seq (using a random file name to
     # avoid conflicts between simultaneous users) for comparison
     random_system <- paste0(random_name, "_system")
     write.fasta(vec_comp, names = "query_prot", paste0("pharaoh_folder/", random_system, ".fa"))

     # Then, execute DIAMOND in server to create .sh.ogs.txt.gz file and execute SHOOT

     database <- ifelse(model.selected5(), "Results_Apr19", "Results_Apr22")

     system(paste0("cd srv/shiny-server/pharaoh_folder;
            diamond blastp --db ", database, "/profile_sequences.all.fa -q ", random_system, ".fa -o ", random_system, ".fa.sh.ogs.txt.gz --quiet -e 0.001 --compress 1 -p 1;
            python3 /home/operador/SHOOT/shoot ", random_system, ".fa ", database, "/"))
    
     # Remove input fasta file once SHOOT results have been generated
     file.remove(paste0("pharaoh_folder/", random_system, ".fa"))
     
     # Load gene tree file depending on the input
     #shoot_tree <- ape::read.tree("../shoot/pruebas_shoot/query.fa.shoot.tre")
     shoot_tree <- ape::read.tree(paste0("pharaoh_folder/", random_system, ".fa.shoot.tre"))
     
     # TODO Remove the rest of useless SHOOT files, except for MSA
     
     return(shoot_tree)

   }) %>% bindEvent(input$run_button5)


   # Tips to keep of each species with proper notation

   tips_to_keep.query5 <- reactive({

     tree <- tree5()

     # Selection of genes from the selected organism
     tips_to_keep.query <- grep(pattern = "query_prot",tree$tip.label)

     return(tips_to_keep.query)
   })

   tips_to_keep.mp5 <- reactive({

     tree <- tree5()
     # Selection of organisms
     organisms.list <- c(selected_values_org5())

     # Selection of genes from the selected organism
     tips_to_keep.mp <- c()
     if ("mp" %in% organisms.list)
     {
       tips_to_keep.mp <- grep(pattern = "marchantia",tree$tip.label)
     }
     return(tips_to_keep.mp)
   })

   tips_to_keep.ot5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.ot <- c()
     if ("ot" %in% organisms.list)
     {
       tips_to_keep.ot <- grep(pattern = "ostreoco",tree$tip.label)
     }
     return(tips_to_keep.ot)
   })

   tips_to_keep.at5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.at <- c()
     if ("at" %in% organisms.list)
     {
       tips_to_keep.at <- grep(pattern = "arabidopsis",tree$tip.label)
     }
     return(tips_to_keep.at)
   })

   tips_to_keep.cp5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.cp <- c()
     if ("cp" %in% organisms.list)
     {
       tips_to_keep.cp <- grep(pattern = "ceratodon",tree$tip.label)
     }

     return(tips_to_keep.cp)
   })

   tips_to_keep.cr5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.cr <- c()
     if ("cr" %in% organisms.list)
     {
       tips_to_keep.cr <- grep(pattern = "chlamy",tree$tip.label)
     }
     return(tips_to_keep.cr)
   })

   tips_to_keep.cz5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.cz <- c()
     if ("cz" %in% organisms.list)
     {
       tips_to_keep.cz <- grep(pattern = "chromochloris",tree$tip.label)
     }
     return(tips_to_keep.cz)
   })

   tips_to_keep.kn5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.kn <- c()
     if ("kn" %in% organisms.list)
     {
       tips_to_keep.kn <- grep(pattern = "klebsormidium",tree$tip.label)
     }
     return(tips_to_keep.kn)
   })

   tips_to_keep.me5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.me <- c()
     if ("me" %in% organisms.list)
     {
       tips_to_keep.me <- grep(pattern = "mesotaenium",tree$tip.label)
     }
     return(tips_to_keep.me)
   })

   tips_to_keep.mi5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.mi <- c()
     if ("mi" %in% organisms.list)
     {
       tips_to_keep.mi <- grep(pattern = "micromonas",tree$tip.label)
     }
     return(tips_to_keep.mi)
   })

   tips_to_keep.pp5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.pp <- c()
     if ("pp" %in% organisms.list)
     {
       tips_to_keep.pp <- grep(pattern = "physcomitrium",tree$tip.label)
     }
     return(tips_to_keep.pp)
   })

   tips_to_keep.sl5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.sl <- c()
     if ("sl" %in% organisms.list)
     {
       tips_to_keep.sl <- grep(pattern = "solanum",tree$tip.label)
     }
     return(tips_to_keep.sl)
   })

   tips_to_keep.sm5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.sm <- c()
     if ("sm" %in% organisms.list)
     {
       tips_to_keep.sm <- grep(pattern = "selaginella",tree$tip.label)
     }
     return(tips_to_keep.sm)
   })

   tips_to_keep.sp5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.sp <- c()
     if ("sp" %in% organisms.list)
     {
       tips_to_keep.sp <- grep(pattern = "spirogloea",tree$tip.label)
     }
     return(tips_to_keep.sp)
   })

   tips_to_keep.ta5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.ta <- c()
     if ("ta" %in% organisms.list)
     {
       tips_to_keep.ta <- grep(pattern = "triticum",tree$tip.label)
     }
     return(tips_to_keep.ta)
   })

   tips_to_keep.vc5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.vc <- c()
     if ("vc" %in% organisms.list)
     {
       tips_to_keep.vc <- grep(pattern = "volvox",tree$tip.label)
     }
     return(tips_to_keep.vc)
   })

   tips_to_keep.bp5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.bp <- c()
     if ("bp" %in% organisms.list)
     {
       tips_to_keep.bp <- grep(pattern = "bathycoccus",tree$tip.label)
     }
     return(tips_to_keep.bp)
   })

   tips_to_keep.cri5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.cri <- c()
     if ("cri" %in% organisms.list)
     {
       tips_to_keep.cri <- grep(pattern = "ceratopteris",tree$tip.label)
     }
     return(tips_to_keep.cri)
   })

   tips_to_keep.ds5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.ds <- c()
     if ("ds" %in% organisms.list)
     {
       tips_to_keep.ds <- grep(pattern = "dunaliella",tree$tip.label)
     }
     return(tips_to_keep.ds)
   })

   tips_to_keep.os5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.os <- c()
     if ("os" %in% organisms.list)
     {
       tips_to_keep.os <- grep(pattern = "oryza",tree$tip.label)
     }
     return(tips_to_keep.os)
   })

   tips_to_keep.smag5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.smag <- c()
     if ("smag" %in% organisms.list)
     {
       tips_to_keep.smag <- grep(pattern = "sphagnum",tree$tip.label)
     }
     return(tips_to_keep.smag)
   })

   tips_to_keep.tp5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.tp <- c()
     if ("tp" %in% organisms.list)
     {
       tips_to_keep.tp <- grep(pattern = "thuja",tree$tip.label)
     }
     return(tips_to_keep.tp)
   })

   tips_to_keep.aa5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.aa <- c()
     if ("aa" %in% organisms.list)
     {
       tips_to_keep.aa <- grep(pattern = "anthoceros",tree$tip.label)
     }
     return(tips_to_keep.aa)
   })

   tips_to_keep.um5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.um <- c()
     if ("um" %in% organisms.list)
     {
       tips_to_keep.um <- grep(pattern = "ulva",tree$tip.label)
     }
     return(tips_to_keep.um)
   })

   tips_to_keep.rs5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.rs <- c()
     if ("rs" %in% organisms.list)
     {
       tips_to_keep.rs <- grep(pattern = "raphidocelis",tree$tip.label)
     }
     return(tips_to_keep.rs)
   })

   tips_to_keep.cyc5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.cyc <- c()
     if ("cyc" %in% organisms.list)
     {
       tips_to_keep.cyc <- grep(pattern = "cycas",tree$tip.label)
     }
     return(tips_to_keep.cyc)
   })

   tips_to_keep.pu5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.pu <- c()
     if ("pu" %in% organisms.list)
     {
       tips_to_keep.pu <- grep(pattern = "porphyra",tree$tip.label)
     }
     return(tips_to_keep.pu)
   })

   tips_to_keep.pt5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.pt <- c()
     if ("pt" %in% organisms.list)
     {
       tips_to_keep.pt <- grep(pattern = "phaeodactylum",tree$tip.label)
     }
     return(tips_to_keep.pt)
   })

   tips_to_keep.ng5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.ng <- c()
     if ("ng" %in% organisms.list)
     {
       tips_to_keep.ng <- grep(pattern = "gaditana",tree$tip.label)
     }
     return(tips_to_keep.ng)
   })

   tips_to_keep.cyano5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.cyano <- c()
     if ("cyano" %in% organisms.list)
     {
       tips_to_keep.cyano <- grep(pattern = "cyanophora",tree$tip.label)
     }
     return(tips_to_keep.cyano)
   })

   tips_to_keep.ca5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.ca <- c()
     if ("ca" %in% organisms.list)
     {
       tips_to_keep.ca <- grep(pattern = "chlorokybus",tree$tip.label)
     }
     return(tips_to_keep.ca)
   })

   tips_to_keep.mv5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.mv <- c()
     if ("mv" %in% organisms.list)
     {
       tips_to_keep.mv <- grep(pattern = "mesostigma",tree$tip.label)
     }
     return(tips_to_keep.mv)
   })

   tips_to_keep.af5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.af <- c()
     if ("af" %in% organisms.list)
     {
       tips_to_keep.af <- grep(pattern = "azolla",tree$tip.label)
     }
     return(tips_to_keep.af)
   })

   tips_to_keep.sc5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.sc <- c()
     if ("sc" %in% organisms.list)
     {
       tips_to_keep.sc <- grep(pattern = "salvinia",tree$tip.label)
     }
     return(tips_to_keep.sc)
   })

   tips_to_keep.aegi5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.aegi <- c()
     if ("aegi" %in% organisms.list)
     {
       tips_to_keep.aegi <- grep(pattern = "aegilops",tree$tip.label)
     }
     return(tips_to_keep.aegi)
   })

   tips_to_keep.sb5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.sb <- c()
     if ("sb" %in% organisms.list)
     {
       tips_to_keep.sb <- grep(pattern = "sorghum",tree$tip.label)
     }
     return(tips_to_keep.sb)
   })

   tips_to_keep.chara5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.chara <- c()
     if ("chara" %in% organisms.list)
     {
       tips_to_keep.chara <- grep(pattern = "chara",tree$tip.label)
     }
     return(tips_to_keep.chara)
   })

   tips_to_keep.guilla5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.guilla <- c()
     if ("guilla" %in% organisms.list)
     {
       tips_to_keep.guilla <- grep(pattern = "guillardia",tree$tip.label)
     }
     return(tips_to_keep.guilla)
   })

   tips_to_keep.crypto5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.crypto <- c()
     if ("crypto" %in% organisms.list)
     {
       tips_to_keep.crypto <- grep(pattern = "cryptophyceae",tree$tip.label)
     }
     return(tips_to_keep.crypto)
   })

   tips_to_keep.cymero5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.cymero <- c()
     if ("cymero" %in% organisms.list)
     {
       tips_to_keep.cymero <- grep(pattern = "cyanidioschyzon",tree$tip.label)
     }
     return(tips_to_keep.cymero)
   })

   tips_to_keep.galsul5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.galsul <- c()
     if ("galsul" %in% organisms.list)
     {
       tips_to_keep.galsul <- grep(pattern = "galdieria",tree$tip.label)
     }
     return(tips_to_keep.galsul)
   })

   tips_to_keep.gracichor5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.gracichor <- c()
     if ("gracichor" %in% organisms.list)
     {
       tips_to_keep.gracichor <- grep(pattern = "gracilariopsis",tree$tip.label)
     }
     return(tips_to_keep.gracichor)
   })

   tips_to_keep.sceobli5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.sceobli <- c()
     if ("sceobli" %in% organisms.list)
     {
       tips_to_keep.sceobli <- grep(pattern = "scenedesmus",tree$tip.label)
     }
     return(tips_to_keep.sceobli)
   })

   tips_to_keep.cocco5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.cocco <- c()
     if ("cocco" %in% organisms.list)
     {
       tips_to_keep.cocco <- grep(pattern = "coccomyxa",tree$tip.label)
     }
     return(tips_to_keep.cocco)
   })

   tips_to_keep.saccha5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.saccha <- c()
     if ("saccha" %in% organisms.list)
     {
       tips_to_keep.saccha <- grep(pattern = "saccharina",tree$tip.label)
     }
     return(tips_to_keep.saccha)
   })

   tips_to_keep.haema5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.haema <- c()
     if ("haema" %in% organisms.list)
     {
       tips_to_keep.haema <- grep(pattern = "haematococcus",tree$tip.label)
     }
     return(tips_to_keep.haema)
   })

   tips_to_keep.zm5 <- reactive({

     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     tips_to_keep.zm <- c()
     if ("zm" %in% organisms.list)
     {
       tips_to_keep.zm <- grep(pattern = "mays",tree$tip.label)
     }
     return(tips_to_keep.zm)
   }) %>% bindEvent(input$run_button5)
   
   # Create complete gene tree with the proper name for each gene
   # For this, we split the species name apart from the gene name
   tree_adj5 <- reactive({
     tree <- tree5()
     organisms.list <- c(selected_values_org5())
     
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
   }) %>% bindEvent(input$run_button5)
   
   # Generate reduced tree when the corresponding button is activated
   tree_reduced5 <- reactive({
     
     tree <- tree_adj5()
     # Define tips to keep (selected organisms) and generate the reduced tree
     tips_to_keep.global <- c(tips_to_keep.mp5(), tips_to_keep.ot5(), tips_to_keep.at5(), tips_to_keep.cp5(),
                              tips_to_keep.cr5(), tips_to_keep.cz5(), tips_to_keep.kn5(), tips_to_keep.me5(),
                              tips_to_keep.mi5(), tips_to_keep.pp5(), tips_to_keep.sl5(), tips_to_keep.sm5(),
                              tips_to_keep.sp5(), tips_to_keep.ta5(), tips_to_keep.vc5(), tips_to_keep.bp5(),
                              tips_to_keep.cri5(), tips_to_keep.ds5(), tips_to_keep.os5(), tips_to_keep.smag5(),
                              tips_to_keep.tp5(), tips_to_keep.aa5(), tips_to_keep.um5(), tips_to_keep.rs5(),
                              tips_to_keep.cyc5(), tips_to_keep.pu5(), tips_to_keep.pt5(), tips_to_keep.ng5(),
                              tips_to_keep.cyano5(), tips_to_keep.ca5(), tips_to_keep.mv5(), tips_to_keep.af5(),
                              tips_to_keep.sc5(), tips_to_keep.aegi5(), tips_to_keep.sb5(), tips_to_keep.chara5(),
                              tips_to_keep.guilla5(), tips_to_keep.crypto5(), tips_to_keep.cymero5(), tips_to_keep.galsul5(),
                              tips_to_keep.gracichor5(), tips_to_keep.sceobli5(), tips_to_keep.cocco5(), tips_to_keep.saccha5(),
                              tips_to_keep.haema5(),tips_to_keep.zm5(), tips_to_keep.query5())
     
     # Error message if trying to build tree with less than two tips
     {
     if (length(tips_to_keep.global) < 2)
     {
       shinyjs::hideElement(id = 'loading.tree5')
       
       if (UI_exist_tree5)
       {
         removeUI(
           selector = "div:has(>> #treeTips5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>>> #presentorg5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_image5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTree5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadNewick5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadTreeSeqs5)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       
       UI_exist_tree5 <<- F
       output$error_tree5 <- renderUI({renderText({print("Unable to construct
      tree with a single tip, please select more organisms.")})})
       validate(need(length(tips_to_keep.global) > 1, " "))
     }
     }
     
     tips_to_drop <- setdiff(1:length(tree$tip.label), tips_to_keep.global)
     tree_reduced <- drop.tip(tree, tips_to_drop)
     
     return(tree_reduced)
   }) %>% bindEvent(input$run_button5)
   
   organims_reduced5 <- reactive({
     
     tree_reduced <- tree_reduced5()
     
     len.mp <- length(tips_to_keep.mp5())
     len.ot <- length(tips_to_keep.ot5())
     len.at <- length(tips_to_keep.at5())
     len.cp <- length(tips_to_keep.cp5())
     len.cr <- length(tips_to_keep.cr5())
     len.cz <- length(tips_to_keep.cz5())
     len.kn <- length(tips_to_keep.kn5())
     len.me <- length(tips_to_keep.me5())
     len.mi <- length(tips_to_keep.mi5())
     len.pp <- length(tips_to_keep.pp5())
     len.sl <- length(tips_to_keep.sl5())
     len.sm <- length(tips_to_keep.sm5())
     len.sp <- length(tips_to_keep.sp5())
     len.ta <- length(tips_to_keep.ta5())
     len.vc <- length(tips_to_keep.vc5())
     len.bp <- length(tips_to_keep.bp5())
     len.cri <- length(tips_to_keep.cri5())
     len.ds <- length(tips_to_keep.ds5())
     len.os <- length(tips_to_keep.os5())
     len.smag <- length(tips_to_keep.smag5())
     len.tp <- length(tips_to_keep.tp5())
     len.aa <- length(tips_to_keep.aa5())
     len.um <- length(tips_to_keep.um5())
     len.rs <- length(tips_to_keep.rs5())
     len.cyc <- length(tips_to_keep.cyc5())
     len.pu <- length(tips_to_keep.pu5())
     len.pt <- length(tips_to_keep.pt5())
     len.ng <- length(tips_to_keep.ng5())
     len.cyano <- length(tips_to_keep.cyano5())
     len.ca <- length(tips_to_keep.ca5())
     len.mv <- length(tips_to_keep.mv5())
     len.af <- length(tips_to_keep.af5())
     len.sc <- length(tips_to_keep.sc5())
     len.aegi <- length(tips_to_keep.aegi5())
     len.sb <- length(tips_to_keep.sb5())
     len.chara <- length(tips_to_keep.chara5())
     len.guilla <- length(tips_to_keep.guilla5())
     len.crypto <- length(tips_to_keep.crypto5())
     len.cymero <- length(tips_to_keep.cymero5())
     len.galsul <- length(tips_to_keep.galsul5())
     len.gracichor <- length(tips_to_keep.gracichor5())
     len.sceobli <- length(tips_to_keep.sceobli5())
     len.cocco <- length(tips_to_keep.cocco5())
     len.saccha <- length(tips_to_keep.saccha5())
     len.haema <- length(tips_to_keep.haema5())
     len.zea <- length(tips_to_keep.zm5())
     
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
   }) #%>% bindEvent(input$run_button5)
   
   tree_plot5 <- reactive({
     
     # Define previous variables
     tree_reduced <- tree_reduced5()
     gene.name.tree <- "query_prot"
     tree <- tree_adj5()
     
     tips_to_keep.mp <- tips_to_keep.mp5()
     tips_to_keep.ot <- tips_to_keep.ot5()
     tips_to_keep.at <- tips_to_keep.at5()
     tips_to_keep.cp <- tips_to_keep.cp5()
     tips_to_keep.cr <- tips_to_keep.cr5()
     tips_to_keep.cz <- tips_to_keep.cz5()
     tips_to_keep.kn <- tips_to_keep.kn5()
     tips_to_keep.me <- tips_to_keep.me5()
     tips_to_keep.mi <- tips_to_keep.mi5()
     tips_to_keep.pp <- tips_to_keep.pp5()
     tips_to_keep.sl <- tips_to_keep.sl5()
     tips_to_keep.sm <- tips_to_keep.sm5()
     tips_to_keep.sp <- tips_to_keep.sp5()
     tips_to_keep.ta <- tips_to_keep.ta5()
     tips_to_keep.vc <- tips_to_keep.vc5()
     tips_to_keep.bp <- tips_to_keep.bp5()
     tips_to_keep.cri <- tips_to_keep.cri5()
     tips_to_keep.ds <- tips_to_keep.ds5()
     tips_to_keep.os <- tips_to_keep.os5()
     tips_to_keep.smag <- tips_to_keep.smag5()
     tips_to_keep.tp <- tips_to_keep.tp5()
     tips_to_keep.aa <- tips_to_keep.aa5()
     tips_to_keep.um <- tips_to_keep.um5()
     tips_to_keep.rs <- tips_to_keep.rs5()
     tips_to_keep.cyc <- tips_to_keep.cyc5()
     tips_to_keep.pu <- tips_to_keep.pu5()
     tips_to_keep.pt <- tips_to_keep.pt5()
     tips_to_keep.ng <- tips_to_keep.ng5()
     tips_to_keep.cyano <- tips_to_keep.cyano5()
     tips_to_keep.ca <- tips_to_keep.ca5()
     tips_to_keep.mv <- tips_to_keep.mv5()
     tips_to_keep.af <- tips_to_keep.af5()
     tips_to_keep.sc <- tips_to_keep.sc5()
     tips_to_keep.aegi <- tips_to_keep.aegi5()
     tips_to_keep.sb <- tips_to_keep.sb5()
     tips_to_keep.chara <- tips_to_keep.chara5()
     tips_to_keep.guilla <- tips_to_keep.guilla5()
     tips_to_keep.crypto <- tips_to_keep.crypto5()
     tips_to_keep.cymero <- tips_to_keep.cymero5()
     tips_to_keep.galsul <- tips_to_keep.galsul5()
     tips_to_keep.gracichor <- tips_to_keep.gracichor5()
     tips_to_keep.sceobli <- tips_to_keep.sceobli5()
     tips_to_keep.cocco <- tips_to_keep.cocco5()
     tips_to_keep.saccha <- tips_to_keep.saccha5()
     tips_to_keep.haema <- tips_to_keep.haema5()
     tips_to_keep.zm <- tips_to_keep.zm5()
     
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
           org.factor <- c(org.factor,"Query sequence")
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
       
       tree_plot <- ggtree(tree_reduced) %<+% d2 + geom_tiplab() + theme(legend.position =) +
             xlim(0, max(tree_reduced$edge.length)*3) + geom_tiplab(aes(label = label, color = organism)) +
             scale_color_manual(values = unique(d2$col), breaks = unique(d2$org)) +
             geom_highlight(mapping=aes(subset = label %in% high.gene,
                                        node = node,
                                        fill = as.factor(node)), extend = 0.8) + 
             labs(fill = "Node of interest")
           
       #shinyjs::hideElement(id = 'loading.tree5')
       return(tree_plot)
     }}) #%>% bindEvent(input$run_button5)
   
   
   # Identify OG from SHOOT output
   og.name5 <- reactive({

     # Path1: read .sh.ogs.txt.gz file for OG
     og.name.file <- read.table("../shoot/pruebas_shoot/query.fa.assign.txt", header = F, sep="\t", skip = 1)
     og.name <- paste0("OG", sprintf("%07d", og.name.file$V2))
     return(og.name)

   }) %>% bindEvent(input$run_button5)
   
   # Create sequences file for extended OG
   ortho_seq5 <- reactive({
     
     file.name <- og.name5()
     shoot_sequence <- shoot_sequence5()
     random_name <- random.file5()
     
     library(seqinr)
     
     # Load orthogroup sequences file
     ortho.seq.name <- ifelse(model.selected5(),
                              paste("Global_Orthogroup_Sequences",paste(file.name, "fa", sep = "."), sep="/"),
                              paste("Green_Orthogroup_Sequences",paste(file.name, "fa", sep = "."), sep="/"))
     
     # Add query protein sequence
     ortho_seq <- read.fasta(ortho.seq.name, seqtype = "AA")
     ortho_seq_names <- c("query_prot", getName(ortho_seq))
     ortho_seq_seqs <- c(list(shoot_sequence), getSequence(ortho_seq))
     write.fasta(ortho_seq_seqs, names =  ortho_seq_names, file.out = paste0(random_name, "_tmp.fa"))
     ortho_seq_ext <- read.fasta(paste0(random_name, "_tmp.fa"), seqtype = "AA")
     file.remove(paste0(random_name, "_tmp.fa"))
     
     return(ortho_seq_ext)
   })
   
   ### Select orthogroup sequences based on the reduced tree
   ortho_reduced5 <- reactive({
     
     tree_reduced <- tree_reduced5()
     ortho_seq <- ortho_seq5()
     ortho_reduced <- ortho_seq[tree_reduced$tip.label]
     return(ortho_reduced)
   }) %>% bindEvent(input$run_button5)
   
   # Outputs
   observeEvent(isTruthy(ortho_reduced5()), {
     output$error_tree5 <- NULL
   })
   
   # Create boxes
   observeEvent(isTruthy(ortho_reduced5()), {
     
     if (UI_exist_tree5)
     {
       removeUI(
         selector = "div:has(>> #treeTips5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>>> #presentorg5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #tree_image5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadTree5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadNewick5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadTreeSeqs5)",
         multiple = TRUE,
         immediate = TRUE
       )
     }
     
     
     insertUI("#box_tree_text5", "afterEnd", ui = {
       box(
         title = "Genes in Orthogroup", status = "primary", solidHeader = TRUE, width = 12,
         collapsible = TRUE,
         verbatimTextOutput("treeTips5")
       )
     }) 
     
     insertUI("#box_tree_pie5", "afterEnd", ui = {
       box(
         title = "Present Organisms", status = "primary", solidHeader = TRUE,
         collapsible = TRUE, width = 12,
         fluidRow(column(1), imageOutput("presentorg5"))
       )
     }) 
     
     insertUI("#box_tree_plot5", "afterEnd", ui = {
       box(width = 12,
           title = "Gene Tree", status = "primary", solidHeader = TRUE,
           collapsible = TRUE, 
           plotOutput("tree_image5", height = 500)
       )
     })
     
     insertUI("#download_tree5", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadTree5", 
                                                                          "Download Tree Plot",
                                                                          size = "sm", color = "primary"))
     })
     
     insertUI("#download_newick5", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadNewick5", 
                                                                          "Download NEWICK Tree",
                                                                          size = "sm", color = "primary"))
     })
     
     insertUI("#download_tree_seqs5", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadTreeSeqs5", 
                                                                          "Download Protein Sequences",
                                                                          size = "sm", color = "primary"))
     })
     
     UI_exist_tree5 <<- TRUE
     shinyjs::hideElement(id = 'loading.tree5')
   })
   
   # Fill boxes with output
   output$treeTips5 <- renderPrint({
     print(tree_reduced5()$tip.label)
   }, width = 400) # %>% bindEvent(input$run_button5)
   
   # Render pie chart
   output$presentorg5 <- renderImage({
     
     {library(ggplot2)
       library(dplyr)
       
       data <- data.frame(table(organims_reduced5()))
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
         #scale_fill_brewer(palette="Set1")
         scale_fill_manual(values = rep(RColorBrewer::brewer.pal(n = 9, name = "Set1"), 
                                        floor(nrow(data)/9)+1))}
     
     png("organims_reduced5.png", height = 450, width = 450)
     plot(a)
     dev.off()
     
     list(src = "organims_reduced5.png",
          contentType="image/png", width=400,height=400)
   }, deleteFile = T)
   
   # Render tree image
   output$tree_image5 <- renderImage({
     png("tree5.png", height = 500, width = 1000)
     plot(tree_plot5())
     dev.off()
     
     list(src = "tree5.png",
          contentType="image/png", width=1000,height=500)
   }, deleteFile = T)
   
   # Download results
   output$downloadTree5 <- downloadHandler(
     filename= function() {
       paste("tree", ".png", sep="")
     },
     content= function(file) {
       image_height <- 300 + 11*length(tree_reduced5()$tip.label)
       image_width <- 200 + 400*max(tree_reduced5()$edge.length)
       png(file, height = image_height, width = image_width)
       plot(tree_plot5())
       dev.off()
     })
   
   # Create and download tree in newick format
   output$downloadNewick5 <- downloadHandler(
     filename= function() {
       paste("tree_newick", ".txt", sep="")
     },
     content= function(file) {
       write.tree(tree_reduced5(), file)
     })
   
   #  # Create and download sequences for genes in tree
   output$downloadTreeSeqs5 <- downloadHandler(
     filename= function() {
       paste("tree_seqs", ".fa", sep="")
     },
     content= function(file) {
       seqinr::write.fasta(sequences = seqinr::getSequence(ortho_reduced5()),
                           names = seqinr::getName(ortho_reduced5()), file.out = file)
     })
   
   ####################### PHYLOWIDGET ############################
   # Remove previous outputs when updated by a new search
   observeEvent(input$run_button5, {
     if (UI_exist_phylo5)
     {
       removeUI(
         selector = "div:has(>>> #phylo_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_phylo5 <<- F
     }
     
   })
   
   
   phylo_tree5 <- reactive({
     
     library(ape)
     tree_phylo <- tree_reduced5()
     
     # Normalize tree depth
     root_id <- length(tree_phylo$tip.label)+1
     norm_factor <- max(dist.nodes(tree_phylo)[root_id,])
     tree_phylo$edge.length <- tree_phylo$edge.length/norm_factor
     
     return(tree_phylo)
     
   }) %>% bindEvent(input$phylo_start5)
   
   observeEvent(isTruthy(phylo_tree5()),{
     
     if(UI_exist_phylo5)
     {
       removeUI(
         selector = "div:has(>>> #phylo_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_phylo5 <<- F
     }
     
     insertUI("#box_phylo5", "afterEnd", ui = {
       phylo_tree <- phylo_tree5()
       phylo_height <- length(phylo_tree$tip.label) *14 + 220
       box(width = 12,
           title = "Interactive Tree", status = "primary", solidHeader = TRUE, height = phylo_height + 100,
           collapsible = TRUE,
           tags$div(id = "phylo_pocket5", style = paste0("width: 1300px; height: ",  phylo_height + 50, "px"),
                    phylowidgetOutput("phylo_plot5", height = paste0(phylo_height,"px"), width = "98%"))
       )
     })
     
     
     UI_exist_phylo5 <<- T
     
   })
   
   output$phylo_plot5 <- renderPhylowidget({
     
     phylo_tree <- phylo_tree5()
     phylowidget(phylo_tree)
   })
   
   
   
   #########################  PFAM  ###############################
   
   observeEvent(input$run_button5, {
     removeUI(
       selector = "div:has(>> #selected_pfamsI5)",
       multiple = TRUE,
       immediate = TRUE
     )
   })
   
   observeEvent(input$pfam_start5, {
     insertUI("#selected_pfams5", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_pfamsI5","Select the desired genes from the tree",
                                 choices=isolate({tree_reduced5()$tip.label}), options = list(`actions-box` = TRUE),
                                 multiple = T, selected = isolate({"query_prot"}))
     })
   })
   
   observeEvent(input$run_button5, {
     removeUI("#pfam_selection5")
   })
   
   observeEvent(input$pfam_start5, {
     insertUI("#pfam_selectionI5", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("pfam_selection5", "Show Pfam Domains", size = "sm",
                                style = "float", color = "royal")
     })
   })
   
   
   observeEvent(input$run_button5, {
     if (UI_exist_pfam5)
     {
       removeUI(
         selector = "div:has(>> #output_pfam_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_download5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadPFAMTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_pfam5 <<- F
     }
   })
   
   
   total_table_pfam5 <- reactive({
     shinyjs::showElement(id = 'loading.pfam.pf5')
     ortho_reduced <- ortho_reduced5()
     sel_genes <- as.vector(input$selected_pfamsI5)
     
     if (length(sel_genes) < 1)
     {
       shinyjs::hideElement(id = 'loading.pfam.pf5')
       removeUI(
         selector = "div:has(>> #output_pfam_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_download5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadPFAMTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       UI_exist_pfam5 <<- F
       output$error_pfam5 <- renderUI({renderText({print("Please select at least one gene.")})})
       validate(need(length(sel_genes) > 0, "Please select at least one gene."))
     }
     
     output$error_pfam5 <- NULL
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
           if (nap.time > 11){
             shinyjs::hideElement(id = 'loading.pfam.pf5')
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
     
   }) %>% bindEvent(input$pfam_selection5)
   
   pfplot5 <- reactive({
     
     total_table_pfam <- total_table_pfam5()
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
     
   }) %>% bindEvent(input$pfam_selection5)
   
   
   # Outputs
   
   observeEvent(isTruthy(pfplot5()), {
     
     if (UI_exist_pfam5)
     {
       removeUI(
         selector = "div:has(>> #output_pfam_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #pfam_download5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadPFAMTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_pfam5", "afterEnd", ui = {
       
       box(
         title = "PFAM Table", status = "primary", solidHeader = TRUE, width = 12,
         collapsible = TRUE,
         dataTableOutput(outputId = "output_pfam_table5"))
     }) 
     
     insertUI("#box_pfplot5", "afterEnd", ui = {
       total_table_pfam <- total_table_pfam5()
       box_pfplot_height <- 150 + 700*length(total_table_pfam$order[nrow(total_table_pfam)])
       box(
         title = "Domains Localization", status = "primary", solidHeader = TRUE, width = 12, #height = box_pfplot_height,
         collapsible = TRUE,
         plotOutput("pfam_plot5", height = box_pfplot_height))
     }) 
     
     insertUI("#pfam_down_button5", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "pfam_download5", "Download PFAM figure",
                                                                          size = "sm", color = "primary"))
     })
     
     insertUI("#download_ui_for_pfam_table5", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadPFAMTable5", "Download PFAM Table",
                                                                          size = "sm", color = "primary"))
     })
     
     UI_exist_pfam5 <<- TRUE
     shinyjs::hideElement(id = 'loading.pfam.pf5')
   })
   
   output$output_pfam_table5 <- renderDataTable({
     total_table_pfam <- total_table_pfam5()
     out_pf_table <- subset(total_table_pfam[,c(1:6,8)], total_table_pfam$type != "CHAIN")
     out_pf_table$description <- sapply(out_pf_table$description, function(x) pfam.link(x))
     colnames(out_pf_table) <- c(colnames(total_table_pfam)[1:6],"biological description")
     return(out_pf_table)
   }, escape=FALSE, options = list(pageLength = 5))
   
   output$pfam_plot5 <- renderImage({
     total_table_pfam <- total_table_pfam5()
     pfam_height <- 50 + 700*length(total_table_pfam$order[nrow(total_table_pfam)])
     pfam_width <- 1000
     pfplot <- pfplot5()
     png("pharaoh_folder/pfam.png",  width = pfam_width, height = pfam_height)
     plot(pfplot)
     dev.off()
     list(src = "pharaoh_folder/pfam.png",
          contentType="image/png")
   }, deleteFile = T
   )
   
   # Download results
   
   output$pfam_download5 <- downloadHandler(
     filename= function() {
       paste("pfam", ".png", sep="")
     },
     content= function(file) {
       total_table_pfam <- total_table_pfam5()
       pfam_height <- 50 + 700*length(total_table_pfam$order[nrow(total_table_pfam)])
       pfam_width <- 1150
       pfplot <- pfplot5()
       
       png(file, height = pfam_height, width = pfam_width)
       plot(pfplot)
       dev.off()
     })
   
   output$downloadPFAMTable5<- downloadHandler(
     filename= function() {
       paste("pfam_table", ".tsv", sep="")
     },
     content= function(file) {
       total_table_pfam <- total_table_pfam5()
       out_pf_table <- subset(total_table_pfam[,c(1:6,8)], total_table_pfam$type != "CHAIN")
       colnames(out_pf_table) <- c(colnames(total_table_pfam)[1:6],"biological description")
       write.table(x = out_pf_table, quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   
   ####################### CAFE #################################
   
   # Remove previous outputs when updated by a new search
   observeEvent(input$run_button5, {
     if (UI_exist_cafe5)
     {
       removeUI(
         selector = "div:has(>> #cafe_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #cafe_mrca5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #cafe_download5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadCAFEPlot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_cafe5 <<- F
     }
     
     if (UI_exist_error_cafe5)
     {
       removeUI(
         selector = "div:has(>> #cafe_error_message5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_error_cafe5 <<- F
     }
   })
   
   ### CAFE parser and tree generator
   cafe_tree5 <- reactive({
     
     shinyjs::showElement(id = 'loading.cafe5')
     
     library(ape)
     
     # Import OG name
     og.cafe <- og.name5()
     
     # Define path to CAFE trees file
     cafe_comp_tree_file <- ifelse(model.selected5(), "pharaoh_folder/global_cafe.tre",
                                   "pharaoh_folder/green_cafe.tre")
     
     # Extract CAFE tree for current OG
     cafe.tree.set <- ape::read.nexus(cafe_comp_tree_file)
     cafe.tree <- cafe.tree.set[[og.cafe]]
     
     if (length(cafe.tree) < 1)
     {
       shinyjs::hideElement(id = 'loading.cafe5')
       if (UI_exist_cafe5)
       {
         removeUI(
           selector = "div:has(>> #cafe_plot5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #cafe_mrca5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #cafe_download5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadCAFEPlot5)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       
       UI_exist_cafe5 <<- F
       
       if(UI_exist_error_cafe5)
       {
         removeUI(
           selector = "div:has(>> #cafe_error_message5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
       }
       insertUI("#error_cafe5", "afterEnd", ui = {
         box(width = 12,
             title = "Ancestral State Reconstruction", status = "primary", solidHeader = TRUE,
             collapsible = TRUE,
             textOutput("cafe_error_message5"))
       })
       
       output$cafe_error_message5 <- renderText({print("No expansions/contraction detected for this orthogroup,
                                                        or infeasible computation due to large size and variance across
                                                        species.")})
       UI_exist_error_cafe5 <<- T
       
       validate(need(length(cafe.tree) > 0 , ""))
     }
     
     return(cafe.tree)
   }) %>% bindEvent(input$cafe_start5)
   
   evo_plot5 <- reactive({
     
     og.cafe <- og.name5()
     cafe.tree <- cafe_tree5()
     
     # Show an error if the orthogroup is not significantly expanded/collapsed in any branch
     
     model.node.number <- ifelse(model.selected5(), 46, 36)
     total.model.node.number <- ifelse(model.selected5(), 91, 71)
     
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
     
     mrca.tree <- read.tree(ifelse(model.selected5(), "pharaoh_folder/species_tree_global.txt",
                                   "pharaoh_folder/species_tree_green.txt"))
     
     node.names <- read.csv(ifelse(model.selected5(), "pharaoh_folder/tax_labels_global.tsv",
                                   "pharaoh_folder/tax_labels_green.tsv"), header = F, sep="\t")
     
     mrca.tree$node.label <- node.names$V2
     
     # Create a timeline for a given OG
     
     tree.name <- ifelse(model.selected5(),
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
     
   }) %>% bindEvent(input$cafe_start5)
   
   evo.paths.id5 <- reactive({
     
     # Create phylogenomic tree with internal nodes names
     og.cafe <- og.name5()
     model.node.number <- ifelse(model.selected5(), 46, 36)
     
     mrca.tree <- read.tree(ifelse(model.selected5(), "pharaoh_folder/species_tree_global.txt",
                                   "pharaoh_folder/species_tree_green.txt"))
     
     node.names <- read.csv(ifelse(model.selected5(), "pharaoh_folder/tax_labels_global.tsv",
                                   "pharaoh_folder/tax_labels_green.tsv"), header = F, sep="\t")
     
     mrca.tree$node.label <- node.names$V2
     
     # Create timeline
     tree.name <- ifelse(model.selected5(),
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
     
   }) %>% bindEvent(input$cafe_start5)
   
   # Outputs
   
   # Remove previous boxes if they exist and create new ones
   observeEvent(isTruthy(evo_plot5()), {
     
     if (UI_exist_cafe5)
     {
       removeUI(
         selector = "div:has(>> #cafe_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #cafe_mrca5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #cafe_download5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadCAFEPlot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     if (UI_exist_error_cafe5)
     {
       removeUI(
         selector = "div:has(>> #cafe_error_message5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_cafe5", "afterEnd", ui = {
       box(width = 12,
           title = "Ancestral State Reconstruction", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           imageOutput("cafe_plot5", height = 500, width = 1000))
     })
     
     insertUI("#box_mrca5", "afterEnd", ui = {
       box(width = 8,
           title = "Most Recent Common Ancestor", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           textOutput("cafe_mrca5")
       )
     })
     
     insertUI("#cafe_down_button5", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "cafe_download5", "Download NEWICK tree",
                                                                          size = "sm", color = "primary"))
     })
     
     insertUI("#download_ui_for_cafe_plot5", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadCAFEPlot5", "Download Ancestral State Plot",
                                                                          size = "sm", color = "primary"))
     })
     
     UI_exist_cafe5 <<- TRUE
     shinyjs::hideElement(id = 'loading.cafe5')
   })
   
   # Fill outputs
   
   output$cafe_plot5 <- renderImage({
     png("evo_plot5.png", height = 500, width = 1000)
     plot(evo_plot5())
     dev.off()
     
     list(src = "evo_plot5.png",
          contentType="image/png", width=1000,height=500)
   }, deleteFile = T)
   
   output$cafe_mrca5 <- renderText({
     print(paste0("Most recent common ancestor for this orthogroup is the
                   ancestor of the clade: ", evo.paths.id5()[1]))
   })
   
   # Download tab's results
   
   output$cafe_download5 <- downloadHandler(
     filename= function() {
       paste("ancestral_newick", ".txt", sep="")
     },
     content= function(file) {
       cafe_tree <- cafe_tree5()
       
       write.tree(cafe_tree, file)
     })
   
   output$downloadCAFEPlot5<- downloadHandler(
     filename= function() {
       paste("ancestral_plot", ".png", sep="")
     },
     content= function(file) {
       evo_plot <- evo_plot5()
       
       png(file)
       plot(evo_plot)
       dev.off()
     })
   
   
   ####################### MSA #################################
   
   observeEvent(input$run_button5, {
     removeUI(
       selector = "div:has(>> #selected_msaI5)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI(
       selector = "div:has(>> #msa_methodI5)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#msa_selection5")
     
     if (UI_exist_msa5)
     {
       removeUI(
         selector = "div:has(>>> #msa_print5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_fa5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_msa5 <<- F
     }
     
   })
   
   observeEvent(input$msa_start5, {
     insertUI("#selected_msa5", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_msaI5","Select the desired genes from the tree to align",
                                 choices=isolate({tree_reduced5()$tip.label}), options = list(`actions-box` = TRUE),
                                 multiple = T, selected = isolate({"query_prot"}))
       
     })
     
     insertUI("#msa_method5", "afterEnd", ui = {
       shinyWidgets::pickerInput(inputId = "msa_methodI5", label = "Choose alignment method", 
                                 choices = c("ClustalOmega", "MAFFT"), selected = "ClustalOmega")
       
     })
     
     insertUI("#msa_selectionI5", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("msa_selection5", "Align Sequences", size = "sm",
                                style = "float", color = "royal")
     })
     
   })
   
   alignseqs5 <- reactive({
     
     library(msa)
     shinyjs::showElement(id = 'loading.msa5')
     
     selected_genes <- as.vector(input$selected_msaI5)
     selected_method <- as.character(input$msa_methodI5)
     file.name <- og.name5()
     
     if (length(selected_genes) < 2)
     {
       shinyjs::hideElement(id = 'loading.msa5')
       
       if (UI_exist_msa5)
       {
         removeUI(
           selector = "div:has(>>> #msa_print5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #msa_download_plot5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #msa_download_fa5)",
           multiple = TRUE,
           immediate = TRUE
         )
       }
       UI_exist_msa5 <<- F
       output$error_msa5 <- renderUI({renderText({print("Please select at least two genes.")})})
       validate(need(length(selected_genes) > 1, "Please select at least two genes."))
     }
     
     output$error_msa5 <- NULL
     
     # If de novo alignment is selected
     {
       if(selected_method == "ClustalOmega")
       {
         # Define path to orthogroup sequences file
         # ortho.seq.name <- ifelse(model.selected5(),
         #                          paste("Global_Orthogroup_Sequences", paste(file.name, "fa", sep = "."), sep="/"),
         #                          paste("Green_Orthogroup_Sequences", paste(file.name, "fa", sep = "."), sep="/"))
         # Load sequences from OG and query
         
         
         # Read orthogroup sequences file and select the genes for alignment
         random.file <- random.file5()
         ortho_reduced <- ortho_reduced5()
         seqinr::write.fasta(sequences = seqinr::getSequence(ortho_reduced),
                             names = seqinr::getName(ortho_reduced), 
                             file.out = paste0(random.file, "_for_msa.fa"))
         
         mySequences1 <- Biostrings::readAAStringSet(paste0(random.file, "_for_msa.fa"))
         mysubseqs <- mySequences1[selected_genes]
         
         # Remove auxiliar fasta and create alignment
         file.remove(paste0(random.file, "_for_msa.fa"))
         alignseqs <- msa(mysubseqs, verbose = F, method = "ClustalOmega")
       }
       
       # If MAFFT alignment is selected
       else
       {
         mySequences1 <- seqinr::read.fasta("../shoot/pruebas_shoot/query.fa.sh.msa.fa", seqtype = "AA")
         #mySequences1 <- seqinr::read.fasta(ortho.seq.name, seqtype = "AA")
         mysubseqs <- mySequences1[selected_genes]
         mysubnames <- seqinr::getName(mySequences1)
         
         # Identify indexes associated with reduced names
         indexes_msa <- sapply(selected_genes, function(x) grep(mysubnames, pattern = x))
         
         # Retrieve those sequences from alignment keeping gaps
         mysubseqs <- mySequences1[indexes_msa]
         names(mysubseqs) <- names(indexes_msa)
         
         # Remove columns with gaps and remove empty spaces in last positions
         seqs_mysubseqs <- seqinr::getSequence(mysubseqs)
         last <- seqs_mysubseqs[[length(seqs_mysubseqs)]]
         last <- last[which(last != " ")]
         seqs_mysubseqs[[length(seqs_mysubseqs)]] <- last
         seqs_mysubseqs <- remove_gaps(seqs_mysubseqs)
         names(seqs_mysubseqs) <- names(mysubseqs)
         
         mysubseqs2 <- unlist(lapply(seqs_mysubseqs, function(x) paste(x, collapse="")))
         
         alignseqs <- Biostrings::AAMultipleAlignment(mysubseqs2, use.names = T)
         
       }
     }
     
     detach("package:msa", unload=TRUE)
     
     return(alignseqs)
     
   }) %>% bindEvent(input$msa_selection5)
   
   # Create boxes for outputs
   observeEvent(isTruthy(alignseqs5()), {
     
     if (UI_exist_msa5)
     {
       removeUI(
         selector = "div:has(>>> #msa_print5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #msa_download_fa5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
    
     insertUI("#box_msa5", "afterEnd", ui = {
       selected_msa <- isolate({input$selected_msaI5})
       msa_height <- ifelse(length(selected_msa) > 14, 550, 400 + 5*length(selected_msa))
       box(width = 12,
           title = "MSA Explorer", status = "primary", solidHeader = TRUE, height = msa_height,
           collapsible = TRUE,
           tags$div(id = "msa_pocket5", style = "width: 1300px; height: 400px",
                    msaROutput("msa_print5"))
           
       )
     })
     
     insertUI("#msa_down_plot5", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "msa_download_plot5", "Download Colored MSA",
                                                                          size = "sm", color = "primary"))
     })
     
     insertUI("#msa_down_fasta5", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "msa_download_fa5", "Download MSA FASTA",
                                                                          size = "sm", color = "primary"))
     })
     
     UI_exist_msa5 <<- TRUE
   })
   
   
   # Fill Output
   output$msa_print5 <- renderMsaR({
     alignseqs <- alignseqs5()
     msaout <- msa::msaConvert(alignseqs, "ape::AAbin")
     msaR(msaout, menu=T, overviewbox = F,  colorscheme = "clustal")
   })
   
   # Prepare variables for pdf construction
   observeEvent(isTruthy(alignseqs5()), {
     alignseqs <- alignseqs5()
     
     library(ggmsa)
     class(alignseqs) <- "AAMultipleAlignment"
     
     for(i in 1:(ncol(alignseqs)%/%100 +1)){
       assign(paste("msapseq", i, sep = ""), ggmsa(alignseqs, 1+(100*(i-1)), i*100, seq_name = TRUE, char_width = 0.5) +
                geom_seqlogo(color = "Chemistry_AA"), envir = as.environment(1), pos=1)
     }
     shinyjs::hideElement(id = 'loading.msa5')
   })
   
   # Download tab's results
   # Download colored MSA in pdf
   output$msa_download_plot5 <- downloadHandler(
     filename= function() {
       paste("msa", ".pdf", sep="")
     },
     content= function(file) {
       selected_msa <- input$selected_msaI5
       alignseqs <- alignseqs5()
       pdf(file, height = 2+length(selected_msa)*0.25, width = 16)
       {
         for(i in 1:(ncol(alignseqs)%/%100 +1)){
           print(mget(paste0("msapseq", i), envir = as.environment(1)))
         }
         dev.off()
       }
     })
   
   # Download MSA in FASTA format
   output$msa_download_fa5<- downloadHandler(
     filename= function() {
       paste("msa", ".fa", sep="")
     },
     content= function(file) {
       alignseqs <- alignseqs5()
       writeXStringSet(as(unmasked(alignseqs), "XStringSet"), file)
     })
   
   
   ####################### GO #################################
   
   observeEvent(input$run_button5, {
     removeUI(
       selector = "div:has(>> #selected_gosI5)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI(
       selector = "div:has(>> #selected_gos_modeI5)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#gos_selectionI5")
     
     if (UI_exist_go5)
     {
       removeUI(
         selector = "div:has(>> #output_gos_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_treeplot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadGOSTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_download5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #tree_gos_download5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_go5 <<- F
     }
     
   })
   
   
   observeEvent(input$go_start5, {
     insertUI("#selected_gos5", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_gosI5","Select the desired genes from the tree",
                                 choices=isolate({tree_reduced5()$tip.label[which(tree_reduced5()$tip.label != "query_prot")]}), 
                                 options = list(`actions-box` = TRUE),
                                 multiple = T, selected = isolate({tree_reduced5()$tip.label[1]}))
       
     })
     
     insertUI("#selected_gos_mode5", "afterEnd", ui = {
       
       selectInput(inputId = "selected_gos_modeI5",
                   choices=c("Biological Processes" = "bp",
                             "Molecular Functions" = "mf",
                             "Cellular Components" = "cc"),
                   label = "Select the gene ontology to use",
                   multiple = F, selected = c("bp"))
       
     })
     
     insertUI("#gos_selection5", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("gos_selectionI5", "Show GO terms", size = "sm",
                                style = "float", color = "royal")
     })
     
   })
   
   total_table_gos5 <- reactive({
     
     shinyjs::showElement(id = 'loading.go5')
     gos_anot <- read.csv("pharaoh_folder/final_anot_table.tsv", sep="\t", header = T)
     sel.genes.go <- as.vector(input$selected_gosI5)
     selected_gos_mode <- as.character(isolate({input$selected_gos_modeI5}))
     
     total_table_gos <- subset(gos_anot, gos_anot$name %in% sel.genes.go)
     
     # Show an error if no terms are identified in the input
     if (nrow(total_table_gos) == 0) 
     {
       shinyjs::hideElement(id = 'loading.go5')
       if (UI_exist_go5)
       {
         removeUI(
           selector = "div:has(>> #output_gos_table5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_plot5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_treeplot5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadGOSTable5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_download5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_gos_download5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_go5 <<- F
       }
       output$error_gos5 <- renderUI({
         renderPrint({cat("0 GO terms identified.")})
       })
       
       validate(need(nrow(total_table_gos) != 0, " "))
     }
     
     
     gos_sel <- paste("gos", selected_gos_mode, sep="_")
     terms_sel <- paste("terms", selected_gos_mode, sep="_")
     total_table_gos <- total_table_gos[,c("organism", "id", "name", gos_sel, terms_sel)]
     
     # Once removed the two GO categories not selected, remove rows with blank cells
     total_table_gos_clean <- subset(total_table_gos, gos_sel != "")
     
     # Show an error if no terms are identified after the previous operation
     if (nrow(total_table_gos_clean) == 0) 
     {
       shinyjs::hideElement(id = 'loading.go5')
       if (UI_exist_go5)
       {
         removeUI(
           selector = "div:has(>> #output_gos_table5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_plot5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_treeplot5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadGOSTable5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #gos_download5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #tree_gos_download5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_go5 <<- F
       }
       
       output$error_gos5 <- renderUI({
         renderPrint({cat("0 GO terms identified.")})
       })
       
       validate(need(nrow(total_table_gos_clean) != 0, " "))
     }
     
     output$error_gos5 <- NULL
     
     return(total_table_gos_clean)
     
   }) %>% bindEvent(input$gos_selectionI5)
   
   enr_table5 <- reactive({
     
     total_table_gos <- total_table_gos5()
     selected_gos_mode <- as.character(isolate({input$selected_gos_modeI5}))
     
     # Create the plot
     
     # Create a list of GO terms vector of each gene
     gos_sel <- paste("gos", selected_gos_mode, sep="_")
     gos_list <- apply(total_table_gos,MARGIN=1,FUN = function(x) trimws(strsplit(as.character(x[gos_sel]), split = "[|]")[[1]]))
     names(gos_list) <- total_table_gos$name
     
     # Count GOs for each gene and create a matrix of gene-GO pairs
     count_gos_in_genes <- sapply(gos_list, FUN = function(x) length(x))
     comp_data <- data.frame(gene = rep(names(count_gos_in_genes), count_gos_in_genes), gos = as.character(unlist(gos_list)))
     
     # Collapse genes that share a same GO
     gene.v <- c()
     for (i in 1:length(unique(comp_data$gos)))
     {
       new.table <- subset(comp_data, gos == unique(comp_data$gos)[i])
       new.cha <- as.character(new.table$gene)
       gene.v <- c(gene.v, paste(new.cha, collapse = "/"))
     }
     
     names(gene.v) <- unique(comp_data$gos)
     
     # Load libraries and create gene chains, count and GO IDs fields (same order)
     library(GO.db)
     library("multienrichjam")
     library(clusterProfiler)
     library(enrichplot)
     library(ggplot2)
     
     count_go <- table(comp_data$gos)
     geneids <- gene.v[names(count_go)]
     count_terms <- mapply(function(x) {Term(x)}, names(count_go), USE.NAMES = F)
     
     # Create pseudo-enrichment table
     enr_table <- data.frame(ID=names(count_go), Description=count_terms, GeneRatio="90/100", BgRatio="90/10000", 
                             pvalue=0.000005, p.adjust=0.000005, qvalue=0.000005, geneID=geneids, Count = as.vector(count_go))
     
     return(enr_table)
     
   }) %>% bindEvent(input$gos_selectionI5)
   
   ema_gos_plot5 <- reactive({
     
     enr_table <- enr_table5()
     
     # Transform to enrichResult object
     enr <- enrichDF2enrichResult(enrichDF = enr_table, keyColname = "ID",
                                  geneColname = "geneID", pvalueColname = "p.adjust",
                                  descriptionColname = "Description", pvalueCutoff = 0.05)
     
     # Create plot
     ema_gos_plot <- emapplot(pairwise_termsim(enr), showCategory = 15) + theme(legend.position='none')
     return(ema_gos_plot)
   })
   
   tree_gos_plot5 <- reactive({
     
     enr_table <- enr_table5()
     
     # Transform to enrichResult object
     enr <- enrichDF2enrichResult(enrichDF = enr_table, keyColname = "ID",
                                  geneColname = "geneID", pvalueColname = "p.adjust",
                                  descriptionColname = "Description", pvalueCutoff = 0.05)
     {
       if (nrow(enr_table) > 4)
       {
         tree_gos_plot <- treeplot(pairwise_termsim(enr),showCategory = 15, cluster.params = list(label_words_n = 3)) +
           theme(legend.position='none')
       }
       else if (nrow(enr_table) > 2)
       {
         tree_gos_plot <- treeplot(pairwise_termsim(enr),showCategory = 15, cluster.params = list(n = 2, label_words_n = 3)) + 
           theme(legend.position='none')
       }
       else
       {
         text <- paste("\n  Unable to create treeplot with less than 3 GO terms \n")
         tree_gos_plot <- ggplot() + 
           annotate("text", x = 4, y = 25, size=8, label = text) + 
           theme_void()
       }
     }
     
     shinyjs::hideElement(id = 'loading.go5')
     return(tree_gos_plot)
   })
   
   # Create boxes for outputs
   observeEvent(isTruthy(tree_gos_plot5()), {
     
     if (UI_exist_go5)
     {
       removeUI(
         selector = "div:has(>> #output_gos_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_treeplot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadGOSTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #gos_download5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #tree_gos_download5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_gos_table5", "afterEnd", ui = {
       box(width = 12,
           title = "GO Terms Table", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_gos_table5")
       )
     })
     
     insertUI("#box_gos_plot5", "afterEnd", ui = {
       box(width = 12,
           title = "GO Terms Plot", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           imageOutput("gos_plot5")
       )
     })
     
     insertUI("#box_gos_treeplot5", "afterEnd", ui = {
       box(width = 12,
           title = "GO Terms Treeplot", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           imageOutput("gos_treeplot5")
       )
     })
     
     insertUI("#download_ui_for_gos_table5", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadGOSTable5", "Download GO Table",
                                                                          size = "sm", color = "primary"))
     })
     
     insertUI("#gos_down_button5", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "gos_download5", "Download GO Plot",
                                                                          size = "sm", color = "primary"))
     })
     
     insertUI("#tree_gos_down_button5", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "tree_gos_download5", "Download GO Treeplot",
                                                                          size = "sm", color = "primary"))
     })
     
     UI_exist_go5 <<- TRUE
   })
   
   # Fill outputs
   # Table
   output$output_gos_table5 <- renderDataTable({
     total_table_gos <- total_table_gos5()
     selected_gos_mode <- as.character(isolate({input$selected_gos_modeI5}))
     gos_sel <- paste("gos", selected_gos_mode, sep="_")
     gos_list <- apply(total_table_gos,MARGIN=1,
                       FUN = function(x) trimws(strsplit(as.character(x[gos_sel]), split = "[|]")[[1]]))
     gos_links <- lapply(gos_list, function(x) sapply(x, go.link))
     gos_formatted <- unlist(lapply(gos_links, function(x) paste0(x, collapse = " | ")))
     total_table_gos[,gos_sel] <- gos_formatted
     total_table_gos
   },escape=FALSE, rownames=F, options =list(pageLength = 5))
   
   # First plot
   output$gos_plot5 <- renderImage({
     ema_gos_plot <- ema_gos_plot5()
     
     png("pharaoh_folder/gosplot.png", width = 590, height = 590, res = 90)
     plot(ema_gos_plot)
     dev.off()
     list(src = "pharaoh_folder/gosplot.png",
          contentType="image/png")
     
   }, deleteFile=T
   )
   
   # Second plot
   output$gos_treeplot5 <- renderImage({
     tree_gos_plot <- tree_gos_plot5()
     
     png("pharaoh_folder/treeplot.png", width = 620, height = 590, res = 80)
     plot(tree_gos_plot)
     dev.off()
     list(src = "pharaoh_folder/treeplot.png",
          contentType="image/png")
     
   }, deleteFile=T
   )
   
   # Download tab's results
   # Download GO table
   output$downloadGOSTable5 <- downloadHandler(
     filename= function() {
       paste("GOS_table", ".tsv", sep="")
     },
     content= function(file) {
       total_table_gos <- total_table_gos5()
       
       write.table(x = total_table_gos,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Download emaplot
   output$gos_download5 <- downloadHandler(
     filename= function() {
       paste("gos_plot", ".png", sep="")
     },
     content= function(file) {
       ema_gos_plot <- ema_gos_plot5()
       
       png(file, height = 1200, width = 1500, res=140)
       plot(ema_gos_plot)
       dev.off()
     })
   
   # Download treeplot
   output$tree_gos_download5 <- downloadHandler(
     filename= function() {
       paste("gos_treeplot", ".png", sep="")
     },
     content= function(file) {
       tree_gos_plot <- tree_gos_plot5()
       
       png(file, height = 1200, width = 1500, res=140)
       plot(tree_gos_plot)
       dev.off()
     })
   
   
   ###################### KEGG ###########################
   
   observeEvent(input$run_button5, {
     removeUI(
       selector = "div:has(>> #selected_kosI5)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#kos_selectionI5")
     
     if (UI_exist_kegg5)
     {
       removeUI(
         selector = "div:has(>> #output_kos_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #output_kegg_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKOSTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #selected_pathsI5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI("#paths_buttonI5")
       
       UI_exist_kegg5 <<- F
     }
     
     if (UI_exist_pathview5)
     {
       removeUI(
         selector = "div:has(>> #path_image5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGpathway5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_pathview5 <<- F
     }
     
     
   })
   
   observeEvent(input$kegg_start5, {
     insertUI("#selected_kos5", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_kosI5","Select the desired genes from the tree",
                                 choices=isolate({tree_reduced5()$tip.label[which(tree_reduced5()$tip.label != "query_prot")]}), 
                                 options = list(`actions-box` = TRUE),
                                 multiple = T, selected = isolate({tree_reduced5()$tip.label[1]}))
       
       
     })
     
     
     insertUI("#kos_selection5", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("kos_selectionI5", "Show KEGG pathways", size = "sm",
                                style = "float", color = "royal")
     })
     
   })
   
   tab_kegg5 <- reactive({
     
     shinyjs::showElement(id = 'loading.ko5')
     # Create KOs set
     kos_anot <- read.csv("pharaoh_folder/ko_table_funtree.tsv", sep="\t", header = T)
     sel.genes.ko <- as.vector(isolate({input$selected_kosI5}))
     
     tab_kegg <- subset(kos_anot, gene %in% sel.genes.ko)
     set_kegg <- tab_kegg$ko[tab_kegg$ko != ""]
     
     # Show an error if no terms are identified in the input
     {
       if (length(set_kegg) == 0) 
       {
         shinyjs::hideElement(id = 'loading.ko5')
         output$error_kos5 <- renderUI({
           renderPrint({cat("0 KO terms identified. Please select more genes. If this 
        message persists, it should be interpreted as a lack of KO annotation for this orthogroup")})
         })
         
         if (UI_exist_kegg5)
         {
           removeUI(
             selector = "div:has(>> #output_kos_table5)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #output_kegg_table5)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #downloadKOSTable5)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #downloadKEGGTable5)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #selected_pathsI5)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI("#paths_buttonI5")
           
           UI_exist_kegg5 <<- F
         }
         
         if (UI_exist_pathview5)
         {
           removeUI(
             selector = "div:has(>> #path_image5)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           removeUI(
             selector = "div:has(>> #downloadKEGGpathway5)",
             multiple = TRUE,
             immediate = TRUE
           )
           
           UI_exist_pathview5 <<- F
         }
         
         validate("No KO terms detected")
       }
     }
     
     output$error_kos5 <- NULL
     
     return(tab_kegg)
     
   }) %>% bindEvent(input$kos_selectionI5)
   
   total_table_kegg5 <- reactive({
     
     tab_kegg <- tab_kegg5()
     set_kegg <- tab_kegg$ko[tab_kegg$ko != ""]
     
     # Load libraries
     library(clusterProfiler)
     library(enrichplot)
     
     # Enrich with pvalue cutoff = 1 to show all paths
     kos_enrich <- enrichKEGG(gene         = set_kegg,
                              organism     = 'ko',
                              pvalueCutoff = 1)
     
     total_table_kegg <- as.data.frame(kos_enrich)
     
     # Filter out pathways that are not present in plants
     kegg_plants <- read.csv("pharaoh_folder/pathways_plant.ids", sep = "\t", header = T)$x
     total_table_kegg <- subset(total_table_kegg, ID %in% kegg_plants)
     
     return(total_table_kegg)
     
   }) %>% bindEvent(input$kos_selectionI5)
   
   total_table_kos5 <- reactive({
     
     tab_kegg <- tab_kegg5()
     
     library(KEGGREST)
     
     # Collapse genes that share KOs
     
     tab_kegg_for_ko <- subset(tab_kegg, ko != "")
     gene.v.ko <- c()
     for (i in 1:length(unique(tab_kegg_for_ko$ko)))
     {
       new.table <- subset(tab_kegg_for_ko, ko == unique(tab_kegg_for_ko$ko)[i])
       new.cha <- as.character(new.table$gene)
       gene.v.ko <- c(gene.v.ko, paste(new.cha, collapse = "/"))
     }
     
     names(gene.v.ko) <- unique(tab_kegg_for_ko$ko)
     
     # Create gene chains, count and KO IDs fields (same order)
     count_ko <- table(tab_kegg_for_ko$ko)
     geneids.ko <- gene.v.ko[names(count_ko)]
     count_terms.ko <- mapply(function(x) {keggFind("ko", x)}, names(count_ko), USE.NAMES = F)
     total_table_kos <- data.frame(ko=names(count_ko), name=count_terms.ko, count=as.numeric(count_ko),
                                   genes=geneids.ko)
     
     return(total_table_kos)
     
   }) %>% bindEvent(input$kos_selectionI5)
   
   # Create boxes for outputs
   observeEvent(isTruthy(total_table_kos5()), {
     
     if (UI_exist_kegg5)
     {
       removeUI(
         selector = "div:has(>> #output_kos_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #output_kegg_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKOSTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #selected_pathsI5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI("#paths_buttonI5")
       
     }
     
     insertUI("#box_kos_table5", "afterEnd", ui = {
       box(width = 12,
           title = "KO Terms Table", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_kos_table5")
       )
     })
     
     
     insertUI("#box_kegg_table5", "afterEnd", ui = {
       box(width = 12,
           title = "KEGG Pathways Table", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_kegg_table5")
       )
     })
     
     insertUI("#download_ui_for_kos_table5", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadKOSTable5", "Download KO Table",
                                                                          size = "sm", color = "primary"))
     })
     
     insertUI("#download_ui_for_kegg_table5", "afterEnd", ui = {
       tags$div(style = "margin-left: 200px;", shinyWidgets::downloadBttn(outputId= "downloadKEGGTable5", "Download Pathways Table",
                                                                          size = "sm", color = "primary"))
     })
     
     UI_exist_kegg5 <<- TRUE
     
     # Remove previous results for pathview
     if (UI_exist_pathview5)
     {
       removeUI(
         selector = "div:has(>> #path_image5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGpathway5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_pathview5 <<- F
     }
   })
   
   # Fill outputs
   # Render KO table
   output$output_kos_table5 <- renderDataTable({
     total_table_kos <- total_table_kos5()
     total_table_kos$ko <- sapply(total_table_kos$ko, ko.link)
     total_table_kos
   },escape=FALSE, rownames= F, options =list(pageLength = 5))
   
   # Render KEGG table
   output$output_kegg_table5 <- renderDataTable({
     total_table_kegg <- total_table_kegg5()
     total_table_kegg$ID <- sapply(total_table_kegg$ID, kegg.link)
     total_table_kegg[,c("ID", "Description", "geneID")]
   },escape=FALSE, rownames= F, options =list(pageLength = 5))
   
   # Download tab's outputs
   # Download KO table
   output$downloadKOSTable5 <- downloadHandler(
     filename= function() {
       paste("KO_table", ".tsv", sep="")
     },
     content= function(file) {
       total_table_kos <- total_table_kos5()
       write.table(x = total_table_kos,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Download KEGG table
   output$downloadKEGGTable5 <- downloadHandler(
     filename= function() {
       paste("KEGG_table", ".tsv", sep="")
     },
     content= function(file) {
       total_table_kegg <- total_table_kegg5()
       write.table(x = total_table_kegg[,c("ID", "Description", "geneID")],quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Create pathway selector and button
   observeEvent(input$kos_selectionI5,{
     
     total_table_kos <- total_table_kos5()
     total_table_kegg <- total_table_kegg5()
     
     if(nrow(total_table_kegg) != 0)
     {
       paths.options <- sapply(strsplit(total_table_kegg$ID, split = "ko"), function(x) x[[2]])
       
       
       insertUI("#selected_paths5", "afterEnd", ui = {
         shinyWidgets::pickerInput(inputId = "selected_pathsI5", label = "Select the pathway to plot", 
                                   choices = paths.options, selected = paths.options[1], multiple = F)
         
       })
       
       
       insertUI("#paths_button5", "afterEnd", ui = {
         
         shinyWidgets::actionBttn("paths_buttonI5", "Plot Pathway", size = "sm",
                                  style = "float", color = "royal")
       })
       
       shinyjs::hideElement(id = 'loading.ko5')
     }
   })
   
   observeEvent(input$paths_buttonI5,{
     
     if (UI_exist_pathview5)
     {
       removeUI(
         selector = "div:has(>> #path_image5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadKEGGpathway5)",
         multiple = TRUE,
         immediate = TRUE
       )
     }
     
     UI_exist_pathview5 <<- F
     
   })
   
   # Create Image to Render and save path name
   pathway.current.id5 <- reactive({
     pathway.current.id <- input$selected_pathsI5
     total_table_kos <- total_table_kos5()
     
     kos_unique <- unique(total_table_kos$ko)
     gene.pathway <- rep(0, length(kos_unique))
     names(gene.pathway) <-  kos_unique
     gene.pathway[kos_unique] <-1
     
     library(pathview)
     pathview(gene.data = sort(gene.pathway,decreasing = TRUE),kegg.dir = "pharaoh_folder",
              pathway.id = pathway.current.id,
              species = "ko",
              limit = list(gene=max(abs(gene.pathway)), cpd=1),
              gene.idtype ="kegg")
     
     return(pathway.current.id)
     
   }) %>% bindEvent(input$paths_buttonI5)
   
   # Create output box and download button
   observeEvent(isTruthy(pathway.current.id5()),{
     
     insertUI("#box_path_image5", "afterEnd", ui = {
       box(width = 12,
           title = "KEGG Pathway Plot", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           fluidRow(column(1), imageOutput("path_image5", width = "100%", height = "700px"))
       )
     })
     
     insertUI("#path_download_ui5", "afterEnd", ui = {
       tags$div(shinyWidgets::downloadBttn(outputId= "downloadKEGGpathway5", "Download KEGG Pathway Plot",
                                           size = "sm", color = "primary"))
     })
     
     UI_exist_pathview5 <<- T
     
   })
   
   # Fill path image output
   output$path_image5 <- renderImage({
     
     pathway.current.id <- pathway.current.id5()
     list(src = paste(c(paste0(c("ko",pathway.current.id), collapse=""),"pathview","png"), collapse="."),
          contentType="image/png",width=900,height=700)
   },deleteFile = F)
   
   # Download and remove path image output
   output$downloadKEGGpathway5 <- downloadHandler(
     filename= function() {
       paste("path_plot", ".png", sep="")
     },
     content= function(file) {
       pathway.current.id <- pathway.current.id5()
       file.copy(paste(c(paste0(c("ko",pathway.current.id), collapse=""),"pathview","png"), collapse="."), file)
       file.remove(paste(c(paste0(c("ko",pathway.current.id), collapse=""),"pathview","png"), collapse="."))
     })   
   
   
   
   ############################# LITERATURE ANNOTATION ##########################################
   
   observeEvent(input$run_button5, {
     removeUI(
       selector = "div:has(>> #selected_litI5)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI(
       selector = "div:has(> #query_litI5)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#lit_selectionI5")
     
     
     if (UI_exist_lit5)
     {
       removeUI(
         selector = "div:has(>> #output_lit_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadLITTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_lit5 <<- F
     }
     
   })
   
   observeEvent(input$lit_start5, {
     
     insertUI("#query_lit5", "afterEnd", ui = {
       
       textInput(inputId = "query_litI5",value = "", label = "Enter search term", placeholder = "CCA1")
       
     })
     
     
     insertUI("#selected_lit5", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_litI5","Select the search mode",
                                 choices=c("Normal","Exact", "Substring", "Alias"),
                                 multiple = F, selected = "Normal")
       
       
     })
     
     
     insertUI("#lit_selection5", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("lit_selectionI5", "Get biological information and papers", size = "sm",
                                style = "float", color = "royal")
     })
     
   })
   
   pc_result5 <- reactive({
     
     shinyjs::showElement(id = 'loading.lit5')
     pc_search <- as.character(input$query_litI5)
     pc_search <- gsub(" ", "%20", pc_search) 
     pc_modality <- tolower(as.character(input$selected_litI5))
     
     # Get PlantConnectome URL for query
     pc_url <- paste(c("https://connectome.plant.tools", pc_modality, pc_search), collapse = "/")
     
     library(RCurl)
     
     pc_res <- getURL(pc_url)
     
     if (!length(grep("No hits", pc_res)) == 0)
     {
       shinyjs::hideElement(id = 'loading.lit5')
       
       if (UI_exist_lit5)
       {
         removeUI(
           selector = "div:has(>> #output_lit_table5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         removeUI(
           selector = "div:has(>> #downloadLITTable5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_lit5 <<- F
       }
       
       output$error_lit5 <- renderUI({
         renderPrint({cat("No results found for this query")})
       })
       
       validate(" ")
       
     }
     
     output$error_lit5 <- NULL
     
     # Isolate data frame from complete HTML file
     pc_split <- strsplit(as.character(pc_res), split = "<tbody")[[1]][2]
     pc_split <- strsplit(as.character(pc_split), split = "</tbody>")[[1]][1]
     
     pc_clean <- gsub("</tr>", "", pc_split)
     pc_clean <- gsub("[\r\n\t]", "", pc_clean)
     
     pc_vector <- strsplit(pc_clean, split = "<tr>")[[1]][-1]
     pc_vector2 <- sapply(pc_vector, FUN=function(x) strsplit(x, split = "<td> | </td>"))
     pc_table <- sapply(pc_vector2, FUN=function(x) as.character(unlist(x)))
     
     colnames(pc_table) <- NULL
     pc_result <- data.frame(t(pc_table[c(2,4,6,8),]))
     colnames(pc_result) <- c("Source", "Interaction Type", "Target", "Pubmed ID")
     
     return(pc_result)
     
   }) %>% bindEvent(input$lit_selectionI5)
   
   pc_result_show5 <- reactive({
     
     pc_result <- pc_result5()
     
     # Add links to papers
     urls_connect <- sapply(pc_result$`Pubmed ID`, FUN = function(x) paste0(c("<a href=\"",
                                                                              "https://pubmed.ncbi.nlm.nih.gov/",x,"/",
                                                                              "\" target=\"_blank\">", x,
                                                                              "</a>"),
                                                                            collapse=""))
     pc_result_show <- pc_result
     pc_result_show$`Pubmed ID` <- urls_connect
     
     return(pc_result_show)
     
   })
   
   # Create boxes for outputs
   observeEvent(isTruthy(pc_result_show5()), {
     
     if (UI_exist_lit5)
     {
       removeUI(
         selector = "div:has(>> #output_lit_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadLITTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     insertUI("#box_lit_table5", "afterEnd", ui = {
       box(width = 12,
           title = "Literature Table", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_lit_table5")
       )
     })
     
     insertUI("#download_ui_for_lit_table5", "afterEnd", ui = {
       tags$div(style = "margin-left: 400px;", shinyWidgets::downloadBttn(outputId= "downloadLITTable5", "Download Literature Table",
                                                                          size = "sm", color = "primary"))
     })
     
     shinyjs::hideElement(id = 'loading.lit5')
     
     UI_exist_lit5 <<- TRUE
     
   })
   
   # Fill outputs
   # Render table
   output$output_lit_table5 <- renderDataTable({
     pc_result_show5()
   },escape=FALSE, rownames= F, options =list(pageLength = 10))
   
   # Download results
   output$downloadLITTable5 <- downloadHandler(
     filename= function() {
       paste("literature_table", ".tsv", sep="")
     },
     content= function(file) {
       pc_result <- pc_result5()
       write.table(x = pc_result,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })   
   
   
   ######################## STRING ###########################
   
   observeEvent(input$run_button5, {
     removeUI(
       selector = "div:has(>> #selected_stringI5)",
       multiple = TRUE,
       immediate = TRUE
     )
     
     removeUI("#string_selectionI5")
     
     shinyjs::hideElement("error_string5")
     
     
     if (UI_exist_string5)
     {
       removeUI(
         selector = "div:has(>> #output_st_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #output_count_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>>> #count_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadSTRINGTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadCOUNTTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #count_download5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #selected_networkI5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI("#network_buttonI5")
       
       UI_exist_string5 <<- F
     }
     
     if (UI_exist_network5)
     {
       removeUI(
         selector = "div:has(>>>> #network_image5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_network5 <<- F
     }
     
   })
   
   # First, create a table that links reduced gene names to its species
   string_sel_table5 <- reactive({
     
     # Define previous variables
     tree_reduced <- tree_reduced5()
     tree <- tree_adj5()
  
     tips_to_keep.mp <- tips_to_keep.mp5()
     tips_to_keep.ot <- tips_to_keep.ot5()
     tips_to_keep.at <- tips_to_keep.at5()
     tips_to_keep.cp <- tips_to_keep.cp5()
     tips_to_keep.cr <- tips_to_keep.cr5()
     tips_to_keep.cz <- tips_to_keep.cz5()
     tips_to_keep.kn <- tips_to_keep.kn5()
     tips_to_keep.me <- tips_to_keep.me5()
     tips_to_keep.mi <- tips_to_keep.mi5()
     tips_to_keep.pp <- tips_to_keep.pp5()
     tips_to_keep.sl <- tips_to_keep.sl5()
     tips_to_keep.sm <- tips_to_keep.sm5()
     tips_to_keep.sp <- tips_to_keep.sp5()
     tips_to_keep.ta <- tips_to_keep.ta5()
     tips_to_keep.vc <- tips_to_keep.vc5()
     tips_to_keep.bp <- tips_to_keep.bp5()
     tips_to_keep.cri <- tips_to_keep.cri5()
     tips_to_keep.ds <- tips_to_keep.ds5()
     tips_to_keep.os <- tips_to_keep.os5()
     tips_to_keep.smag <- tips_to_keep.smag5()
     tips_to_keep.tp <- tips_to_keep.tp5()
     tips_to_keep.aa <- tips_to_keep.aa5()
     tips_to_keep.um <- tips_to_keep.um5()
     tips_to_keep.rs <- tips_to_keep.rs5()
     tips_to_keep.cyc <- tips_to_keep.cyc5()
     tips_to_keep.pu <- tips_to_keep.pu5()
     tips_to_keep.pt <- tips_to_keep.pt5()
     tips_to_keep.ng <- tips_to_keep.ng5()
     tips_to_keep.cyano <- tips_to_keep.cyano5()
     tips_to_keep.ca <- tips_to_keep.ca5()
     tips_to_keep.mv <- tips_to_keep.mv5()
     tips_to_keep.af <- tips_to_keep.af5()
     tips_to_keep.sc <- tips_to_keep.sc5()
     tips_to_keep.aegi <- tips_to_keep.aegi5()
     tips_to_keep.sb <- tips_to_keep.sb5()
     tips_to_keep.chara <- tips_to_keep.chara5()
     tips_to_keep.guilla <- tips_to_keep.guilla5()
     tips_to_keep.crypto <- tips_to_keep.crypto5()
     tips_to_keep.cymero <- tips_to_keep.cymero5()
     tips_to_keep.galsul <- tips_to_keep.galsul5()
     tips_to_keep.gracichor <- tips_to_keep.gracichor5()
     tips_to_keep.sceobli <- tips_to_keep.sceobli5()
     tips_to_keep.cocco <- tips_to_keep.cocco5()
     tips_to_keep.saccha <- tips_to_keep.saccha5()
     tips_to_keep.haema <- tips_to_keep.haema5()
     tips_to_keep.zm <- tips_to_keep.zm5()
     tips_to_keep.query <- tips_to_keep.query5()
     
     # Table construction
     org.factor <- c()
     
     for (i in 1:length(tree_reduced$tip.label))
     {
       
       if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mp])
       {
         org.factor <- c(org.factor,"Marchantia")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ot])
       {
         org.factor <- c(org.factor,"Ostreococcus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.at])
       {
         org.factor <- c(org.factor,"Arabidopsis")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cp])
       {
         org.factor <- c(org.factor,"Ceratodon")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cr])
       {
         org.factor <- c(org.factor,"Chlamydomonas")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cz])
       {
         org.factor <- c(org.factor,"Chromochloris")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.kn])
       {
         org.factor <- c(org.factor,"Klebsormidium")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.me])
       {
         org.factor <- c(org.factor,"Mesotaenium")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mi])
       {
         org.factor <- c(org.factor,"Micromonas")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pp])
       {
         org.factor <- c(org.factor,"Physcomitrium")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sl])
       {
         org.factor <- c(org.factor,"Solanum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sm])
       {
         org.factor <- c(org.factor,"Selaginella")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sp])
       {
         org.factor <- c(org.factor,"Spirogloea")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ta])
       {
         org.factor <- c(org.factor,"Triticum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.vc])
       {
         org.factor <- c(org.factor,"Volvox")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.bp])
       {
         org.factor <- c(org.factor,"Bathycoccus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cri])
       {
         org.factor <- c(org.factor,"Ceratopteris")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ds])
       {
         org.factor <- c(org.factor,"Dunaliella")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.os])
       {
         org.factor <- c(org.factor,"Oryza")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.smag])
       {
         org.factor <- c(org.factor,"Sphagnum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.tp])
       {
         org.factor <- c(org.factor,"Thuja")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.aa])
       {
         org.factor <- c(org.factor,"Anthoceros")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.um])
       {
         org.factor <- c(org.factor,"Ulva")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.rs])
       {
         org.factor <- c(org.factor,"Raphidocelis")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cyc])
       {
         org.factor <- c(org.factor,"Cycas")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pu])
       {
         org.factor <- c(org.factor,"Porphyra")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.pt])
       {
         org.factor <- c(org.factor,"Phaeodactylum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ng])
       {
         org.factor <- c(org.factor,"Nannochloropsis")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cyano])
       {
         org.factor <- c(org.factor,"Cyanophora")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.ca])
       {
         org.factor <- c(org.factor,"Chlorokybus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.mv])
       {
         org.factor <- c(org.factor,"Mesostigma")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.af])
       {
         org.factor <- c(org.factor,"Azolla")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sc])
       {
         org.factor <- c(org.factor,"Salvinia")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.aegi])
       {
         org.factor <- c(org.factor,"Aegilops")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sb])
       {
         org.factor <- c(org.factor,"Sorghum")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.chara])
       {
         org.factor <- c(org.factor,"Chara")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.guilla])
       {
         org.factor <- c(org.factor,"Guillardia")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.crypto])
       {
         org.factor <- c(org.factor,"Cryptophyceae")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cymero])
       {
         org.factor <- c(org.factor,"Cyanidioschyzon")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.galsul])
       {
         org.factor <- c(org.factor,"Galdieria")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.gracichor])
       {
         org.factor <- c(org.factor,"Gracilariopsis")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.sceobli])
       {
         org.factor <- c(org.factor,"Scenedesmus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.cocco])
       {
         org.factor <- c(org.factor,"Coccomyxa")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.saccha])
       {
         org.factor <- c(org.factor,"Saccharina")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.haema])
       {
         org.factor <- c(org.factor,"Haematococcus")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.zm])
       {
         org.factor <- c(org.factor,"Zea")
       }
       else if (tree_reduced$tip.label[i] %in% tree$tip.label[tips_to_keep.query])
       {
         org.factor <- c(org.factor,"Query")
       }
       
     }
     
     #Matrix with labels and colors and transform to dplyr format
     string.sel.table <- data.frame(node = 1:length(tree_reduced$tip.label), label = tree_reduced$tip.label,
                                    org = org.factor)
     
     return(string.sel.table)
     
   }) %>% bindEvent(input$string_start5)
   
   # Now, selection is allowed for genes of species with STRING support
   observeEvent(input$string_start5, {
     
     string_sel_table <- string_sel_table5()
     allow_string_species <- c("Aegilops", "Arabidopsis", "Bathycoccus", "Chara", "Chlamydomonas",
                               "Coccomyxa", "Cyanidioschyzon", "Galdieria", "Gracilariopsis",
                               "Guillardia", "Klebsormidium", "Micromonas","Oryza",
                               "Ostreococcus", "Phaeodactylum","Physcomitrium", "Raphidocelis",
                               "Scenedesmus", "Selaginella", "Solanum", "Sorghum", "Triticum",
                               "Volvox")
     
     st_genes <- subset(string_sel_table, string_sel_table$org %in% allow_string_species)$label
     
     if (length(st_genes) == 0)
     {
       shinyjs::showElement("error_string5")
       output$error_string5 <- renderUI({renderText({print("No results for this
      analysis due to lack of genes of STRING-supported species in the selection.")})})
       validate(" ")
     }
     
     output$error_string5 <- NULL
     
     insertUI("#selected_string5", "afterEnd", ui = {
       
       shinyWidgets::pickerInput("selected_stringI5","Select the desired genes from the tree",
                                 choices=st_genes, options = list(`actions-box` = TRUE),
                                 multiple = T)
       
     })
     
     
     insertUI("#string_selection5", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("string_selectionI5", "Show STRING Interactions", size = "sm",
                                style = "float", color = "royal")
     })
     
   })
   
   phys_table5 <- reactive({
     
     shinyjs::showElement(id = 'loading.string5')
     query_genes <- input$selected_stringI5
     
     # Load complete STRING annotation table
     library(data.table)
     
     # Load iteratively from split files using data.table format
     data_phys <- fread("pharaoh_folder/string_physical/string_physical_1.tsv")
     
     for (x in list.files("pharaoh_folder/string_physical/")[-1])
     {
       data_phys <- rbind(data_phys, 
                          fread(paste0("pharaoh_folder/string_physical/", x)))
     }
     
     
     
     # Subset by query genes using data.table for speed
     #string_res <- subset(data_phys, data_phys$prot_query %in% query_genes)
     string_res <- data_phys[prot_query %in% query_genes,]
     string_res <- as.data.frame(string_res)
     
     # Assign OG ID to each target
     ortho_data_file <- ifelse(model.selected5(), "Global_Gene_Trees/Orthogroups.tsv",
                               "Green_Gene_Trees/Orthogroups.tsv")
     
     ortho_data <- as.data.frame(fread(ortho_data_file))
     
     ortho_char <- apply(ortho_data, MARGIN = 1, function(x) paste(x, collapse = ","))
     ortho.numbers <- sapply(string_res$prot_interaction, function(x) grep(x, ortho_char), USE.NAMES = F)
     
     # If a pattern is found in several names, i.e., is a subpattern of several genes,
     # search for the exact match
     if(class(ortho.numbers) == "list")
     {
       # If a gene isn't associated to an OG
       index.none <- which(sapply(ortho.numbers, function(x) length(x) == 0))
       
       if (length(index.none) != 0)
       {
         # We create another row for OG table to associate those genes
         ortho_data <- rbind(ortho_data, "No OG")
         ortho.numbers[index.none] <- nrow(ortho_data)
       }
       
       # If a gene has more than one match due to subpatterns
       index.wrong <- which(sapply(ortho.numbers, function(x) length(x) > 1))
       {
         if (length(index.wrong) == 0)
         {
           ortho.numbers <- unlist(ortho.numbers, use.names = F)
         }
         else
         {
           for (i in index.wrong)
           {
             for (j in ortho.numbers[[i]])
             {
               ortho_char_split <- strsplit(ortho_char[j],split = ",")
               ortho_char_split_clean <- sapply(ortho_char_split, function(x) gsub(" ", "", x))
               if (string_res$prot_interaction[i] %in% ortho_char_split_clean)
               {
                 ortho.numbers[i] <- j
                 ortho.numbers <- unlist(ortho.numbers, use.names = F)
               }
             }
           }
         }
       }
     }
     
     
     
     # Create the final table
     ortho.string.names <- ortho_data$Orthogroup[ortho.numbers]
     phys_table <- data.frame(string_res, orthogroup = ortho.string.names)
     
     return(phys_table)
   }) %>% bindEvent(input$string_selectionI5)
   
   # Create count table to identify enriched OGs in STRING result
   string_counts5 <- reactive({
     
     phys_table <- phys_table5()
     string_counts <- sort(table(phys_table$orthogroup), decreasing = T)
     
     return(string_counts)
     
   }) %>% bindEvent(input$string_selectionI5)
   
   string_count_plot5 <- reactive({
     
     library(ggplot2)
     library(dplyr)
     
     data_count <- as.data.frame(string_counts5())
     colnames(data_count) <- c("orthogroup", "value")
     
     # Compute the position of labels
     data_count <- data_count %>%
       arrange(desc(orthogroup)) %>%
       mutate(prop = value / sum(data_count$value) *100) %>%
       mutate(ypos = cumsum(prop)- 0.5*prop )
     
     # Create plot
     count_plot <- ggplot(data_count, aes(x="", y=prop, fill=orthogroup)) +
       geom_bar(stat="identity", width=1, color="white") +
       coord_polar("y", start=0) +
       theme_void() +
       theme(legend.position="none") +
       #geom_text(aes(y = ypos, label = orthogroup), color = "white", size=6) +
       scale_fill_manual(values = rep(RColorBrewer::brewer.pal(n = 9, name = "Set1"), 
                                      floor(nrow(data_count)/9)+1))
     
     return(count_plot)
     
   }) %>% bindEvent(input$string_selectionI5)
   
   # Create boxes for outputs
   observeEvent(isTruthy(string_count_plot5()), {
     
     if (UI_exist_string5)
     {
       removeUI(
         selector = "div:has(>> #output_st_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #output_count_table5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>>> #count_plot5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadSTRINGTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #downloadCOUNTTable5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #count_download5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI(
         selector = "div:has(>> #selected_networkI5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       removeUI("#network_buttonI5")
       
     }
     
     insertUI("#box_st_table5", "afterEnd", ui = {
       box(width = 12,
           title = "STRING Interactions Table", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_st_table5")
       )
     })
     
     insertUI("#box_count_table5", "afterEnd", ui = {
       box(width = 12,
           title = "Interacting Orthogroups Table", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           dataTableOutput("output_count_table5")
       )
     })
     
     insertUI("#box_count_plot5", "afterEnd", ui = {
       box(width = 12,
           title = "Interacting Orthogroups Plot", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           fluidRow(column(1), imageOutput("count_plot5"))
       )
     })
     
     
     insertUI("#download_ui_for_st_table5", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadSTRINGTable5", "Download STRING Table",
                                                                          size = "sm", color = "primary"))
     })
     
     insertUI("#download_ui_for_count_table5", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "downloadCOUNTTable5", "Download OG Count Table",
                                                                          size = "sm", color = "primary"))
     })
     
     insertUI("#count_down_button5", "afterEnd", ui = {
       tags$div(style = "margin-left: 100px;", shinyWidgets::downloadBttn(outputId= "count_download5", "Download OG Count Plot",
                                                                          size = "sm", color = "primary"))
     })
     
     UI_exist_string5 <<- TRUE
     
     # Remove previous results for STRING network
     if (UI_exist_network5)
     {
       removeUI(
         selector = "div:has(>>>> #network_image5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
       UI_exist_network5 <<- F
     }
     
   })
   
   # Fill outputs
   # Render STRING table
   output$output_st_table5 <- renderDataTable({
     phys_table <- phys_table5()
     datatable(phys_table, escape=FALSE, rownames= F, options =list(pageLength = 10)) %>%
       formatStyle(
         'type',
         color = styleEqual(
           c("Direct interaction", "Interolog"), c('green', '#CA931B')
         )
       )
   }) 
   
   
   # Render OG count table
   output$output_count_table5 <- renderDataTable({
     string_counts <- as.data.frame(string_counts5())
     colnames(string_counts) <- c("orthogroup", "count")
     string_counts
   },escape=FALSE, rownames= F, options =list(pageLength = 7))
   
   # Render OG count pie chart
   output$count_plot5 <- renderImage({
     
     string_count_plot <- string_count_plot5()
     
     png("string_count_plot5.png", height = 450, width = 450)
     plot(string_count_plot)
     dev.off()
     
     list(src = "string_count_plot5.png",
          contentType="image/png", width=400,height=400)
   }, deleteFile = T)
   
   
   # Download tab's outputs
   # Download STRING table
   output$downloadSTRINGTable5 <- downloadHandler(
     filename= function() {
       paste("string_table", ".tsv", sep="")
     },
     content= function(file) {
       phys_table <- phys_table5()
       write.table(x = phys_table,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Download count table
   output$downloadCOUNTTable5 <- downloadHandler(
     filename= function() {
       paste("string_count_table", ".tsv", sep="")
     },
     content= function(file) {
       string_counts <- string_counts5()
       write.table(x = string_counts,quote = F,sep = "\t",
                   file=file,row.names=FALSE,col.names=TRUE)
     })
   
   # Download count plot
   output$count_download5 <- downloadHandler(
     filename= function() {
       paste("string_count", ".png", sep="")
     },
     content= function(file) {
       string_count_plot <- string_count_plot5()
       
       png(file, height = 450, width = 450)
       plot(string_count_plot)
       dev.off()
     })
   
   # Create gene selector and button for STRING network representation
   observeEvent(input$string_selectionI5,{
     
     phys_table <- phys_table5()
     network_genes <- unique(phys_table$prot_query)
     
     # Error message if no genes are allowed for selection should have  been reported
     # earlier
     
     insertUI("#selected_network5", "afterEnd", ui = {
       
       shinyWidgets::pickerInput(inputId = "selected_networkI5", label = "Select the gene whose network you want to plot", 
                                 choices = network_genes, selected = network_genes[1],
                                 options = list(`actions-box` = TRUE), multiple = T)
       
     })
     
     
     insertUI("#network_button5", "afterEnd", ui = {
       
       shinyWidgets::actionBttn("network_buttonI5", "Plot STRING Network", size = "sm",
                                style = "float", color = "royal")
     })
     
     shinyjs::hideElement(id = 'loading.string5')
     
   })
   
   mapped_string5 <- reactive({
     
     # Load genes (PharaohFUN IDs) and convert to STRING IDs
     library(data.table)
     
     network_genes <- input$selected_networkI5
     map_table <- fread("pharaoh_folder/string_map.tsv")
     
     # Fast subset using data.table
     map_network <- map_table[pharaohfun_id %in% network_genes,]
     
     # Error if no genes from selection have an associated high fidelity STRING ID
     if (nrow(map_network) == 0)
     {
       
       if (UI_exist_network5)
       {
         removeUI(
           selector = "div:has(>>>> #network_image5)",
           multiple = TRUE,
           immediate = TRUE
         )
         
         UI_exist_network5 <<- F
       }
       
       output$error_network5 <- renderUI({renderText({print("It's not possible to map any
                               genes from selection to STRING IDs, please select different ones.")})})
       validate( " ")
       
     }
     
     output$error_network5 <- NULL
     
     # Get only STRING IDS and paste in a format interpretable by JS function
     string_ids <- as.data.frame(map_network)$string_id
     
     
     mapped_string <- paste0(string_ids, collapse = "%0d")
     return(mapped_string)
     
   }) %>% bindEvent(input$network_buttonI5)
   
   # Create boxes
   observeEvent(isTruthy(mapped_string5()),{
     
     mapped_string <- mapped_string5()
     
     if (UI_exist_network5)
     {
       removeUI(
         selector = "div:has(>>>> #network_image5)",
         multiple = TRUE,
         immediate = TRUE
       )
       
     }
     
     url_interactive <- paste0("https://string-db.org/cgi/network?identifiers=", 
                               mapped_string, 
                               "&add_color_nodes=25&network_flavor=confidence&show_query_node_labels=1")
     
     insertUI("#box_output_network5", "afterEnd", ui = {
       box(width = 12,
           title = "Image", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           fluidRow(column(1), 
                    column(8, htmlOutput("network_image5")), 
                    column(3, div( style = "margin-top: 300px;", 
                                   shinyWidgets::actionBttn("network_link5", "Interactive Network", size = "md", 
                                                            icon = icon("circle-nodes"), style = "float", color = "royal", 
                                                            onclick=paste0("window.open('", url_interactive,"','_blank')")))
                           
                    ))
           
       )
     })
     
     UI_exist_network5 <<- T
     
   })
   
   # Fill network box
   
   output$network_image5 <- renderText({
     
     mapped_string <- mapped_string5()
     src_map <- paste0("https://string-db.org/api/image/network?identifiers=", mapped_string, 
                       "&add_color_nodes=25&network_flavor=confidence")
     
     c('<img src="',src_map,'"width="675" height="625">')
   })
   
   
# End of SHOOT Search
   
   
# End of the whole server function
     }




shinyApp(ui, server)