
# PharaohFUN: PHylogenomic Analysis foR plAnt prOtein History and FUNction elucidation

This repository contains the source code of the web tool PharaohFUN. This tool aims to overcome functional annotation limitations in photosynthetic organisms by facilitating the inter-species transfer of functional information based on the orthology relationships between genes. 


## Authors

- Marcos Ramos González [@ramosgonzmarc](https://www.github.com/ramosgonzmarc)
- Víctor Ramos González
- Emma Serrano Pérez
- Christina Arvanitidou
- Jorge Hernández García
- Mercedes García González
- Francisco J. Romero Campero


## Documentation
Models and methods are available to execute PharaohFUN locally. For additional information, refer to the pre-print ([https://www.biorxiv.org/content/10.1101/2023.08.01.551440v1.full](https://www.biorxiv.org/content/10.1101/2023.08.01.551440v1.full)). 

R version 4.2.0 and Python version 2.7.8 were used for all the analyses. For the "New organism search" to work, user must install [SHOOT](https://github.com/davidemms/SHOOT), place it inside the main folder PharaohFUN and generate profile databases for both models. Also, both "New organism" and "Sequence" search modes need [DIAMOND aligner](https://github.com/bbuchfink/diamond) to be installed.

R packages dependecies:
- shinydashboard
- bslib
- gridlayout
- DT
- ape
- msaR
- shinyjs
- phylowidget
- plotly
- stringr
- phangorn
- glue
- ggtree
- ggplot2
- dplyr
- RCurl
- drawProteins
- msa
- ggmsa
- GO.db
- multienrichjam
- clusterProfiler
- enrichplot
- KEGGREST
- pathview
- data.table
- seqinr
- rdiamond


## License

[GNU GPLv3](https://choosealicense.com/licenses/gpl-3.0/#)


## Support

For support, send an email to mramos5@us.es.
## 🔗 Links
PharaohFUN is available at https://greennetwork.us.es/PharaohFUN/ and you can consult a tutorial at:

[![youtube](https://img.shields.io/youtube/channel/views/UCRBDDVQHHisLcZtLPlYvmow)](https://www.youtube.com/watch?v=DlU7JuBW7Pk)
