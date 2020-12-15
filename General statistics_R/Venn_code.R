require("readxl")
library("readxl")
DATA <- read_excel("All_groups_orchid_metabolites (1).xlsx", 1)

Venn <- read_excel("All_groups_orchid_metabolites (1).xlsx", 2)
WildType <- na.omit(Venn$Wild_Control)
MutantControl <- na.omit(Venn$Mutant_Control)
mfTreatment <- na.omit(Venn$MF_Treatment)

##this generate the black and white graph shown in plots 

VennDiagram::get.venn.partitions(list(ad=WildType, ld=MutantControl, md=mfTreatment))
grid.newpage()
grid::grid.draw(VennDiagram::venn.diagram(list(WT=WildType, MC=MutantControl, MFT=mfTreatment), NULL))

##This code below is for customization like the picture, 
#so it generate the picture in ur diretory 

library(RColorBrewer)
myCol <- brewer.pal(3, "Pastel2")

# Chart
venn.diagram(
  x = list(WildType, MutantControl, mfTreatment),
  category.names = c("WT" , " MC " , "MFT"),
  filename = '#14_venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)