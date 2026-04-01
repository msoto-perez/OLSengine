# ==============================================================================
# ARCHITECTURE DIAGRAM GENERATOR (Figure 1)
# Uses DiagrammeR to programmatically generate the flowchart
# ==============================================================================

# install.packages("DiagrammeR")
library(DiagrammeR)

# Generate the flowchart using Graphviz (DOT language)
diagram_code <- "
digraph architecture {
  
  # 1. Global Graph Settings (Minimalist, Academic Style)
  graph [layout = dot, rankdir = TB, nodesep = 0.5, ranksep = 0.8]
  
  # 2. Node Settings (Square boxes, black borders, sans-serif font)
  node [shape = box, 
        style = solid, 
        color = black, 
        penwidth = 1.5, 
        fontname = 'Arial', 
        fontsize = 12, 
        margin = '0.3,0.2']
  
  # 3. Define the Nodes
  Wrapper [label = 'High-Level Wrapper\n[paper_engine()]']
  
  Motor [label = 'Mathematical Motor\n(Base R Matrix Algebra)\n[Calculates Coefficients, Sums of Squares, Exact Statistics]']
  
  Customs [label = 'Methodological Customs\n(Aduana)\n[Evaluates VIF, Breusch-Pagan, Shapiro-Wilk]']
  
  Output [label = 'Structured Output\n[Generates APA Tables and Diagnostic Warnings]']
  
  # 4. Edge Settings
  edge [color = black, 
        penwidth = 1.5, 
        fontname = 'Arial', 
        fontsize = 10, 
        arrowsize = 0.8]
        
  # 5. Define the Connections
  Wrapper -> Motor [label = ' Routes data and formula ']
  
  Motor -> Customs [label = ' Feeds raw statistics ']
  Motor -> Output [label = ' Formats raw numbers ']
  
  # 6. Force 'Customs' and 'Output' to be on the same horizontal level
  { rank = same; Customs; Output }
}
"

# Render the plot in the Viewer Pane
grViz(diagram_code)

# Instalar los paquetes necesarios para exportar si no los tienes
# install.packages(c("DiagrammeRsvg", "rsvg"))

library(DiagrammeRsvg)
library(rsvg)

# 1. Guardar el gráfico en una variable
my_graph <- grViz(diagram_code)

# 2. Convertir y exportar a un PNG de alta resolución (2400 pixeles de ancho)
export_svg(my_graph) |>
  charToRaw() |>
  rsvg_png("Figure1_Architecture.png", width = 2400)

cat("Image successfully saved as Figure1_Architecture.png in your working directory.\n")
