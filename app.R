library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)

simular_coleccion <- function(n, repeticiones, tamano, probabilidades = rep(1/n, n), sin_repetidos = FALSE, meta_porcentaje = 100) {
  resultados <- numeric(repeticiones)
  
  for (i in seq_len(repeticiones)) {
    cupones <- integer(n)
    contador <- 0
    
    # Condición de salida cuando se obtiene el porcentaje de cupones deseado
    while (sum(cupones > 0) < n * meta_porcentaje / 100) {
      # Sobres con varios ítems
      if (sin_repetidos) {
        nuevos <- sample(1:n, tamano, replace = FALSE, prob = probabilidades)  # Sobres sin repetidos
      } else {
        nuevos <- sample(1:n, tamano, replace = TRUE, prob = probabilidades)  # Sobres con repetidos
      }
      
      cupones[nuevos] <- cupones[nuevos] + 1
      contador <- contador + 1
    }
    
    resultados[i] <- contador
  }
  
  return(resultados)
}



simular_teorica <- function(n, tamano, repeticiones = 100000) {
  resultados <- numeric(repeticiones)
  for (i in seq_len(repeticiones)) {
    cupones <- logical(n)
    contador <- 0
    while (sum(cupones) < n) {
      nuevos <- sample.int(n, tamano, replace = TRUE)
      cupones[nuevos] <- TRUE
      contador <- contador + 1
    }
    resultados[i] <- contador
  }
  resultados
}

archivo_csv <- "jugadores_4cupones.csv"

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(disable = TRUE),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Bienvenida", tabName = "inicio", icon = icon("home")),
      
      menuItem("Fundamentos", tabName = "fundamentos", icon = icon("book"),
               menuSubItem("Contexto", tabName = "contexto", icon = icon("info-circle")),
               menuSubItem("Teoría", tabName = "teoria", icon = icon("book-open")),
               menuSubItem("Aplicaciones", tabName = "aplicaciones", icon = icon("project-diagram")),
               menuSubItem("Variantes", tabName = "variantes", icon = icon("random")),
               menuSubItem("Monte Carlo", tabName = "montecarlo", icon = icon("dice"))
      ),
      
      menuItem("Juego", tabName = "juegos", icon = icon("gamepad"),
               menuSubItem("Reglas", tabName = "explicacion", icon = icon("list-ol")),
               menuSubItem("Aplicación", tabName = "juego", icon = icon("puzzle-piece"))
      ),
      
      menuItem("Simulación", tabName = "simulacion", icon = icon("cogs"))
    ),
    tags$div(
      tags$img(src = "Ash.png",
               style = "
               width: 130%;
               margin-top: 5px;
               margin-bottom: auto;
               display: block;
               margin-left: -30px;
               margin-right: 150px;
             "),
      style = "text-align: center;"
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
body, .content-wrapper, .box, .box-header, .box-body, .sidebar-menu, .main-sidebar {
  font-size: 22px !important;
}

/* Tamaños específicos para títulos */
h1 { font-size: 34px !important; }
h2 { font-size: 36px !important; font-weight: bold !important;}
h3 { font-size: 26px !important; }
h4 { font-size: 24px !important; font-weight: bold !important;}

/* Títulos de cajas */
.box-title { font-size: 24px !important; }

/* Menú del sidebar */
.sidebar-menu li a { font-size: 20px !important; }
.sidebar-menu .treeview-menu li a { font-size: 19px !important; }

/* Ancho del sidebar */
.main-sidebar { width: 250px !important; padding-top: 25px !important }

/* Margen del contenido principal */
.content-wrapper {
  margin-left: 240px !important;
  margin-top: 0px !important;
}

/* Inputs y botones */
input[type='text'], input[type='number'], .selectize-input, .action-button {
  font-size: 20px !important;
  height: 45px !important;
}

/* Gráficos */
.shiny-plot-output + div > div > div > div > svg > g > text {
  font-size: 20px !important;
}
.shiny-plot-output text {
  font-size: 16px !important;
}

/* Estadísticas */
.well, .well p, .well code {
  font-size: 22px !important;
}

/* valueBox */
.small-box h3, .small-box p {
  font-size: 24px !important;
}

/* Logo de Pokémon arriba a la derecha */
.custom-header-logo {
  position: fixed;
  top: 10px;
  right: 15px;
  z-index: 9999;
}
.custom-header-logo img {
  height: 100px;
}

/* Logos institucionales abajo a la derecha */
.custom-footer-logos {
  position: fixed;
  bottom: 10px;
  right: 15px;
  z-index: 9999;
}
.custom-footer-logos img {
  height: 50px;
  margin-left: 10px;
}

pre {
  font-size: 22px !important;
  font-family: 'Courier New', monospace;
  line-height: 1.4;
}
  "))
    ),

    tags$div(
      class = "custom-footer-logos",
      tags$img(src = "ESPOL.png"),
      tags$img(src = "DATACLUB.png")
    ),

    tags$div(
      class = "custom-header-logo",
      tags$img(src = "FCNM.png")
    ),
    useShinyjs(),
    tabItems(
      
      # Página de inicio
      tabItem(
        tabName = "inicio",
        fluidPage(
          
          # -------- estilos (mueve a www/custom.css si quieres) --------
          tags$style(HTML("
      /* fondo con gradiente animado */
      .wow-hero {
        position: relative; overflow: hidden; border-radius: 18px;
        background: linear-gradient(120deg,#662D91,#8B5CF6,#22D3EE);
        background-size: 300% 300%;
        animation: grad 16s ease infinite;
        box-shadow: 0 18px 40px rgba(0,0,0,.12); 
      }
      @keyframes grad {
        0%{background-position:0% 50%} 50%{background-position:100% 50%} 100%{background-position:0% 50%}
      }
      /* blobs decorativos */
      .wow-hero:before, .wow-hero:after{
        content:''; position:absolute; width:380px; height:380px; border-radius:50%;
        filter: blur(60px); opacity:.35; pointer-events:none;
      }
      .wow-hero:before{ background:#F59E0B; top:-120px; left:-120px;}
      .wow-hero:after { background:#10B981; bottom:-120px; right:-120px;}

      .wow-inner { display:flex; align-items:center; gap:36px; padding:48px 40px; }
      .wow-left  { flex:1; color:#fff; }
      .wow-right { flex:1; text-align:center; }

      .wow-title { font-size:55px; line-height:1.1; font-weight:900; margin:0 0 10px; text-shadow:0 2px 10px rgba(0,0,0,.25); }
      .wow-sub   { font-size:24px; line-height:1.55; margin:0 0 14px; opacity:.95; }

      /* chips */
      .chip-wrap{ display:flex; gap:10px; flex-wrap:wrap; margin:10px 0 16px; }
      .chip{ background:rgba(255,255,255,.14); border:1px solid rgba(255,255,255,.35);
             color:#fff; padding:8px 12px; border-radius:999px; font-weight:700; backdrop-filter: blur(8px); }

      /* botones */
      .cta-wrap{ margin-top:12px; }
      .btn-white{ background:#fff; color:#662D91; border:none; font-weight:800; }
      .btn-ghost{ background:transparent; color:#fff; border:2px solid rgba(255,255,255,.7); margin-left:8px; font-weight:800; }

      /* imagen con tilt */
      .wow-img{
        max-width:100%; height:auto; border-radius:18px;
        box-shadow:0 18px 40px rgba(0,0,0,.25);
        transform: perspective(900px) rotateY(-8deg) rotateX(2deg);
        transition: transform .4s ease, box-shadow .4s ease;
      }
      .wow-right:hover .wow-img{
        transform: perspective(900px) rotateY(0deg) rotateX(0deg) scale(1.02);
        box-shadow:0 28px 60px rgba(0,0,0,.30);
      }

      /* responsive */
      @media (max-width: 900px){
        .wow-inner{ flex-direction:column-reverse; padding:34px 22px; }
        .wow-title{ font-size:30px; }
      }

      /* Caja con los nombres */
      .team-info {
        background-color: #F3EAFB;
        padding: 20px;
        border-radius: 12px;
        margin-top: 30px;
        text-align: center;
        font-size: 20px;
        font-weight: 700;
        color: #662D91;
      }
      .team-info strong {
        font-size: 24px;
      }
    ")),
          
          # ----------------- contenido -----------------
          tags$div(class="wow-hero", style="margin:100px auto 40px; max-width:1100px;",
                   tags$div(class="wow-inner",
                            
                            # texto
                            tags$div(class="wow-left",
                                     tags$h1(class="wow-title", "Simulación del coleccionista de cupones"),
                                     tags$p(class="wow-sub",
                                            "Un clásico de probabilidad, presentado en vivo. Estima el ",
                                            tags$b("número promedio de intentos"),
                                            " para completar una colección y compruébalo aquí mismo."),
                                     tags$div(class="chip-wrap",
                                              tags$span(class="chip", HTML("📘 Teoría")),
                                              tags$span(class="chip", HTML("🎮 Juego")),
                                              tags$span(class="chip", HTML("⚙️ Simulación"))
                                     ),
                                     tags$div(class="cta-wrap",
                                              actionButton("go_fund", "Empezar por Fundamentos", icon = icon("book"), class="btn-white"),
                                              actionButton("go_play", "Probar el Juego", icon = icon("gamepad"), class="btn-ghost")
                                     )
                            ),
                            
                            tags$div(class="wow-right",
                                     tags$img(src="coleccion.png", class="wow-img")
                            )
                   )
          ),
          
          # Sección de nombres dentro de una caja
          tags$div(class="team-info",
                   tags$p("Desarrollado por:"),
                   tags$strong("Jenny Ochoa, Jaime Arce"),  
                   tags$p("Profesores: Erick Lamilla, Gabriel Barreto")
          )
        )
      ),
      
      tabItem(
        tabName = "contexto",
        fluidPage(
          
          # ---- Estilos (puedes moverlos a www/custom.css) ----
          tags$style(HTML("
      .ctx-wrap {max-width:1100px; margin:150px auto 40px;}
      .ctx-card {background:#fff; border-radius:16px; box-shadow:0 10px 28px rgba(0,0,0,.08);}
      .ctx-inner {display:flex; align-items:center; gap:32px; padding:28px;}
      .ctx-left {flex:1;}
      .ctx-right {flex:1; text-align:center;}
      .ctx-tag {display:inline-block; font-weight:700; color:#662D91; background:#F3EAFB;
                border:1px solid #E4D2F6; padding:6px 10px; border-radius:999px; margin-bottom:8px; font-size:36px;}
      .ctx-title {font-size:28px; font-weight:800; margin:6px 0 10px;}
      .ctx-p {font-size:24px; line-height:1.55; color:#222; margin:0 0 10px;}
      .ctx-img {max-width:100%; height:auto; border-radius:12px;
                box-shadow:0 8px 24px rgba(102,45,145,.25);}
      @media (max-width: 900px){ .ctx-inner{flex-direction:column-reverse;} }
    ")),
          
          # ---- Contenido ----
          tags$div(class="ctx-wrap",
                   tags$div(class="ctx-card",
                            tags$div(class="ctx-inner",
                                     
                                     # Texto
                                     tags$div(class="ctx-left",
                                              tags$span(class="ctx-tag", "Contexto"),
                                              tags$h2(class="ctx-title",
                                                      "¿Cuántos sobres necesito, en promedio, para completar un álbum?"),
                                              tags$p(class="ctx-p",
                                                     "Imagina que intentas llenar un álbum de cromos. 
                    Cada sobre trae cromos al azar: a veces nuevos, a veces repetidos."),
                                              tags$p(class="ctx-p", HTML(
                                                "Este escenario es el <b>problema del coleccionista de cupones</b>:
               un modelo que nos permite estimar ese número promedio."
                                              ))
                                     ),
                                     
                                     # Imagen
                                     tags$div(class="ctx-right",
                                              tags$img(src = "panini2022.png", class = "ctx-img")
                                     )
                            )
                   )
          )
        )
      ),
      
      tabItem(
        tabName = "variantes",
        fluidPage(
          
          # === Estilos locales (si prefieres, muévelos a www/custom.css) ===
          tags$style(HTML("
      .var-wrap {max-width: 1100px; margin: 80px auto 40px;}
      .var-grid {display: grid; grid-template-columns: repeat(2, 1fr); gap: 22px;}
      @media (max-width: 900px){ .var-grid {grid-template-columns: 1fr;} }
      .var-card{
        background:#fff; border-radius:14px; padding:18px 20px;
        box-shadow:0 6px 22px rgba(0,0,0,.08);
        border-left:8px solid #662D91;
      }
      .var-title{font-size:24px; font-weight:700; margin:0 0 6px 0;}
      .var-text{font-size:20px; color:#444; margin:0 0 8px 0;}
      .var-ul{margin:0 0 6px 18px; color:#444; font-size:20px;}
      .var-rule{font-size:18px; color:#555; background:#F6F1FA; border:1px solid #E5D8F4;
                padding:8px 10px; border-radius:8px; display:inline-block;}
      .var-cta{text-align:center; margin-top:26px;}
    ")),
          
          tags$div(class="var-wrap",
                   tags$h2("Variantes"),
                   tags$p(style="font-size:24px; color:#444; margin-bottom:14px;",
                          "El modelo básico supone un ítem por intento y todos igual de probables.
              Estas variantes cambian esas reglas y afectan el tiempo para completar la colección:"),
                   
                   tags$div(class="var-grid",
                            
                            # 1 Packs con varios ítems
                            tags$div(class="var-card",
                                     tags$div(class="var-title", "Sobres con varios ítems (k > 1)"),
                                     tags$p(class="var-text",
                                            "Cada intento trae k figuritas independientes (pueden repetirse)."),
                                     tags$ul(class="var-ul",
                                             tags$li("Progreso más rápido porque agregas varios en un intento."),
                                             tags$li("Si dentro del sobre no se repiten, mejora aún más.")
                                     ),
                                     tags$div(class="var-rule", HTML("<b>Regla rápida:</b> el número de <i>sobres</i> esperado ≈ (n·H<sub>n</sub>)/k."))
                            ),
                            
                            # 2 Probabilidades desiguales (raras/comunes)
                            tags$div(class="var-card",
                                     tags$div(class="var-title", "Probabilidades desiguales (raras y comunes)"),
                                     tags$p(class="var-text",
                                            "Algunos tipos salen mucho menos que otros."),
                                     tags$ul(class="var-ul",
                                             tags$li("El más raro suele ser el último en aparecer."),
                                             tags$li("El tiempo total crece notablemente si hay ‘ultra raros’.")
                                     ),
                                     tags$div(class="var-rule", HTML("<b>Regla rápida:</b> si hay un tipo con probabilidad <i>p</i> muy pequeña, el tiempo típico para verlo ronda 1/<i>p</i>."))
                            ),
                            
                            # 3 Sin repetidos dentro del sobre
                            tags$div(class="var-card",
                                     tags$div(class="var-title", "Sobres sin repetidos (sin reemplazo dentro del sobre)"),
                                     tags$p(class="var-text",
                                            "En cada sobre vienen k diferentes entre sí (pero entre sobres sí puede repetirse)."),
                                     tags$ul(class="var-ul",
                                             tags$li("Mejor que el caso independiente, aprovecha mejor cada sobre."),
                                             tags$li("Efecto más fuerte cuando k es grande respecto a n.")
                                     ),
                                     tags$div(class="var-rule", HTML("<b>Regla rápida:</b> mejora frente a (n·H<sub>n</sub>)/k; cuánto, depende de k/n."))
                            ),
                            
                            # 4 r copias de cada tipo
                            tags$div(class="var-card",
                                     tags$div(class="var-title", "Pedir r copias de cada tipo (r-colecta)"),
                                     tags$p(class="var-text",
                                            "La colección se completa cuando tienes al menos r de cada tipo."),
                                     tags$ul(class="var-ul",
                                             tags$li("Crece mucho la espera: no basta con ‘salga una vez’, deben salir varias."),
                                             tags$li("La variabilidad entre jugadores también aumenta.")
                                     ),
                                     tags$div(class="var-rule", HTML("<b>Regla rápida:</b> pedir 2 o 3 copias incrementa el tiempo más que de forma lineal."))
                            ),
                            
                            # 5 Parcial (p.ej., 80% del set)
                            tags$div(class="var-card",
                                     tags$div(class="var-title", "Meta parcial (completar un % del set)"),
                                     tags$p(class="var-text",
                                            "Se detiene al alcanzar, por ejemplo, el 80% de los tipos."),
                                     tags$ul(class="var-ul",
                                             tags$li("Útil cuando el set es muy grande."),
                                             tags$li("Reduce drásticamente el tiempo comparado con el 100%.")
                                     ),
                                     tags$div(class="var-rule", HTML("<b>Regla rápida:</b> llegar al 90% es ‘barato’; el último 10% es lo más costoso."))
                            ),
                            
                            # 6 Intercambios trueque
                            tags$div(class="var-card",
                                     tags$div(class="var-title", "Intercambios (trueque de repetidos)"),
                                     tags$p(class="var-text",
                                            "Permite cambiar repetidos entre jugadores para cubrir faltantes."),
                                     tags$ul(class="var-ul",
                                             tags$li("Acelera mucho el completado en grupos."),
                                             tags$li("Se aproxima a ‘mezclar’ colecciones y repartir faltantes.")
                                     ),
                                     tags$div(class="var-rule", HTML("<b>Regla rápida:</b> cuantos más jugadores y más equitativos los cambios, menos sobres se necesitan."))
                            )
                   )
          )
        )
      ),
      
      tabItem(
        tabName = "aplicaciones",
        fluidPage(
          
          tags$style(HTML("
      .apps-wrap {max-width: 1100px; margin: 80px auto 40px;}
      .apps-grid {display: grid; grid-template-columns: repeat(2, 1fr); gap: 22px;}
      @media (max-width: 900px){ .apps-grid {grid-template-columns: 1fr;} }
      .app-card {
        background: #fff; border-radius: 14px; padding: 20px 22px;
        box-shadow: 0 6px 22px rgba(0,0,0,.08);
        border-left: 8px solid #662D91; /* morado institucional */
        cursor: pointer;
      }
      .app-icon {font-size: 28px; color: #662D91; margin-right: 10px;}
      .app-title {font-size: 24px; font-weight: 700; margin: 0 0 6px 0;}
      .app-text {font-size: 20px; color: #444; margin: 0;}
      .apps-note {
        margin-top: 18px; font-size: 20px; color:#555;
        background:#F6F1FA; border:1px solid #E5D8F4; padding:12px 14px; border-radius:10px;
      }
      .apps-cta {text-align:center; margin-top: 28px;}
      .app-image {width: 100%; height: auto; border-radius: 10px; box-shadow: 0 0 12px rgba(0,0,0,0.1); margin: 0 auto;}
    ")),
          
          tags$div(class="apps-wrap",
                   tags$h2("Aplicaciones"),
                   tags$p(style="font-size:24px; color:#444;",
                          "Este modelo aparece cada vez que debemos completar un conjunto de elementos que llegan al azar. Algunas aplicaciones:"),
                   
                   # Grid de aplicaciones
                   tags$div(class="apps-grid",
                            
                            # 1. Coleccionismo / promociones
                            tags$div(class="app-card",
                                     tags$div(style="display:flex; align-items:center;",
                                              tags$i(class="fas fa-images app-icon"),
                                              tags$span(class="app-title", "Coleccionismo y promociones")
                                     ),
                                     tags$p(class="app-text",
                                            "Álbumes de figuras, cartas coleccionables o campañas de marketing donde debes juntar todos los ítems de una serie."),
                                     tags$img(src="labubu.png", class="app-image")
                            ),
                            
                            # 2. Ecología / biodiversidad
                            tags$div(class="app-card",
                                     tags$div(style="display:flex; align-items:center;",
                                              tags$i(class="fas fa-leaf app-icon"),
                                              tags$span(class="app-title", "Ecología y biodiversidad")
                                     ),
                                     tags$p(class="app-text",
                                            "Estimación del número de muestreos necesarios para registrar todas las especies de un ecosistema (o la mayoría de ellas)."),
                                     tags$img(src="biomuestra.jpg", class="app-image")
                            ),
                            
                            # 3. Genómica / secuenciación
                            tags$div(class="app-card",
                                     tags$div(style="display:flex; align-items:center;",
                                              tags$i(class="fas fa-dna app-icon"),
                                              tags$span(class="app-title", "Genómica y secuenciación")
                                     ),
                                     tags$p(class="app-text",
                                            "¿Cuántas lecturas hacen falta para cubrir todos los fragmentos de un genoma al menos una vez?"),
                                     tags$img(src="genomica.jpg", class="app-image")
                            ),
                            
                            # 4. Computación / algoritmos
                            tags$div(class="app-card",
                                     tags$div(style="display:flex; align-items:center;",
                                              tags$i(class="fas fa-network-wired app-icon"),
                                              tags$span(class="app-title", "Computación y redes")
                                     ),
                                     tags$p(class="app-text",
                                            "Tiempo esperado para ‘ver’ todos los estados/identificadores en procesos aleatorios, pruebas de software o protocolos de red."),
                                     tags$img(src="computacion.jpeg", class="app-image")
                            )
                   )
          )
        )
      ),
     
      tabItem(
        tabName = "teoria",
        fluidPage(
          withMathJax(),
          
          tags$style(HTML("
      .theo-wrap {max-width:1100px; margin:80px auto 40px;}
      .grid-2 {display:grid; grid-template-columns: 1.2fr 0.8fr;margin-top: 24px; gap: 26px;}
      @media (max-width:900px){ .grid-2 {grid-template-columns:1fr;} }
      .card {background:#fff; border-radius:16px; box-shadow:0 8px 26px rgba(0,0,0,.08); padding:22px;}
      .tag {display:inline-block; font-weight:800; color:#662D91; background:#F3EAFB;
            border:1px solid #E5D8F4; padding:6px 10px; border-radius:999px; margin-bottom:8px; font-size:36px;}
      .title {font-size:26px; font-weight:900; margin:6px 0 10px;}
      .p {font-size:22px; color:#222; line-height:1.55; margin:0 0 8px;}
      .big {font-size:24px;}
      .formula {background:#F7F4FB; border:1px solid #E5D8F4; border-radius:10px; padding:10px 12px; margin:10px 0;}
      details {margin-top:10px;}
      summary {cursor:pointer; font-weight:800; color:#662D91;}
      .pill {display:inline-block; background:#662D91; color:#fff; border-radius:999px; padding:6px 10px; font-weight:800;}
    ")),
          
          tags$div(class="theo-wrap",
                   
                   # Bloque 1: idea y descomposición
                   tags$div(class="card",
                            tags$span(class="tag","Teoría"),
                            tags$h2(class="title","Modelo y descomposición"),
                            tags$p(class="p", HTML(
                              "Tenemos <b>\\(n\\)</b> tipos. Llamamos <b>\\(T\\)</b> al número total de sobres para completar la colección.
           Lo modelamos como una suma de esperas:")),
                            tags$div(class="formula",
                                     "$$T=\\sum_{i=0}^{n-1} X_i,\\quad X_i\\sim\\text{Geom}(p_i),\\; p_i=\\frac{n-i}{n}.$$"
                            ),
                            tags$p(class="p",
                                   "Cada ", tags$b("\\(X_i\\)"), " cuenta cuántos sobres necesito desde que ya tengo ",
                                   "\\(i\\) tipos hasta obtener uno nuevo.")
                   ),
                   
                   # Bloque 2: resultados + definiciones
                   tags$div(class="grid-2",
                            
                            tags$div(class="card",
                                     tags$h3(class="title","Resultados clave"),
                                     tags$p(class="p big", HTML("<span class='pill'>Valor esperado</span>")),
                                     tags$div(class="formula",
                                              "$$\\mathbb{E}[T]=n\\,H_n
              =n\\left(1+\\tfrac12+\\cdots+\\tfrac1n\\right).$$"
                                     ),
                                     
                                     tags$p(class="p big", HTML("<span class='pill'>Varianza</span>")),
                                     tags$div(class="formula",
                                              "$$\\mathrm{Var}(T)=n^2\\,H_{n,2}-n\\,H_n
              =n^2\\sum_{j=1}^{n}\\frac{1}{j^2}-n\\,H_n.$$"
                                     ),
                                     
                                     tags$p(class="p",
                                            "El último tipo domina la espera: si ya tienes \\(n-1\\) tipos, ",
                                            "$$\\mathbb{E}[X_{n-1}] = \\frac{n}{1} = n.$$"),
                                     
                                     tags$details(
                                       tags$summary("Ver demostración breve"),
                                       tags$p(class="p", "1) Con ", "$$p_i=\\tfrac{n-i}{n},$$",
                                              " cada \\(X_i\\) es geométrica (soporte \\(1,2,\\dots\\))."),
                                       tags$p(class="p",
                                              "2) ", "$$\\mathbb{E}[X_i]=\\tfrac{n}{n-i},\\qquad
                           \\mathrm{Var}(X_i)=\\tfrac{i\\,n}{(n-i)^2}.$$"),
                                       tags$p(class="p",
                                              "3) Sumando esperanzas y varianzas de las etapas se obtienen las fórmulas.")
                                     )
                            ),
                            
                            tags$div(class="card",
                                     tags$h3(class="title","Definiciones"),
                                     tags$p(class="p big", HTML("<b>Números armónicos</b>")),
                                     tags$div(class="formula",
                                              "$$H_n=\\sum_{j=1}^{n}\\frac{1}{j},\\qquad
               H_{n,2}=\\sum_{j=1}^{n}\\frac{1}{j^2}.$$"
                                     ),
                                     tags$p(class="p", "Aproximaciones útiles para \\(n\\) grande:"),
                                     tags$div(class="formula",
                                              "$$H_n \\approx \\ln n + \\gamma,\\quad \\gamma\\approx0.57721;$$",
                                              "$$H_{n,2} \\approx \\frac{\\pi^2}{6}.$$"
                                     ),
                                     tags$p(class="p big", HTML("<b>Ejemplo rápido (n = 5)</b>")),
                                     tags$div(class="formula",
                                              "$$\\mathbb{E}[T]=5\\,H_5\\approx\\mathbf{11.417},$$",
                                              "$$\\sigma=\\sqrt{\\mathrm{Var}(T)}\\approx\\mathbf{5.02}.$$"
                                     ))
                   )
          )
        )
      ),
      
      tabItem(
        tabName = "montecarlo",
        fluidPage(
          # --- Estilos para la sección de Monte Carlo ---
          tags$style(HTML("
  .mc-wrap {max-width:1100px; margin:150px auto 40px;}
  .mc-card {background:#fff; border-radius:16px; box-shadow:0 10px 28px rgba(0,0,0,.08);}
  .mc-inner {display:flex; align-items:center; gap:32px; padding:28px;}
  .mc-left {flex:1;}
  .mc-right {flex:1; text-align:center;}
  .mc-tag {display:inline-block; font-weight:700; color:#662D91; background:#F3EAFB;
            border:1px solid #E4D2F6; padding:6px 10px; border-radius:999px; margin-bottom:8px; font-size:36px;}
  .mc-title {font-size:28px; font-weight:800; margin:6px 0 10px;}
  .mc-p {font-size:24px; line-height:1.55; color:#222; margin:0 0 10px;}
  .mc-img {max-width:100%; height:auto; border-radius:12px;
            box-shadow:0 8px 24px rgba(102,45,145,.25);}
  @media (max-width: 900px){ .mc-inner{flex-direction:column-reverse;} }
")),
          
          # --- Contenido de la sección de Monte Carlo ---
          tags$div(class="mc-wrap",
                   tags$div(class="mc-card",
                            tags$div(class="mc-inner",
                                     
                                     # Texto a la izquierda
                                     tags$div(class="mc-left",
                                              tags$span(class="mc-tag", "Monte Carlo"),
                                              tags$h2(class="mc-title", "Método de Simulación Monte Carlo"),
                                              tags$p(class="mc-p",
                                                     "El método Monte Carlo se usa para resolver problemas complejos mediante simulación de escenarios aleatorios. Con cada simulación, se obtiene un conjunto de datos que nos permite obtener estimaciones más precisas."),
                                              tags$p(class="mc-p", HTML(
                                                "Usos típicos: <b>estimación de probabilidades</b>, <b>valor de pi</b>, <b>simulación de sistemas físicos</b>, entre otros."
                                              ))
                                     ),
                                     
                                     # Imagen a la derecha
                                     tags$div(class="mc-right",
                                              tags$img(src = "lanzamientos.jpg", class="mc-img")
                                     )
                            )
                   )
          )
        )
      ),
      
      tabItem(
        tabName = "explicacion",
        fluidPage(
          
          # ====== Estilos (puedes moverlos a www/custom.css) ======
          tags$style(HTML("
      .game-wrap {max-width:1100px; margin:90px auto 40px;}
      .card {background:#fff; border-radius:16px; box-shadow:0 10px 28px rgba(0,0,0,.08); padding:24px; margin-top: 28px; gap: 28px;}
      .tag {display:inline-block; font-weight:800; color:#662D91; background:#F3EAFB;
            border:1px solid #E5D8F4; padding:6px 10px; border-radius:999px; margin-bottom:10px; font-size:36px;}
      .title {font-size:28px; font-weight:900; margin:6px 0 12px;}
      .p {font-size:24px; color:#222; line-height:1.6; margin:0 0 8px;}
      .big {font-size:24px;}
      .formula {background:#F7F4FB; border:1px solid #E5D8F4; border-radius:10px; padding:10px 12px; margin:10px 0;}
      details {margin-top:10px;}
      summary {cursor:pointer; font-weight:800; color:#662D91;}
      .pill {display:inline-block; background:#662D91; color:#fff; border-radius:999px; padding:6px 10px; font-weight:800;}
      .grid-2 {display:grid; grid-template-columns: 1.5fr 0.8fr; gap:22px;}
      .chips {display:flex; gap:10px; flex-wrap:wrap; margin:10px 0 0;}
      .chip {background:#662D91; color:#fff; border-radius:999px; padding:6px 10px; font-weight:800; font-size:20px;}
      .hero-img {max-width:100%; height:auto; border-radius:14px; box-shadow:0 14px 36px rgba(102,45,145,.25);}
      .list {margin:8px 0 0 18px; font-size:22px; color:#333;}
      .cards-3 {display:grid; grid-template-columns: repeat(3,1fr); gap:16px; margin-top:14px;}
      .mini {background:#F7F4FB; border:1px solid #E5D8F4; border-radius:12px; padding:14px;}
      .mini h4 {margin:0 0 6px; font-size:20px; font-weight:900; color:#662D91;}
      .mini p {margin:0; font-size:20px; color:#333;}
      .callout {background:#F3EAFB; border:1px solid #E5D8F4; border-left:8px solid #662D91;
                border-radius:12px; padding:16px 18px; margin-top:18px;}
      .callout h4 {margin:0 0 6px; font-size:20px;}
      .center {text-align:center;}
      .disc-img {width:180px; display:block; margin:16px auto 6px; border-radius:12px;
                 box-shadow:0 10px 26px rgba(0,0,0,.12);}
      @media (max-width:900px){ .grid-2{grid-template-columns:1fr;} .cards-3{grid-template-columns:1fr;} }
    ")),
          
          tags$div(class="game-wrap",
                   
                   # ==== Hero: Cómo se juega ====
                   tags$div(class="card",
                            tags$div(class="grid-2",
                                     
                                     # Texto izquierda
                                     tags$div(
                                       tags$span(class="tag", "Sobre el juego"),
                                       tags$p(class="p",
                                              "Tu misión es abrir cajas y conseguir los ",
                                              tags$b("5 Pokémon diferentes")),
                                       
                                       tags$ul(class="list",
                                               tags$li("Cada vez que completas la colección, el sistema registra cuántos paquetes necesitaste."),
                                               tags$li("Luego comparamos tu resultado con lo que predice la teoría y con simulaciones.")
                                       ),
                                       
                                       tags$div(class="chips",
                                                tags$span(class="chip","🎯 Objetivo: 5 distintos"),
                                                tags$span(class="chip","🎲 Aleatorio"),
                                                tags$span(class="chip","📊 Registramos tus intentos")
                                       )
                                     ),
                                     
                                     # Imagen derecha (Profesor Oak, ahora flotante)
                                     tags$div(
                                       tags$img(src="Oak.webp", style="position: absolute; top: 115px; right: 150px; width: 250px; z-index:10;")
                                     )
                            )
                   ),
                   
                   # ==== Títulos individuales ====
                   tags$div(class="card",
                            tags$span(class="tag", "Reconocimientos"),
                            tags$p(class="p","Según tu desempeño, recibes un título:"),
                            
                            tags$div(class="cards-3",
                                     tags$div(class="mini",
                                              tags$h4("🥇 Maestro Pokémon"),
                                              tags$p("Lo logras en ", tags$b("5 paquetes"), " → ", tags$b("3 medallas"))
                                     ),
                                     tags$div(class="mini",
                                              tags$h4("🥈 Experto Pokémon"),
                                              tags$p("Lo logras en ", tags$b("6 a 8 paquetes"), " → ", tags$b("2 medallas"))
                                     ),
                                     tags$div(class="mini",
                                              tags$h4("🥉 Entrenador Pokémon"),
                                              tags$p("Lo logras en ", tags$b("9 a 11 paquetes"), " → ", tags$b("1 medalla"))
                                     )
                            )
                   ),
                   
                   # ==== Reto de medallas + premio ====
                   tags$div(class="card",
                            tags$span(class="tag","Reto final"),
                            tags$p(class="p",
                                   "Las medallas que ganes se acumulan. Cuando tú (o tu grupo de amigos) junten un total de 8…"),
                            tags$div(class="callout",
                                     tags$h4("🏆 ¡Obtienen el Disco de medallas de Galar!"),
                                     tags$img(src="Disco.png", class="disc-img"),
                                     tags$p(class="p center","Si además logran las 8 medallas + el disco… 🎁 ",
                                            tags$b("¡se llevan una colección física de Pokémon en 3D!"))
                            )
                   )
          )
        )
      ),
      
      # Juego Real
      tabItem(tabName = "juego",
              fluidRow(
                column(width = 5,
                       # Caja para ingresar datos del jugador
                       box(title = "¡Atrapa a tus Pokémon!", status = "primary", solidHeader = TRUE, width = 12,
                           style = "
                        background: #fff; 
                        border-radius: 14px; 
                        padding: 20px 22px; 
                        box-shadow: 0 6px 22px rgba(0,0,0,.08); 
                        border-left: 8px solid #662D91; /* Morado institucional */
                     ",
                           textInput("nombre", label = "Nombre del entrenador", placeholder = "Ingresa tu nombre..."),
                           uiOutput("selector_cupon"),
                           actionButton("registrar", "Registrar Pokémon", icon = icon("plus-circle"), class = "btn-primary"),
                           actionButton("nuevo_jugador", "Nuevo entrenador", icon = icon("user-plus"), class = "btn-success"),
                           verbatimTextOutput("estado_juego"),
                           uiOutput("insignia")
                       ),
                       
                       # Estadísticas teóricas y reales
                       box(title = "Estadísticas Teóricas y Reales", status = "warning", solidHeader = TRUE, width = 12,
                           style = "
                        background: #fff; 
                        border-radius: 14px; 
                        padding: 20px 22px; 
                        box-shadow: 0 6px 22px rgba(0,0,0,.08); 
                        border-left: 8px solid #F59E0B; /* Amarillo suave para resaltar */
                     ",
                           verbatimTextOutput("estadisticas_teoricas")
                       )
                ),
                
                column(width = 7,
                       # Caja para mostrar resultados
                       box(title = "Progreso de Resultados", status = "info", solidHeader = TRUE, width = 12,
                           style = "
                        background: #fff; 
                        border-radius: 14px; 
                        padding: 20px 22px; 
                        box-shadow: 0 6px 22px rgba(0,0,0,.08); 
                        border-left: 8px solid #22D3EE; /* Azul claro para un contraste */
                     ",
                           plotOutput("grafico_acumulado"),
                           
                           # Mostrar el registro de jugadores
                           h4("Registro de Entrenadores"),
                           fluidRow(
                             column(width = 6,
                                    tableOutput("registro")
                             ),
                             column(width = 6,
                                    # Imagen de Bulbasaur o tu Pokémon preferido
                                    tags$img(src = "Bullbasaur.png",
                                             style = "max-width: 100%; height: auto; display: block; margin: 0 auto; margin-right: 100px")
                             )
                           )
                       )
                )
              )
      ),
      
      # Simulación Automática
      tabItem(tabName = "simulacion",
              fluidRow(
                box(title = "Parámetros de Simulación", status = "warning", solidHeader = TRUE, width = 4,
                    numericInput("n_sim", label = "Número de cupones", value = 25, min = 2),
                    numericInput("tamano_sim", label = "Cantidad de cupones por paquete", value = 5, min = 1),
                    numericInput("reps", label = "Número de simulaciones", value = 1000, min = 10),
                    
                    # Agregar opción para probabilidades desiguales
                    checkboxInput("probabilidades_desiguales", "Probabilidades desiguales", FALSE),
                    conditionalPanel(
                      condition = "input.probabilidades_desiguales == true",
                      numericInput("num_grupos_prob", label = "Número de grupos de probabilidades", value = 1, min = 1),
                      uiOutput("grupos_probabilidades_ui")  # Aquí generaremos los inputs para cada grupo
                    ),
                    
                    checkboxInput("sin_repetidos", "Sin repetidos", FALSE),
                    sliderInput("meta_porcentaje", "Parcial (%)", min = 0, max = 100, value = 100),
                    actionButton("simular", "Ejecutar Simulación", icon = icon("play-circle"), class = "btn-warning")
                ),
                box(title = "Resultados de la Simulación", status = "success", solidHeader = TRUE, width = 8,
                    h4("Densidad"),
                    plotOutput("histograma_normal"),
                    
                    verbatimTextOutput("estadisticas")
                )
              ),
              
              tags$img(src = "Piplup.png",
                       style = "max-width: 25%; height: auto; display: block; margin-top: -500px; margin-bottom: 0px; margin-left: 30px; margin-right: auto;
           z-index: 9999;")
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$go_fund, { updateTabItems(session, "tabs", "contexto") })
  observeEvent(input$go_play, { updateTabItems(session, "tabs", "juego") })
  
  valores <- reactiveValues(
    jugador_actual = NULL,
    cupones_jugador = list(),
    registro = data.frame(Jugador = character(), Intentos = integer())
  )
  
  if (file.exists(archivo_csv)) {
    valores$registro <- read.csv(archivo_csv, stringsAsFactors = FALSE)
  }
  
  observeEvent(input$nuevo_jugador, {
    req(input$nombre != "")
    valores$jugador_actual <- input$nombre
    valores$cupones_jugador[[input$nombre]] <- c()
  })
  
  output$selector_cupon <- renderUI({
    selectInput("cupon_obtenido",
                label = "Selecciona el Pokémon obtenido",
                choices = c("Pikachu", "Bulbasour", "Machop", "Charmander", "Mew", "Squirtle", "Piplup", "Dialga", "Palkia"))
  })
  
  observeEvent(input$registrar, {
    req(valores$jugador_actual)
    nombre <- valores$jugador_actual
    cupones <- input[["cupon_obtenido"]]
    valores$cupones_jugador[[nombre]] <- c(valores$cupones_jugador[[nombre]], cupones)
    
    if (length(unique(valores$cupones_jugador[[nombre]])) == 5) {
      if (!(nombre %in% valores$registro$Jugador)) {
        nuevo_registro <- data.frame(Jugador = nombre, Intentos = length(valores$cupones_jugador[[nombre]]))
        valores$registro <- rbind(valores$registro, nuevo_registro)
        
        write.csv(valores$registro, archivo_csv, row.names = FALSE)
      }
    }
  })
  
  output$estado_juego <- renderPrint({
    req(valores$jugador_actual)
    nombre <- valores$jugador_actual
    obtenidos <- length(unique(valores$cupones_jugador[[nombre]]))
    intentos <- length(valores$cupones_jugador[[nombre]])
    cat("Jugador:", nombre, "\n")
    cat("Cupones únicos obtenidos:", obtenidos, "/5 \n")
    cat("Total de paquetes abiertos:", intentos)
  })
  
  output$registro <- renderTable({
    valores$registro
  })
  
  output$grafico_acumulado <- renderPlot({
    req(nrow(valores$registro) > 0)
    
    ggplot(valores$registro, aes(x = Intentos)) +
      geom_histogram(binwidth = 1, fill = "#5B2C8F", color = "#5B2C8F", alpha = 0.8) +
      labs(
        title = "Distribución de intentos reales",
        x = "Intentos", y = "Frecuencia"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22, face = "bold")
      )
  })
  

  
  
  # Generar los inputs para los grupos de probabilidades cuando el usuario selecciona probabilidades desiguales
  output$grupos_probabilidades_ui <- renderUI({
    num_grupos <- input$num_grupos_prob
    lapply(1:num_grupos, function(i) {
      tagList(
        numericInput(paste0("num_cupones_", i), label = paste("Número de cupones en el grupo", i), value = 1, min = 1),
        numericInput(paste0("probabilidad_", i), label = paste("Probabilidad para el grupo", i), value = 1/num_grupos, min = 0, max = 1)
      )
    })
  })
  
  simulacion_normal <- eventReactive(input$simular, {
    # Si se activan probabilidades desiguales
    probabilidades <- rep(1 / input$n_sim, input$n_sim)  # Probabilidades uniformes por defecto
    
    if (input$probabilidades_desiguales) {
      total_cupones <- input$n_sim
      probabilidad_total <- 0  # Para controlar la suma de las probabilidades
      probabilidad_lista <- numeric(total_cupones)
      
      # Asignar probabilidades a los grupos de cupones
      for (i in 1:input$num_grupos_prob) {
        num_cupones <- input[[paste0("num_cupones_", i)]]
        probabilidad <- input[[paste0("probabilidad_", i)]]
        
        # Asignamos la probabilidad a los cupones del grupo
        probabilidad_lista[(sum(probabilidad_lista) + 1):(sum(probabilidad_lista) + num_cupones)] <- rep(probabilidad, num_cupones)
        
        probabilidad_total <- probabilidad_total + num_cupones * probabilidad
      }
      
      # Normalizamos las probabilidades para que sumen 1
      if (probabilidad_total != 1) {
        probabilidad_lista <- probabilidad_lista / probabilidad_total
      }
      
      probabilidades <- probabilidad_lista
    }
    
    # Llamada a la función de simulación con las probabilidades desiguales
    simular_coleccion(
      n = input$n_sim,
      repeticiones = input$reps,
      tamano = input$tamano_sim,
      probabilidades = probabilidades,
      sin_repetidos = input$sin_repetidos,
      meta_porcentaje = input$meta_porcentaje
    )
  })
  
  output$grupos_probabilidades_ui <- renderUI({
    num_grupos <- input$num_grupos_prob
    lapply(1:num_grupos, function(i) {
      tagList(
        numericInput(paste0("num_cupones_", i), label = paste("Número de cupones en el grupo", i), value = 1, min = 1),
        numericInput(paste0("probabilidad_", i), label = paste("Probabilidad para el grupo", i), value = 1/num_grupos, min = 0, max = 1)
      )
    })
  })
  
  output$histograma_normal <- renderPlot({
    req(simulacion_normal())
    df <- data.frame(intentos = simulacion_normal())
    
    ggplot(df, aes(x = intentos)) +
      geom_histogram(aes(y = ..density..), binwidth = 1, 
                     fill = "#5B2C8F", color = "black", alpha = 0.6) +
      geom_density(color = "red", size = 1.2) +
      labs(
        x = "Intentos", y = "Densidad"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 18),
        plot.title = element_text(size = 20, face = "bold")
      )
  })
  
  output$estadisticas <- renderPrint({
    req(simulacion_normal())
    intentos <- simulacion_normal()
    promedio <- mean(intentos)
    varianza <- var(intentos)
    
    cat("Promedio simulado:", round(promedio, 2), "\n")
    cat("Varianza simulada:", round(varianza, 2), "\n")
    cat("Desviación estándar simulada:", round(sqrt(varianza), 2), "\n")
  })
  
  output$estadisticas_teoricas <- renderPrint({
    
    n <- 5
    tamano <- 1
    
    # Estadísticas teóricas
    esperado <- n * sum(1 / (1:n)) / tamano
    var_teo <- sum(((1:n - 1) * n) / (n - (1:n) + 1)^2)
    
    cat(" Estadística teórica:\n")
    cat(" - Promedio teórico:", round(esperado, 2), "\n")
    if (tamano == 1) {
      cat(" - Varianza teórica:", round(var_teo, 2), "\n")
    }
    
    # Estadísticas reales (jugadores)
    if (nrow(valores$registro) > 0) {
      intentos_reales <- valores$registro$Intentos
      promedio_real <- mean(intentos_reales)
      varianza_real <- var(intentos_reales)
      moda_real <- as.numeric(names(sort(table(intentos_reales), decreasing = TRUE)[1]))
      
      cat("\n Estadística real:\n")
      cat(" - Promedio observado:", round(promedio_real, 2), "\n")
      cat(" - Varianza observada:", round(varianza_real, 2), "\n")
      cat(" - Moda:", moda_real, "\n")
    } else {
      cat("\n Aún no hay datos registrados de jugadores.")
    }
  })
  
  output$insignia <- renderUI({
    req(valores$jugador_actual)
    nombre <- valores$jugador_actual
    intentos <- length(valores$cupones_jugador[[nombre]])
    obtenidos <- length(unique(valores$cupones_jugador[[nombre]]))
    
    if (obtenidos == 5) {
      if (intentos == 5) {
        insignia <- "🥇 Leyenda Pokémon (¡3 medallas!)"
      } else if (intentos >= 6 && intentos <= 8) {
        insignia <- "🥈 Experto Pokémon (2 medallas)"
      } else if (intentos >= 9 && intentos <= 11) {
        insignia <- "🥉 Entrenador Pokémon (1 medalla)"
      } else {
        return(NULL)
      }
      
      return(tags$div(
        style = "margin-top: 20px; font-weight: bold; color: #2E86C1;",
        paste("Título obtenido:", insignia)
      ))
    }
  })
  
}

shinyApp(ui, server)
