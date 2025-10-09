fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#2c3e50",
    primary = "#CB3452",
    secondary = "#A2BDF4",
    success = "#BFB74C",
    info = "#F58442",
    warning = "#FFE100",
    danger = "#D96C81",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Poppins")
  ),
  
  shinyjs::useShinyjs(),
    
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/gsap/3.12.2/gsap.min.js"),
    tags$style(HTML("
      * {
        margin: 0;
        padding: 0;
        box-sizing: border-box;
      }
      
      body {
        background: #f5f5f5;
      }
      
      /* Page d'accueil */
      #home_page {
        min-height: 100vh;
        background: linear-gradient(135deg, #CB3452 0%, #D96C81 50%, #A2BDF4 100%);
        display: flex;
        align-items: center;
        justify-content: center;
        margin: -20px;
        padding: 20px;
      }
      
      .home-content {
        text-align: center;
        color: white;
        animation: fadeInUp 1s ease-out;
      }
      
      .home-content h1 {
        font-size: 4rem;
        font-weight: 700;
        margin-bottom: 1rem;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
      }
      
      .home-content p {
        font-size: 1.5rem;
        margin-bottom: 3rem;
        opacity: 0.95;
      }
      
      #enter_app {
        background: white;
        color: #CB3452;
        border: none;
        padding: 1rem 3rem;
        font-size: 1.2rem;
        font-weight: 600;
        border-radius: 50px;
        box-shadow: 0 8px 16px rgba(0,0,0,0.2);
        transition: all 0.3s ease;
        cursor: pointer;
      }
      
      #enter_app:hover {
        background: #FFE100;
        color: #CB3452;
        transform: translateY(-3px);
        box-shadow: 0 12px 24px rgba(0,0,0,0.3);
      }
      
      @keyframes fadeInUp {
        from { opacity: 0; transform: translateY(30px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      /* Cacher l'app au démarrage */
      #main_app {
        display: none;
      }
      
      /* Header */
      .custom-header {
        background: linear-gradient(135deg, #CB3452 0%, #D96C81 50%, #F58442 100%);
        color: white;
        padding: 2rem 2rem 4rem 2rem;
        margin: -2rem -2rem 0rem -2rem;
        border-radius: 0 0 16px 16px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        position: relative;
        z-index: 100;
      }
      
      .header-content {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      
      /* Pill Navigation Container */
      .pill-nav-container {
        position: absolute;
        bottom: -25px;
        left: 50%;
        transform: translateX(-50%);
        z-index: 99;
      }
      
      .pill-nav {
        --nav-h: 70px;
        --pill-pad-x: 20px;
        --pill-gap: 4px;
        --base: linear-gradient(135deg, #CB3452 0%, #D96C81 100%);
        --pill-bg: linear-gradient(135deg, #D96C81 0%, #F58442 100%);
        --hover-bg: white;
        --pill-text: white;
        --hover-text: #CB3452;
        
        width: 950px;
        display: flex;
        align-items: center;
        opacity: 1;
        transform: translateY(0);
        transition: all 0.5s cubic-bezier(0.68, -0.55, 0.265, 1.55);
      }
      
      .pill-nav-items {
        max-width: auto;
        position: relative;
        display: flex;
        align-items: center;
        height: var(--nav-h);
        background: linear-gradient(135deg, #CB3452 0%, #D96C81 100%);
        border-radius: 9999px;
        box-shadow: 0 6px 20px rgba(203, 52, 82, 0.4);
      }
      
      .pill-list {
        list-style: none;
        display: flex;
        align-items: stretch;
        gap: var(--pill-gap);
        margin: 0;
        padding: 4px;
        height: auto; 
        flex-wrap: wrap;
        justify-content: center;
      }
      
      .pill-list > li {
        display: flex;
        height: 100%;
      }
      
      .pill {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        height: calc(var(--nav-h) - 40px);  /* ajuste pour compenser le padding */
        font-size: 15px;   
        padding: 0 var(--pill-pad-x);
        background: linear-gradient(135deg, #D96C81 0%, #F58442 100%);
        color: white;
        text-decoration: none;
        border-radius: 9999px;
        box-sizing: border-box;
        font-weight: 600;
        font-size: 14px;
        line-height: 1;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        white-space: nowrap;
        cursor: pointer;
        position: relative;
        overflow: hidden;
        transition: all 0.3s ease;
      }
      
      /* Circle hover effect */
      .pill .hover-circle {
        position: absolute;
        left: 50%;
        bottom: 0;
        border-radius: 50%;
        background: white;
        z-index: 1;
        display: block;
        pointer-events: none;
        will-change: transform;
        transform: translateX(-50%) scale(0);
        transform-origin: 50% 100%;
      }
      
      /* Label stack for text animation */
      .pill .label-stack {
        position: relative;
        display: inline-block;
        line-height: 1;
        z-index: 2;
      }
      
      .pill .pill-label {
        position: relative;
        z-index: 2;
        display: inline-block;
        line-height: 1;
        will-change: transform;
        transition: transform 0.4s cubic-bezier(0.68, -0.55, 0.265, 1.55);
      }
      
      .pill .pill-label-hover {
        position: absolute;
        left: 0;
        top: 0;
        color: #CB3452;
        z-index: 3;
        display: inline-block;
        will-change: transform, opacity;
        opacity: 0;
        transform: translateY(30px);
        transition: all 0.2s cubic-bezier(0.68, -0.55, 0.265, 1.55);
      }
      
      /* Active indicator */
      .pill.is-active::after {
        content: '';
        position: absolute;
        bottom: -8px;
        left: 50%;
        transform: translateX(-50%);
        width: 8px;
        height: 8px;
        background: white;
        border-radius: 50%;
        z-index: 4;
        box-shadow: 0 2px 8px rgba(255,255,255,0.6);
      }
      
      /* Contenu principal */
      .main-content {
        margin-top: 4rem;
        padding: 2rem;
        max-width: 1400px;
        margin-left: auto;
        margin-right: auto;
        min-height: 60vh;
        position: relative;
      }
      
      /* Page content - utiliser visibility au lieu de display pour Leaflet */
      .page-content {
        visibility: hidden;
        opacity: 0;
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        pointer-events: none;
        animation: fadeIn 0.5s;
      }
      
      .page-content.active {
        visibility: visible;
        opacity: 1;
        position: relative;
        pointer-events: auto;
      }
      
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      /* Footer */
      footer {
        margin-top: 3rem;
        padding: 2rem;
        text-align: center;
        background: linear-gradient(90deg, #A2BDF4 0%, #BFB74C 100%);
      }
    "))
  ),
  
  # PAGE D'ACCUEIL
  div(
    id = "home_page",
    div(
      class = "home-content",
      tags$h1("Les inégalités de genre dans le monde"),
      tags$p("Analyse et visualisation de données avancées"),
      tags$div(
        style = "display: flex; gap: 1rem; justify-content: center; align-items: center; margin-bottom: 2rem;",
        tags$div(style = "width: 40px; height: 4px; background: #FFE100; border-radius: 2px;"),
        tags$div(style = "width: 40px; height: 4px; background: #F58442; border-radius: 2px;"),
        tags$div(style = "width: 40px; height: 4px; background: #BFB74C; border-radius: 2px;"),
        tags$div(style = "width: 40px; height: 4px; background: #A2BDF4; border-radius: 2px;")
      ),
      actionButton(
        "enter_app",
        "Entrer dans l'application",
        icon = icon("arrow-right"),
        class = "btn-lg"
      )
    )
  ),
  
  # APPLICATION PRINCIPALE
  div(
    id = "main_app",
    
    # Header avec navbar pill
    tags$div(
      class = "custom-header",
      tags$div(
        class = "header-content",
        tags$div(
          tags$h1(style = "margin: 0; font-weight: 600;", "Mon Application"),
          tags$p(style = "margin: 0.5rem 0 0 0; opacity: 0.9;", 
                 "Analyse et visualisation des données")
        ),
        actionButton(
          "back_home",
          "Retour Accueil",
          icon = icon("home"),
          style = "background: rgba(255,255,255,0.2); border: 2px solid white; 
                   color: white; border-radius: 8px; padding: 0.5rem 1.5rem;
                   font-weight: 600; transition: all 0.3s;"
        )
      ),
      
      # Pill Navigation
      tags$div(
        class = "pill-nav-container",
        tags$nav(
          class = "pill-nav",
          tags$div(
            class = "pill-nav-items",
            tags$ul(
              class = "pill-list",
              role = "menubar",
              
              tags$li(
                role = "none",
                tags$a(
                  role = "menuitem",
                  href = "#",
                  class = "pill is-active pill-item",
                  `data-page` = "page1",
                  tags$span(class = "hover-circle"),
                  tags$span(
                    class = "label-stack",
                    tags$span(class = "pill-label", "Gendered indicator map"),
                    tags$span(class = "pill-label-hover", "Gendered indicator map")
                  )
                )
              ),
              
              tags$li(
                role = "none",
                tags$a(
                  role = "menuitem",
                  href = "#",
                  class = "pill pill-item",
                  `data-page` = "page2",
                  tags$span(class = "hover-circle"),
                  tags$span(
                    class = "label-stack",
                    tags$span(class = "pill-label", "Indicator threshold map"),
                    tags$span(class = "pill-label-hover", "Indicator threshold map")
                  )
                )
              ),
              
              tags$li(
                role = "none",
                tags$a(
                  role = "menuitem",
                  href = "#",
                  class = "pill pill-item",
                  `data-page` = "page3",
                  tags$span(class = "hover-circle"),
                  tags$span(
                    class = "label-stack",
                    tags$span(class = "pill-label", "Axis based comparison"),
                    tags$span(class = "pill-label-hover", "Axis based comparison")
                  )
                )
              ),
              
              tags$li(
                role = "none",
                tags$a(
                  role = "menuitem",
                  href = "#",
                  class = "pill pill-item",
                  `data-page` = "page4",
                  tags$span(class = "hover-circle"),
                  tags$span(
                    class = "label-stack",
                    tags$span(class = "pill-label", "Country Report"),
                    tags$span(class = "pill-label-hover", "Country Report")
                  )
                )
              ),
              
              tags$li(
                role = "none",
                tags$a(
                  role = "menuitem",
                  href = "#",
                  class = "pill pill-item",
                  `data-page` = "page5",
                  tags$span(class = "hover-circle"),
                  tags$span(
                    class = "label-stack",
                    tags$span(class = "pill-label", "Country Visualization Report"),
                    tags$span(class = "pill-label-hover", "Country Visualization Report")
                  )
                )
              ),
              
              tags$li(
                role = "none",
                tags$a(
                  role = "menuitem",
                  href = "#",
                  class = "pill pill-item",
                  `data-page` = "page6",
                  tags$span(class = "hover-circle"),
                  tags$span(
                    class = "label-stack",
                    tags$span(class = "pill-label", "PCA"),
                    tags$span(class = "pill-label-hover", "PCA")
                  )
                )
              ),
              
              tags$li(
                role = "none",
                tags$a(
                  role = "menuitem",
                  href = "#",
                  class = "pill pill-item",
                  `data-page` = "page7",
                  tags$span(class = "hover-circle"),
                  tags$span(
                    class = "label-stack",
                    tags$span(class = "pill-label", "Descriptive Stats"),
                    tags$span(class = "pill-label-hover", "Descriptive Stats")
                  )
                )
              ),
              
              tags$li(
                role = "none",
                tags$a(
                  role = "menuitem",
                  href = "#",
                  class = "pill pill-item",
                  `data-page` = "page8",
                  tags$span(class = "hover-circle"),
                  tags$span(
                    class = "label-stack",
                    tags$span(class = "pill-label", "Temporal indicator comparison"),
                    tags$span(class = "pill-label-hover", "Temporal indicator comparison")
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # Contenu principal
    tags$div(
      class = "main-content",
      
      tags$div(
        class = "page-content active",
        id = "content_page1",
        tags$div(style = "margin-top: 1rem;",
                 PageCarteUI("carte")
        )
      ),
      tags$div(
        class = "page-content",
        id = "content_page2",
        tags$div(style = "margin-top: 1rem;",
                 PageCarteThresholdUI("carte_threshold")
        )
      ),
      tags$div(
        class = "page-content", 
        id = "content_page3", 
        tags$div(style = "margin-top: 1rem;",
                 pageComparaisonUI("comp")
        )
      ),
      tags$div(
        class = "page-content", 
        id = "content_page4", 
        tags$div(style = "margin-top: 1rem;",
                 pageRecapPaysUI("recap")
        )
      ),
      tags$div(
        class = "page-content", 
        id = "content_page5", 
        tags$div(style = "margin-top: 1rem;",
                 pageVisualisationUI("visu")
        )
      ),
      tags$div(
        class = "page-content", 
        id = "content_page6", 
        tags$div(style = "margin-top: 1rem;",
                 pageACPUI("acp")
        )
      ),
      tags$div(
        class = "page-content", 
        id = "content_page7", 
        tags$div(style = "margin-top: 1rem;",
                 pageUI_stat_desc("statdesc")
        )
      ),
      tags$div(
        class = "page-content", 
        id = "content_page8", 
        tags$div(style = "margin-top: 1rem;",
                 pageTempComparaisonUI("temp_comp")
        )
      ),
    ),
    
    # Footer
    tags$footer(
      tags$p(
        style = "color: white; margin: 0; font-weight: 500;",
        "© 2025 Léna Causeur Louis Prusiewicz Matéo Rosa | Version 1.0"
      )
    )
  ),
  
  # JavaScript avec GSAP
  tags$script(HTML("
    // Attendre que GSAP soit chargé
    function initPillNav() {
      if (typeof gsap === 'undefined') {
        setTimeout(initPillNav, 100);
        return;
      }
      
      const pills = document.querySelectorAll('.pill-item');
      const circles = document.querySelectorAll('.hover-circle');
      const timelines = [];
      
      // Fonction pour calculer les dimensions du cercle
      function layoutCircles() {
        pills.forEach((pill, i) => {
          const circle = circles[i];
          if (!circle) return;
          
          const rect = pill.getBoundingClientRect();
          const w = rect.width;
          const h = rect.height;
          
          // Calcul du rayon pour créer l'effet de vague
          const R = ((w * w) / 4 + h * h) / (2 * h);
          const D = Math.ceil(2 * R) + 2;
          const delta = Math.ceil(R - Math.sqrt(Math.max(0, R * R - (w * w) / 4))) + 1;
          const originY = D - delta;
          
          circle.style.width = D + 'px';
          circle.style.height = D + 'px';
          circle.style.bottom = -delta + 'px';
          
          gsap.set(circle, {
            xPercent: -50,
            scale: 0,
            transformOrigin: '50% ' + originY + 'px'
          });
          
          const label = pill.querySelector('.pill-label');
          const hoverLabel = pill.querySelector('.pill-label-hover');
          
          if (label) gsap.set(label, { y: 0 });
          if (hoverLabel) gsap.set(hoverLabel, { y: h + 12, opacity: 0 });
          
          // Créer la timeline d'animation
          const tl = gsap.timeline({ paused: true });
          
          tl.to(circle, { 
            scale: 1.2, 
            xPercent: -50, 
            duration: 0.6, 
            ease: 'power3.out' 
          }, 0);
          
          if (label) {
            tl.to(label, { 
              y: -(h + 8), 
              duration: 0.6, 
              ease: 'power3.out' 
            }, 0);
          }
          
          if (hoverLabel) {
            tl.to(hoverLabel, { 
              y: 0, 
              opacity: 1, 
              duration: 0.6, 
              ease: 'power3.out' 
            }, 0);
          }
          
          timelines[i] = tl;
        });
      }
      
      layoutCircles();
      window.addEventListener('resize', layoutCircles);
      
      // Gestion des événements hover et click
      pills.forEach((pill, i) => {
        pill.addEventListener('mouseenter', () => {
          if (timelines[i]) {
            timelines[i].play();
          }
        });
        
        pill.addEventListener('mouseleave', () => {
          if (timelines[i]) {
            timelines[i].reverse();
          }
        });
        
        pill.addEventListener('click', (e) => {
          e.preventDefault();
          
          // Retirer la classe active de tous
          pills.forEach(p => p.classList.remove('is-active'));
          
          // Ajouter la classe active au pill cliqué
          pill.classList.add('is-active');
          
          // Changer le contenu
          const page = pill.getAttribute('data-page');
          const targetContent = document.getElementById('content_' + page);
          
          document.querySelectorAll('.page-content').forEach(content => {
            content.classList.remove('active');
          });
          
          targetContent.classList.add('active');
          
          // Forcer le redimensionnement des cartes Leaflet - PLUSIEURS tentatives
          setTimeout(function() {
            // Event resize général
            window.dispatchEvent(new Event('resize'));
            
            // Chercher toutes les cartes Leaflet dans la page active
            const maps = targetContent.querySelectorAll('.leaflet-container');
            
            maps.forEach(function(container) {
              // Méthode 1: Via l'API Leaflet directe
              if (container._leaflet_id && L.Map && L.map) {
                const map = container._leaflet_map || L.map(container);
                if (map && map.invalidateSize) {
                  map.invalidateSize(true);
                }
              }
              
              // Méthode 2: Via HTMLWidgets
              if (container.id && window.HTMLWidgets) {
                const widget = window.HTMLWidgets.find('#' + container.id);
                if (widget && widget.getMap) {
                  const map = widget.getMap();
                  if (map && map.invalidateSize) {
                    map.invalidateSize(true);
                  }
                }
              }
            });
          }, 50);
          
          // Deuxième tentative après animation
          setTimeout(function() {
            window.dispatchEvent(new Event('resize'));
            
            const maps = targetContent.querySelectorAll('.leaflet-container');
            maps.forEach(function(container) {
              if (container._leaflet_id) {
                const map = container._leaflet_map;
                if (map && map.invalidateSize) {
                  map.invalidateSize(true);
                }
              }
            });
          }, 300);
          
          // Envoyer à Shiny
          if (typeof Shiny !== 'undefined') {
            Shiny.setInputValue('nav_page', page, {priority: 'event'});
          }
        });
      });
    }
    
    // Message personnalisé de Shiny pour forcer le resize
    if (typeof Shiny !== 'undefined') {
      Shiny.addCustomMessageHandler('triggerResize', function(message) {
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
          
          if (typeof L !== 'undefined' && L.Map) {
            document.querySelectorAll('.leaflet-container').forEach(function(mapElement) {
              const mapId = mapElement.id;
              if (mapId && window.LeafletWidget && window.LeafletWidget.methods) {
                const map = window.LeafletWidget.methods.getMap(mapId);
                if (map) {
                  map.invalidateSize();
                }
              }
            });
          }
        }, 100);
      });
    }
    
    // Initialiser
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', initPillNav);
    } else {
      initPillNav();
    }
  "))
)