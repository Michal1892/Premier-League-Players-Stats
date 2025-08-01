library(shiny)
library(DT)
library(DBI)

shinyServer(
  function(input, output, session){
    
    observe({
      def_clubs <- dbGetQuery(con, "SELECT club FROM average WHERE last_season_in_premier_league = 'Yes' ORDER BY club")
      def_players <- dbGetQuery(con, 'SELECT full_name FROM average ORDER BY full_name')
      def_stats_selected = dbGetQuery(con, "SELECT * FROM average")
      position_query <- dbGetQuery(con, 'SELECT * FROM positions')
      players_selected = dbGetQuery(con, "SELECT DISTINCT position FROM all_info")
      
      updateSelectInput(session, 'avg_club', choices = c("All clubs",def_clubs$club), selected = "All clubs")
      updateSelectizeInput(session, 'avg_position', choices = c("All positions",position_query$position))
      updateSelectInput(session, 'avg_stats_def', choices = colnames(def_stats_selected))
      updateSelectInput(session, 'avg_player', choices = players_selected$position)
      
      ######################################################################################
      # We want the input to allow filtering players by chosen club and position.          #
      # For example:                                                                        #
      # - If we choose "Midfielders", only players who play in midfield should be shown.   #
      # - If we choose the club "Bournemouth", only Bournemouth players should appear.     #
      ######################################################################################
      observeEvent(list(input$avg_club,input$avg_position),{
        req(input$avg_club)
        position_n <- input$avg_position
        
        # We determine the available player options based on the selected club and position
        if(input$avg_position=='All positions' || input$avg_position == ''){
          position_n = sprintf("Goalkeeper','Defender','Midfielder','Forward")
        }  
        
        if (input$avg_club == "All clubs" & position_n == sprintf("Goalkeeper','Defender','Midfielder','Forward")) {
          query <- sprintf("SELECT DISTINCT full_name FROM average WHERE position IN ('%s')", position_n)
          avg_player <- dbGetQuery(con, query)
          
          club_dictionary = list(GOALKEEPERS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM average WHERE position IN ('Goalkeeper')") )$full_name),
                                 DEFENDERS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM average WHERE position IN ('Defender')") )$full_name),
                                 MIDFIELDERS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM average WHERE position IN ('Midfielder')") )$full_name),
                                 FORWARDS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM average WHERE position IN ('Forward')") )$full_name))
          updatePickerInput(session, 'avg_player', choices = club_dictionary)
          
          
        }
        else if (input$avg_club == "All clubs" & position_n != sprintf("Goalkeeper','Defender','Midfielder','Forward")) {
          query <- sprintf("SELECT DISTINCT full_name FROM average WHERE position IN ('%s')", position_n)
          avg_player <- dbGetQuery(con, query)
          
          updatePickerInput(session, 'avg_player', choices = avg_player$full_name)
          
        } else if(input$avg_club != "All clubs" & position_n == sprintf("Goalkeeper','Defender','Midfielder','Forward")) {
          query <- sprintf("SELECT DISTINCT full_name FROM average WHERE club = '%s' AND position IN ('%s')", input$avg_club, position_n)
          def_player <- dbGetQuery(con, query)
          
          club_dictionary = list(GOALKEEPERS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM average WHERE club = '%s' AND position IN ('Goalkeeper')",input$avg_club))$full_name),
                                 DEFENDERS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM average WHERE club = '%s' AND position IN ('Defender')",input$avg_club))$full_name),
                                 MIDFIELDERS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM average WHERE club = '%s' AND position IN ('Midfielder')",input$avg_club))$full_name),
                                 FORWARDS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM average WHERE club = '%s' AND position IN ('Forward')",input$avg_club))$full_name))
          
          
          
          updatePickerInput(session, 'avg_player', choices = club_dictionary)
          
          
        } else {
          query <- sprintf("SELECT DISTINCT full_name FROM average WHERE club = '%s' AND position IN ('%s')", input$avg_club, position_n)
          avg_player <- dbGetQuery(con, query)
          updatePickerInput(session, 'avg_player', choices = avg_player$full_name)
          
          
        }
        
      })
    })
    
    #################################################################
    # We want to create dynamic sliders for stats which are numeric #
    #################################################################
    
    df <- dbGetQuery(con, "SELECT * FROM average")
    `%notin%` <- Negate(`%in%`)
    created_sliders <- reactiveVal(character(0)) # stores stats for which sliders have already been created
    
    observeEvent(input$avg_stats_def, {
      current <- input$avg_stats_def # stats currently selected by the user
      existing <- created_sliders() # stats that already have corresponding sliders
      
      new_stats <- setdiff(current, existing) 
      
      #######################################################
      # we want now to insert dynamic sliders for new stats #
      #######################################################
      lapply(new_stats, function(x) { 
        if (x %notin% c('player_id', 'full_name', 'club', 'position', 'power','last_season_in_premier_league')){ # these stats are not numeric   
          vals <- dbGetQuery(con, sprintf("SELECT %s FROM average", x))[[1]]
          # there we insert dynamic sliders
          insertUI(
            selector = "#avg_dynamic_sliders_container",
            where = "beforeEnd",
            ui = div(
              id = paste0("avg_slider_wrapper_", x),
              sliderInput(
                inputId = paste0("avg_slider_", x),
                label = paste("Values for", x),
                min = min(vals, na.rm = TRUE), # min value for a selected stat
                max = max(vals, na.rm = TRUE), # max value for a selected stat
                value = c(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE)),
                step = 0.1
              )
            )
          )
        } else if (x %in% c('power', 'last_season_in_premier_league')) { # we want to create sliders for them but these stats are not numeric
          vals <- unique(dbGetQuery(con, sprintf("SELECT %s FROM average", x))[[1]]) 
          insertUI(
            selector = "#avg_dynamic_sliders_container",
            where = "beforeEnd",
            ui = div(
              id = paste0("avg_slider_wrapper_", x),
              selectInput(
                inputId = paste0("avg_slider_", x),
                label = paste("Values", x),
                choices = vals,
                multiple = TRUE
              )
            )
          )
        }
      })
    
      #############################################################
      # if we untick a stat, we want a dynamic slider to disappear #
      #############################################################
      removed <- setdiff(existing, current) 
      for (x in removed) {
        removeUI(selector = paste0("#avg_slider_wrapper_", x), immediate = TRUE) #removes a slider
      }

      created_sliders(setdiff(union(existing, new_stats), removed))
      
      
      
    })
    
    
    ################
    # table output #
    ################
    
    output$tabela_avg <- renderDT({
      
      
      ranges <- lapply(input$avg_stats_def, function(stat) if(stat %notin% c('player_id', 'full_name', 'club', 'position')) {
        input[[paste0("avg_slider_", stat)]]
      }) # we want to extract ranges from dynamic sliders
      
      
      conditions <- mapply(function(stat, range) {
        if (stat %notin% c('player_id', 'full_name', 'club', 'position', 'rival',  'power','last_season_in_premier_league')) {
          if(is.null(input[[paste0("avg_slider_", stat)]])){
            sprintf("NULL IS NULL")
          }
          else{
            sprintf("(%s BETWEEN %s AND %s)", stat, range[1], range[2])
          }
        }
        else if (stat %in% c('power','last_season_in_premier_league')){
          
          if(is.null(input[[paste0("avg_slider_", stat)]])){
            sprintf("NULL IS NULL")
          }
          else{
            sprintf("(%s IN ('%s'))", stat, paste(range, collapse="','") )
          }
        }
        else if (stat %in% c('player_id', 'full_name', 'club', 'position')){
          sprintf("NULL IS NULL")
        }
        else {
          NA_character_ 
        }
      }, input$avg_stats_def, ranges, SIMPLIFY = TRUE, USE.NAMES = FALSE) # preparing final query basing on selected stats
      
      
      position_name <- input$avg_position
      if(input$avg_position=='All positions' || input$avg_position == ''){
        position_name = sprintf("Goalkeeper','Defender','Midfielder','Forward")
      } # preparing final where query basing on selected position
      
      # now we want to clean from where query elements with character(0)
      conditions <- conditions[!is.na(conditions) & nzchar(conditions)]
      where_clause <- paste(conditions, collapse = " AND ")
      where_clause <- gsub("AND character\\(0\\)", "", where_clause)
      where_clause <- trimws(gsub("character\\(0\\)", "", where_clause))
      
      
      if(where_clause == ''){
        where_clause <- 'NULL IS NULL'
      }
      if(is.null(input$avg_stats_def)){
        variables <- '*'   #we want to select everything if we do not select any stat
      }
      else {
        variables <- paste(input$avg_stats_def, collapse = ", ")
      }
      
      ###########################################################
      # final query basing on selected position, club and stats #
      ###########################################################
      if(("All clubs" %in% input$avg_club || is.null(input$avg_club))  & is.null(input$avg_player)){
        query <- sprintf("SELECT %s FROM average WHERE position IN ('%s') AND %s ORDER BY full_name" , variables, position_name, where_clause)
      }
      else if("All clubs" %notin% input$avg_club & is.null(input$avg_club)==FALSE & is.null(input$avg_player)){
        query <- sprintf("SELECT %s FROM average WHERE position IN ('%s') AND club = '%s' AND %s ORDER BY full_name" , variables,position_name, input$avg_club, where_clause)
      }
      else if("All clubs" %in% input$avg_club & is.null(input$avg_player)==FALSE){
        query <- sprintf("SELECT %s FROM average WHERE position IN ('%s') AND full_name IN (%s) AND %s ORDER BY full_name" , variables, position_name, paste0("'", gsub("'", "''", input$avg_player), "'", collapse = ", "), where_clause)
      }
      else if("All clubs" %notin% input$avg_club & is.null(input$avg_player)==FALSE){
        query <- sprintf("SELECT %s FROM average WHERE position IN ('%s') AND full_name IN (%s) AND club = '%s' AND %s ORDER BY full_name" , variables,position_name,  paste0("'", gsub("'", "''", input$avg_player), "'", collapse = ", "), input$avg_club, where_clause)
      }
      
      data <- dbGetQuery(con, query) # extracting data from table basing on the sql query
      
      #############################################################################
      # we want to color the cellse basing on the min and max value in the column #
      #############################################################################
      
      columns_num <- names(data)[sapply(data, is.numeric)] # only columns that are numeric
      columns_num <- setdiff(columns_num, "player_id") # substract the column player_id if is selected
      agg_cols <- paste0("MIN(", columns_num, ") AS min_", columns_num, ",", "MAX(", columns_num, ") AS max_", columns_num) # preparing query to get min and max value in the column
      
      if(length(columns_num)==0){
        cols_agg = ''
      }
      else{
        cols_agg = paste(agg_cols, collapse = ", ")
      }
      
      colors_dataframe_query <- sprintf(
        "SELECT %s FROM average",
        cols_agg
      ) 
      
      colors_dataframe <- dbGetQuery(con, colors_dataframe_query) # data with min and max values for each selected stat
      
      ##############################################################################
      # now we prepare cuts and colors for each interval for a given selected stat #
      ##############################################################################
      color_styles <- lapply(columns_num, function(col) {
        min_col_name <- paste0("min_", col)
        max_col_name <- paste0("max_", col)
        
        
        if (min_col_name %in% names(colors_dataframe)) {
          values <- c(na.omit(colors_dataframe[[min_col_name]]),na.omit(colors_dataframe[[max_col_name]])) # vector with min and max value for a selected stat
          rng <- range(values)
          
          if (all(is.finite(rng)) && diff(rng) > 0) {
            
            n_intervals <- 5
            if (n_intervals < 1) return(NULL)
            
            breaks <- seq(rng[1], rng[2], length.out = n_intervals)
            cuts <- (breaks[-1] + breaks[-length(breaks)]) / 2
            
            if(col %in% c("price","avg_yellow_cards", "avg_red_cards", "avg_fouls_committed",
                          "avg_offsides","avg_errors", "avg_miscontrols", "avg_disspossesed" )){
              colors <- colorRampPalette(c("green", 'yellow', 'red'))(length(cuts)+1)
              
            }            
            else{
              colors <- colorRampPalette(c("red", 'yellow', 'green'))(length(cuts)+1)
            }
            return(list(col = col, cuts = cuts, colors = colors))
          }
        }
        NULL
      })
      color_styles <- Filter(Negate(is.null), color_styles)
      
      dt <- datatable(
        data,
        options = list(pageLength = 10),
        class = "compact"
      )
      
      
      for (style in color_styles) {
        col <- style$col
        interval_indices <- findInterval(data[[col]], style$cuts) + 1
        colors_vec <- style$colors[interval_indices]
        data[[col]] <- sprintf('<div style="background-color:%s;color:black;padding:4px;">%s</div>', colors_vec, data[[col]]) #a applying colors
      }
      
      dt <- datatable(data, escape = FALSE, options = list(pageLength = 15), class = "compact")
      dt
    })
    
    ##################################################################################
    # For the second table with detailed statistics game by game the code is similar #
    ##################################################################################
    
    observe({
      def_clubs <- dbGetQuery(con, "SELECT name FROM clubs WHERE country='England' ORDER BY name")
      def_players <- dbGetQuery(con, 'SELECT full_name FROM all_info ORDER BY full_name')
      position_query <- dbGetQuery(con, 'SELECT * FROM positions')
      updateSelectInput(session, 'def_club', choices = c("All clubs",def_clubs$name), selected = "All clubs")
      updateSelectizeInput(session, 'position', choices = c("All positions",position_query$position))
      def_stats_selected = dbGetQuery(con, "SELECT * FROM all_info")
      updateSelectInput(session, 'def_stats_def', choices = colnames(def_stats_selected))
      players_selected = dbGetQuery(con, "SELECT DISTINCT position FROM all_info")
      updateSelectInput(session, 'def_player', choices = players_selected$position)
      
      
      
      observeEvent(list(input$def_club,input$position),{
        req(input$def_club)
        position_n <- input$position
        if(input$position=='All positions' || input$position == ''){
          position_n = sprintf("Goalkeeper','Defender','Midfielder','Forward")
        }          
                   if (input$def_club == "All clubs" & position_n == sprintf("Goalkeeper','Defender','Midfielder','Forward")) {
                       query <- sprintf("SELECT DISTINCT full_name FROM all_info WHERE position IN ('%s')", position_n)
                       def_player <- dbGetQuery(con, query)
                       club_dictionary = list(GOALKEEPERS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM all_info WHERE position IN ('Goalkeeper')") )$full_name),
                                              DEFENDERS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM all_info WHERE position IN ('Defender')") )$full_name),
                                              MIDFIELDERS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM all_info WHERE position IN ('Midfielder')") )$full_name),
                                              FORWARDS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM all_info WHERE position IN ('Forward')") )$full_name))
                       updatePickerInput(session, 'def_player', choices = club_dictionary)
                       
                       
                   }
                       else if (input$def_club == "All clubs" & position_n != sprintf("Goalkeeper','Defender','Midfielder','Forward")) {
                         query <- sprintf("SELECT DISTINCT full_name FROM all_info WHERE position IN ('%s')", position_n)
                         def_player <- dbGetQuery(con, query)
                         
                         updatePickerInput(session, 'def_player', choices = def_player$full_name)
                       
                   } else if(input$def_club != "All clubs" & position_n == sprintf("Goalkeeper','Defender','Midfielder','Forward")) {
                       query <- sprintf("SELECT DISTINCT full_name FROM all_info WHERE club = '%s' AND position IN ('%s')", input$def_club, position_n)
                       def_player <- dbGetQuery(con, query)
                       club_dictionary = list(GOALKEEPERS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM all_info WHERE club = '%s' AND position IN ('Goalkeeper')",input$def_club))$full_name),
                                              DEFENDERS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM all_info WHERE club = '%s' AND position IN ('Defender')",input$def_club))$full_name),
                                              MIDFIELDERS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM all_info WHERE club = '%s' AND position IN ('Midfielder')",input$def_club))$full_name),
                                              FORWARDS = as.list(dbGetQuery(con,sprintf("SELECT DISTINCT full_name FROM all_info WHERE club = '%s' AND position IN ('Forward')",input$def_club))$full_name))
                      
                       
                       updatePickerInput(session, 'def_player', choices = club_dictionary)
                       
                       
                   } else {
                     query <- sprintf("SELECT DISTINCT full_name FROM all_info WHERE club = '%s' AND position IN ('%s')", input$def_club, position_n)
                     def_player <- dbGetQuery(con, query)
                     updatePickerInput(session, 'def_player', choices = def_player$full_name)
                     
                     
                   }
                  
      })
         
      })
    df <- dbGetQuery(con, "SELECT * FROM all_info")
   `%notin%` <- Negate(`%in%`)
   created_sliders <- reactiveVal(character(0))
   
   observeEvent(input$def_stats_def, {
     current <- input$def_stats_def
     existing <- created_sliders()
     
     
     new_stats <- setdiff(current, existing)
     
     lapply(new_stats, function(x) {
       if (x %notin% c('player_id', 'full_name', 'club', 'position', 'rival', 'position_in_the_game', 'power')) {
         vals <- dbGetQuery(con, sprintf("SELECT %s FROM all_info", x))[[1]]
         insertUI(
           selector = "#dynamic_sliders_container",
           where = "beforeEnd",
           ui = div(
             id = paste0("slider_wrapper_", x),
             sliderInput(
               inputId = paste0("slider_", x),
               label = paste("Values for", x),
               min = min(vals, na.rm = TRUE),
               max = max(vals, na.rm = TRUE),
               value = c(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE)),
               step = 0.1
             )
           )
         )
       } else if (x %in% c('rival', 'power')) {
         vals <- unique(dbGetQuery(con, sprintf("SELECT %s FROM all_info", x))[[1]])
         insertUI(
           selector = "#dynamic_sliders_container",
           where = "beforeEnd",
           ui = div(
             id = paste0("slider_wrapper_", x),
             selectInput(
               inputId = paste0("slider_", x),
               label = paste("Values for", x),
               choices = vals,
               multiple = TRUE
             )
           )
         )
       }
     })
     
     
     removed <- setdiff(existing, current)
     for (x in removed) {
       removeUI(selector = paste0("#slider_wrapper_", x), immediate = TRUE)
     }
    
     created_sliders(setdiff(union(existing, new_stats), removed))
     
   })
   
   output$selected_stats <- renderPrint({
     input$def_stats_def
   })
  
  
    output$tabela <- renderDT({
      ranges <- lapply(input$def_stats_def, function(stat) if(stat %notin% c('player_id', 'full_name', 'club', 'position', 'position_in_the_game')) {
        input[[paste0("slider_", stat)]]
      })
      
      conditions <- mapply(function(stat, range) {
        if (stat %notin% c('player_id', 'full_name', 'club', 'position', 'rival', 'position_in_the_game', 'power')) {
          if(is.null(input[[paste0("slider_", stat)]])){
            sprintf("NULL IS NULL")
          }
          else{
            sprintf("(%s BETWEEN %s AND %s)", stat, range[1], range[2])
          }
        }
        else if (stat %in% c('power', 'rival')){
    
          if(is.null(input[[paste0("slider_", stat)]])){
            sprintf("NULL IS NULL")
          }
          else{
            sprintf("(%s IN ('%s'))", stat, paste(range, collapse="','") )
          }
        }
        else if (stat %in% c('player_id', 'full_name', 'club', 'position',  'position_in_the_game')){
          sprintf("NULL IS NULL")
        }
        else {
          NA_character_  
        }
      }, input$def_stats_def, ranges, SIMPLIFY = TRUE, USE.NAMES = FALSE)
      
      
      position_name <- input$position
      if(input$position=='All positions' || input$position == ''){
        position_name = sprintf("Goalkeeper','Defender','Midfielder','Forward")
      }
      
      conditions <- conditions[!is.na(conditions) & nzchar(conditions)]
      where_clause <- paste(conditions, collapse = " AND ")
      where_clause <- gsub("AND character\\(0\\)", "", where_clause)
      where_clause <- trimws(gsub("character\\(0\\)", "", where_clause))
      
      where <- paste('WHERE', where_clause, sep=' ')
      where_and <- paste("AND", where_clause, sep = ' ')
      
      if(where_clause == ''){
        where_clause <- 'NULL IS NULL'
      }
      if(is.null(input$def_stats_def)){
        variables <- '*'
      }
      else {
        variables <- paste(input$def_stats_def, collapse = ", ")
      }
      
      if(("All clubs" %in% input$def_club || is.null(input$def_club))  & is.null(input$def_player)){
        query <- sprintf("SELECT %s FROM all_info WHERE position IN ('%s') AND %s ORDER BY full_name" , variables, position_name, where_clause)
      }
      else if("All clubs" %notin% input$def_club & is.null(input$def_club)==FALSE & is.null(input$def_player)){
        query <- sprintf("SELECT %s FROM all_info WHERE position IN ('%s') AND club = '%s' AND %s ORDER BY full_name" , variables,position_name, input$def_club, where_clause)
      }
      else if("All clubs" %in% input$def_club & is.null(input$def_player)==FALSE){
        query <- sprintf("SELECT %s FROM all_info WHERE position IN ('%s') AND full_name IN (%s) AND %s ORDER BY full_name" , variables, position_name, paste0("'", gsub("'", "''", input$def_player), "'", collapse = ", "), where_clause)
      }
      else if("All clubs" %notin% input$def_club & is.null(input$def_player)==FALSE){
        query <- sprintf("SELECT %s FROM all_info WHERE position IN ('%s') AND full_name IN (%s) AND club = '%s' AND %s ORDER BY full_name" , variables,position_name,  paste0("'", gsub("'", "''", input$def_player), "'", collapse = ", "), input$def_club, where_clause)
      }
      
      
      
      data <- dbGetQuery(con, query)
      
      dt <- datatable(data, escape = FALSE, options = list(pageLength = 15), class = "compact")
      dt
    })
  }
  
)