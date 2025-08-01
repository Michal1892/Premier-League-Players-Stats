library(shiny)
library(DT)
library(shinyWidgets)

#######################################
# CREATING DICTIONARY WITH STATISTICS #
#######################################

dictionary <- list(
  GENERAL = c(
    "player_id", "full_name", "price", "club", "position",
    "rival", "power", "position_in_the_game", "minutes_played","defensive_actions",
    "yellow_cards", "red_cards", "fouls_committed", "fouls_drawn",
    "offsides", "penalties_won", "penalties_taken", "penalties_scored"
  ),
  
  SHOTS = c(
    "goals_scored", "shots", "shots_on_target", "xg", "npxg",
    "xa", "xg_xa", "shot_created_actions", "goal_created_actions"
  ),
  
  PASSES = c(
    "passes_completed", "passes_attempted", "total_passing_distance",
    "progressive_passes", "progressive_passing_distance",
    "short_passes_completed", "short_passes_attempted",
    "mid_passes_completed", "mid_passes_attempted",
    "long_passes_completed", "long_passes_attempted",
    "key_passes", "passes_into_final_third", "passes_into_penalty_area",
    "crosses_into_penalty_area", "live_ball_passes", "dead_ball_passes",
    "passes_from_free_kicks", "through_balls", "switches",
    "crosses", "throw_ins", "corner_kicks",
    "inswingins_corners", "outswinging_corners", "straight_corners"
  ),
  
  
  TACKLES = c(
    "tackles", "tackles_won", "tackles_def_3rd",
    "tackles_mid_3rd", "tackles_att_3rd", "dribbles_tackled",
    "dribbles_challenged", "challenges_lost"
  ),
  
  BLOCKS = c(
    "blocks", "shots_blocked", "passes_blocked"
  ),
  
  INTERCEPTIONS = as.list(
    "interceptions"
  ),
  
  CLEARANCES = c(
    "clearances", "ball_recoveries", "aerials_won", "aerials_lost"
  ),
  

  TOUCHES = c(
    "touches", "touches_def_pen", "touches_def_3rd", "touches_mid_3rd",
    "touches_att_3rd", "touches_att_pen", "live_ball_touches"
  ),
  
  CARRIES = c(
    "carries", "carries_tot_distance", "progressive_carries",
    "progressive_carrying_distance", "carries_into_final_3rd",
    "carries_into_pen_area", "take_ons_attempted", "successful_take_ons",
    "passes_received", "progressive_passes_received"
  ),
  
  ERRORS = c(
    "errors", "miscontrols", "disspossesed"
  )
)

###########################################
# CREATING DICTIONARY WITH AVG_STATISTICS #
###########################################

avg_dictionary <- list(
  GENERAL = c(
    "player_id", "full_name", "price", "club", "position","last_season_in_premier_league",
    "matches_played","avg_minutes_played", "avg_defensive_actions",
    "avg_yellow_cards", "avg_red_cards", "avg_fouls_committed", "avg_fouls_drawn",
    "avg_offsides", "avg_penalties_won", "avg_penalties_taken", "avg_penalties_scored"
  ),
  
  SHOTS = c(
    "sum_goals_scored", "avg_shots", "avg_shots_on_target", "avg_xg", "avg_npxg",
    "avg_xg","avg_xg_xa", "avg_shot_created_actions", "avg_goal_created_actions"
  ),
  
  PASSES = c(
    "sum_assists","avg_passes_completed", "avg_passes_attempted", "avg_total_passing_distance",
    "avg_progressive_passes", "avg_progressive_passing_distance",
    "avg_short_passes_completed", "avg_short_passes_attempted",
    "avg_mid_passes_completed", "avg_mid_passes_attempted",
    "avg_long_passes_completed", "avg_long_passes_attempted",
    "avg_key_passes", "avg_passes_into_final_third", "avg_passes_into_penalty_area",
    "avg_crosses_into_penalty_area", "avg_live_ball_passes", "avg_dead_ball_passes",
    "avg_passes_from_free_kicks", "avg_through_balls", "avg_switches",
    "avg_crosses", "avg_throw_ins", "avg_corner_kicks",
    "avg_inswingins_corners", "avg_outswinging_corners", "avg_straight_corners"
  ),
  
  TOUCHES = c(
    "avg_touches", "avg_touches_def_pen", "avg_touches_def_3rd", "avg_touches_mid_3rd",
    "avg_touches_att_3rd", "avg_touches_att_pen", "avg_live_ball_touches"
  ),
  
  
  TACKLES = c(
    "avg_tackles", "avg_tackles_won", "avg_tackles_def_3rd",
    "avg_tackles_mid_3rd", "avg_tackles_att_3rd", "avg_dribbles_tackled",
    "avg_dribbles_challenged", "avg_challenges_lost"
  ),
  
  BLOCKS = c(
    "avg_blocks", "avg_shots_blocked", "avg_passes_blocked"
  ),
  
  INTERCEPTIONS = as.list(
    "avg_interceptions"
  ),
  
  CLEARANCES = c(
    "avg_clearances", "avg_ball_recoveries", "avg_aerials_won", "avg_aerials_lost"
  ),
  
  CARRIES = c(
    "avg_carries", "avg_carries_tot_distance", "avg_progressive_carries",
    "avg_progressive_carrying_distance", "avg_carries_into_final_3rd",
    "avg_carries_into_pen_area", "avg_take_ons_attempted", "avg_successful_take_ons",
    "avg_passes_received", "avg_progressive_passes_received"
  ),
  
  
  ERRORS = c(
    "avg_errors", "avg_miscontrols", "avg_disspossesed"
  )
)



dictionary_positions <- list(
  GOALKEEPERS = dbGetQuery(con, "SELECT DISTINCT full_name FROM all_info WHERE position = 'Goalkeeper' ")$full_name,
  DEFENDERS   = dbGetQuery(con, "SELECT DISTINCT full_name FROM all_info WHERE position = 'Defender' ")$full_name,
  MIDFIELDERS = dbGetQuery(con, "SELECT DISTINCT full_name FROM all_info WHERE position = 'Midfielder' ")$full_name,
  FORWARDS    = dbGetQuery(con, "SELECT DIsTINCT full_name FROM all_info WHERE position = 'Forward' ")$full_name
)


avg_dictionary_positions <- list(
  GOALKEEPERS = dbGetQuery(con, "SELECT DISTINCT full_name FROM average WHERE position = 'Goalkeeper' ")$full_name,
  DEFENDERS   = dbGetQuery(con, "SELECT DISTINCT full_name FROM average WHERE position = 'Defender' ")$full_name,
  MIDFIELDERS = dbGetQuery(con, "SELECT DISTINCT full_name FROM average WHERE position = 'Midfielder' ")$full_name,
  FORWARDS    = dbGetQuery(con, "SELECT DIsTINCT full_name FROM average WHERE position = 'Forward' ")$full_name
)


ui <- fluidPage(
  tabsetPanel(
    ##########################################
    # CREATING PANEL WITH AVERAGE STATISTICS #
    ##########################################
    tabPanel('Average statistics',
             titlePanel("Average statistics"),
             sidebarLayout(
               sidebarPanel(
                 selectInput('avg_club', 'Select club:',choices = NULL,selected = NULL),
                 selectizeInput('avg_position', 'Select position:', choices=NULL, selected=NULL),
                 pickerInput('avg_player', 'Select player:',choices = avg_dictionary_positions,selected = NULL, multiple=TRUE,options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                 pickerInput('avg_stats_def', 'Select stats:',choices = avg_dictionary, multiple = TRUE,options = list(`live-search` = TRUE)),
                 
                 div(id = "avg_dynamic_sliders_container") #For dynamic sliders created for selected stats
               ),
               mainPanel(
                 DTOutput("tabela_avg")
               )
             )
             
             
             ),
    tabPanel('Game by game',
             ##################################
             # CREATING PANEL WITH STATISTICS #
             ##################################
             titlePanel("Game by game"),
             sidebarLayout(
               sidebarPanel(
                 selectInput('def_club', 'Select club:',choices = NULL,selected = NULL),
                 selectizeInput('position', 'Select position:', choices=NULL, selected=NULL),
                 pickerInput('def_player', 'Select player:',choices = dictionary_positions,selected = NULL, multiple=TRUE,options = list( `actions-box` = TRUE, `live-search` = TRUE)),
                 pickerInput('def_stats_def', 'Select stats:',choices = dictionary, multiple = TRUE,options = list(`live-search` = TRUE)),
                 
                 div(id = "dynamic_sliders_container") #For dynamic sliders created for selected stats
               ),
               mainPanel(
                 DTOutput("tabela")
               )
             )
             
    )
  )
)