## Add Fantasy Points and Expected Fantasy Points

  ## 1. Data Import ----
  
  nfl_keep()
  
    ### PBP Data
  
  pbp <- fread(paste0(inter_data_filepath, "exp_pbp2022.csv"), na.strings = "")
  
  ## 2. Data Construct ----
  
## Position Points -- Standard
  
## 1. Passing                     #  ## 2. Rushing
# 
# Passing Yard             -- 0.04  #  # Running Yard             -- 0.1
# 
# Touchdown Pass           -- 4     #  # Touchdown Rush           -- 6
# 
# Interception             -- -2    #  # 2pt Rush Conversion      -- 2
#
# 2pt Pass Conversion      -- 2     #
#
###########################################################################
#
## 3. Receiving                   # 
#
# Receiving Yard           -- 0.1   # 
#
# Touchdown Reception      -- 6     #
#
# 2pt Receiving Conversion -- 2     # 
#
# (Half PPR) Reception     -- 0/.5/1#
 
  fp <- pbp %>%
      
      mutate(
          
          exp_td = case_when(
              
              exp_rt == 1 | exp_pt == 1 ~ 1,
              
              TRUE                      ~ 0
              
          )
          
      ) %>%
      
      group_by(game_id, drive, name, exp_td) %>%
      
      mutate(
          
          n = case_when(
              
              exp_td == 1 & !is.na(name) ~ n(),
              
              TRUE        ~ NA_integer_
              
          ),
          
          i = case_when(
              
              exp_td == 1 & !is.na(name) ~ row_number(),
              
              TRUE        ~ NA_integer_
              
          )
          
      ) %>%
      
      ungroup() %>%
      
      mutate(
          
          exp_td = case_when(
              
              n == i ~ exp_td, # Remove extra touchdowns!!!
              
              TRUE   ~ 0
              
          ),
          
          across(
              
              c(exp_pt, exp_rt),
              
              ~ case_when(
                  
                  !is.na(.x) & exp_td == 0 ~ 0,
                  
                  TRUE                     ~ as.numeric(.x)
                  
              )
              
          )
          
      ) %>%
      
      select(
          
          -c(n, i)
          
      ) %>%
      
      mutate(
          
          across(
              
              c(exp_py, exp_pt),
              
              ~ case_when(
                  
                  is.na(passer_id) | (!is.na(passer_id) & is.na(receiver_id)) ~ NA_real_, # Make sure we don't get scramblers
                  
                  TRUE                                                        ~ as.numeric(.x)
                  
              )
              
          ),
          
          fp_passer =
              
              case_when(
                  
                  complete_pass == 1 ~ 0.04 * yards_gained,
                  
                  TRUE               ~ 0
                  
              )
          
          + 4    * pass_touchdown
          
          + (-2) * interception
          
          + (-2) * case_when(
              
              fumble_lost == 1 & fumbled_1_player_id == passer_id & is.na(fumbled_2_player_id) ~ 1, # Otherwise it shouldn't be attributed to QB
              
              TRUE                                                                             ~ 0
              
          )
          
          + 2    * case_when(
              
              two_point_success == 1 & !is.na(passer) ~ 1,
              
              TRUE                                    ~ 0
              
          ),
          
          fp_receiver =
              
              case_when(
                  
                  complete_pass == 1 ~ ppr + 0.1 * yards_gained,
                  
                  TRUE               ~ 0
                  
              )
          
          + 6    * pass_touchdown
          
          + (-2) * case_when(
              
              fumble_lost == 1 & fumbled_1_player_id == receiver_id & is.na(fumbled_2_player_id)  ~ 1,
              
              fumble_lost == 1 & fumbled_2_player_id == receiver_id & !is.na(fumbled_2_player_id) ~ 1, # To ensure we don't get NA equals
              
              TRUE                                                                                ~ 0
              
          )
          
          + 2    * case_when(
              
              two_point_success == 1 & !is.na(passer) ~ 1,
              
              TRUE                                    ~ 0
              
          ),
          
          fp_rusher =
              
              case_when(
                  
                  rush_attempt == 1 | (pass_attempt == 1 & is.na(receiver) & sack == 0) ~ 0.1 * yards_gained, # nflfastR doesn't quantify scrambles as rush attempts so use pass + NA receiver method; no fantasy points for yards on sacks
                  
                  TRUE                                                                    ~ 0
                  
              )
          
          + 6    * rush_touchdown
          
          + (-2) * case_when(
              
              fumble_lost == 1 & fumbled_1_player_id == rusher_id & is.na(fumbled_2_player_id)  ~ 1,
              
              fumble_lost == 1 & fumbled_2_player_id == rusher_id & !is.na(fumbled_2_player_id) ~ 1,
              
              TRUE                                                                              ~ 0
              
          )
          
          + 2    * case_when(
              
              two_point_success == 1 & !is.na(rusher) ~ 1,
              
              TRUE                                    ~ 0
              
          ),
          
          exp_fp_passer   = case_when(
              
              !is.na(exp_py) ~ 0.04 * exp_py + 4 * exp_pt,
              
              TRUE ~ 0
              
          ),
          
          exp_fp_receiver = case_when(
              
              !is.na(exp_py) ~ 0.1 * exp_py + ppr * cp + 6 * exp_pt, # cp is expected completion probability
              
              TRUE ~ 0
              
          ),
          
          exp_fp_rusher   = case_when(
              
              !is.na(exp_ry) ~ 0.1  * exp_ry + 6 * exp_rt,
              
              TRUE ~ 0
              
          )
          
      )
  
  fantasy_points <- fp %>%
      
      pivot_longer(
          
          cols          = c(fp_passer, fp_receiver, fp_rusher, exp_fp_passer, exp_fp_receiver, exp_fp_rusher),
          
          names_to      = c("type", "role"),
          
          names_pattern = "^(fp|exp_fp)_(.*)$",
          
          values_to     = "fp_points"
          
      ) %>%
      
      pivot_wider(
          
          names_from  = type,
          
          values_from = fp_points
          
      ) %>%
      
      rename(
          
          fp_points     = fp,
          
          exp_fp_points = exp_fp
          
      ) %>%
      
      mutate(
          
          name = case_when(
              
              role == "passer"   ~ passer_full_name,
              
              role == "receiver" ~ receiver_full_name,
              
              role == "rusher"   ~ rusher_full_name
              
          ),
          
          name_id   = case_when(
              
              role == "passer"   ~ passer_id,
              
              role == "receiver" ~ receiver_id,
              
              role == "rusher"   ~ rusher_id
              
          ),
          
          position  = case_when(
              
              role == "passer"   ~ passer_position,
              
              role == "receiver" ~ receiver_position,
              
              role == "rusher"   ~ rusher_position
              
          ),
          
          game_played = 0,
          
          across(
              
              c(target_number, total_targets, cum_py, total_py, cum_ay, total_ay),
              
              ~ case_when(
                  
                  role == "receiver" ~ as.numeric(.x),
                  
                  TRUE               ~ NA_real_
                  
              )
              
          ),
          
          across(
              
              c(run_number, total_runs, cum_ry, total_ry),
              
              ~ case_when(
                  
                  role == "rusher" ~ as.numeric(.x),
                  
                  TRUE             ~ NA_real_
                  
              )
              
          )
          
      ) %>%
      
      group_by(game_id, name) %>%
      
      mutate(
          
          game_played = case_when(
              
              row_number() == 1 ~ 1,
              
              TRUE              ~ 0
              
          )
          
      ) %>%
      
      ungroup() %>%
      
      group_by(game_id, posteam) %>%
      
      mutate(
          
          total_targets = last(na.omit(total_targets)), # Gets the whole amount for the game
          
          total_runs    = last(na.omit(total_runs)),
          
          total_py      = last(na.omit(total_py)),
          
          total_ry      = last(na.omit(total_ry)),
          
          total_ay      = last(na.omit(total_ay))
          
      ) %>%
      
      ungroup() %>%
      
      group_by(game_id, name, name_id, role) %>% # Need to have type so that it doesn't count n() wrong
      
      mutate(
          
          across(
              
              c(total_targets, total_py, total_ay),
              
              ~ case_when(
                  
                  row_number() == n() & role == "receiver" ~ as.numeric(.x),
                  
                  TRUE                                     ~ NA_real_
                  
              )
              
          ),
          
          across(
              
              c(total_runs, total_ry),
              
              ~ case_when(
                  
                  row_number() == n() & role == "rusher" ~ as.numeric(.x),
                  
                  TRUE                                   ~ NA_real_
              )
              
          ),
          
          target_share  = (target_number / total_targets) * 100,
          
          run_share     = (run_number / total_runs) * 100,
          
          py_share      = (cum_py / total_py) * 100,
          
          ry_share      = (cum_ry / total_ry) * 100,
          
          ay_share      = (cum_ay / total_ay) * 100,
          
          across(
              
              c(py_share, ry_share, ay_share),
              
              ~ case_when(
                  
                  .x < 0   ~ 0,
                  
                  .x > 100 ~ 100,
                  
                  TRUE     ~ .x
                  
              )
              
          )
          
      ) %>%
      
      ungroup() %>%
      
      group_by(game_id, name, name_id) %>%
      
      mutate(
          
          target_share = case_when(
              
              row_number() == n() & length(last(na.omit(total_targets))) == 0 ~ 0,
              
              TRUE                                                            ~ target_share
              
          ),
          
          run_share = case_when(
              
              row_number() == n() & length(last(na.omit(total_runs))) == 0 ~ 0,
              
              TRUE                                                         ~ run_share
              
          ),
          
          py_share = case_when(
              
              row_number() == n() & length(last(na.omit(total_py))) == 0 ~ 0,
              
              TRUE                                                       ~ py_share
              
          ),
          
          ry_share = case_when(
              
              row_number() == n() & length(last(na.omit(total_ry))) == 0 ~ 0,
              
              TRUE                                                       ~ ry_share
              
          ),
          
          ay_share = case_when(
              
              row_number() == n() & length(last(na.omit(total_ay))) == 0 ~ 0,
              
              TRUE                                                     ~ ay_share
              
          ),
          
          wopr = case_when(
              
              !is.na(ay_share) ~ (1.5 * target_share + 0.7 * ay_share) / 100,
              
              TRUE             ~ NA_real_
              
          )
          
      ) %>%
      
      ungroup()
  
  overall_table <- fantasy_points
  
  for(i in 1:weeks) {
      
      overall_table <- overall_table %>%
          
          mutate(
              
              !!paste0("exp_fp_week", i) := case_when(
                  
                  week == i ~ exp_fp_points,
                  
                  TRUE      ~ 0
                  
              ),
              
              !!paste0("fp_week", i) := case_when(
                  
                  week == i ~ fp_points,
                  
                  TRUE      ~ 0
                  
              )
              
          )
      
  }
  
  overall_table <- overall_table %>%
      
      group_by(name, name_id) %>%
      
      summarize(
          
          games    = sum(game_played, na.rm = TRUE),
          
          posteam  = last(posteam),
          
          position = last(position),
          
          exp_fantasy_points = round(sum(exp_fp_points, na.rm = TRUE), 2),
          
          fantasy_points     = round(sum(fp_points,     na.rm = TRUE), 2),
          
          across(
              
              starts_with("exp_fp_week"),
              
              ~ round(sum(.x, na.rm = TRUE), 2)
              
          ),
          
          across(
              
              starts_with("fp_week"),
              
              ~ round(sum(.x, na.rm = TRUE), 2)
              
          ),
          
          ave_target_share = round(mean(target_share, na.rm = TRUE), 2),
          
          ave_run_share    = round(mean(run_share,    na.rm = TRUE), 2),
          
          ave_py_share     = round(mean(py_share,     na.rm = TRUE), 2),
          
          ave_ry_share     = round(mean(ry_share,     na.rm = TRUE), 2),
          
          ave_ay_share     = round(mean(ay_share,     na.rm = TRUE), 2),
          
          wopr             = round(mean(wopr,         na.rm = TRUE), 2)
          
      ) %>%
      
      ungroup() %>%
      
      mutate(
          
          exp_fp_per_game       = round(exp_fantasy_points / games, 2),
          
          fp_per_game           = round(fantasy_points / games, 2),
          
          per_game_diff         = round(fp_per_game - exp_fp_per_game, 2)
          
      ) %>%
      
      filter(
          
          !is.na(name) & position %in% c("QB", "RB", "WR", "TE")
          
      ) %>%
      
      group_by(position) %>%  
      
      arrange(-exp_fantasy_points) %>%
      
      mutate(
          
          exp_fp_pos_rank = row_number(),
          
          exp_fp_rank = paste0(position, " ", exp_fp_pos_rank)
          
      ) %>%
      
      arrange(-fantasy_points) %>%
      
      mutate(
          
          fp_pos_rank = row_number(),
          
          fp_rank = paste0(position, " ", fp_pos_rank),
          
          rank_diff = exp_fp_pos_rank - fp_pos_rank
          
      ) %>%
      
      select(
          
          exp_fp_rank, fp_rank, name, posteam, everything(),
          
          -c(ave_target_share, ave_py_share, ave_ay_share, wopr, ave_run_share, ave_ry_share),
          
          c(ave_target_share, ave_py_share, ave_ay_share, wopr, ave_run_share, ave_ry_share),
          
          -matches("week[0-9]{1,2}"), matches("week[0-9]{1,2}"),
          
          -c(name_id, exp_fp_pos_rank, fp_pos_rank)
          
      ) %>%
      
      arrange(-exp_fantasy_points) %>%
      
      ungroup()
  
  for(i in 1:weeks) {
      
      overall_table <- overall_table %>%
          
          relocate(
              
              !!paste0("fp_week", i),
              
              .after = !!paste0("exp_fp_week", i)
              
          )
      
  }
  
  overall_table <- overall_table %>% # Add IDs
      
      left_join(
          
          fantasy_points %>% select(name, name_id) %>% distinct()
          
      ) %>%
      
      mutate( # Name Homogeneization
          
          name = str_replace_all(name, "((\\.)|(\\sJr)|(\\s(I{2,3}|IV|V)(?![A-Za-z])))", "")
          
      )
  
  if(weeks >= 3) {
      
      last_three <- fantasy_points
      
      for(i in 1:32) {
          
          temp_posteam <- fantasy_points %>%
              
              group_by(posteam) %>%
              
              count() %>%
              
              ungroup() %>%
              
              select(posteam) %>%
              
              dplyr::slice(n = i) %>%
              
              pluck(1)
          
          weeks_to_keep <- fantasy_points %>%
              
              filter(posteam == temp_posteam) %>%
              
              group_by(week) %>%
              
              count() %>%
              
              ungroup() %>%
              
              slice_tail(n = 3) %>%
              
              select(week) %>%
              
              pluck(1)
          
          last_three <- last_three %>%
              
              filter(!(posteam == temp_posteam & !(week %in% weeks_to_keep)))
          
      }
      
      last_three_table <- last_three %>%
          
          group_by(name, name_id) %>%
          
          summarize(
              
              games              = sum(game_played,   na.rm = TRUE),
              
              posteam            = last(posteam),
              
              position           = last(position),
              
              exp_fantasy_points = round(sum(exp_fp_points, na.rm = TRUE), 2),
              
              fantasy_points     = round(sum(fp_points,     na.rm = TRUE), 2),
              
              ave_target_share   = round(sum(target_share,  na.rm = TRUE) / 3, 2),
              
              ave_run_share      = round(sum(run_share,     na.rm = TRUE) / 3, 2),
              
              ave_py_share       = round(sum(py_share,      na.rm = TRUE) / 3, 2),
              
              ave_ry_share       = round(sum(ry_share,      na.rm = TRUE) / 3, 2),
              
              ave_ay_share       = round(sum(ay_share,      na.rm = TRUE) / 3, 2),
              
              wopr               = round(sum(wopr,          na.rm = TRUE) / 3, 2)
              
          ) %>%
          
          ungroup() %>%
          
          mutate(
              
              exp_fp_per_game       = round(exp_fantasy_points / games, 2),
              
              fp_per_game           = round(fantasy_points / games, 2),
              
              per_game_diff         = round(fp_per_game - exp_fp_per_game, 2)
              
          ) %>%
          
          filter(
              
              !is.na(name) & position %in% c("QB", "RB", "WR", "TE")
              
          ) %>%
          
          group_by(position) %>%  
          
          arrange(-exp_fantasy_points) %>%
          
          mutate(
              
              exp_fp_pos_rank = row_number(),
              
              exp_fp_rank = paste0(position, " ", exp_fp_pos_rank)
              
          ) %>%
          
          arrange(-fantasy_points) %>%
          
          mutate(
              
              fp_pos_rank = row_number(),
              
              fp_rank = paste0(position, " ", fp_pos_rank),
              
              rank_diff = exp_fp_pos_rank - fp_pos_rank
              
          ) %>%
          
          select(
              
              exp_fp_rank, fp_rank, name, posteam, everything(),
              
              -c(ave_target_share, ave_py_share, ave_ay_share, wopr, ave_run_share, ave_ry_share),
              
              c(ave_target_share, ave_py_share, ave_ay_share, wopr, ave_run_share, ave_ry_share),
              
              -c(name_id, exp_fp_pos_rank, fp_pos_rank)
              
          ) %>%
          
          arrange(-exp_fantasy_points) %>%
          
          ungroup()
      
      last_three_table <- last_three_table %>% # Add IDs
          
          left_join(
              
              fantasy_points %>% select(name, name_id) %>% distinct()
              
          )
  
  }
  
  ## 3. Dataset Export ----
  
  fwrite(
      
      fp, paste0(inter_data_filepath, "fp2022_", ppr, ".csv"),
      
      row.names = FALSE, na = ""
      
  )
  
  fwrite(
      
      fantasy_points,
      
      paste0(inter_data_filepath, "fantasy_pbp_", ppr, ".csv"),
      
      row.names = FALSE, na = ""
      
  )
  
  fwrite(
      
      overall_table,
      
      paste0(inter_data_filepath, "exp_fp_data_", ppr, ".csv"),
      
      row.names = FALSE, na = ""
      
  )
  
  if(weeks >= 3) {
      
      write.csv(
          
          last_three_table,
          
          paste0(inter_data_filepath, "exp_fp_data_last_three_", ppr, ".csv"),
          
          row.names = FALSE, na = ""
          
      )
      
  }
  