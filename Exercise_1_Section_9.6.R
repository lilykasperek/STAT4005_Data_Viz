library(tidyverse)
atp_df <- read_csv("~/Desktop/data/atp_matches_2019.csv")
wta_df <- read_csv("data/wta_matches_2019.csv")

both_df <- bind_rows(atp_df, wta_df)

both_long <- both_df %>% pivot_longer(c(winner_name, loser_name))


both_n50 <- both_long %>% group_by(value) %>% count() %>%
  filter(n > 50)

major_tennis <- semi_join(both_long, both_n50, by = c("value"))
major_tennis <- major_tennis %>% mutate(w_svperc = 100 * w_1stIn / w_svpt,
                                        l_svperc = 100 * l_1stIn / l_svpt,
                                        w_firstwon = 100 * w_1stWon / w_1stIn,
                                        l_firstwon = 100 * l_1stWon / l_1stIn,
                                        w_secondwon = 100 * w_2ndWon / (w_svpt - w_1stIn),
                                        l_secondwon = 100 * l_2ndWon / (l_svpt - l_1stIn))

major_tennis_w <- major_tennis %>% filter(name == "winner_name")
major_tennis_l <- major_tennis %>% filter(name == "loser_name")

w_small <- major_tennis_w %>% select(value, winner_seed, w_ace, w_df, w_svperc,
                                     w_firstwon, w_secondwon) %>%
  rename(seed = winner_seed, ace = w_ace, df = w_df, svperc = w_svperc,
         firstwon = w_firstwon, secondwon = w_secondwon)

l_small <- major_tennis_l %>% select(value, loser_seed, l_ace, l_df, l_svperc, l_firstwon, l_secondwon)  %>%
  rename(seed = loser_seed, ace = l_ace, df = l_df, svperc = l_svperc,
         firstwon = l_firstwon, secondwon = l_secondwon)

df <- bind_rows(w_small, l_small) %>%
  rename(player = "value")
df

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(selectizeInput(inputId = "playerselect",
                                label = "Choose a Player", 
                                choices = levels(factor(df$player))),
                 radioButtons(inputId = "varselect",
                              label = "Choose a Variable",
                              choices = (names(df)[c(3, 4, 5, 6, 7)]))),
    numericInput("binnumber", label = "Choose a Number of Bins", 
                 min = 1, max = 50, value = 15, step = 1)),
  mainPanel(plotOutput("histplot"))
)

server <- function(input, output, session) {
  
  dm_df <- reactive({df %>% filter(player == input$playerselect)
  })
  
  output$histplot <- renderPlot({
    ggplot(data = dm_df(), aes(x = .data[[input$varselect]])) +
      geom_histogram(colour = "black", fill = "white", bins = input$binnumber)
  })
}

shinyApp(ui, server)
