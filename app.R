
library(shiny)
library(bslib)
library(tidyverse)

load("github_inno.RData")

#preparing pushes dataset
pushes <- pushes |>
  select(-c(year, quarter))

#adding developers and ratio
pushes <- pushes |>
  inner_join(
    developers |> select(date, iso2_code, developers), 
    by = c("date", "iso2_code")) |>
  mutate(dev_ratio = git_pushes/developers)

#adding repositories and ratio
pushes <- pushes |>
  inner_join(
    repositories |> select(date, iso2_code, repositories),
    by = c("date", "iso2_code")) |>
  mutate(repo_ratio = git_pushes/repositories)

#R pushers
r_pushers <- languages |>
  filter(language == "R") |>
  select(name, date, num_pushers)

#Julia pushers
julia_pushers <- languages |>
  filter(language == "Julia") |>
  select(name, date, num_pushers)

#last row of each country for value boxes
last_row <- pushes |>
  group_by(name) |>
  arrange(date) |>
  slice(n()) |>
  ungroup() |>
  mutate(across(c(git_pushes, developers, repositories), ~ scales::label_number(big.mark = ",")(.)))

countries <- sort(unique(pushes$name))

country_select <- selectInput("country", 
                           "Select the country:",
                           countries)

card_devs <- card(card_header("Pushes by developer:"),
                  plotOutput("plot_devs"))

card_repo <- card(card_header("Pushes by repository:"),
                  plotOutput("plot_repo"))

card_r <- card(card_header("Pushers using R:"),
               plotOutput("plot_r"))

card_julia <- card(card_header("Pushers using Julia:"),
                   plotOutput("plot_julia"))

vb_pushers <- value_box(title = "Pushes",
                       value = textOutput("pushers"),
                       showcase = bsicons::bs_icon("people-fill"),
                       theme = "teal")

vb_devs <- value_box(title = "Developers",
                        value = textOutput("devs"),
                        showcase = bsicons::bs_icon("people-fill"),
                        theme = "teal")

vb_repos <- value_box(title = "Repositories",
                        value = textOutput("repos"),
                        showcase = bsicons::bs_icon("github"),
                        theme = "info")


ui <- page_sidebar(title = "GitHub Innovation",
                   sidebar = sidebar(country_select),
                   layout_columns(vb_pushers, vb_devs, vb_repos),
                   layout_columns(card_devs, card_repo),
                   layout_columns(card_r, card_julia))


server <- function(input, output){
  
  country_row <- reactive({last_row |>
      filter(name == input$country)})

  output$plot_devs <- renderPlot({pushes |>
      filter(name == input$country) |>
      ggplot(aes(date, dev_ratio)) +
      geom_line() +
      theme_minimal()})
  
  output$plot_repo <- renderPlot({pushes |>
      filter(name == input$country) |>
      ggplot(aes(date, repo_ratio)) +
      geom_line() +
      theme_minimal()})
  
  output$plot_r <- renderPlot({r_pushers |>
      filter(name == input$country) |>
      ggplot(aes(date, num_pushers)) +
      geom_line() +
      theme_minimal()
  })
  
  output$plot_julia <- renderPlot({julia_pushers |>
      filter(name == input$country) |>
      ggplot(aes(date, num_pushers)) +
      geom_line() +
      theme_minimal()})
  
  output$pushers <- renderText({
    country_row() |>
      pull(git_pushes)
  })
  
  output$devs <- renderText({
    country_row() |>
      pull(developers)
  })
  
  output$repos <- renderText({
    country_row() |>
      pull(repositories)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


