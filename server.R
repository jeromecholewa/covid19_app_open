#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Qq conseils

# ##############
shinyServer(function(input, output) {
  observe({

    ###########
    #
    # if (!is.na(as.numeric(input$dynamic_unuse)) &&
    #     as.numeric(input$dynamic_unuse) <5) {
    #   dyn_unuse <- as.numeric(input$dynamic_unuse)
    # } else dyn_unuse <- 0


    new_or_cum <- as.character(input$new_or_cumu)
    conf_rem_dec <-  as.numeric(c(input$confirmed,input$recovered,input$deaths))
    permillion <- as.logical(input$ppermillion)
    country_chosen <- as.character(input$country_chosen)
    output$countries_graph <- renderText({input$country_chosen})
    # output$combination <- renderText({as.numeric(conf_rem_dec)})
    # output$new_OR_cum <- renderText({new_or_cum})
    # output$texti <- renderText({country_chosen[1]})
    # output$covid_data <- renderTable({ covid_country })
    url <- a("John Hopkins CSSEGISandData COVID-19",
             href= url_address)
    output$tab <- output$tab2 <- output$tab3 <-
      renderUI({
        tagList("Source :", url)
      })

    okpermillion <- permillion & all(country_chosen %in% pays_jc)


    multiplier_zero_abs <-  rep(1, length.out =  length(country_chosen))
    multiplier_zero_million <- rep(1000000 /unname(getpop_jc[country_chosen]),
                                each = 1)

    if (!okpermillion )  {
      multiplier <- rep(1, length.out = 3* length(country_chosen))
      multiplier_zero <- multiplier_zero_abs
    } else {
      multiplier <- rep(1000000 /unname(getpop_jc[country_chosen]),
                        each = 3)
      multiplier_zero <- multiplier_zero_million
    }

    select_columns <-numeric(0)
    for (j in 1:length(country_chosen)) {
      select_columns <- c(select_columns,
                          which(gsub("_.+","",
                                     names(covid_country)) ==
                                  country_chosen[j]))
    }

    # names(covid_country)[select_columns]

    ylim_max <-0
    for (i in 0:(length(select_columns)-1)) {
      ylim_max <- max(ylim_max,
                      multiplier[i+1] * conf_rem_dec[(i %% 3)+1] *
                        max(eval(as.symbol(paste0("covid_country",
                                                  new_or_cum)))[,
                                                                select_columns[i+1]]))
    }
    # print(ylim_max)
    ylim_max <- 1.08* ylim_max

    par(mar = c(5, 4, 3, 4) + 0.1)

    if (!permillion || !all(country_chosen %in% pays_jc) ) {
      ylaby <- "nb cas"
      yax_label <- list( title = "nb cas")
    } else {
      ylaby <- "nb cas / million"
      yax_label <- list( title = "nb cas / million")
    }

    if (permillion & !all(country_chosen %in% pays_jc)) {
      output$errorMessage <-  renderUI({
          str1 <- "Population non disponible pour au moins 1 pays selectionné."
          str2 <- "Si nécessaire, contactez jeromecholewa@gmail.com"
          HTML(paste(str1, str2, sep = '<br/>'))
        })

    } else {
      output$errorMessage <-renderUI({ HTML("")})
    }

    if (!all(country_chosen %in% pays_jc)) {
      output$errorMessage2 <- output$errorMessage3 <- renderUI({
          str1 <- "Population non disponible pour au moins 1 pays selectionné."
          str2 <- "Si nécessaire, contactez jeromecholewa@gmail.com"
          HTML(paste(str1, str2, sep = '<br/>'))
        })

    } else {
      output$errorMessage2 <- output$errorMessage3 <-
        renderUI({ HTML("")})
    }



    ############################
    # The function below plots each country (by index j+1 of chosen_country)
    #### conf_rem_dec is a 3 integer vector
    #### should be entered by the user
    #### it looks like c(0,1,1) or c(1,1,1) if you want to plot the last 2 ou  3 plots, respectively
    # new_or_cum <- "_new"   # should be entered by the user

    plot_covid <- function(j
                           #, conf_rem_dec
                           #, select_columns,
                           # permillion, multiplier, okpermillion
    ) {
      pchar <-  j-1
      #lty_jc <- c(1,2,1)
      # col_jc <- c("lightblue", "darkgreen", "red")
      for (k in 1:3) {   #   1 is confirmed; 2 is recovered; 3 is deaths
        if (conf_rem_dec[k] == 1 ) {
          lines(dates,
                eval(as.symbol(paste0("covid_country",
                                      new_or_cum)))[,
                                                    select_columns[3*(j-1)+k]]*multiplier[(j)*3],
                type = "l", col = col_jc[k], lty = lty_jc[k])
          points(dates,
                 eval(as.symbol(paste0("covid_country",
                                       new_or_cum)))[,select_columns[3*(j-1)+k]]*multiplier[(j)*3],
                 pch = pchar, cex = 0.7, col = col_jc[k])
          text(x = tail(covid_country$dates,1),
               y = tail(eval(as.symbol(paste0("covid_country",
                                              new_or_cum)))[,select_columns[3*(j-1)+k]]*multiplier[(j)*3],
                        1),
               pos = 3, cex = 0.7,
               labels = format(tail(eval(as.symbol(paste0("covid_country",
                                                          new_or_cum)))[,select_columns[3*(j-1)+k]]*multiplier[(j)*3],
                                    1), digits = 1,
                               nsmall = 0 + okpermillion),
               col = col_jc[k])
        }
      }
    }


    ########### initialize plot
    plot_covid_all <- function(
      country_chosen
      # , conf_rem_dec
      # , new_or_cum
      # , permillion
    ) {

      # select_columns <-numeric(0)
      # for (j in 1:length(country_chosen)) {
      #   select_columns <- c(select_columns,
      #                       which(gsub("_.+","",
      #                                  names(covid_country)) ==
      #                               country_chosen[j]))
      # }
      # conf_rem_dec <-  c(0,1,1)
      # names(covid_country)[select_columns]
      # permillion <- T

      # okpermillion <- permillion & all(country_chosen %in% pays_jc)
      #
      # if (!okpermillion )  {
      #   multiplier <- rep(1, length.out = 3* length(country_chosen))
      # } else {
      #   multiplier <- rep(1000000 /unname(getpop_jc[country_chosen]),
      #                     each = 3)
      # }



      # ylim_max <-0
      # for (i in 0:(length(select_columns)-1)) {
      #   ylim_max <- max(ylim_max,
      #                   multiplier[i+1] * conf_rem_dec[(i %% 3)+1] *
      #                     max(eval(as.symbol(paste0("covid_country",
      #                                               new_or_cum)))[,
      #                                                             select_columns[i+1]]))
      # }
      # # print(ylim_max)
      # ylim_max <- 1.08* ylim_max
      #
      # par(mar = c(5, 4, 3, 4) + 0.1)
      # if (!permillion|| !all(country_chosen %in% pays_jc) ) {
      #   ylaby <- "nb cas"
      # } else {
      #   ylaby <- "nb cas / million"
      # }
      #
      # if (permillion & !all(country_chosen %in% pays_jc)) {
      #   output$errorMessage <- renderUI({
      #     str1 <- "Population non disponible pour au moins 1 pays selectionné."
      #     str2 <- "Si nécessaire, contactez jeromecholewa@gmail.com"
      #     HTML(paste(str1, str2, sep = '<br/>'))
      #   })
      # } else {
      #   output$errorMessage <- renderUI({ HTML("")})
      # }

      ### initiating empty plot with correct axes and labels and legends
      plot(
        x = dates,
        # x = eval(as.symbol(paste0("covid_country",new_or_cum)))$dates,
        y = numeric(length(dates)),
        ylab = ylaby, xlab = "Date", ylim = c(0, ylim_max),
        pch = "",
        xaxt = "n", las = 2, cex.axis = 0.7,
        cex.main = 0.9
        #,
        # main = paste0("Données actualisées le ",
        #               update_datetime),
        # as.character(format(file.mtime(grep("global",
        #                                     list.files(),
        #                                     value = TRUE))[1],
        #                     "%d %b %Y at %H:%M:%S")),

        # cex.sub = 0.7, sub = paste0("Source: ",
        #                             url_address)
      )
      abline( v=dates[seq(1, length(covid_country$dates), by=2)],
              lty = "dotted", lwd=0.4)
      abline( h = seq(0, by = tail(axTicks(2),1)/(2*(length(axTicks(2))-1)),
                      length.out = 2* length(axTicks(2))) ,
              lty = "dotted", lwd=0.4)
      axis( side = 4, las = 2, cex.axis = 0.7)
      axis(1, las = 2, cex.axis = 0.7,
           at = dates[seq(1, length(dates), by=2)],
           labels = format(dates[seq(1,
                                     length(dates),
                                     by=2)], "%m/%d" ))
      legend(x = "topleft", legend=c("Cas confirmés", "Remissions", "Deces"),
             # pch = 20,
             col=c("lightblue", "darkgreen", "red"), lty=c(1,2,1), cex=0.8)
      legend(x = "top", legend = country_chosen,
             pch = 1:length(country_chosen) -1 , cex=1)

      ##### calling plot function for each country
      for (j in 1:length(country_chosen)) {
        plot_covid( j
                    #, conf_rem_dec
                    #, select_columns, permillion, multiplier, okpermillion
        )
      }

    }

    ######### calling plot_ly function for each conf_rem_dec
    plotly_covid <- function(pp,j, k
                             # , select_columns, permillion, multiplier, okpermillion
                             # k in 1:3  #  1 is confirmed; 2 is recovered; 3 is deaths
    ) {
      pp <- add_trace(pp,
                      #x = ~dates,
                      x = dates,
                      y = eval(as.symbol(
                        paste0("covid_country",
                               new_or_cum)))[[select_columns[3*(j-1)+k]]]*multiplier[(j)*3],
                      type = "scatter",
                      line = lty_ploty[[k]],
                      # color = I(col_jc[k]), # color by type of data
                      # need to specify color by country
                      color = I(cl[j]),
                      mode = "lines+markers",
                      marker = list(
                        symbol = vals_open_dot[j],
                        size = 6),
                      name = names(eval(as.symbol(paste0("covid_country",
                                                         new_or_cum))))[select_columns[3*(j-1)+k]]
      )
    }


    # if (!permillion|| !all(country_chosen %in% pays_jc) ) {
    #   yax_label <- list( title = "nb cas")
    # } else {
    #   yax_label <- list( title = "nb cas / million")
    # }

    # plot_ly_covid_all(pp, country_chosen,
    #    conf_rem_dec, new_or_cum, permillion)
    plot_ly_covid_all <- function(pp
                                  # ,country_chosen, conf_rem_dec, new_or_cum, permillion
    ) {

      # select_columns <-numeric(0)
      # for (l in 1:length(country_chosen)) {
      #   select_columns <- c(select_columns,
      #                       which(gsub("_.+","",
      #                                  names(covid_country)) ==
      #                               country_chosen[l]))
      # }
      # conf_rem_dec <-  c(0,1,1)
      # names(covid_country)[select_columns]
      # new_or_cum <-  "" # _new"


      # ylim_max <-0
      # for (i in 0:(length(select_columns)-1)) {
      #   ylim_max <- max(ylim_max,
      #                   conf_rem_dec[(i %% 3)+1]*
      #                     max(eval(as.symbol(paste0("covid_country",
      #                                               new_or_cum)))[,
      #                                                             select_columns[i+1]]))
      # }
      # ylim_max <- 1.08* ylim_max

      # par(mar = c(5, 4, 3, 4) + 0.1)

      ### initiating empty plot with correct axes and labels and legends
      # covid_country[1:4, select_columns]

      #covid_country[[select_columns[j]]]
      # rm(pp)
      # j <- 1
      # k <- 1
      # conf_rem_dec <- c(1,1,1)
      # pchar <-  j  # j is between 1 and 3 x nb of countries chosen
      # col_jc[(j-1) %% 3 +1]
      # rm(pp)
      # pp <- plot_ly()
      # for (j in 1:4  ) {for (k in 1:3) {   print(paste0(as.character(3*(j-1)+k),"&",as.character(j),"&",as.character(k)))} }

      # okpermillion <- permillion & all(country_chosen %in% pays_jc)
      #
      # if (!okpermillion )  {
      #   multiplier <- rep(1, length.out = 3* length(country_chosen))
      # } else {
      #   multiplier <- rep(1000000 /unname(getpop_jc[country_chosen]),
      #                     each = 3)
      # }

      # j <- 1
      # k <- 2
      for (j in 1:length(country_chosen)) {
        for (k in 1:3) {   #   1 is confirmed; 2 is recovered; 3 is deaths
          if (conf_rem_dec[k] == 1 ) {
            pp <- plotly_covid(pp,j,k
                               # , select_columns, permillion, multiplier, okpermillion
            )
          }
        }
      }

      # if (!permillion|| !all(country_chosen %in% pays_jc) ) {
      #   yax_label <- list( title = "nb cas")
      # } else {
      #   yax_label <- list( title = "nb cas / million")
      # }

      xax_label <- list(  title = "Date")
      pp <- layout(pp, yaxis = yax_label, xaxis = xax_label,
                   legend = list(xanchor = "center",
                                 x = 0.5, # centered to plot
                                 yref = 'paper', y = 0.99,
                                 yanchor = 'top'
                   ),
                   # title = paste0("Données actualisées le ",
                   #                update_datetime),
                   showlegend= TRUE )

    }

    # output$Plot1 <- renderPlot({
    #
    #   plot_covid_all(
    #     country_chosen
    #     #, conf_rem_dec
    #     #, new_or_cum, permillion
    #   )
    # })

    output$Plot_ly1 <- renderPlotly({
      # , pp, select_columns)
      # rm(pp)
      pp <- plot_ly()
      pp <- plot_ly_covid_all(pp
                              # , country_chosen,
                              # conf_rem_dec, new_or_cum, permillion
      )
      pp
    })

    ####################
    ############# jour 0  deces
    ####################

    select_columns_deces <-numeric(0)
    for (j in 1:length(country_chosen)) {
      select_columns_deces <- c(select_columns_deces,
                                which(gsub("_.+","",
                                           names(covid_country)) ==
                                        country_chosen[j] &
                                        gsub(".+_","",
                                             names(covid_country)) ==
                                        "décès"
                                ))
    }

    # names(covid_country)[select_columns_deces]

    ##### initialize list of countries
    covid_country_deces_jour <- vector(mode = "list",
                                       length = length(country_chosen))

    # j <-  2
    for (j in 1:length(country_chosen)) {
      covid_country_deces_jour[[j]] <- covid_country[,c(1,select_columns_deces[j])]

      if (all(covid_country_deces_jour[[j]][,2] < 10)){
        covid_country_deces_jour[[j]] <- tail(covid_country_deces_jour[[j]], 1)
      } else {
        covid_country_deces_jour[[j]] <- covid_country_deces_jour[[j]][covid_country_deces_jour[[j]][,2] >= 10,]
      }

      covid_country_deces_jour[[j]]$jour <- 0:(length(covid_country_deces_jour[[j]]$dates) -1)
    }

    # j <- 1
    output$Plot_ly_deces_jour0 <- renderPlotly({
      # , pp, select_columns)
      # rm(pp)
      pp_jour <- plot_ly()
      for (j in 1:length(country_chosen)) {
        pp_jour <- add_trace(pp_jour,
                             x = covid_country_deces_jour[[j]]$jour,
                             #x = dates,
                             y = covid_country_deces_jour[[j]][,2],
                             type = "scatter",
                             line = lty_ploty[[3]],
                             #color = I(col_jc[3]),
                             mode = "lines+markers",
                             marker = list(
                               symbol = vals_open_dot[j],
                               size = 6),
                             name = names(covid_country_deces_jour[[j]][2])
        )
      }

      pp_jour <- layout(pp_jour,
                        yaxis = list(title = "Nb de cas totaux")#, size = 24)
                        ,
                        xaxis = list(title = "Jours"),
                        legend = list(xanchor = "left",
                                      x = 0.08, # centered to plot
                                      yref = 'paper', y = 0.99,
                                      yanchor = 'top'
                        ),
                        # title = paste0("Données actualisées le ",
                        #                update_datetime),
                        showlegend= TRUE )


      pp_jour
    })   ## fin de renderPlotly pour "jour zero deces"

    ######### Plotly jour_deces_zero_million
    ######### debut de renderPlotly pour "jour zero deces per million"

    ##### initialize list of countries
    covid_country_deces_jzero_million <- vector(mode = "list",
                                                length = length(country_chosen))

    j <- 1
    for (j in 1:length(country_chosen)) {
      if (!is.na(multiplier_zero_million[j])) {

        covid_country_deces_jzero_million[[j]] <- covid_country[,c(1,select_columns_deces[j])]
        covid_country_deces_jzero_million[[j]][,2] <- covid_country_deces_jzero_million[[j]][,2] * multiplier_zero_million[j]

        if (all(covid_country_deces_jzero_million[[j]][,2] < 0.15)){
          covid_country_deces_jzero_million[[j]] <- tail(covid_country_deces_jzero_million[[j]], 1)
          #str(tail(covid_country_deces_jzero_million[[j]], 1))
          #covid_country_deces_jzero_million[[j]][83,] <- c(12/12/2019,0)
        } else {
          covid_country_deces_jzero_million[[j]] <- covid_country_deces_jzero_million[[j]][covid_country_deces_jzero_million[[j]][,2] >= 0.15,]
        }

        covid_country_deces_jzero_million[[j]]$jour <- 0:(length(covid_country_deces_jzero_million[[j]]$dates) -1)
      }
    }



    output$Plot_ly_deces_jour0_million <- renderPlotly({
      # , pp, select_columns)
      # rm(pp)
      pp_jour_deces_million <- plot_ly()
      for (j in 1:length(country_chosen)) {
        if (!is.null(covid_country_deces_jzero_million[[j]])) {
          pp_jour_deces_million <- add_trace(pp_jour_deces_million,
                                             x = covid_country_deces_jzero_million[[j]]$jour,
                                             #x = dates,
                                             y = covid_country_deces_jzero_million[[j]][,2],
                                             type = "scatter",
                                             line = lty_ploty[[3]],
                                             #color = I(col_jc[3]),
                                             mode = "lines+markers",
                                             marker = list(
                                               symbol = vals_open_dot[j],
                                               size = 6),
                                             name = names(covid_country_deces_jzero_million[[j]][2])
          )
        }
      }

      pp_jour_deces_million <- layout(pp_jour_deces_million,
                        yaxis = list(title = "Nb cas / million"),
                        xaxis = list(title = "Jours"),
                        legend = list(xanchor = "left",
                                      x = 0.08, # centered to plot
                                      yref = 'paper', y = 0.99,
                                      yanchor = 'top'
                        ),
                        # title = paste0("Données actualisées le ",
                        #                update_datetime),
                        showlegend= TRUE )


      pp_jour_deces_million
    })   ## fin de renderPlotly pour "jour zero deces million"


    ####################
    ############# jour 0  confirmes
    ####################

    select_columns_confimes <-numeric(0)
    for (j in 1:length(country_chosen)) {
      select_columns_confimes <- c(select_columns_confimes,
                                which(gsub("_.+","",
                                           names(covid_country)) ==
                                        country_chosen[j] &
                                        gsub(".+_","",
                                             names(covid_country)) ==
                                        "confirmés"
                                ))
    }

    # names(covid_country)[select_columns_confimes]

    ##### initialize list of countries
    covid_country_confirmes_jour <- vector(mode = "list",
                                       length = length(country_chosen))

    # j <-  2
    for (j in 1:length(country_chosen)) {
      covid_country_confirmes_jour[[j]] <- covid_country[,c(1,select_columns_confimes[j])]

      if (all(covid_country_confirmes_jour[[j]][,2] < 10)){
        covid_country_confirmes_jour[[j]] <- tail(covid_country_confirmes_jour[[j]], 1)
      } else {
        covid_country_confirmes_jour[[j]] <- covid_country_confirmes_jour[[j]][covid_country_confirmes_jour[[j]][,2] >= 30,]
      }

      covid_country_confirmes_jour[[j]]$jour <- 0:(length(covid_country_confirmes_jour[[j]]$dates) -1)
    }

    # j <- 1
    output$Plot_ly_confirmes_jour0 <- renderPlotly({
      # , pp, select_columns)
      # rm(pp)
      pp_jour_confirmes <- plot_ly()
      for (j in 1:length(country_chosen)) {
        pp_jour_confirmes <- add_trace(pp_jour_confirmes,
                             x = covid_country_confirmes_jour[[j]]$jour,
                             #x = dates,
                             y = covid_country_confirmes_jour[[j]][,2]*multiplier_zero_abs[j],
                             type = "scatter",
                             line = lty_ploty[[3]],
                             #color = I(col_jc[3]),
                             mode = "lines+markers",
                             marker = list(
                               symbol = vals_open_dot[j],
                               size = 6),
                             name = names(covid_country_confirmes_jour[[j]][2])
        )
      }

      pp_jour_confirmes <- layout(pp_jour_confirmes,
                                  yaxis = list(title = "Nb de cas totaux", size = 24),
                        xaxis = list(title = "Jours"),
                        legend = list(xanchor = "left",
                                      x = 0.08, # centered to plot
                                      yref = 'paper', y = 0.99,
                                      yanchor = 'top'
                        ),
                        # title = paste0("Données actualisées le ",
                        #                update_datetime),
                        showlegend= TRUE )


      pp_jour_confirmes
    })   ## fin de renderPlotly pour "jour zero confirmes"


    ####################
    ############# jour 0  confirmes per million
    ####################
    ##### initialize list of countries
    covid_country_confirmed_j0_million <- vector(mode = "list",
                                                length = length(country_chosen))

    j <- 1
    for (j in 1:length(country_chosen)) {
      if (!is.na(multiplier_zero_million[j])) {
        covid_country_confirmed_j0_million[[j]] <- covid_country[,c(1,select_columns_confimes[j])]
        covid_country_confirmed_j0_million[[j]][,2] <- covid_country_confirmed_j0_million[[j]][,2] * multiplier_zero_million[j]

        if (all(covid_country_confirmed_j0_million[[j]][,2] < 0.5)){
          covid_country_confirmed_j0_million[[j]] <- tail(covid_country_confirmed_j0_million[[j]], 1)
        } else {
          covid_country_confirmed_j0_million[[j]] <- covid_country_confirmed_j0_million[[j]][covid_country_confirmed_j0_million[[j]][,2] >= 0.5,]
        }

        covid_country_confirmed_j0_million[[j]]$jour <- 0:(length(covid_country_confirmed_j0_million[[j]]$dates) -1)
      }
    }

    output$Plot_ly_confirmes_jour0_million <- renderPlotly({
      # , pp, select_columns)
      # rm(pp)
      pp_jour_confirmes_million <- plot_ly()
      for (j in 1:length(country_chosen)) {
        if (!is.null(covid_country_confirmed_j0_million[[j]])) {
          pp_jour_confirmes_million <- add_trace(pp_jour_confirmes_million,
                                                 x = covid_country_confirmed_j0_million[[j]]$jour,
                                                 #x = dates,
                                                 y = covid_country_confirmed_j0_million[[j]][,2],
                                                 type = "scatter",
                                                 line = lty_ploty[[3]],
                                                 #color = I(col_jc[3]),
                                                 mode = "lines+markers",
                                                 marker = list(
                                                   symbol = vals_open_dot[j],
                                                   size = 6),
                                                 name = names(covid_country_confirmed_j0_million[[j]][2])
          )
        }
      }

      pp_jour_confirmes_million <- layout(pp_jour_confirmes_million,
                                          yaxis = list(title = "Nb cas / million"),
                                  xaxis = list(title = "Jours"),
                                  legend = list(xanchor = "left",
                                                x = 0.08, # centered to plot
                                                yref = 'paper', y = 0.99,
                                                yanchor = 'top'
                                  ),
                                  # title = paste0("Données actualisées le ",
                                  #                update_datetime),
                                  showlegend= TRUE )


      pp_jour_confirmes_million
    })   ## fin de renderPlotly pour "jour zero confirmes million"


  })   ## fin de "observe()

})
